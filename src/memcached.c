#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/epoll.h>
#include <sys/resource.h>
#include <assert.h>
#include <signal.h>
#include <pthread.h>
#include <errno.h>
#include <string.h>
#include <inttypes.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <syscall.h>
#include "memcached.h"
#include "cache.h"
#include "common.h"
#include "arr_func.h"
#include "malloc_interface.h"
#include "../concurrent_queue/concurrent_queue.h"
#include "text_manage.h"
#include "bin_manage.h"
#include "user_data.h"
#define MAX_THREADS 6
#define MAX_EVENTS 10

Cache memcache;

typedef struct epoll_loop {
	int epollfd;
	int fd_text;
	int fd_bin;
} epoll_loop;

void handle_user(int epollfd, User_data* ud) {
	if(ud->mode == BINARY){
		while (1) {
			puts("antes de read");
			int readRet = user_data_read(ud);
			printf("readRet=%d\n",readRet);
			if(-1 == readRet) {
				close(ud->fd);
				epoll_ctl(ud->fd, EPOLL_CTL_DEL,
					ud->fd, NULL);
				user_data_destroy(ud);		
				return;
			}
			if(0 == readRet) { 
					puts("bin");
					bin_consume(memcache, 
						ud->buf, ud->fd);
					user_data_restart(ud);
					struct epoll_event event;
					event.data.ptr = ud;
					event.events = EPOLLIN | EPOLLONESHOT;
					epoll_ctl(epollfd, EPOLL_CTL_MOD,
						ud->fd, &event);				
				return;
			}
			if(1 == readRet) {
				struct epoll_event event;
				event.data.ptr = ud;
				event.events = EPOLLIN | EPOLLONESHOT;
				epoll_ctl(epollfd, EPOLL_CTL_MOD,
					ud->fd, &event);
				return;				
			}
			printf("readRet VALOR DESCONOCIDO %d\n", readRet);	
			perror("readRet error");
			exit(EXIT_FAILURE);
		}
	}
	else if(ud->mode == TEXT){
		if(ud->buf == NULL){
			ud->buf = allocate_mem(2048,NULL);
			ud->bufSize = 2048;
		}
		while(1){
			int res = text_consume(memcache,ud->fd,ud->buf,&ud->offset);
			if(res == -1){
				close(ud->fd);
				epoll_ctl(ud->fd, EPOLL_CTL_DEL,
				ud->fd, NULL);
				user_data_destroy(ud);		
			return;
			}else{
				struct epoll_event event;
				event.data.ptr = ud;
				event.events = EPOLLIN | EPOLLONESHOT;
				epoll_ctl(epollfd, EPOLL_CTL_MOD,
					ud->fd, &event);
				return;			
			}
			}
	} else {
					perror("handle_user: invalid mode");
					printf("fd=%d, mode=%d\n",
						ud->fd, ud->mode);
					exit(EXIT_FAILURE);
				}
}

void setnonblocking(int sockfd) {
	// Configurar el socket para que sea no bloqueante
  int flags = fcntl(sockfd, F_GETFL, 0);
  if (-1 == flags) {
    perror("Error obteniendo los flags del socket");
    exit(EXIT_FAILURE);
  }
  if (fcntl(sockfd, F_SETFL, flags | O_NONBLOCK) == -1) {
    perror("Error configurando el socket como no bloqueante");
    exit(EXIT_FAILURE);
  }
}

void user_accept(epoll_loop* eloop, int mode) {
	struct epoll_event epollevent;
	int acceptret;
	while(1) {		
		if(mode == BINARY) {
			puts("antes de accept4");
			acceptret = accept4(eloop->fd_bin,
			 NULL, NULL, O_NONBLOCK);
		} else if(mode == TEXT) {
			acceptret = accept4(eloop->fd_text,
			 NULL, NULL, O_NONBLOCK);
		} else {
			perror("invalid mode");
			exit(EXIT_FAILURE);
		}
		printf("acceptret=%d\n", acceptret);		
		if(acceptret == -1) {
			int err = errno;
			if(err == EAGAIN || err == EWOULDBLOCK) {
				return;
			}
			printf("error in accept4()! %s\n",
				strerror(errno));
			exit(EXIT_FAILURE);
		}
		epollevent.events = EPOLLIN | EPOLLONESHOT;
		epollevent.data.ptr = 
			(void*)user_data_init(acceptret, mode); 
		epoll_ctl(eloop->epollfd, EPOLL_CTL_ADD,
			acceptret, &epollevent);
	}
}

void* eventloop(void* arg) {
	epoll_loop* eloop = (epoll_loop*)arg;
	struct epoll_event events[MAX_EVENTS];
	int num_events;
	while(1) {
		num_events = 
			epoll_wait(eloop->epollfd,
				events, MAX_EVENTS, -1);
		printf("PASO WAIT, hilo=%ld\n numevents=%d\n", syscall(__NR_gettid), num_events);
		if (num_events == -1) {
			perror("binepoll_wait");
			exit(EXIT_FAILURE);
		}
		for(int i = 0; i < num_events; i++) {
			User_data* ud = 
				(User_data*)events[i].data.ptr;
			assert(ud);		
			if(ud->fd == eloop->fd_bin) {
				puts("por aceptar cliente binario");
				user_accept(eloop, BINARY);
				puts("cliente aceptado");
			} else if(ud->fd == eloop->fd_text) {	
				puts("por aceptar cliente texto");			
				user_accept(eloop, TEXT);
			} else {
				puts("manejo peticion");
 				handle_user(eloop->epollfd, ud);
				puts("peticion manejada");
			}
		}
	}
}

void epoll_start(int binsock, int textsock){
	setnonblocking(binsock);
	setnonblocking(textsock);
	int epoll_fd = epoll_create1(0);
	int bcsock;
  struct epoll_event epollevent;
  epollevent.events = EPOLLIN | EPOLLET;
	
	epoll_loop eloop;
	eloop.epollfd = epoll_fd;
	eloop.fd_bin = binsock;
	eloop.fd_text = textsock;
	
  epollevent.data.ptr = user_data_init(binsock, BINARY);
  epoll_ctl(epoll_fd, EPOLL_CTL_ADD,
		binsock, &epollevent);

	epollevent.data.ptr = user_data_init(textsock, TEXT);
	epoll_ctl(epoll_fd, EPOLL_CTL_ADD,
		textsock, &epollevent);

	printf("epoll bin configurado\n");

	pthread_t threads[MAX_THREADS];
	for (size_t i = 0; i < MAX_THREADS; i++) {
		pthread_create(threads+i, NULL,
			eventloop, (void*) &eloop);
	}

	for (size_t i = 0; i < MAX_THREADS; i++) {
		pthread_join(threads[i],NULL);
	}
}

void server_start(int textsock, int binsock) {
	//text fd
	printf("iniciando servidor\n");	
	epoll_start(binsock, textsock);	
}

unsigned str_KRHash(const char *s, uint32_t len) {
  unsigned hashval;
  uint32_t i = 0;
  for (hashval = 0;i<len; ++s) {
    hashval = *s + 31 * hashval;
	i++;
  }
  return hashval;
}

int memcached_cache_start(int tsock,int bsock){
	printf("iniciando\n");
	memcache = cache_create(1000000,str_KRHash);
	printf("cache creada\n");
	server_start(tsock, bsock);
	return 0;
};
