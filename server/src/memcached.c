#define _GNU_SOURCE

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
#include "malloc_interface.h"
#include "text_manage.h"
#include "bin_manage.h"
#include "user_data.h"
#define MAX_THREADS 6
#define MAX_EVENTS 10

Cache memcache;

typedef struct epoll_loop {
	int epollfd; //File descriptor de epoll
	int fd_text; //File descriptor del socket de texto
	int fd_bin;  //File descriotor del socket binario
} epoll_loop;

/**
 * Función auxiliar de handle_user, esta
 * función se llama cuando el usuario está
 * en modo binario
 * @param epollfd file desctiptor de epoll.
 * @param ud puntero a estructura con
 * datos del usuario.
*/
void handle_binUser(int epollfd, User_data* ud) {
	while (1) {
		int readRet = readBin(ud);
		if(-1 == readRet) {
			close(ud->fd);
			epoll_ctl(ud->fd, EPOLL_CTL_DEL,
				ud->fd, NULL);
			user_data_destroy(ud);
			puts("usuario desconectado");		
			return;
		}
		if(0 == readRet) { 
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

/**
 * Función auxiliar de handle_user, esta
 * función se llama cuando el usuario está
 * en modo texto
 * @param epollfd file desctiptor de epoll.
 * @param ud puntero a estructura con
 * datos del usuario.
*/
void handle_textUser(int epollfd, User_data* ud) {
	if(ud->buf == NULL){
		ud->readNext = 0;
		ud->buf = allocate_mem(2048,NULL);
		ud->bufSize = 2048;
	}
	while(1){
		int res = text_consume(memcache,
			ud);
		if(res == -1) {
			close(ud->fd);
			epoll_ctl(ud->fd, EPOLL_CTL_DEL,
			ud->fd, NULL);
			user_data_destroy(ud);
			puts("usuario desconectado");		
			return;
		} else {
			struct epoll_event event;
			event.data.ptr = ud;
			event.events = EPOLLIN | EPOLLONESHOT;
			epoll_ctl(epollfd, EPOLL_CTL_MOD,
				ud->fd, &event);
			return;			
		}
	}
}

/**
 * Dependiendo de si el usuario está en modo
 * texto o binario llama a handle_textUser
 * o handle_binUser, las cuales leen lo que
 * el usuario envío al servidor y les
 * contestan. Si no se pudo leer todo el
 * paquete porque es demasiado grande para
 * que entre en el buffer, guarda el estado
 * de lectura en ud para seguir leyendo desde
 * donde quedo, además modifica los file
 * descriptors para que vuelvan a ser tenido en
 * cuenta por epoll (ya que están en modo
 * EPOLLONESHOT).
 * @param epollfd file desctiptor de epoll.
 * @param ud puntero a estructura con
 * datos del usuario.
*/
void handle_user(int epollfd, User_data* ud) {
	if(ud->mode == BINARY){
		handle_binUser(epollfd, ud);
	}
	else if(ud->mode == TEXT){
		handle_textUser(epollfd, ud);
	} else {
		perror("handle_user: invalid mode");
		exit(EXIT_FAILURE);
	}
}

/**
 * Dado un file descriptor lo configurar
 * como no bloqueante.
 * @param sockfd Socket el cual se quiere
 * configurar como no bloqueante.
*/
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

/**
 * Acepta usuarios que se quieren conectar
 * al modo texto o binario, agregandolos en
 * epoll con eventos EPOLLIN | EPOLLONESHOT
 * @param eloop estructura con file desctiptors.
 * @param mode modo en el que se quiere conectar
 * se debe iniciar con las variables TEXT o 
 * BINARY (definidas en user_data.h).
*/
void user_accept(epoll_loop* eloop, int mode) {
	struct epoll_event epollevent;
	int acceptret;
	while(1) {		
		if(mode == BINARY) {
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

/**
 * Esta función tiene un bucle infinito
 * en el cual se atienden las peticiones
 * de los clientes.
 * @param arg Se espera un puntero a una
 * estructura de tipo epoll_loop.
*/
void* eventloop(void* arg) {
	epoll_loop* eloop = (epoll_loop*)arg;
	struct epoll_event events[MAX_EVENTS];
	int num_events;
	while(1) {
		num_events = 
			epoll_wait(eloop->epollfd,
				events, MAX_EVENTS, -1);
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

/**
 * Inicualiza el epoll, agregando en el mismo
 * binsock y textsock en modo edge triggering
 * y se pone a la escucha de eventos en los
 * sockets agregados
 * @param tsock Socket de escucha para modo texto
 * @param bsock Socket de escucha para modo binario
*/
void epoll_start(int binsock, int textsock){
	setnonblocking(binsock);
	setnonblocking(textsock);
	int epoll_fd = epoll_create1(0);
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

void memcached_start(int tsock,int bsock){
	printf("iniciando\n");
	memcache = cache_create(1000000);
	printf("cache creada\n");
	epoll_start(bsock, tsock);
};
