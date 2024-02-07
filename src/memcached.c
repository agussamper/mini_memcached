#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/epoll.h>
#include <sys/resource.h>
#include <assert.h>
#include <signal.h>
#include <pthread.h>
#include <errno.h>
#include <inttypes.h>
#include "memcached.h"
#include "cache.h"
#include "common.h"
#include "arr_func.h"
#include "malloc_interface.h"
#include "../concurrent_queue/concurrent_queue.h"
#include "text_manage.h"
#include "bin_manage.h"
#include "bin_data.h"
#include "epollfd.h"
#define MAX_THREADS 6
#define MAX_EVENTS 10

Cache memcache;

int textsock;
int binsock;

void* wait_for_req(void* argv){
	ConcurrentQueue* conqueue = 
		(ConcurrentQueue*) argv;
	while(1){
	epollfd* epfd = 
		concurrent_queue_dequeue(
			conqueue,
			(Destroy)epfd_dstr,
			(Copy) epfd_copy);
	printf("hola que tal que necesita\n");
	if(!epfd->bd){
		printf("texto detectado\n");
		char buf[2048];
		int n = text_consume(
			memcache, epfd->fd, buf); //TODO: no pasar buf
		printf("text:%d\n",n);
		if(n < 0) {			
			close(epfd->fd);
			epoll_ctl(epfd->fd, EPOLL_CTL_DEL,
				binsock, NULL);
		}
	} else {
		printf("binario detectado\n");
		int n = bin_consume(memcache, epfd);

		printf("bin:%d\n",n);
		if(n < 0) {
			close(epfd->fd);
			epoll_ctl(epfd->fd, EPOLL_CTL_DEL,
				binsock, NULL);
		}
	}
	free(epfd);
	}
}

void* text_epoll(void* argv){
	ConcurrentQueue* conqueue
	 	= (ConcurrentQueue*) argv;
	int epoll_fd = epoll_create1(0);
  printf("text:hola\n");
	int tcsock;
  struct epoll_event textevent;
  textevent.events = EPOLLIN;
  textevent.data.fd = textsock;
  epoll_ctl(epoll_fd, EPOLL_CTL_ADD,
		textsock, &textevent);
	struct epoll_event textevents[MAX_EVENTS];
	printf("epoll texto configurado\n");
	while (1){
		int text_num_events = epoll_wait(
			epoll_fd, textevents, MAX_EVENTS, -1);
		if (text_num_events == -1) {
			perror("textepoll_wait");
			exit(EXIT_FAILURE);
		}
		for(int i = 0; i<text_num_events; i++){
			if(textevents[i].data.fd == textsock){
				tcsock = accept(textsock, NULL, NULL);
				if (tcsock < 0) quit("accept");
				printf("cliente aceptado\n");
				textevent.events = EPOLLIN | EPOLLET;
				textevent.data.fd = tcsock;
				epoll_ctl(epoll_fd, EPOLL_CTL_ADD,
					tcsock, &textevent);
			}
			else{
				epollfd* epollfd = malloc(sizeof(epollfd));
				epollfd->bd = NULL;
				epollfd->fd = textevents[i].data.fd;
				concurrent_queue_enqueue(conqueue,
					epollfd, (Copy) epfd_copy);
			}
		}
  }
}

void* bin_epoll(void* argv){
	ConcurrentQueue* conqueue =
		 (ConcurrentQueue*) argv;
  printf("bin:hola\n");
	int epoll_fd = epoll_create1(0);
	int bcsock;
  struct epoll_event binevent;
  binevent.events = EPOLLIN;
  binevent.data.fd = binsock;
  epoll_ctl(epoll_fd, EPOLL_CTL_ADD,
		binsock,&binevent);
	struct epoll_event binevents[MAX_EVENTS];
	printf("epoll bin configurado\n");
	while (1){
		int bin_num_events = 
			epoll_wait(epoll_fd,
				binevents, MAX_EVENTS, -1);
		if (bin_num_events == -1) {
			perror("binepoll_wait");
			exit(EXIT_FAILURE);
		}
		for(int i = 0; i<bin_num_events; i++){
			if(binevents[i].data.fd == binsock) {
				bcsock = accept(binsock, NULL, NULL);
				if (bcsock < 0) quit("accept");
				printf("cliente aceptado\n");
				binevent.events = EPOLLIN | EPOLLET;
				binevent.data.fd = bcsock;
				Bin_data* bd = bin_data_init();
				bin_data_setReading(bd,0);
				binevent.data.ptr = bd;
				epoll_ctl(epoll_fd,EPOLL_CTL_ADD,
					bcsock,&binevent);
			} else {
				Bin_data* bd = (Bin_data*)binevents[i].data.ptr;
				pthread_mutex_lock(&bd->buf_mutex);
				if(!bd->reading) {
					pthread_mutex_unlock(&bd->buf_mutex);
					epollfd* epollfd = allocate_mem
						(sizeof(epollfd), NULL); //TODO: cuando se libera??
					epollfd->bd = binevents[i].data.ptr;					
					epollfd->fd = binevents[i].data.fd;
					concurrent_queue_enqueue(conqueue,
						epollfd, (Copy) epfd_copy);
				} else {
					pthread_mutex_unlock(&bd->buf_mutex);
				}
			}
		}
  }
}

void server_start(){
	//text fd
	printf("iniciando servidor\n");


	/**
	 * ACA SE VAN A GUARDAR LOS PEDIDOS, LOS
	 * THREADS TOMARAN DE A UNO Y ATENDERAN
	 * SIGUIENDO UNA LOGICA DE PRODUCTOR 
	 * CONSUMIDOR CON ESTRUCTURA FIFO
	*/
	ConcurrentQueue* conqueue = 
		malloc(sizeof(ConcurrentQueue));

	concurrent_queue_init(conqueue,30);
	printf("conqueue inicializada\n");
	pthread_t threads[MAX_THREADS];
	for (size_t i = 0; i < MAX_THREADS -2; i++)
	{
		pthread_create(&threads[i], NULL,
			wait_for_req, (void*) (conqueue));
	}
	pthread_t eptext;
	pthread_t epbin;
	pthread_create(&eptext, NULL,
		text_epoll, (void*) (conqueue));
	pthread_create(&epbin, NULL, 
		bin_epoll, (void*) (conqueue));
	printf("hilos creados \n");
	pthread_join(epbin,NULL);
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
	textsock=tsock;
	binsock=bsock;
	printf("iniciando\n");
	memcache = cache_create(1000000,str_KRHash);
	printf("cache creada\n");
	server_start();
	return 0;
};
