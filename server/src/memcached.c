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
#include <sys/timerfd.h>
#include "memcached.h"
#include "cache.h"
#include "common.h"
#include "malloc_interface.h"
#include "text_manage.h"
#include "bin_manage.h"
#include "user_data.h"
#include "timer.h"

#define MAX_EVENTS 10

long MAX_THREADS;

Cache memcache;

typedef struct epoll_loop {
	int epollfd; //File descriptor de epoll
	int epoll_timer; //File descriptor de epoll para timer
	int fd_text; //File descriptor del socket de texto
	int fd_bin;  //File descriotor del socket binario
} epoll_loop;

/**
 * Setea el file descriptor de ud para que
 * vuelva a ser tenido en cuenta por epollfd
 * @param epollfd file desciptor de epoll
 * @param ud datos de usuario
*/
void listenAgain(int epollfd, User_data* ud) {
	struct epoll_event event;
	event.data.ptr = ud;
	event.events = EPOLLIN | EPOLLONESHOT;
	epoll_ctl(epollfd, EPOLL_CTL_MOD,
		ud->fd, &event);	
}

/**
 * Cierra la conexión de un usuario, también
 * elimina sus datos
 * @param timer_epoll sirve para eliminar el
 * timer asociado en caso de que exista.
 * @param ud Datos del usuario a eliminar.
*/
void disconnect_user(int timer_epoll ,User_data* ud) {
	if(timer_epoll != -1 &&
			ud->udBin->prevRead == 1) {
		timerDel(timer_epoll, ud);
	}
	close(ud->fd);
	epoll_ctl(ud->fd, EPOLL_CTL_DEL,
		ud->fd, NULL);
	user_data_destroy(ud);
	puts("usuario desconectado");		
}

/**
 * Función auxiliar de handle_user, esta
 * función se llama cuando el usuario está
 * en modo binario.
 * En caso que el paquete se envie por partes,
 * si los datos esperados no lleguen en
 * TIMEOUT_MS(timer.c), se descarta todo lo enviado
 * hasta el momento y se envía EINVALID
 * al cliente.
 * @param epollfd file desctiptor de epoll.
 * @param ud puntero a estructura con
 * datos del usuario.
 * @param timer_epoll epoll de los timer
 * para el manejo de los mismos
*/
void handle_binUser(int epollfd, 
		User_data* ud, int timer_epoll) {
	int readRet = readBin(ud);
	if(-1 == readRet) {
		disconnect_user(timer_epoll, ud);
		return;
	}
	if(0 == readRet) {
		if(1 == ud->udBin->prevRead) {
			timerDel(timer_epoll, ud);
		}
		ud->udBin->prevRead = 0; 		
		bin_consume(memcache, 
			ud->buf, ud->fd);
		user_data_restart(ud);
		listenAgain(epollfd, ud);
		return;
	}
	if(1 == readRet) {
		if(0 == ud->udBin->prevRead) {
			ud->udBin->prevRead = 1;
			setTimer(ud, timer_epoll);	
		}
		listenAgain(epollfd, ud);
		return;				
	}
	printf("readRet VALOR DESCONOCIDO %d\n", readRet);	
	perror("readRet error");
	exit(EXIT_FAILURE);
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
		ud->buf = allocate_mem(2048,NULL);
	}
	int res = text_consume(memcache,
		ud);
	if(res == -1) {
		disconnect_user(-1, ud);
		return;
	} else {
		listenAgain(epollfd, ud);
		return;			
	}
}

/**
 * Dependiendo de si el usuario está en modo
 * texto o binario llama a handle_textUser
 * o handle_binUser, las cuales leen lo que
 * el usuario envió al servidor y les
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
void handle_user(int epollfd, int timer_epoll, User_data* ud) {
	if(ud->udBin != NULL)
		handle_binUser(epollfd, ud, timer_epoll);
	else
		handle_textUser(epollfd, ud);
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
				//puts("manejo peticion");
 				handle_user(eloop->epollfd, 
					eloop->epoll_timer, ud);
				//puts("peticion manejada");
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

	int epoll_timer = epoll_create1(0);
	pthread_t timerThread;
	pthread_create(&timerThread, NULL,
			timer_epollStart, (void*)&epoll_timer);

	setnonblocking(binsock);
	setnonblocking(textsock);
	int epoll_fd = epoll_create1(0);
  struct epoll_event epollevent;
  epollevent.events = EPOLLIN | EPOLLET;
	
	epoll_loop eloop;
	eloop.epollfd = epoll_fd;
	eloop.epoll_timer = epoll_timer;
	eloop.fd_bin = binsock;
	eloop.fd_text = textsock;	
	
  epollevent.data.ptr = user_data_init(binsock, BINARY);
  epoll_ctl(epoll_fd, EPOLL_CTL_ADD,
		binsock, &epollevent);

	epollevent.data.ptr = user_data_init(textsock, TEXT);
	epoll_ctl(epoll_fd, EPOLL_CTL_ADD,
		textsock, &epollevent);

	printf("epoll configurado\n");

	pthread_t threads[MAX_THREADS];
	for (long i = 0; i < MAX_THREADS; i++) {
		pthread_create(threads+i, NULL,
			eventloop, (void*) &eloop);
	}	

	pthread_join(timerThread,NULL);
	for (long i = 0; i < MAX_THREADS; i++) {
		pthread_join(threads[i],NULL);
	}
}

void memcached_start(int tsock,int bsock){
	printf("iniciando\n");
	memcache = cache_create(1000000);
	printf("cache creada\n");
	MAX_THREADS = sysconf(_SC_NPROCESSORS_ONLN);	
	epoll_start(bsock, tsock);
};
