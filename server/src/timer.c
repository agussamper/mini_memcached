#define _GNU_SOURCE

#include "timer.h"
#include "common.h"
#include "malloc_interface.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/epoll.h>
#include <assert.h>
#include <signal.h>
#include <pthread.h>
#include <syscall.h>
#include <sys/timerfd.h>
#include <stdint.h>

#define TIMEOUT_MS 15000 //miliseconds
#define MAX_EVENTS 10

struct _Timerfd {
  int timefd;
  struct User_data* ud;
};

void timeOut(int timer_epoll,
	 Timerfd tfd) {
	puts("TIME OUT");	
	char c = EINVALID;
	write(tfd->ud->fd,&c,1);
	epoll_ctl(timer_epoll,
	 	EPOLL_CTL_DEL, tfd->timefd,
		NULL);
	user_data_restart(tfd->ud);
	free(tfd);	
}

/**
 * Configura el timer en TIMEOUT_MS,
 * lo asocia con ud y lo agrega al epoll.
 * @param ud datos de usuario a los que
 * estará asociado el timer.
 * @param timer_epoll file descriptor del
 * epoll de los timer.
*/
void setTimer(User_data* ud, int timer_epoll) {
	int timer_fd = timerfd_create(CLOCK_MONOTONIC, 0);
  if (timer_fd == -1) {
      perror("timerfd_create");
      exit(EXIT_FAILURE);
  }

  // Configurar el temporizador para expirar después de TIMEOUT_MS milisegundos
  struct itimerspec timer_spec;
  timer_spec.it_interval.tv_sec = 0;
  timer_spec.it_interval.tv_nsec = 0;
  timer_spec.it_value.tv_sec = TIMEOUT_MS / 1000;
  timer_spec.it_value.tv_nsec = (TIMEOUT_MS % 1000) * 1000000;
  if (timerfd_settime(timer_fd, 0, &timer_spec, NULL) == -1) {
      perror("timerfd_settime");
      exit(EXIT_FAILURE);
  }

	// Agregar el temporizador al epoll FD
  struct epoll_event ev;
  ev.events = EPOLLIN;
	Timerfd tfd = 
		allocate_mem(sizeof(struct _Timerfd), NULL);
	tfd->timefd = timer_fd;
	tfd->ud = ud;
  ev.data.ptr = tfd;
  if (epoll_ctl(timer_epoll,
		EPOLL_CTL_ADD, timer_fd, &ev) == -1) {
      perror("epoll_ctl");
      exit(EXIT_FAILURE);
  }
	ud->udBin->tfd = tfd;
}

/**
 * Elimina el timer asociados
 * a los datos de usuarios pasados
 * por argumentos del epoll de los
 * timer.
 * @param epoll file descriptor del
 * epoll de los timer.
 * @param ud datos de usuario a los
 * cuales se le quiere eliminar el
 * timer del epoll
*/
void timerDel(int timer_epoll,
		User_data* ud) {
	epoll_ctl(timer_epoll,
	 	EPOLL_CTL_DEL, 
		ud->udBin->tfd->timefd,
		NULL);
	free(ud->udBin->tfd);
	ud->udBin->tfd = NULL;
}

void* timer_epollStart(void* epoll_timer_fd) {
	int epoll_timer = *((int*)epoll_timer_fd);

	printf("epoll timer configurado\n");

	struct epoll_event events[MAX_EVENTS];
	int num_events;
	while(1) {
		num_events = 
			epoll_wait(epoll_timer,
				events, MAX_EVENTS, -1);
		if (num_events == -1) {
			perror("binepoll_wait");
			exit(EXIT_FAILURE);
		}
		for(int i = 0; i < num_events; i++) {
			Timerfd tfd = 
				(Timerfd)events[i].data.ptr;
			timeOut(epoll_timer, tfd);		
		}
	}
}