//! @file

#ifndef __TIMER_H__
#define __TIMER_H__

#include "user_data.h"

/**
 * Reinicia los datos del usuario
 * asociados a tfd, elimina tfd del
 * epoll del timer y manda al cliente
 * EINVALID
 * @param timer_epoll file descriptor de
 * epoll para los temporizadores.
 * @param tfd file descriptor de timerfd. 
*/
void timeOut(int timer_epoll,
	 Timerfd tfd);

/**
 * Configura el timer en TIMEOUT_MS,
 * lo asocia con ud y lo agrega al epoll.
 * @param ud datos de usuario a los que
 * estará asociado el timer.
 * @param timer_epoll file descriptor del
 * epoll de los timer.
*/
void setTimer(User_data* ud, int timer_epoll);

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
		User_data* ud);

/**
 * Escucha los eventos del epoll de los timer,
 * cuando ocurre un evento reiniciará los datos
 * de usuario asociados y eliminará el file
 * descriptor del evento ocurrido de la
 * lista de eventos que debe notificar el
 * epoll, además liberará su memoria
 * @param epoll_timer_fd puntero a file
 * descriptor del epoll de los timer. 
 * */ 
void* timer_epollStart(void* epoll_timer_fd);

#endif