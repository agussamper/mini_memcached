//! @file

#ifndef __TEXT_MANAGE_H__
#define __TEXT_MANAGE_H__

#include <sys/epoll.h>
#include "cache.h"
#include "user_data.h"

/**
 * Lee hasta llenar el buffer, atiende
 * todos los pedidos encontrados, deja en el 
 * buffer los caracteres correspondientes
 * a un pedido incompleto que será
 * atendido en alguna proxima llamada, 
 * en offset la longitud de este pedido incompleto.
 * 
 * Si el cliente se desconecta o por algun motivo 
 * debemos cerrar la conexión retornamos -1.
 * En caso contrario retornamos 0.
 * 
 * @param cache sobre la cual se realizan peticiones.
 * @param fd file descriptor con el cual nos
 * comunicamos con el cliente.
 * @param buf buffer donde guardamos lo leido
 * @param offset puntero en donde guardamos hasta
 * donde leimos en el buffer.
 * @return 
 * 0: todo ok.
 * -1: errores criticos o desconección del cliente.
 */
int text_consume(Cache cache,
  User_data* ud);

#endif