//! @file

#ifndef __BIN_MANAGE_H__
#define __BIN_MANAGE_H__

#include <stdio.h>
#include "common.h"
#include "cache.h"
#include "user_data.h"

/**
 * Responde la consulta del cliente. 
 * Escribe la respuesta en fd.
 * @param cache cache a la cual modificar/consultar.
 * @param buf buffer con la consulta.
 * @param fd file descritor al cual escribir
 * la respuesta.
 * 
*/
void bin_consume(Cache cache , char* buf, int fd);

/**
 * Lee lo que llegó al socket y lo guarda en
 * ud->buf
 * @param ud Datos del usuario
 * @return
 * Retorna menos uno si hubo un error o el
 * usuario se desconecto, 0 si leyó 
 * correctamente y 1 si todavía le falta
 * leer parte del paquete
*/
int readBin(User_data* ud);

#endif