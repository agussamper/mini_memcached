#ifndef __BIN_MANAGE_H__
#define __BIN_MANAGE_H__

#include <stdio.h>
#include "common.h"
#include "cache.h"
#include "user_data.h"

void bin_consume(Cache cache , char* buf, int fd);

/**
 * Retorna menos uno si hubo un error o el
 * usuario se desconecto, 0 si leyó 
 * correctamente y 1 si todavía le falta
 * leer parte del paquete
*/
int readBin(User_data* ud);

#endif