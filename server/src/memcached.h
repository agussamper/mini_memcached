//! @file

#ifndef __MEMCACHED_H__
#define __MEMCACHED_H__

#include "cache.h"

/**
 * Inicia la memcached, espera consultas y las
 * responde
 * @param tsock Socket de escucha para modo texto
 * @param bsock Socket de escucha para modo binario
*/
void memcached_start(int tsock,int bsock);

#endif