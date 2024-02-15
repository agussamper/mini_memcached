//! @file

#ifndef __MEMCACHED_H__
#define __MEMCACHED_H__

#include "cache.h"

/**
 * Inicia la memcached, espera consultas y las
 * responde
 * @param tsock
 * @param bsock
*/
int memcached_cache_start(int tsock,int bsock);

#endif