#include "malloc_interface.h"
#include "cache.h"
#include "memcached.h"

#include <stdlib.h>
#include <stdio.h>

extern Cache memcache;

void* allocate_mem(size_t size, pthread_mutex_t* listMutex) {
  void* ptr = malloc(size);
  int cacheEmpty = cache_empty(memcache);
  while(!ptr && !cacheEmpty) {   
    cacheEmpty = cache_evict(memcache, listMutex);
    ptr = malloc(size);
  }
  return ptr;
}