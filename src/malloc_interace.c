#include "malloc_interface.h"
#include "cache.h"
#include "memcached.h"
#include "evict.h"

#include <stdlib.h>

extern Cache cache;

void* allocate_mem(size_t size) {
  void* ptr = malloc(size);
  Evict evict = cache_getEvict(cache);
  while(!ptr && !evict_empty(evict)) {  
    evict_dismiss(cache, evict); //TODO: terminar
    ptr = malloc(size);
  }
  return ptr;
}