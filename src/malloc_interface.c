#include "malloc_interface.h"
#include "cache.h"
#include "memcached.h"

#include <stdlib.h>

extern Cache cache;

void* allocate_mem(size_t size) {
  void* ptr = malloc(size);
  while(!ptr && !cache_empty(cache)) {  
    cache_evict(cache);
    ptr = malloc(size);
  }
  return ptr;
}