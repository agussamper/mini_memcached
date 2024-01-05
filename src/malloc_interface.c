#include "malloc_interface.h"
#include "cache.h"
#include "memcached.h"

#include <stdlib.h>
#include <stdio.h>

extern Cache cache;

void* allocate_mem(size_t size) {
  void* ptr = malloc(size); 
  //TODO: ver por que hay solo un
  //elemento en evict cuando ocurre el error
  while(!ptr && !cache_empty(cache)) {  
    //puts("nomem");
    //TODO: notar que esto no puede dejar la cache vacia
    //ya que no podra eliminar los elementos que
    //pertenezcan a una lista que tenga tomado el mutex 
    cache_evict(cache);
    ptr = malloc(size);
    //if(ptr)
    //  puts("encontre memoria");
  }
  return ptr;
}