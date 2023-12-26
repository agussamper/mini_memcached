#include "cache.h"
#include "codes.h"
#include "list.h"

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <unistd.h>
#include <pthread.h>

typedef struct Stats {
  unsigned long put;
  unsigned long get;
  unsigned long del;
  unsigned long keys;
} Stats;

struct _Cache {
  List *listArr;
  unsigned numElems; //Cantidad de elementos ingresados a la cache 
  unsigned size; //Número de slots para listas
  Evict* evict;
  pthread_mutex_t* mutex_arr;
  unsigned size_mutex_arr; 
  HashFunction hash;
};

Cache cache_create(
    unsigned size,
    HashFunction hash) {

  Cache cache = malloc(sizeof(struct _Cache));
  assert(cache);
  cache->listArr = malloc(sizeof(List)*size);
  assert(cache->listArr);

  evict_init(&(cache->evict));

  long number_of_processors = sysconf(_SC_NPROCESSORS_ONLN);
  long size_mutex_arr = number_of_processors + 1;
  cache->size_mutex_arr = size_mutex_arr;
  cache->mutex_arr = 
    malloc(sizeof(pthread_mutex_t)*size_mutex_arr);

  for(long i = 0; i < size_mutex_arr; i++) {
    pthread_mutex_init((cache->mutex_arr)+i, NULL); 
  }

  cache->numElems = 0;
  cache->size = size;
  cache->hash = hash;
  return cache;
}

int cache_nelems(Cache cache) {
  return cache->numElems;
}

int cache_size(Cache cache) {
  return cache->size;
}

Evict cache_getEvict(Cache cache) {
  return cache->evict;
}

pthread_mutex_t* get_mutex_by_key(
  Cache cache, unsigned idx, unsigned key 
) {  
  unsigned idx_mutex = idx%(cache->size_mutex_arr);
  return cache->mutex_arr+idx_mutex;
}

void cache_insert(Cache cache, 
  char *key, unsigned key_length, 
  char *value, unsigned value_length
) {
  unsigned idx = cache->hash(key) % cache->size;
  pthread_mutex_t* mutex = 
    get_mutex_by_key(cache, idx, key);
  List* list = cache->listArr+idx;
  pthread_mutex_lock(mutex);
  if(cache->listArr[idx] == NULL) {
    cache->listArr[idx] = list_create();
  }
  int res = list_add(list,
    key, key_length, value, value_length);
  if(0 == res) {
    return NOMEM;
  }
  res = evict_add(cache->evict, *list);
  if(0 == res) {
    list_remove(*list);
    return NOMEM;
  }
  pthread_mutex_unlock(mutex);
}

void cache_delete(Cache cache, char* key) {
  unsigned idx = cache->hash(key) % cache->size;
  pthread_mutex_t* mutex = 
    get_mutex_by_key(cache, idx, key);
  List* list = cache->listArr+idx;
  pthread_mutex_lock(mutex);
  if(list_empty(*list)) {
    pthread_mutex_unlock(mutex);
    return;  
  }
  List ptr = list_getByKey(list, key);
  if(!ptr) {
    pthread_mutex_unlock(mutex);
    return;
  }
  evict_remove(cache->evict, ptr);
  list_remove(ptr);
  pthread_mutex_unlock(mutex);
}