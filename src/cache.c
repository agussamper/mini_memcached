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
  unsigned numElems;
  unsigned size;
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

void cache_insert(Cache table, 
    char *key, unsigned key_length, 
    char *value, unsigned value_length) {

  unsigned idx = table->hash(key) % table->size;
  unsigned idx_mutex = idx%(table->size_mutex_arr);
  List* listSelected = table->listArr+idx;
  pthread_mutex_lock(table->mutex_arr+idx_mutex);
  if(table->listArr[idx] == NULL) {
    table->listArr[idx] = list_create();
  }
  int res = list_add(listSelected, key, value); //TODO: hacer que devuela el nodo donde se guardo en la lista
  if(0 == res) {
    return NOMEM;
  }
  res = evict_add(table->evict);
  if(0 == res) {
    list_remove(listSelected);
    return NOMEM;
  }
  pthread_mutex_unlock(table->mutex_arr+idx_mutex);
}
/*
//TODO: Modificar version cuando se hace find
void* hashtable_find(HashTable table, void *key) {
  unsigned idx = table->hash(key) % table->size;
  if(NULL != table->avl_arr[idx]) {
    return avl_buscar(table->avl_arr[idx],
      key, table->funcs); 
  }
  return NULL;
}

void hashtable_delete(HashTable table, void *key) {
  unsigned idx = table->hash(key) % table->size;
  if(NULL != table->avl_arr[idx]) {
    avl_eliminar((table->avl_arr[idx]), key,
      table->funcs);
  }
}*/