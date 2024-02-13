#include "cache.h"
#include "list.h"

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <unistd.h>
#include <pthread.h>
#include <string.h>

struct _Cache {
  List *listArr;
  unsigned size; //Número de slots para listas
  Evict evict;
  pthread_mutex_t* mutex_arr;
  unsigned size_mutex_arr; 
  HashFunction hash;
  Stats stats;
};

Cache cache_create(
    unsigned size,
    HashFunction hash) {

  Cache cache = malloc(sizeof(struct _Cache));
  assert(cache);
  cache->listArr = malloc(sizeof(List)*size);
  assert(cache->listArr);
  for(long i = 0; i < size; i++) {
    cache->listArr[i] = NULL; 
  }

  evict_init(&(cache->evict));
  assert(cache->evict);

  long number_of_processors =
    sysconf(_SC_NPROCESSORS_ONLN);
  long size_mutex_arr = number_of_processors + 1;
  cache->size_mutex_arr = size_mutex_arr;
  cache->mutex_arr = 
    malloc(sizeof(pthread_mutex_t)*size_mutex_arr);

  for(long i = 0; i < size_mutex_arr; i++) {
    pthread_mutex_init(cache->mutex_arr+i, NULL); 
  }

  cache->size = size;
  cache->hash = hash;

  cache->stats = stats_init();

  return cache;
}

void cache_destroy(Cache cache) {
  List* listArr = cache->listArr;
  for(int i = 0; i < cache->size; i++) {
    list_destroy(listArr[i]); 
  }
  free(cache->listArr);
  evict_destroy(cache->evict);

  pthread_mutex_t* mutex_arr = cache->mutex_arr;
  for(int i = 0; i < cache->size_mutex_arr; i++) {
    pthread_mutex_destroy(mutex_arr+i);
  }
  free(mutex_arr);
  stats_destroy(cache->stats);
  free(cache);
}

int cache_size(Cache cache) {
  return cache->size;
}

/**
 * Devuelve el indice de la cache
 * correspondiente a key.
*/
unsigned get_idx(Cache cache, char* key, uint32_t len) {
  return cache->hash(key,len) % cache->size;
}

/**
 * Devuelve el mutex de la cache
 * correspondiente a idx.
*/
pthread_mutex_t* get_mutex_by_idx(
  Cache cache, unsigned idx) {  
  unsigned idx_mutex = idx%(cache->size_mutex_arr);
  return cache->mutex_arr+idx_mutex;
}

int cache_insert(Cache cache, 
  char *key, unsigned key_length, 
  char *value, unsigned value_length,
  int isBin
) {
  stats_putsInc(cache->stats);
  unsigned idx = get_idx(cache, key,key_length);
  pthread_mutex_t* mutex = 
    get_mutex_by_idx(cache, idx);
  List* list = cache->listArr+idx;   
  pthread_mutex_lock(mutex);
  int res = list_add(list,
    key, key_length, value,
    value_length, isBin, mutex);  
  switch (res) {
  case 0:
    pthread_mutex_unlock(mutex);
    return 0;
    break;
  case 1:
    res = evict_add(cache->evict, *list,
      idx, mutex);
    if(0 == res) {
      list_remove_node(list, *list);
      pthread_mutex_unlock(mutex);
      return 0;
    }
    stats_keysInc(cache->stats);
    break;
  default:
    evict_update(cache->evict, *list);
    break;
  }
  pthread_mutex_unlock(mutex);
  return 1;
}

ValData* cache_get(Cache cache, char* key,
    uint32_t keyLen) {
  stats_getsInc(cache->stats);
  unsigned idx = get_idx(cache, key, keyLen);
  pthread_mutex_t* mutex = 
    get_mutex_by_idx(cache, idx);  
  List* list = cache->listArr+idx;
  pthread_mutex_lock(mutex);
  ValData* valData = 
    list_getValue(list, key, keyLen, mutex);
  if(!valData) {
    pthread_mutex_unlock(mutex);
    return NULL;
  }
  
  //Actualizo evict e intento volver a agregar
  //el elemento por si fué desalojado por
  //allocate_mem en list_getValue
  int res = list_add(list,
    key, keyLen, valData->value,
    valData->valSize, valData->isBin,
    mutex);  
  assert(res != 0); //res no debería ser 0
  switch (res) {
  case 1:
    res = evict_add(cache->evict, *list,
      idx, mutex);
    if(0 == res) {
      list_remove_node(list, *list);
      pthread_mutex_unlock(mutex);
      return valData;
    }
    stats_keysInc(cache->stats);
    break;
  default:
    evict_update(cache->evict, *list);
    break;
  }
  
  pthread_mutex_unlock(mutex);
  return valData;
}

int cache_delete(Cache cache, char* key,
    uint32_t keyLen) {
  stats_delsInc(cache->stats);
  unsigned idx = get_idx(cache, key, keyLen);
  pthread_mutex_t* mutex = 
    get_mutex_by_idx(cache, idx);
  List* list = cache->listArr+idx;
  pthread_mutex_lock(mutex);
  if(list_empty(*list)) {
    pthread_mutex_unlock(mutex);
    return 0;  
  }
  List ptr = list_getByKey(list, key,
    keyLen);
  if(!ptr) {
    pthread_mutex_unlock(mutex);
    return 0;
  }
  evict_remove(cache->evict, ptr);
  list_remove_node(list, ptr);
  stats_keysDec(cache->stats);
  pthread_mutex_unlock(mutex);
  return 1;
}

int cache_evict(Cache cache, pthread_mutex_t* listMutex) {
  Evict evict = cache->evict;
  evict_lock(evict);
  NodeEvict nodeEvict = evict_getLru(evict);
  for(
    int i = 0;
    i < 10 && !evict_empty(evict);
    i++
  ) {    
    List list = evict_getList(nodeEvict);
    unsigned idx = evict_getListIdx(nodeEvict);
    pthread_mutex_t* mutex = 
      get_mutex_by_idx(cache, idx);
    if(mutex != listMutex && 0 != pthread_mutex_trylock(mutex)) {
      nodeEvict = evict_getNextNode(nodeEvict);
      continue;
    }
    NodeEvict next = evict_getNextNode(nodeEvict);
    evict_removeNode(evict, nodeEvict);
    list_remove_node(cache->listArr+idx, list);
    stats_keysDec(cache->stats);
    if(mutex != listMutex) {
      pthread_mutex_unlock(mutex);
    }
    nodeEvict = next;
  }
  int res = evict_empty(evict);
  evict_unlock(evict);
  return res;
}

char* cache_getStats(Cache cache) {
  return stats_getStats(cache->stats, NULL);
}

int cache_empty(Cache cache) {
  return evict_empty(cache->evict);
}