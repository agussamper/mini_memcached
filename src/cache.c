#include "cache.h"
#include "list.h"

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <unistd.h>
#include <pthread.h>

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

  evict_init(&(cache->evict));
  assert(cache->evict);

  long number_of_processors =
    sysconf(_SC_NPROCESSORS_ONLN);
  long size_mutex_arr = number_of_processors + 1;
  cache->size_mutex_arr = size_mutex_arr;
  cache->mutex_arr = 
    malloc(sizeof(pthread_mutex_t)*size_mutex_arr);

  for(long i = 0; i < size_mutex_arr; i++) {
    pthread_mutex_init((cache->mutex_arr)+i, NULL); 
  }

  cache->size = size;
  cache->hash = hash;

  cache->stats = stats_init();

  return cache;
}

int cache_size(Cache cache) {
  return cache->size;
}

/**
 * Devuelve el indice de la cache
 * correspondiente a key.
*/
unsigned get_idx(Cache cache, char* key) {
  return cache->hash(key) % cache->size;
}

/**
 * Devuelve el mutex de la cache
 * correspondiente a idx.
*/
pthread_mutex_t* get_mutex_by_key(
  Cache cache, unsigned idx) {  
  unsigned idx_mutex = idx%(cache->size_mutex_arr);
  return cache->mutex_arr+idx_mutex;
}

int cache_insert(Cache cache, 
  char *key, unsigned key_length, 
  char *value, unsigned value_length
) {
  stats_putsInc(cache->stats);
  unsigned idx = get_idx(cache, key);
  pthread_mutex_t* mutex = 
    get_mutex_by_key(cache, idx);
  List* list = cache->listArr+idx;
  pthread_mutex_lock(mutex);
  if(cache->listArr[idx] == NULL) {
    cache->listArr[idx] = list_create();
  }
  int res = list_add(list,
    key, key_length, value, value_length);
  switch (res) {
  case 0:
    pthread_mutex_unlock(mutex);
    return 0;
    break;
  case 1:
    res = evict_add(cache->evict, *list);
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
}

char* cache_get(Cache cache, char* key) {
  stats_getsInc(cache->stats);
  unsigned idx = get_idx(cache, key);
  pthread_mutex_t* mutex = 
    get_mutex_by_key(cache, idx);
  List* list = cache->listArr+idx;
  pthread_mutex_lock(mutex);
  char* value = list_getValue(list, key);
  pthread_mutex_unlock(mutex);
  return value;
}

int cache_delete(Cache cache, char* key) {
  stats_delsInc(cache->stats);
  unsigned idx = get_idx(cache, key);
  pthread_mutex_t* mutex = 
    get_mutex_by_key(cache, idx);
  List* list = cache->listArr+idx;
  pthread_mutex_lock(mutex);
  if(list_empty(*list)) {
    pthread_mutex_unlock(mutex);
    return 0;  
  }
  List ptr = list_getByKey(list, key);
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

void cache_evict(Cache cache) {
  Evict evict = cache->evict;
  evict_lock(evict);
  for(
    int i = 0;
    i < 10 && !evict_empty(evict);
    i++
  ) {
    NodeEvict nodeEvict = evict_getLru(evict);
    List list = evict_getList(nodeEvict);
    
    char* key = list_getKey(list);
    unsigned idx = get_idx(cache, key);
    pthread_mutex_t* mutex = 
      get_mutex_by_key(cache, idx);
    if(0 != pthread_mutex_trylock(mutex)) {
      continue;
    }
    evict_removeLru(evict);
    list_remove_node(cache->listArr+idx, list);
    stats_keysDec(cache->stats);
    pthread_mutex_unlock(mutex);
  }
  evict_unlock(evict);
}

int cache_empty(Cache cache) {
  return stats_getKeys(cache->stats) == 0;
}