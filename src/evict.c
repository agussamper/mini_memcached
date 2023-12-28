#include "list.h"
#include "cache.h"

#include <evict.h>
#include <pthread.h>
#include <stdlib.h>
#include <assert.h>
#include <malloc_interface.h>
#include <assert.h>

struct _NodeEvict {
  struct _NodeEvict* next;
  struct _NodeEvict* prev;
  List list_ptr;
};

/**
 * Si desde lru hacemos next vamos al siguiente
 * elemento más recientemente usado
 * Si desde mru hacemos prev vamos al previo
 * elemento menos recientemente usado
 * Como la estructura es circular, si desde mru
 * hacemos next tenemos lru y si desde lru
 * hacemos prev tenemos mru
*/
struct _Evict {
  NodeEvict mru;
  NodeEvict lru;
  pthread_mutex_t mutex;
};

void evict_init(Evict* evict_ptr) {
  Evict evict = *evict_ptr;
  evict = malloc(sizeof(struct _Evict));
  assert(evict);
  evict->mru = NULL;
  evict->lru = NULL;
  assert(!pthread_mutex_init(&(evict->mutex), NULL));
}

int evict_add(Evict evict, const List list) {
  NodeEvict node = 
    allocate_mem(sizeof(struct _NodeEvict));
  if(!node) {
    return 0;
  }
  node->list_ptr = list;
  pthread_mutex_lock(&evict->mutex);
  if(NULL == evict->lru) {
    node->next = node;
    node->prev = node;
    evict->lru = node;
    evict->mru = node;
  } else if(evict->lru->next == evict->lru) {
    evict->lru->next = node;
    evict->lru->prev = node;
    node->next = evict->lru;
    node->prev = evict->lru;
    evict->mru = node;
  } else {
    node->next = evict->lru;
    node->prev = evict->mru;
    evict->mru->next = node;
    evict->lru->prev = node;
    evict->mru = node;
  } 
  pthread_mutex_unlock(&evict->mutex);
  return 1;
}

void evict_update(Evict evict, List list) {
  assert(list);
  NodeEvict node = list_getEvictNode(list);
  assert(node);
  pthread_mutex_lock(&evict->mutex);  
  if(node == evict->mru) {
    pthread_mutex_unlock(&evict->mutex);
    return;
  }
  node->prev->next = node->next;
  node->next->prev = node->prev;
  node->prev = evict->mru;
  node->next = evict->lru;
  evict->mru->next = node;
  evict->lru->prev = node;
  evict->mru = node;
  pthread_mutex_unlock(&evict->mutex);
}

void evict_remove(Evict evict, const List list) {
  pthread_mutex_lock(&evict->mutex);
  if(evict->mru == NULL) {
    pthread_mutex_unlock(&evict->mutex);
    return;
  }
  if(evict->mru == evict->lru) {
    evict->mru = NULL;
    evict->lru = NULL;
  } else {
    NodeEvict node = list_getEvictNode(list);
    node->prev->next = node->next;
    node->next->prev = node->prev;
  }
  pthread_mutex_unlock(&evict->mutex);
}

void evict_dismiss(Cache cache, Evict evict) {
  pthread_mutex_lock(&evict->mutex);
  NodeEvict node = evict->lru;
  for(int i = 0; node && i < 10;
      node = node->next, i++) {
    pthread_mutex_t* mutex = 
      cache_trylock(cache, node->list_ptr);
    if(!mutex) {
      continue;
    }
    if(evict->mru == evict->lru) {
      evict->mru = NULL;
      evict->lru = NULL;
    } else {
      node->prev->next = node->next;
      node->next->prev = node->prev;
    }
    list_remove(node->list_ptr);
    stats_keysDec(cache_getStats(cache));
    pthread_mutex_unlock(mutex);
  }
  pthread_mutex_unlock(&evict->mutex);
}