#include "list.h"
#include "cache.h"
#include "codes.h"

#include <evict.h>
#include <pthread.h>
#include <stdlib.h>
#include <assert.h>
#include <malloc_interface.h>

struct _NodeEvict {
  struct _NodeEvict* next;
  struct _NodeEvict* prev;
  List list_ptr;
};

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

int evict_add(Evict evict, List list) {
  NodeEvict node = 
    allocate_mem(sizeof(struct _NodeEvict));
  if(!node) {
    return NOMEM;
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

/*void evict_remove(Evict evict, List list) {
  pthread_mutex_lock(&evict->mutex);
  if(evict->mru == NULL) {
    pthread_mutex_unlock(&evict->mutex);
    return;
  }
  if(evict->mru == evict->lru) {
    free(evict->mru);
    free(evict->lru);
    evict->mru = NULL;
    evict->lru = NULL;
  } else {
    if(evict->mru->list_ptr == list) {
      evict->mru = evict->mru->prevEntry;
      evict->lru->prevEntry = evict->mru; 
    }
    else if(evict->lru == node) {
      evict->lru = evict->lru->nextEntry;
      evict->mru->nextEntry = evict->lru;
    }
    node->prevEntry->nextEntry = node->nextEntry;
    node->nextEntry->prevEntry = node->prevEntry;    
  }
  pthread_mutex_unlock(&evict->mutex);  
}*/

void evict_remove(Evict evict, List list) {
  pthread_mutex_lock(&evict->mutex);
  if(evict->mru == NULL) {
    pthread_mutex_unlock(&evict->mutex);
    return;
  }
  if(evict->mru == evict->lru) {
    free(evict->mru);
    free(evict->lru);
    evict->mru = NULL;
    evict->lru = NULL;
  } else {
    if(evict->mru->list_ptr == list) {
      evict->mru = evict->mru->prev;
      evict->lru->prev = evict->mru; 
    }
    else if(evict->lru == node) {
      evict->lru = evict->lru->nextEntry;
      evict->mru->nextEntry = evict->lru;
    }
  }
  pthread_mutex_unlock(&evict->mutex);  
}