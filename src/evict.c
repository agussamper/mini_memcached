#include "evict.h"
#include "malloc_interface.h"

#include <pthread.h>
#include <stdlib.h>
#include <assert.h>
#include <assert.h>

struct _NodeEvict {
  struct _NodeEvict* next;
  struct _NodeEvict* prev;
  List* list;
  List lNode;
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
  *evict_ptr = malloc(sizeof(struct _Evict));
  assert(*evict_ptr);
  (*evict_ptr)->mru = NULL;
  (*evict_ptr)->lru = NULL;
  assert(!pthread_mutex_init(&((*evict_ptr)->mutex), NULL));
}

int evict_add(Evict evict, List* list, List lnode) {
  assert(evict);
  NodeEvict node = 
    allocate_mem(sizeof(struct _NodeEvict));
  if(!node) {
    return 0;
  }
  node->list = list;
  node->lNode = lnode;
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
  list_setEvictNode(lnode, node); 
  pthread_mutex_unlock(&evict->mutex);
  return 1;
}

void evict_update(Evict evict, const List list) {
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

void evict_removeLru(Evict evict) {  
  if(evict->mru == evict->lru) {
      evict->mru = NULL;
      evict->lru = NULL;
  } else {
    NodeEvict node = evict->lru;
    node->prev->next = node->next;
    node->next->prev = node->prev;
  }
}

void evict_lock(Evict evict) {
  pthread_mutex_lock(&evict->mutex);
}

void evict_unlock(Evict evict) {
  pthread_mutex_unlock(&evict->mutex);
}

int evict_empty(Evict evict) {
  return evict->lru == NULL ? 1 : 0;
}

List* evict_getList(NodeEvict nEvict) {
  return nEvict->list;
}

List evict_getLNode(NodeEvict nEvict) {
  return nEvict->lNode;
}

NodeEvict evict_getLru(Evict evict) {
  return evict->lru;
}