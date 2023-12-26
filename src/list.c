#include "list.h"
#include "evict.h"

#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

typedef struct Node {
  char* key;
  char* value;
  struct Node* next;
  struct Node* prev; //TODO: hacer cambios para que funcione cone este nodo

  /**
   * Puntero al nodo que apunta a este nodo
   * en la estructura evict  
  */
  NodeEvict evict;
} Node; 

List list_create() {
  return NULL;
}

void list_destroy(List list) {
  Node* top = list;
  Node* toDelete;
  while (top != NULL) {
    toDelete = top;
    top = top->next;
    free(toDelete->key);
    free(toDelete->value);            
    free(toDelete);
  }
  list = NULL;
}

int list_empty(List list) {
  return list == NULL ? 1 : 0;
}

/**
 * Retorna 1 si el elemento si key está
 * en la lista y 0 en caso contrario.
 * Si key está en la lista list va a
 * apuntar al nodo de la lista donde
 * se encuentra key
*/
int isInList(List* list, char *key) {
  Node* node = *list;
  while(node != NULL) {
    if(0 == strcmp(node->key, key)) {
      *list = node;
      return 1;
    }
    node = node->next;
  }
  return 0;
}

int list_add(List* list,
    char* key, unsigned klen,
    char* value, unsigned vlen) {
  assert(list != NULL);

  Node* node = *list;
  if(isInList(&node, key)) {
    void* newValue = cpyV(value);
    if(NULL == newValue) {
      return 0;  
    }
    destroyValue(node->value);
    node->value = newValue;
    return 1;
  }

  Node* newNode = malloc(sizeof(Node));
  if(newNode == NULL) {
    return 0;
  }
  Node* top = *list;
  newNode->next = top; 
  
  void* newKey = cpyK(key);
  if(NULL == newKey) {
    return 0;
  }
  void* newValue = cpyV(value);
  if(NULL == newValue) {
    return 0;  
  }
  newNode->key = newKey;
  newNode->value = newValue;

  *list = newNode;
  return 1;
}

/**
 * Remueve node de la estructura de desalojo
*/
void evict_remove(Evict* evict, Node* node) {
  pthread_mutex_lock(&evict->mutex_evict);
  if(evict->newest == evict->oldest) {
      evict->newest = NULL;
      evict->oldest = NULL;
  } else {
    if(evict->newest == node) {
      evict->newest = evict->newest->prevEntry;
      evict->oldest->prevEntry = evict->newest; 
    }
    else if(evict->oldest == node) {
      evict->oldest = evict->oldest->nextEntry;
      evict->newest->nextEntry = evict->oldest;
    }
    node->prevEntry->nextEntry = node->nextEntry;
    node->nextEntry->prevEntry = node->prevEntry;    
  }
  pthread_mutex_unlock(&evict->mutex_evict);
}

int list_remove_at(List* list,
    char* key) {
  Node* node = *list;
  if(node == NULL) {
    return 0;
  }
  if(0 == compareK(node->key, key)) {
    destK(node->key);
    destV(node->value);    
    evict_remove(evict, node);
    *list = node->next;
    free(node);    
    return 1;
  }
  while(node->next != NULL) {
    if(0 == compareK(node->next->key, key)) {
      destK(node->next->key);
      destV(node->next->value);
      evict_remove(evict, node->next);
      Node* toDelete = node->next;
      node->next = node->next->next;
      free(toDelete);            
      return 1;
    }
    node = node->next;
  }
  if(0 == compareK(node->key, key)) {
    destK(node->key);
    destV(node->value);
    evict_remove(evict, node);
    free(node);
    return 1;
  }
  return 0;
}

void *list_getValue(List* list,
    char* key) {
  Node* node = *list;
  while(node != NULL) {
    if(0 == cmpK(key, node->key)) {
      void* val = cpyValue(node->value);
      evict_update(evict, node);
      return val;
    }
    node = node->next;
  }
  return NULL;
}