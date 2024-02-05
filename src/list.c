#include "list.h"
#include "malloc_interface.h"
#include "arr_func.h"

#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

typedef struct Node {
  char* key;
  char* value;
  uint32_t lenKey;
  uint32_t lenVal; 
  int isBin; 
  struct Node* next;
  struct Node* prev;    
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
int isInList(List* list, char *key,
    uint32_t klen) {
  Node* node = *list;
  while(node != NULL) {
    if(0 == arrcmp(node->key,
        node->lenKey, key, klen)) {
      *list = node;
      return 1;
    }
    node = node->next;
  }
  return 0;
}

int list_add(List* list,
    char* key, unsigned klen,
    char* value, unsigned vlen,
    int isBin,
    pthread_mutex_t* listMutex) {
  assert(list != NULL);  
  char* newValue = allocate_mem(
    sizeof(char)*vlen, listMutex);
  List node = *list;
  if(!newValue) {
    return 0;
  }
  arrcpy(newValue, value, vlen);
  if(isInList(&node, key, klen)) {
    free(node->value);    
    node->value = newValue;
    node->lenVal = vlen;
    node->isBin = isBin;
    Node *top = *list;
    if(top == node) {
      return 2;
    }
    if(node->next != NULL) {
      node->prev->next = node->next;
      node->next->prev = node->prev;
      top->prev = node;
      node->next = top;
      node->prev = NULL;
    } else {
      node->prev->next = NULL;
      top->prev = node;
      node->next = top;
      node->prev = NULL;
    }
    *list = node;
    return 2;
  }

  Node* newNode = allocate_mem(sizeof(Node), listMutex);
  char* newKey = allocate_mem(sizeof(char)*klen, listMutex);
  if(!newNode || !newKey) {
    return 0;
  }
  Node* top = *list;
  newNode->next = top;
  if(NULL != top) {
    top->prev = newNode;
  }
  newNode->prev = NULL;
  
  arrcpy(newKey, key, klen);
  newNode->key = newKey;
  newNode->value = newValue;
  newNode->lenVal = vlen;
  newNode->isBin = isBin;

  *list = newNode;
  return 1;
}

int list_remove_key(List* list,
    char* key, uint32_t keyLen) {
  Node* node = *list;
  if(node == NULL) {
    return 0;
  }
  for(; arrcmp(node->key, 
      node->lenKey, key, keyLen) != 0
    && node != NULL;
      node = node->next);
  
  if(!node) {
    return 0;
  }

  free(node->key);
  free(node->value);
  free(node->evict);
  if(node->prev != NULL) {
    node->prev->next = node->next;
  }
  if(node->next != NULL) {
    node->next->prev = node->prev;  
  }
  if(*list == node) {
    *list = (*list)->next;
  }
  free(node);
  return 1;
}

void list_remove_node(List* list, List lNode) {
  if(!list || !lNode) {
    return;
  }
  Node* node = lNode;
  if(node->prev != NULL) {
    node->prev->next = node->next;
  }
  if(node->next != NULL) {
    node->next->prev = node->prev;
  }
  free(node->key);
  free(node->value);
  free(node->evict);
  if(*list == lNode) {
    *list = lNode->next;
  }
  free(node);
}

ValData* list_getValue(List* list,
    char* key,
    uint32_t lenK,
    pthread_mutex_t* listMutex) {
  Node* node = *list;
  for(; node != NULL; node = node->next) {
    if(0 == arrcmp(key, lenK,
        node->key, node->lenKey)) {
      uint32_t lenVal = node->lenVal;
      char val[lenVal];
      arrcpy(val, node->value, lenVal);
      int isBin = node->isBin;      
      ValData* toReturn =
        allocate_mem(
          sizeof(ValData), listMutex);
      char* valCpy =
        allocate_mem(
          sizeof(char)*lenVal,
          listMutex);
        arrcpy(valCpy, val, lenVal);
        toReturn->isBin = isBin;
        toReturn->valSize = lenVal;
        toReturn->value = valCpy;
      return toReturn;
    }
  }
  return NULL;
}

List list_getByKey(List* list,
    char* key, uint32_t keyLen) {
  Node* node = *list;  
  for(; node != NULL; node = node->next) {    
    if(0 == arrcmp(key, keyLen,
        node->key, node->lenKey)) {      
      return node;
    }
  }
  return NULL;
}

NodeEvict list_getEvictNode(List list) {
  return list->evict;
}

void list_setEvictNode(List list, NodeEvict node) {
  list->evict = node;
}

char* list_getKey(List list) {
  return list->key;
}