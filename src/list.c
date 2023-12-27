#include "list.h"
#include "evict.h"
#include "malloc_interface.h"
#include "codes.h"

#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

typedef struct Node {
  char* key;
  char* value;
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
  char* newValue = allocate_mem(sizeof(char)*vlen);
  if(!newValue) {
    return NOMEM;
  }
  strcpy(newValue, value);
  if(isInList(&node, key)) {
    free(node->value);    
    node->value = newValue;
    node->prev->next = node->next;
    node->next->prev = node->prev;
    Node *top = list;
    top->prev = node;
    node->next = top;
    node->prev = NULL;
    return 2;
  }

  Node* newNode = allocate_mem(sizeof(Node));
  char* newKey = allocate_mem(sizeof(char)*vlen);
  if(!newNode || !newKey) {
    return 0;
  }
  Node* top = *list;
  newNode->next = top;
  top->prev = newNode;
  newNode->prev = NULL;
  
  strcpy(newKey, key);
  newNode->key = newKey;
  newNode->value = newValue;

  *list = newNode;
  return 1;
}

int list_remove_key(List* list,
    char* key) {
  Node* node = *list;
  if(node == NULL) {
    return 0;
  }
  for(; strcmp(node->key, key) != 0 && node != NULL;
      node = node->next);
  
  if(!node) {
    return 0;
  }

  free(node->key);
  free(node->value);
  free(node->evict);
  node->prev->next = node->next;
  node->next->prev = node->prev;
  free(node);

  return 1;
}

void list_remove(List list) {
  if(!list) {
    return;
  }
  Node* node = list;
  if(node->prev != NULL) {
    node->prev->next = node->next;
  }
  if(node->next != NULL) {
    node->next->prev = node->prev;
  }
  free(node->key);
  free(node->value);
  free(node->evict);
}

void* list_getValue(List* list,
    char* key) {
  Node* node = *list;
  for(; node != NULL; node->next) {
    if(0 == strcmp(key, node->key)) {
      char* val =
        allocate_mem(strlen(key)*sizeof(char));
      return val;
    }
  }
  return NULL;
}

List list_getByKey(List* list,
    char* key) {
  Node* node = *list;
  for(; node != NULL; node->next) {
    if(0 == strcmp(key, node->key)) {      
      return node;
    }
  }
  return NULL;
}

NodeEvict list_getEvictNode(List list) {
  return list->evict;
}