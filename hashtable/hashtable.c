#include "hashtable.h"
#include "avl/avl.h"
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

struct _HashTable {
  AVL *avl_arr;
  unsigned numElems;
  unsigned size;
  Cpy_key cpyK;
  Cpy_value cpyV;
  Compare_key compK;
  Destroy_key destK;
  Destroy_value destV;
  HashFunction hash;
};

HashTable hashtable_create(
    unsigned size,
    Cpy_key cpyK,
    Cpy_value cpyV,
    Compare_key compK,
    Destroy_key destK,
    Destroy_value destV,
    HashFunction hash) {

  HashTable table = malloc(sizeof(struct _HashTable));
  assert(table);
  table->avl_arr = malloc(sizeof(AVL)*size);
  assert(table->avl_arr);
  for(int i = 0; i < size; i++) {
    table->avl_arr[i] = NULL;
  }
  table->numElems = 0;
  table->size = size;
  table->cpyK = cpyK;
  table->cpyV = cpyV;
  table->compK = compK;
  table->destK = destK;
  table->destV = destV;
  table->hash = hash;
  return table;
}

int hashtable_nelems(HashTable table) {
  return table->numElems;
}

int hashtable_size(HashTable table) {
  return table->size;
}

void hashtable_destroy(HashTable table) {
  for(int i = 0 ; i < table->size; i++) {
    if(NULL != table->avl_arr[i]) {
      avl_destruir(table->avl_arr[i],
        table->destK, table->destV);
    }
  }
  free(table->avl_arr);
  free(table);
  return;
}

void hashtable_insert(HashTable table, void *key,
    void *value) {

  unsigned idx = table->hash(key) % table->size;
  if(NULL == table->avl_arr[idx]) {
    table->avl_arr[idx] = avl_crear();
  }
  while(avl_insertar(table->avl_arr[idx], key, value,
      table->cpyK, table->cpyV, table->compK) == 0) {
    //TODO: desalojar un elemento
  }
  return;
}

void* hashtable_find(HashTable table, void *key) {
  unsigned idx = table->hash(key) % table->size;
  if(NULL != table->avl_arr[idx]) {
    return avl_buscar(table->avl_arr[idx],
      key, table->compK, table->cpyV); 
  }
  return NULL;
}