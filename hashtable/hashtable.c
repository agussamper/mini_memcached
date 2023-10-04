#include "hashtable.h"
#include "avl/avl.h"
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <unistd.h>

long unsigned version;

typedef struct Stats {
  unsigned long put;
  unsigned long get;
  unsigned long del;
  unsigned long keys;
} Stats;

struct _HashTable {
  AVL *avl_arr;
  unsigned numElems;
  unsigned size;
  FUNC funcs;
  HashFunction hash;
  pthread_mutex_t* mutex_arr; 
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
  table->hash = hash;

  table->funcs = malloc(sizeof(struct _FUNC));
  table->funcs->cpyK = cpyK;
  table->funcs->cpyV = cpyV;
  table->funcs->compK = compK;
  table->funcs->destK = destK;
  table->funcs->destV = destV;  

  long number_of_processors = sysconf(_SC_NPROCESSORS_ONLN);
  long size_mutex_arr = number_of_processors + 1;

  table->mutex_arr = 
    malloc(sizeof(pthread_mutex_t)*size_mutex_arr);

  for(long i = 0; i < size_mutex_arr; i++) {
    pthread_mutex_init((table->mutex_arr)+i, NULL); 
  }

  return table;
}

int hashtable_nelems(HashTable table) {
  return table->numElems;
}

int hashtable_size(HashTable table) {
  return table->size;
}

void hashtable_destroy(HashTable table) {
  for(int i = 0; i < table->size; i++) {
    if(NULL != table->avl_arr[i]) {
      avl_destruir(table->avl_arr[i],
        table->funcs);
    }
  }
  free(table->avl_arr);
  free(table->funcs);
  free(table->mutex_arr);
  free(table);
  return;
}

void hashtable_insert(HashTable table, void *key,
    void *value) {

  unsigned idx = table->hash(key) % table->size;
  if(NULL == table->avl_arr[idx]) {
    table->avl_arr[idx] = avl_crear();
  }
  int updated;
  //TODO: Crear array para pasar argumentos
  //TODO: version puede ser modificado por otros hilos mientras esta ejecutando insertar, solucionar
  while(avl_insertar(table->avl_arr[idx],
      key, value, &updated, 
      version+1, table->funcs) == 0) {
    //TODO: desalojar un elemento
    if(updated == 1) {
      //TODO: proteger con mutex
      version += 1;
    }
  }
  return;
}

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
}