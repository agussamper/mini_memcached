#include "hashtable.h"
#include "list.h"
#include "functions_kv.h"
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

struct _HashTable {
  List *listArr;
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
  table->listArr = malloc(sizeof(List)*size);
  assert(table->listArr);
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
    if(table->listArr[i] != NULL) {
      list_destroy(table->listArr[i],
        table->destK, table->destV);
    }
  }
  free(table->listArr);
  free(table);
  return;
}

void hashtable_insert(HashTable table, void *key,
    void *value) {
  /*
  Deberíamos hacer la tabla los suficientemente grande
  para que este caso no ocurra, es decir para que no
  necesitemos hacer un rehash de la tabla
  if (table->numElems*10/table->size >= 6) {
    //TODO: resolver acá lo que sucede cuando el factor de carga es mayor a 0.6   
  }
  */
  
  unsigned idx = table->hash(key) % table->size;
  if(table->listArr[idx] == NULL) {
    table->listArr[idx] = list_create();
  }
  while(list_add(table->listArr[idx], key, value,
      table->cpyK, table->cpyV) == 0) {
    //TODO: desalojar un elemento
  }
  return;
}