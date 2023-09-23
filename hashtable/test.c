#include "hashtable.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

/**
 * Casos de prueba para arboles AVL
 */

 /**
  * Funciones para usar arboles AVL con datos de tipo int *
  */
static void* copiar_puntero_entero(void* i) {
  int* p = malloc(sizeof(int));
  *p = *(int*)i;
  return p;
}
static int comparar_puntero_entero(void* i1, void* i2) {
  return (*(int*)i1 - *(int*)i2);
}
static void destruir_puntero_entero(void* i) { free(i); }
static void imprimir_puntero_entero(void* i,
  __attribute__((unused)) void* extra) {
  printf("%d ", *(int*)i);
}

unsigned int hash(unsigned int* x) {
    int result = *x;
    result = ((result >> 16) ^ result) * 0x45d9f3b;
    result = ((result >> 16) ^ result) * 0x45d9f3b;
    result = (result >> 16) ^ result;
    return result;
}

void test1() {
  HashTable ht = hashtable_create(
    1000, 
    copiar_puntero_entero,
    copiar_puntero_entero,
    comparar_puntero_entero,
    destruir_puntero_entero,
    destruir_puntero_entero,
    (HashFunction)hash
  );

  int keys[] = { 10, 20, 15, 25, 30, 16, 18, 19 };
  int values[8];
  for(int i = 0; i < 8; i++) {
    values[i] = keys[i]*10;
    hashtable_insert(ht, keys+i, values+i);
  }

  for(int i = 0; i < 8; i++) {
    int* num = 
      (int*)(hashtable_find(ht, (void*)(keys+i))); 
    assert(keys[i]*10 == *num);
    free(num);
  }
  int otrosNums[] = {5,2,4,6};
  for(int i = 0; i < 4; i++) {
    assert(
      hashtable_find(ht, (void*)(otrosNums+i))
      == NULL);
  }
  
  hashtable_destroy(ht);

  puts("test1: ok");
}

void test2() {
  puts("test2 running");
  HashTable ht = hashtable_create(
    1000, 
    copiar_puntero_entero,
    copiar_puntero_entero,
    comparar_puntero_entero,
    destruir_puntero_entero,
    destruir_puntero_entero,
    (HashFunction)hash
  );

  int size = 200000;
  int keys[size];
  puts("insertando elementos....");
  for(int i = 0; i < size; i++) {
    keys[i] = i;
    int val = keys[i]*10;
    hashtable_insert(ht, keys+i, &val);
  }
  puts("elementos insertados");
  puts("");
  puts("Buscando elementos");
  for(int i = 0; i < size; i++) {
    int* num = 
      (int*)(hashtable_find(ht, (void*)(keys+i))); 
    assert(keys[i]*10 == *num);
    free(num);
  }
  puts("Busquedas finalizadas");

  hashtable_destroy(ht);

  puts("test2: ok");
} 

int main() {
  test1();
  test2();
  return 0;
}