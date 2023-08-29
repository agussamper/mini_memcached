#ifndef __LIST_H__
#define __LIST_H__

#include "functions_kv.h"

typedef struct _List *List;

/**
 * Devuelve una lista vacia
*/
List list_create();

/**
 * Libera la memoria de la lista
*/
void list_destroy(List list,
  Destroy_key destroyK,
  Destroy_value destroyV);

/**
 * Devuelve 1 si la lista esta
 * vacia y 0 en caso contrario
*/
int list_empty(List list);

/**
 * Agrega la clave y el valor pasadas
 * por argumento a la lista, si la clave
 * ya se encuentra en la lista sobreescribe
 * el valor asociado a la misma. 
 * Devuelve 1 si agregó el elemento
 * y 0 si no lo pudo agregar 
*/
int list_add(List list, void *key,
  void *value, Cpy_key cpyK,
  Cpy_value cpyV);

/**
 * Borra de la lista el elemento que tiene
 * a la clave pasada por argumentos.
 * Devuelve 1 si encontró y borró el 
 * elemento y si no lo encontró devuelve 0 
*/
int list_remove(List list, void *key,
  Destroy_key destK, Destroy_value destV);

/**
 * Busca el valor asociado a la clave pasada
 * por argumentos, si encuentra tal valor
 * devuelve una copia del mismo, si no 
 * devuelve NULL 
*/
void* list_getValue(List list, void *key,
  Compare_key cmpK);

#endif