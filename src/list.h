#ifndef __LIST_H__
#define __LIST_H__

typedef struct Node *List;

/**
 * Devuelve una lista vacia
*/
List list_create();

/**
 * Libera la memoria de la lista
*/
void list_destroy(List list);

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
//TODO: Me conviene devolver el nodo
int list_add(List* list,
  char* key, unsigned klen,
  char* value, unsigned vlen);

/**
 * Elimina el último elemento ingresado a la lista
*/
int list_remove(List* list);

/**
 * Borra de la lista el elemento que tiene
 * a la clave pasada por argumentos.
 * Devuelve 1 si encontró y borró el 
 * elemento y si no lo encontró devuelve 0 
*/
int list_remove_key(List* list, char* key);

/**
 * Busca el valor asociado a la clave pasada
 * por argumentos, si encuentra tal valor
 * devuelve una copia del mismo, si no 
 * devuelve NULL 
*/
void* list_getValue(List* list, char* key);

/**
 * Devuelve un puntero al nodo de la
 * estructura evict donde se encuentra
 * el puntero a list 
*/
NodeEvict list_getNodeEvict(List list);

#endif