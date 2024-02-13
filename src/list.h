//! @file

#ifndef __LIST_H__
#define __LIST_H__

#include <pthread.h>
#include <stdint.h>

typedef struct Node *List;
typedef struct _NodeEvict *NodeEvict;

typedef struct ValData {
  char* value;
  uint32_t valSize;
  int isBin;
} ValData;

/**
 * Devuelve una lista vacia.
*/
List list_create();

/**
 * Libera la memoria de la lista.
*/
void list_destroy(List list);

/**
 * Devuelve 1 si la lista esta
 * vacia y 0 en caso contrario.
*/
int list_empty(List list);

/**
 * Agrega la clave y el valor pasadas
 * por argumento a la lista, además de la
 * longitud del valor y si los datos están
 * en modo binario o no. Si la clave
 * ya se encuentra en la lista sobreescribe
 * el valor asociado a la misma. Si se 
 * sobreescribe o se agrega, el nodo
 * modificado o agregado, se coloca en el tope
 * de la lista.
 * @param list puntero a lista que se quiere
 * modificar.
 * @param key clave que se quiere agregar a
 * list.
 * @param klen longitud de la clave.
 * @param value valor que se quiere agregar
 * a list.
 * @param vlen longitud del valor.
 * @param isBin Pasar 1 si lo que se guarda
 * está en modo binario y 0 si no
 * @param listMutex se debe pasar el 
 * mutex de la cache que esté tomado
 * (un elemento de mutex_arr) al llamar
 * la función.
 * @return 
 * Devuelve 1 si agregó el elemento, 2 si
 * se sobreesceibió y 0 si no se pudo agregar.
*/
int list_add(List* list,
  char* key, unsigned klen,
  char* value, unsigned vlen,
  int isBin,
  pthread_mutex_t* listMutex);

/**
 * Elimina lNode de la lista apuntada por list.
*/
void list_remove_node(List* list, List lNode);

/**
 * Borra de la lista el elemento que tiene
 * a la clave pasada por argumentos.
 * @param list puntero a lista que se quiere
 * modificar.
 * @param key clave que se quiere eliminar de
 * list.
 * @param keyLen longitud de la clave.
 * @return
 * Devuelve 1 si encontró y borró el 
 * elemento y si no lo encontró devuelve 0.
*/
int list_remove_key(List* list,
  char* key, uint32_t keyLen);

/**
 * Busca el valor asociado a la clave pasada
 * por argumentos.
 * Si se encuentra la clave deja en NULL
 * el valor asociado a la clave
 * y es posible que se elimine de la lista.
 * @param list puntero a la lista dónde se
 * quiere buscar la clave.
 * @param key clave que se quiere encontrar.
 * @param lenK longitud de key.
 * @param listMutex se debe pasar el 
 * mutex de la cache que esté tomado
 * (un elemento de mutex_arr) al llamar
 * la función.
 * @return
 * si encuentra tal valor devuelve una copia
 * del mismo, en una estructura de tipo
 * ValData, si no lo encuentra devuelve NULL.
*/
ValData* list_getValue(List* list,
  char* key, uint32_t lenK,
  pthread_mutex_t* listMutex);

/**
 * Devuelve el puntero al nodo de la lista
 * que tiene a key. Si no lo encuentra
 * devuelve NULL.
*/
List list_getByKey(List* list, char* key,
  uint32_t keyLen);

/**
 * Devuelve un puntero al nodo de la
 * estructura evict donde se encuentra
 * el puntero a list. 
*/
NodeEvict list_getEvictNode(List list);

/**
 * Guarda node en list
*/
void list_setEvictNode(List list, NodeEvict node);

/**
 * Dada un puntero a nodo de la lista
 * devuelve su clave.
*/
char* list_getKey(List list);

#endif