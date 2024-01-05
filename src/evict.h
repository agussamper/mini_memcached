#ifndef __EVICT_H__
#define __EVICT_H__

#include <pthread.h>
#include "list.h"

typedef struct _Evict *Evict;
typedef struct _NodeEvict *NodeEvict;

/**
 * Inicializa la estructura evict
*/
void evict_init(Evict* evict_ptr);

/**
 * Agrega list junto con listIdx en la
 * estructura evict.
 * Devuelve 1 si pudo agregar y 0
 * en caso contrario.
*/
int evict_add(Evict evict, List list,
    unsigned listIdx);

/**
 * Dada un nodo de la lista que ya se encuentra
 * en evict, lo mueve y lo coloca como mru
*/
void evict_update(Evict evict, const List list);

/**
 * Elimina list de la estructura
*/
void evict_remove(Evict evict, const List list);

/**
 * Elimina node de evict, esta función no es
 * thread safety
*/
void evict_removeNode(Evict evict, NodeEvict node);

/**
 * Agarra el lock de evict
*/
void evict_lock(Evict evict);

/**
 * Suelta el lock de evict
*/
void evict_unlock(Evict evict);

/**
 * Si la estructura evict está vacía retorna
 * 1, en caso contrario 0.
*/
int evict_empty(Evict evict);

/**
 * Devuelve el elemento menos recientemente
 * usado en la estructura evict
*/
NodeEvict evict_getLru(Evict evict);

/**
 * Dado un node de tipo NodeEvict, devuelve
 * el siguiente
*/
NodeEvict evict_getNextNode(NodeEvict node);

/**
 * Devuelve la lista del nodo pasado por
 * argumentos
*/
List evict_getList(NodeEvict nEvict);

/**
 * Devuelve el indice de la lista de la cache
 * a la que apunte nodeEvict
*/
unsigned evict_getListIdx(NodeEvict nodeEvict);

#endif