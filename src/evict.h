#ifndef __EVICT_H__
#define __EVICT_H__

#include "list.h"

typedef struct _Evict *Evict;
typedef struct _NodeEvict *NodeEvict;

/**
 * Inicializa la estructura evict
*/
void evict_init(Evict* evict_ptr);

/**
 * Agrega list junto con lnode en la
 * estructura evict
 * Devuelve 1 si pudo agregar y 0
 * en caso contrario
*/
int evict_add(Evict evict, List list);

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
 * Elimina el elemento menos recientemente usado
 * de evict. Esta función no es thread safety
*/
void evict_removeLru(Evict evict);

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
 * Devuelve la lista del nodo pasado por
 * argumentos
*/
List evict_getList(NodeEvict nEvict);

#endif