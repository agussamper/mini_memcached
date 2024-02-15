//! @file

#ifndef __EVICT_H__
#define __EVICT_H__

#include <pthread.h>
#include "list.h"

typedef struct _Evict *Evict;
typedef struct _NodeEvict *NodeEvict;

/**
 * Inicializa la estructura evict.
 * @param evict_ptr Puntero a estructura
 * evict que se quiere inicilizar.
*/
void evict_init(Evict* evict_ptr);

/**
 * Destruye la estructura evict pasado por
 * argumentos.
 * No libera list de la estructura _NodeEvict.
*/
void evict_destroy(Evict evict);

/**
 * Agrega list junto con listIdx en la
 * estructura evict.
 * @param evict al que se quiere agregar
 * list junto con listIdx.
 * @param list puntero a nodo a agregar.
 * @param listIdx Slot correspondiente a 
 * list en la cache.
 * @param listMutex se debe pasar el 
 * mutex de la cache que esté tomado
 * (un elemento de mutex_arr) al llamar
 * la función.
 * @return
 * Devuelve 1 si pudo agregar y 0
 * en caso contrario.
*/
int evict_add(Evict evict, List list,
    unsigned listIdx, pthread_mutex_t* listMutex);

/**
 * Dada un nodo de la lista que ya se encuentra
 * en evict, lo mueve y lo coloca como mru.
 * @param evict que se quiere modificar.
 * @param list puntero a nodo de la lista que
 * se quiere poner como mru en evict.
*/
void evict_update(Evict evict, const List list);

/**
 * Elimina el nodo que corresponde a list de
 * la estructura.
 * @param evict dónde se quiere eliminar el
 * nodo correspondiente a list.
 * @param list del cual se quiere eliminar el
 * nodo correspondiente de evict.
*/
void evict_remove(Evict evict, const List list);

/**
 * Elimina node de evict, esta función no es
 * thread safety.
 * @param evict a modificar.
 * @param node nodo a eliminar de evict.
*/
void evict_removeNode(Evict evict, NodeEvict node);

/**
 * Agarra el lock de evict.
 * @param evict del cual se quiere tomar el lock.
*/
void evict_lock(Evict evict);

/**
 * Suelta el lock de evict.
 * @param evict del cual se quiere soltar el lock.
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
 * Devuelve la lista asociada al nodo pasado por
 * argumentos
*/
List evict_getList(NodeEvict nEvict);

/**
 * Devuelve el índice de la lista de la cache
 * a la que apunte nodeEvict
*/
unsigned evict_getListIdx(NodeEvict nodeEvict);

#endif