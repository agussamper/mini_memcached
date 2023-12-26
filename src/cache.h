#ifndef __CACHE_H__
#define __CACHE_H__

#include "evict.h"

// Funcion hash
typedef unsigned (*HashFunction)(void* data);

typedef struct _Cache *Cache;

Cache cache_create(
    unsigned size,
    HashFunction hash);

/**
 * Retorna el numero de elementos en la cache
*/
int cache_nelems(Cache cache);

/**
 * Devuelve la cantidad de slots en la cache
*/
int cache_size(Cache cache);

/**
 * Devuelve la estructura evict de la cache
*/
Evict cache_getEvict(Cache cache);

/**
 * Inserta la clave y el valor en la cache,
 * si la clave ya se encontraba, reemplaza
 * el valor por el valor pasado por parametro.
 */
void cache_insert(Cache table, 
    char *key, unsigned key_length, 
    char *value, unsigned value_length);

/**
 * Retorna un puntero a una copia del valor 
 * que se relacione con la clave
 * dada o NULL si la clave buscada
 * no se encuentra en la cache. 
*/
void* cache_find(Cache cache, char* key);

/**
 * Elimina el dato de la cache que coincida
 * con la clave dada.
 */
void cache_delete(Cache cache, char* key);

#endif