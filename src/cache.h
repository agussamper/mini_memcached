#ifndef __CACHE_H__
#define __CACHE_H__

#include "evict.h"
#include "stats.h"

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
char* cache_get(Cache cache, char* key);

/**
 * Elimina el dato de la cache que coincida
 * con la clave dada.
 * Si lo elimina con éxito retorna 1
 * Si no encuentra key en la cache retorna 0
 */
int cache_delete(Cache cache, char* key);

/**
 * Intenta tomar el mutex correspondiente a la
 * clave que contenga list.
 * Retorna un puntero al mutex si el mutex
 * es tomado, en caso contrario retorna NULL
*/
pthread_mutex_t* cache_trylock(
    Cache cache, List list);

/**
 * Devuelve la estructura stats de la cache
*/
Stats cache_getStats(Cache cache);

#endif