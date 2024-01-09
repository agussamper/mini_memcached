#ifndef __CACHE_H__
#define __CACHE_H__

#include "evict.h"
#include "stats.h"

// Funcion hash
typedef unsigned (*HashFunction)(const char* data);

typedef struct _Cache *Cache;

Cache cache_create(
    unsigned size,
    HashFunction hash);
    
/**
 * Devuelve la cantidad de slots en la cache
*/
int cache_size(Cache cache);

/**
 * Inserta la clave y el valor en la cache,
 * si la clave ya se encontraba, reemplaza
 * el valor por el valor pasado por parametro.
 * Si pude insertar retorna 1, en caso
 * contrario 0.
 */
int cache_insert(Cache table, 
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
 * Elimina a lo sumo los 10 elementos menos
 * usados de la cache.
 * Retorna 1 si no hay más elementos para
 * eliminar en la cache y 0 en caso contrario. 
*/
int cache_evict(Cache cache,
    pthread_mutex_t* listMutex);

/**
 * Devuelve un string con la estadisticas de la
 * cache, debe hacerse un free del string cuando
 * no se necesite más
*/
char* cache_getStats(Cache cache);

/**
 * Retorna 1 si la cache está vacía y 0 si no
*/
int cache_empty(Cache cache);

#endif