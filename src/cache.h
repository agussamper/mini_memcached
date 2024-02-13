//! @file

#ifndef __CACHE_H__
#define __CACHE_H__

#include "evict.h"
#include "stats.h"
#include <stdint.h>

// Funcion hash
typedef unsigned (*HashFunction)(
  const char* data, uint32_t len);

typedef struct _Cache *Cache;

/**
 * Crea la cache
 * @param size cantidad de slots que tendrá
 * la caché (no es la cantidad máxima de 
 * elementos que se pueden guardar).
 * @param hashFuncion función hash que
 * utiizará la caché para guardar sus 
 * elementos.
 * @return
 * Devuelve una cache inicializada.
*/
Cache cache_create(
    unsigned size,
    HashFunction hash);

/**
 * Destruye la cache pasada por argumentos.
*/
void cache_destroy(Cache cache);

/**
 * Devuelve la cantidad de slots en la cache.
*/
int cache_size(Cache cache);

/**
 * Inserta la clave y el valor en la cache,
 * si la clave ya se encontraba, reemplaza
 * el valor por el valor pasado por parametro.
 * @param cache donde se quiere insertar la
 * clave y el valor.
 * @param key clave que se quiere insertar
 * a cache.
 * @param key_length longitud de la clave.
 * @param value valor que se quiere insertar
 * a cache.
 * @param value_length longitud del valor.
 * @param isBin Si la clave y el valor están
 * en binarios pasar 1, si no 0.
 * @return Si pude insertar retorna 1, en caso
 * contrario 0.
 * */
int cache_insert(Cache table, 
    char *key, unsigned key_length, 
    char *value, unsigned value_length,
    int isBin);

/**
 * Retorna un puntero a una estructura
 * valData, la cual tiene el puntero del
 * valor que se asocia con la clave
 * dada, el cual se debe liberar luego
 * de su uso. Retorna NULL si la clave buscada
 * no se encuentra en la cache.
 * @param cache dónde se quiere encontrar
 * la clave.
 * @param key clave para la cuál se quiere
 * obtener su valor asociado. 
 * @param keyLen longitud de la clave.
*/
ValData* cache_get(Cache cache, 
  char* key, uint32_t keyLen);

/**
 * Elimina el dato de la cache que coincida
 * con la clave dada.
 * @param cache de la cuál se quiere eliminar
 * el dato. 
 * @param key clave que se quiere eliminar.
 * @return 
 * Si lo elimina con éxito retorna 1
 * Si no encuentra key en la cache retorna 0.
 */
int cache_delete(Cache cache, char* key,
  uint32_t keyLen);

/**
 * Elimina a lo sumo los 10 elementos menos
 * usados de la cache.
 * 
 * @return 
 * Retorna 1 si no hay más elementos para
 * eliminar en la cache y 0 en caso contrario. 
*/
int cache_evict(Cache cache,
    pthread_mutex_t* listMutex);

/**
 * Devuelve un string con la estadisticas de la
 * cache.
*/
char* cache_getStats(Cache cache);

/**
 * Retorna 1 si la cache está vacía y 0 si no
*/
int cache_empty(Cache cache);

#endif