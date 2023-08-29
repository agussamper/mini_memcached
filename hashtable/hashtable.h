#ifndef __HASHTABLE_H__
#define __HASHTABLE_H__

#include "functions_kv.h"

// Funcion hash
typedef unsigned (*HashFunction)(void *data);

typedef struct _HashTable *HashTable;

HashTable hashtable_create(
    unsigned size,
    Cpy_key cpyK,
    Cpy_value cpyV,
    Compare_key compK,
    Destroy_key destK,
    Destroy_value destV,
    HashFunction hash);

/**
 * Retorna el numero de elementos en la tabla
*/
int hashtable_nelems(HashTable table);

/**
 * Devuelve la cantidad de slots en la tabla
*/
int hashtable_size(HashTable table);

/**
 * Destruye la tabla
*/
void hashtable_destroy(HashTable table);

/**
 * Inserta la clave y el valor en la tabla,
 * si la clave ya se encontraba, reemplaza
 * el valor por el valor pasado por parametro.
 */
void hashtable_insert(HashTable, void *key,
    void *value);

/**
 * Retorna el valor que se relacione con la clave
 * dada o NULL si la clave buscada no se encuentra
 * en la tabla. 
*/
void* hashtable_find(HashTable table, void *key);

/**
 * Elimina el dato de la tabla que coincida
 * con la clave dada.
 */
void hashtable_delete(HashTable table, void *key);

#endif