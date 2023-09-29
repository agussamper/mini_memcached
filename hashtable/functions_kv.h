#ifndef __FUNCTIONS_KV_H__
#define __FUNCTIONS_KV_H__

/** Libera la memoria alocada para la clave */
typedef void (*Destroy_key)(void *key);

/** Libera la memoria alocada para el valor */
typedef void (*Destroy_value)(void *value);

/** Retorna una copia fisica del valor */
typedef void *(*Cpy_key)(void *key);

/** Retorna una copia fisica de la clave */
typedef void *(*Cpy_value)(void *value);

/**
 * Compara las claves 
 * Retorna un entero negativo si key1 < key2,
 * 0 si son iguales y un entero
 * positivo si key1 > key2
 */
typedef int (*Compare_key)(void *key1, void *key2);

/**
 * Compara los valores 
 * Retorna un entero negativo si value1 < value2,
 * 0 si son iguales y un entero
 * positivo si value1 > value2
 */ //TODO: borrar si no es necesaria
typedef int (*Compare_value)(void *value1, void *value2);

#endif