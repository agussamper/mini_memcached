#ifndef __MALLOC_INTERFACE_H__
#define __MALLOC_INTERFACE_H__

#include <stddef.h>
#include <pthread.h>

/**
 * Aloca espacio en memoria para el size dado.
 * Mientras que no tenga espacio para alocar
 * el size pasado por argumentos eliminará
 * elementos de la cache.
 * Si luego de eliminar todos los elementos
 * de la cache no consigue el espacio requerido
 * retornará NULL, en caso que lo haya alocado
 * con éxito retorna el puntero a la memoria
 * dada.
 * se debe pasar en listMutex el mutex de la
 * cache que esté tomado (un elemento de
 * mutex_arr) al llamar la función. Notar
 * que esta función puede modificar las
 * listas que estén protegidas por listMutex
*/
void* allocate_mem(size_t size,
  pthread_mutex_t* listMutex);

#endif