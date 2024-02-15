//! @file

#ifndef __MALLOC_INTERFACE_H__
#define __MALLOC_INTERFACE_H__

#include <stddef.h>
#include <pthread.h>

/**
 * Aloca espacio en memoria para el size dado.
 * Mientras que no tenga espacio para alocar
 * el size pasado por argumentos eliminará
 * elementos de la cache. Notar
 * que esta función puede modificar las
 * listas que estén protegidas por listMutex
 * @param size es la cantidad de bytes a guardar
 * en el heap.
 * @param listMutex se debe pasar el 
 * mutex de la cache que esté tomado
 * (un elemento de mutex_arr) al llamar
 * la función.
 * @return
 * Si luego de eliminar todos los elementos
 * de la cache no consigue el espacio requerido
 * retornará NULL, en caso que lo haya alocado
 * con éxito retorna el puntero a la memoria
 * dada.
*/
void* allocate_mem(size_t size,
  pthread_mutex_t* listMutex);

/**
 * Realoca espacio en memoria para el size dado.
 * Mientras que no tenga espacio para alocar
 * el size pasado por argumentos eliminará
 * elementos de la cache. Notar
 * que esta función puede modificar las
 * listas que estén protegidas por listMutex
 * @param ptr es lo que se quiere realocar.
 * @param size es la cantidad de bytes a guardar
 * en el heap.
 * @param listMutex se debe pasar el 
 * mutex de la cache que esté tomado
 * (un elemento de mutex_arr) al llamar
 * la función.
 * @return
 * Si luego de eliminar todos los elementos
 * de la cache no consigue el espacio requerido
 * retornará NULL, en caso que lo haya alocado
 * con éxito retorna el puntero a la memoria
 * dada.
*/
void* realloc_mem(void* ptr,
    size_t size,
    pthread_mutex_t* listMutex);

#endif