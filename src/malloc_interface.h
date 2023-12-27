#ifndef __MALLOC_INTERFACE_H__
#define __MALLOC_INTERFACE_H__

#include <stddef.h>

/**
 * Aloca espacio en memoria para el size dado.
 * Mientras que no tenga espacio para alocar
 * el size pasado por argumentos eliminará
 * elementos de la cache.
 * Si luego de eliminar todos los elementos
 * de la cache no consigue el espacio requerido
 * retornará NOMEM, en caso que lo haya alocado
 * con éxito retorna 0
*/
void* allocate_mem(size_t size);

#endif