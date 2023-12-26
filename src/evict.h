#ifndef __EVICT_H__
#define __EVICT_H__

typedef struct _Evict *Evict;
typedef struct _NodeEvict *NodeEvict;

/**
 * Inicializa la estructura evict
*/
void evict_init(Evict* evict_ptr);

/**
 * Agrega list a la estructura evict
 * Devuelve 1 si pudo agregar y 0
 * en caso contrario
*/
int evict_add(Evict evict, const List list);

/**
 * Elimina list de la estructura
*/
void evict_remove(Evict evict, const List list);

/**
 * Elimina a lo sumo los 10 elementos menos
 * usados de la cache 
*/
void evict_dismiss(Cache cache, Evict evict);

#endif