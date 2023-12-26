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
 * Devuelve 1 si pudo agregar y NOMEM
 * si no hay memoria para a agregar el
 * elemento 
*/
int evict_add(Evict evict, List list);

/**
 * Elimina list de la estructura
*/
void evict_remove(Evict evict, List list);

/**
 * Elimina el elemento menos usado de
 * la estructura evict 
*/
void evict_dismiss(Evict evict);

#endif