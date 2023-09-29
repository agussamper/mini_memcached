#ifndef __AVL_H__
#define __AVL_H__

#include "../functions_kv.h"

typedef struct _AVL *AVL;

/**
 * Retorna un arbol AVL vacio
 */
AVL avl_crear();

/**
 * Destruye el arbol y sus datos.
 */
void avl_destruir(AVL, Destroy_key,
  Destroy_value);

/**
 * Retorna un puntero a una copia del valor
 * si la clave se encuentra en el avl
 * y NULL en caso contrario
 */
void* avl_buscar(AVL, void *key,
  Compare_key, Cpy_value);

/**
 * Inserta si es posible un dato no repetido
 * en el arbol, manteniendo la propiedad de los
 * arboles AVL.
 * Si la inserción fue exitosa devuelve 1, en
 * caso contrario devuelve 0.
 */
int avl_insertar(AVL,
  void* key, void* value,
  Cpy_key, Cpy_value,
  Compare_key, Destroy_key,
  int *updated,
  unsigned long version);

/**
 * Retorna 1 si el arbol cumple la propiedad de los arboles AVL, y 0 en caso
 * contrario.
 */
int avl_validar(AVL, Compare_key);

/**
 * Elimina el elemento del arbol indicado por parametros
 */
void avl_eliminar(AVL arbol, void* key,
  Compare_key cmpK, Destroy_key destK,
  Destroy_value destV, Cpy_key cpyK,
  Cpy_value cpyV);

#endif /* __AVL_H__*/