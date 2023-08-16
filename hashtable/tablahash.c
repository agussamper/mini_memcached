#include "tablahash.h"
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

/**
 *  tablahash que maneja las colisiones con direccionamiento abierto
 *  (linear probing)
 *
 *  Se resuelven los problemas de colisión con la heurística conocida
 *  como Robin Hood
 */

/**
 * Casillas en la que almacenaremos los datos de la tabla hash.
 */
typedef struct {
  unsigned dist; // Indica la distancia del slot al que fue hasheado el dato   
  void *clave;
  void *valor;
} CasillaHash;

/**
 * Estructura principal que representa la tabla hash.
 */
struct _TablaHash {
  CasillaHash *elems;
  unsigned numElems;
  unsigned capacidad;
  Copia_claveType copia_clave;
  Copia_valorType copia_valor;
  Compara_claveType comp;
  Destruye_claveType destr_clave;
  Destruye_valorType destr_valor;
  FuncionHash hash;
};

/**
 * Crea una nueva tabla hash vacia, con la capacidad dada.
 */
TablaHash tablahash_crear(unsigned capacidad,
                          Copia_claveType copia_clave,
                          Copia_valorType copia_valor,
                          Compara_claveType comp,
                          Destruye_claveType destr_clave,
                          Destruye_valorType destr_valor,
                          FuncionHash hash) {

  // Pedimos memoria para la estructura principal y las casillas.
  TablaHash tabla = malloc(sizeof(struct _TablaHash));
  assert(tabla != NULL);
  tabla->elems = malloc(sizeof(CasillaHash) * capacidad);
  assert(tabla->elems != NULL);
  tabla->numElems = 0;
  tabla->capacidad = capacidad;
  tabla->copia_clave = copia_clave;
  tabla->copia_valor = copia_valor;
  tabla->comp = comp;
  tabla->destr_clave = destr_clave;
  tabla->destr_valor = destr_valor;
  tabla->hash = hash;

  // Inicializamos las casillas con datos nulos.
  for (unsigned idx = 0; idx < capacidad; ++idx) {
    tabla->elems[idx].clave = NULL;
    tabla->elems[idx].valor = NULL;
    tabla->elems[idx].dist = 0;
  }

  return tabla;
}

int tablahash_nelems(TablaHash tabla) { return tabla->numElems; }

int tablahash_capacidad(TablaHash tabla) { return tabla->capacidad; }

void tablahash_destruir(TablaHash tabla) {

  // Destruir cada uno de los datos.
  for (unsigned idx = 0; idx < tabla->capacidad; ++idx) {
    if (tabla->elems[idx].clave != NULL) {
      tabla->destr_clave(tabla->elems[idx].clave);
      if (tabla->elems[idx].valor != NULL)
        tabla->destr_valor(tabla->elems[idx].valor);
    }
  }

  // Liberar el arreglo de casillas y la tabla.
  free(tabla->elems);
  free(tabla);
  return;
}

void tablahash_insertar(TablaHash tabla,
    void *clave, void *valor) { 
  
  if (tabla->numElems*10/tabla->capacidad >= 6) {
    //TODO: resolver acá lo que sucede cuando el factor de carga es mayor a 0.6   
  }
  
  // Calculamos la posicion del dato dado, de acuerdo a la funcion hash.
  unsigned idx = tabla->hash(clave) % tabla->capacidad;  

  unsigned dist = 0;
  
  void *copiaClave = tabla->copia_clave(clave);
  void *copiaValor = tabla->copia_valor(valor);

  while(1) {    
    // Insertar la clave y el valor si la casilla está libre.
    if (tabla->elems[idx].clave == NULL) {
      tabla->numElems++;
      tabla->elems[idx].clave = copiaClave;
      tabla->elems[idx].valor = copiaValor;
      tabla->elems[idx].dist = dist;
      return;
    }
    // Sobrescribir el valor si la clave ya se
    // encontraba en la tabla.
    else if (tabla->comp(tabla->elems[idx].clave, copiaClave) == 0) {
      tabla->destr_valor(tabla->elems[idx].valor);
      tabla->elems[idx].valor = copiaValor;
      return;
    }
    /* Intercambio lo que hay en la tabla por el valor y clave 
     * a ingresar si la distancia de lo que hay en la tabla 
     * es menor que la distancia que esta teniendo el dato
     * a ingresar de su slot
     */
    else if (tabla->elems[idx].dist < dist) {
      void* aux_clave = tabla->elems[idx].clave;
      void* aux_valor = tabla->elems[idx].valor;
      tabla->elems[idx].clave = copiaClave;
      tabla->elems[idx].valor = copiaValor;
      copiaClave = aux_clave;
      copiaValor = aux_valor;
      unsigned distAnt = tabla->elems[idx].dist;
      tabla->elems[idx].dist = dist;
      dist = distAnt;
    }    
    dist++;
    idx = (idx + 1) % tabla->capacidad;
  }
}

void* cpy_return(void* toReturn) {
  return toReturn;
}

void *tablahash_buscar(TablaHash tabla, void *clave) {
  // Calculamos la posicion del dato dado, de acuerdo a la funcion hash.
  unsigned idx = tabla->hash(clave) % tabla->capacidad;
  unsigned distance = 0;
  do {
    // Retornar NULL si la casilla estaba vacia.
    if (tabla->elems[idx].clave == NULL)
      return NULL;
    // Retornar el dato de la casilla si hay concidencia.
    else if (tabla->comp(tabla->elems[idx].clave, clave) == 0)
      return tabla->elems[idx].valor;
    
    idx = (idx + 1) % tabla->capacidad;
    distance++;
    
  } while (distance <= tabla->elems[idx].dist);

  return NULL;
}

/**
 * Elimina el dato de la tabla que coincida con el dato dado.
 */
void tablahash_eliminar(TablaHash tabla, void *clave) {

  // Calculamos la posicion del dato dado, de acuerdo a la funcion hash.
  unsigned idx = tabla->hash(clave) % tabla->capacidad;
  unsigned distance = 0;

  do {
    // Retornar si la casilla estaba vacia.
    if (tabla->elems[idx].clave == NULL)
      return;
    // Vaciar la casilla si hay coincidencia.
    else if (tabla->comp(tabla->elems[idx].clave, clave) == 0) {
      tabla->numElems--;
      tabla->destr_clave(tabla->elems[idx].clave);
      tabla->elems[idx].clave = NULL;
      tabla->destr_valor(tabla->elems[idx].valor);      
      tabla->elems[idx].valor = NULL;
      tabla->elems[idx].dist = 0;

      idx = (idx + 1) % tabla->capacidad;
      while (tabla->elems[idx].dist > 0) {
        tabla->elems[(idx - 1) % tabla->capacidad].clave =
          tabla->elems[idx].clave;
        tabla->elems[(idx - 1) % tabla->capacidad].valor =
          tabla->elems[idx].valor;  
        tabla->elems[(idx - 1) % tabla->capacidad].dist =
          tabla->elems[idx].dist - 1;
        tabla->destr_clave(tabla->elems[idx].clave);
        tabla->destr_valor(tabla->elems[idx].valor);
        tabla->elems[idx].clave = NULL;
        tabla->elems[idx].valor = NULL;
        tabla->elems[idx].dist = 0;
        idx = (idx + 1) % tabla->capacidad;
      } 
      return;
    }
    distance++;
    idx = (idx + 1) % tabla->capacidad;
  } while (distance <= tabla->elems[idx].dist);
}
