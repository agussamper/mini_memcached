#include "avl.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

/**
 * Estructura del nodo del arbol AVL.
 * Tiene un puntero a la llave,
 * Tiene un puntero al valor,
 * un puntero al nodo raiz del subarbol izquierdo (izq),
 * un puntero al nodo raiz del subarbol derecho (der), y
 * un entero para representar la altura del arbol (altura)
 */
typedef struct _AVL_Nodo {  
  void* key;
  void* value;
  struct _AVL_Nodo* izq, *der;  
  int altura;
  unsigned long version;
} AVL_Nodo;

/**
 * Estructura del arbol AVL.
 * Tiene un puntero al nodo raiz (raiz),
 */
struct _AVL {
  AVL_Nodo* raiz;
};

/**
 * avl_crear: Retorna un arbol AVL vacio
 */
AVL avl_crear() {
  AVL arbol = malloc(sizeof(struct _AVL));
  assert(arbol != NULL);
  arbol->raiz = NULL;
  return arbol;
}

/**
 * avl_destruir: Destruye el arbol y sus datos.
 */
static void avl_nodo_destruir(AVL_Nodo* raiz,
    Destroy_key destK, Destroy_value destV) {
  if (raiz != NULL) {
    // destruir los nodos en postorder
    avl_nodo_destruir(raiz->izq,
      destK, destV);
    avl_nodo_destruir(raiz->der,
      destK, destV);
    destK(raiz->key);
    destV(raiz->value);
    free(raiz);
  }
}
void avl_destruir(AVL arbol, Destroy_key destK,
    Destroy_value destV) {
  avl_nodo_destruir(arbol->raiz, destK, destV);
  free(arbol);
}

static void* avl_nodo_buscar(AVL_Nodo* raiz,
  void* dato, Compare_key compK,
  Cpy_value cpyV) {
  if (raiz == NULL) {
    return NULL;
  } else if (compK(dato, raiz->key) == 0) {
    return cpyV(raiz->value);
  }
  else if (compK(dato, raiz->key) < 0) // dato < raiz->key
    return avl_nodo_buscar(raiz->izq,
      dato, compK, cpyV);
  else // raiz->key < dato
    return avl_nodo_buscar(raiz->der,
      dato, compK, cpyV);
}
void* avl_buscar(AVL arbol, void* key,
    Compare_key compK, Cpy_value cpyV) {
  return avl_nodo_buscar(arbol->raiz, key,
    compK, cpyV);
}

/**
 * avl_nodo_altura: Funcion interna que retorna la altura del arbol.
 * La altura del arbol vacio se define como -1.
 */
static int avl_nodo_altura(AVL_Nodo* raiz) {
  return (raiz == NULL ? -1 : raiz->altura);
}

/**
 * avl_nodo_max_altura_hijos: Funcion interna que retorna la maxima altura de
 * los hijos.
 */
static unsigned int avl_nodo_max_altura_hijos(AVL_Nodo* raiz) {
  assert(raiz != NULL);
  int alturaIzq = avl_nodo_altura(raiz->izq);
  int alturaDer = avl_nodo_altura(raiz->der);
  return (alturaIzq < alturaDer ? alturaDer : alturaIzq);
}

/**
 * avl_nodo_factor_balance: Funcion interna que retorna el factor de balance de
 * un nodo.
 */
static int avl_nodo_factor_balance(AVL_Nodo* raiz) {
  assert(raiz != NULL);
  int factor = avl_nodo_altura(raiz->der) - avl_nodo_altura(raiz->izq);
  assert(-2 <= factor || factor <= 2);
  return factor;
}
/**
 * avl_nodo_rotacion_simple_izq: Funcion interna que realiza una rotacion simple
 * a izquierda y retorna la nueva raiz.
 */
static AVL_Nodo* avl_nodo_rotacion_simple_izq(AVL_Nodo* raiz) {
  AVL_Nodo* hijoDer = raiz->der;
  assert(hijoDer != NULL);
  // actualizar punteros
  raiz->der = hijoDer->izq;
  hijoDer->izq = raiz;
  // actualizar alturas
  raiz->altura = 1 + avl_nodo_max_altura_hijos(raiz);
  hijoDer->altura = 1 + avl_nodo_max_altura_hijos(hijoDer);
  return hijoDer;
}

/**
 * avl_nodo_rotacion_simple_der: Funcion interna que realiza una rotacion simple
 * a derecha y retorna la nueva raiz.
 */
static AVL_Nodo* avl_nodo_rotacion_simple_der(AVL_Nodo* raiz) {
  AVL_Nodo* hijoIzq = raiz->izq;
  assert(hijoIzq != NULL);
  // actualizar punteros
  raiz->izq = hijoIzq->der;
  hijoIzq->der = raiz;
  // actualizar alturas
  raiz->altura = 1 + avl_nodo_max_altura_hijos(raiz);
  hijoIzq->altura = 1 + avl_nodo_max_altura_hijos(hijoIzq);
  return hijoIzq;
}

/**
 * avl_nodo_crear: Funcion interna que crea un nuevo nodo y lo retorna.
 * La altura de un nodo hoja es 0.
 */
static AVL_Nodo* avl_nodo_crear(void* key,
    void* value, Cpy_key cpyK,
    Cpy_value cpyV) {
  AVL_Nodo* nuevoNodo = malloc(sizeof(AVL_Nodo));
  assert(nuevoNodo != NULL);
  nuevoNodo->key = cpyK(key);
  nuevoNodo->value = cpyV(value);
  nuevoNodo->izq = nuevoNodo->der = NULL;
  nuevoNodo->altura = 0;
  nuevoNodo->version = 0;
  return nuevoNodo;
}

/**
 * avl_insertar: Inserta un dato no repetido en el arbol, manteniendo la
 * propiedad de los arboles AVL.
 */
//TODO: pasar las funciones en un arreglo
static AVL_Nodo* avl_nodo_insertar(AVL_Nodo* raiz,
    Cpy_key cpyK, Cpy_value cpyV,
    Compare_key compK, AVL_Nodo* newNode,
    int* updated, Destroy_key destK,
    unsigned long version) {
  if (raiz == NULL) { // insertamos el nuevo elemento
    *updated = 0;
    return newNode;
  }else if (compK(newNode->key, raiz->key) < 0) { // el dato debe ir en el subarbol izq
    raiz->izq = avl_nodo_insertar(raiz->izq,
      cpyK, cpyV, compK, newNode, updated,
      destK, version);
    // chequear balance
    if (avl_nodo_factor_balance(raiz) == -2) {
      // casos 1 o 2
      if (avl_nodo_factor_balance(raiz->izq) == 1) // caso 2
        raiz->izq = avl_nodo_rotacion_simple_izq(raiz->izq);
      raiz = avl_nodo_rotacion_simple_der(raiz); // caso 1
    }
    raiz->altura = 1 + avl_nodo_max_altura_hijos(raiz);
    return raiz;
  }
  else if (compK(newNode->key, raiz->key) > 0) { // el dato debe ir en el subarbol der
    raiz->der = avl_nodo_insertar(raiz->der,
      cpyK, cpyV, compK, newNode, updated,
      destK, version);
    // chequear balance
    if (avl_nodo_factor_balance(raiz) == 2) {
      // casos 3 o 4
      if (avl_nodo_factor_balance(raiz->der) == -1) // caso 3
        raiz->der = avl_nodo_rotacion_simple_der(raiz->der);
      raiz = avl_nodo_rotacion_simple_izq(raiz); // caso 4
    }
    raiz->altura = 1 + avl_nodo_max_altura_hijos(raiz);
    return raiz;
  }
  else // Modificar valor
    *updated = 1;
    raiz->version = version;
    raiz->value = newNode->value; 
    destK(newNode->key);
    free(newNode);
    return raiz;
}
int avl_insertar(AVL arbol, void* key,
  void *value, Cpy_key cpyK, 
  Cpy_value cpyV, Compare_key compK,
  Destroy_key destK, int *updated,
  unsigned long version) {
  AVL_Nodo* newNode = avl_nodo_crear(key, value, cpyK, cpyV);
  if(newNode != NULL) {
    int *updated;
    *updated = 0;
    arbol->raiz = avl_nodo_insertar(
                  arbol->raiz,
                  cpyK,
                  cpyV, compK,
                  newNode,
                  updated,
                  destK,
                  version);
    return 1;
  } else {
    return 0;
  }
}

/**
 * avl_validar: Retorna 1 si el arbol cumple la propiedad de los arboles AVL,
 * y 0 en caso contrario.
 * avl_nodo_validar_abb: Funcion interna que retorna 1 si el arbol cumple la
 * propiedad de los arboles BB, y 0 en caso contrario.
 * avl_nodo_validar_altura_y_balance: Funcion interna que retorna 1 si la
 * altura y el factor de balance de los nodos son correctos, y 0 en caso
 * contrario.
 */
static int avl_nodo_validar_abb(AVL_Nodo* raiz,
    void* min, void* max,
    Compare_key comp) {
  // si la raiz es vacia, retornar exitosamente
  if (raiz == NULL)
    return 1;
  else {
    // sino, validar intervalo
    if (min != NULL && comp(raiz->key, min) <= 0)
      return 0;
    if (max != NULL && comp(max, raiz->key) <= 0)
      return 0;
    // y validar subarboles recursivamente
    return (avl_nodo_validar_abb(raiz->izq, min, raiz->key, comp) &&
      avl_nodo_validar_abb(raiz->der, raiz->key, max, comp));
  }
}
static int avl_nodo_validar_altura_y_balance(AVL_Nodo* raiz) {
  // si la raiz es vacia, retornar exitosamente
  if (raiz == NULL)
    return 1;
  // sino, validar subarboles recursivamente
  int ret1 = avl_nodo_validar_altura_y_balance(raiz->izq);
  int ret2 = avl_nodo_validar_altura_y_balance(raiz->der);
  if (ret1 && ret2) {
    // si ambos subarboles son validos, validar altura y balance de raiz
    int altura = 1 + avl_nodo_max_altura_hijos(raiz);
    int balance = avl_nodo_factor_balance(raiz);
    if ((raiz->altura == altura) && (balance >= -1) && (balance <= 1))
      return 1;
  }
  // en cualquier otro caso, retornar falso
  return 0;
}
int avl_validar(AVL arbol, Compare_key compK) {
  return (
    avl_nodo_validar_altura_y_balance(arbol->raiz) &&
    avl_nodo_validar_abb(arbol->raiz, NULL,
      NULL, compK));
}

static AVL_Nodo* avl_nodo_encuentra_min(AVL_Nodo* raiz) {
  if(raiz == NULL)
    return NULL;
  while(raiz->izq)
    raiz = raiz->izq;
  return raiz;
}
static AVL_Nodo* avl_nodo_eliminar(AVL_Nodo* raiz,
  void* key, Compare_key cmpK, Destroy_key destK,
  Destroy_value destV, Cpy_value cpyK, 
  Cpy_value cpyV) {
  if (raiz == NULL)
    return raiz;
  else if (cmpK(key, raiz->key) < 0) {
    raiz->izq = avl_nodo_eliminar(raiz->izq,
      key, cmpK, destK, destV, cpyK,
      cpyV);
    if (avl_nodo_factor_balance(raiz) == 2) {
      if (avl_nodo_factor_balance(raiz->der) == -1)
        raiz->der =
          avl_nodo_rotacion_simple_der(raiz->der);
      raiz = avl_nodo_rotacion_simple_izq(raiz);
    }
    raiz->altura =
      1 + avl_nodo_max_altura_hijos(raiz);
    return raiz;    
  } else if (cmpK(key, raiz->key) > 0) {
    raiz->der = avl_nodo_eliminar(raiz->der,
      key, cmpK, destK, destV, cpyK, cpyV);
    if (avl_nodo_factor_balance(raiz) == -2) {
      if (avl_nodo_factor_balance(raiz->izq) == 1)
        raiz->izq =
          avl_nodo_rotacion_simple_izq(raiz->izq);
      raiz = avl_nodo_rotacion_simple_der(raiz);
    }
    raiz->altura = 1 + avl_nodo_max_altura_hijos(raiz);
    return raiz;
  } else {
    // Si el nodo no tiene hijos elimina el nodo
    if (raiz->izq == NULL && raiz->der == NULL) {
      avl_nodo_destruir(raiz, destK, destV);      
      raiz = NULL;
      return raiz;
    }
    
    // Si el nodo sólo tiene hijos a la derecha
    else if (raiz->izq == NULL) {
      AVL_Nodo* aux = raiz;
      raiz = raiz->der;
      destK(aux->key);
      destV(aux->value);
      free(aux);
      aux = NULL;
      return raiz;
    }

    // Si el nodo sólo tiene hijos a la izquierda
    else if (raiz->der == NULL) {
      AVL_Nodo* aux = raiz;
      raiz = raiz->izq;
      destK(aux->key);
      destV(aux->value);
      free(aux);
      aux = NULL;
      return raiz;
    }

    // Si el nodo tiene ambos hijos
    else {
      AVL_Nodo* temp = avl_nodo_encuentra_min(raiz->der);
      destK(raiz->key);
      destV(raiz->value);
      raiz->key = cpyK(temp->key);
      raiz->value = cpyV(temp->value);
      raiz->der = avl_nodo_eliminar(raiz->der,
        key, cmpK, destK, destV, cpyK, cpyV);
      // Si está desbalanceado, lo balanceo y ajusto la altura
      if (avl_nodo_factor_balance(raiz) == -2) {
        if (avl_nodo_factor_balance(raiz->izq) == 1)
          raiz->izq = 
            avl_nodo_rotacion_simple_izq(raiz->izq);
        raiz = avl_nodo_rotacion_simple_der(raiz);
      } 
      raiz->altura =
        1 + avl_nodo_max_altura_hijos(raiz);
      return raiz;
    }
  }
}
void avl_eliminar(AVL arbol, void* key,
  Compare_key cmpK, Destroy_key destK,
  Destroy_value destV, Cpy_key cpyK,
  Cpy_value cpyV) {
  arbol->raiz = avl_nodo_eliminar(arbol->raiz,
    key, cmpK, destK, destV, cpyK, cpyV);
}
