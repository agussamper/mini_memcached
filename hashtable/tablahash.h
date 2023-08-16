#ifndef __TABLAHASH_H__
#define __TABLAHASH_H__

/** Retorna una copia fisica del dato */
typedef void *(*Copia_claveType)(void *dato);

/** Retorna una copia fisica del dato */
typedef void *(*Copia_valorType)(void *dato);

/** Retorna un entero negativo si dato1 < dato2,
 *  0 si son iguales y un entero
 * positivo si dato1 > dato2  */
typedef int (*Compara_claveType)(void *dato1, void *dato2);

/** Libera la memoria alocada para la clave */
typedef void (*Destruye_claveType)(void *clave);

/** Libera la memoria alocada para el valor */
typedef void (*Destruye_valorType)(void *valor);

/** Retorna un entero sin signo para el dato */
typedef unsigned (*FuncionHash)(void *dato);

typedef struct _TablaHash *TablaHash;

/**
 * Crea una nueva tabla hash vacia, con la capacidad dada.
 */
TablaHash tablahash_crear(unsigned capacidad,
                          Copia_claveType copia_claveT,
                          Copia_valorType copia_valT,
                          Compara_claveType comp,
                          Destruye_claveType destr_clave,
                          Destruye_valorType destr_valor,
                          FuncionHash hash);

/**
 * Retorna el numero de elementos de la tabla.
 */
int tablahash_nelems(TablaHash tabla);

/**
 * Retorna la capacidad de la tabla.
 */
int tablahash_capacidad(TablaHash tabla);

/**
 * Destruye la tabla.
 */
void tablahash_destruir(TablaHash tabla);

/**
 * Inserta la clave y el valor en la tabla,
 * si la clave ya se encontraba, reemplaza
 * el valor por el valor pasado por parametro.
 */
void tablahash_insertar(TablaHash tabla, void *clave, void *valor);

/**
 * Retorna el valor que se relacione con la clave
 * dada o NULL si la clave buscada no se encuentra
 * en la tabla.
 */
void *tablahash_buscar(TablaHash tabla, void *dato);

/**
 * Elimina el dato de la tabla que coincida con el dato dado.
 */
void tablahash_eliminar(TablaHash tabla, void *dato);

#endif /* __TABLAHASH_H__ */
