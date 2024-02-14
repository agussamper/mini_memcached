//! @file

#ifndef __BIN_DATA_H__
#define __BIN_DATA_H__

#include <pthread.h>
#include <stdint.h>

#define BINARY 0
#define TEXT 1

typedef struct User_data {
  int fd; // File descriptor
  int mode; // Indica si esta en BINARY O TEXT
  char* buf; 
  uint64_t bufSize; // Tamaño del buffer
  uint64_t offset; // Posición del buffer en
          // la que estoy leyendo
  uint32_t bytesToRead;
          // Contienen la longitud de la 
          // clave o del valor
  uint32_t keySize; // Longitud de la clave
  char kv; // 2 si para la operacion falta
           // leer clave y valor, 1 si sólo
           // falta la clave y 0 si no falta
           // nada
  char kv_const;
  char reading; // 1 si esta leyendo una clave
                // o valor, 0 en caso contrario 
  char readNext; //Usado en readBin
} User_data;

/**
 * Devuelve un puntero a una estrucura User_data
 * con el fd dado y el modo, inicializa
 * ud->buf con NULL.
 * @param fd File descriptor.
 * @param mode aqui se debe indicar el modo 
 * con las constantes BINARY y TEXT.
 * @return
 * Devuelve un puntero a una estrucura User_data
 * inicializada.
*/
User_data* user_data_init(int fd, int mode);

/**
 * Libera ud->buf, pone en 0 ud->offset, ud->size
 * ud->reading, ud->bytesToRead y ud->keySize.
 * @param ud puntero a estructura a reiniciar
*/
User_data* user_data_restart(User_data* ud);

/**
 * Libera ud
 * @param ud estructura a liberar. 
*/
void user_data_destroy(User_data* ud);

#endif