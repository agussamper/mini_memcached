//! @file

#ifndef __BIN_DATA_H__
#define __BIN_DATA_H__

#include <pthread.h>
#include <stdint.h>
#include <time.h>

#define BINARY 0
#define TEXT 1

struct User_data;

typedef struct Timerfd {
  int timefd;
  struct User_data* ud;
} Timerfd;

typedef struct User_dataBin {
  Timerfd* tfd;
  uint64_t bufSize; // Tamaño del buffer
  uint32_t bytesToRead;
          // Contienen la longitud de la 
          // clave o del valor
  uint32_t keySize; // Longitud de la clave
  char kv; // 2 si para la operacion falta 
           // leer clave y valor, 1 si sólo
           // falta la clave y 0 si no falta
           // nada
  char kv_const; 
          // Debe tener el mismo valor que en
          // la primera asignación a kv y debe
          // ser constante 
  char reading; // 1 si esta leyendo una clave
                // o valor, 0 en caso contrario 
  
  char prevRead; // Guarda lo último retornado por
                // por readBin
} User_dataBin;

typedef struct User_data {  
  User_dataBin* udBin; // Estructura necesaria
    // para usuario en modo binario, además
    // si su valor es NULL indica que está en
    // modo texto 
  char* buf;  
  uint64_t offset; // Posición del buffer en
          // la que estoy leyendo
  int fd; // File descriptor.
  char readNext; // Usado en readBin para decidir si hay
                 // que leer la entrada
                 // usado en text_manage para representar
                 // el maximo de EBIG permitidos
} User_data;

/**
 * Devuelve un puntero a una estrucura User_data
 * con el fd dado y el modo, inicializa
 * ud->buf con NULL.
 * Si está en modo binario se iniciliza
 * ud->udBin->prevRead en 0
 * @param fd File descriptor.
 * @param mode aqui se debe indicar el modo 
 * con las constantes BINARY y TEXT.
 * @return
 * Devuelve un puntero a una estrucura User_data
 * inicializada.
*/
User_data* user_data_init(int fd, int mode);

/**
 * Libera ud->buf, pone en 0 ud->offset, 
 * ud->udBin->size, ud->udBin->reading,
 * ud->udBin->bytesToRead, 
 * ud->udBin->keySize y ud->udBin->prevRead.
 * @param ud puntero a estructura a reiniciar
*/
User_data* user_data_restart(User_data* ud);

/**
 * Libera ud
 * @param ud estructura a liberar. 
*/
void user_data_destroy(User_data* ud);

#endif