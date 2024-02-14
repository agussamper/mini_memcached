#ifndef __BIN_DATA_H__
#define __BIN_DATA_H__

#include <pthread.h>
#include <stdint.h>

#define BINARY 0
#define TEXT 1

typedef struct User_data {
  int fd;
  int mode;
  char* buf;
  uint64_t bufSize;
  uint64_t offset;
  uint32_t bytesToRead;
  uint32_t keySize;
  char kv; // 2 si para la operacion falta
           // leer clave y valor, 1 si sólo
           // falta la clave y 0 si no falta
           // nada
  char kv_const;
  char reading; // 1 si esta leyendo una clave
                // o valor, 0 en caso contrario 
  char readNext;
} User_data;

User_data* user_data_init(int fd, int mode);

/**
 * Reinicia todo menos el mutex
 * y el file descriptor
*/
User_data* user_data_restart(User_data* bd);

void user_data_destroy(User_data* bd);

int user_data_read(User_data* bd);

#endif