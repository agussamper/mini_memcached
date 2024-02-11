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
  uint64_t bytesToRead; 
} User_data;

User_data* user_data_init(int fd, int mode);

/**
 * Reinicia todo menos el mutex
 * y el file descriptor
*/
User_data* user_data_restart(User_data* bd);

void user_data_destroy(User_data* bd);

/**
 * Retorna menos uno si hubo un error o el
 * usuario se desconecto, 0 si leyó 
 * correctamente y 1 si todavía le falta
 * leer parte del paquete
*/
int user_data_read(User_data* bd);

#endif