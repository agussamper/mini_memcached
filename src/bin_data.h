#ifndef __BIN_DATA_H__
#define __BIN_DATA_H__

#include <pthread.h>
#include <stdint.h>

typedef struct Bin_data {
  int fd;
  char* buf;
  uint64_t bufSize;
  uint64_t offset;
  uint64_t bytesToRead;
  pthread_mutex_t r_mutex;  
} Bin_data;

Bin_data* bin_data_init(int fd);

/**
 * Reinicia todo menos el mutex
 * y el file descriptor
*/
Bin_data* bin_data_restart(Bin_data* bd);

void bin_data_destroy(Bin_data* bd);

int bin_data_read(Bin_data* bd,
  int fd);

#endif