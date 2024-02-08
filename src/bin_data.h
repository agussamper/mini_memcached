#ifndef __BIN_DATA_H__
#define __BIN_DATA_H__

#include <pthread.h>
#include <stdint.h>

typedef struct Bin_data {
  int fd;
  int reading;    
  pthread_mutex_t r_mutex;  
} Bin_data;

Bin_data* bin_data_init(int fd);

char* bin_data_read(Bin_data* bd,
  int* bytesRead, int fd);

#endif