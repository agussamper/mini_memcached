#ifndef __BIN_DATA_H__
#define __BIN_DATA_H__

#include <pthread.h>
#include <stdint.h>

typedef struct Bin_data {
  int fd;
  int reading;    
  char* buf;
  uint64_t bufSize;
  uint64_t offset;
  uint64_t bytesToRead;
  pthread_mutex_t r_mutex;  
} Bin_data;

Bin_data* bin_data_init(int fd);

int bin_data_read(Bin_data* bd,
  int fd);

#endif