#ifndef __BIN_DATA_H__
#define __BIN_DATA_H__

#include <pthread.h>

typedef struct Bin_data {
  int reading;
  pthread_mutex_t buf_mutex;  
} Bin_data;

Bin_data* bin_data_init();

Bin_data bin_data_setReading(
  Bin_data* bd, int set);

#endif