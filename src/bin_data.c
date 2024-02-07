#include "bin_data.h"
#include "malloc_interface.h"

Bin_data* bin_data_init() {
  Bin_data* bd = 
    allocate_mem(sizeof(Bin_data), NULL);
  bd->reading = 0;
  pthread_mutex_init(
    &(bd->buf_mutex), NULL);
  return bd;
}

Bin_data bin_data_setReading(
    Bin_data* bd, int set) {
  pthread_mutex_lock(&bd->buf_mutex);
  bd->reading = set;
  pthread_mutex_unlock(&bd->buf_mutex);
}

int bin_data_isReading(
    Bin_data* bd) {
  return bd->reading;
}