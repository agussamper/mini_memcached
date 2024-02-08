#include "bin_data.h"
#include "malloc_interface.h"
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

#define READSIZE 1000

Bin_data* bin_data_init(int fd) {
  Bin_data* bd = 
    allocate_mem(sizeof(Bin_data), NULL);
  bd->fd = fd;
  bd->reading = 0;
  assert(!pthread_mutex_init(
    &bd->r_mutex, NULL));
  return bd;
}

char* bin_data_read(Bin_data* bd, int* bytesRead, int fd) {
  pthread_mutex_lock(&bd->r_mutex);
  bd->reading = 1;
  pthread_mutex_unlock(&bd->r_mutex);
  size_t bufSize=2000;
  char* buf =
    allocate_mem(bufSize, NULL);    
  int i = 0, rc;
  int stop = 0;
	while (stop != 1) {
    if(i + READSIZE > bufSize) {
      bufSize *= 2;
      buf = realloc(buf, bufSize); //TODO: crear realloc en allocate_mem
    }
		rc = read(fd, buf+i, READSIZE);
		if (rc <= 0) {
      printf("Oh dear, something went wrong with read()! %s\n", strerror(errno));
      printf("rc=%d\n", rc);
      free(buf);
			return NULL;
    }
    if(rc < READSIZE) {
      puts("STOP");
      stop = 1;
    }
		i += rc;
	}
  *bytesRead = i;
  pthread_mutex_lock(&bd->r_mutex);
  bd->reading = 1;
  pthread_mutex_unlock(&bd->r_mutex);
	return buf;  
}