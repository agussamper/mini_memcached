#include "bin_data.h"
#include "malloc_interface.h"
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <arpa/inet.h>

#define READSIZE 1000

Bin_data* bin_data_init(int fd) {
  Bin_data* bd = 
    allocate_mem(sizeof(Bin_data), NULL);
  bd->fd = fd;
  bd->buf = NULL;
  bd->bufSize = 0;
  bd->offset = 0;
  bd->bytesToRead = 0;
  assert(!pthread_mutex_init(
    &bd->r_mutex, NULL));
  return bd;
}

Bin_data* bin_data_restart(Bin_data* bd) {
  pthread_mutex_lock(&bd->r_mutex);
  if(bd->buf !=NULL) {
    free(bd->buf);
    bd->buf = NULL;
  }
  bd->bufSize = 0;
  bd->offset = 0;
  bd->bytesToRead = 0;
  pthread_mutex_unlock(&bd->r_mutex);
  return bd;
}

void bin_data_destroy(Bin_data* bd) {
  pthread_mutex_lock(&bd->buf);
  if(bd->buf !=NULL) {
    free(bd->buf);
    bd->buf = NULL;
  }
  pthread_mutex_unlock(&bd->buf);
  pthread_mutex_destroy(&bd->r_mutex);
}

int bin_data_read(Bin_data* bd, int fd) {
  pthread_mutex_lock(&bd->r_mutex);  
  if(bd->buf == NULL) {
    bd->bufSize=2000;
    bd->buf =
      allocate_mem(bd->bufSize, NULL); 
    bd->offset = 0;
    char buflen[8];   
    read(fd, buflen, 8);
    char* ptr = (char*)&bd->bytesToRead;
    int j = 7;
    for(int i = 0; i < 8; i++) {
      ptr[i] = buflen[j--];
    }
  }
  printf("toRead=%d, offset=%d\n", bd->bytesToRead, bd->offset);
  int rc;
  int stop = 0;
	while (stop != 1) {    
    if(bd->offset + READSIZE > bd->bufSize) {
      bd->bufSize *= 2;
      printf("%d\n", bd->bufSize);
      bd->buf = realloc(bd->buf, bd->bufSize); //TODO: crear realloc en allocate_mem
      assert(bd->buf);
    }
    assert(bd->buf);    
		rc = read(fd, bd->buf+bd->offset, READSIZE);    
    int error = errno;
		if (rc <= 0) {
      if(rc < 0) {
        printf("error in read()! %s\n", strerror(error));
      }
      printf("offset=%d, sizebuf=%d\n", bd->offset, bd->bufSize);
      printf("rc=%d\n", rc);
      pthread_mutex_unlock(&bd->r_mutex);
			return -1;
    }
    bd->offset += rc;
    printf("offset=%d, sizebuf=%d\n", bd->offset, bd->bufSize);
    if(rc < READSIZE) {
      puts("STOP");
      stop=1;
    }		
	}
  pthread_mutex_unlock(&bd->r_mutex); 
  return 0;
}