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
  //bd->bytesToRead = 0;
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
  pthread_mutex_lock(&bd->r_mutex);
  if(bd->buf !=NULL) {
    free(bd->buf);
    bd->buf = NULL;
  }
  pthread_mutex_unlock(&bd->r_mutex);
  pthread_mutex_destroy(&bd->r_mutex);
}

int bin_data_read(Bin_data* bd) {
  puts("QUIERO EL MUTEX");
  pthread_mutex_lock(&bd->r_mutex);
  puts("MUTEX TOMADO");
  int fd = bd->fd;
  if(bd->buf == NULL) {
    bd->bufSize=2000;
    bd->buf =
      allocate_mem(bd->bufSize, NULL); 
    puts("buf inicializado");
    bd->offset = 0;
    char buflen[8];   
    read(fd, buflen, 8);
    char* ptr = (char*)&bd->bytesToRead;
    int j = 7;
    for(int i = 0; i < 8; i++) {
      ptr[i] = buflen[j--];
    }
  }
  int rc;
  int stop = 0;
	while (stop != 1) {    
    if(bd->offset + READSIZE > bd->bufSize) {
      bd->bufSize *= 2;
      bd->buf = realloc(bd->buf, bd->bufSize); //TODO: crear realloc en allocate_mem
      assert(bd->buf);
    }
    assert(bd->buf); 
		rc = read(fd, bd->buf+bd->offset, READSIZE);    
    int error = errno;
    if (rc == 0) {
      puts("MENOS 1");
      pthread_mutex_unlock(&bd->r_mutex);
      return -1;
    }
		if (rc < 0) {
      if(error == EINVAL || error == EWOULDBLOCK) {
        puts("EINVAL O EWOULDBLOCK");
        pthread_mutex_unlock(&bd->r_mutex);
        return 1;
      }
      printf("error in read()! %s\n", strerror(error));      
      pthread_mutex_unlock(&bd->r_mutex);
			return -1;
    }
    bd->offset += rc;
    if(rc < READSIZE) {
      printf("OFFSET=%d\n",bd->offset);
      puts("STOP");
      stop=1;
      if(bd->offset >= bd->bytesToRead) {
        pthread_mutex_unlock(&bd->r_mutex);
        return 0;
      }
      pthread_mutex_unlock(&bd->r_mutex);
      return 1;
    }		
	}   
}