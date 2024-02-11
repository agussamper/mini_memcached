#include "user_data.h"
#include "malloc_interface.h"
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <arpa/inet.h>

#define READSIZE 1000

User_data* user_data_init(int fd, int mode) {
  User_data* ud = 
    allocate_mem(sizeof(User_data), NULL);
  ud->fd = fd;
  ud->mode = mode;
  ud->buf = NULL;
  ud->bufSize = 0;
  ud->offset = 0;
  ud->bytesToRead = 0;
  return ud;
}

User_data* user_data_restart(User_data* bd) {
  if(bd->buf !=NULL) {
    free(bd->buf);
    bd->buf = NULL;
  }
  bd->bufSize = 0;
  bd->offset = 0;
  bd->bytesToRead = 0;
  return bd;
}

void user_data_destroy(User_data* ud) {
  if(ud->buf !=NULL) {
    free(ud->buf);
    ud->buf = NULL;
  }
  free(ud);
}

int user_data_read(User_data* bd) {
  int fd = bd->fd;
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
      return -1;
    }
		if (rc < 0) {
      if(error == EINVAL || error == EWOULDBLOCK) {
        puts("user_data_read: EINVAL O EWOULDBLOCK");
        return 1;
      }
      printf("error in read()! %s\n", strerror(error));      
			return -1;
    }
    bd->offset += rc;
    if(rc < READSIZE) {
      printf("OFFSET=%d\n",bd->offset);
      puts("STOP");
      stop=1;
      if(bd->offset >= bd->bytesToRead) {
        return 0;
      }
      return 1;
    }		
	}   
}