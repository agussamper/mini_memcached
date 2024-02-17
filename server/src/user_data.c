#include "user_data.h"
#include "malloc_interface.h"
#include "common.h"
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <arpa/inet.h>

User_data* user_data_init(int fd, int mode) {
  User_data* ud = 
    allocate_mem(sizeof(User_data), NULL);
  ud->fd = fd;
  ud->buf = NULL;
  ud->offset = 0;
  if(mode == BINARY) {
    User_dataBin* udBin =
      allocate_mem(sizeof(User_dataBin), NULL);
    ud->udBin = udBin;
    ud->udBin->prevRead = 0;
  } else {
    ud->udBin = NULL;
  }
  return ud;
}

User_data* user_data_restart(User_data* ud) {
  if(NULL != ud->buf) {
    free(ud->buf);
    ud->buf = NULL;
  }
  ud->offset = 0;
  if(NULL != ud->udBin) {
    ud->udBin->bufSize = 0;  
    ud->udBin->reading = 0;
    ud->udBin->bytesToRead = 0;
    ud->udBin->keySize = 0;
    ud->udBin->prevRead = 0;
  }
  return ud;
}

void user_data_destroy(User_data* ud) {
  if(ud->buf !=NULL) {
    free(ud->buf);    
  }
  if(ud->udBin != NULL) {
    free(ud->udBin);
  }
  free(ud);
}