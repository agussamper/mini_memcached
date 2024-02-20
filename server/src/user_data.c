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

/**
 * Función auxiliar
 * setea 2 en ud->udBin->kv
 * si tiene que leer clave valor,
 * 1 en ud->udBin->kv y si tiene
 * que leer sólo la clave y 0 ud->udBin->kv
 * y si no tiene que
 * dependiendo del código pasado por
 * argumentos. setea -1 si el código
 * pasado no es válido.
*/
void set_kv(User_data* ud) {
  switch (ud->buf[0]) {
  case PUT:
    ud->udBin->kv = 2;
    break;
  case DEL:
    ud->udBin->kv = 1;
    break;
  case GET:
    ud->udBin->kv = 1;
    break;
  case STATS:
    ud->udBin->kv = 0;
    break;
  default:
    ud->udBin->kv = -1;
    break;
  }
}

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

User_data* user_data_adjust(User_data* ud) {
  uint64_t procesed = 1;
  if(ud->udBin->kv_const == 2){
    procesed =  (ud->udBin->keySize + ud->udBin->bytesToRead + 9);
  }else if(ud->udBin->kv_const == 1){
    procesed =  (ud->udBin->bytesToRead + 5);
  }
  uint64_t dif = ud->offset - procesed;
  printf("procesed: %ld\noffset: %ld\ndif: %ld\n",procesed,ud->offset,dif);
  if(dif == 0) return user_data_restart(ud);
  char* new_buf = allocate_mem(dif + 2000,NULL);
  memcpy(new_buf,ud->buf+procesed,dif);
  free(ud->buf);
  ud->buf = new_buf;
  ud->udBin->bufSize = dif + 2000;
  ud->udBin->reading = 0;
  ud->udBin->bytesToRead = 0;
  ud->udBin->keySize = 0;
  ud->udBin->prevRead = 0;
  ud->offset = dif;
  set_kv(ud);
  ud->udBin->kv_const = ud->udBin->kv;
  ud->readNext = 2;
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