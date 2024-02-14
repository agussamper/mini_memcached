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
  ud->mode = mode;
  ud->buf = NULL;
  ud->bufSize = 0;
  ud->offset = 0;
  ud->reading = 0;
  return ud;
}

User_data* user_data_restart(User_data* ud) {
  if(ud->buf !=NULL) {
    free(ud->buf);
    ud->buf = NULL;
  }
  ud->bufSize = 0;
  ud->offset = 0;
  ud->reading = 0;
  return ud;
}

void user_data_destroy(User_data* ud) {
  if(ud->buf !=NULL) {
    free(ud->buf);
    ud->buf = NULL;
  }
  free(ud);
}