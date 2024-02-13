#ifndef __TEXT_MANAGE_H__
#define __TEXT_MANAGE_H__

#include <sys/epoll.h>
#include "cache.h"

int text_consume(Cache cache,
  int fd, char buf[], u_int64_t* offset);

#endif