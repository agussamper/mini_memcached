//! @file

#ifndef __COMMON_H
#define __COMMON_H 1

#include <netinet/ip.h>
#include <stdio.h>
#include <stdlib.h>

#define READ(fd, buf, n) ({						\
	int rc = read(fd, buf, n);					\
	int error = errno;  \
  if (rc == 0)  \
    return -1;  \
  if (rc < 0) { \
    if(error == EINVAL || error == EWOULDBLOCK) { \
      puts("user_data_read: EINVAL O EWOULDBLOCK"); \
      return 1; \
    } \
    printf("error in read()! %s\n", strerror(error));      \
	  return -1;  \
  } \
  rc; })

enum code {
	PUT = 11,
	DEL = 12,
	GET = 13,

	STATS = 21,

	OK = 101,
	EINVALID = 111,
	ENOTFOUND = 112,
	EBINARY = 113,
	EBIG = 114,
	EUNK = 115,
	EOOM = 116,
};

void quit(char *s);

#define STATIC_ASSERT(p)			\
	int _ass_ ## __LINE__ [(!!(p)) - 1];

#endif