#ifndef __BIN_MANAGE_H__
#define __BIN_MANAGE_H__

#include <stdio.h>
#include "common.h"
#include "cache.h"
#include "epollfd.h"

int bin_consume(Cache cache , epollfd* efd);

#endif