#ifndef __BIN_MANAGE_H__
#define __BIN_MANAGE_H__

#include <stdio.h>
#include "common.h"
#include "cache.h"

void bin_consume(Cache cache , char* buf, int fd);

#endif