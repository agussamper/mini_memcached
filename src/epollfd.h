#ifndef __EPOLLFD_H__
#define __EPOLLFD_H__

#include "bin_data.h"

typedef struct _epollfd{
	// bd es NULL si es texto
	// de lo contrario bin
	Bin_data* bd;
	int fd;
} epollfd;

epollfd epfd_copy(epollfd fd);

void epfd_dstr(epollfd fd);

#endif