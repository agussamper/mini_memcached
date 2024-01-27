#ifndef __COMMON_H
#define __COMMON_H 1

#include <netinet/ip.h>
#include <stdio.h>
#include <stdlib.h>
//#include "log.h"
/*
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

*/
int valid_rq(int code);




 void quit(char *s);

#define STATIC_ASSERT(p)			\
	int _ass_ ## __LINE__ [(!!(p)) - 1];

//const char * error_str(enum code e);

#endif