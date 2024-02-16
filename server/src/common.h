//! @file

#ifndef __COMMON_H
#define __COMMON_H 1

#include <netinet/ip.h>
#include <stdio.h>
#include <stdlib.h>

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