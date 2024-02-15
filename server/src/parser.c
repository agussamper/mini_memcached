#define _GNU_SOURCE /* strchrnul */

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include "parser.h"

int text_parser(const char *buf, char *toks[3], int lens[3])
{
	int ntok;

	/* Separar tokens */
	{
		char *p = (char*)buf;
		ntok = 0;
		toks[ntok++] = p;
		while (ntok < 10 && (p = strchrnul(p, ' ')) && *p) {
			/* Longitud token anterior */
			lens[ntok-1] = p - toks[ntok-1];
			*p++ = 0;
			/* Comienzo nueva token */
			toks[ntok++] = p;
		}
		lens[ntok-1] = p - toks[ntok-1];
	}
	return ntok;
}


