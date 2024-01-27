#define _GNU_SOURCE /* strchrnul */

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include "parser.h"



/* Macro interna */
#define READ(fd, buf, n) ({						\
	int rc = read(fd, buf, n);					\
	if (rc < 0 && (errno == EAGAIN || errno == EWOULDBLOCK))	\
		return 0;						\
	if (rc <= 0)							\
		return -1;						\
	rc; })



int text_parser(const char *buf, char *toks[3], int lens[3])
{
//	char *toks[10];
//	int lens[10];
	int ntok;

	//log(3, "pasrse(%s)", buf);

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

	//log(3, "checking '%s', ntok = %i", toks[0], ntok);
	return ntok;

}


