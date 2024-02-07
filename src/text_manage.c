#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include "text_manage.h"
#include "parser.h"

#define READ(fd, buf, n) ({						\
	int rc = read(fd, buf, n);				\
	if (rc < 0)	\
		return -2;						\
	if (rc == 0)						\
		return -1;	\
	rc; })

void text_handle(
    Cache cache, int fd,
 		char *toks[3], int lens[3],
		int ntok){
	if(!strcmp(toks[0],"PUT")){
		if(ntok !=3){
			char response[20];
			int len = sprintf(response,
				"EINVAL NTOK %d\n",ntok);
			write(fd,response,len);
			return;
		} 
		cache_insert(cache,
			toks[1], lens[1], 
			toks[2], lens[2], 0);
		write(fd,"OK\n",3);
		return;
	}
	else if(!strcmp(toks[0],"GET")){
		if(ntok != 2){
			write(fd,"EINVAL\n",7);
			return;
		}
		ValData* val = cache_get(cache,
		 	toks[1], strlen(toks[1]));
		if(val == NULL){
			write(fd,"ENOTFOUND\n",10);
			return;
		}
		if(val->isBin){
			write(fd,"EBINARY\n",8);
			return;
		}
		char res[2024];
		sprintf(res,"OK %s\n",val->value);
		write(fd,res,strlen(res));
		free(val->value);
		free(val);
		return;
	}
	else if(!strcmp(toks[0],"DEL")){
		if(ntok != 2){
			write(fd,"EINVAL\n",7);
			return;
		}
		int res = cache_delete(cache, 
			toks[1], strlen(toks[1]));
		if(res){
			write(fd,"OK\n",3);		
		}else{
			write(fd,"ENOTFOUND\n",10);
		}
		return;
	}
	else if(!strcmp(toks[0],"STATS")){
		if(ntok != 1){
			write(fd,"EINVAL\n",7);
			return;
		}
		char* stats = cache_getStats(cache);
		write(fd,stats,strlen(stats));
		free(stats);
		return;
	}else{
			write(fd,"EINVAL\n",7);
			return;
	}
}

/* 0: todo ok, continua. -1 errores */
int text_consume(Cache cache, int fd, char buf[]) { 
	int blen = 1;
	write(fd,"YA TE ATIENDO\n",14);
	int nread = READ(fd,buf,2048);
	blen += nread;
	char *p, *p0 = buf;
	int nlen = blen;
	/* Para cada \n, procesar, y avanzar punteros */
	while ((p = memchr(p0, '\n', nlen)) != NULL) {
	/* Mensaje completo */
		int len = p - p0;
		*p++ = 0;
        //log(3, "full command: <%s>", p0);
		char *toks[3]= {NULL};
		int lens[3] = {0};
		int ntok;
		ntok = text_parser(buf,toks,lens);

		text_handle(cache,fd,
      toks,lens,ntok);
		nlen -= len + 1;
		p0 = p;
	}

	/* Si consumimos algo, mover */
	if (p0 != buf) {
		memmove(buf, p0, nlen);
		blen = nlen;
	}else if(blen == 2048){
		write(fd,"EBIG\n",5);
		return -1;
	}
	while (1) {
		int rem = sizeof *buf - blen;
		if(rem < 0) return -1;
		/* Buffer lleno, no hay comandos, matar */
		if (rem == 0)
			return 0;
		printf("REM: %d\n",rem);
		int nread = READ(fd, buf + blen, rem);
		printf("a\n");
		//log(3, "Read %i bytes from fd %i", nread, fd);
		blen += nread;
		char *p, *p0 = buf;
		int nlen = blen;

		/* Para cada \n, procesar, y avanzar punteros */
		while ((p = memchr(p0, '\n', nlen)) != NULL) {
			/* Mensaje completo */
			int len = p - p0;
			*p++ = 0;
            //log(3, "full command: <%s>", p0);
			char *toks[3]= {NULL};
			int lens[3] = {0};
			int ntok;
			ntok = text_parser(buf,toks,lens);

			text_handle(cache, fd,
        toks,lens,ntok);
			nlen -= len + 1;
			p0 = p;
		}

		/* Si consumimos algo, mover */
		if (p0 != buf) {
			memmove(buf, p0, nlen);
			blen = nlen;
		} else if(blen == 2048){
			write(fd,"EBIG\n",5);
			return -1;
		}
	}
	return 0;
}