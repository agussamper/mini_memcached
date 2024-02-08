#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>
#include "bin_manage.h"
#include "malloc_interface.h"

int readn(int fd, void *buf, int len)
{
	int i = 0, rc;
	puts("en readn");
	while (i < len) {
		printf("i=%d len=%d\n", i, len);
		rc = read(fd, buf + i, len - i);
		if (rc <= 0)
			return rc;
		i += rc;
	}
	printf("rc=%d\n", rc);
	return rc;
}

/* Macro interna */
#define READ(fd, buf, n) ({						\
	int rc = readn(fd, buf, n);				\
	if (rc < 0)	\
		return -2;						\
	if (rc == 0)						\
		return -1;	\
	rc; })

void bin_consume(Cache cache , char* buf, int fd) {
	puts("ESTOY EN BIN CONSUME");
	char comm;
	int i = 0;
	comm = buf[i++];
  char buflen[4];
	int lenk;
	switch (comm)
	{
	case PUT:
		puts("ENTRO A PUT");
		memcpy(buflen, buf+i, 4);
		i += 4;
		int startKey = i;
	  lenk = ntohl(*(int*)buflen);
		i+=lenk;
		memcpy(buflen,buf+i,4);
		i+=4;
		int startVal = i;
		int lenv = ntohl(*(int*)buflen);
		if(cache_insert(cache,
				buf+startKey, lenk,
				buf+startVal, lenv, 1)){
			char response = OK;
			puts("ESCRIBO RESPUESTA");
			write(fd,&response,1);
		}
		else{
			char response = EINVALID;
			write(fd,&response,1);
			printf("insert error\n");	
		}
		puts("SALGO PUT");
		break;
	case DEL:
		memcpy(buflen, buf+i, 4);
		i+=4;
		lenk = ntohl(*(int*)buflen);
		if(cache_delete(cache,
				buf+i,lenk)){
			char response = OK;
			write(fd,&response,1);
		}
		else{
			char response = ENOTFOUND;
			write(fd,&response,1);
		}
		break;
	case GET:    
		memcpy(buflen, buf+i, 4);
		i+=4;
		lenk = ntohl(*(int*)buflen);
    ValData* resp = 
			cache_get(cache, buf+i, lenk);
    if(NULL == resp){
      char response = ENOTFOUND;
      write(fd,&response,1);
    }
		uint32_t bigLen = resp->valSize;		
    long len = bigLen + 5;
    char* response = allocate_mem(len,NULL);
    response[0] = OK;
		for(int i = 4; i > 0; i--) {
			response[i] = bigLen & 0xFF;
			bigLen = bigLen >> 8;
		}
		memcpy(response+5,
			resp->value, resp->valSize);
    write(fd,response,len);
		free(response);
		free(resp->value);
		free(resp);
		break;
	case STATS:
		char* stats = cache_getStats(cache);
		uint32_t len_stats = strlen(stats);
		uint32_t len_msj = len_stats+5;
		char* stats_msj = malloc(len_msj);
		stats_msj[0] = OK;
		for(int i = 4; i > 0; i--) {
			stats_msj[i] = len_stats & 0xFF;
			len_stats = len_stats >> 8;
		}
		strcpy(stats_msj+5,stats);
		write(fd,stats_msj,len_msj);
		free(stats_msj);
		free(stats);
		break;
	default:
		char c = EINVALID;
		write(fd,&c,1);
		break;
	}	
}