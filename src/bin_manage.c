#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
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

int bin_consume(Cache cache ,int fd) {
	puts("ESTOY EN BIN CONSUME");
	char comm;
	int rc = READ(fd,&comm,1);
  char buf[4];
  char* key;
  int lenk;
	switch (comm)
	{
	case PUT:
		rc = READ(fd,buf,4);
		if(rc < 4){
			char response = EINVALID;
			write(fd,&response,1);
			return 0;
		}
	  lenk = ntohl(*(int*)buf);
		key = allocate_mem(lenk+1, NULL);
		rc = READ(fd,key,lenk);
		if(rc != lenk){
			char response = EINVALID;
			write(fd,&response,1);
			printf("keylen error lenk: %d actual:%d\n",
				lenk, rc);
			free(key);
			return 0;
		}
		rc = READ(fd,buf,4);
		if(rc < 4){
			char response = EINVALID;
			write(fd,&response,1);
			free(key);
			return 0;
		}
		int lenv = ntohl(*(int*)buf);
		char* value = allocate_mem(lenv+1,NULL);
		rc = READ(fd,value,lenv);
		if(rc != lenv){
			char response = EINVALID;
			write(fd,&response,1);
			printf("vallen error lenv: %d actual:%d\n",
				lenv, rc);
			free(key);
			free(value);
			return 0;
		}
		if(cache_insert(cache, key,
				lenk, value, lenv, 1)){
			char response = OK;
			write(fd,&response,1);
		}
		else{
			char response = EINVALID;
			write(fd,&response,1);
			printf("insert error\n");	
		}
		free(key);
		free(value);
		break;
	case DEL:
		rc = READ(fd,buf,4);
		if(rc < 4){
			char response = EINVALID;
			write(fd,&response,1);
			return 0;
		}
		lenk = ntohl(*(int*)buf);
		key = malloc(lenk+1);
		rc = READ(fd,key,lenk);
		if(rc != lenk){
			free(key);
			char response = EINVALID;
			write(fd,&response,1);
			printf("keylen error lenk: %d actual:%d\n",
				lenk,rc);
			return 0;
		}
		if(cache_delete(cache,key,lenk)){
			char response = OK;
			write(fd,&response,1);
		}
		else{
			char response = ENOTFOUND;
			write(fd,&response,1);
		}
		free(key);
		break;
	case GET:    
		rc = READ(fd,buf,4);
		if(rc < 4){
			char response = EINVALID;
			write(fd,&response,1);
			return 0;
		}
		lenk = ntohl(*(int*)buf);
		key = malloc(lenk+1);
		rc = READ(fd,key,lenk);
		if(rc != lenk){
			char response = EINVALID;
			write(fd,&response,1);
			printf("keylen error lenk: %d actual:%d\n",
				lenk,rc);
			free(key);
			return 0;
		}
    ValData* resp = cache_get(cache, 
			key, lenk);
    if(NULL == resp){
      char response = ENOTFOUND;
      write(fd,&response,1);
			free(key);
      return 0;
    }
		uint32_t bigLen = resp->valSize;		
    long len = bigLen + 5;
    char* response = malloc(len);
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
		uint64_t* stats = cache_getStats(cache);
		char* stats_msj = malloc(33);
		stats_msj[0] = OK;
		for(int i = 0; i < 4; i++){
			uint64_t bignum = stats[i];
			for(int j = 8; j>0;j--){
				stats_msj[(i*8)+j] = bignum & 0xFF;
				bignum = bignum >>8;
			}
		}
		break;
	default:
		char c = EINVALID;
		write(fd,&c,1);
		return 0;
		break;
	}
  return 0;	
}