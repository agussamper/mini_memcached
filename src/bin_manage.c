#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include "bin_manage.h"
#include "malloc_interface.h"

#define READSIZE 1000

#define READ(fd, buf, n) ({						\
	int rc = read(fd, buf, n);					\
	int error = errno;  \
  if (rc == 0)  \
    return -1;  \
  if (rc < 0) { \
    if(error == EINVAL || error == EWOULDBLOCK) { \
      puts("user_data_read: EINVAL O EWOULDBLOCK"); \
      return 1; \
    } \
    printf("error in read()! %s\n", strerror(error));      \
	  return -1;  \
  } \
  rc; })

void bin_consume(Cache cache , char* buf, int fd) {
	puts("ESTOY EN BIN CONSUME");
	char comm;
	int i = 0;
	comm = buf[i++];
	printf("comm=%d\n", comm);
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
		} else {
			uint32_t valLen = resp->valSize;
			char code_len[5];
			code_len[0] = OK;
			for(int i = 4; i > 0; i--) {
				code_len[i] = valLen & 0xFF;
				valLen = valLen >> 8;
			}
			write(fd, code_len, 5);
			write(fd, resp->value, resp->valSize);
			free(resp->value);
			free(resp);
		}
		break;
	case STATS:
		puts("EN STATS");
		char* stats = cache_getStats(cache);
		uint32_t len_stats = strlen(stats);
		uint32_t len_stats_m = len_stats;
		char code_len[5];
		code_len[0] = OK;
		for(int i = 4; i > 0; i--) {
			code_len[i] = len_stats_m & 0xFF;
			len_stats_m = len_stats_m >> 8;
		}
		write(fd, code_len, 5);
		write(fd, stats, len_stats);
		free(stats);
		break;
	default:
		char c = EINVALID;
		write(fd,&c,1);
		break;
	}	
}

/**
 * setea 2 en ud->kv
 * si tiene que leer clave valor,
 * 1 en ud->kv y si tiene
 * que leer sólo la clave y 0 ud->kv
 * y si no tiene que
 * dependiendo del código pasado por
 * argumentos. setea -1 
 * si el código pasado no es válido.
*/
int set_kv(User_data* ud) {
  switch (ud->buf[0]) {
  case PUT:
    ud->kv = 2;
    break;
  case DEL:
    ud->kv = 1;
    break;
  case GET:
    ud->kv = 1;
    break;
  case STATS:
    ud->kv = 0;
    break;
  default:
    return -1;
    break;
  }
}

void get_bytesToRead(User_data* ud) {
  char buflen[4];
  if(ud->offset > 1) {
    if(ud->offset >= ud->keySize + 9) {
      ud->bytesToRead = ud->keySize;
      memcpy(buflen, ud->buf+ud->keySize+5, 4);
    } else {
      int rc = read(ud->fd, buflen, 4); //TODO: usar macro
      memcpy(ud->buf+ud->offset, buflen, 4);
      ud->offset += rc;  
    }
  } else {
    int rc = read(ud->fd, buflen, 4); //TODO: usar macro
    memcpy(ud->buf+ud->offset, buflen, 4);
    ud->offset += rc;
  }  
  uint32_t aux = 0;
  char* ptr = (char*)&aux;
  int j = 3;
  for(int i = 0; i < 3; i++) {
    ptr[i] = buflen[j--];
  }
  ud->bytesToRead = aux;
}

int getRest(User_data* ud) {
  if(ud->kv_const == 2) {
    if(ud->kv == 2) {
      return 5;
    } else if(ud->kv == 1) {
      return 9+ud->keySize;
    } else {
      return -1;
    }
  } else if(ud->kv_const == 1) {
    return 5;
  }
  return -1;
}

int readBin(User_data* ud) {
  if(ud->buf == NULL) {
    ud->bufSize=2000;
    ud->buf =
      allocate_mem(ud->bufSize, NULL); 
    ud->offset = 0;
    int rc = READ(ud->fd, ud->buf, 1);
    ud->offset+=1;
    ud->readNext = 1;
    set_kv(ud);
    if(ud->kv == 0) {
      return 0;
    }
    ud->kv_const = ud->kv;
  }
  int rc = 0;
  while(1) {  
    if(!ud->reading) {
      get_bytesToRead(ud);
      ud->reading = 1;
      if(ud->kv == 2) {
        ud->keySize = ud->bytesToRead;
      }
    }
    if(ud->offset + READSIZE > ud->bufSize) {
      ud->bufSize *= 2;
      ud->buf = realloc(ud->buf, ud->bufSize); //TODO: crear realloc en allocate_mem
      assert(ud->buf);
    }
    if(ud->readNext) {
      rc = READ(ud->fd, ud->buf+ud->offset, READSIZE);
      ud->offset += rc;
    } 
    if(ud->offset-getRest(ud) >= ud->bytesToRead) {
      if(ud->kv == 1) {
        return 0;
      } else {
        ud->kv--;
        ud->reading = 0;
        if(rc < READSIZE) {
          ud->readNext = 0;
        }
      }
    } 
  }
}