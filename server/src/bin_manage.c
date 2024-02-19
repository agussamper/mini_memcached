#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include "bin_manage.h"
#include "malloc_interface.h"

#define READSIZE 1000

void bin_consume(Cache cache , char* buf, int fd) {
	char comm;
	uint64_t i = 0;
	comm = buf[i++];
	char buflen[4];
	uint32_t lenk;
	switch (comm)
	{
	case PUT:
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
			write(fd,&response,1);
		}
		else{
			char response = EBIG;
			write(fd,&response,1);
			printf("insert error\n");	
		}
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
    puts("COMM DESCONOCIDO");
		char c = EINVALID;
		write(fd,&c,1);
		break;
	}	
}

/**
 * Función auxiliar
 * setea 2 en ud->udBin->kv
 * si tiene que leer clave valor,
 * 1 en ud->udBin->kv y si tiene
 * que leer sólo la clave y 0 ud->udBin->kv
 * y si no tiene que
 * dependiendo del código pasado por
 * argumentos. setea -1 si el código
 * pasado no es válido.
*/
void set_kv(User_data* ud) {
  switch (ud->buf[0]) {
  case PUT:
    ud->udBin->kv = 2;
    break;
  case DEL:
    ud->udBin->kv = 1;
    break;
  case GET:
    ud->udBin->kv = 1;
    break;
  case STATS:
    ud->udBin->kv = 0;
    break;
  default:
    ud->udBin->kv = -1;
    break;
  }
}

/**
 * Función auxiliar
 * Obtiene la longitud de la clave o del
 * valor dependiendo del valor de los
 * parámetros en ud
*/
int get_bytesToRead(User_data* ud) {
  char buflen[4];
  if(ud->offset > 1) {
    if(ud->offset >= ud->udBin->keySize + 9) {
      ud->udBin->bytesToRead = ud->udBin->keySize;
      memcpy(buflen,
        ud->buf+ud->udBin->keySize+5, 4);
    } else {
      int rc = READ(ud->fd, buflen, 4);
      memcpy(ud->buf+ud->offset, buflen, 4);
      ud->offset += rc;  
    }
  } else {
    int rc = READ(ud->fd, buflen, 4);
    memcpy(ud->buf+ud->offset, buflen, 4);
    ud->offset += rc;
  }  
  uint32_t aux = 0;
  char* ptr = (char*)&aux;
  int j = 3;
  for(int i = 0; i < 3; i++) {
    ptr[i] = buflen[j--];
  }
  ud->udBin->bytesToRead = aux;
  return 0;
}

/**
 * Función auxiliar
 * Obtiene cuánto restar al offset para
 * compararlo con ud->bytesToRead en
 * readBin 
*/
int getRest(User_data* ud) {
  if(ud->udBin->kv_const == 2) {
    if(ud->udBin->kv == 2) {
      return 5;
    } else if(ud->udBin->kv == 1) {
      return 9+ud->udBin->keySize;
    } else {
      return -1;
    }
  } else if(ud->udBin->kv_const == 1) {
    return 5;
  }
  return -1;
}

int readBin(User_data* ud) {
  if(ud->buf == NULL) {
    ud->udBin->bufSize=2000;
    ud->buf =
      allocate_mem(ud->udBin->bufSize, NULL); 
    ud->offset = 0;
    int rc = READ(ud->fd, ud->buf, 1);
    ud->offset+=rc;
    ud->readNext = 1;
    ud->udBin->reading = 0;
    set_kv(ud);    
    if(ud->udBin->kv == 0 || ud->udBin->kv == -1) {
      return 0;
    }
    ud->udBin->kv_const = ud->udBin->kv;
  }
  int rc = 0;
  while(1) {  
    if(!ud->udBin->reading) {
      int res = get_bytesToRead(ud);
      if(res != 0) {
        return res;
      }
      ud->udBin->reading = 1;
      if(ud->udBin->kv == 2) {
        ud->udBin->keySize = ud->udBin->bytesToRead;
      }
    }
    if(ud->offset + READSIZE > ud->udBin->bufSize) {
      ud->udBin->bufSize *= 2;
      ud->buf = realloc_mem(ud->buf, ud->udBin->bufSize, NULL);
      assert(ud->buf);
    }
    if(ud->readNext) {
      rc = READ(ud->fd, ud->buf+ud->offset, READSIZE);
      ud->offset += rc;
    } 
    if(ud->offset-getRest(ud) >= ud->udBin->bytesToRead) {
      if(ud->udBin->kv == 1) {
        return 0;
      } else {
        ud->udBin->kv--;
        ud->udBin->reading = 0;
        if(rc < READSIZE) { 
          // En este caso leyó todo, con con lo
          // cual no se necesita volver a leer 
          // del fd para leer el valor y su longitud
          ud->readNext = 0;
        }
      }
    } 
  }
}