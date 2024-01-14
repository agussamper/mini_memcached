#include "../src/cache.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <assert.h>
#include <unistd.h>

#include <sys/time.h>
#include <sys/resource.h>
#include <stdlib.h>

//#define MEM_ENV_VAR "MAXMEM_MB"

Cache cache;

void* run(void* arg) {
  int id = arg - (void*)0;
  printf("id=%d\n",id);

  char clave[7] = "clave";
  char valor[7] = "valor";
  clave[6] = '\0'; 
  valor[6] = '\0'; 
  for(int i = 0; i < 6; i++) {
    clave[5]= i + '0';
    valor[5]= i + '0';
    cache_insert(cache, clave, 6, valor, 6);
  }
  for(int i = 0; i < 6; i++) {
    clave[5]= i + '0';
    char* valorObt = cache_get(cache, clave);
    if(valorObt != NULL) {
      printf("clave=%s, valor=%s\n", clave, valorObt);
      free(valorObt);
    } else {
      printf("clave=%s no encontrada linea 37, id=%d\n", clave, id);
    }
  }
  char s[100];
  switch (id) {
  case 0:
    strcpy(s,"valor dado por id=0");
    cache_insert(cache, "clave0", 6, "valor dado por id=0", strlen(s));
    break;
  case 1:
    strcpy(s,"valor dado por id=1");
    cache_insert(cache, "clave1", 6, "valor dado por id=1", strlen(s));
    break;
  case 2:
    strcpy(s,"valor dado por id=2");
    cache_insert(cache, "clave2", 6, "valor dado por id=2", strlen(s));
    break;
  default:
    strcpy(s,"valor dado por id=3");
    cache_insert(cache, "clave3", 6, "valor dado por id=3", strlen(s));
    break;
  }
  for(int i = 0; i < 4; i++) {
    clave[5]= i + '0';
    char* valorObt = cache_get(cache, clave);
    if(valorObt != NULL) {
      printf("clave=%s, valor=%s\n", clave, valorObt);
      free(valorObt);
    } else {
      printf("clave=%s no encontrada linea 66, id=%d\n", clave, id);
    }
  }
  int res = cache_delete(cache, "clave0");
  printf("id=%d: pase el delete\n", id);
  if(0 == res) {
    printf("id=%d: clave0 no encontrada\n", id);
  } else {
    printf("id=%d: encontrada\n", id);
  }
  res = cache_delete(cache, "clave0");
  printf("id=%d: pase el delete\n", id);
  if(0 == res) {
    printf("id=%d: clave0 no encontrada\n", id);
  } else {
    printf("id=%d: encontrada\n", id);
  }
  return NULL;
}

void* mem(void* arg) {
  int id = arg - (void*)0;
  printf("id=%d\n",id);

  char clave[100] = "clave";
  char valor[200] = "este valor 1000 seta usado para insertar en cache, es mas largo con el objetivo que se llene la memoria";
  int lenval = strlen(valor); 
  valor[lenval] = '\0'; 
  for(int i = 0; i < 10000000; i++) {
    sprintf(clave+5, "%d", i);
    int lenIdx = strlen(clave+5);
    if(lenIdx < 7) {
      int rep = 7 - lenIdx;
      for(int j = 0; j < rep; j++) {
        sprintf(clave+5+j, "%d", 0);
      }
      sprintf(clave+5+rep, "%d", i);
    }    
    cache_insert(cache, clave, strlen(clave), valor, lenval);
    if(i % 100000 == 0) {
      printf("insertando... i=%d, id=%d\n", i, id);
    }
  }
  /*for(int i = 0; i < 10000000; i++) {
    sprintf(clave+5, "%d", i);
    cache_insert(cache, clave, strlen(clave), valor, lenval);
    if(i % 100000 == 0) {
      printf("insertando devuelta... i=%d, id=%d\n", i, id);
    }
  }*/
}

unsigned str_KRHash(const char *s) {
  unsigned hashval;
  for (hashval = 0; *s != '\0'; ++s) {
    hashval = *s + 31 * hashval;
  }
  return hashval;
}

void setmemlimit()
{
  struct rlimit memlimit;
  long bytes;

  //if(getenv(MEM_ENV_VAR)!=NULL)
  //{
    //bytes = atol(getenv(MEM_ENV_VAR))*(1024*1024);
    bytes = 500*(1024*1024);
    memlimit.rlim_cur = bytes;
    memlimit.rlim_max = bytes;
    setrlimit(RLIMIT_AS, &memlimit);
  //}
}

int main() {
  setmemlimit();
  cache = cache_create(1000000, str_KRHash);
  int numThreads = 10;
  pthread_t threads[numThreads];
  for (int i = 0; i < numThreads; i++) {
	  pthread_create(threads+i, NULL, run, i + (void*)0);    
  }
  for (int i = 0; i < numThreads; i++) {
    pthread_join(threads[i], NULL);
  }  
  //mem(NULL);
  char* stats = cache_getStats(cache);
  printf("%s\n", stats);
  free(stats);
  return 0;
}