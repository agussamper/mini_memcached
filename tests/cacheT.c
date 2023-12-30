#include "../src/cache.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <assert.h>
#include <unistd.h>

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
    printf("clave=%s, valor=%s\n", clave, valorObt);
    free(valorObt);
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
    printf("clave=%s, valor=%s\n", clave, valorObt);
    free(valorObt);
  }
  int res = cache_delete(cache, "clave0"); //TODO: solucionar deadlock
  printf("id=%d: pase el delete\n", res);
  if(0 == res) {
    printf("id=%d: clave0 no encontrada\n", res);
  } else {
    printf("id=%d: encontrada\n", res);
  }
  assert(!cache_get(cache, "clave0"));

  return NULL;
}

unsigned str_KRHash(const char *s) {
  unsigned hashval;
  for (hashval = 0; *s != '\0'; ++s) {
    hashval = *s + 31 * hashval;
  }
  return hashval;
}

int main() {
  cache = cache_create(1000, str_KRHash);
  int numThreads = 4;
  pthread_t threads[numThreads];
  for (int i = 0; i < numThreads; i++) {
	  pthread_create(threads+i, NULL, run, i + (void*)0);    
  }
  for (int i = 0; i < numThreads; i++) {
    pthread_join(threads[i], NULL);
  }
  return 0;
}