#include <stdlib.h>
#include <stdint.h>
#include "arr_func.h"

uint32_t arrcmp(char* arr1,
  char* arr2, uint32_t len){
  for(uint32_t i = 0; i<len;i++){
    if(arr1[i] != arr2[i])return 0;
  }
  return 1;
}

void arrcpy(char* dest, char* src, uint32_t len){
  for(uint32_t i = 0; i<len;i++){
    dest[i] = src[i];
  }
}