#include <stdlib.h>
#include <stdint.h>
#include "arr_func.h"

uint32_t arrcmp(char* arr1,
  uint32_t len1,
  char* arr2, uint32_t len2){
  if (len1 != len2) return 1;
  for(uint32_t i = 0; i < len1; i++){
    if(arr1[i] != arr2[i]) return 1;
  }
  return 0;
}

void arrcpy(char* dest, char* src, uint32_t len){
  for(uint32_t i = 0; i < len; i++){
    dest[i] = src[i];
  }
}