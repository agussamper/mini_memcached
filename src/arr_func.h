//! @file

#ifndef __ARR_FUNC_H__
#define __ARR_FUNC_H__

/**
 * compara dos arreglos byte a byte.
 * Si son iguales retorna 0, en caso
 * contrario 1
 * */
uint32_t arrcmp(char* arr1,
  uint32_t len1, char* arr2,
  uint32_t len2);

// copia el array src al array dest byte a byte
void arrcpy(char* dest, char* src, uint32_t len); 

#endif // __ARR_FUNC_H__
