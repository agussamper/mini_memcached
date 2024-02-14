//! @file

#ifndef __ARR_FUNC_H__
#define __ARR_FUNC_H__

/**
 * compara dos arreglos byte a byte.
 * @param arr1 string a comparar con arr1.
 * @param len1 longitud de arr1.
 * @param arr2 string a comparar con arr2.
 * @param len2 longitud de arr2.
 * @return
 * Si son iguales retorna 0, en caso
 * contrario 1.
 * */
uint32_t arrcmp(char* arr1,
  uint32_t len1, char* arr2,
  uint32_t len2);

/**
 * Copia len bytes desde src a dest
 * @param dest Donde se van a copiar los bytes
 * @param src bytes que se van a copiar
 * @param len cantidad de bytes a copiar
*/
void arrcpy(char* dest, char* src, uint32_t len); 

#endif // __ARR_FUNC_H__
