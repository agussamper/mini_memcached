#include <stdlib.h>
#include "arr_func.h"

int arrcmp(char* arr1, char* arr2, int len){
    for(int i = 0; i<len;i++){
        if(arr1[i] != arr2[i])return 0;
    }
    return 1;
}

void arrcpy(char* dest, char* src, int len){
    for(int i = 0; i<len;i++){
        dest[i] = src[i];
    }
}