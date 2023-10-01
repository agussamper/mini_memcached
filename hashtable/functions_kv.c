#include "functions_kv.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>



struct _FUNC{
    Destroy_key dstr_k;
    Destroy_value dstr_v;
    Cpy_key cpy_k;
    Cpy_value cpy_v;
    Compare_key cmp_k;
    Compare_value cmp_v;
};


FUNC crear_func(Destroy_key d_k, Destroy_value d_v, Cpy_key c_k, 
                Cpy_value c_v, Compare_key cm_k, Compare_value cm_v){
                    FUNC F = malloc(sizeof(struct _FUNC));
                    F->dstr_k = d_k;
                    F->dstr_v = d_v;
                    F->cpy_k = c_k;
                    F->cpy_v = c_v;
                    F->cmp_k = cm_k;
                    F->cmp_v = cm_v;
                    return F;
                }

void dstr_key(FUNC F,void* key){
    F->dstr_k(key);
    return;
}
void dstr_value(FUNC F,void* value){
    F->dstr_v(value);
    return;
}
void* copy_k(FUNC F,void* key){
    void* k = F->cpy_k(key);
    return k;
}
void* copy_v(FUNC F,void* value){
    void* v = F->cpy_v(value);
    return v;
}
int comp_k(FUNC F,void* key1,void* key2){
    int c = F->cmp_k(key1,key2);
    return c;
}
int comp_v(FUNC F,void* val1,void* val2){
    int c = F->cmp_v(val1,val2);
    return c;
}

