//! @file

#ifndef __PARSER_H
#define __PARSER_H 1

#include "common.h"

/**
 * @brief Parser de texto.
 * Dado un buffer guarda en toks[i] lo que hay en
 * buffer hasta que encuentra un espacio, sigue con
 * i+1 leyendo de buf desde donde quedó anteriormente.
 * Notar que 0 <= i <= 2.
 * @param[in] buf - const char*.  No debe contener el "\n".
 * @param[out] toks - char*: arreglo de tokens.
 * @param[out] lens - arreglo de enteros, 
 * contiene la longitud de los tokens.
 * @param[out] ntok - cantidad de tokens.
*/
int text_parser(const char *buf, char *toks[3], int lens[3]);


#endif
