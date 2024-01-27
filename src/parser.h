#ifndef __PARSER_H
#define __PARSER_H 1

#include "common.h"




//! @brief Parser de texto.
//!
//! @param[in] buf - const char *.  No debe contener el '\n'
//! @param[out] toks - char *: arreglo de tokens
//! @param[out] lens - arreglo de enteros, contiene la longitud de los tokens
//! @param[out] ntok - cantidad de tokens

int text_parser(const char *buf, char *toks[3], int *lens );


#endif
