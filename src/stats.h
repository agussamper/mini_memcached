#ifndef __STATS_H__
#define __STATS_H__

#include <stdint.h>

typedef struct _Stats *Stats;

/**
 * Inicializa una estructura de tipo
 * _Stats y devuelve un puntero a la
 * misma
*/
Stats stats_init();

/**
 * Incrementa en uno el campo puts de
 * stats
*/
void stats_putsInc(Stats stats);

/**
 * Incrementa en uno el campo gets de
 * stats
*/
void stats_getsInc(Stats stats);

/**
 * Incrementa en uno el campo dels de
 * stats
*/
void stats_delsInc(Stats stats);

/**
 * Incrementa en uno el campo keys de
 * stats
*/
void stats_keysInc(Stats stats);

/**
 * Decrementa en uno el campo keys de
 * stats
*/
void stats_keysDec(Stats stats);

uint64_t stats_getKeys(Stats stats);

/**
 * Devuelve un string con las
 * estadísticas
*/
char* stats_show(Stats stats);

#endif