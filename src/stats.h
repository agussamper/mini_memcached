//! @file

#ifndef __STATS_H__
#define __STATS_H__

#include <stdint.h>
#include <pthread.h>

typedef struct _Stats *Stats;

/**
 * Inicializa una estructura de tipo
 * _Stats y devuelve un puntero a la
 * misma
*/
Stats stats_init();

/**
 * Destruye la estructura stats
*/
void stats_destroy(Stats stats);

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

/**
 * Devuelve un string con las
 * estadísticas.
 * @param stats estructura con las estadisticas.
 * @param listMutex se debe pasar el 
 * mutex de la cache que esté tomado
 * (un elemento de mutex_arr) al llamar
 * la función.
 * @return
 * El string que devuelve la función tiene la
 * siguiente forma:
 * "OK PUTS=%"PRIu64" DELS=%"PRIu64" GETS=%"PRIu64"
*/
uint64_t* stats_getStats(Stats stats, pthread_mutex_t* listMutex);

#endif