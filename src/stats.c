#include "stats.h"
#include "malloc_interface.h"

#include <stdlib.h>
#include <pthread.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <inttypes.h>

struct _Stats {
  uint64_t puts;
  uint64_t gets;
  uint64_t dels;
  uint64_t keys;
  pthread_mutex_t mutex;
};

Stats stats_init() {
  Stats stats = malloc(sizeof(struct _Stats));
  stats->puts = 0;
  stats->gets = 0;
  stats->dels = 0;
  stats->keys = 0;
  assert(!pthread_mutex_init(&(stats->mutex), NULL));
  return stats;
}

void stats_putsInc(Stats stats) {
  pthread_mutex_lock(&stats->mutex);
  stats->puts += 1;
  pthread_mutex_unlock(&stats->mutex);
}

void stats_getsInc(Stats stats) {
  pthread_mutex_lock(&stats->mutex);
  stats->gets += 1;
  pthread_mutex_unlock(&stats->mutex);
}

void stats_delsInc(Stats stats) {
  pthread_mutex_lock(&stats->mutex);
  stats->dels += 1;
  pthread_mutex_unlock(&stats->mutex);
}

void stats_keysInc(Stats stats) {
  pthread_mutex_lock(&stats->mutex);
  stats->keys += 1;
  pthread_mutex_unlock(&stats->mutex);
}

void stats_keysDec(Stats stats) {
  pthread_mutex_lock(&stats->mutex);
  stats->keys -= 1;
  pthread_mutex_unlock(&stats->mutex);
}

char* stats_getStats(Stats stats, pthread_mutex_t* listMutex) {
  pthread_mutex_lock(&stats->mutex);
  char* statsinf = allocate_mem(sizeof(char)*200, listMutex);
  char* s = "OK PUTS=%"PRIu64" DELS=%"PRIu64" GETS=%"PRIu64" KEYS=%"PRIu64"\0";
  sprintf(statsinf, s,
    stats->puts, stats->dels, stats->gets,
    stats->keys);
  pthread_mutex_unlock(&stats->mutex);
  return statsinf;
}