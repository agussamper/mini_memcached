#ifndef CONCURRENT_QUEUE_H
#define CONCURRENT_QUEUE_H

#include <stdlib.h>
#include "queue.h"

typedef struct ConcurrentQueue
{

    struct Queue* queue;

    /************************************************
    The mutual exclusion lock for updating the queue.
    ************************************************/
    pthread_mutex_t mutex;

    /*****************************
    Guards against an empty queue.
    *****************************/
    pthread_cond_t  empty_condition_variable;

    /***************************
    Guards against a full queue.
    ***************************/
    pthread_cond_t  full_condition_variable;
}
ConcurrentQueue;

/*****************************************
Initializes a new, empty concurrent queue.
*****************************************/
void concurrent_queue_init(ConcurrentQueue* conqueue, size_t capacity);

/****************************************
Enqueue a datum onto the rear of the queue.
****************************************/
void concurrent_queue_enqueue(ConcurrentQueue* conqueue, void* data, Copy cpy);

/******************************************
Returns, but does not remove the top datum.
******************************************/
void* concurrent_queue_first(ConcurrentQueue* conqueue);

/****************************************
Removes the topmost datum from the queue.
****************************************/
void* concurrent_queue_dequeue(ConcurrentQueue* conqueue,Destroy dstry, Copy cpy);

/*******************************************
Returns the number of elements in the queue.
*******************************************/
size_t concurrent_queue_elems(ConcurrentQueue* conqueue);

/*********************************
Returns the capacity of the queue.
*********************************/
size_t concurrent_queue_capacity(ConcurrentQueue* conqueue);

/***********************************
Releases all resources of the queue.
***********************************/
void concurrent_queue_free(ConcurrentQueue* conqueue, Destroy dtry);

#endif /* CONCURRENT_queue_H */