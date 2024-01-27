#include <pthread.h>
#include "concurrent_queue.h"
#include <stdio.h>

#define MAX(A,B) (((A) > (B)) ? (A) : (B))


void concurrent_queue_init(ConcurrentQueue* conqueue, size_t capacity){
    conqueue->queue = createQueue(capacity);
    printf("a");
    pthread_mutex_init(&conqueue->mutex,NULL);
    printf("b");
    pthread_cond_init(&conqueue->empty_condition_variable,NULL);
    printf("c\n");
    pthread_cond_init(&conqueue->full_condition_variable,NULL);
}

void concurrent_queue_enqueue(ConcurrentQueue* conqueue, void* data, Copy cpy){
    pthread_mutex_lock(&conqueue->mutex);
    while(isFull(conqueue->queue)){
        pthread_cond_wait(&conqueue->full_condition_variable,&conqueue->mutex);
    }
    enqueue(conqueue->queue,data,cpy);
    pthread_cond_signal(&conqueue->empty_condition_variable);
    printf("lo mandamos para ser atendido\n");
    pthread_mutex_unlock(&conqueue->mutex);
}


void* concurrent_queue_first(ConcurrentQueue* conqueue){
    void* ret;
    pthread_mutex_lock(&conqueue->mutex);
    while(isEmpty(conqueue->queue)){
        pthread_cond_wait(&conqueue->empty_condition_variable,&conqueue->mutex);   
    }
    ret = first(conqueue->queue);
    pthread_mutex_unlock(&conqueue->mutex);
    return ret;
}


void* concurrent_queue_dequeue(ConcurrentQueue* conqueue,Destroy dstry, Copy cpy){
    void* ret;
    pthread_mutex_lock(&conqueue->mutex);
    while(isEmpty(conqueue->queue)){
        pthread_cond_wait(&conqueue->empty_condition_variable,&conqueue->mutex);   
    }
    ret = dequeue(conqueue->queue,dstry,cpy);
    pthread_mutex_unlock(&conqueue->mutex);
    return ret;
}



size_t concurrent_queue_elems(ConcurrentQueue* conqueue){
    size_t e;
    pthread_mutex_lock(&conqueue->mutex);
    e = elems(conqueue->queue);
    pthread_mutex_unlock(&conqueue->mutex);
    return e;
}


size_t concurrent_queue_capacity(ConcurrentQueue* conqueue){
    return capacity(conqueue->queue);
}

void concurrent_queue_free(ConcurrentQueue* conqueue, Destroy dtry){
    queueFree(conqueue->queue,dtry);
    pthread_cond_destroy(&conqueue->empty_condition_variable);
    pthread_cond_destroy(&conqueue->full_condition_variable);
    pthread_mutex_destroy(&conqueue->mutex);
    free(conqueue);
}
