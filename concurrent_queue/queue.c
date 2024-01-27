#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include "queue.h"

struct Queue* createQueue(unsigned capacity){
    struct Queue* q = malloc(sizeof(struct Queue));
    q->capacity = capacity;
    q->first = 0;
    q->last = -1;
    q->elems = 0;
    q->array = malloc(sizeof(void*) * capacity);
    return q;
}

int isFull(struct Queue* queue){
    return  (queue->elems == queue->capacity);
}

int isEmpty(struct Queue* queue){
    return !(queue->elems);
}

int enqueue(struct Queue* queue, void* data, Copy cpy){
    if(isFull(queue)){
        return 0;
    }
    queue->array[(queue->last+1) % queue->capacity] = cpy(data);
    queue->last = (queue->last+1) % queue->capacity;
    queue->elems++;
    return 1;
}


void* dequeue(struct Queue* queue, Destroy dstry, Copy cpy){
    if(isEmpty(queue)){
        return 0;
    }
    void* ret = cpy(queue->array[queue->first]);
    dstry(queue->array[queue->first]);
    queue->first = (queue->first+1)%queue->capacity;
    queue->elems--;
    return ret;
}

void* first(struct Queue* queue){
    return queue->array[queue->first];
}

void* last(struct Queue* queue){
    return queue->array[queue->last];
}

size_t elems(struct Queue* queue){
    return queue->elems;
}

size_t capacity(struct Queue* queue){
    return queue->capacity;
}

void queueFree(struct Queue* queue, Destroy dstry){
    if(queue->first <= queue->last){
        for(int i = queue->first; i<=queue->last; i++){
            dstry(queue->array[i]);
        }
    }else{
        for(int i = queue->first; i<queue->capacity; i++){
            dstry(queue->array[i]);
        }
        for(int i = 0; i<=queue->last;i++){
            dstry(queue->array[i]);
        }
    }
    free(queue->array);
    free(queue);
}