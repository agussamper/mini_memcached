#ifndef QUEUE_H
#define QUEUE_H

#include <stdlib.h>

typedef void (*Destroy)(void *dato);
typedef void *(*Copy)(void *dato);
typedef void (*Visit)(void *dato);

// A structure to represent a queue
struct Queue {
	int first;
    int last;
	int elems;
	unsigned capacity;
	void** array;
};

struct Queue* createQueue(unsigned capacity);
 
int isFull(struct Queue* queue);

int isEmpty(struct Queue* queue);

//if full return 0 
int enqueue(struct Queue* queue, void* data, Copy cpy);

//if empty return 0 
void* dequeue(struct Queue* queue, Destroy dstry, Copy cpy);
 
void* first(struct Queue* queue);

void* last(struct Queue* queue);

size_t elems(struct Queue* queue);

size_t capacity(struct Queue* queue);

void queueFree(struct Queue* queue, Destroy dstry);

#endif /*QUEUE_H*/