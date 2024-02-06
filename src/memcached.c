#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/epoll.h>
#include <sys/resource.h>
#include <assert.h>
#include <signal.h>
#include <pthread.h>
#include <errno.h>
#include <inttypes.h>
#include <sys/ioctl.h>
#include "memcached.h"
#include "cache.h"
#include "common.h"
#include "parser.h"
#include "arr_func.h"
#include "../concurrent_queue/concurrent_queue.h"
#define MAX_THREADS 6
#define MAX_EVENTS 10

Cache memcache;

int textsock;
int binsock;

typedef struct _epollfd{
	int type;
	// 0 : bin
	// 1 : text
	int fd;
} epollfd;

epollfd epfd_copy(epollfd fd){
	return fd;
}

void epfd_dstr(epollfd fd){
	return;
}

/* Macro interna */
#define READ(fd, buf, n) ({						\
	int rc = read(fd, buf, n);				\
	if (rc < 0)	\
		return -2;						\
	if (rc == 0)						\
		return -1;	\
	rc; })

void text_handle(epollfd* evd,
 		char *toks[3], int lens[3],
		int ntok){
	int fd = evd->fd;
	if(!strcmp(toks[0],"PUT")){
		if(ntok !=3){
			char response[20];
			int len = sprintf(response,
				"EINVAL NTOK %d\n",ntok);
			write(fd,response,len);
			return;
		} 
		cache_insert(memcache,
			toks[1], lens[1], 
			toks[2], lens[2], 0);
		write(fd,"OK\n",3);
		return;
	}
	else if(!strcmp(toks[0],"GET")){
		if(ntok != 2){
			write(fd,"EINVAL\n",7);
			return;
		}
		ValData* val = cache_get(memcache,
		 	toks[1], strlen(toks[1]));
		if(val == NULL){
			write(fd,"ENOTFOUND\n",10);
			return;
		}
		if(val->isBin){
			write(fd,"EBINARY\n",8);
			return;
		}
		char res[2024];
		sprintf(res,"OK %s\n",val->value);
		write(fd,res,strlen(res));
		free(val->value);
		free(val);
		return;
	}
	else if(!strcmp(toks[0],"DEL")){
		if(ntok != 2){
			write(fd,"EINVAL\n",7);
			return;
		}
		int res = cache_delete(memcache, 
			toks[1], strlen(toks[1]));
		if(res){
			write(fd,"OK\n",3);		
		}else{
			write(fd,"ENOTFOUND\n",10);
		}
		return;
	}
	else if(!strcmp(toks[0],"STATS")){
		if(ntok != 1){
			write(fd,"EINVAL\n",7);
			return;
		}
		uint64_t* stats = cache_getStats(memcache);
		char response[1000];
		char* s = "OK PUTS=%"PRIu64" DELS=%"PRIu64" GETS=%"PRIu64" KEYS=%"PRIu64"\n\0";
		sprintf(response, s,
			stats[0], stats[1], stats[2],
			stats[3]);	
		write(fd,response,strlen(response));
		return;
	}else{
			write(fd,"EINVAL\n",7);
			return;
	}
}

/* 0: todo ok, continua. -1 errores */
int text_consume(epollfd* evd, char buf[])
{
	int fd = evd->fd; 
	int blen = 1;
	write(fd,"YA TE ATIENDO\n",14);
	int nread = READ(fd,buf,2048);
	blen += nread;
	char *p, *p0 = buf;
	int nlen = blen;
	/* Para cada \n, procesar, y avanzar punteros */
	while ((p = memchr(p0, '\n', nlen)) != NULL) {
	/* Mensaje completo */
		int len = p - p0;
		*p++ = 0;
        //log(3, "full command: <%s>", p0);
		char *toks[3]= {NULL};
		int lens[3] = {0};
		int ntok;
		ntok = text_parser(buf,toks,lens);

		text_handle(evd,toks,lens,ntok);
		nlen -= len + 1;
		p0 = p;
	}

	/* Si consumimos algo, mover */
	if (p0 != buf) {
		memmove(buf, p0, nlen);
		blen = nlen;
	}else if(blen == 2048){
		write(fd,"EBIG\n",5);
		return -1;
	}
	while (1) {
		int rem = sizeof *buf - blen;
		if(rem < 0) return -1;
		/* Buffer lleno, no hay comandos, matar */
		if (rem == 0)
			return 0;
		printf("REM: %d\n",rem);
		int nread = READ(fd, buf + blen, rem);
		printf("a\n");
		//log(3, "Read %i bytes from fd %i", nread, fd);
		blen += nread;
		char *p, *p0 = buf;
		int nlen = blen;

		/* Para cada \n, procesar, y avanzar punteros */
		while ((p = memchr(p0, '\n', nlen)) != NULL) {
			/* Mensaje completo */
			int len = p - p0;
			*p++ = 0;
            //log(3, "full command: <%s>", p0);
			char *toks[3]= {NULL};
			int lens[3] = {0};
			int ntok;
			ntok = text_parser(buf,toks,lens);

			text_handle(evd,toks,lens,ntok);
			nlen -= len + 1;
			p0 = p;
		}

		/* Si consumimos algo, mover */
		if (p0 != buf) {
			memmove(buf, p0, nlen);
			blen = nlen;
		}else if(blen == 2048){
			write(fd,"EBIG\n",5);
			return -1;
		}
	}
	return 0;
}

int bin_consume(epollfd* evd){
	int fd = evd->fd;
	char comm;
	int rc = READ(fd,&comm,1);
  char buf[4];
  char* key;
  int lenk;
	switch (comm)
	{
	case PUT:
		rc = READ(fd,buf,4);
		if(rc < 4){
			char response = EINVALID;
			write(fd,&response,1);
			return 0;
		}
	  lenk = ntohl(*(int*)buf);
		//TODO: usar allocate_mem???
		key = malloc(lenk+1);
		rc = READ(fd,key,lenk);
		if(rc != lenk){
			char response = EINVALID;
			write(fd,&response,1);
			printf("keylen error lenk: %d actual:%d\n",
				lenk, rc);
			return 0;
		}
		rc = READ(fd,buf,4);
		if(rc < 4){
			char response = EINVALID;
			write(fd,&response,1);
			return 0;
		}
		int lenv = ntohl(*(int*)buf);
		char* value = malloc(lenv+1);
		rc = READ(fd,value,lenv);
		if(rc != lenv){
			char response = EINVALID;
			write(fd,&response,1);
			printf("vallen error lenv: %d actual:%d\n",
				lenv, rc);
			return 0;
		}
		if(cache_insert(memcache, key,
				lenk, value, lenv, 1)){
			char response = OK;
			write(fd,&response,1);
		}
		else{
			char response = EINVALID;
			write(fd,&response,1);
			printf("insert error\n");
			return 0;	
		}
		break;
	case DEL:
		rc = READ(fd,buf,4);
		if(rc < 4){
			char response = EINVALID;
			write(fd,&response,1);
			return 0;
		}
		lenk = ntohl(*(int*)buf);
		key = malloc(lenk+1);
		rc = READ(fd,key,lenk);
		if(rc != lenk){
			char response = EINVALID;
			write(fd,&response,1);
			printf("keylen error lenk: %d actual:%d\n",
				lenk,rc);
			return 0;
		}
		if(cache_delete(memcache,key,lenk)){
			char response = OK;
			write(fd,&response,1);
		}
		else{
			char response = ENOTFOUND;
			write(fd,&response,1);
		}
		break;
	case GET:    
		rc = READ(fd,buf,4);
		if(rc < 4){
			char response = EINVALID;
			write(fd,&response,1);
			return 0;
		}
		lenk = ntohl(*(int*)buf);
		key = malloc(lenk+1);
		rc = READ(fd,key,lenk);
		if(rc != lenk){
			char response = EINVALID;
			write(fd,&response,1);
			printf("keylen error lenk: %d actual:%d\n",
				lenk,rc);
			return 0;
		}
    ValData* resp = cache_get(memcache, 
			key, lenk);
    if(NULL == resp){
      char response = ENOTFOUND;
      write(fd,&response,1);
      return 0;
    }
		uint32_t bigLen = resp->valSize;		
    long len = bigLen + 5;
    char* response = malloc(len);
    response[0] = OK;
		for(int i = 4; i > 0; i--) {
			response[i] = bigLen & 0xFF;
			bigLen = bigLen >> 8;
		}
    arrcpy(response+5,
			resp->value, resp->valSize);
    write(fd,response,len);
		free(resp->value);
		free(resp);
		break;
	case STATS:
		uint64_t* stats = cache_getStats(memcache);
		char* stats_msj = malloc(33);
		stats_msj[0] = OK;
		for(int i = 0; i < 4; i++){
			uint64_t bignum = stats[i];
			for(int j = 8; j>0;j--){
				stats_msj[(i*8)+j] = bignum & 0xFF;
				bignum = bignum >>8;
			}
		}
		break;
	default:
		char c = EINVALID;
		write(fd,&c,1);
		return 0;
		break;
	}
  return 0;
	
}

void* wait_for_req(void* argv){
	ConcurrentQueue* conqueue = 
		(ConcurrentQueue*) argv;
	while(1){
	epollfd* epfd = 
		concurrent_queue_dequeue(
			conqueue,
			(Destroy)epfd_dstr,
			(Copy) epfd_copy);
	printf("hola que tal que nececita\n");
	if(epfd->type){
		printf("texto detectado\n");
		char buf[2048];
		int n = text_consume(epfd,buf);
		printf("text:%d\n",n);
		if(n < 0) {			
			close(epfd->fd);
			epoll_ctl(epfd->fd, EPOLL_CTL_DEL,
				binsock, NULL);
		}
	}else{
		printf("binario detectado\n");
		int n = bin_consume(epfd);

		printf("bin:%d\n",n);
		if(n < 0) {
			close(epfd->fd);
			epoll_ctl(epfd->fd, EPOLL_CTL_DEL,
				binsock, NULL);
		}
	}
	free(epfd);
	}
}


void* text_epoll(void* argv){
	ConcurrentQueue* conqueue
	 	= (ConcurrentQueue*) argv;
	int epoll_fd = epoll_create1(0);
  printf("text:hola\n");
	int tcsock;
  struct epoll_event textevent;
  textevent.events = EPOLLIN;
  textevent.data.fd = textsock;
  epoll_ctl(epoll_fd, EPOLL_CTL_ADD,
		textsock, &textevent);
	struct epoll_event textevents[MAX_EVENTS];
	printf("epoll texto configurado\n");
	while (1){
		int text_num_events = epoll_wait(
			epoll_fd, textevents, MAX_EVENTS, -1);
		if (text_num_events == -1) {
			perror("textepoll_wait");
			exit(EXIT_FAILURE);
		}
		for(int i = 0; i<text_num_events; i++){
			if(textevents[i].data.fd == textsock){
				tcsock = accept(textsock, NULL, NULL);
				if (tcsock < 0) quit("accept");
				printf("cliente aceptado\n");
				textevent.events = EPOLLIN | EPOLLET;
				textevent.data.fd = tcsock;
				epoll_ctl(epoll_fd, EPOLL_CTL_ADD,
					tcsock, &textevent);
			}
			else{
				epollfd* fd = malloc(sizeof(epollfd));
				fd->type = 1;
				fd->fd = textevents[i].data.fd;
				concurrent_queue_enqueue(conqueue,
					fd, (Copy) epfd_copy);
			}
		}
  }
}


void* bin_epoll(void* argv){
	ConcurrentQueue* conqueue =
		 (ConcurrentQueue*) argv;
  printf("bin:hola\n");
	int epoll_fd = epoll_create1(0);
	int bcsock;
  struct epoll_event binevent;
  binevent.events = EPOLLIN;
  binevent.data.fd = binsock;
  epoll_ctl(epoll_fd, EPOLL_CTL_ADD,
		binsock,&binevent);
	struct epoll_event binevents[MAX_EVENTS];
	printf("epoll bin configurado\n");
	while (1){
		int bin_num_events = 
			epoll_wait(epoll_fd,
				binevents, MAX_EVENTS, -1);
		if (bin_num_events == -1) {
			perror("binepoll_wait");
			exit(EXIT_FAILURE);
		}
		for(int i = 0; i<bin_num_events; i++){
			if(binevents[i].data.fd == binsock) {
				bcsock = accept(binsock, NULL, NULL);
				if (bcsock < 0) quit("accept");
				printf("cliente aceptado\n");
				binevent.events = EPOLLIN | EPOLLET;
				binevent.data.fd = bcsock;
				epoll_ctl(epoll_fd,EPOLL_CTL_ADD,
					bcsock,&binevent);
			} else {
				epollfd* fd = malloc(sizeof(epollfd));
				fd->type = 0;
				fd->fd = binevents[i].data.fd;
				concurrent_queue_enqueue(conqueue,
					fd, (Copy) epfd_copy);
			}
		}
  }
}

void server_start(){
	//text fd
	printf("iniciando servidor\n");


	/**
	 * ACA SE VAN A GUARDAR LOS PEDIDOS, LOS
	 * THREADS TOMARAN DE A UNO Y ATENDERAN
	 * SIGUIENDO UNA LOGICA DE PRODUCTOR 
	 * CONSUMIDOR CON ESTRUCTURA FIFO
	*/
	ConcurrentQueue* conqueue = 
		malloc(sizeof(ConcurrentQueue));

	concurrent_queue_init(conqueue,30);
	printf("conqueue inicializada\n");
	pthread_t threads[MAX_THREADS];
	for (size_t i = 0; i < MAX_THREADS -2; i++)
	{
		pthread_create(&threads[i], NULL,
			wait_for_req, (void*) (conqueue));
	}
	pthread_t eptext;
	pthread_t epbin;
	pthread_create(&eptext, NULL,
		text_epoll, (void*) (conqueue));
	pthread_create(&epbin, NULL, 
		bin_epoll, (void*) (conqueue));
	printf("hilos creados \n");
	pthread_join(epbin,NULL);
}

unsigned str_KRHash(const char *s, uint32_t len) {
  unsigned hashval;
  uint32_t i = 0;
  for (hashval = 0;i<len; ++s) {
    hashval = *s + 31 * hashval;
	i++;
  }
  return hashval;
}

int memcached_cache_start(int tsock,int bsock){
	textsock=tsock;
	binsock=bsock;
	printf("iniciando\n");
	memcache = cache_create(1000000,str_KRHash);
	printf("cache creada\n");
	server_start();
	return 0;
};
