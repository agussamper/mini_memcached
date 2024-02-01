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
#include "memcached.h"
#include "cache.h"
#include "common.h"
#include "parser.h"
#include "../concurrent_queue/concurrent_queue.h"
#define MAX_THREADS 6
#define MAX_EVENTS 10

Cache memcache;

int textsock;
int binsock;

/*
PUT A B
GET A


*/

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
	int rc = read(fd, buf, n);					\
	if (rc < 0 && (errno == EAGAIN || errno == EWOULDBLOCK))	\
		return 0;						\
	if (rc <= 0)							\
		return -1;						\
	rc; })


void text_handle(epollfd* evd, char *toks[3], int lens[3], int ntok){
	int fd = evd->fd;
	if(!strcmp(toks[0],"PUT")){
		if(ntok !=3){
			char * response = malloc(sizeof(char) * 20);
			int len = sprintf(response,"EINVAL NTOK %d\n",ntok);
			write(fd,response,len);
			free(response);
			return;
		} 
		cache_insert(memcache,toks[1],lens[1],toks[2],lens[2]);
		write(fd,"OK\n",3);
		return;
	}
	else if(!strcmp(toks[0],"GET")){
		if(ntok != 2){
			write(fd,"EINVAL\n",7);
			return;
		}
		char* val = malloc(sizeof(char) * 2024); 
		val = cache_get(memcache,toks[1]);
		if(val == NULL){
			free(val);
			write(fd,"ENOTFOUND\n",10);
			return;
		}
		char res[2024];
		sprintf(res,"OK %s\n",val);
		write(fd,res,strlen(res));
		free(val);
		return;
	}
	else if(!strcmp(toks[0],"DEL")){
		if(ntok != 2){
			write(fd,"EINVAL\n",7);
			return;
		}
		int res = cache_delete(memcache,toks[1]);
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
		char* response = cache_getStats(memcache);	
		write(fd,response,strlen(response));		
		return;
	}else{
			write(fd,"EINVAL\n",7);
			return;
	}
}

/**
 
PUT A B
GET A
STATS




*/
/* 0: todo ok, continua. -1 errores */
int text_consume(epollfd* evd, char buf[2048])
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
	/*
	char* buf = malloc(sizeof(char) * 6);	
	int rc = read(fd, buf, 5);
	char com = buf[0];
	switch (com)
	{
	case 11:
		int lenk = *(int*)buf+1;
		char* key = malloc(sizeof(char) * lenk+1);
		int rc = read(fd, key,lenk);
		if(rc!=lenk){
			char response = 111;
			write(fd,&response,1);
      		printf("keylen error lenk: %d actual:%d\n",lenk,rc);
			return -1;
		}
		int lenv = ntohl(*(int*)buf+5+lenk);
		char* val = malloc(sizeof(char) * lenv+1);
		rc = read(fd, val,lenv);
		if(rc!=lenv){
			char response = 111;
			write(fd,&response,1);
      		printf("vallen error lenv: %d actual:%d\n",lenv,rc);
			return -1;
		}
		if(cache_insert(memcache,key,lenk,val,lenv)){
			char response = 101;
			write(fd,&response,1);
		}
		else{
			char response = 111;
			write(fd,&response,1);
            printf("insert error\n");
			return -1;	
		}
		break;
	case 12:
	
		int len = ntohl(*(int*)buf+1);
		free(buf);
		char* key = malloc(sizeof(char) * len+1);
		read(fd, key,len);
	

		break;
	case 13:
		int len = ntohl(*(int*)buf+1);
		free(buf);
		char* getkey = malloc(sizeof(char) * len+1);
		int rcget =read(fd, getkey,len);
		if(rcget!=lenv){
			char response = 111;
			write(fd,&response,1);
			return -1;
		}
		char* resp = cache_get(memcache,getkey);
		if(resp = NULL){
			char response = 112;
			write(fd,&response,1);
			return -1;
		}
		len = strlen(resp) + 6;
		char* response = malloc(sizeof(char) * len);
		response[0] = 101;
		response[1] =  htonl(strlen(resp));
		strcpy(response+5,resp);
		write(fd,response,len);
		break;
	case 21:
		free(buf);
		break;
	default:
			write(fd,"EINVAL\n",7);
			return -1;
		break;
	}	
	*/



		char comm;
		int rc = READ(fd,&comm,1);
    char* buf;
    char* key;
    int lenk;
		switch (comm)
		{
		case 11:
			buf = malloc(5);
			rc = READ(fd,buf,4);
			if(rc < 4){
				char response = 111;
				write(fd,&response,1);
				return -1;
			}
		  lenk = ntohl(*(int*)buf);
			key = malloc(lenk+1);
			rc = READ(fd,key,lenk);
			if(rc != lenk){
				char response = 111;
				write(fd,&response,1);
				printf("keylen error lenk: %d actual:%d\n",lenk,rc);
				return -1;
			}
			rc = READ(fd,buf,4);
			if(rc < 4){
				char response = 111;
				write(fd,&response,1);
				return -1;
			}
			int lenv = ntohl(*(int*)buf);
			char* value = malloc(lenv+1);
			rc = READ(fd,value,lenv);
			if(rc != lenv){
				char response = 111;
				write(fd,&response,1);
				printf("vallen error lenv: %d actual:%d\n",lenv,rc);
				return -1;
			}
			
			if(cache_insert(memcache,key,lenk,value,lenv)){
				char response = 101;
				write(fd,&response,1);
			}
			else{
				char response = 111;
				write(fd,&response,1);
				printf("insert error\n");
				return -1;	
			}
			break;
		case 12:
			break;
		case 13:
      
      buf = malloc(5);
			rc = READ(fd,buf,4);
			if(rc < 4){
				char response = 111;
				write(fd,&response,1);
				return -1;
			}
			lenk = ntohl(*(int*)buf);
			key = malloc(lenk+1);
			rc = READ(fd,key,lenk);
			if(rc != lenk){
				char response = 111;
				write(fd,&response,1);
				printf("keylen error lenk: %d actual:%d\n",lenk,rc);
				return -1;
			}
      char* resp = cache_get(memcache,key);
      if(resp = NULL){
            char response = 112;
            write(fd,&response,1);
            return -1;
          }
      int len = strlen(resp) + 6;
      char* response = malloc(sizeof(char) * len);
      response[0] = 101;
      response[1] =  htonl(strlen(resp));
      strcpy(response+5,resp);
      write(fd,response,len);
			break;
		case 21:
			break;
		default:
			break;
		}
  return 0;
	
}

void* wait_for_req(void* argv){
	ConcurrentQueue* conqueue = (ConcurrentQueue*) argv;
	while(1){
	epollfd* epfd = concurrent_queue_dequeue(conqueue,(Destroy)epfd_dstr,(Copy) epfd_copy);
	printf("hola que tal que nececita\n");
	if(epfd->type){
		printf("texto detectado\n");
		char* buf = malloc(sizeof(char) * 2048);
		int n = text_consume(epfd,buf);
		printf("text:%d\n",n);
		free(buf);
	}else{
		printf("binario detectado\n");
		int n = bin_consume(epfd);

		printf("bin:%d\n",n);

	}
	free(epfd);
	}
}


void* text_epoll(void* argv){
	ConcurrentQueue* conqueue = (ConcurrentQueue*) argv;
	int epoll_fd = epoll_create1(0);
  printf("text:hola\n");
	int tcsock;
  struct epoll_event textevent;
  textevent.events = EPOLLIN;
  textevent.data.fd = textsock;
  epoll_ctl(epoll_fd,EPOLL_CTL_ADD,textsock,&textevent);
	struct epoll_event textevents[MAX_EVENTS];
	printf("epoll texto configurado\n");
	while (1){
			int text_num_events = epoll_wait(epoll_fd, textevents, MAX_EVENTS, -1);
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
					epoll_ctl(epoll_fd,EPOLL_CTL_ADD,tcsock,&textevent);
				}
				else{
					epollfd* fd = malloc(sizeof(epollfd));
					fd->type = 1;
					fd->fd = textevents[i].data.fd;
					concurrent_queue_enqueue(conqueue,fd,(Copy) epfd_copy);
				}
			}
  }
}


void* bin_epoll(void* argv){
	ConcurrentQueue* conqueue = (ConcurrentQueue*) argv;
  printf("bin:hola\n");
	int epoll_fd = epoll_create1(0);
	int bcsock;
  struct epoll_event binevent;
  binevent.events = EPOLLIN;
  binevent.data.fd = binsock;
  epoll_ctl(epoll_fd,EPOLL_CTL_ADD,binsock,&binevent);
	struct epoll_event binevents[MAX_EVENTS];
	printf("epoll bin configurado\n");
	while (1){
			int bin_num_events = epoll_wait(epoll_fd, binevents, MAX_EVENTS, -1);
			if (bin_num_events == -1) {
				perror("binepoll_wait");
				exit(EXIT_FAILURE);
			}
			for(int i = 0; i<bin_num_events; i++){
				if(binevents[i].data.fd == binsock){
					bcsock = accept(binsock, NULL, NULL);
					if (bcsock < 0) quit("accept");
					printf("cliente aceptado\n");
					binevent.events = EPOLLIN | EPOLLET;
					binevent.data.fd = bcsock;
					epoll_ctl(epoll_fd,EPOLL_CTL_ADD,bcsock,&binevent);
				}
				else{
					epollfd* fd = malloc(sizeof(epollfd));
					fd->type = 0;
					fd->fd = binevents[i].data.fd;
					concurrent_queue_enqueue(conqueue,fd,(Copy) epfd_copy);
				}
			}
  }

}

void server_start(){
	//text fd
	printf("iniciando servidor\n");



	//ACA SE VAN A GUARDAR LOS PEDIDOS, LOS THREADS TOMARAN DE A UNO Y ATENDERAN
	// SIGUIENDO UNA LOGICA DE PRODUCTOR CONSUMIDOR CON ESTRUCTURA FIFO
	ConcurrentQueue* conqueue = malloc(sizeof(ConcurrentQueue));

	concurrent_queue_init(conqueue,30);
	printf("conqueue inicializada\n");
	pthread_t threads[MAX_THREADS];
	for (size_t i = 0; i < MAX_THREADS -2; i++)
	{
	//	epfd[i].id = i;
		pthread_create(&threads[i],NULL,wait_for_req, (void*) (conqueue));
	}
	pthread_t eptext;
	pthread_t epbin;
//	pthread_create(&eptext,NULL,text_epoll, (void*) (conqueue));
	pthread_create(&epbin,NULL,bin_epoll, (void*) (conqueue));
	printf("hilos creados \n");
	pthread_join(epbin,NULL);
	/*
	while (1){
		int text_num_events = epoll_wait(epoll_fd, events, MAX_EVENTS, -1);
        if (text_num_events == -1) {
            perror("textepoll_wait");
            exit(EXIT_FAILURE);
        }
        for(int i = 0; i<text_num_events; i++){
            if(events[i].data.fd == textsock){
                tcsock = accept(textsock, NULL, NULL);
                if (tcsock < 0) quit("accept");
				printf("cliente aceptado\n");
                textevent.events = EPOLLIN | EPOLLET;
                textevent.data.fd = tcsock;
                epoll_ctl(epoll_fd,EPOLL_CTL_ADD,tcsock,&textevent);
            }
            else{
                epollfd* fd = malloc(sizeof(epollfd));
				fd->type = 1;
				fd->fd = textevents[i].data.fd;
				concurrent_queue_enqueue(conqueue,fd,(Copy) epfd_copy);
            }
        }

		//lo mismo para bin

	}
	
	*/

}

unsigned str_KRHash(const char *s) {
  unsigned hashval;
  for (hashval = 0; *s != '\0'; ++s) {
    hashval = *s + 31 * hashval;
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
