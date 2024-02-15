#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include "text_manage.h"
#include "parser.h"

#define MAX_FORWARD 11

#define READ(fd, buf, n) ({						\
	int rc = read(fd, buf, n);				\
	if (rc < 0)	\
		return -2;						\
	if (rc == 0)						\
		return -1;	\
	rc; })

void text_handle(
    Cache cache, int fd,
 		char *toks[3], int lens[3],
		int ntok){
	if(!strcmp(toks[0],"PUT")){
		if(ntok !=3){
			char response[20];
			int len = sprintf(response,
				"EINVAL NTOK %d\n",ntok);
			write(fd,response,len);
			return;
		} 
		char val[lens[2]+1]; 
		sprintf(val,"%s\0",toks[2]);
		cache_insert(cache,
			toks[1], lens[1], 
			val , lens[2] + 1, 0);
		write(fd,"OK\n",3);
		return;
	}
	else if(!strcmp(toks[0],"GET")){
		if(ntok != 2){
			write(fd,"EINVAL\n",7);
			return;
		}
		ValData* val = cache_get(cache,
		 	toks[1], strlen(toks[1]));
		if(val == NULL){
			write(fd,"ENOTFOUND\n",10);
			return;
		}
		if(val->isBin){
			write(fd,"EBINARY\n",8);
			return;
		}
		char res[2045];
		//printf("get value: %s \n len value %ld \n %d \n",val->value,val->valSize,strlen(val->value));
		sprintf(res,"OK %s\n",val->value);
		write(fd,res,val->valSize+4);
		free(val->value);
		free(val);
		return;
	}
	else if(!strcmp(toks[0],"DEL")){
		if(ntok != 2){
			write(fd,"EINVAL\n",7);
			return;
		}
		int res = cache_delete(cache, 
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
		char* stats = cache_getStats(cache);
		write(fd,stats,strlen(stats));
		free(stats);
		return;
	}else{
			write(fd,"EINVAL\n",7);
			return;
	}
}


int ebig(char buf[2048], uint64_t* offset, int fd){
	  int i = 0;
		int nlen = 0;
		char* p = buf;
		int nread = READ(fd,buf,2048);
		while(i<MAX_FORWARD && (p = memchr(buf, '\n', nread)) == NULL){
			i++;
			nread = READ(fd,buf,2048);
		}
		if(i == MAX_FORWARD && p == NULL){
			return -1;
		}else{
			p++;
			nlen = p - buf;
			*offset = nread - nlen;
			memmove(buf, p, *offset);
			if(buf[nread-1] == '\n'){
					return 1;
			}
			return 0;
		}
}


int text_consume(Cache cache, int fd, char buf[2048], uint64_t* offset){
		int nread = READ(fd, buf + *offset, 2048-*offset);
		*offset += nread;
		char *p, *p0 = buf;
		uint64_t nlen = *offset;

		while ((p = memchr(p0, '\n', nlen)) != NULL) {
			uint64_t len = p - p0;
			*p++ = 0;
            //log(3, "full command: <%s>", p0);
			char *toks[3]= {NULL};
			int lens[3] = {0};
			int ntok;
			ntok = text_parser(buf,toks,lens);
			for(int i =0; i <ntok;i++){
				printf("tok%d : %s len : %d\n",i,toks[i],lens[i]);
			}
			text_handle(cache, fd,
        toks,lens,ntok);
			nlen -= len + 1;
			p0 = p;
		}
    if (p0 != buf) {
			memmove(buf, p0, nlen);
			*offset = nlen;
			}
		if(*offset == 2048){
			write(fd,"EBIG\n",5);
			*offset = 0;
			int fwd = ebig(buf,offset,fd);
			if(fwd == 1){
				return text_consume(cache,fd,buf,offset);
			}else return fwd;
		}
  return 0;
}