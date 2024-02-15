#include <assert.h>
#include <string.h>
#include <unistd.h>
#include "common.h"

 void quit(char *s){
	perror(s);
	exit(1);
}