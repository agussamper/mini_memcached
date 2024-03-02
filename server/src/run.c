#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/ip.h>

#include "memcached.h"
#include "common.h"

/**
 * Crea un socket de escucha en el puerto
 * indicado por parámetro
 * @param puerto puerto en el cual se quiere
 * crear el socket de escucha.
 * @return
 * retorna el file descriptor del socket
 * creado.
*/
int mk_lsock(in_port_t puerto)
{
	struct sockaddr_in sa;
	int lsock;
	int rc;
	int yes = 1;

	/* Crear socket */
	lsock = socket(AF_INET, SOCK_STREAM, 0);
	if (lsock < 0)
		quit("socket");

	/* Setear opción reuseaddr... normalmente no es necesario */
	if (setsockopt(lsock, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof yes) == 1)
		quit("setsockopt");

	sa.sin_family = AF_INET;
	sa.sin_port = htons(puerto);
	sa.sin_addr.s_addr = htonl(INADDR_ANY);

	/* Bindear al puerto "puerto" TCP, en todas las direcciones disponibles */
	rc = bind(lsock, (struct sockaddr *)&sa, sizeof sa);
	if (rc < 0)
		quit("bind");

	/* Setear en modo escucha */
	rc = listen(lsock, 10);
	if (rc < 0)
		quit("listen");

	return lsock;
}

/**
 * Setea el límite de memoria del programa
 * @param bytes Cantidad máxima de bytes que
 * puede usar el programa
*/
void setmemlimit(uint64_t bytes) {
  struct rlimit memlimit;
  memlimit.rlim_cur = bytes;
  memlimit.rlim_max = bytes;
  setrlimit(RLIMIT_AS, &memlimit);
}

void lower_privileges() {
	char* uid_str = getenv("SUDO_UID");
	if (uid_str == NULL) {
    fprintf(stderr, 
			"The UID enviroment variable is not defined.\n");
    exit(EXIT_FAILURE);
  }
	uid_t uid = atoi(uid_str);
	if(setuid(uid) == -1) {
		perror("Error changing effective user ID");
    exit(EXIT_FAILURE);
	}
}

int main()
{
	uid_t uid = getuid();
  if(uid){
    puts("SE REQUIEREN PERMISOS DE SUPERUSUARIO");
		return 0;
  }
	uint64_t maxBytes = 1;
	maxBytes = maxBytes << 30; //1GB
	setmemlimit(maxBytes);
	int tSock, bSock;
	tSock = mk_lsock(888);
  bSock = mk_lsock(889);
	lower_privileges();
	memcached_start(tSock,bSock);
	return 0;
}