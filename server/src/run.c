#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>

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

int main()
{
//	uid_t uid = getuid();
 // if(uid){
  //    quit("SE REQUIEREN PERMISOS DE SUPERUSUARIO");
  //}
	int tSock, bSock;
	tSock = mk_lsock(8888);
  bSock = mk_lsock(8889);
	setuid(1000);
  memcached_start(tSock,bSock);
}