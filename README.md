# Memcached

Memcached a la cual se pueden conectar clientes en modo binario y modo texto.\
La memcached se encuentra en la carpeta server, mientras que en la carpeta client se encuentrra un cliente para utilizar el servidor en modo binario.


## Uso/Ejemplos Memcached

Para compilar la memcached se debe ir a la carpeta server y ejecutar el comando make, luego de ejecutar el mismo se crearán dos carpetas, una es la carpeta obj, la cual tiene object files de la compilación y la otra carpeta se llama bin, la cual tiene el programa a ejecutar. Se debe ejecutar el programa como superusuario.\
Una vez ejecutado el servidor, este creará dos sockets de escucha en los puertos 888 y 889, el puerto 888 es usado para que se conecten clientes en modo texto y el puerto 889 es usado para que se conecta clientes en modo binario.

### Modo texto
A modo de ejemplo se explica a continuación como conectarse al modo texto usando netcat:\
Para conectarnos al servidor en nuestro localhost usamos el siguiente comando
```bash
$ nc localhost 888
```
Una vez iniciado la conexión podremos ejecutar los comandos PUT, GET, DEL Y STATS.
- El comando PUT se utiliza para insertar elementos en la memcached indicando la clave y el valor, en el siguiente comando K representa la clave y V el valor.
```bash
 PUT K V
```
- El comando GET se utiliza para obtener los valores insertados con PUT en el caso que la clave no se encuentra el servidor enviará ENOTFOUND, en el siguiente comando K es la clave.
```bash
 GET K
```
- El comando DEL se utiliza para borrar los elementos ingresados con PUT, si tal clave no se encuentra, el servidor devolverá ENOTFOUND, en el siguiente comando K es la clave.
```bash
 DEL K
```
- El comando STATS se utiliza para obtener infomración del uso de la memcached. Luego de ingresar tal comando, el servidor contesta con lo siguiente, dónde n1, n2 y n3 son los números de consultas PUT, DEL y GET respectivamente y n4 es el número de elementos guardados actualmente en la memcache
```bash
 OK PUTS=n1 DELS=n2 GETS=n3 KEYS=n4
```
Tener en cuenta que ningún pedido ni respuesta puede superar los 2048 caracteres, en caso de superarse tal límite, el servidor devolverá EBIG. Además si el formato de las consultas no es correcta el sevidor contestará EINVALID
### Modo binario
Ahora explicaremos como usar el modo binario con el cliente que se encuentra en la carpeta client.\
El cliente está escrito en el lenguaje Erlang y debemos usar la shell del lenguaje para utilizarlo, una vez iniciada la shell ya podemos compilar el cliente de la siguiente forma
```bash
 > c(client).
```
Luego podemos establecer una conexión con el servidor, con el comando start, el argumento Address puede ser un hostname o la dirección IP de donde está corriendo el servidor. Por ejemplo, si el servidor está corriendo en su máquina y lo quiere ejecutar desde la misma debería pasarle localhost.
```bash
 > client:start(Address).
```
La función start devolverá un process ID (PID), el cual debe ser pasado a las demás funciones del cliente. A continuación se explicarán las demás funciones.
- put/3 se utiliza para insertar un elemento en la memcached, el primer argumento debe ser el PID devuelto por start, el primer y segundo argumento la clave y valor a ingresar respectivamente. El servidor responderá con ok si el elemento fué ingresado correctamente.
```bash
 > client:put(Pid, K, V).
```
- get/2 se utiliza para obtener el valor asociado a una clave dada. El primer argumento es el PID devuelto por start y el segundo es la clave. Si se encuentra la clave en la memcached el servidor contestará con {ok, Value} dónde Value es el valor asociado a la clave y si no encuentra la clave contestará con enotfound. 
```bash
 > client:get(Pid, K).
```
- del/2 se utiliza para borrar una clave y su valor asociado de la memcached. El primer argumento es el PID devuelto por start y el segundo es la clave. El servidor responderá ok en el caso que la clave y su valor asociado sean eliminados con éxito o contestará enotfound si la clave dada no se encuentra guardada.
```bash
 > client:del(Pid, K).
```
- stats/1 se utiliza para obtener infomración del uso de la memcached. El primer argumento es el PID devuelto por start. El servidor responde con la misma información con la que responde STATS en modo texto.
```bash
 > client:stats(Pid).
```
-close/1 se utiliza para cerrar la conexión con el servidor. El primer argumento es el PID devuelto por start.
```bash
 > client:close(Pid).
```
## Documentación

La documentación de la memcached se encuentra en la carpeta server y se puede generar con el comando doxygen, una vez ejecutado el comando se crearán dos carpetas llamadas html y latex, se puede ver la documentación en cualquier navegador abriendo el archivo index.html que se encuentra en la carpeta html. Además se encuentra el informe del proyecto en la carpeta informe.
