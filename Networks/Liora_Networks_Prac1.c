#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <string.h>

/*****************************************************************************/
/* MAINTAINABILITY                                                           */
/* How can we enable the user to modify the port number (and if you're keen, */
/* the buffer size) without having to recompile the program?                 */
/*****************************************************************************/

#define PORT    56789           /* default port number for the echo service */
#define BSIZE   11              /* artificially short buffer size           */

#define PROMPT  "Networks Practical Echo Server\n"
#define QUIT    ".\r\n"
#define ENDLN   '\n'

#define TRUE    (0==0)

/*****************************************************************************/
/* MAINTAINABILITY                                                           */
/* How will the user know what this function does?                           */
/*****************************************************************************/

int main(int argc, char *argv[]) {

    int sock;                   /* file descriptor for the server socket */
    struct sockaddr_in server;

    char buf[BSIZE];

    int counter;      // counts telnet session number

    /* 1. Create socket*/

    if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
      //(domain, type, protocol) - IPv4 socket, a TCP stream (fine for text), 0 is IP protocol
                /*************************************************************/
                /* ARGUMENTS look at "man 2 socket" for information          */
                /*************************************************************/
        perror("cannot open a socket");
        exit(EXIT_FAILURE);
    };

    /* 2. Bind an address at the socket*/

    server.sin_family = AF_INET;               // same as socket
    /********************************/
    server.sin_addr.s_addr = INADDR_ANY;     /* ARGUMENT: see "man 2 bind"   */
    server.sin_port = htons( PORT ); //host to network short of port number
    /********************************/

    if (bind(sock, (struct sockaddr *) &server, sizeof(server)) < 0) {
        perror("bind");
        exit(EXIT_FAILURE);
    };

    /* 3. Accept connections*/
                                             /********************************/
    if (listen(sock, 1) < 0) {               /* ARGUMENT: see "man 2 listen" */ //(socket, backlog)
    // backlog = how many pending connections there can be
                                             /********************************/
        perror("listen");
        exit(EXIT_FAILURE);
    };

    counter = 0;      // init session counter for new session

    while (TRUE) {

    /* 4. Wait for client requests*/

        struct sockaddr_in client;
        socklen_t client_len = sizeof(client);
        int stream = accept(sock, (struct sockaddr *) &client, &client_len);

        if (stream < 0) {       // throw error if negative val returned
            perror("accept");
            exit(EXIT_FAILURE);
        };

        int size;
        counter += 1;         // new session; increment counter

        /*********************************************************************/
        /* ERROR HANDLING                                                    */
        /*      is "stream" a valid socket descriptor here?                  */
        /*      can anything go wrong in an of the code that follows?        */
        /*********************************************************************/

                                             /********************************/
        send(stream, PROMPT, sizeof(PROMPT), 0);                /* ARGUMENTS: see "man 2 send"  */
        //(stream socket, welcome prompt, length of prompt, flags)
        // flags: 0 = no flags set

        fprintf(stderr, "Session number: %d\n", counter); // put counter in message

        size = 1;   // init size to >0 to begin loop

        while (size > 0){ //recv returns 0 only when telnet session ends; until then, keep receiving and echoing
          /*                              */
          size = recv(stream, buf, BSIZE, 0);         /* ARGUMENTS: see "man 2 recv"  */
          //(stream socket, buffer, length, flags)
          // length = buffer size, 0 = no flags set
          if (size < 0) { // neg val returned - throw error
              perror("recv");
              exit(EXIT_FAILURE);
          };
                                             /********************************/

          buf[size] = '\0';                    /* null-termination for strings */

          fprintf(stderr, "Text received: %s\n", buf);  // put text in message

                                             /********************************/
          send(stream, buf, size, 0);                /* ARGUMENTS: see "man 2 send"  */
          //(stream socket, buffer, length, flags)
          // length = buffer size, 0 = no flags set
      }
                                             /********************************/
        close(stream);

    }; /* while(TRUE) */

}; /* main */


 /*  Demonstration:
> telnet localhost 56789
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
Networks Practical Echo Server
Hello
Hello
123456789
123456789
1234567891011     // longer string than fits in buffer
1234567891011
Bye
Bye
^]
telnet> close
Connection closed.
> telnet localhost 56789
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
Networks Practical Echo Server
Session 2
Session 2
^]
telnet> close
Connection closed.



./echo
Session number: 1
Text received: Hello

Text received: 123456789

Text received: 12345678910
Text received: 11

Text received: Bye

Text received:
Session number: 2
Text received: Session 2

Text received:

 */
