/**
 * Game of luck: Implementation of the Gamemaster
 *
 * Course: Operating Systems and Multicore Programming - OSM lab
 * assignment 1: game of luck.
 *
 * Author: Nikos Nikoleris <nikos.nikoleris@it.uu.se>
 *
 * History
 *
 * 2016-01-31 (Mahdad Davari)
 *
 * Added more annotations and fine-grained TODO lists 
 *
 */

#include <stdio.h> /* I/O functions: printf() ... */
#include <stdlib.h> /* rand(), srand() */
#include <unistd.h> /* read(), write() calls */
#include <assert.h> /* assert() */
#include <time.h>   /* time() */
#include <signal.h> /* kill(), raise() and SIG???? */

#include <sys/types.h> /* pid */
#include <sys/wait.h> /* waitpid() */

#include "common.h"

int main(int argc, char *argv[])
{
  pid_t pid[NUM_PLAYERS];
  int i, seed;

	char arg0[] = "./shooter"; 
	char arg1[10]; 
	char *args[] = {arg0, arg1, NULL};

	int seedPipeArray[NUM_PLAYERS][2];
	int scorePipeArray[NUM_PLAYERS][2];
	
	for ( i = 0; i < NUM_PLAYERS; i++ ) {
	  //For every player, add a seedPipe and a scorePipe
	  int tempPipe = pipe( seedPipeArray[i] );
	  if ( tempPipe == -1 ) {
	    perror("Pipe");
	    exit(EXIT_FAILURE);
	  }
	  
	  tempPipe = pipe( scorePipeArray[i] );
	  if ( tempPipe == -1 ) { //If making a pipe fails it returns -1
	    perror("Pipe");
	    exit(EXIT_FAILURE);
	  }
	}

	//Fork once per NUM_PLAYERS
	for (i = 0; i < NUM_PLAYERS; i++) {
	  pid[i] = fork();
	  if ( pid[i] == -1) {
	    perror("fork");
	    exit(EXIT_FAILURE);
	  }
	  else if ( pid[i] == 0) {
	    //Child
	    close( seedPipeArray[i][1] );
	    close( scorePipeArray[i][0] );

	    dup2( seedPipeArray[i][0], STDIN_FILENO );
	    dup2( scorePipeArray[i][1], STDOUT_FILENO );

	    sprintf( arg1, "%d", i ) ;
	    
	    execv( arg0, args );
	    //shooter (i, seedPipeArray[i][0], scorePipeArray[i][1]);
	    perror("Child didn't exec properly!");
	    exit(EXIT_FAILURE);
	  }
	  else {
	    //Parent
	    close(seedPipeArray[i][0]);
	    close(scorePipeArray[i][1]);
	  }
	}


	seed = time(NULL);

	for (i = 0; i < NUM_PLAYERS; i++) {
	  seed++;
	  write(seedPipeArray[i][1], &seed, sizeof(int) );
	  //Not sure how long the seed is, but let's put it as int.
	}

	int winner = 0;
	int maxResult = 0;

	int results[NUM_PLAYERS];
	
	for (i = 0; i < NUM_PLAYERS; i++) {
	  read( scorePipeArray[i][0], &results[i], sizeof(int) );
	  if ( results[i] > maxResult ) {
	    winner = i;
	    maxResult = results[i];
	  }
	}


	printf("master: player %d WINS\n", winner);
	kill( pid[winner], SIGUSR1 );

	for (i = 0; i < NUM_PLAYERS; i++) {
	  kill( pid[i], SIGUSR2 ); //Kill all players / Childs.
	}


	printf("master: the game ends\n");

	int* pExitStatus = 0;
	for (i = 0; i < NUM_PLAYERS; i++) {
	  wait(pExitStatus);
	}

	return 0;
}
