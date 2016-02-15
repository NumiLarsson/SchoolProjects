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

	// TODO 1: Un-comment the following variables to use them in the 
	//         exec system call. Using the function sprintf and the arg1 
	//         variable you can pass the id parameter to the children 

	char arg0[] = "./shooter"; 
	char arg1[10]; 
	char *args[] = {arg0, arg1, NULL};
	

	// TODO 2: Declare pipe variables
	//         - Of which data type should they be?
	//         - How many pipes are needed?
	//         - Try to choose self-explanatory variable names, e.g. seedPipe, scorePipe

	// TODO 3: initialize the communication with the players, i.e. create the pipes
	//Array of pipes, size of NUM_PLAYERS
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
	  if ( tempPipe == -1 ) {
	    perror("Pipe");
	    exit(EXIT_FAILURE);
	  }
	}

	

	// TODO 4: spawn/fork the processes that simulate the players
	//         - check if players were successfully spawned
	//         - is it needed to store the pid of the players? Which data structure to use for this?
	//         - re-direct standard-inputs/-outputs of the players
	//         - use execv to start the players
	//         - pass arguments using args and sprintf

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

	    dup2( 0, seedPipeArray[i][0] );
	    dup2( 1, scorePipeArray[i][1] );
	    
	    execv( arg0, args );
	    //shooter (i, seedPipeArray[i][0], scorePipeArray[i][1]);
	    // void shooter(int pid, int seed_fd_rd, int score_fd_write);
	    //Has to use exec to initialise ./shooter, otherwise each child creates a new child.
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
	  write(seedPipeArray[i][1], &seed, sizeof(int) ); //1 byte is the mystery.
	   // TODO 5: send the seed to the players (write using pipes)
	}


	// TODO 6: read the dice results from the players via pipes, find the winner
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


	// TODO 7: signal the winner
	//         - which command do you use to send signals?
	//         - you will need the pid of the winner
	kill( pid[winner], SIGUSR1 );
	
	// TODO 8: signal all players the end of game
	//         - you will need the pid of all the players

	for (i = 0; i < NUM_PLAYERS; i++) {
	  kill( pid[i], SIGUSR2 );
	}


	printf("master: the game ends\n");


	// TODO 9: cleanup resources and exit with success
	//         wait for all the players/children to exit
	//         before game master exits
	int* pExitStatus = 0;
	for (i = 0; i < NUM_PLAYERS; i++) {
	  wait(pExitStatus);
	}

	return 0;
}
