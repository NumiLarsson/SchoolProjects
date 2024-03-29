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
	int i, seed;
	pid_t pid[NUM_PLAYERS];
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
	
	
	int seedPipe[NUM_PLAYERS][2];
	int scorePipe[NUM_PLAYERS][2];
	
	
	
	
	// TODO 3: initialize the communication with the players, i.e. create the pipes

	for (i = 0; i < NUM_PLAYERS; i++) {
	  pipe(seedPipe[i]);
	  pipe(scorePipe[i]);
	}
	

	// TODO 4: spawn/fork the processes that simulate the players
	//         - check if players were successfully spawned
	//         - is it needed to store the pid of the players? Which data structure to use for this?
	//         - re-direct standard-inputs/-outputs of the players
	//         - use execv to start the players
	//         - pass arguments using args and sprintf
	
	for (i= 0; i < NUM_PLAYERS; i++) {
	   pid[i] = fork();
	  
	  if (pid[i]==-1){
	    perror("Fork error!"); 
	    exit(EXIT_FAILURE);
	  }
	  else if(pid[i]==0){
	    close(seedPipe[i][1]);
	    close(scorePipe[i][0]);
	  
  
	    dup2(seedPipe[i][0],STDIN_FILENO);
	    dup2(scorePipe[i][1],STDOUT_FILENO);
	    sprintf(arg1,"%d",i);
	    execve(args[0],args,NULL);
	    
	  }



	    

	}
	seed = time(NULL);
	
	
	for (i = 0; i < NUM_PLAYERS; i++) {
	  seed++;
	  // TODO 5: send the seed to the players (write using pipes)
	    write(seedPipe[i][1],&seed,sizeof(int));
	    close(seedPipe[i][1]);
	  }


	// TODO 6: read the dice results from the players via pipes, find the winner
	  int maxScore = 0;
	  int tempScore = 0;
	  
	  for (i = 0; i < NUM_PLAYERS; i++) {
	    read(scorePipe[i][0],&tempScore,sizeof(int));
	    close(scorePipe[i][0]);

	    if (tempScore > maxScore){
	      maxScore = tempScore;
	      winner =i;
	    }
	    
	  }
	  

	printf("master: player %d WINS\n", winner);


	// TODO 7: signal the winner
	//         - which command do you use to send signals?
	//         - you will need the pid of the winner
	kill(pid[winner],SIGUSR1);
	

	// TODO 8: signal all players the end of game
	//         - you will need the pid of all the players

	for (i = 0; i < NUM_PLAYERS; i++) {
	  kill(pid[i],SIGUSR2);
	}


	printf("master: the game ends\n");


	// TODO 9: cleanup resources and exit with success
	//         wait for all the players/children to exit
	//         before game master exits 

	int status;
	 
	for (i = 0; i < NUM_PLAYERS; i++) {
	 
	  wait(&status);
	  exit(EXIT_SUCCESS);
	  
	}
	
	return 0;
	}
	
