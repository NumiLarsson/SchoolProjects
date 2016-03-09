/**
 * Rendezvous
 *
 * Two threads executing chunks of work in a lock step - skeleton
 * code.
 *
 * Author: Nikos Nikoleris <nikos.nikoleris@it.uu.se>
 *
 */

#include <stdio.h>     /* printf() */
#include <stdlib.h>    /* abort(), [s]rand() */
#include <unistd.h>    /* sleep() */
#include <semaphore.h> /* sem_...() */
#include <pthread.h>   /* pthread_...() */

#define LOOPS 4
#define NTHREADS 3
#define MAX_SLEEP_TIME 1


/* TODO: Make the two threads perform their iterations in a
 * predictable way. Both should perform iteration 1 before iteration 2
 * and then 2 before 3 etc. */
typedef volatile int semphore_t;

semphore_t* init(int n) {
    semphore_t *S = malloc(sizeof(int));
    *S = n;
    return S;
}
void fix_wait(semphore_t *S) {
    int oldvalue = *S;
    while (oldvalue <= 0){
        oldvalue = *S;
    };
    __sync_fetch_and_sub(S, 1);
}
void signal(semphore_t *S) {
    __sync_fetch_and_add(S, 1);
}
/*void destroy(semphore_t *S) {
    //free(S);
    }*/

semphore_t* semphoreA;
semphore_t* semphoreB;

void *
threadA(void *param __attribute__((unused)))
{
    int i;

    for (i = 0; i < LOOPS; i++) {

	printf("threadA --> %d iteration\n", i);
	sleep(rand() % MAX_SLEEP_TIME);
        signal(semphoreA);
        fix_wait(semphoreB);
    }

    pthread_exit(0);
}


void *
threadB(void *param  __attribute__((unused)))
{
    int i;

    for (i = 0; i < LOOPS; i++) {

        fix_wait(semphoreA);
	printf("threadB --> %d iteration\n", i);
	sleep(rand() % MAX_SLEEP_TIME);
        
        signal(semphoreB);
    }

    pthread_exit(0);
}

int
main()
{
    pthread_t tidA, tidB;

    srand(time(NULL));
    pthread_setconcurrency(3);
    
    semphoreA = init(0);
    semphoreB = init(0);

    if (pthread_create(&tidA, NULL, threadA, NULL) ||
	pthread_create(&tidB, NULL, threadB, NULL)) {
	perror("pthread_create");
	abort();
    }
    if (pthread_join(tidA, NULL) != 0 ||
        pthread_join(tidB, NULL) != 0) {
	perror("pthread_join");
	abort();
    }

    return 0;
}

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * c-file-style: "stroustrup"
 * End:
 */
