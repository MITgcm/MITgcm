/*
 *
 *
 *  A PTHREADS convenience wrapper intended primarily for use with
 *  gcc/g77 which does not support an "automatic" (here, meant as a
 *  compiler-supported) threading mechanism.
 *
 */

#ifdef HAVE_PTHREADS

#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>
#include <pthread.h>


void ptreentry_(int* myThid);

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
int             num_threads = 0;

void *
threadfunc(void *parm)
{
    int myThid;
    int rc;

    rc = pthread_mutex_lock(&mutex);
    num_threads++;
    myThid = num_threads;
    rc = pthread_mutex_unlock(&mutex);
    ptreentry_(&myThid);
}

void
ptinit_(int* nthreads)
{
    int             i,j;
    int             rc;
    pthread_t       threadids[10];
    pthread_attr_t  pta;

    pthread_attr_init(&pta);
    pthread_attr_setdetachstate(&pta, PTHREAD_CREATE_JOINABLE);

    /*  start the threads  */
    for (i=0; i<(*nthreads); i++)
	rc = pthread_create(&threadids[i], &pta, threadfunc, NULL);

    /*  wait on thread termination  */
    for (i=0; i<(*nthreads); i++)
        rc = pthread_join(threadids[i], NULL);
}

#endif /*  HAVE_PTHREADS  */
