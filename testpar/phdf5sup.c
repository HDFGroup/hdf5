/* debugging tools */
#define MESG(x)\
	printf("%s\n", x);\

#ifdef HAVE_PARALLEL
#define MPI_BANNER(mesg)\
    {printf("================================\n");\
    printf("Proc %d: ", myid); \
    printf("*** %s\n", mesg);\
    printf("================================\n");}
#else
#define MPI_BANNER(mesg)\
    {printf("================================\n");\
    printf("*** %s\n", mesg);\
    printf("================================\n");}
#endif

#ifdef HAVE_PARALLEL
#define SYNC(comm)\
    {MPI_BANNER("doing a SYNC"); MPI_Barrier(comm); MPI_BANNER("SYNC DONE");}

/* pause the process for a moment to allow debugger to attach if desired. */
/* Will pause more if greenlight file is not persent but will eventually */
/* continue. */
#include <sys/types.h>
#include <sys/stat.h>
void pause_proc(MPI_Comm comm, int myid, char* processor_name, int namelen,
    int argc, char **argv)
{

    int pid;
    struct stat statbuf;
    char greenlight[] = "go";
    int maxloop = 10;
    int time_int = 10;

    /* check if an pause interval option is given */
    if (--argc > 0 && isdigit(*++argv))
	time_int = atoi(*argv);
    pid = getpid();
    printf("Proc %d (%*s): pid = %d\n",
	myid, namelen, processor_name, pid);

    if (myid == 0)
	while ((stat(greenlight, &statbuf) == -1) && maxloop-- > 0){
	    printf("waiting(%ds) for file %s ...", time_int, greenlight);
	    fflush(stdout);
	    sleep(time_int);
	}
    MPI_Barrier(comm);
}
#endif /*HAVE_PARALLEL*/


