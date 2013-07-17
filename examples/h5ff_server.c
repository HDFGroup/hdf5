/* 
 * test_server.c: Server side of Milestone 3.3 Asynchronous I/O and initial
 * IOD VOL plugin demonstration.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "mpi.h"
#include "hdf5.h"

int main(int argc, char **argv) {
    int my_size, my_rank;
    int provided;

    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
    if(MPI_THREAD_MULTIPLE != provided) {
        printf("MPI does not have MPI_THREAD_MULTIPLE support\n");
        exit(1);
    }

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &my_size);
    printf("Number of server processes = %d, my rank is %d\n", my_size, my_rank);

    //H5open();
    /* This call initiliazes the FS for the server processes (create metadata and
       bulk data handles). It also registers with the Function shipper the 
       HDF5 VOL server routines that will be executed when the clients ship the VOL 
       routines. Then it executes a loop to receive requests from clients that map 
       to one of the registered callbacks. 

       Whenever a request is received, the corresponding callback is called which 
       inserts the operation into the AXE (the Asynchronous eXecution Engine) and 
       returns to receive another request. The AXE schedules that operation to 
       execute by assigning a thread to it. After the operation is complete, the 
       result is returned to the client through the function shipper complete call.

       Finally, when all clients send a terminate call, the function shipper interface
       is finalized the operation returns. */
    H5VLiod_start_handler(MPI_COMM_WORLD, MPI_INFO_NULL);
    //H5close();
    MPI_Finalize();

    return 0;
}

