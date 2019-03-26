#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <mpi.h>
#include <hdf5.h>
#include <H5VLrados_public.h>

/* Macros for printing standard messages and issuing errors */
#define AT()            printf ("        at %s:%d in %s()...\n", __FILE__, __LINE__, __FUNCTION__)
#define FAILED()        do {puts("*FAILED*");fflush(stdout);} while(0)
#define ERROR           do {FAILED(); AT(); goto error;} while(0)
#define PRINTF_ERROR(...) do {FAILED(); AT(); printf("        " __VA_ARGS__); printf("\n"); goto error;} while(0)

/* Config file */
#ifdef HDF5_USE_MOBJECT
#define CEPH_CONFIG_FILE "/tmp/mobject-cluster-test.gid"
#else
#define CEPH_CONFIG_FILE "ceph.conf"
#endif

