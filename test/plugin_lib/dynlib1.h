#include <stdlib.h>
#include <stdio.h>
#include <hdf5.h>

#define FILTER_DYNLIB1_VERS 1

/* Local prototypes for filter functions */
static size_t H5Z_filter_dynlib1(unsigned int flags, size_t cd_nelmts,
    const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);
