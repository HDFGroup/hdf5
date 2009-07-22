#ifndef __BENCHMARK_COMMON_H
#define __BENCHMARK_COMMON_H

/* Headers needed */

#define _GNU_SOURCE
#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include "hdf5.h"

/* Macros */

#ifndef TRUE
#define TRUE 1
#endif /* TRUE */
#ifndef FALSE
#define FALSE 0
#endif /* FALSE */

#define NLEVELS         5
#define NMAPPING        9

#define FILENAME        "swmr_data.h5"
#define DTYPE_SIZE      150

/* Typedefs */

/* Information about a symbol/dataset */
typedef struct {
    char *name;         /* Dataset name for symbol */
    hid_t dsid;         /* Dataset ID for symbol */
    hsize_t nrecords;   /* Number of records for the symbol */
} symbol_info_t;

/* A symbol's record */
typedef struct {
    uint64_t rec_id;    /* ID for this record (unique in symbol) */
    uint8_t info[DTYPE_SIZE];   /* "Other" information for this record */
} symbol_t;

/* Global variables */
extern symbol_info_t *symbol_info[NLEVELS];
extern unsigned symbol_count[NLEVELS];

/* Prototypes */
extern symbol_info_t * choose_dataset(void);
extern hid_t create_symbol_datatype(void);
extern int generate_name(char *name_buf, unsigned level, unsigned count);
extern int generate_symbols(void);
extern int shutdown_symbols(void);

#endif /* __BENCHMARK_COMMON_H */

