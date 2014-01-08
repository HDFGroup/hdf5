/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:     swmr_common.c
 *
 * Purpose:     Utility functions for the SWMR test code.
 *
 *-------------------------------------------------------------------------
 */

/***********/
/* Headers */
/***********/

#include <assert.h>

#include "swmr_common.h"

/*******************/
/* Local Variables */
/*******************/

/* The SWMR data arrays:
 *
 * The code uses a 2-D jagged array of datasets.  The first dimension is called
 * the 'level' and there are five of them.
 *
 * #define NLEVELS         5
 *
 * The second dimension is the 'count' and there are quite a few datasets per
 * 'level'.
 *
 * unsigned symbol_count[NLEVELS] = {100, 200, 400, 800, 1600};
 *
 * These datasets are created when the skeleton is generated and are initially
 * empty.  Each dataset has no upper bound on size (H5S_UNLIMITED).  They
 * are of compound type, with two members: an integer ID and an opaque
 * 'data part'.  The data part is not used by the SWMR testing.
 *
 * The SWMR testing will then randomly add and/or remove entries
 * from these datasets.  The selection of the level is skewed by a mapping
 * table which preferentially hammers on the lower levels with their smaller
 * number of datasets.
 *
 * static unsigned symbol_mapping[NMAPPING] = {0, 0, 0, 0, 1, 1, 2, 3, 4};
 *
 * The information about each dataset (name, hid_t, etc.) is stored in a
 * separate array.
 *
 * symbol_info_t *symbol_info[NLEVELS];
 */

/* An array of dataset levels, used to select the level for a SWMR operation
 * Note that this preferentially selects the lower levels with their smaller
 * number of datasets.
 */
static unsigned symbol_mapping[NMAPPING] = {0, 0, 0, 0, 1, 1, 2, 3, 4};

/* The number of datasets at each level */
unsigned symbol_count[NLEVELS] = {100, 200, 400, 800, 1600};

/* Array of dataset information entries (1 per dataset) */
symbol_info_t *symbol_info[NLEVELS];


/*-------------------------------------------------------------------------
 * Function:    choose_dataset
 *
 * Purpose:     Selects a random dataset in the SWMR file
 *
 * Parameters:  N/A
 *
 * Return:      Success:    A pointer to information about a dataset.
 *              Failure:    Can't fail
 *
 *-------------------------------------------------------------------------
 */
symbol_info_t *
choose_dataset(void)
{
    unsigned level;             /* The level of the dataset */
    unsigned offset;            /* The "offset" of the dataset at that level */

    /* Determine level of dataset */
    level = symbol_mapping[random() % NMAPPING];

    /* Determine the offset of the level */
    offset = random() % symbol_count[level];

    return &symbol_info[level][offset];
} /* end choose_dataset() */


/*-------------------------------------------------------------------------
 * Function:    create_symbol_datatype
 *
 * Purpose:     Create's the HDF5 datatype used for elements in the SWMR
 *              testing datasets.
 *
 * Parameters:  N/A
 *
 * Return:      Success:    An HDF5 type ID
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
hid_t
create_symbol_datatype(void)
{
    hid_t sym_type_id;          /* Datatype ID for symbol */
    hid_t opaq_type_id;         /* Datatype ID for opaque part of record */

    /* Create opaque datatype to represent other information for this record */
    if((opaq_type_id = H5Tcreate(H5T_OPAQUE, (size_t)DTYPE_SIZE)) < 0)
        return -1;

    /* Create compound datatype for symbol */
    if((sym_type_id = H5Tcreate(H5T_COMPOUND, sizeof(symbol_t))) < 0)
        return -1;

    /* Insert fields in symbol datatype */
    if(H5Tinsert(sym_type_id, "rec_id", HOFFSET(symbol_t, rec_id), H5T_NATIVE_UINT64) < 0)
        return -1;
    if(H5Tinsert(sym_type_id, "info", HOFFSET(symbol_t, info), opaq_type_id) < 0)
        return -1;

    /* Close opaque datatype */
    if(H5Tclose(opaq_type_id) < 0)
        return -1;

    return sym_type_id;
} /* end create_symbol_datatype() */


/*-------------------------------------------------------------------------
 * Function:    generate_name
 *
 * Purpose:     Generates a SWMR testing dataset name given a level and
 *              count.
 *              The name is in the format <name>-<level> (%u-%04u).
 *
 * Parameters:  char *name_buf
 *              Buffer for the created name.  Must be pre-allocated.
 *              Since the name is formulaic, this isn't considered an issue.
 *
 *              unsigned level
 *              The dataset's level
 *
 *              unsigned count
 *              The dataset's count
 *
 * Return:      Success:    0
 *                          
 *              Failure:    Can't fail
 *
 *-------------------------------------------------------------------------
 */
int
generate_name(char *name_buf, unsigned level, unsigned count)
{
    assert(name_buf);
    
    sprintf(name_buf, "%u-%04u", level, count);

    return 0;
} /* end generate_name() */


/*-------------------------------------------------------------------------
 * Function:    generate_symbols
 *
 * Purpose:     Initializes the global dataset infomration arrays.
 *
 * Parameters:  N/A
 *
 * Return:      Success:    0
 *              Failure:    Can't fail
 *
 *-------------------------------------------------------------------------
 */
int
generate_symbols(void)
{
    unsigned u, v;      /* Local index variables */

    for(u = 0; u < NLEVELS; u++) {
        symbol_info[u] = (symbol_info_t *)malloc(symbol_count[u] * sizeof(symbol_info_t));
        for(v = 0; v < symbol_count[u]; v++) {
            char name_buf[64];

            generate_name(name_buf, u, v);
            symbol_info[u][v].name = (char *)malloc(strlen(name_buf) + 1);
            strcpy(symbol_info[u][v].name, name_buf);
            symbol_info[u][v].dsid = -1;
            symbol_info[u][v].nrecords = 0;
        } /* end for */
    } /* end for */

    return 0;
} /* end generate_symbols() */


/*-------------------------------------------------------------------------
 * Function:    shutdown_symbols
 *
 * Purpose:     Cleans up the global dataset information arrays.
 *
 * Parameters:  N/A
 *
 * Return:      Success:    0
 *              Failure:    Can't fail
 *
 *-------------------------------------------------------------------------
 */
int
shutdown_symbols(void)
{
    unsigned u, v;      /* Local index variables */

    /* Clean up the symbols */
    for(u = 0; u < NLEVELS; u++) {
        for(v = 0; v < symbol_count[u]; v++)
            free(symbol_info[u][v].name);
        free(symbol_info[u]);
    } /* end for */

    return 0;
} /* end shutdown_symbols() */


/*-------------------------------------------------------------------------
 * Function:    print_metadata_retries_info
 *
 * Purpose:     To retrieve and print the collection of metadata retries for the file.
 *
 * Parameters:  fid: the currently opened file identifier
 *
 * Return:      Success:    0
 *              Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
int
print_metadata_retries_info(hid_t fid)
{
    H5F_retry_info_t info;
    unsigned i;

    /* Retrieve the collection of retries */
    if(H5Fget_metadata_read_retry_info(fid, &info) < 0)
	return (-1);

    /* Print information for each non-NULL retries[i] */
    for(i = 0; i < H5F_NUM_METADATA_READ_RETRY_TYPES; i++) {
        unsigned power;
        unsigned j;

	if(NULL == info.retries[i])
	    continue;

	fprintf(stderr, "Metadata read retries for item %u:\n", i);
	power = 1;
	for(j = 0; j < info.nbins; j++) {
	    if(info.retries[i][j])
		fprintf(stderr, "\t# of retries for %u - %u retries: %u\n", 
		       power, (power * 10) - 1, info.retries[i][j]);
	    power *= 10;
	} /* end for */
    } /* end for */

    /* Free memory for each non-NULL retries[i] */
    for(i = 0; i < H5F_NUM_METADATA_READ_RETRY_TYPES; i++)
        if(info.retries[i] != NULL)
            free(info.retries[i]);

    return 0;
} /* print_metadata_retries_info() */

