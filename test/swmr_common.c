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

#include "swmr_common.h"

static unsigned symbol_mapping[NMAPPING] = {0, 0, 0, 0, 1, 1, 2, 3, 4};
unsigned symbol_count[NLEVELS] = {100, 200, 400, 800, 1600};

symbol_info_t *symbol_info[NLEVELS];

symbol_info_t *
choose_dataset(void)
{
    unsigned level;             /* The level of the dataset */
    unsigned offset;            /* The "offset" of the dataset at that level */

    /* Determine level of dataset */
    level = symbol_mapping[random() % NMAPPING];

    /* Determine the offset of the level */
    offset = random() % symbol_count[level];

    return(&symbol_info[level][offset]);
} /* end choose_dataset() */

hid_t
create_symbol_datatype(void)
{
    hid_t sym_type_id;          /* Datatype ID for symbol */
    hid_t opaq_type_id;         /* Datatype ID for opaque part of record */

    /* Create opaque datatype to represent other information for this record */
    if((opaq_type_id = H5Tcreate(H5T_OPAQUE, DTYPE_SIZE)) < 0)
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

    return(sym_type_id);
} /* end create_symbol_datatype() */
 
int
generate_name(char *name_buf, unsigned level, unsigned count)
{
    sprintf(name_buf, "%u-%04u", level, count);

    return 0;
} /* end generate_name() */

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

