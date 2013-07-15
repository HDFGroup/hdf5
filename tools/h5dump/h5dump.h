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
#ifndef H5DUMP_H__
#define H5DUMP_H__

#include "hdf5.h"
#include "H5private.h"
#include "h5tools.h"
#include "h5tools_utils.h"
#include "h5tools_ref.h"
#include "h5trav.h"
#include "h5dump_defines.h"

/**
 **  This is the global dispatch table for the dump functions.
 **/
/* the table of dump functions */
typedef struct dump_functions_t {
    void     (*dump_group_function) (hid_t, const char *);
    void     (*dump_named_datatype_function) (hid_t, const char *);
    void     (*dump_dataset_function) (hid_t, const char *, struct subset_t *);
    void     (*dump_dataspace_function) (hid_t);
    void     (*dump_datatype_function) (hid_t);
    herr_t   (*dump_attribute_function) (hid_t, const char *, const H5A_info_t *, void *);
    void     (*dump_data_function) (hid_t, int, struct subset_t *, int);
} dump_functions;

/* List of table structures.  There is one table structure for each file */
typedef struct h5dump_table_list_t {
    size_t      nalloc;
    size_t      nused;
    struct {
        unsigned long   fileno;         /* File number that these tables refer to */
        hid_t           oid;            /* ID of an object in this file, held open so fileno is consistent */
        table_t         *group_table;   /* Table of groups */
        table_t         *dset_table;    /* Table of datasets */
        table_t         *type_table;    /* Table of datatypes */
    } *tables;
} h5dump_table_list_t;

h5dump_table_list_t  table_list = {0, 0, NULL};
table_t             *group_table = NULL, *dset_table = NULL, *type_table = NULL;
int                  dump_indent = 0;              /*how far in to indent the line         */

int          unamedtype = 0;     /* shared datatype with no name */
hbool_t      hit_elink = FALSE;  /* whether we have traversed an external link */
size_t       prefix_len = 1024;
char         *prefix = NULL;
const char   *fp_format = NULL;

/* things to display or which are set via command line parameters */
int          display_all       = TRUE;
int          display_oid       = FALSE;
int          display_data      = TRUE;
int          display_attr_data = TRUE;
int          display_char      = FALSE; /*print 1-byte numbers as ASCII */
int          usingdasho        = FALSE;
int          display_bb        = FALSE; /*superblock */
int          display_dcpl      = FALSE; /*dcpl */
int          display_fi        = FALSE; /*file index */
int          display_ai        = TRUE;  /*array index */
int          display_escape    = FALSE; /*escape non printable characters */
int          display_region    = FALSE; /*print region reference data */
int          enable_error_stack= FALSE; /* re-enable error stack */
int          disable_compact_subset= FALSE; /* disable compact form of subset notation */
int          display_packed_bits = FALSE; /*print 1-8 byte numbers as packed bits*/
int          include_attrs     = TRUE; /* Display attributes */

/* sort parameters */
H5_index_t   sort_by           = H5_INDEX_NAME; /*sort_by [creation_order | name]  */
H5_iter_order_t sort_order     = H5_ITER_INC; /*sort_order [ascending | descending]   */

#define PACKED_BITS_MAX         8  /* Maximum number of packed-bits to display */
#define PACKED_BITS_SIZE_MAX    8*sizeof(long long)  /* Maximum bits size of integer types of packed-bits */
/* mask list for packed bits */
unsigned long long packed_mask[PACKED_BITS_MAX];  /* packed bits are restricted to 8*sizeof(llong) bytes */

/* packed bits display parameters */
int packed_offset[PACKED_BITS_MAX];
int packed_length[PACKED_BITS_MAX];

/*
 * The global table is set to either ddl_function_table or
 * xml_function_table in the initialization.
 */
const dump_functions *dump_function_table;

#ifdef __cplusplus
"C" {
#endif

void     add_prefix(char **prfx, size_t *prfx_len, const char *name);
hid_t    h5_fileaccess(void);
ssize_t  table_list_add(hid_t oid, unsigned long file_no);
ssize_t  table_list_visited(unsigned long file_no);

#ifdef __cplusplus
}
#endif

#endif  /* !H5DUMP_H__ */
