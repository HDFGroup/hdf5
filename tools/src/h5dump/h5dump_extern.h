/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
#ifndef H5DUMP_EXTERN_H__
#define H5DUMP_EXTERN_H__

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

extern h5dump_table_list_t  table_list;
extern table_t             *group_table, *dset_table, *type_table;
extern unsigned                  dump_indent;              /*how far in to indent the line         */

extern int          unamedtype;     /* shared datatype with no name */
extern hbool_t      hit_elink;  /* whether we have traversed an external link */
extern size_t       prefix_len;
extern char         *prefix;
extern const char   *fp_format;

/* things to display or which are set via command line parameters */
extern int          display_all;
extern int          display_oid;
extern int          display_data;
extern int          display_attr_data;
extern int          display_char; /*print 1-byte numbers as ASCII */
extern int          usingdasho;
extern int          display_bb; /*superblock */
extern int          display_dcpl; /*dcpl */
extern int          display_fi; /*file index */
extern int          display_ai;  /*array index */
extern int          display_escape; /*escape non printable characters */
extern int          display_region; /*print region reference data */
extern int          disable_compact_subset; /* disable compact form of subset notation */
extern int          display_packed_bits; /*print 1-8 byte numbers as packed bits*/
extern int          include_attrs; /* Display attributes */
extern int          display_vds_first; /* vds display to first missing */
extern int          vds_gap_size; /* vds skip missing files */

#define PACKED_BITS_MAX         8  /* Maximum number of packed-bits to display */
#define PACKED_BITS_SIZE_MAX    8*sizeof(long long)  /* Maximum bits size of integer types of packed-bits */
/* mask list for packed bits */
extern unsigned long long packed_mask[PACKED_BITS_MAX];  /* packed bits are restricted to 8*sizeof(llong) bytes */

/* packed bits display parameters */
extern unsigned packed_offset[PACKED_BITS_MAX];
extern unsigned packed_length[PACKED_BITS_MAX];

/*
 * The global table is set to either ddl_function_table or
 * xml_function_table in the initialization.
 */
extern const dump_functions *dump_function_table;

#ifdef __cplusplus
extern "C" {
#endif

void     add_prefix(char **prfx, size_t *prfx_len, const char *name);
hid_t    h5_fileaccess(void);
ssize_t  table_list_add(hid_t oid, unsigned long file_no);
ssize_t  table_list_visited(unsigned long file_no);

#ifdef __cplusplus
}
#endif

#endif  /* !H5DUMP_EXTERN_H__ */
