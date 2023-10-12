/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef H5DIFF_H
#define H5DIFF_H

#include "hdf5.h"
#include "h5tools.h"
#include "h5trav.h"

#define MAX_FILENAME 1024

/*-------------------------------------------------------------------------
 * This is used to pass multiple args into diff().
 * Passing this instead of several each arg provides smoother extensibility
 * through its members along with MPI code for ph5diff
 * as it doesn't require interface change.
 *------------------------------------------------------------------------*/
typedef struct {
    h5trav_type_t type[2];
    bool          is_same_trgobj;
} diff_args_t;
/*-------------------------------------------------------------------------
 * command line options
 *-------------------------------------------------------------------------
 */
/* linked list to keep exclude path list */
struct exclude_path_list {
    const char               *obj_path;
    h5trav_type_t             obj_type;
    struct exclude_path_list *next;
};

/* Enumeration value for keeping track of whether an error occurred or differences were found */
typedef enum {
    H5DIFF_NO_ERR,   /* No error occurred */
    H5DIFF_ERR_DIFF, /* Differences were found */
    H5DIFF_ERR       /* An error occurred */
} diff_err_t;

typedef struct {
    int                       mode_quiet;   /* quiet mode: no output at all */
    int                       mode_report;  /* report mode: print the data */
    int                       mode_verbose; /* verbose mode: print the data, list of objcets, warnings */
    int                       mode_verbose_level;     /* control verbose details */
    int                       mode_list_not_cmp;      /* list not comparable messages */
    int                       print_header;           /* print header */
    int                       print_percentage;       /* print percentage */
    int                       print_dims;             /* print dimension index */
    int                       delta_bool;             /* delta, absolute value to compare */
    double                    delta;                  /* delta value */
    int                       use_system_epsilon;     /* flag to use system epsilon (1 or 0) */
    int                       percent_bool;           /* relative error to compare*/
    double                    percent;                /* relative error value */
    bool                      follow_links;           /* follow symbolic links */
    int                       no_dangle_links;        /* return error when find dangling link */
    int                       cmn_objs;               /* do we have common objects */
    int                       not_cmp;                /* are the objects comparable */
    int                       contents;               /* equal contents */
    int                       do_nans;                /* consider Nans while diffing floats */
    int                       disable_compact_subset; /* disable compact form of subset notation */
    int                       exclude_path;           /* exclude path to an object */
    int                       exclude_attr_path;      /* exclude path to an object */
    struct exclude_path_list *exclude;                /* keep exclude path list */
    struct exclude_path_list *exclude_attr;           /* keep exclude attribute list */
    int                       count_bool;             /* count, compare up to count */
    hsize_t                   count;                  /* count value */
    diff_err_t                err_stat;  /* an error occurred (2, error, 1, differences, 0, no error) */
    hsize_t                   nelmts;    /* total number of elements */
    hsize_t                   hs_nelmts; /* number of elements to read at a time*/
    int                       rank;      /* dimensionality */
    size_t                    m_size;    /* m_size for diff */
    hid_t                     m_tid;     /* m_tid for diff */
    hsize_t                   dims[H5S_MAX_RANK];      /* dimensions of object */
    hsize_t                   p_min_idx[H5S_MAX_RANK]; /* min selected index */
    hsize_t                   p_max_idx[H5S_MAX_RANK]; /* max selected index */
    hsize_t                   acc[H5S_MAX_RANK];       /* accumulator position */
    hsize_t                   pos[H5S_MAX_RANK];       /* matrix position */
    hsize_t                   sm_pos[H5S_MAX_RANK];    /* stripmine position */
    char                     *obj_name[2];             /* name for object */
    struct subset_t          *sset[2];                 /* subsetting parameters */
    h5tools_vol_info_t        vol_info[2];             /* VOL information for input file, output file */
    h5tools_vfd_info_t        vfd_info[2];             /* VFD information for input file, output file */
    bool                      custom_vol[2];           /* Using a custom input, output VOL? */
    bool                      custom_vfd[2];           /* Using a custom input, output VFD? */
} diff_opt_t;

/*-------------------------------------------------------------------------
 * public functions
 *-------------------------------------------------------------------------
 */

#ifdef __cplusplus
extern "C" {
#endif

H5TOOLS_DLL hsize_t h5diff(const char *fname1, const char *fname2, const char *objname1, const char *objname2,
                           diff_opt_t *opts);

H5TOOLS_DLL hsize_t diff(hid_t file1_id, const char *path1, hid_t file2_id, const char *path2,
                         diff_opt_t *opts, diff_args_t *argdata);

#ifdef H5_HAVE_PARALLEL
H5TOOLS_DLL void phdiff_dismiss_workers(void);
H5TOOLS_DLL void print_manager_output(void);
#endif

#ifdef __cplusplus
}
#endif

/*-------------------------------------------------------------------------
 * private functions
 *-------------------------------------------------------------------------
 */

hsize_t diff_dataset(hid_t file1_id, hid_t file2_id, const char *obj1_name, const char *obj2_name,
                     diff_opt_t *opts);

hsize_t diff_datasetid(hid_t dset1_id, hid_t dset2_id, const char *obj1_name, const char *obj2_name,
                       diff_opt_t *opts);

hsize_t diff_match(hid_t file1_id, const char *grp1, trav_info_t *info1, hid_t file2_id, const char *grp2,
                   trav_info_t *info2, trav_table_t *table, diff_opt_t *opts);

hsize_t diff_array(void *_mem1, void *_mem2, diff_opt_t *opts, hid_t container1_id, hid_t container2_id);

int diff_can_type(hid_t f_type1, hid_t f_type2, int rank1, int rank2, hsize_t *dims1, hsize_t *dims2,
                  hsize_t *maxdim1, hsize_t *maxdim2, diff_opt_t *opts, int is_compound);

hsize_t diff_attr_data(hid_t attr1_id, hid_t attr2_id, const char *name1, const char *name2,
                       const char *path1, const char *path2, diff_opt_t *opts);

hsize_t diff_attr(hid_t loc1_id, hid_t loc2_id, const char *path1, const char *path2, diff_opt_t *opts);

/*-------------------------------------------------------------------------
 * utility functions
 *-------------------------------------------------------------------------
 */

/* in h5diff_util.c */
void        print_found(hsize_t nfound);
void        print_type(hid_t type);
const char *diff_basename(const char *name);
const char *get_type(h5trav_type_t type);
const char *get_class(H5T_class_t tclass);
const char *get_sign(H5T_sign_t sign);
void        print_dimensions(int rank, hsize_t *dims);
herr_t      match_up_memsize(hid_t f_tid1_id, hid_t f_tid2_id, hid_t *m_tid1, hid_t *m_tid2, size_t *m_size1,
                             size_t *m_size2);
/* in h5diff.c */
int  print_objname(diff_opt_t *opts, hsize_t nfound);
void do_print_objname(const char *OBJ, const char *path1, const char *path2, diff_opt_t *opts);
void do_print_attrname(const char *attr, const char *path1, const char *path2);

#endif /* H5DIFF_H */
