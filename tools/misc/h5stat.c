/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include <stdlib.h>
#include <string.h>
#include "H5private.h"		/* Generic Functions			*/
#include "h5tools.h"
#include "h5tools_utils.h"
#include "h5tools_ref.h"
#include "h5trav.h"
#include "hdf5.h"

/* Parameters to control statistics gathered */
#define SIZE_SMALL_GROUPS       10
#define SIZE_SMALL_DSETS        10

#define  H5_NFILTERS_IMPL        8     /* Number of currently implemented filters + one to
                                          accommodate for user-define filters + one
                                          to accomodate datasets whithout any filters */



/* Datatype statistics for datasets */
typedef struct dtype_info_t {
    hid_t tid;                          /* ID of datatype */
    unsigned long count;                /* Number of types found */
    unsigned long named;                /* Number of types that are named */
} dtype_info_t;

typedef struct ohdr_info_t {
    hsize_t total_size;                 /* Total size of object headers */
    hsize_t free_size;                  /* Total free space in object headers */
} ohdr_info_t;

/* Info to pass to the iteration functions */
typedef struct iter_t {
    const char *container;              /* Full name of the container object */
    unsigned long curr_depth;           /* Current depth of hierarchy */

    unsigned long uniq_groups;          /* Number of unique groups */
    unsigned long uniq_dsets;           /* Number of unique datasets */
    unsigned long uniq_types;           /* Number of unique named datatypes */
    unsigned long uniq_links;           /* Number of unique links */
    unsigned long uniq_others;          /* Number of other unique objects */

    unsigned long max_depth;            /* Maximum depth of hierarchy */
    unsigned long max_links;            /* Maximum # of links to an object */
    hsize_t max_fanout;                 /* Maximum fanout from a group */
    unsigned long num_small_groups[SIZE_SMALL_GROUPS];     /* Size of small groups tracked */
    unsigned group_nbins;               /* Number of bins for group counts */
    unsigned long *group_bins;          /* Pointer to array of bins for group counts */
    ohdr_info_t group_ohdr_info;        /* Object header information for groups */

    unsigned long max_dset_rank;        /* Maximum rank of dataset */
    unsigned long dset_rank_count[H5S_MAX_RANK];     /* Number of datasets of each rank */
    hsize_t max_dset_dims;              /* Maximum dimension size of dataset */
    unsigned long small_dset_dims[SIZE_SMALL_DSETS];    /* Size of dimensions of small datasets tracked */
    unsigned long dset_layouts[H5D_NLAYOUTS];           /* Type of storage for each dataset */
    unsigned long dset_comptype[H5_NFILTERS_IMPL]; /* Number of currently implemented filters */
    unsigned long dset_ntypes;          /* Number of diff. dataset datatypes found */
    dtype_info_t *dset_type_info;       /* Pointer to dataset datatype information found */
    unsigned dset_dim_nbins;            /* Number of bins for dataset dimensions */
    unsigned long *dset_dim_bins;       /* Pointer to array of bins for dataset dimensions */
    ohdr_info_t dset_ohdr_info;         /* Object header information for datasets */
    hsize_t dset_storage_size;          /* Size of raw data for datasets */
    unsigned long nexternal;            /* Number of external files for a dataset */
    int           local;                /* Flag to indicate iteration over the object*/
} iter_t;


/* Table containing object id and object name */
static struct {
    int  nalloc;                /* number of slots allocated */
    int  nobjs;                 /* number of objects */
    struct {
        haddr_t id;             /* object number */
        char *name;             /* full object name */
    } *obj;
} idtab_g;

const char *progname = "h5stat";
int               d_status = EXIT_SUCCESS;
static int        display_all = TRUE;
static int        display_file_metadata = FALSE;
static int        display_file = FALSE;
static int        display_group_metadata = FALSE;
static int        display_group = FALSE;
static int        display_dset_metadata = FALSE;
static int        display_dset = FALSE;
static int        display_dtype_metadata = FALSE;
static int        display_dtype = FALSE;
static int        display_object = FALSE;

/* a structure for handling the order command-line parameters come in */
struct handler_t {
    void (*func)(void *);
    int flag;
    char *obj;
};


static const char *s_opts ="FfhGgDdTO:V";
static struct long_options l_opts[] = {
    {"help", no_arg, 'h'},
    {"hel", no_arg, 'h'},
    {"file", no_arg, 'f'},
    {"fil", no_arg, 'f'},
    {"fi", no_arg, 'f'},
    {"filemetadata", no_arg, 'F'},
    {"filemetadat", no_arg, 'F'},
    {"filemetada", no_arg, 'F'},
    {"filemetad", no_arg, 'F'},
    {"filemeta", no_arg, 'F'},
    {"filemet", no_arg, 'F'},
    {"fileme", no_arg, 'F'},
    {"filem", no_arg, 'F'},
    {"group", no_arg, 'g'},
    {"grou", no_arg, 'g'},
    {"gro", no_arg, 'g'},
    {"gr", no_arg, 'g'},
    {"groupmetadata", no_arg, 'G'},
    {"groupmetadat", no_arg, 'G'},
    {"groupmetada", no_arg, 'G'},
    {"groupmetad", no_arg, 'G'},
    {"groupmeta", no_arg, 'G'},
    {"groupmet", no_arg, 'G'},
    {"groupme", no_arg, 'G'},
    {"groupm", no_arg, 'G'},
    {"dset", no_arg, 'd'},
    {"dse", no_arg, 'd'},
    {"ds", no_arg, 'd'},
    {"d", no_arg, 'd'},
    {"dsetmetadata", no_arg, 'D'},
    {"dsetmetadat", no_arg, 'D'},
    {"dsetmetada", no_arg, 'D'},
    {"dsetmetad", no_arg, 'D'},
    {"dsetmeta", no_arg, 'D'},
    {"dsetmet", no_arg, 'D'},
    {"dsetme", no_arg, 'D'},
    {"dsetm", no_arg, 'D'},
    {"dtypemetadata", no_arg, 'T'},
    {"dtypemetadat", no_arg, 'T'},
    {"dtypemetada", no_arg, 'T'},
    {"dtypemetad", no_arg, 'T'},
    {"dtypemeta", no_arg, 'T'},
    {"dtypemet", no_arg, 'T'},
    {"dtypeme", no_arg, 'T'},
    {"dtypem", no_arg, 'T'},
    {"dtype", no_arg, 'T'},
    { "object", require_arg, 'O' },
    { "objec", require_arg, 'O' },
    { "obje", require_arg, 'O' },
    { "obj", require_arg, 'O' },
    { "ob", require_arg, 'O' },
    { "version", no_arg, 'V' },
    { "versio", no_arg, 'V' },
    { "versi", no_arg, 'V' },
    { "vers", no_arg, 'V' },
    { "ver", no_arg, 'V' },
    { "ve", no_arg, 'V' },
    { NULL, 0, '\0' }
};

static void
leave(int ret)
{
   h5tools_close();
   exit(ret);
}


static void                                                                                                                    usage(const char *prog)
{
     fflush(stdout);
     fprintf(stdout, "\n");
     fprintf(stdout, "This tool is under development. For detailed information\n");
     fprintf(stdout, "please see the specification document at\n");
     fprintf(stdout, "http://hdf.ncsa.uiuc.edu/RFC/h5stat/h5stat-spec.pdf\n");
     fprintf(stdout, "\n");
     fprintf(stdout, "Please send your comments and questions to help@hdfgroup.org\n");
     fprintf(stdout, "\n");
     fprintf(stdout, "Usage: %s [OPTIONS] file\n", prog);
     fprintf(stdout, "\n");
     fprintf(stdout, "      OPTIONS\n");
     fprintf(stdout, "     -h, --help            Print a usage message and exit\n");
     fprintf(stdout, "     -V, --version         Print version number and exit\n");
     fprintf(stdout, "     -f, --file            Print file information\n");
     fprintf(stdout, "     -F, --filemetadata    Print file metadata\n");
     fprintf(stdout, "     -g, --group           Print group information\n");
     fprintf(stdout, "     -G, --groupmetadata   Print group metadata\n");
     fprintf(stdout, "     -d, --dset            Print dataset information\n");
     fprintf(stdout, "     -D, --dsetmetadata    Print dataset metadata\n");
     fprintf(stdout, "     -T, --dtypemetadata   Print datatype metadata\n");
     fprintf(stdout, "\n");
}


/*-------------------------------------------------------------------------
 * Function: ceil_log10
 *
 * Purpose: Compute the ceiling of log_10(x)
 *
 * Return: >0 on success, 0 on failure
 *
 * Programmer: Quincey Koziol
 *              Monday, August 22, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static unsigned
ceil_log10(unsigned long x)
{
    unsigned long pow10 = 1;
    unsigned ret = 0;

    while(x >= pow10) {
        pow10 *= 10;
        ret++;
    } /* end while */

    return(ret);
}


/*-------------------------------------------------------------------------
 * Function: sym_insert
 *
 * Purpose: Add a symbol to the table.
 *
 * Return: void
 *
 * Programmer: Robb Matzke
 *              Thursday, January 21, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
sym_insert(H5G_stat_t *sb, const char *name)
{
    haddr_t objno;              /* Compact form of object's location */
    int  n;

    /* Don't add it if the link count is 1 because such an object can only
     * have one name. */
    if (sb->nlink<2) return;

    /* Extend the table */
    if (idtab_g.nobjs>=idtab_g.nalloc) {
        idtab_g.nalloc = MAX(256, 2*idtab_g.nalloc);
        idtab_g.obj = realloc(idtab_g.obj,
            idtab_g.nalloc*sizeof(idtab_g.obj[0]));
    }

    /* Insert the entry */
    n = idtab_g.nobjs++;
    objno = (haddr_t)sb->objno[0] | ((haddr_t)sb->objno[1] << (8 * sizeof(long)));
    idtab_g.obj[n].id = objno;
    idtab_g.obj[n].name = strdup(name);
}


/*-------------------------------------------------------------------------
 * Function: sym_lookup
 *
 * Purpose: Find another name for the specified object.
 *
 * Return: Success: Ptr to another name.
 *
 *  Failure: NULL
 *
 * Programmer: Robb Matzke
 *              Thursday, January 21, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static char *
sym_lookup(H5G_stat_t *sb)
{
    haddr_t objno;              /* Compact form of object's location */
    int  n;

    if (sb->nlink<2)
        return NULL; /*only one name possible*/
    objno = (haddr_t)sb->objno[0] | ((haddr_t)sb->objno[1] << (8 * sizeof(long)));
    for (n=0; n<idtab_g.nobjs; n++) {
            if (idtab_g.obj[n].id==objno)
            return idtab_g.obj[n].name;
    }
    return NULL;
}


/*-------------------------------------------------------------------------
 * Function: fix_name
 *
 * Purpose: Returns a malloc'd buffer that contains the PATH and BASE
 *  names separated by a single slash. It also removes duplicate
 *  and trailing slashes.
 *
 * Return: Success: Ptr to fixed name from malloc()
 *
 *  Failure: NULL
 *
 * Programmer: Robb Matzke
 *              Thursday, January 21, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static char *
fix_name(const char *path, const char *base)
{
    size_t n = (path ? strlen(path) : 0) + (base ? strlen(base) : 0) + 3;
    char *s = malloc(n), prev='\0';
    size_t len = 0;

    if (path) {
        /* Path, followed by slash */
        for (/*void*/; *path; path++)
            if ('/'!=*path || '/'!=prev)
                prev = s[len++] = *path;
        if ('/' != prev)
            prev = s[len++] = '/';
    }

    if (base) {
        /* Base name w/o trailing slashes */
        const char *end = base + strlen(base);
        while (end > base && '/' == end[-1])
            --end;

        for (/*void*/; base < end; base++)
            if ('/' != *base || '/' != prev)
                prev = s[len++] = *base;
    }

    s[len] = '\0';
    return s;
}

/*-------------------------------------------------------------------------
 * Function: group_stats
 *
 * Purpose: Gather statistics about the group
 *
 * Return: Success: 0
 *
 *  Failure: -1
 *
 * Programmer: Quincey Koziol
 *              Tuesday, August 16, 2005
 *
 * Modifications: Refactored code from the walk_function 
 *                EIP, Wednesday, August 16, 2006 
 *
 *-------------------------------------------------------------------------
 */
static herr_t
group_stats (hid_t group, const char *name, const char * fullname, H5G_stat_t * _sb, H5G_iterate_t _walk, iter_t *_iter)


{
    hid_t gid;                      /* Group ID */
    const char *last_container;
    hsize_t num_objs;
    unsigned bin;                   /* "bin" the number of objects falls in */
    iter_t *iter = (iter_t*)_iter;
    H5G_stat_t *sb = _sb;
    H5G_iterate_t walk = _walk;
    herr_t ret;

    /* Gather statistics about this type of object */
    iter->uniq_groups++;
    if(iter->curr_depth > iter->max_depth)
    iter->max_depth = iter->curr_depth;

    /* Get object header information */
    iter->group_ohdr_info.total_size += sb->ohdr.size;
    iter->group_ohdr_info.free_size += sb->ohdr.free;

    gid = H5Gopen(group, name);
    assert(gid > 0);

    H5Gget_num_objs(gid, &num_objs);
    if(num_objs < SIZE_SMALL_GROUPS)
        (iter->num_small_groups[num_objs])++;
    if(num_objs > iter->max_fanout)
        iter->max_fanout = num_objs;

    /* Add group count to proper bin */
     bin = ceil_log10((unsigned long)num_objs);
     if((bin + 1) > iter->group_nbins) {
     /* Allocate more storage for info about dataset's datatype */
        iter->group_bins = realloc(iter->group_bins, (bin + 1) * sizeof(unsigned long));
        assert(iter->group_bins);

    /* Initialize counts for intermediate bins */
        while(iter->group_nbins < bin)
            iter->group_bins[iter->group_nbins++] = 0;
        iter->group_nbins++;

        /* Initialize count for new bin */
        iter->group_bins[bin] = 1;
     } /* end if */
     else {
         (iter->group_bins[bin])++;
     } /* end else */

     ret = H5Gclose(gid);
     assert(ret >= 0);

     last_container = iter->container;
     iter->container = fullname;
     iter->curr_depth++;

     H5Giterate(group, name, NULL, walk, iter);

     iter->container = last_container;
     iter->curr_depth--;
     
    return 0;
}

/*-------------------------------------------------------------------------
 * Function: dataset_stats
 *
 * Purpose: Gather statistics about the datset
 *
 * Return:  Success: 0
 *
 *          Failure: -1
 *
 * Programmer:    Quincey Koziol
 *                Tuesday, August 16, 2005
 *
 * Modifications: Refactored code from the walk_function 
 *                EIP, Wednesday, August 16, 2006 
 *
 *-------------------------------------------------------------------------
 */
static herr_t
dataset_stats (hid_t group, const char *name, H5G_stat_t * _sb,  iter_t *_iter)


{
    unsigned bin;                   /* "bin" the number of objects falls in */
    iter_t *iter = (iter_t*)_iter;
    H5G_stat_t *sb = _sb;
    herr_t ret;

    hid_t did;                      /* Dataset ID */
    hid_t sid;                      /* Dataspace ID */
    hid_t tid;                      /* Datatype ID */
    hid_t dcpl;                     /* Dataset creation property list ID */
    hsize_t dims[H5S_MAX_RANK];     /* Dimensions of dataset */
    H5D_layout_t lout;              /* Layout of dataset */
    unsigned type_found;            /* Whether the dataset's datatype was */
                                    /* already found */
    int ndims;                      /* Number of dimensions of dataset */
    hsize_t storage;                /* Size of dataset storage */
    unsigned u;                     /* Local index variable */
    int num_ext;                    /* Number of external files for a dataset */
    int nfltr;                      /* Number of filters for a dataset */
    H5Z_filter_t fltr;              /* Filter identifier */

    /* Gather statistics about this type of object */
    iter->uniq_dsets++;

    /* Get object header information */
    iter->dset_ohdr_info.total_size += sb->ohdr.size;
    iter->dset_ohdr_info.free_size += sb->ohdr.free;

    did = H5Dopen(group, name);
    assert(did > 0);

    /* Get storage info */
    storage = H5Dget_storage_size(did);
    iter->dset_storage_size += storage;

    /* Gather dataspace statistics */
    sid = H5Dget_space(did);
    assert(sid > 0);

    ndims = H5Sget_simple_extent_dims(sid, dims, NULL);
    assert(ndims >= 0);

    /* Check for larger rank of dataset */
    if((unsigned)ndims > iter->max_dset_rank)
        iter->max_dset_rank = ndims;

    /* Track the number of datasets with each rank */
    (iter->dset_rank_count[ndims])++;

    /* Only gather dim size statistics on 1-D datasets */
    if(ndims == 1) {

       if(dims[0] > iter->max_dset_dims)
           iter->max_dset_dims = dims[0];
       if(dims[0] < SIZE_SMALL_DSETS)
           (iter->small_dset_dims[dims[0]])++;

       /* Add dim count to proper bin */
       bin = ceil_log10((unsigned long)dims[0]);
       if((bin + 1) > iter->dset_dim_nbins) {
          /* Allocate more storage for info about dataset's datatype */
          iter->dset_dim_bins = realloc(iter->dset_dim_bins, (bin + 1) * sizeof(unsigned long));
          assert(iter->dset_dim_bins);

          /* Initialize counts for intermediate bins */
          while(iter->dset_dim_nbins < bin)
              iter->dset_dim_bins[iter->dset_dim_nbins++] = 0;
          iter->dset_dim_nbins++;

          /* Initialize count for this bin */
          iter->dset_dim_bins[bin] = 1;
        } /* end if */
        else {
            (iter->dset_dim_bins[bin])++;
        } /* end else */
    } /* end if */

    ret = H5Sclose(sid);
    assert(ret >= 0);

    /* Gather datatype statistics */
    tid = H5Dget_type(did);
    assert(tid > 0);

    type_found = FALSE;
    for(u = 0; u < iter->dset_ntypes; u++)
        if(H5Tequal(iter->dset_type_info[u].tid, tid) > 0) {
            type_found = TRUE;
            break;
        } /* end for */
    if(type_found) {
         (iter->dset_type_info[u].count)++;
    } /* end if */
    else {
        unsigned curr_ntype = iter->dset_ntypes;

        /* Increment # of datatypes seen for datasets */
        iter->dset_ntypes++;

        /* Allocate more storage for info about dataset's datatype */
        iter->dset_type_info = realloc(iter->dset_type_info, iter->dset_ntypes * sizeof(dtype_info_t));
        assert(iter->dset_type_info);

        /* Initialize information about datatype */
        iter->dset_type_info[curr_ntype].tid = H5Tcopy(tid);
        assert(iter->dset_type_info[curr_ntype].tid > 0);
        iter->dset_type_info[curr_ntype].count = 1;
        iter->dset_type_info[curr_ntype].named = 0;

        /* Set index for later */
        u = curr_ntype;
     } /* end else */

     /* Check if the datatype is a named datatype */
     if(H5Tcommitted(tid) > 0)
         (iter->dset_type_info[u].named)++;

     ret = H5Tclose(tid);
     assert(ret >= 0);

     /* Gather layout statistics */
     dcpl = H5Dget_create_plist(did);
     assert(dcpl > 0);

     lout = H5Pget_layout(dcpl);
     assert(lout >= 0);

     /* Track the layout type for dataset */
     (iter->dset_layouts[lout])++;

     num_ext = H5Pget_external_count(dcpl);
     assert (num_ext >= 0);

    if(num_ext) iter->nexternal = iter->nexternal + num_ext;

    /* Track different filters */

    if ((nfltr=H5Pget_nfilters(dcpl)) >= 0) {

       if (nfltr == 0) iter->dset_comptype[0]++;
          for (u=0; u < (unsigned) nfltr; u++) {
#ifdef H5_WANT_H5_V1_6_COMPAT
               fltr = H5Pget_filter(dcpl, u, 0, 0, 0, 0, 0);
#else /* H5_WANT_H5_V1_6_COMPAT */
               fltr = H5Pget_filter(dcpl, u, 0, 0, 0, 0, 0, NULL);
#endif /* H5_WANT_H5_V1_6_COMPAT */
               if (fltr < (H5_NFILTERS_IMPL-1))
                 iter->dset_comptype[fltr]++;
               else
                 iter->dset_comptype[H5_NFILTERS_IMPL-1]++; /*other filters*/
          }

        } /*endif nfltr */

     ret = H5Pclose(dcpl);
     assert(ret >= 0);

     ret = H5Dclose(did);
     assert(ret >= 0);

     return 0;
} 


/*-------------------------------------------------------------------------
 * Function: walk
 *
 * Purpose: Gather statistics about the file
 *
 * Return: Success: 0
 *
 *  Failure: -1
 *
 * Programmer: Quincey Koziol
 *              Tuesday, August 16, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
walk (hid_t group, const char *name, void *_iter)
{
    char *fullname = NULL;
    char *s;
    H5G_stat_t sb;
    iter_t *iter = (iter_t*)_iter;
    herr_t ret;                     /* Generic return value */

    /* Get the full object name */
    fullname = fix_name(iter->container, name);
/*
printf("walk: fullname = %s\n", fullname);
*/
    /* Get object information */
    ret = H5Gget_objinfo(group, name, FALSE, &sb);
    assert(ret >= 0);

    /* If the object has already been printed then just show the object ID
     * and return. */
    if ((s=sym_lookup(&sb))) {
        printf("same as %s", s);
    } else {
        sym_insert(&sb, fullname);

        /* Gather some statistics about the object */
        if(sb.nlink > iter->max_links)
            iter->max_links = sb.nlink;

        switch(sb.type) {
            case H5G_GROUP:
                group_stats(group, name, fullname, &sb, walk, iter);     
                break;

            case H5G_DATASET:
                dataset_stats(group, name, &sb, iter);
                break;

            case H5G_TYPE:
                /* Gather statistics about this type of object */
                iter->uniq_types++;
                break;

            case H5G_LINK:
                /* Gather statistics about links and UD links */
                iter->uniq_links++;
                break;

            default:
                /* Gather statistics about this type of object */
                iter->uniq_others++;
                break;
        } /* end switch */
    }

    if (fullname)
        free(fullname);

    return 0;
}


/*-------------------------------------------------------------------------
 * Function: parse_command_line
 *
 * Purpose: Parses command line and sets up global variable to control output
 *
 * Return: Success: 0
 *
 * Failure: -1
 *
 * Programmer: Elena Pourmal
 *             Saturday, August 12, 2006
 *
 * Modifications: 
 *
 *-------------------------------------------------------------------------
 */
static struct handler_t *
parse_command_line(int argc, const char *argv[])
{
    int                opt, i;
    struct handler_t   *hand;

    /* Allocate space to hold the command line info */
    hand = calloc((size_t)argc, sizeof(struct handler_t));


    /* parse command line options */
    while ((opt = get_option(argc, argv, s_opts, l_opts)) != EOF) {
        switch ((char)opt) {
        case 'F':
            display_all = FALSE;
            display_file_metadata = TRUE;
            break;
        case 'f':
            display_all = FALSE;
            display_file = TRUE;
            break;
        case 'G':
            display_all = FALSE;
            display_group_metadata = TRUE;
            break;
        case 'g':
            display_all = FALSE;
            display_group = TRUE;
            break;
        case 'T':
            display_all = FALSE;
            display_dtype_metadata = TRUE;
            break;
        case 'D':
            display_all = FALSE;
            display_dset_metadata = TRUE;
            break;
        case 'd':
            display_all = FALSE;
            display_dset = TRUE;
            break;
        case 'h':
            usage(progname);
            leave(EXIT_SUCCESS);
        case 'V':
            print_version(progname);
            leave(EXIT_SUCCESS);
            break;
        case 'O':
            display_object = TRUE;
            for (i = 0; i < argc; i++)
                if (!hand[i].obj) {
                    hand[i].obj = HDstrdup(opt_arg);
                    hand[i].flag = 1;
                    break;
                }
            break;
        default:
            usage(progname);
            leave(EXIT_FAILURE);
        }
    }

    /* check for file name to be processed */
    if (argc <= opt_ind) {
        error_msg(progname, "missing file name\n");
        usage(progname);
        leave(EXIT_FAILURE);
    }
    return hand;
}


/*-------------------------------------------------------------------------
 * Function: init_iter
 *
 * Purpose: Initialize iter structure
 *
 * Return: Success: 0
 *
 * Failure: Never fails
 *
 * Programmer: Elena Pourmal
 *             Saturday, August 12, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
iter_init(iter_t * _iter)
{
        iter_t *iter = (iter_t*)_iter;
        herr_t ret =0;                     /* Generic return value */
        
        unsigned u;                        /* Local index variable */
        
        /* Initilaize file' metadata information */
        iter->container = "/";
        iter->uniq_groups = 0;
        iter->uniq_dsets = 0;
        iter->uniq_types = 0;
        iter->uniq_links = 0;
        iter->uniq_others = 0;
        iter->curr_depth = 0;
        iter->max_depth = 0;
        iter->max_links = 0;
        iter->max_fanout = 0;
        for(u = 0; u < SIZE_SMALL_GROUPS; u++)
            iter->num_small_groups[u] = 0;

        /* Initilaize groups' metadata information */
        iter->group_nbins = 0;
        iter->group_bins = NULL;
        iter->group_ohdr_info.total_size = 0;
        iter->group_ohdr_info.free_size = 0;

        /* Initilaize datasets' metadata information */
        iter->max_dset_rank = 0;
        for(u = 0; u < H5S_MAX_RANK; u++)
            iter->dset_rank_count[u] = 0;
        iter->max_dset_dims = 0;
        for(u = 0; u < SIZE_SMALL_DSETS; u++)
            iter->small_dset_dims[u] = 0;
        for(u = 0; u < H5D_NLAYOUTS; u++)
            iter->dset_layouts[u] = 0;
        for(u = 0; u < H5_NFILTERS_IMPL; u++)
            iter->dset_comptype[u] = 0;
        iter->dset_ntypes = 0;
        iter->dset_type_info = NULL;
        iter->dset_dim_nbins = 0;
        iter->dset_dim_bins = NULL;
        iter->dset_ohdr_info.total_size = 0;
        iter->dset_ohdr_info.free_size = 0;
        iter->dset_storage_size = 0;
        iter->nexternal = 0; 
        iter->local = 0; 

        return ret;
}

/*-------------------------------------------------------------------------
 * Function: print_file_info
 *
 * Purpose: Prints information about file
 *
 * Return: Success: 0
 *
 * Failure: Never fails
 *
 * Programmer: Elena Pourmal
 *             Saturday, August 12, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
print_file_info(iter_t * _iter)
{
        iter_t *iter = (iter_t*)_iter;
        herr_t ret =0;                     /* Generic return value */

        printf("File information\n");
        printf("\t# of unique groups: %lu\n", iter->uniq_groups);
        printf("\t# of unique datasets: %lu\n", iter->uniq_dsets);
        printf("\t# of unique named dataypes: %lu\n", iter->uniq_types);
        printf("\t# of unique links: %lu\n", iter->uniq_links);
        printf("\t# of unique other: %lu\n", iter->uniq_others);
        printf("\tMax. # of links to object: %lu\n", iter->max_links);
        printf("\tMax. depth of hierarchy: %lu\n", iter->max_depth);
        HDfprintf(stdout, "\tMax. # of objects in group: %Hu\n", iter->max_fanout);

        return ret;
}
        

/*-------------------------------------------------------------------------
 * Function: print_file_metadata
 *
 * Purpose: Prints metadata information about file
 *
 * Return: Success: 0
 *
 * Failure: Never fails
 *
 * Programmer: Elena Pourmal
 *             Saturday, August 12, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
print_file_metadata(iter_t * _iter)
{
        iter_t *iter = (iter_t*)_iter;
        herr_t ret =0;                     /* Generic return value */

        printf("Object header size: (total/unused)\n");
        HDfprintf(stdout, "\tGroups: %Hu/%Hu\n", iter->group_ohdr_info.total_size, 
                                                 iter->group_ohdr_info.free_size);
        HDfprintf(stdout, "\tDatasets: %Hu/%Hu\n", iter->dset_ohdr_info.total_size, 
                                                   iter->dset_ohdr_info.free_size);

        return ret;
}

/*-------------------------------------------------------------------------
 * Function: print_group_info
 *
 * Purpose: Prints information about groups in the file
 *
 * Return: Success: 0
 *
 * Failure: Never fails
 *
 * Programmer: Elena Pourmal
 *             Saturday, August 12, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
print_group_info(iter_t * _iter)
{
        iter_t *iter = (iter_t*)_iter;
        herr_t ret =0;                     /* Generic return value */
        unsigned u;                        /* Local index variable */
        unsigned        long power;        /* Temporary "power" for bins */
        unsigned long   total;             /* Total count for various statistics */

        printf("Small groups:\n");
        total = 0;
        for(u = 0; u < SIZE_SMALL_GROUPS; u++) {
            if(iter->num_small_groups[u] > 0) {
                printf("\t# of groups of size %u: %lu\n", u, iter->num_small_groups[u]);
                total += iter->num_small_groups[u];
            } /* end if */
        } /* end for */
        printf("\tTotal # of small groups: %lu\n", total);

        printf("Group bins:\n");
        total = 0;
        if(iter->group_bins[0] > 0) {
           printf("\t# of groups of size 0: %lu\n", iter->group_bins[0]);
           total = iter->group_bins[0];
        } /* end if */
        power = 1;
        for(u = 1; u < iter->group_nbins; u++) {
            if(iter->group_bins[u] > 0) {
               printf("\t# of groups of size %lu - %lu: %lu\n", power, (power * 10) - 1, 
                        iter->group_bins[u]);
               total += iter->group_bins[u];
            } /* end if */
            power *= 10;
        } /* end for */
        printf("\tTotal # of groups: %lu\n", total);

        return ret;
}

/*-------------------------------------------------------------------------
 * Function: print_dataset_info
 *
 * Purpose: Prints information about datasets in the file
 *
 * Return: Success: 0
 *
 * Failure: Never fails
 *
 * Programmer: Elena Pourmal
 *             Saturday, August 12, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
print_dataset_info(iter_t * _iter)
{
        iter_t *iter = (iter_t*)_iter;
        herr_t ret =0;                     /* Generic return value */
        unsigned u;                        /* Local index variable */
        unsigned        long power;        /* Temporary "power" for bins */
        unsigned long   total;             /* Total count for various statistics */
        size_t          dtype_size;        /* Size of encoded datatype */

        if(iter->uniq_dsets > 0) {
            printf("Dataset dimension information:\n");
            printf("\tMax. rank of datasets: %lu\n", iter->max_dset_rank);
            printf("\tDataset ranks:\n");
            for(u = 0; u < H5S_MAX_RANK; u++)
                    if(iter->dset_rank_count[u] > 0)
                    printf("\t\t# of dataset with rank %u: %lu\n", u, iter->dset_rank_count[u]);

            printf("1-D Dataset information:\n");
            HDfprintf(stdout, "\tMax. dimension size of 1-D datasets: %Hu\n", iter->max_dset_dims);
            printf("\tSmall 1-D datasets:\n");
            total = 0;
            for(u = 0; u < SIZE_SMALL_DSETS; u++) {
                if(iter->small_dset_dims[u] > 0) {
                    printf("\t\t# of dataset dimensions of size %u: %lu\n", u, 
                             iter->small_dset_dims[u]);
                    total += iter->small_dset_dims[u];
                } /* end if */
            } /* end for */
            printf("\t\tTotal small datasets: %lu\n", total);

            /* Protect against no datasets in file */
            if(iter->dset_dim_nbins > 0) {
                printf("\t1-D Dataset dimension bins:\n");
                total = 0;
                if(iter->dset_dim_bins[0] > 0) {
                    printf("\t\t# of datasets of size 0: %lu\n", iter->dset_dim_bins[0]);
                    total = iter->dset_dim_bins[0];
                } /* end if */
                power = 1;
                for(u = 1; u < iter->dset_dim_nbins; u++) {
                    if(iter->dset_dim_bins[u] > 0) {
                        printf("\t\t# of datasets of size %lu - %lu: %lu\n", power, (power * 10) - 1, 
                                 iter->dset_dim_bins[u]);
                        total += iter->dset_dim_bins[u];
                    } /* end if */
                    power *= 10;
                } /* end for */
                printf("\t\tTotal # of datasets: %lu\n", total);
            } /* end if */

            printf("Dataset storage information:\n");
            HDfprintf(stdout, "\tTotal raw data size: %Hu\n", iter->dset_storage_size);

            printf("Dataset layout information:\n");
            for(u = 0; u < H5D_NLAYOUTS; u++)
            printf("\tDataset layout counts[%s]: %lu\n", (u == 0 ? "COMPACT" :
                    (u == 1 ? "CONTIG" : "CHUNKED")), iter->dset_layouts[u]);
            printf("\tNumber of external files : %lu\n", iter->nexternal);
            printf("Dataset filters information:\n");
                printf("\tNumber of datasets with \n");

                printf("\t                        NO filter: %lu\n", iter->dset_comptype[H5Z_FILTER_ERROR+1]);

                printf("\t                        GZIP filter: %lu\n", iter->dset_comptype[H5Z_FILTER_DEFLATE]);

                printf("\t                        SHUFFLE filter: %lu\n", iter->dset_comptype[H5Z_FILTER_SHUFFLE]);

                printf("\t                        FLETCHER32 filter: %lu\n", iter->dset_comptype[H5Z_FILTER_FLETCHER32]);

                printf("\t                        SZIP filter: %lu\n", iter->dset_comptype[H5Z_FILTER_SZIP]);

                printf("\t                        NBIT filter: %lu\n", iter->dset_comptype[H5Z_FILTER_NBIT]);

                printf("\t                        SCALEOFFSET filter: %lu\n", iter->dset_comptype[H5Z_FILTER_SCALEOFFSET]);

                printf("\t                        USER-DEFINED filter: %lu\n", iter->dset_comptype[H5_NFILTERS_IMPL-1]);
            if(display_dtype_metadata) {
               printf("Dataset datatype information:\n");
               printf("\t# of unique datatypes used by datasets: %lu\n", iter->dset_ntypes);
               total = 0;
               for(u = 0; u < iter->dset_ntypes; u++) {
                   H5Tencode(iter->dset_type_info[u].tid, NULL, &dtype_size);
                   printf("\tDataset datatype #%u:\n", u);
                   printf("\t\tCount (total/named) = (%lu/%lu)\n", iter->dset_type_info[u].count, iter->dset_type_info[u].named);
                   printf("\t\tSize (desc./elmt) = (%lu/%lu)\n", (unsigned long)dtype_size, 
                                 (unsigned long)H5Tget_size(iter->dset_type_info[u].tid));
                   H5Tclose(iter->dset_type_info[u].tid);
                   total += iter->dset_type_info[u].count;
               } /* end for */
               printf("\tTotal dataset datatype count: %lu\n", total);
            }
           } /* end if */

        return ret;
}

/*-------------------------------------------------------------------------
 * Function: print_file_statistics
 *
 * Purpose: Prints file statistics
 *
 * Return: Success: 0
 *
 * Failure: Never fails
 *
 * Programmer: Elena Pourmal
 *             Saturday, August 12, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
print_file_statistics(iter_t * _iter)
{
        iter_t *iter = (iter_t*)_iter;

        if(display_all) {
           display_file = TRUE;
           display_file_metadata = TRUE;
           display_group = TRUE;
           display_group_metadata = TRUE;
           display_dset = TRUE;
           display_dtype_metadata = TRUE;
        }


        if(display_file)          print_file_info(iter);
        if(display_file_metadata) print_file_metadata(iter);
        if(display_group)         print_group_info(iter);
        if(display_dset)          print_dataset_info(iter);

}

/*-------------------------------------------------------------------------
 * Function: print_object_statistics
 *
 * Purpose: Prints object statistics
 *
 * Return: Success: 0
 *
 * Failure: Never fails
 *
 * Programmer: Elena Pourmal
 *             Thursday, August 17, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
print_object_statistics(char *name, iter_t * _iter)
{
        iter_t *iter = (iter_t*)_iter;
        printf("Object name %s\n", name);
}

/*-------------------------------------------------------------------------
 * Function: print_statistics
 *
 * Purpose: Prints statistics 
 *
 * Return: Success: 0
 *
 * Failure: Never fails
 *
 * Programmer: Elena Pourmal
 *             Thursday, August 17, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
print_statistics(char *name, iter_t * _iter)
{
        iter_t *iter = (iter_t*)_iter;
        if(display_object) 
           print_object_statistics(name, iter);
        else
           print_file_statistics(iter);
}


int
main(int argc, const char *argv[])
{
    iter_t          iter;
    const char     *fname = NULL;
    hid_t           fid;
    struct handler_t   *hand;
    herr_t          status;       
    char            *root = "/";
    int             i;

    /* Disable error reporting */
    H5Eset_auto_stack(H5E_DEFAULT, NULL, NULL);

    /* Initialize h5tools lib */
    h5tools_init();
    hand = parse_command_line (argc, argv);
    if (!hand) {
        error_msg(progname, "unable to parse command line arguments \n");
        leave(EXIT_FAILURE);
    }


    fname = argv[opt_ind];
    hand[opt_ind].obj = root;
    hand[opt_ind].flag = 1;
    if (display_object) hand[opt_ind].flag = 0;

    printf("Filename: %s\n", fname);

    fid = H5Fopen(fname, H5F_ACC_RDONLY, H5P_DEFAULT);
    if (fid < 0) {
        error_msg(progname, "unable to open file \"%s\"\n", fname);
        leave(EXIT_FAILURE);
    }

    /* Initialize iter structure */
    status = iter_init(&iter);
    
    /* Walk the objects or all file */
    for (i = 0; i < argc; i++) { 
         if (hand[i].obj) {
              if(hand[i].flag) {
                   walk(fid, hand[i].obj, &iter);
                   print_statistics(hand[i].obj, &iter);
              }
         }
    
    }
    free(hand);
    H5Fclose(fid);
    if (fid < 0) {
        error_msg(progname, "unable to close file \"%s\"\n", fname);
        leave(EXIT_FAILURE);
    }

    leave(EXIT_SUCCESS);
}

