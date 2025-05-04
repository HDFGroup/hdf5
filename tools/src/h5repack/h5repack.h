/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef H5REPACK_H
#define H5REPACK_H

/** \page H5TOOL_RP_UG The HDF5 h5repack Tool
 *
 * Navigate back: \ref index "Main" / \ref UG / \ref CommandTools
 * <hr>
 *
 * \section sec_cltools_h5repack h5repack
 *
 * \subsection subsec_cltools_h5repack_intro Introduction
 *  With h5repack, you can write an HDF5 file to a new file and change the layout for objects in the new file.
 *
 * \subsection subsec_cltools_h5repack_usage Usage
 * <h4>h5repack [OPTIONS] file1 file2</h4>
 * \li <strong>file1</strong> Input HDF5 File
 * \li <strong>file2</strong> Output HDF5 File
 *
 * \subsection subsec_cltools_h5repack_error Error Report Option
 * \li <strong>--enable-error-stack</strong>    Prints messages from the HDF5 error stack as they occur.
 *             Optional value 2 also prints file open errors, --enable-error-stack=2.
 *
 * \subsection subsec_cltools_h5repack_options Options
 * \li <strong>--help</strong>           Print a usage message and exit
 * \li <strong>--verbose=N</strong>      Verbose mode, print object information.
 *        N - is an integer greater than 1, 2 displays read/write timing
 * \li <strong>--version</strong>        Print the library version number and exit
 * \li <strong>--native</strong>         Use a native HDF5 type when repacking
 * \li <strong>--page-buffer-size=N</strong>  Set the page buffer cache size, N=non-negative integers
 * \li <strong>--src-vol-value</strong>  Value (ID) of the VOL connector to use for opening the
 *                             input HDF5 file specified
 * \li <strong>--src-vol-name</strong>   Name of the VOL connector to use for opening the input
 *                             HDF5 file specified
 * \li <strong>--src-vol-info</strong>   VOL-specific info to pass to the VOL connector used for
 *                             opening the input HDF5 file specified
 * \li <strong>--dst-vol-value</strong>  Value (ID) of the VOL connector to use for opening the
 *                             output HDF5 file specified
 * \li <strong>--dst-vol-name</strong>   Name of the VOL connector to use for opening the output
 *                             HDF5 file specified
 * \li <strong>--dst-vol-info</strong>   VOL-specific info to pass to the VOL connector used for
 *                             opening the output HDF5 file specified
 * \li <strong>--src-vfd-value</strong>  Value (ID) of the VFL driver to use for opening the
 *                             input HDF5 file specified
 * \li <strong>--src-vfd-name</strong>   Name of the VFL driver to use for opening the input
 *                             HDF5 file specified
 * \li <strong>--src-vfd-info</strong>   VFD-specific info to pass to the VFL driver used for
 *                             opening the input HDF5 file specified
 * \li <strong>--dst-vfd-value</strong>  Value (ID) of the VFL driver to use for opening the
 *                             output HDF5 file specified
 * \li <strong>--dst-vfd-name</strong>   Name of the VFL driver to use for opening the output
 *                             HDF5 file specified
 * \li <strong>--dst-vfd-info</strong>   VFD-specific info to pass to the VFL driver used for
 *                             opening the output HDF5 file specified
 * \li <strong>--latest</strong>         Use latest version of file format
 *                             This option will take precedence over the options
 *                             --low and --high
 * \li <strong>--low=BOUND</strong>      The low bound for library release versions to use
 *                             when creating objects in the file
 *                             (default is #H5F_LIBVER_EARLIEST)
 * \li <strong>--high=BOUND</strong>     The high bound for library release versions to use
 *                             when creating objects in the file
 *                             (default is #H5F_LIBVER_LATEST)
 * \li <strong>--merge</strong>          Follow external soft link recursively and merge data
 * \li <strong>--prune</strong>          Do not follow external soft links and remove link
 * \li <strong>--merge --prune</strong>  Follow external link, merge data and remove dangling link
 * \li <strong>--compact=L1</strong>    Maximum number of links in header messages
 * \li <strong>--indexed=L2</strong>    Minimum number of links in the indexed format
 * \li <strong>--ssize=S[:F]</strong>   Shared object header message minimum size
 * \li <strong>--minimum=M</strong>     Do not apply the filter to datasets smaller than M
 * \li <strong>--file=E</strong>        Name of file E with the --file and --layout options
 * \li <strong>--ublock=U</strong>      Name of file U with user block data to be added
 * \li <strong>--block=B</strong>       Size of user block to be added
 * \li <strong>--metadata_block_size=A</strong>  Metadata block size for #H5Pset_meta_block_size
 * \li <strong>--threshold=T</strong>   Threshold value for #H5Pset_alignment
 * \li <strong>--alignment=A</strong>   Alignment value for #H5Pset_alignment
 * \li <strong>--sort_by=Q</strong>     Sort groups and attributes by index Q
 * \li <strong>--sort_order=Z</strong>  Sort groups and attributes by order Z
 * \li <strong>--filter=FILT</strong>   Filter type
 * \li <strong>--layout=LAYT</strong>   Layout type
 * \li <strong>--fs_strategy=FS_STRATEGY</strong>  File space management strategy for
 *                                    #H5Pset_file_space_strategy
 * \li <strong>--fs_persist=FS_PERSIST</strong>    Persisting or not
 *                                    persisting free-space for #H5Pset_file_space_strategy
 * \li <strong>--fs_threshold=FS_THRESHOLD</strong> : Free-space section
 *                                    threshold for #H5Pset_file_space_strategy
 * \li <strong>--fs_pagesize=FS_PAGESIZE</strong>  File space page size for #H5Pset_file_space_page_size
 *
 * \subsubsection subsubsec_cltools_h5repack_options_args Arguments to Certain Options
 * \li <strong>M</strong> - is an integer greater than 1, size of dataset in bytes (default is 0)
 * \li <strong>E</strong> - is a filename.
 * \li <strong>S</strong> - is an integer
 * \li <strong>U</strong> - is a filename.
 * \li <strong>T</strong> - is an integer
 * \li <strong>A</strong> - is an integer greater than zero
 * \li <strong>Q</strong> - is the sort index type for the input file. It can be "name" or
 *         "creation_order" (default)
 * \li <strong>Z</strong> - is the sort order type for the input file. It can be "descending" or
 *          "ascending" (default)
 * \li <strong>B</strong> - is the user block size, any value that is 512 or greater and is
 *          a power of 2 (1024 default)
 * \li <strong>F</strong> - is the shared object header message type, any of <dspace|dtype|fill|
 *          pline|attr>. If F is not specified, S applies to all messages
 *
 * \subsubsection subsubsec_cltools_h5repack_options_bound Library Version Bounds
 * <strong>BOUND</strong> is an integer indicating the library release versions to use when
 *            creating objects in the file (see #H5Pset_libver_bounds()):
 * \li <strong>0</strong> This is #H5F_LIBVER_EARLIEST in #H5F_libver_t struct
 * \li <strong>1</strong> This is #H5F_LIBVER_V18 in #H5F_libver_t struct
 * \li <strong>2</strong> This is #H5F_LIBVER_V110 in #H5F_libver_t struct
 * \li <strong>3</strong> This is #H5F_LIBVER_V112 in #H5F_libver_t struct
 * \li <strong>4</strong> This is #H5F_LIBVER_V114 in #H5F_libver_t struct
 * \li <strong>5</strong> This is #H5F_LIBVER_V200 in #H5F_libver_t struct
 * \li #H5F_LIBVER_LATEST is aliased to #H5F_LIBVER_V200 for this release
 *
 * \subsubsection subsubsec_cltools_h5repack_options_fs File Strategy Settings
 * <strong>FS_STRATEGY</strong> is a string indicating the file space strategy used:
 * \li <strong>FSM_AGGR</strong>
 *                 The mechanisms used in managing file space are free-space
 *                 managers, aggregators and virtual file driver.
 * \li <strong>PAGE</strong>
 *                 The mechanisms used in managing file space are free-space
 *                 managers with embedded paged aggregation and virtual file driver.
 * \li <strong>AGGR</strong>
 *                 The mechanisms used in managing file space are aggregators and
 *                 virtual file driver.
 * \li <strong>NONE</strong>
 *                 The mechanisms used in managing file space are virtual file
 *                 driver.
 * \li The default strategy when not set is \b FSM_AGGR without persisting free-space.
 *
 * \li <strong>FS_PERSIST</strong> is 1 for persisting free-space or 0 for not persisting free-space.
 *        The default when not set is not persisting free-space.
 *        The value is ignored for \b AGGR and \b NONE strategies.
 *
 * \li <strong>FS_THRESHOLD</strong> is the minimum size (in bytes) of free-space sections to be
 *        tracked by the library. The default when not set is 1.
 *        The value is ignored for \b AGGR and \b NONE strategies.
 *
 * \li <strong>FS_PAGESIZE</strong> is the size (in bytes) >=512 that is used by the library when
 *        the file space strategy \b PAGE is used.
 *        The default when not set is 4096.
 *
 * \subsubsection subsubsec_cltools_h5repack_options_filt Applying a Third-party Filter
 * <strong>FILT</strong> - is a string with the format:
 *
 * \li <strong>\<objects list\>:\<name of filter\>=\<filter parameters\></strong>
 *
 * \li  <strong>\<objects list\></strong> is a comma separated list of object names, meaning apply
 *          compression only to those objects. If no names are specified, the filter
 *          is applied to all objects
 * \li  <strong>\<name of filter\></strong> can be:
 *          <ul><li><strong>GZIP</strong> to apply the HDF5 GZIP filter (GZIP compression)</li>
 *          <li><strong>SZIP</strong> to apply the HDF5 SZIP filter (SZIP compression)</li>
 *          <li><strong>SHUF</strong> to apply the HDF5 shuffle filter</li>
 *          <li><strong>FLET</strong> to apply the HDF5 checksum filter</li>
 *          <li><strong>NBIT</strong> to apply the HDF5 NBIT filter (NBIT compression)</li>
 *          <li><strong>SOFF</strong> to apply the HDF5 Scale/Offset filter</li>
 *          <li><strong>UD</strong>   to apply a user defined filter</li>
 *          <li><strong>NONE</strong> to remove all filters</li></ul>
 * \li  <strong>\<filter parameters\></strong> is optional filter parameter information
 *          <ul><li><strong>GZIP=\<deflation level\></strong> from 1-9</li>
 *          <li><strong>SZIP=<pixels per block,coding></strong> pixels per block is a even number in
 *              2-32 and coding method is either EC or NN</li>
 *          <li><strong>SHUF</strong> (no parameter)</li>
 *          <li><strong>FLET</strong> (no parameter)</li>
 *          <li><strong>NBIT</strong> (no parameter)</li>
 *          <li><strong>SOFF=\<scale_factor,scale_type\></strong> scale_factor is an integer and scale_type
 *              is either IN or DS</li>
 *          <li><strong>UD=\<filter_number,filter_flag,cd_value_count,value1[,value2,...,valueN]\></strong>
 *              <ul><li><strong>Required values</strong> filter_number, filter_flag, cd_value_count,
 * value1</li> <li><strong>Optional values</strong> value2 to valueN</li> <li><strong>filter_flag</strong> 1
 * is OPTIONAL or 0 is MANDATORY</li></ul></li> <li><strong>NONE</strong> (no parameter)</li></ul>
 *
 * \subsubsection subsubsec_cltools_h5repack_options_lay Layout Settings
 * <strong>LAYT</strong> - is a string with the format:
 *
 * \li <strong>\<objects list\>:\<layout type\>=\<layout parameters\></strong>
 *
 * \li <strong>\<objects list\></strong> is a comma separated list of object names, meaning that
 *          layout information is supplied for those objects. If no names are
 *          specified, the layout type is applied to all objects
 * \li <strong>\<layout type\></strong> can be:
 *          <ul><li><strong>CHUNK</strong> to apply chunking layout</li>
 *          <li><strong>COMPA</strong> to apply compact layout</li>
 *          <li><strong>CONTI</strong> to apply contiguous layout</li></ul>
 * \li <strong>\<layout parameters\></strong> is optional layout information
 *          <ul><li><strong>CHUNK=DIM[xDIM...xDIM]</strong>, the chunk size of each dimension</li>
 *          <li><strong>COMPA</strong> (no parameter)</li>
 *          <li><strong>CONTI</strong> (no parameter)</li></ul>
 *
 * \subsubsection subsubsec_cltools_h5repack_options_note NOTE
 *      The environment variable <strong>H5TOOLS_BUFSIZE</strong> can be set to
 *      the number of MBs to change the default hyperslab buffer size from 32MB.
 *      \code setenv H5TOOLS_BUFSIZE=64 to double the hyperslab buffer. \endcode
 *
 * \subsection subsec_cltools_h5repack_examples Usage Examples
 *
 * \li 1) h5repack --verbose --filter=GZIP=1 file1 file2
 *
 *      GZIP compression with level 1 to all objects
 *
 * \li 2) h5repack --verbose --filter=dset1:SZIP=8,NN file1 file2
 *
 *      SZIP compression with 8 pixels per block and NN coding method to object dset1
 *
 * \li 3) h5repack --verbose --layout=dset1,dset2:CHUNK=20x10 --filter=dset3,dset4,dset5:NONE file1 file2
 *
 *      Chunked layout, with a layout size of 20x10, to objects dset1 and dset2
 *      and remove filters to objects dset3, dset4, dset5
 *
 * \li 4) h5repack --latest --compact=10 --ssize=20:dtype file1 file2
 *
 *      Using latest file format with maximum compact group size of 10 and
 *      minimum shared datatype size of 20
 *
 * \li 5) h5repack --filter=SHUF --filter=GZIP=1 file1 file2
 *
 *      Add both filters SHUF and GZIP in this order to all datasets
 *
 * \li 6) h5repack --filter=UD=307,0,1,9 file1 file2
 *
 *      Add bzip2 filter to all datasets
 *
 * \li 7) h5repack --low=0 --high=1 file1 file2
 *
 *      Set low=H5F_LIBVER_EARLIEST and high=H5F_LIBVER_V18 via
 *      H5Pset_libver_bounds() when creating the repacked file, file2
 *
 *
 * Previous Chapter \ref sec_cltools_h5ls - Next Chapter \ref sec_cltools_h5stat
 *
 * <hr>
 * Navigate back: \ref index "Main" / \ref UG / \ref CommandTools
 *
 */

#include "H5private.h"
#include "hdf5.h"
#include "h5trav.h"
#include "h5tools.h"
#include "h5tools_utils.h"

#define H5FOPENERROR      "unable to open file"
#define PFORMAT           "%-7s %-7s %-7s\n" /* chunk info, compression info, name*/
#define PFORMAT1          "%-7s %-7s %-7s"   /* chunk info, compression info, name*/
#define MAX_NC_NAME       256                /* max length of a name */
#define MAX_VAR_DIMS      32                 /* max per variable dimensions */
#define FORMAT_OBJ        " %-27s %s\n"      /* obj type, name */
#define FORMAT_OBJ_ATTR   "  %-27s %s\n"     /* obj type, name */
#define MAX_COMPACT_DSIZE 64512              /* max data size for compact layout. -1k for header size */

/* timing formats */
#define FORMAT_OBJ_TIME        " %-27s  %e/%e   %s\n"                      /* obj type, name */
#define FORMAT_OBJ_ATTR_TIME   "  %-27s  %e/%e   %s\n"                     /* obj type, name */
#define FORMAT_OBJ_NOTIME      " %-27s                              %s\n"  /* obj type, name */
#define FORMAT_OBJ_ATTR_NOTIME "  %-27s                              %s\n" /* obj type, name */

/* File space default information */
#define FS_PAGESIZE_DEF  4096
#define FS_STRATEGY_DEF  H5F_FSPACE_STRATEGY_FSM_AGGR
#define FS_PERSIST_DEF   false
#define FS_THRESHOLD_DEF 1

/*-------------------------------------------------------------------------
 * data structures for command line options
 *-------------------------------------------------------------------------
 */

/* a list of names */
typedef struct {
    char obj[MAX_NC_NAME];
} obj_list_t;

/*
 the type of filter and additional parameter
 type can be one of the filters
 H5Z_FILTER_NONE        0,  uncompress if compressed
 H5Z_FILTER_DEFLATE     1 , deflation like gzip
 H5Z_FILTER_SHUFFLE     2 , shuffle the data
 H5Z_FILTER_FLETCHER32  3 , letcher32 checksum of EDC
 H5Z_FILTER_SZIP        4 , szip compression
 H5Z_FILTER_NBIT        5 , nbit compression
 H5Z_FILTER_SCALEOFFSET 6 , scaleoffset compression
*/

typedef struct {
    H5Z_filter_t filtn;                       /* filter identification number */
    unsigned     filt_flag;                   /* filter definition flag */
    unsigned     cd_values[DEFAULT_CDELEMTS]; /* filter client data values */
    size_t       cd_nelmts;                   /* filter client number of values */
} filter_info_t;

/* chunk lengths along each dimension and rank */
typedef struct {
    hsize_t chunk_lengths[MAX_VAR_DIMS];
    int     rank;
} chunk_info_t;

/* we currently define a maximum value for the filters array,
   that corresponds to the current library filters */
#define H5_REPACK_MAX_NFILTERS 6

/* information for one object, contains PATH, CHUNK info and FILTER info */
typedef struct {
    char          path[MAX_NC_NAME];              /* name of object */
    filter_info_t filter[H5_REPACK_MAX_NFILTERS]; /* filter array */
    int           nfilters;                       /* current number of filters */
    H5D_layout_t  layout;                         /* layout information */
    chunk_info_t  chunk;                          /* chunk information */
    hid_t         refobj_id;                      /* object ID, references */
} pack_info_t;

/* store a table of all objects */
typedef struct {
    unsigned int size;
    unsigned int nelems;
    pack_info_t *objs;
} pack_opttbl_t;

/*-------------------------------------------------------------------------
 * command line options
 *-------------------------------------------------------------------------
 */

/* all the above, ready to go to the hrepack call */
typedef struct {
    pack_opttbl_t *op_tbl;                           /* table with all -c and -f options */
    int            all_layout;                       /* apply the layout to all objects */
    int            all_filter;                       /* apply the filter to all objects */
    filter_info_t  filter_g[H5_REPACK_MAX_NFILTERS]; /*global filter array for the ALL case */
    int            n_filter_g;                       /* number of global filters */
    chunk_info_t   chunk_g;                          /* global chunk INFO for the ALL case */
    H5D_layout_t   layout_g;                         /* global layout information for the ALL case */
    int            verbose;                          /* verbose mode */
    bool           merge;                            /* Merge external file. */
    bool           prune;                            /* Don't follow external file. */
    hsize_t        min_comp;                         /* minimum size to compress, in bytes */
    int            use_native;                       /* use a native type in write */
    bool           latest;                           /* pack file with the latest file format */
    H5F_libver_t   low_bound;                        /* The file's low bound as in H5Fset_libver_bounds() */
    H5F_libver_t   high_bound;                       /* The file's high bound as in H5Fset_libver_bounds() */
    hid_t          fin_fapl;                         /* FAPL to use for opening the input file */
    hid_t          fout_fapl;                        /* FAPL to use for opening/creating the output file */
    int            grp_compact; /* Set the maximum number of links to store as header messages in the group */
    int            grp_indexed; /* Set the minimum number of links to store in the indexed format */
    int            msg_size[8]; /* Minimum size of shared messages: dataspace,
                                   datatype, fill value, filter pipeline, attribute */
    const char           *ublock_filename; /* user block file name */
    hsize_t               ublock_size;     /* user block size */
    hsize_t               meta_block_size; /* metadata aggregation block size (for H5Pset_meta_block_size) */
    hsize_t               threshold;       /* alignment threshold for H5Pset_alignment */
    hsize_t               alignment;       /* alignment for H5Pset_alignment */
    H5F_fspace_strategy_t fs_strategy;     /* File space handling strategy */
    int                   fs_persist;      /* Free space section threshold */
    long                  fs_threshold;    /* Free space section threshold */
    long long             fs_pagesize;     /* File space page size */
    bool                  fin_vol;         /* Custom VOL for input file */
    bool                  fin_vfd;         /* Custom VFD for input file */
    bool                  fout_vol;        /* Custom VOL for output file */
    bool                  fout_vfd;        /* Custom VFD for output file */
} pack_opt_t;

typedef struct named_dt_t {
    H5O_token_t        obj_token; /* Object token for the named dtype in the in file */
    hid_t              id_out;    /* Open identifier for the dtype in the out file */
    struct named_dt_t *next;      /* Next dtype */
} named_dt_t;

/*-------------------------------------------------------------------------
 * public functions
 *-------------------------------------------------------------------------
 */

#ifdef __cplusplus
extern "C" {
#endif

int h5repack(const char *infile, const char *outfile, pack_opt_t *options);
int h5repack_addfilter(const char *str, pack_opt_t *options);
int h5repack_addlayout(const char *str, pack_opt_t *options);
int h5repack_init(pack_opt_t *options, int verbose, bool latest);
int h5repack_end(pack_opt_t *options);
int h5repack_verify(const char *in_fname, const char *out_fname, pack_opt_t *options);
int h5repack_cmp_pl(const char *fname1, const char *fname2, pack_opt_t *options);

/* Note: The below copy_named_datatype(), named_datatype_free(), copy_attr()
 * and struct named_dt_t were located in h5repack_copy.c as static prior to
 * bugfix1726.
 * Made shared functions as copy_attr() was needed in h5repack_refs.c.
 * However copy_attr() may be obsoleted when H5Acopy is available and put back
 * others to static in h5repack_copy.c.
 */
hid_t copy_named_datatype(hid_t type_in, hid_t fidout, named_dt_t **named_dt_head_p, trav_table_t *travt,
                          pack_opt_t *options);
int   named_datatype_free(named_dt_t **named_dt_head_p, int ignore_err);
int   copy_attr(hid_t loc_in, hid_t loc_out, named_dt_t **named_dt_head_p, trav_table_t *travt,
                pack_opt_t *options);

#ifdef __cplusplus
}
#endif

/*-------------------------------------------------------------------------
 * private functions
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * copy module
 *-------------------------------------------------------------------------
 */

int copy_objects(const char *fnamein, const char *fnameout, pack_opt_t *options);

int do_copy_refobjs(hid_t fidin, hid_t fidout, trav_table_t *travt, pack_opt_t *options);

/*-------------------------------------------------------------------------
 * filters and verify module
 *-------------------------------------------------------------------------
 */
void init_packobject(pack_info_t *obj);

/*-------------------------------------------------------------------------
 * filters and copy module
 *-------------------------------------------------------------------------
 */

int apply_filters(const char    *name,    /* object name from traverse list */
                  int            rank,    /* rank of dataset */
                  const hsize_t *dims,    /* dimensions of dataset */
                  size_t         msize,   /* size of type */
                  hid_t          dcpl_id, /* dataset creation property list */
                  pack_opt_t    *options, /* repack options */
                  int           *has_filter);       /* (OUT) object NAME has a filter */

/*-------------------------------------------------------------------------
 * options table
 *-------------------------------------------------------------------------
 */
int options_table_init(pack_opttbl_t **tbl);
int options_table_free(pack_opttbl_t *table);
int options_add_layout(obj_list_t *obj_list, unsigned n_objs, pack_info_t *pack, pack_opttbl_t *table);
int options_add_filter(obj_list_t *obj_list, unsigned n_objs, filter_info_t filt, pack_opttbl_t *table);
pack_info_t *options_get_object(const char *path, pack_opttbl_t *table);

/*-------------------------------------------------------------------------
 * parse functions
 *-------------------------------------------------------------------------
 */

obj_list_t *parse_filter(const char *str, unsigned *n_objs, filter_info_t *filt, pack_opt_t *options,
                         int *is_glb);

obj_list_t *parse_layout(const char *str, unsigned *n_objs, pack_info_t *pack, /* info about object */
                         pack_opt_t *options);

#endif /* H5REPACK_H */
