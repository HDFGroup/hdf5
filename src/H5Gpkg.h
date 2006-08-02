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

/*
 * Programmer: Robb Matzke <matzke@llnl.gov>
 *             Thursday, September 18, 1997
 *
 * Purpose:     This file contains declarations which are visible
 *              only within the H5G package. Source files outside the
 *              H5G package should include H5Gprivate.h instead.
 */
#ifndef H5G_PACKAGE
#error "Do not include this file outside the H5G package!"
#endif

#ifndef _H5Gpkg_H
#define _H5Gpkg_H

/* Get package's private header */
#include "H5Gprivate.h"

/* Other private headers needed by this file */
#include "H5ACprivate.h"	/* Metadata cache			*/
#include "H5Oprivate.h"		/* Object headers		  	*/
#include "H5SLprivate.h"	/* Skip lists				*/

#define H5G_SIZE_HINT   256    /* default root grp size hint         */

/* H5G_NLINKS is deprecated */
#define H5G_NLINKS H5L_NLINKS_DEF

/*
 * Various types of object header information can be cached in a symbol
 * table entry (it's normal home is the object header to which the entry
 * points).  This datatype determines what (if anything) is cached in the
 * symbol table entry.
 */
typedef enum H5G_type_t {
    H5G_CACHED_ERROR	= -1, 	/*force enum to be signed		     */
    H5G_NOTHING_CACHED  = 0,    /*nothing is cached, must be 0               */
    H5G_CACHED_STAB     = 1,    /*symbol table, `stab'                       */
    H5G_CACHED_SLINK	= 2, 	/*symbolic link				     */
    H5G_CACHED_ULINK	= 3, 	/*user-defined link		             */

    H5G_NCACHED         = 4     /*THIS MUST BE LAST                          */
} H5G_type_t;

/*
 * A symbol table entry caches these parameters from object header
 * messages...  The values are entered into the symbol table when an object
 * header is created (by hand) and are extracted from the symbol table with a
 * callback function registered in H5O_init_interface().  Be sure to update
 * H5G_ent_decode(), H5G_ent_encode(), and H5G_ent_debug() as well.
 */
typedef union H5G_cache_t {
    struct {
        haddr_t btree_addr;             /*file address of symbol table B-tree*/
        haddr_t heap_addr;              /*file address of stab name heap     */
    } stab;

    struct {
	size_t	lval_offset;		/*link value offset		     */
    } slink;

    struct {
        size_t  udata_size;             /*size of user data buffer            */
        size_t  udata_offset;           /*link's user data buffer             */
        H5L_link_t link_type;           /*link type ID                        */
    } ulink;
} H5G_cache_t;

/*
 * A symbol table entry.  The two important fields are `name_off' and
 * `header'.  The remaining fields are used for caching information that
 * also appears in the object header to which this symbol table entry
 * points.
 */
typedef struct H5G_entry_t {
    hbool_t     dirty;                  /*entry out-of-date?                 */
    H5G_type_t  type;                   /*type of information cached         */
    H5G_cache_t cache;                  /*cached data from object header     */
    size_t      name_off;               /*offset of name within name heap    */
    haddr_t     header;                 /*file address of object header      */
    H5F_t       *file;                  /*file to which this obj hdr belongs */
} H5G_entry_t;

/*
 * Shared information for all open group objects
 */
struct H5G_shared_t {
    int fo_count;                   /* open file object count */
    hbool_t mounted;                /* Group is mount point */
};

/*
 * A group handle passed around through layers of the library within and
 * above the H5G layer.
 */
struct H5G_t {
    H5G_shared_t *shared;               /* Shared file object data */
    H5O_loc_t oloc;                     /* Object location for group */
    H5G_name_t path;                    /* Group hierarchy path   */
};

/*
 * Common data exchange structure for symbol table nodes.  This structure is
 * passed through the B-link tree layer to the methods for the objects
 * to which the B-link tree points.
 */
typedef struct H5G_bt_ud_common_t {
    /* downward */
    const char  *name;                  /*points to temporary memory         */
    haddr_t     heap_addr;              /*symbol table heap address          */
} H5G_bt_ud_common_t;

/*
 * Data exchange structure for symbol table nodes.  This structure is
 * passed through the B-link tree layer to the methods for the objects
 * to which the B-link tree points for operations which require no
 * additional information.
 *
 * (Just an alias for the "common" info).
 */
typedef H5G_bt_ud_common_t H5G_bt_ud0_t;

/*
 * Data exchange structure for symbol table nodes.  This structure is
 * passed through the B-link tree layer to the insert method for entries.
 */
typedef struct H5G_bt_ud1_t {
    /* downward */
    H5G_bt_ud_common_t common;          /* Common info for B-tree user data (must be first) */
    const H5O_link_t *lnk;              /* Link to insert into table         */
} H5G_bt_ud1_t;

/*
 * Data exchange structure for symbol table nodes.  This structure is
 * passed through the B-link tree layer to the methods for the objects
 * to which the B-link tree points.
 */
typedef struct H5G_bt_ud2_t {
    /* downward */
    H5G_bt_ud_common_t common;          /* Common info for B-tree user data (must be first) */
    hbool_t adj_link;                   /* Whether to adjust the link count on objects */

    /* upward */
    H5G_obj_t    *obj_type;             /*member type to be returned                 */
} H5G_bt_ud2_t;

/* Typedef for B-tree 'find' operation */
typedef herr_t (*H5G_bt_find_op_t)(const H5G_entry_t *ent/*in*/, void *operator_data/*in,out*/);

/*
 * Data exchange structure for symbol table nodes.  This structure is
 * passed through the B-link tree layer to the 'find' method for entries.
 */
typedef struct H5G_bt_ud3_t {
    /* downward */
    H5G_bt_ud_common_t common;          /* Common info for B-tree user data (must be first) */
    H5G_bt_find_op_t op;                /* Operator to call when correct entry is found */
    void *op_data;                      /* Data to pass to operator */

    /* upward */
} H5G_bt_ud3_t;

/*
 * Data exchange structure to pass through the B-tree layer for the
 * H5B_iterate function.
 */
typedef struct H5G_bt_it_ud1_t {
    /* downward */
    hid_t	group_id;	/*group id to pass to iteration operator     */
    haddr_t     heap_addr;      /*symbol table heap address                  */
    H5G_iterate_t op;		/*iteration operator			     */
    void	*op_data;	/*user-defined operator data		     */
    int		skip;		/*initial entries to skip		     */

    /* upward */
    int		*final_ent;	/*final entry looked at                      */
} H5G_bt_it_ud1_t;

/*
 * Data exchange structure to pass through the B-tree layer for the
 * H5B_iterate function.
 */
typedef struct H5G_bt_it_ud2_t {
    /* downward */
    haddr_t     heap_addr;      /*symbol table heap address                  */
    hsize_t     idx;            /*index of group member to be queried        */
    hsize_t     num_objs;       /*the number of objects having been traversed*/

    /* upward */
    char         *name;         /*member name to be returned                 */
} H5G_bt_it_ud2_t;

/*
 * Data exchange structure to pass through the B-tree layer for the
 * H5B_iterate function.
 */
typedef struct H5G_bt_it_ud3_t {
    /* downward */
    hsize_t     idx;            /*index of group member to be queried        */
    hsize_t     num_objs;       /*the number of objects having been traversed*/

    /* upward */
    H5G_obj_t    type;          /*member type to be returned                 */
} H5G_bt_it_ud3_t;

/*
 * Data exchange structure to pass through the B-tree layer for the
 * H5B_iterate function.
 */
typedef struct H5G_bt_it_ud4_t {
    /* downward */
    haddr_t     heap_addr;      /*symbol table heap address                  */
    size_t      max_links;      /* Max. # of links array can store           */

    /* upward */
    H5O_link_t *lnk_table;      /* Pointer to array of links to fill in      */
    size_t      nlinks;         /* Links inserted into table                 */
} H5G_bt_it_ud4_t;

/* Data passed to B-tree iteration for copying copy symblol table content */
typedef struct H5G_bt_it_ud5_t {
    H5O_loc_t   *src_oloc;      /* source object location */
    haddr_t     src_heap_addr;  /* heap address of the source symbol table  */
    H5F_t       *dst_file;      /* File of destination group */
    H5O_stab_t  *dst_stab;      /* symbol table info for destination group */
    H5O_copy_t  *cpy_info;      /* Information for copy operation */
} H5G_bt_it_ud5_t;

/* Typedef for path traversal operations */
/* grp_loc is the location of the group in which the targeted object is located.
 * name is the last component of the object's name
 * lnk is the link between the group and the object
 * obj_loc is the target of the traversal (or NULL if the object doesn't exist)
 * operator_data is whatever udata was supplied when H5G_traverse was called
 * own_obj_loc should be set to TRUE if this callback takes ownership of obj_loc,
 *     and FALSE if obj_loc needs to be deleted.
 */
typedef herr_t (*H5G_traverse_t)(H5G_loc_t *grp_loc/*in*/, const char *name,
    const H5O_link_t *lnk/*in*/, H5G_loc_t *obj_loc/*out*/, void *operator_data/*in,out*/,
    hbool_t *own_obj_loc/*out*/);

/*
 * During name lookups (see H5G_traverse()) we sometimes want information about
 * a symbolic link or a mount point.  The normal operation is to follow the
 * symbolic link or mount point and return information about its target.
 */
#define H5G_TARGET_NORMAL	0x0000
#define H5G_TARGET_SLINK	0x0001
#define H5G_TARGET_MOUNT	0x0002
#define H5G_TARGET_UDLINK	0x0004
#define H5G_CRT_INTMD_GROUP	0x0008

/*
 * This is the class identifier to give to the B-tree functions.
 */
H5_DLLVAR H5B_class_t H5B_SNODE[1];

/* The cache subclass */
H5_DLLVAR const H5AC_class_t H5AC_SNODE[1];

/*
 * Utility functions
 */
H5_DLL char * H5G_normalize(const char *name);
H5_DLL H5G_t *H5G_rootof(H5F_t *f);
H5_DLL const char * H5G_component(const char *name, size_t *size_p);
H5_DLL herr_t H5G_traverse_term_interface(void);
H5_DLL herr_t H5G_traverse(const H5G_loc_t *loc, const char *name,
    unsigned target, H5G_traverse_t op, void *op_data, hid_t lapl_id,
    hid_t dxpl_id);

/*
 * Functions that understand symbol tables but not names.  The
 * functions that understand names are exported to the rest of
 * the library and appear in H5Gprivate.h.
 */
H5_DLL herr_t H5G_stab_create(H5O_loc_t *grp_oloc, H5O_stab_t *stab, hid_t dxpl_id);
H5_DLL herr_t H5G_stab_create_components(H5F_t *f, H5O_stab_t *stab, size_t size_hint, hid_t dxpl_id);
H5_DLL herr_t H5G_stab_insert(H5O_loc_t *grp_oloc, const char *name,
    H5O_link_t *obj_lnk, hid_t dxpl_id);
H5_DLL herr_t H5G_stab_insert_real(H5F_t *f, H5O_stab_t *stab, const char *name,
    H5O_link_t *obj_lnk, hid_t dxpl_id);
H5_DLL herr_t H5G_stab_delete(H5F_t *f, hid_t dxpl_id, const H5O_stab_t *stab, hbool_t adj_link);
H5_DLL herr_t H5G_stab_iterate(H5O_loc_t *oloc, hid_t gid, int skip,
    int *last_obj, H5G_iterate_t op, void *op_data, hid_t dxpl_id);
H5_DLL herr_t H5G_stab_count(struct H5O_loc_t *oloc, hsize_t *num_objs, hid_t dxpl_id);
H5_DLL ssize_t H5G_stab_get_name_by_idx(H5O_loc_t *oloc, hsize_t idx, char* name,
    size_t size, hid_t dxpl_id);
H5_DLL H5G_obj_t H5G_stab_get_type_by_idx(H5O_loc_t *oloc, hsize_t idx,
    hid_t dxpl_id);
H5_DLL herr_t H5G_stab_remove(H5O_loc_t *oloc, const char *name,
    H5G_obj_t *obj_type, hid_t dxpl_id);
H5_DLL herr_t H5G_stab_lookup(H5O_loc_t *grp_oloc, const char *name,
    H5O_link_t *lnk, hid_t dxpl_id);

/*
 * Functions that understand symbol table entries.
 */
H5_DLL herr_t H5G_ent_copy(H5G_entry_t *dst, const H5G_entry_t *src,
            H5_copy_depth_t depth);
H5_DLL herr_t H5G_ent_reset(H5G_entry_t *ent);
H5_DLL herr_t H5G_ent_decode_vec(H5F_t *f, const uint8_t **pp,
				  H5G_entry_t *ent, unsigned n);
H5_DLL herr_t H5G_ent_encode_vec(H5F_t *f, uint8_t **pp,
				  const H5G_entry_t *ent, unsigned n);
H5_DLL herr_t H5G_ent_modified(H5G_entry_t *ent, H5G_type_t cache_type);
H5_DLL herr_t H5G_ent_convert(H5F_t *f, haddr_t heap_addr, const char *name,
    const H5O_link_t *lnk, H5G_entry_t *ent, hid_t dxpl_id);
H5_DLL herr_t H5G_ent_debug(H5F_t *f, hid_t dxpl_id, const H5G_entry_t *ent, FILE * stream,
			     int indent, int fwidth, haddr_t heap);

struct H5HL_t; /* defined in H5HLprivate.h */

/* Functions that understand symbol table nodes */
H5_DLL herr_t H5G_node_init(H5F_t *f);
H5_DLL int H5G_node_iterate (H5F_t *f, hid_t dxpl_id, const void *_lt_key, haddr_t addr,
		     const void *_rt_key, void *_udata);
H5_DLL int H5G_node_sumup(H5F_t *f, hid_t dxpl_id, const void *_lt_key, haddr_t addr,
		     const void *_rt_key, void *_udata);
H5_DLL int H5G_node_name(H5F_t *f, hid_t dxpl_id, const void *_lt_key, haddr_t addr,
		     const void *_rt_key, void *_udata);
H5_DLL int H5G_node_type(H5F_t *f, hid_t dxpl_id, const void *_lt_key, haddr_t addr,
		     const void *_rt_key, void *_udata);
H5_DLL int H5G_node_copy(H5F_t *f, hid_t dxpl_id, const void *_lt_key, haddr_t addr,
		     const void *_rt_key, void *_udata);
#ifdef H5_GROUP_REVISION
H5_DLL int H5G_node_stab_convert(H5F_t *f, hid_t dxpl_id, const void *_lt_key, haddr_t addr,
		  const void *_rt_key, void *_udata);
#endif /* H5_GROUP_REVISION */

/* Functions that understand link messages */
/* forward reference for later use */
struct H5HL_t; /* defined in H5HLprivate.h */
H5_DLL herr_t H5G_link_convert(H5O_link_t *lnk, const H5G_entry_t *ent,
    const struct H5HL_t *_heap, const char *name);
H5_DLL herr_t H5G_link_insert(H5O_loc_t *grp_oloc, H5O_link_t *obj_lnk,
    hid_t dxpl_id);
H5_DLL ssize_t H5G_link_get_name_by_idx(H5O_loc_t *oloc, hsize_t idx, char* name,
    size_t size, hid_t dxpl_id);
H5_DLL H5G_obj_t H5G_link_get_type_by_idx(H5O_loc_t *oloc, hsize_t idx,
    hid_t dxpl_id);
H5_DLL herr_t H5G_link_remove(const H5O_loc_t *oloc, const char *name,
    H5G_obj_t *obj_type, hid_t dxpl_id);
H5_DLL herr_t H5G_link_iterate(H5O_loc_t *oloc, hid_t gid, int skip,
    int *last_obj, H5G_iterate_t op, void *op_data, hid_t dxpl_id);
H5_DLL herr_t H5G_link_lookup(H5O_loc_t *grp_oloc, const char *name,
    H5O_link_t *lnk, hid_t dxpl_id);

/* Functions that understand objects */
H5_DLL herr_t H5G_obj_create(H5F_t *f, hid_t dxpl_id,
#ifdef H5_GROUP_REVISION
    H5O_ginfo_t *ginfo,
#endif /* H5_GROUP_REVISION */
    H5O_loc_t *oloc/*out*/);
H5_DLL herr_t H5G_obj_insert(H5O_loc_t *grp_oloc, const char *name,
    H5O_link_t *obj_lnk, hbool_t inc_link, hid_t dxpl_id);
H5_DLL herr_t H5G_obj_lookup(H5O_loc_t *grp_oloc, const char *name,
    H5O_link_t *lnk, hid_t dxpl_id);
H5_DLL herr_t H5G_obj_iterate(hid_t loc_id, const char *name, int skip,
    int *last_obj, H5G_iterate_t op, void *op_data, hid_t dxpl_id);
H5_DLL herr_t H5G_obj_count(struct H5O_loc_t *oloc, hsize_t *num_objs, hid_t dxpl_id);
H5_DLL ssize_t H5G_obj_get_name_by_idx(H5O_loc_t *oloc, hsize_t idx,
    char* name, size_t size, hid_t dxpl_id);
H5_DLL H5G_obj_t H5G_obj_get_type_by_idx(H5O_loc_t *oloc, hsize_t idx,
    hid_t dxpl_id);
H5_DLL herr_t H5G_obj_remove(H5O_loc_t *oloc, const char *name,
    H5G_obj_t *obj_type, hid_t dxpl_id);
H5_DLL herr_t H5G_obj_find(H5G_loc_t *loc, const char *name,
    unsigned traverse_flags, H5O_link_t *lnk, H5O_loc_t *obj_oloc,
    hid_t lapl_id, hid_t dxpl_id);

/*
 * These functions operate on group hierarchy names.
 */
H5_DLL herr_t H5G_name_init(H5G_name_t *name, const char *path);
H5_DLL herr_t H5G_name_set(H5G_name_t *loc, H5G_name_t *obj, const char *name);

/*
 * These functions operate on group "locations"
 */
H5_DLL herr_t H5G_loc_copy(H5G_loc_t *dst, H5G_loc_t *src, H5_copy_depth_t depth);
H5_DLL herr_t H5G_loc_insert(H5G_loc_t *grp_loc, const char *name,
    H5G_loc_t *obj_loc, hbool_t inc_link, hid_t dxpl_id);
H5_DLL herr_t H5G_loc_exists(const H5G_loc_t *loc, const char *name, hid_t dxpl_id);
H5_DLL herr_t H5G_loc_remove(H5G_loc_t *grp_loc, const char *name,
    H5G_loc_t *obj_loc, hid_t dxpl_id);

/* Testing functions */
#ifdef H5G_TESTING
H5_DLL htri_t H5G_is_empty_test(hid_t gid);
H5_DLL htri_t H5G_has_links_test(hid_t gid, unsigned *nmsgs);
H5_DLL htri_t H5G_has_stab_test(hid_t gid);
H5_DLL herr_t H5G_lheap_size_test(hid_t gid, size_t *lheap_size);
H5_DLL herr_t H5G_user_path_test(hid_t obj_id, char *user_path, size_t *user_path_len, unsigned *user_path_hidden);
#endif /* H5G_TESTING */

#endif

