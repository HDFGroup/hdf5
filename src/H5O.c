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

/*-------------------------------------------------------------------------
 *
 * Created:		H5O.c
 *			Aug  5 1997
 *			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		Object header virtual functions.
 *
 *-------------------------------------------------------------------------
 */

#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */
#define H5O_PACKAGE		/*suppress error about including H5Opkg	  */


#include "H5private.h"		/* Generic Functions			*/
#include "H5Dprivate.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"             /* File access				*/
#include "H5FLprivate.h"	/* Free lists                           */
#include "H5HGprivate.h"        /* Global Heaps                         */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Lprivate.h"         /* Links			  	*/
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Opkg.h"             /* Object headers			*/

#ifdef H5_HAVE_GETTIMEOFDAY
#include <sys/time.h>
#endif /* H5_HAVE_GETTIMEOFDAY */

/* Local macros */

/* Load native information for a message, if it's not already present */
/* (Only works for messages with decode callback) */
#define LOAD_NATIVE(F, DXPL, MSG, ERR)                                        \
    if(NULL == (MSG)->native) {                                               \
        const H5O_msg_class_t	*decode_type;                                 \
                                                                              \
        /* Check for shared message */                                        \
        if ((MSG)->flags & H5O_FLAG_SHARED)                                   \
            decode_type = H5O_MSG_SHARED;                                     \
        else                                                                  \
            decode_type = (MSG)->type;                                        \
                                                                              \
        /* Decode the message */                                              \
        HDassert(decode_type->decode);                                        \
        if(NULL == ((MSG)->native = (decode_type->decode)((F), (DXPL), (MSG)->raw))) \
            HGOTO_ERROR(H5E_OHDR, H5E_CANTDECODE, ERR, "unable to decode message") \
    } /* end if */

/* Private typedefs */

/* User data for iteration while removing a message */
typedef struct {
    H5F_t      *f;              /* Pointer to file for insertion */
    hid_t dxpl_id;              /* DXPL during iteration */
    int sequence;               /* Sequence # to search for */
    unsigned nfailed;           /* # of failed message removals */
    H5O_operator_t op;          /* Callback routine for removal operations */
    void *op_data;              /* Callback data for removal operations */
    hbool_t adj_link;           /* Whether to adjust links when removing messages */
} H5O_iter_ud1_t;

/* Typedef for "internal library" iteration operations */
typedef herr_t (*H5O_lib_operator_t)(H5O_mesg_t *mesg/*in,out*/, unsigned idx,
    unsigned * oh_flags_ptr, void *operator_data/*in,out*/);

/* Some syntactic sugar to make the compiler happy with two different kinds of iterator callbacks */
typedef union {
    H5O_operator_t app_op;      /* Application callback for each message */
    H5O_lib_operator_t lib_op;  /* Library internal callback for each message */
} H5O_mesg_operator_t;

/*
 * This table contains a list of object types, descriptions, and the
 * functions that determine if some object is a particular type.  The table
 * is allocated dynamically.
 */
typedef struct H5O_typeinfo_t {
    H5G_obj_t 	type;			        /*one of the public H5G_* types	     */
    htri_t	(*isa)(H5O_t*);			/*function to determine type	     */
    char	*desc;			        /*description of object type	     */
} H5O_typeinfo_t;

/* Node in skip list to map addresses from one file to another during object header copy */
typedef struct H5O_addr_map_t {
    haddr_t     src_addr;               /* Address of object in source file */
    haddr_t     dst_addr;               /* Address of object in destination file */
    hbool_t     is_locked;              /* Indicate that the destination object is locked currently */
    hsize_t     inc_ref_count;          /* Number of deferred increments to reference count */
} H5O_addr_map_t;

/* Package variables */

/* Header message ID to class mapping */
const H5O_msg_class_t *const H5O_msg_class_g[] = {
    H5O_MSG_NULL,		/*0x0000 Null				*/
    H5O_MSG_SDSPACE,		/*0x0001 Simple Dimensionality		*/
#ifdef H5_GROUP_REVISION
    H5O_MSG_LINFO,		/*0x0002 Link information		*/
#else /* H5_GROUP_REVISION */
    NULL,
#endif /* H5_GROUP_REVISION */
    H5O_MSG_DTYPE,		/*0x0003 Data Type			*/
    H5O_MSG_FILL,       	/*0x0004 Old data storage -- fill value */
    H5O_MSG_FILL_NEW,		/*0x0005 New Data storage -- fill value */
#ifdef H5_GROUP_REVISION
    H5O_MSG_LINK,		/*0x0006 Link 				*/
#else /* H5_GROUP_REVISION */
    NULL,
#endif /* H5_GROUP_REVISION */
    H5O_MSG_EFL,		/*0x0007 Data storage -- external data files */
    H5O_MSG_LAYOUT,		/*0x0008 Data Layout			*/
#ifdef H5O_ENABLE_BOGUS
    H5O_MSG_BOGUS,		/*0x0009 "Bogus"			*/
#else /* H5O_ENABLE_BOGUS */
    NULL,			/*0x0009 "Bogus"			*/
#endif /* H5O_ENABLE_BOGUS */
#ifdef H5_GROUP_REVISION
    H5O_MSG_GINFO,		/*0x000A Group Information		*/
#else /* H5_GROUP_REVISION */
    NULL,
#endif /* H5_GROUP_REVISION */
    H5O_MSG_PLINE,		/*0x000B Data storage -- filter pipeline */
    H5O_MSG_ATTR,		/*0x000C Attribute list			*/
    H5O_MSG_NAME,		/*0x000D Object name			*/
    H5O_MSG_MTIME,		/*0x000E Object modification date and time */
    H5O_MSG_SHARED,		/*0x000F Shared header message		*/
    H5O_MSG_CONT,		/*0x0010 Object header continuation	*/
    H5O_MSG_STAB,		/*0x0011 Symbol table			*/
    H5O_MSG_MTIME_NEW,		/*0x0012 New Object modification date and time */
};

/* Header object ID to class mapping */
/*
 * Initialize the object class info table.  Begin with the most general types
 * and end with the most specific. For instance, any object that has a
 * datatype message is a datatype but only some of them are datasets.
 */
const H5O_obj_class_t *const H5O_obj_class_g[] = {
    H5O_OBJ_DATATYPE,		/* Datatype object (H5G_TYPE - 2) */
    H5O_OBJ_DATASET,		/* Dataset object (H5G_DATASET - 1) */
    H5O_OBJ_GROUP,		/* Group object (H5G_GROUP - 0) */
};

/* Declare a free list to manage the H5O_t struct */
H5FL_DEFINE(H5O_t);

/* Declare a free list to manage the H5O_mesg_t sequence information */
H5FL_SEQ_DEFINE(H5O_mesg_t);

/* Declare a free list to manage the H5O_chunk_t sequence information */
H5FL_SEQ_DEFINE(H5O_chunk_t);

/* Declare a free list to manage the chunk image information */
H5FL_BLK_DEFINE(chunk_image);

/* Library private variables */

/* Local variables */

/* Declare external the free list for time_t's */
H5FL_EXTERN(time_t);

/* Declare extern the free list for H5O_cont_t's */
H5FL_EXTERN(H5O_cont_t);

/* Declare a free list to manage the H5O_addr_map_t struct */
H5FL_DEFINE_STATIC(H5O_addr_map_t);

/* PRIVATE PROTOTYPES */
static hid_t H5O_open_by_loc(H5G_loc_t *obj_loc, hid_t dxpl_id);
static H5O_loc_t * H5O_get_oloc(hid_t id);
static herr_t H5O_new(H5F_t *f, hid_t dxpl_id, size_t size_hint,
    H5O_loc_t *loc/*out*/, haddr_t header);
static herr_t H5O_reset_real(const H5O_msg_class_t *type, void *native);
static void * H5O_copy_real(const H5O_msg_class_t *type, const void *mesg,
        void *dst);
static int H5O_count_real(H5O_loc_t *loc, const H5O_msg_class_t *type,
        hid_t dxpl_id);
#ifdef NOT_YET
static herr_t H5O_share(H5F_t *f, hid_t dxpl_id, const H5O_msg_class_t *type, const void *mesg,
			 H5HG_t *hobj/*out*/);
#endif /* NOT_YET */
static unsigned H5O_find_in_ohdr(H5F_t *f, hid_t dxpl_id, H5O_t *oh,
                                 const H5O_msg_class_t **type_p, int sequence);
static int H5O_modify_real(H5O_loc_t *loc, const H5O_msg_class_t *type,
    int overwrite, unsigned flags, unsigned update_flags, const void *mesg,
    hid_t dxpl_id);
static int H5O_append_real(H5F_t *f, hid_t dxpl_id, H5O_t *oh,
    const H5O_msg_class_t *type, unsigned flags, const void *mesg,
    unsigned * oh_flags_ptr);
static herr_t H5O_remove_real(const H5O_loc_t *loc, const H5O_msg_class_t *type,
    int sequence, H5O_operator_t op, void *op_data, hbool_t adj_link, hid_t dxpl_id);
static unsigned H5O_alloc(H5F_t *f, hid_t dxpl_id, H5O_t *oh,
    const H5O_msg_class_t *type, size_t size, hbool_t * oh_dirtied_ptr);
static herr_t H5O_alloc_msgs(H5O_t *oh, size_t min_alloc);
static htri_t H5O_move_msgs_forward(H5F_t *f, H5O_t *oh, hid_t dxpl_id);
static htri_t H5O_merge_null(H5F_t *f, H5O_t *oh);
static htri_t H5O_remove_empty_chunks(H5F_t *f, H5O_t *oh, hid_t dxpl_id);
static herr_t H5O_condense_header(H5F_t *f, H5O_t *oh, hid_t dxpl_id);
static htri_t H5O_alloc_extend_chunk(H5F_t *f, H5O_t *oh,
    unsigned chunkno, size_t size, unsigned * msg_idx);
static unsigned H5O_alloc_new_chunk(H5F_t *f, hid_t dxpl_id, H5O_t *oh,
    size_t size);
static herr_t H5O_delete_oh(H5F_t *f, hid_t dxpl_id, H5O_t *oh);
static herr_t H5O_delete_mesg(H5F_t *f, hid_t dxpl_id, H5O_mesg_t *mesg,
    hbool_t adj_link);
static unsigned H5O_new_mesg(H5F_t *f, H5O_t *oh, unsigned *flags,
    const H5O_msg_class_t *orig_type, const void *orig_mesg, H5O_shared_t *sh_mesg,
    const H5O_msg_class_t **new_type, const void **new_mesg, hid_t dxpl_id,
    unsigned * oh_flags_ptr);
static herr_t H5O_write_mesg(H5O_t *oh, unsigned idx, const H5O_msg_class_t *type,
    const void *mesg, unsigned flags, unsigned update_flags,
    unsigned * oh_flags_ptr);
static herr_t H5O_iterate_real(const H5O_loc_t *loc, const H5O_msg_class_t *type,
    H5AC_protect_t prot, hbool_t internal, H5O_mesg_operator_t op, void *op_data, hid_t dxpl_id);
static H5G_obj_t H5O_obj_type_real(H5O_t *oh);
static const H5O_obj_class_t *H5O_obj_class(H5O_t *oh);
static void * H5O_copy_mesg_file(const H5O_msg_class_t *type, H5F_t *file_src, void *mesg_src,
    H5F_t *file_dst, hid_t dxpl_id, H5O_copy_t *cpy_info, void *udata);
static herr_t H5O_copy_header_real(const H5O_loc_t *oloc_src, H5O_loc_t *oloc_dst /*out */,
    hid_t dxpl_id, H5O_copy_t *cpy_info);
static herr_t H5O_copy_free_addrmap_cb(void *item, void *key, void *op_data);
static herr_t H5O_copy_obj_by_ref(H5O_loc_t *src_oloc, hid_t dxpl_id,
    H5O_loc_t *dst_oloc, H5G_loc_t *dst_root_loc, H5O_copy_t *cpy_info);


/*-------------------------------------------------------------------------
 * Function:	H5Oopen
 *
 * Purpose:	Opens an object within an HDF5 file.
 *
 *              This function opens an object in the same way that H5Gopen,
 *              H5Topen, and H5Dopen do. However, H5Oopen doesn't require
 *              the type of object to be known beforehand. This can be
 *              useful in user-defined links, for instance, when only a
 *              path is known.
 *
 *              The opened object should be closed again with H5Oclose
 *              or H5Gclose, H5Tclose, or H5Dclose.
 * Return:	Success:	An open object identifier
 *		Failure:	Negative
 *
 * Programmer:	James Laird
 *		July 14 2006
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Oopen(hid_t loc_id, const char *name, hid_t lapl_id)
{
    H5G_loc_t	loc;
    H5G_loc_t   obj_loc;                /* Location used to open group */
    H5G_name_t  obj_path;            	/* Opened object group hier. path */
    H5O_loc_t   obj_oloc;            	/* Opened object object location */
    hbool_t     loc_found = FALSE;      /* Entry at 'name' found */
    hid_t       ret_value = FAIL;

    FUNC_ENTER_API(H5Oopen, FAIL)
    H5TRACE3("i","isi",loc_id,name,lapl_id);

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")

    /* Set up opened group location to fill in */
    obj_loc.oloc = &obj_oloc;
    obj_loc.path = &obj_path;
    H5G_loc_reset(&obj_loc);

    /* Find the object's location */
    if(H5G_loc_find(&loc, name, &obj_loc/*out*/, lapl_id, H5AC_dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "group not found")
    loc_found = TRUE;

    if((ret_value = H5O_open_by_loc(&obj_loc, H5AC_dxpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open object")

done:
    if(ret_value < 0) {
        if(loc_found) {
            if(H5G_loc_free(&obj_loc) < 0)
                HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't free location")
        }
    } /* end if */

    FUNC_LEAVE_API(ret_value)
} /* end H5Oopen() */


/*-------------------------------------------------------------------------
 * Function:	H5Oopen_by_addr
 *
 * Purpose:	Warning! This function is EXTREMELY DANGEROUS!
 *              Improper use can lead to FILE CORRUPTION, INACCESSIBLE DATA,
 *              and other VERY BAD THINGS!
 *
 *              This function opens an object using its address within the
 *              HDF5 file, similar to an HDF5 hard link. The open object
 *              is identical to an object opened with H5Oopen() and should
 *              be closed with H5Oclose() or a type-specific closing
 *              function (such as H5Gclose() ).
 *
 *              This function is very dangerous if called on an invalid
 *              address. For this reason, H5Oincr_refcount() should be
 *              used to prevent HDF5 from deleting any object that is
 *              referenced by address (e.g. by a user-defined link).
 *              H5Odecr_refcount() should be used when the object is
 *              no longer being referenced by address (e.g. when the UD link
 *              is deleted).
 *         
 *              The address of the HDF5 file on disk has no effect on
 *              H5Oopen_by_addr(), nor does the use of any unusual file
 *              drivers. The "address" is really the offset within the
 *              HDF5 file, and HDF5's file drivers will transparently
 *              map this to an address on disk for the filesystem.
 *
 * Return:	Success:	An open object identifier
 *		Failure:	Negative
 *
 * Programmer:	James Laird
 *		July 14 2006
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Oopen_by_addr(hid_t loc_id, haddr_t addr)
{
    H5G_loc_t	loc;
    H5G_loc_t   obj_loc;                /* Location used to open group */
    H5G_name_t  obj_path;            	/* Opened object group hier. path */
    H5O_loc_t   obj_oloc;            	/* Opened object object location */
    hbool_t     loc_found = FALSE;      /* Location at 'name' found */
    hid_t       ret_value = FAIL;

    FUNC_ENTER_API(H5Oopen_by_addr, FAIL)
    H5TRACE2("i","ia",loc_id,addr);

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(addr == 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no address supplied")

    /* Set up opened group location to fill in */
    obj_loc.oloc = &obj_oloc;
    obj_loc.path = &obj_path;
    H5G_loc_reset(&obj_loc);
    obj_loc.oloc->addr = addr;
    obj_loc.oloc->file = loc.oloc->file;
    H5G_name_reset(obj_loc.path);

    if((ret_value = H5O_open_by_loc(&obj_loc, H5AC_dxpl_id)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open object")

done:
    if(ret_value < 0) {
        if(loc_found) {
            if(H5G_loc_free(&obj_loc) < 0)
                HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't free location")
        }
    } /* end if */

    FUNC_LEAVE_API(ret_value)
} /* end H5Oopen_by_addr() */


/*-------------------------------------------------------------------------
 * Function:	H5Oclose
 *
 * Purpose:	Close an open file object.
 *
 *              This is the companion to H5Oopen. It is used to close any
 *              open object in an HDF5 file (but not IDs are that not file
 *              objects, such as property lists and dataspaces). It has
 *              the same effect as calling H5Gclose, H5Dclose, or H5Tclose.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	James Laird
 *		July 14 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Oclose(hid_t object_id)
{
    herr_t       ret_value = SUCCEED;

    FUNC_ENTER_API(H5Oclose, FAIL)
    H5TRACE1("e","i",object_id);

    /* Get the type of the object and open it in the correct way */
    switch(H5I_get_type(object_id))
    {
        case(H5I_GROUP):
        case(H5I_DATATYPE):
        case(H5I_DATASET):
            if(H5I_object(object_id) == NULL)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a valid object")
            if(H5I_dec_ref(object_id) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTRELEASE, FAIL, "unable to close object")
            break;

        default:
            HGOTO_ERROR(H5E_ARGS, H5E_CANTRELEASE, FAIL, "not a valid file object ID (dataset, group, or datatype)")
        break;
    } /* end switch */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Oclose() */


/*-------------------------------------------------------------------------
 * Function:	H5Oincr_refcount
 *
 * Purpose:	Warning! This function is EXTREMELY DANGEROUS!
 *              Improper use can lead to FILE CORRUPTION, INACCESSIBLE DATA,
 *              and other VERY BAD THINGS!
 *
 *              This function increments the "hard link" reference count
 *              for an object. It should be used a user-defined link
 *              that references an object by address is deleted. When the
 *              link is deleted, H5Odecr_refcount should be used.
 *
 * Return:	Success:	The object's new refcount
 *		Failure:	Negative
 *
 * Programmer:	James Laird
 *		July 14 2006
 *
 *-------------------------------------------------------------------------
 */
int
H5Oincr_refcount(hid_t object_id)
{
    H5O_loc_t  *oloc;
    int         ret_value;

    FUNC_ENTER_API(H5Oincr_refcount, FAIL)
    H5TRACE1("Is","i",object_id);

    /* Get the object's oloc so we can adjust its link count */
    if((oloc = H5O_get_oloc(object_id)) == NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADVALUE, FAIL, "unable to get object location from ID")

    ret_value = H5O_link(oloc, 1, H5AC_dxpl_id);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5O_incr_refcount() */


/*-------------------------------------------------------------------------
 * Function:	H5Odecr_refcount
 *
 * Purpose:	Warning! This function is EXTREMELY DANGEROUS!
 *              Improper use can lead to FILE CORRUPTION, INACCESSIBLE DATA,
 *              and other VERY BAD THINGS!
 *
 *              This function decrements the "hard link" reference count
 *              for an object. It should be used when user-defined links
 *              that reference an object by address are deleted, and only
 *              after H5Oincr_refcount has already been used.
 *
 * Return:	Success:	The object's new refcount
 *		Failure:	Negative
 *
 * Programmer:	James Laird
 *		July 14 2006
 *
 *-------------------------------------------------------------------------
 */
int
H5Odecr_refcount(hid_t object_id)
{
    H5O_loc_t  *oloc;
    int         ret_value;

    FUNC_ENTER_API(H5Odecr_refcount, FAIL)

    /* Get the object's oloc so we can adjust its link count */
    if((oloc = H5O_get_oloc(object_id)) == NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADVALUE, FAIL, "unable to get object location from ID")

    ret_value = H5O_link(oloc, -1, H5AC_dxpl_id);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Odecr_refcount() */


/*-------------------------------------------------------------------------
 * Function:	H5O_open_by_loc
 *
 * Purpose:	Opens an object and returns an ID given its group loction.
 *
 *              This functions simply invokes H5G_open, H5T_open, or
 *              H5D_open depending on the object type.
 *
 * Return:	Success:	Open object identifier
 *		Failure:	Negative
 *
 * Programmer:	James Laird
 *		July 25 2006
 *
 *-------------------------------------------------------------------------
 */
static hid_t
H5O_open_by_loc(H5G_loc_t *obj_loc, hid_t dxpl_id)
{
    H5G_t       *grp = NULL;
    H5D_t       *dset = NULL;
    H5T_t       *type = NULL;
    hid_t      ret_value;

    FUNC_ENTER_NOAPI(H5O_open_by_loc, FAIL)

    HDassert(obj_loc);

    /* Get the type of the object and open it in the correct way */
    switch(H5O_obj_type(obj_loc->oloc, dxpl_id))
    {
      case(H5G_GROUP):
        /* Open the group */
        if((grp = H5G_open(obj_loc, dxpl_id)) == NULL)
            HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open group")
        /* Register an atom for the group */
        if((ret_value = H5I_register(H5I_GROUP, grp)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register group")
      break;

      case(H5G_DATASET):
        /* Open the group */
        if((dset = H5D_open(obj_loc, dxpl_id)) == NULL)
            HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open dataset")
        /* Register an atom for the group */
        if((ret_value = H5I_register(H5I_DATASET, dset)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register dataset")
      break;

      case(H5G_TYPE):
        /* Open the group */
        if((type = H5T_open(obj_loc, dxpl_id)) == NULL)
            HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open datatype")
        /* Register an atom for the group */
        if((ret_value = H5I_register(H5I_DATATYPE, type)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register datatype")
      break;

      default:
        HGOTO_ERROR(H5E_SYM, H5E_BADTYPE, FAIL, "invalid object type")
    }
 
done:
    if(ret_value < 0) {
        if(grp != NULL)
            H5G_close(grp);
        else if(dset != NULL)
            H5D_close(dset);
        else if(type != NULL)
            H5T_close(type);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5O_get_oloc
 *
 * Purpose:	Gets the oloc for an object given its ID.
 *
 * Return:	Success:	Pointer to H5O_loc_t
 *		Failure:	NULL
 *
 * Programmer:	James Laird
 *		July 25 2006
 *
 *-------------------------------------------------------------------------
 */
static H5O_loc_t *
H5O_get_oloc(hid_t object_id)
{
    H5G_t       *grp = NULL;
    H5D_t       *dset = NULL;
    H5T_t       *type = NULL;
    H5O_loc_t   *ret_value;

    FUNC_ENTER_NOAPI(H5O_get_oloc, NULL)

    switch(H5I_get_type(object_id))
    {
      case(H5I_GROUP):
        if((grp = H5I_object(object_id)) == NULL)
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "couldn't get group from ID")
        if((ret_value = H5G_oloc(grp)) == NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "unable to get object location from group ID")
      break;

      case(H5I_DATASET):
        if((dset = H5I_object(object_id)) == NULL)
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "couldn't get dataset from ID")
        if((ret_value = H5D_oloc(dset)) == NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "unable to get object location from dataset ID")
      break;

      case(H5I_DATATYPE):
        if((type = H5I_object(object_id)) == NULL)
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "couldn't get type from ID")
        if((ret_value = H5T_oloc(type)) == NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "unable to get object location from datatype ID")
      break;

      default:
        HGOTO_ERROR(H5E_SYM, H5E_BADTYPE, NULL, "invalid object type")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5O_create
 *
 * Purpose:	Creates a new object header. Allocates space for it and
 *              then calls an initialization function. The object header
 *              is opened for write access and should eventually be
 *              closed by calling H5O_close().
 *
 * Return:	Success:	Non-negative, the ENT argument contains
 *				information about the object header,
 *				including its address.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  5 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_create(H5F_t *f, hid_t dxpl_id, size_t size_hint, H5O_loc_t *loc/*out*/)
{
    haddr_t	header;
    herr_t      ret_value = SUCCEED;    /* return value */

    FUNC_ENTER_NOAPI(H5O_create, FAIL)

    /* check args */
    HDassert(f);
    HDassert(loc);

    size_hint = H5O_ALIGN(MAX(H5O_MIN_SIZE, size_hint));

    /* allocate disk space for header and first chunk */
    if(HADDR_UNDEF == (header = H5MF_alloc(f, H5FD_MEM_OHDR, dxpl_id,
            (hsize_t)H5O_SIZEOF_HDR(f) + size_hint)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "file allocation failed for object header header")

    /* initialize the object header */
    if(H5O_new(f, dxpl_id, size_hint, loc, header) != SUCCEED)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to initialize object header")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_create() */


/*-------------------------------------------------------------------------
 * Function:	H5O_new
 *
 * Purpose:	Initialize a new object header, sets the link count to 0,
 *              and caches the header. The object header is opened for
 *              write access and should eventually be closed by calling
 *              H5O_close().
 *
 * Return:	Success:    SUCCEED, the ENT argument contains
 *                          information about the object header,
 *                          including its address.
 *		Failure:    FAIL
 *
 * Programmer:	Bill Wendling
 *		1, November 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_new(H5F_t *f, hid_t dxpl_id, size_t size_hint, H5O_loc_t *loc/*out*/, haddr_t header)
{
    H5O_t      *oh = NULL;
    haddr_t     tmp_addr;
    herr_t      ret_value = SUCCEED;    /* return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_new)

    /* check args */
    HDassert(f);
    HDassert(loc);

    size_hint = H5O_ALIGN(MAX(H5O_MIN_SIZE, size_hint));
    loc->file = f;
    loc->addr = header;

    /* allocate the object header and fill in header fields */
    if(NULL == (oh = H5FL_MALLOC(H5O_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    oh->version = H5O_VERSION;
    oh->nlink = 0;

    /* create the chunk list and initialize the first chunk */
    oh->nchunks = 1;
    oh->alloc_nchunks = H5O_NCHUNKS;

    if(NULL == (oh->chunk = H5FL_SEQ_MALLOC(H5O_chunk_t, (size_t)oh->alloc_nchunks)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    tmp_addr = loc->addr + (hsize_t)H5O_SIZEOF_HDR(f);
    oh->chunk[0].dirty = TRUE;
    oh->chunk[0].addr = tmp_addr;
    oh->chunk[0].size = size_hint;

    if(NULL == (oh->chunk[0].image = H5FL_BLK_CALLOC(chunk_image, size_hint)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    /* create the message list and initialize the first message */
    oh->nmesgs = 1;
    oh->alloc_nmesgs = H5O_NMESGS;

    if(NULL == (oh->mesg = H5FL_SEQ_CALLOC(H5O_mesg_t, (size_t)oh->alloc_nmesgs)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    oh->mesg[0].type = H5O_MSG_NULL;
    oh->mesg[0].dirty = TRUE;
    oh->mesg[0].native = NULL;
    oh->mesg[0].raw = oh->chunk[0].image + H5O_SIZEOF_MSGHDR(f);
    oh->mesg[0].raw_size = size_hint - H5O_SIZEOF_MSGHDR(f);
    oh->mesg[0].chunkno = 0;

    /* cache it */
    if(H5AC_set(f, dxpl_id, H5AC_OHDR, loc->addr, oh, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to cache object header")

    /* open it */
    if(H5O_open(loc) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTOPENOBJ, FAIL, "unable to open object header")

done:
    if(ret_value < 0 && oh) {
        if(H5O_dest(f, oh) < 0)
	    HDONE_ERROR(H5E_OHDR, H5E_CANTFREE, FAIL, "unable to destroy object header data")
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_new() */


/*-------------------------------------------------------------------------
 * Function:	H5O_open
 *
 * Purpose:	Opens an object header which is described by the symbol table
 *		entry OBJ_ENT.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Monday, January	 5, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_open(const H5O_loc_t *loc)
{
    FUNC_ENTER_NOAPI_NOFUNC(H5O_open)

    /* Check args */
    HDassert(loc);
    HDassert(loc->file);

#ifdef H5O_DEBUG
    if (H5DEBUG(O))
	HDfprintf(H5DEBUG(O), "> %a\n", loc->addr);
#endif

    /* Increment open-lock counters */
    loc->file->nopen_objs++;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_open() */


/*-------------------------------------------------------------------------
 * Function:	H5O_close
 *
 * Purpose:	Closes an object header that was previously open.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Monday, January	 5, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_close(H5O_loc_t *loc)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5O_close, FAIL)

    /* Check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(loc->file->nopen_objs > 0);

    /* Decrement open-lock counters */
    --loc->file->nopen_objs;

#ifdef H5O_DEBUG
    if (H5DEBUG(O)) {
	if(loc->file->file_id < 0 && 1 == loc->file->shared->nrefs) {
	    HDfprintf(H5DEBUG(O), "< %a auto %lu remaining\n",
		      loc->addr,
		      (unsigned long)(loc->file->nopen_objs));
	} else {
	    HDfprintf(H5DEBUG(O), "< %a\n", loc->addr);
	}
    } /* end if */
#endif

    /*
     * If the file open object count has reached the number of open mount points
     * (each of which has a group open in the file) attempt to close the file.
     */
    if(loc->file->nopen_objs == loc->file->mtab.nmounts) {
        /* Attempt to close down the file hierarchy */
        if(H5F_try_close(loc->file) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTCLOSEFILE, FAIL, "problem attempting file close")
    } /* end if */

    if(H5O_loc_free(loc) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTRELEASE, FAIL, "problem attempting to free location")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_close() */


/*-------------------------------------------------------------------------
 * Function:	H5O_reset
 *
 * Purpose:	Some message data structures have internal fields that
 *		need to be freed.  This function does that if appropriate
 *		but doesn't free NATIVE.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug 12 1997
 *
 * Modifications:
 *              Changed to use IDs for types, instead of type objects, then
 *              call "real" routine.
 *              Quincey Koziol
 *		Feb 14 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_reset(unsigned type_id, void *native)
{
    const H5O_msg_class_t *type;            /* Actual H5O class type for the ID */
    herr_t      ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(H5O_reset,FAIL);

    /* check args */
    assert(type_id < NELMTS(H5O_msg_class_g));
    type=H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    assert(type);

    /* Call the "real" reset routine */
    if((ret_value=H5O_reset_real(type, native)) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_READERROR, FAIL, "unable to reset object header");

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5O_reset() */


/*-------------------------------------------------------------------------
 * Function:	H5O_reset_real
 *
 * Purpose:	Some message data structures have internal fields that
 *		need to be freed.  This function does that if appropriate
 *		but doesn't free NATIVE.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug 12 1997
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_reset_real(const H5O_msg_class_t *type, void *native)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_reset_real);

    /* check args */
    assert(type);

    if (native) {
	if (type->reset) {
	    if ((type->reset) (native) < 0)
		HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "reset method failed");
	} else {
	    HDmemset(native, 0, type->native_size);
	}
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5O_reset_real() */


/*-------------------------------------------------------------------------
 * Function:	H5O_free
 *
 * Purpose:	Similar to H5O_reset() except it also frees the message
 *		pointer.
 *
 * Return:	Success:	NULL
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Thursday, May 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *
H5O_free (unsigned type_id, void *mesg)
{
    const H5O_msg_class_t *type;            /* Actual H5O class type for the ID */
    void * ret_value;                   /* Return value */

    FUNC_ENTER_NOAPI_NOFUNC(H5O_free)

    /* check args */
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);

    /* Call the "real" free routine */
    ret_value = H5O_free_real(type, mesg);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_free() */


/*-------------------------------------------------------------------------
 * Function:	H5O_free_mesg
 *
 * Purpose:	Call H5O_free_real() on a message.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, Sep  6, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_free_mesg(H5O_mesg_t *mesg)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_free_mesg)

    /* check args */
    HDassert(mesg);

    /* Free any native information */
    if(mesg->flags & H5O_FLAG_SHARED)
        mesg->native = H5O_free_real(H5O_MSG_SHARED, mesg->native);
    else
        mesg->native = H5O_free_real(mesg->type, mesg->native);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_free_mesg() */


/*-------------------------------------------------------------------------
 * Function:	H5O_free_real
 *
 * Purpose:	Similar to H5O_reset() except it also frees the message
 *		pointer.
 *
 * Return:	Success:	NULL
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Thursday, May 21, 1998
 *
 *-------------------------------------------------------------------------
 */
void *
H5O_free_real(const H5O_msg_class_t *type, void *msg_native)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_free_real)

    /* check args */
    HDassert(type);

    if(msg_native) {
        H5O_reset_real(type, msg_native);
        if (NULL!=(type->free))
            (type->free)(msg_native);
        else
            H5MM_xfree (msg_native);
    } /* end if */

    FUNC_LEAVE_NOAPI(NULL)
} /* end H5O_free_real() */


/*-------------------------------------------------------------------------
 * Function:	H5O_copy
 *
 * Purpose:	Copies a message.  If MESG is is the null pointer then a null
 *		pointer is returned with no error.
 *
 * Return:	Success:	Ptr to the new message
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Thursday, May 21, 1998
 *
 * Modifications:
 *              Changed to use IDs for types, instead of type objects, then
 *              call "real" routine.
 *              Quincey Koziol
 *		Feb 14 2003
 *
 *-------------------------------------------------------------------------
 */
void *
H5O_copy (unsigned type_id, const void *mesg, void *dst)
{
    const H5O_msg_class_t *type;            /* Actual H5O class type for the ID */
    void	*ret_value;             /* Return value */

    FUNC_ENTER_NOAPI(H5O_copy, NULL);

    /* check args */
    assert(type_id < NELMTS(H5O_msg_class_g));
    type=H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    assert(type);

    /* Call the "real" copy routine */
    if((ret_value=H5O_copy_real(type, mesg, dst))==NULL)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, NULL, "unable to copy object header message");

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5O_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5O_copy_real
 *
 * Purpose:	Copies a message.  If MESG is is the null pointer then a null
 *		pointer is returned with no error.
 *
 * Return:	Success:	Ptr to the new message
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Thursday, May 21, 1998
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_copy_real(const H5O_msg_class_t *type, const void *mesg, void *dst)
{
    void	*ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5O_copy_real)

    /* check args */
    HDassert(type);
    HDassert(type->copy);

    if(mesg)
	if(NULL == (ret_value = (type->copy)(mesg, dst, 0)))
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, NULL, "unable to copy object header message")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_copy_real() */



/*-------------------------------------------------------------------------
 * Function:	H5O_link
 *
 * Purpose:	Adjust the link count for an object header by adding
 *		ADJUST to the link count.
 *
 * Return:	Success:	New link count
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  5 1997
 *
 *-------------------------------------------------------------------------
 */
int
H5O_link(const H5O_loc_t *loc, int adjust, hid_t dxpl_id)
{
    H5O_t	*oh = NULL;
    unsigned oh_flags = H5AC__NO_FLAGS_SET; /* used to indicate whether the
                                            * object was deleted as a result
                                            * of this action.
                                            */
    int	ret_value = FAIL;

    FUNC_ENTER_NOAPI(H5O_link, FAIL)

    /* check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));
    if(adjust != 0 && 0 == (loc->file->intent & H5F_ACC_RDWR))
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "no write intent on file")

    /* get header */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr,
				   NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* adjust link count */
    if(adjust < 0) {
	if(oh->nlink + adjust < 0)
	    HGOTO_ERROR(H5E_OHDR, H5E_LINKCOUNT, FAIL, "link count would be negative")
	oh->nlink += adjust;
        oh_flags |= H5AC__DIRTIED_FLAG;

        /* Check if the object should be deleted */
        if(oh->nlink == 0) {
            /* Check if the object is still open by the user */
            if(H5FO_opened(loc->file, loc->addr) != NULL) {
                /* Flag the object to be deleted when it's closed */
                if(H5FO_mark(loc->file, loc->addr, TRUE) < 0)
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, FAIL, "can't mark object for deletion")
            } /* end if */
            else {
                /* Delete object right now */
                if(H5O_delete_oh(loc->file, dxpl_id, oh) < 0)
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, FAIL, "can't delete object from file")

                /* Mark the object header as deleted */
                oh_flags = H5C__DELETED_FLAG;
            } /* end else */
        } /* end if */
    } else if (adjust > 0) {
        /* A new object, or one that will be deleted */
        if(oh->nlink == 0) {
            /* Check if the object is current open, but marked for deletion */
            if(H5FO_marked(loc->file, loc->addr) > 0) {
                /* Remove "delete me" flag on the object */
                if(H5FO_mark(loc->file, loc->addr, FALSE) < 0)
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, FAIL, "can't mark object for deletion")
            } /* end if */
        } /* end if */

	oh->nlink += adjust;
        oh_flags |= H5AC__DIRTIED_FLAG;
    } /* end if */

    /* Set return value */
    ret_value = oh->nlink;

done:
    if (oh && H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, oh_flags) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_link() */


/*-------------------------------------------------------------------------
 * Function:	H5O_count
 *
 * Purpose:	Counts the number of messages in an object header which are a
 *		certain type.
 *
 * Return:	Success:	Number of messages of specified type.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Tuesday, April 21, 1998
 *
 *-------------------------------------------------------------------------
 */
int
H5O_count(H5O_loc_t *loc, unsigned type_id, hid_t dxpl_id)
{
    const H5O_msg_class_t *type;            /* Actual H5O class type for the ID */
    int	ret_value;                      /* Return value */

    FUNC_ENTER_NOAPI(H5O_count, FAIL)

    /* Check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);

    /* Call the "real" count routine */
    if((ret_value = H5O_count_real(loc, type, dxpl_id)) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTCOUNT, FAIL, "unable to count object header messages")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_count() */


/*-------------------------------------------------------------------------
 * Function:	H5O_count_real
 *
 * Purpose:	Counts the number of messages in an object header which are a
 *		certain type.
 *
 * Return:	Success:	Number of messages of specified type.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Tuesday, April 21, 1998
 *
 *-------------------------------------------------------------------------
 */
static int
H5O_count_real(H5O_loc_t *loc, const H5O_msg_class_t *type, hid_t dxpl_id)
{
    H5O_t	*oh = NULL;
    int	acc;
    unsigned	u;
    int	ret_value;

    FUNC_ENTER_NOAPI(H5O_count_real, FAIL)

    /* Check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));
    HDassert(type);

    /* Load the object header */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* Loop over all messages, counting the ones of the type looked for */
    for(u = acc = 0; u < oh->nmesgs; u++)
	if(oh->mesg[u].type == type)
            acc++;

    /* Set return value */
    ret_value = acc;

done:
    if (oh && H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, H5AC__NO_FLAGS_SET) != SUCCEED)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_count_real() */


/*-------------------------------------------------------------------------
 * Function:	H5O_exists
 *
 * Purpose:	Determines if a particular message exists in an object
 *		header without trying to decode the message.
 *
 * Return:	Success:	FALSE if the message does not exist; TRUE if
 *				th message exists.
 *
 *		Failure:	FAIL if the existence of the message could
 *				not be determined due to some error such as
 *				not being able to read the object header.
 *
 * Programmer:	Robb Matzke
 *              Monday, November  2, 1998
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5O_exists(H5O_loc_t *loc, unsigned type_id, int sequence, hid_t dxpl_id)
{
    H5O_t	*oh = NULL;             /* Object header for location */
    htri_t      ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(H5O_exists, FAIL)

    HDassert(loc);
    HDassert(loc->file);
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    HDassert(sequence >= 0);

    /* Load the object header */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* Call the "real" exists routine */
    if((ret_value = H5O_exists_oh(oh, type_id, sequence)) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_READERROR, FAIL, "unable to verify object header message")

done:
    if(oh && H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, H5AC__NO_FLAGS_SET) != SUCCEED)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_exists() */


/*-------------------------------------------------------------------------
 * Function:	H5O_exists_oh
 *
 * Purpose:	Determines if a particular message exists in an object
 *		header without trying to decode the message.
 *
 * Return:	Success:	FALSE if the message does not exist; TRUE if
 *				th message exists.
 *
 *		Failure:	FAIL if the existence of the message could
 *				not be determined due to some error such as
 *				not being able to read the object header.
 *
 * Programmer:	Robb Matzke
 *              Monday, November  2, 1998
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5O_exists_oh(H5O_t *oh, unsigned type_id, int sequence)
{
    const H5O_msg_class_t *type;    /* Actual H5O class type for the ID */
    unsigned	u;              /* Local index variable */
    htri_t      ret_value;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_exists_oh)

    HDassert(oh);
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);
    HDassert(sequence >= 0);

    /* Scan through the messages looking for the right one */
    for(u = 0; u < oh->nmesgs; u++) {
	if(type->id != oh->mesg[u].type->id)
            continue;
	if(--sequence < 0)
            break;
    } /* end for */

    /* Set return value */
    ret_value = (sequence < 0);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_exists_oh() */


/*-------------------------------------------------------------------------
 * Function:	H5O_read
 *
 * Purpose:	Reads a message from an object header and returns a pointer
 *		to it.	The caller will usually supply the memory through
 *		MESG and the return value will be MESG.	 But if MESG is
 *		the null pointer, then this function will malloc() memory
 *		to hold the result and return its pointer instead.
 *
 * Return:	Success:	Ptr to message in native format.  The message
 *				should be freed by calling H5O_reset().  If
 *				MESG is a null pointer then the caller should
 *				also call H5MM_xfree() on the return value
 *				after calling H5O_reset().
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  6 1997
 *
 *-------------------------------------------------------------------------
 */
void *
H5O_read(const H5O_loc_t *loc, unsigned type_id, int sequence, void *mesg, hid_t dxpl_id)
{
    const H5O_msg_class_t *type;            /* Actual H5O class type for the ID */
    void *ret_value;                    /* Return value */

    FUNC_ENTER_NOAPI(H5O_read, NULL)

    /* check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);
    HDassert(sequence >= 0);

    /* Call the "real" read routine */
    if((ret_value = H5O_read_real(loc, type, sequence, mesg, dxpl_id)) == NULL)
	HGOTO_ERROR(H5E_OHDR, H5E_READERROR, NULL, "unable to load object header")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_read() */


/*-------------------------------------------------------------------------
 * Function:	H5O_read_real
 *
 * Purpose:	Reads a message from an object header and returns a pointer
 *		to it.	The caller will usually supply the memory through
 *		MESG and the return value will be MESG.	 But if MESG is
 *		the null pointer, then this function will malloc() memory
 *		to hold the result and return its pointer instead.
 *
 * Return:	Success:	Ptr to message in native format.  The message
 *				should be freed by calling H5O_reset().  If
 *				MESG is a null pointer then the caller should
 *				also call H5MM_xfree() on the return value
 *				after calling H5O_reset().
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  6 1997
 *
 *-------------------------------------------------------------------------
 */
void *
H5O_read_real(const H5O_loc_t *loc, const H5O_msg_class_t *type, int sequence, void *mesg, hid_t dxpl_id)
{
    H5O_t          *oh = NULL;
    int             idx;
    void           *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5O_read_real)

    /* check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));
    HDassert(type);
    HDassert(sequence >= 0);

    /* copy the message to the user-supplied buffer */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "unable to load object header")

    /* can we get it from the object header? */
    if((idx = H5O_find_in_ohdr(loc->file, dxpl_id, oh, &type, sequence)) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, NULL, "unable to find message in object header")

    if(oh->mesg[idx].flags & H5O_FLAG_SHARED) {
	/*
	 * If the message is shared then then the native pointer points to an
	 * H5O_MSG_SHARED message.  We use that information to look up the real
	 * message in the global heap or some other object header.
	 */
	H5O_shared_t *shared;

	shared = (H5O_shared_t *)(oh->mesg[idx].native);
        ret_value = H5O_shared_read(loc->file, dxpl_id, shared, type, mesg);
    } else {
	/*
	 * The message is not shared, but rather exists in the object
	 * header.  The object header caches the native message (along with
	 * the raw message) so we must copy the native message before
	 * returning.
	 */
	if(NULL==(ret_value = (type->copy) (oh->mesg[idx].native, mesg, 0)))
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, NULL, "unable to copy message to user space")
    } /* end else */

done:
    if(oh && H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, H5AC__NO_FLAGS_SET) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, NULL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_read_real() */


/*-------------------------------------------------------------------------
 * Function:	H5O_find_in_ohdr
 *
 * Purpose:     Find a message in the object header without consulting
 *              a symbol table entry.
 *
 * Return:      Success:    Index number of message.
 *              Failure:    Negative
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Aug  6 1997
 *
 * Modifications:
 *      Robb Matzke, 1999-07-28
 *      The ADDR argument is passed by value.
 *
 *      Bill Wendling, 2003-09-30
 *      Modified so that the object header needs to be AC_protected
 *      before calling this function.
 *-------------------------------------------------------------------------
 */
static unsigned
H5O_find_in_ohdr(H5F_t *f, hid_t dxpl_id, H5O_t *oh, const H5O_msg_class_t **type_p, int sequence)
{
    unsigned		u;
    unsigned		ret_value;

    FUNC_ENTER_NOAPI_NOINIT(H5O_find_in_ohdr);

    /* Check args */
    assert(f);
    assert(oh);
    assert(type_p);

    /* Scan through the messages looking for the right one */
    for (u = 0; u < oh->nmesgs; u++) {
	if (*type_p && (*type_p)->id != oh->mesg[u].type->id)
            continue;
	if (--sequence < 0)
            break;
    }

    if (sequence >= 0)
	HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, UFAIL, "unable to find object header message");

    /*
     * Decode the message if necessary.  If the message is shared then decode
     * a shared message, ignoring the message type.
     */
    LOAD_NATIVE(f, dxpl_id, &(oh->mesg[u]), UFAIL)

    /*
     * Return the message type. If this is a shared message then return the
     * pointed-to type.
     */
    *type_p = oh->mesg[u].type;

    /* Set return value */
    ret_value=u;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_modify
 *
 * Purpose:	Modifies an existing message or creates a new message.
 *		The cache fields in that symbol table entry ENT are *not*
 *		updated, you must do that separately because they often
 *		depend on multiple object header messages.  Besides, we
 *		don't know which messages will be constant and which will
 *		not.
 *
 *		The OVERWRITE argument is either a sequence number of a
 *		message to overwrite (usually zero) or the constant
 *		H5O_NEW_MESG (-1) to indicate that a new message is to
 *		be created.  If the message to overwrite doesn't exist then
 *		it is created (but only if it can be inserted so its sequence
 *		number is OVERWRITE; that is, you can create a message with
 *		the sequence number 5 if there is no message with sequence
 *		number 4).
 *
 *              The UPDATE_TIME argument is a boolean that allows the caller
 *              to skip updating the modification time.  This is useful when
 *              several calls to H5O_modify will be made in a sequence.
 *
 * Return:	Success:	The sequence number of the message that
 *				was modified or created.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  6 1997
 *
 *-------------------------------------------------------------------------
 */
int
H5O_modify(H5O_loc_t *loc, unsigned type_id, int overwrite,
   unsigned flags, unsigned update_flags, const void *mesg, hid_t dxpl_id)
{
    const H5O_msg_class_t *type;            /* Actual H5O class type for the ID */
    int	ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(H5O_modify, FAIL)

    /* check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);
    HDassert(mesg);
    HDassert(0 == (flags & ~H5O_FLAG_BITS));

    /* Call the "real" modify routine */
    if((ret_value = H5O_modify_real(loc, type, overwrite, flags, update_flags, mesg, dxpl_id)) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "unable to write object header message")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_modify() */


/*-------------------------------------------------------------------------
 * Function:	H5O_modify_real
 *
 * Purpose:	Modifies an existing message or creates a new message.
 *		The cache fields in that symbol table entry ENT are *not*
 *		updated, you must do that separately because they often
 *		depend on multiple object header messages.  Besides, we
 *		don't know which messages will be constant and which will
 *		not.
 *
 *		The OVERWRITE argument is either a sequence number of a
 *		message to overwrite (usually zero) or the constant
 *		H5O_NEW_MESG (-1) to indicate that a new message is to
 *		be created.  If the message to overwrite doesn't exist then
 *		it is created (but only if it can be inserted so its sequence
 *		number is OVERWRITE; that is, you can create a message with
 *		the sequence number 5 if there is no message with sequence
 *		number 4).
 *
 *              The UPDATE_TIME argument is a boolean that allows the caller
 *              to skip updating the modification time.  This is useful when
 *              several calls to H5O_modify will be made in a sequence.
 *
 * Return:	Success:	The sequence number of the message that
 *				was modified or created.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  6 1997
 *
 *-------------------------------------------------------------------------
 */
static int
H5O_modify_real(H5O_loc_t *loc, const H5O_msg_class_t *type, int overwrite,
   unsigned flags, unsigned update_flags, const void *mesg, hid_t dxpl_id)
{
    H5O_t		*oh = NULL;
    unsigned		oh_flags = H5AC__NO_FLAGS_SET;
    int		        sequence;
    unsigned		idx;            /* Index of message to modify */
    H5O_mesg_t         *idx_msg;        /* Pointer to message to modify */
    H5O_shared_t	sh_mesg;
    int		        ret_value;

    FUNC_ENTER_NOAPI_NOINIT(H5O_modify_real)

    /* check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));
    HDassert(type);
    HDassert(mesg);
    HDassert(0 == (flags & ~H5O_FLAG_BITS));

    if(0 == (loc->file->intent & H5F_ACC_RDWR))
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "no write intent on file")

    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* Count similar messages */
    for(idx = 0, sequence = -1, idx_msg=&oh->mesg[0]; idx < oh->nmesgs; idx++, idx_msg++) {
	if(type->id != idx_msg->type->id)
            continue;
	if(++sequence == overwrite)
            break;
    } /* end for */

    /* Was the right message found? */
    if(overwrite >= 0 && (idx >= oh->nmesgs || sequence != overwrite)) {
	/* But can we insert a new one with this sequence number? */
	if(overwrite == sequence + 1)
	    overwrite = -1;
	else
	    HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, FAIL, "message not found")
    } /* end if */

    /* Check for creating new message */
    if(overwrite < 0) {
        /* Create a new message */
        if((idx = H5O_new_mesg(loc->file, oh, &flags, type, mesg, &sh_mesg, &type, &mesg, dxpl_id, &oh_flags)) == UFAIL)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to create new message")

        /* Set the correct sequence number for the message created */
	sequence++;

    } else if(oh->mesg[idx].flags & H5O_FLAG_CONSTANT) {
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "unable to modify constant message")
    } else if(oh->mesg[idx].flags & H5O_FLAG_SHARED) {
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "unable to modify shared (constant) message")
    }

    /* Write the information to the message */
    if(H5O_write_mesg(oh, idx, type, mesg, flags, update_flags, &oh_flags) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to write message")

    /* Update the modification time message if any */
    if(update_flags & H5O_UPDATE_TIME)
        H5O_touch_oh(loc->file, dxpl_id, oh, FALSE, &oh_flags);

    /* Set return value */
    ret_value = sequence;

done:
    if(oh && H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, oh_flags) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_modify_real() */


/*-------------------------------------------------------------------------
 * Function:	H5O_protect
 *
 * Purpose:	Wrapper around H5AC_protect for use during a H5O_protect->
 *              H5O_append->...->H5O_append->H5O_unprotect sequence of calls
 *              during an object's creation.
 *
 * Return:	Success:	Pointer to the object header structure for the
 *                              object.
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Dec 31 2002
 *
 *-------------------------------------------------------------------------
 */
H5O_t *
H5O_protect(H5O_loc_t *loc, hid_t dxpl_id)
{
    H5O_t	       *ret_value;      /* Return value */

    FUNC_ENTER_NOAPI(H5O_protect, NULL)

    /* check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));

    if(0 == (loc->file->intent & H5F_ACC_RDWR))
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, NULL, "no write intent on file")

    if(NULL == (ret_value = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "unable to load object header")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_protect() */


/*-------------------------------------------------------------------------
 * Function:	H5O_unprotect
 *
 * Purpose:	Wrapper around H5AC_unprotect for use during a H5O_protect->
 *              H5O_append->...->H5O_append->H5O_unprotect sequence of calls
 *              during an object's creation.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Dec 31 2002
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_unprotect(H5O_loc_t *loc, H5O_t *oh, hid_t dxpl_id, unsigned oh_flags)
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5O_unprotect, FAIL)

    /* check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));
    HDassert(oh);

    if(H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, oh_flags) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_unprotect() */


/*-------------------------------------------------------------------------
 * Function:	H5O_append
 *
 * Purpose:	Simplified version of H5O_modify, used when creating a new
 *              object header message (usually during object creation)
 *
 * Return:	Success:	The sequence number of the message that
 *				was created.
 *
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Dec 31 2002
 *
 *-------------------------------------------------------------------------
 */
int
H5O_append(H5F_t *f, hid_t dxpl_id, H5O_t *oh, unsigned type_id, unsigned flags,
    const void *mesg, unsigned * oh_flags_ptr)
{
    const H5O_msg_class_t *type;            /* Actual H5O class type for the ID */
    int	ret_value;                      /* Return value */

    FUNC_ENTER_NOAPI(H5O_append,FAIL);

    /* check args */
    assert(f);
    assert(oh);
    assert(type_id < NELMTS(H5O_msg_class_g));
    type=H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    assert(type);
    assert(0==(flags & ~H5O_FLAG_BITS));
    assert(mesg);
    assert(oh_flags_ptr);

    /* Call the "real" append routine */
    if((ret_value=H5O_append_real( f, dxpl_id, oh, type, flags, mesg, oh_flags_ptr)) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "unable to append to object header");

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5O_append() */


/*-------------------------------------------------------------------------
 * Function:	H5O_append_real
 *
 * Purpose:	Simplified version of H5O_modify, used when creating a new
 *              object header message (usually during object creation)
 *
 * Return:	Success:	The sequence number of the message that
 *				was created.
 *
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Dec 31 2002
 *
 *-------------------------------------------------------------------------
 */
static int
H5O_append_real(H5F_t *f, hid_t dxpl_id, H5O_t *oh, const H5O_msg_class_t *type,
    unsigned flags, const void *mesg, unsigned * oh_flags_ptr)
{
    unsigned		idx;            /* Index of message to modify */
    H5O_shared_t	sh_mesg;
    int		        ret_value = FAIL;

    FUNC_ENTER_NOAPI_NOINIT(H5O_append_real);

    /* check args */
    assert(f);
    assert(oh);
    assert(type);
    assert(0==(flags & ~H5O_FLAG_BITS));
    assert(mesg);
    assert(oh_flags_ptr);

    /* Create a new message */
    if((idx=H5O_new_mesg(f,oh,&flags,type,mesg,&sh_mesg,&type,&mesg,dxpl_id,oh_flags_ptr))==UFAIL)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to create new message");

    /* Write the information to the message */
    if(H5O_write_mesg(oh,idx,type,mesg,flags,0,oh_flags_ptr) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to write message");

    /* Set return value */
    ret_value = idx;

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5O_append_real () */


/*-------------------------------------------------------------------------
 * Function:	H5O_new_mesg
 *
 * Purpose:	Create a new message in an object header
 *
 * Return:	Success:	Index of message
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Friday, September  3, 2003
 *
 *-------------------------------------------------------------------------
 */
static unsigned
H5O_new_mesg(H5F_t *f, H5O_t *oh, unsigned *flags, const H5O_msg_class_t *orig_type,
    const void *orig_mesg, H5O_shared_t *sh_mesg, const H5O_msg_class_t **new_type,
    const void **new_mesg, hid_t dxpl_id, unsigned * oh_flags_ptr)
{
    size_t	size;                   /* Size of space allocated for object header */
    unsigned    ret_value=UFAIL;        /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_new_mesg);

    /* check args */
    assert(f);
    assert(oh);
    assert(flags);
    assert(orig_type);
    assert(orig_mesg);
    assert(sh_mesg);
    assert(new_mesg);
    assert(new_type);
    assert(oh_flags_ptr);

    /* Check for shared message */
    if (*flags & H5O_FLAG_SHARED) {
        HDmemset(sh_mesg,0,sizeof(H5O_shared_t));

        if (NULL==orig_type->get_share)
            HGOTO_ERROR(H5E_OHDR, H5E_UNSUPPORTED, UFAIL, "message class is not sharable");
        if ((orig_type->get_share)(f, orig_mesg, sh_mesg/*out*/) < 0) {
            /*
             * If the message isn't shared then turn off the shared bit
             * and treat it as an unshared message.
             */
            H5E_clear_stack (NULL);
            *flags &= ~H5O_FLAG_SHARED;
        } else {
            /* Change type & message to use shared information */
            *new_type=H5O_MSG_SHARED;
            *new_mesg=sh_mesg;
        } /* end else */
    } /* end if */
    else {
        *new_type=orig_type;
        *new_mesg=orig_mesg;
    } /* end else */

    /* Compute the size needed to store the message on disk */
    if ((size = ((*new_type)->raw_size) (f, *new_mesg)) >=H5O_MAX_SIZE)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, UFAIL, "object header message is too large");

    /* Allocate space in the object headed for the message */
    if ((ret_value = H5O_alloc(f, dxpl_id, oh, orig_type, size, oh_flags_ptr)) == UFAIL)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, UFAIL, "unable to allocate space for message");

    /* Increment any links in message */
    if((*new_type)->link && ((*new_type)->link)(f,dxpl_id,(*new_mesg)) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_LINKCOUNT, UFAIL, "unable to adjust shared object link count");

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5O_new_mesg() */


/*-------------------------------------------------------------------------
 * Function:	H5O_write_mesg
 *
 * Purpose:	Write message to object header
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Friday, September  3, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_write_mesg(H5O_t *oh, unsigned idx, const H5O_msg_class_t *type,
    const void *mesg, unsigned flags, unsigned update_flags,
    unsigned * oh_flags_ptr)
{
    H5O_mesg_t         *idx_msg;        /* Pointer to message to modify */
    herr_t      ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_write_mesg);

    /* check args */
    assert(oh);
    assert(type);
    assert(mesg);
    assert(oh_flags_ptr);

    /* Set pointer to the correct message */
    idx_msg=&oh->mesg[idx];

    /* Reset existing native information */
    if(!(update_flags&H5O_UPDATE_DATA_ONLY))
        H5O_reset_real(type, idx_msg->native);

    /* Copy the native value for the message */
    if (NULL == (idx_msg->native = (type->copy) (mesg, idx_msg->native, update_flags)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to copy message to object header");

    idx_msg->flags = flags;
    idx_msg->dirty = TRUE;
    *oh_flags_ptr |= H5AC__DIRTIED_FLAG;

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5O_write_mesg() */


/*-------------------------------------------------------------------------
 * Function:	H5O_touch_oh
 *
 * Purpose:	If FORCE is non-zero then create a modification time message
 *		unless one already exists.  Then update any existing
 *		modification time message with the current time.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Monday, July 27, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_touch_oh(H5F_t *f,
             hid_t dxpl_id,
	     H5O_t *oh,
	     hbool_t force,
             unsigned * oh_flags_ptr)
{
    unsigned	idx;
    time_t	now;
    size_t	size;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_touch_oh);

    assert(oh);
    assert(oh_flags_ptr);

    /* Look for existing message */
    for (idx=0; idx < oh->nmesgs; idx++) {
	if (H5O_MSG_MTIME==oh->mesg[idx].type || H5O_MSG_MTIME_NEW==oh->mesg[idx].type)
            break;
    }

#ifdef H5_HAVE_GETTIMEOFDAY
    {
        struct timeval now_tv;

        HDgettimeofday(&now_tv, NULL);
        now = now_tv.tv_sec;
    }
#else /* H5_HAVE_GETTIMEOFDAY */
    now = HDtime(NULL);
#endif /* H5_HAVE_GETTIMEOFDAY */

    /* Create a new message */
    if (idx==oh->nmesgs) {
	if (!force)
            HGOTO_DONE(SUCCEED); /*nothing to do*/
	size = (H5O_MSG_MTIME_NEW->raw_size)(f, &now);

	if ((idx=H5O_alloc(f, dxpl_id, oh, H5O_MSG_MTIME_NEW, size, oh_flags_ptr))==UFAIL)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to allocate space for modification time message");
    }

    /* Update the native part */
    if (NULL==oh->mesg[idx].native) {
	if (NULL==(oh->mesg[idx].native = H5FL_MALLOC(time_t)))
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "memory allocation failed for modification time message");
    }
    *((time_t*)(oh->mesg[idx].native)) = now;
    oh->mesg[idx].dirty = TRUE;
    *oh_flags_ptr |= H5AC__DIRTIED_FLAG;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_touch
 *
 * Purpose:	Touch an object by setting the modification time to the
 *		current time and marking the object as dirty.  Unless FORCE
 *		is non-zero, nothing happens if there is no MTIME message in
 *		the object header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Monday, July 27, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_touch(H5O_loc_t *loc, hbool_t force, hid_t dxpl_id)
{
    H5O_t	*oh = NULL;
    unsigned 	oh_flags = H5AC__NO_FLAGS_SET;
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5O_touch, FAIL)

    /* check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));
    if(0 == (loc->file->intent & H5F_ACC_RDWR))
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "no write intent on file")

    /* Get the object header */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* Create/Update the modification time message */
    if(H5O_touch_oh(loc->file, dxpl_id, oh, force, &oh_flags) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to update object modificaton time")

done:
    if(oh && H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, oh_flags) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_touch() */

#ifdef H5O_ENABLE_BOGUS

/*-------------------------------------------------------------------------
 * Function:	H5O_bogus_oh
 *
 * Purpose:	Create a "bogus" message unless one already exists.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              <koziol@ncsa.uiuc.edu>
 *              Tuesday, January 21, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_bogus_oh(H5F_t *f, hid_t dxpl_id, H5O_t *oh, hbool_t * oh_flags_ptr)
{
    int	idx;
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER(H5O_bogus_oh, FAIL)

    HDassert(f);
    HDassert(oh);
    HDassert(oh_flags_ptr);

    /* Look for existing message */
    for(idx = 0; idx < oh->nmesgs; idx++)
	if(H5O_MSG_BOGUS == oh->mesg[idx].type)
            break;

    /* Create a new message */
    if(idx == oh->nmesgs) {
        size_t	size = (H5O_MSG_BOGUS->raw_size)(f, NULL);

	if((idx = H5O_alloc(f, dxpl_id, oh, H5O_MSG_BOGUS, size, oh_flags_ptr)) < 0)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to allocate space for 'bogus' message")

        /* Allocate the native message in memory */
	if(NULL == (oh->mesg[idx].native = H5MM_malloc(sizeof(H5O_bogus_t))))
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "memory allocation failed for 'bogus' message")

        /* Update the native part */
        ((H5O_bogus_t *)(oh->mesg[idx].native))->u = H5O_BOGUS_VALUE;

        /* Mark the message and object header as dirty */
	*oh_flags_ptr = TRUE;
        oh->mesg[idx].dirty = TRUE;
        oh->dirty = TRUE;
    } /* end if */

done:
    FUNC_LEAVE(ret_value)
} /* end H5O_bogus_oh() */


/*-------------------------------------------------------------------------
 * Function:	H5O_bogus
 *
 * Purpose:	Create a "bogus" message in an object.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              <koziol@ncsa.uiuc.edu>
 *              Tuesday, January 21, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_bogus(H5O_loc_t *loc, hid_t dxpl_id)
{
    H5O_t	*oh = NULL;
    unsigned	oh_flags = H5AC__NO_FLAGS_SET;
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER(H5O_bogus, FAIL)

    /* check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));

    /* Verify write access to the file */
    if(0 == (loc->file->intent & H5F_ACC_RDWR))
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "no write intent on file")

    /* Get the object header */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* Create the "bogus" message */
    if(H5O_bogus_oh(ent->file, dxpl_id, oh, &oh_flags) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to update object 'bogus' message")

done:
    if(oh && H5AC_unprotect(ent->file, dxpl_id, H5AC_OHDR, ent->header, oh, oh_flags) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE(ret_value)
} /* end H5O_bogus() */
#endif /* H5O_ENABLE_BOGUS */


/*-------------------------------------------------------------------------
 * Function:	H5O_remove
 *
 * Purpose:	Removes the specified message from the object header.
 *		If sequence is H5O_ALL (-1) then all messages of the
 *		specified type are removed.  Removing a message causes
 *		the sequence numbers to change for subsequent messages of
 *		the same type.
 *
 *		No attempt is made to join adjacent free areas of the
 *		object header into a single larger free area.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug 28 1997
 *
 * Modifications:
 *
 *	Robb Matzke, 7 Jan 1998
 *	Does not remove constant messages.
 *
 *      Changed to use IDs for types, instead of type objects, then
 *      call "real" routine.
 *      Quincey Koziol
 *	Feb 14 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_remove(H5O_loc_t *loc, unsigned type_id, int sequence, hbool_t adj_link, hid_t dxpl_id)
{
    const H5O_msg_class_t *type;            /* Actual H5O class type for the ID */
    herr_t      ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(H5O_remove, FAIL)

    /* check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);

    /* Call the "real" remove routine */
    if((ret_value = H5O_remove_real(loc, type, sequence, NULL, NULL, adj_link, dxpl_id)) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, FAIL, "unable to remove object header message")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5O_remove_op
 *
 * Purpose:	Removes messages from the object header that a callback
 *              routine indicates should be removed.
 *
 *		No attempt is made to join adjacent free areas of the
 *		object header into a single larger free area.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Sep  6 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_remove_op(const H5O_loc_t *loc, unsigned type_id, int sequence,
    H5O_operator_t op, void *op_data, hbool_t adj_link, hid_t dxpl_id)
{
    const H5O_msg_class_t *type;            /* Actual H5O class type for the ID */
    herr_t      ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(H5O_remove_op, FAIL)

    /* check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);

    /* Call the "real" remove routine */
    if((ret_value = H5O_remove_real(loc, type, sequence, op, op_data, adj_link, dxpl_id)) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, FAIL, "unable to remove object header message")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_remove_op() */


/*-------------------------------------------------------------------------
 * Function:	H5O_remove_cb
 *
 * Purpose:	Object header iterator callback routine to remove messages
 *              of a particular type that match a particular sequence number,
 *              or all messages if the sequence number is H5O_ALL (-1).
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Sep  6 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_remove_cb(H5O_mesg_t *mesg/*in,out*/, unsigned idx, unsigned * oh_flags_ptr,
    void *_udata/*in,out*/)
{
    H5O_iter_ud1_t *udata = (H5O_iter_ud1_t *)_udata;   /* Operator user data */
    htri_t try_remove = FALSE;         /* Whether to try removing a message */
    herr_t ret_value = H5O_ITER_CONT;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_remove_cb)

    /* check args */
    HDassert(mesg);

    /* Check for callback routine */
    if(udata->op) {
        /* Call the iterator callback */
        if((try_remove = (udata->op)(mesg->native, idx, udata->op_data)) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, H5O_ITER_ERROR, "object header message deletion callback failed")
    } /* end if */
    else {
        /* If there's no callback routine, does the sequence # match? */
        if((int)idx == udata->sequence || H5O_ALL == udata->sequence)
            try_remove = H5O_ITER_STOP;
    } /* end else */

    /* Try removing the message, if indicated */
    if(try_remove) {
        /*
         * Keep track of how many times we failed trying to remove constant
         * messages.
         */
        if(mesg->flags & H5O_FLAG_CONSTANT) {
            udata->nfailed++;
        } /* end if */
        else {
            /* Free any space referred to in the file from this message */
            if(H5O_delete_mesg(udata->f, udata->dxpl_id, mesg, udata->adj_link) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, H5O_ITER_ERROR, "unable to delete file space for object header message")

            /* Free any native information */
            H5O_free_mesg(mesg);

            /* Change message type to nil and zero it */
            mesg->type = H5O_MSG_NULL;
            HDmemset(mesg->raw, 0, mesg->raw_size);

            /* Indicate that the message was modified */
            mesg->dirty = TRUE;

            /* Indicate that the object header was modified */
            *oh_flags_ptr |= H5AC__DIRTIED_FLAG;
        } /* end else */

        /* Break out now, if we've found the correct message */
        if(udata->sequence == H5O_FIRST || udata->sequence != H5O_ALL)
            HGOTO_DONE(H5O_ITER_STOP)
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_remove_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5O_remove_real
 *
 * Purpose:	Removes the specified message from the object header.
 *		If sequence is H5O_ALL (-1) then all messages of the
 *		specified type are removed.  Removing a message causes
 *		the sequence numbers to change for subsequent messages of
 *		the same type.
 *
 *		No attempt is made to join adjacent free areas of the
 *		object header into a single larger free area.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug 28 1997
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_remove_real(const H5O_loc_t *loc, const H5O_msg_class_t *type, int sequence,
    H5O_operator_t app_op, void *op_data, hbool_t adj_link, hid_t dxpl_id)
{
    H5O_iter_ud1_t udata;               /* User data for iterator */
    H5O_mesg_operator_t op;             /* Wrapper for operator */
    herr_t ret_value=SUCCEED;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_remove_real)

    /* check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(type);

    if (0==(loc->file->intent & H5F_ACC_RDWR))
	HGOTO_ERROR(H5E_HEAP, H5E_WRITEERROR, FAIL, "no write intent on file")

    /* Set up iterator operator data */
    udata.f = loc->file;
    udata.dxpl_id = dxpl_id;
    udata.sequence = sequence;
    udata.nfailed = 0;
    udata.op = app_op;
    udata.op_data = op_data;
    udata.adj_link = adj_link;

    /* Iterate over the messages, deleting appropriate one(s) */
    op.lib_op = H5O_remove_cb;
    if(H5O_iterate_real(loc, type, H5AC_WRITE, TRUE, op, &udata, dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "error iterating over messages")

    /* Fail if we tried to remove any constant messages */
    if(udata.nfailed)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to remove constant message(s)")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_remove_real() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5O_alloc_msgs
 *
 * Purpose:     Allocate more messages for a header
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Nov 21 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_alloc_msgs(H5O_t *oh, size_t min_alloc)
{
    size_t old_alloc;                   /* Old number of messages allocated */
    size_t na;                          /* New number of messages allocated */
    H5O_mesg_t *new_mesg;               /* Pointer to new message array */
    herr_t ret_value = SUCCEED; 	/* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_alloc_msgs)

    /* check args */
    HDassert(oh);

    /* Initialize number of messages information */
    old_alloc = oh->alloc_nmesgs;
    na = oh->alloc_nmesgs + MAX (H5O_NMESGS, min_alloc);

    /* Attempt to allocate more memory */
    if(NULL == (new_mesg = H5FL_SEQ_REALLOC(H5O_mesg_t, oh->mesg, na)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    /* Update ohdr information */
    oh->alloc_nmesgs = na;
    oh->mesg = new_mesg;

    /* Set new object header info to zeros */
    HDmemset(&oh->mesg[old_alloc], 0, (oh->alloc_nmesgs - old_alloc) * sizeof(H5O_mesg_t));

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O_alloc_msgs() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5O_move_msgs_forward
 *
 * Purpose:     Move messages toward first chunk
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Oct 17 2005
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5O_move_msgs_forward(H5F_t *f, H5O_t *oh, hid_t dxpl_id)
{
    hbool_t packed_msg;                 /* Flag to indicate that messages were packed */
    hbool_t did_packing = FALSE;        /* Whether any messages were packed */
    htri_t ret_value; 	                /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_move_msgs_forward)

    /* check args */
    HDassert(oh);

    /* Loop until no messages packed */
    /* (Double loop is not very efficient, but it would be some extra work to add
     *      a list of messages to each chunk -QAK)
     */
    do {
        H5O_mesg_t *curr_msg;       /* Pointer to current message to operate on */
        unsigned	u;              /* Local index variable */

        /* Reset packed messages flag */
        packed_msg = FALSE;

        /* Scan through messages for messages that can be moved earlier in chunks */
        for(u = 0, curr_msg = &oh->mesg[0]; u < oh->nmesgs; u++, curr_msg++) {
            if(H5O_NULL_ID == curr_msg->type->id) {
                H5O_chunk_t *chunk;     /* Pointer to chunk that null message is in */

                /* Check if null message is not last in chunk */
                chunk = &(oh->chunk[curr_msg->chunkno]);
                if((curr_msg->raw + curr_msg->raw_size) != (chunk->image + chunk->size)) {
                    H5O_mesg_t *nonnull_msg;       /* Pointer to current message to operate on */
                    unsigned	v;              /* Local index variable */

                    /* Loop over messages again, looking for the message in the chunk after the null message */
                    for(v = 0, nonnull_msg = &oh->mesg[0]; v < oh->nmesgs; v++, nonnull_msg++) {
                        /* Locate message that is immediately after the null message */
                        if((curr_msg->chunkno == nonnull_msg->chunkno) &&
                                ((curr_msg->raw + curr_msg->raw_size) == (nonnull_msg->raw - H5O_SIZEOF_MSGHDR(f)))) {
                            /* Don't swap messages if the second message is also a null message */
                            /* (We'll merge them together later, in another routine) */
                            if(H5O_NULL_ID != nonnull_msg->type->id) {
                                /* Make certain non-null message has been translated into native form */
                                /* (So that when we mark it dirty, it will get copied back into raw chunk image) */
                                LOAD_NATIVE(f, dxpl_id, nonnull_msg, FAIL)

                                /* Adjust non-null message's offset in chunk */
                                nonnull_msg->raw = curr_msg->raw;

                                /* Adjust null message's offset in chunk */
                                curr_msg->raw = nonnull_msg->raw +
                                    nonnull_msg->raw_size + H5O_SIZEOF_MSGHDR(f);

                                /* Mark messages dirty */
                                curr_msg->dirty = TRUE;
                                nonnull_msg->dirty = TRUE;

                                /* Set the flag to indicate that the null message
                                 * was packed - if its not at the end its chunk,
                                 * we'll move it again on the next pass.
                                 */
                                packed_msg = TRUE;
                            } /* end if */

                            /* Break out of loop */
                            break;
                        } /* end if */
                    } /* end for */
                    /* Should have been message after null message */
                    HDassert(v < oh->nmesgs);
                } /* end if */
            } /* end if */
            else {
                H5O_mesg_t *null_msg;       /* Pointer to current message to operate on */
                unsigned	v;              /* Local index variable */

                /* Loop over messages again, looking for large enough null message in earlier chunk */
                for(v = 0, null_msg = &oh->mesg[0]; v < oh->nmesgs; v++, null_msg++) {
                    if(H5O_NULL_ID == null_msg->type->id && curr_msg->chunkno > null_msg->chunkno
                            && curr_msg->raw_size <= null_msg->raw_size) {
                        unsigned old_chunkno;   /* Old message information */
                        uint8_t *old_raw;
                        size_t old_raw_size;

                        /* Keep old information about non-null message */
                        old_chunkno = curr_msg->chunkno;
                        old_raw = curr_msg->raw;
                        old_raw_size = curr_msg->raw_size;

                        /* Make certain non-null message has been translated into native form */
                        /* (So that when we mark it dirty, it will get copied back into raw chunk image) */
                        LOAD_NATIVE(f, dxpl_id, curr_msg, FAIL)

                        /* Change information for non-null message */
                        if(curr_msg->raw_size == null_msg->raw_size) {
                            /* Point non-null message at null message's space */
                            curr_msg->chunkno = null_msg->chunkno;
                            curr_msg->raw = null_msg->raw;

                            /* Mark non-null message dirty */
                            curr_msg->dirty = TRUE;

                            /* Point null message at old non-null space */
                            /* (Instead of freeing it and allocating new message) */
                            null_msg->chunkno = old_chunkno;
                            null_msg->raw = old_raw;

                            /* Mark null message dirty */
                            null_msg->dirty = TRUE;
                        } /* end if */
                        else {
                            unsigned new_null_msg;          /* Message index for new null message */

                            /* Point non-null message at null message's space */
                            curr_msg->chunkno = null_msg->chunkno;
                            curr_msg->raw = null_msg->raw;

                            /* Mark non-null message dirty */
                            curr_msg->dirty = TRUE;

                            /* Adjust null message's size & offset */
                            null_msg->raw += curr_msg->raw_size + H5O_SIZEOF_MSGHDR(f);
                            null_msg->raw_size -= curr_msg->raw_size + H5O_SIZEOF_MSGHDR(f);

                            /* Mark null message dirty */
                            null_msg->dirty = TRUE;

                            /* Create new null message for previous location of non-null message */
                            if(oh->nmesgs >= oh->alloc_nmesgs) {
                                if(H5O_alloc_msgs(oh, (size_t)0) < 0)
                                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate more space for messages")
                            } /* end if */

                            /* Get message # for new message */
                            new_null_msg = oh->nmesgs++;

                            /* Initialize new null message */
                            oh->mesg[new_null_msg].type = H5O_MSG_NULL;
                            oh->mesg[new_null_msg].dirty = TRUE;
                            oh->mesg[new_null_msg].native = NULL;
                            oh->mesg[new_null_msg].raw = old_raw;
                            oh->mesg[new_null_msg].raw_size = old_raw_size;
                            oh->mesg[new_null_msg].chunkno = old_chunkno;
                        } /* end else */

                        /* Indicate that we packed messages */
                        packed_msg = TRUE;

                        /* Break out of loop */
                        /* (If it's possible to move messge to even ealier chunk
                         *      we'll get it on the next pass - QAK)
                         */
                        break;
                    } /* end if */
                } /* end for */

                /* If we packed messages, get out of loop and start over */
                /* (Don't know if this has any benefit one way or the other -QAK) */
                if(packed_msg)
                    break;
            } /* end else */
        } /* end for */

        /* If we did any packing, remember that */
        if(packed_msg)
            did_packing = TRUE;
    } while(packed_msg);

    /* Set return value */
    ret_value = did_packing;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O_move_msgs_forward() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5O_merge_null
 *
 * Purpose:     Merge neighboring null messages in an object header
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Oct 10 2005
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5O_merge_null(H5F_t *f, H5O_t *oh)
{
    hbool_t merged_msg;                 /* Flag to indicate that messages were merged */
    hbool_t did_merging = FALSE;        /* Whether any messages were merged */
    htri_t ret_value; 	                /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_merge_null)

    /* check args */
    HDassert(oh != NULL);

    /* Loop until no messages merged */
    /* (Double loop is not very efficient, but it would be some extra work to add
     *      a list of messages to each chunk -QAK)
     */
    do {
        H5O_mesg_t *curr_msg;       /* Pointer to current message to operate on */
        unsigned	u;              /* Local index variable */

        /* Reset merged messages flag */
        merged_msg = FALSE;

        /* Scan messages for adjacent null messages & merge them */
        for(u = 0, curr_msg = &oh->mesg[0]; u < oh->nmesgs; u++, curr_msg++) {
            if(H5O_NULL_ID == curr_msg->type->id) {
                H5O_mesg_t *curr_msg2;       /* Pointer to current message to operate on */
                unsigned	v;              /* Local index variable */

                /* Loop over messages again, looking for null message in same chunk */
                for(v = 0, curr_msg2 = &oh->mesg[0]; v < oh->nmesgs; v++, curr_msg2++) {
                    if(u != v && H5O_NULL_ID == curr_msg2->type->id && curr_msg->chunkno == curr_msg2->chunkno) {

                        /* Check for second message after first message */
                        if((curr_msg->raw + curr_msg->raw_size) == (curr_msg2->raw - H5O_SIZEOF_MSGHDR(f))) {
                            /* Extend first null message length to cover second null message */
                            curr_msg->raw_size += (H5O_SIZEOF_MSGHDR(f) + curr_msg2->raw_size);

                            /* Message has been merged */
                            merged_msg = TRUE;
                        } /* end if */
                        /* Check for second message befre first message */
                        else if((curr_msg->raw - H5O_SIZEOF_MSGHDR(f)) == (curr_msg2->raw + curr_msg2->raw_size)) {
                            /* Adjust first message address and extend length to cover second message */
                            curr_msg->raw -= (H5O_SIZEOF_MSGHDR(f) + curr_msg2->raw_size);
                            curr_msg->raw_size += (H5O_SIZEOF_MSGHDR(f) + curr_msg2->raw_size);

                            /* Message has been merged */
                            merged_msg = TRUE;
                        } /* end if */

                        /* Second message has been merged, delete it */
                        if(merged_msg) {
                            /* Release any information/memory for second message */
                            H5O_free_mesg(curr_msg2);

                            /* Mark first message as dirty */
                            curr_msg->dirty = TRUE;

                            /* Remove second message from list of messages */
                            if(v < (oh->nmesgs - 1))
                                HDmemmove(&oh->mesg[v], &oh->mesg[v + 1], ((oh->nmesgs - 1) - v) * sizeof(H5O_mesg_t));

                            /* Decrement # of messages */
                            /* (Don't bother reducing size of message array for now -QAK) */
                            oh->nmesgs--;

                            /* Get out of loop */
                            break;
                        } /* end if */
                    } /* end if */
                } /* end for */

                /* Get out of loop if we merged messages */
                if(merged_msg)
                    break;
            } /* end if */
        } /* end for */

        /* If we did any merging, remember that */
        if(merged_msg)
            did_merging = TRUE;
    } while(merged_msg);

    /* Set return value */
    ret_value = did_merging;

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O_merge_null() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5O_remove_empty_chunks
 *
 * Purpose:     Attempt to eliminate empty chunks from object header.
 *
 *              This examines a chunk to see if it's empty
 *              and removes it (and the continuation message that points to it)
 *              from the object header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Oct 17 2005
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5O_remove_empty_chunks(H5F_t *f, H5O_t *oh, hid_t dxpl_id)
{
    hbool_t deleted_chunk;              /* Whether to a chunk was deleted */
    hbool_t did_deleting = FALSE;       /* Whether any chunks were deleted */
    htri_t ret_value; 	                /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_remove_empty_chunks)

    /* check args */
    HDassert(oh != NULL);

    /* Loop until no chunks are freed */
    do {
        H5O_mesg_t *null_msg;       /* Pointer to null message found */
        H5O_mesg_t *cont_msg;       /* Pointer to continuation message found */
        unsigned	u, v;       /* Local index variables */

        /* Reset 'chunk deleted' flag */
        deleted_chunk = FALSE;

        /* Scan messages for null messages that fill an entire chunk */
        for(u = 0, null_msg = &oh->mesg[0]; u < oh->nmesgs; u++, null_msg++) {
            /* If a null message takes up an entire object header chunk (and
             * its not the "base" chunk), delete that chunk from object header
             */
            if(H5O_NULL_ID == null_msg->type->id && null_msg->chunkno > 0 &&
                    (H5O_SIZEOF_MSGHDR(f) + null_msg->raw_size) == oh->chunk[null_msg->chunkno].size) {
                H5O_mesg_t *curr_msg;           /* Pointer to current message to operate on */
                unsigned null_msg_no;           /* Message # for null message */
                unsigned deleted_chunkno;       /* Chunk # to delete */

                /* Locate continuation message that points to chunk */
                for(v = 0, cont_msg = &oh->mesg[0]; v < oh->nmesgs; v++, cont_msg++) {
                    if(H5O_CONT_ID == cont_msg->type->id) {
                        /* Decode current continuation message if necessary */
                        if(NULL == cont_msg->native) {
                            HDassert(H5O_MSG_CONT->decode);
                            cont_msg->native = (H5O_MSG_CONT->decode) (f, dxpl_id, cont_msg->raw);
                            if(NULL == cont_msg->native)
                                HGOTO_ERROR(H5E_OHDR, H5E_CANTDECODE, FAIL, "unable to decode message")
                        } /* end if */

                        /* Check for correct chunk to delete */
                        if(oh->chunk[null_msg->chunkno].addr == ((H5O_cont_t *)(cont_msg->native))->addr)
                            break;
                    } /* end if */
                } /* end for */
                /* Must be a continuation message that points to chunk containing null message */
                HDassert(v < oh->nmesgs);
                HDassert(cont_msg);

                /* Initialize information about null message */
                null_msg_no = u;
                deleted_chunkno = null_msg->chunkno;

                /*
                 * Convert continuation message into a null message
                 */

                /* Free space for chunk referred to in the file from the continuation message */
                if(H5O_delete_mesg(f, dxpl_id, cont_msg, TRUE) < 0)
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, H5O_ITER_ERROR, "unable to delete file space for object header message")

                /* Free any native information */
                H5O_free_mesg(cont_msg);

                /* Change message type to nil and zero it */
                cont_msg->type = H5O_MSG_NULL;
                HDmemset(cont_msg->raw, 0, cont_msg->raw_size);

                /* Indicate that the continuation message was modified */
                cont_msg->dirty = TRUE;

                /*
                 * Remove chunk from object header's data structure
                 */

                /* Free memory for chunk image */
                H5FL_BLK_FREE(chunk_image, oh->chunk[null_msg->chunkno].image);

                /* Remove chunk from list of chunks */
                if(null_msg->chunkno < (oh->nchunks - 1))
                    HDmemmove(&oh->chunk[null_msg->chunkno], &oh->chunk[null_msg->chunkno + 1], ((oh->nchunks - 1) - null_msg->chunkno) * sizeof(H5O_chunk_t));

                /* Decrement # of chunks */
                /* (Don't bother reducing size of chunk array for now -QAK) */
                oh->nchunks--;

                /*
                 * Delete null message (in empty chunk that was be freed) from list of messages
                 */

                /* Release any information/memory for message */
                H5O_free_mesg(null_msg);

                /* Remove null message from list of messages */
                if(null_msg_no < (oh->nmesgs - 1))
                    HDmemmove(&oh->mesg[null_msg_no], &oh->mesg[null_msg_no + 1], ((oh->nmesgs - 1) - null_msg_no) * sizeof(H5O_mesg_t));

                /* Decrement # of messages */
                /* (Don't bother reducing size of message array for now -QAK) */
                oh->nmesgs--;

                /* Adjust chunk # for messages in chunks after deleted chunk */
                for(u = 0, curr_msg = &oh->mesg[0]; u < oh->nmesgs; u++, curr_msg++) {
                    HDassert(curr_msg->chunkno != deleted_chunkno);
                    if(curr_msg->chunkno > deleted_chunkno)
                        curr_msg->chunkno--;
                } /* end for */

                /* Found chunk to delete */
                deleted_chunk = TRUE;
                break;
            } /* end if */
        } /* end for */

        /* If we deleted any chunks, remember that */
        if(deleted_chunk)
            did_deleting = TRUE;
    } while(deleted_chunk);

    /* Set return value */
    ret_value = did_deleting;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O_remove_empty_chunks() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5O_condense_header
 *
 * Purpose:     Attempt to eliminate empty chunks from object header.
 *
 *              Currently, this just examines a chunk to see if it's empty
 *              and removes it from the object header.  It's possible that
 *              a more sophiticated algorithm would repack messages from
 *              one chunk to another.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Oct  4 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_condense_header(H5F_t *f, H5O_t *oh, hid_t dxpl_id)
{
    hbool_t rescan_header;              /* Whether to rescan header */
    htri_t result;                      /* Result from packing/merging/etc */
    herr_t ret_value = SUCCEED; 	/* return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_condense_header)

    /* check args */
    HDassert(oh != NULL);

    /* Loop until no changed to the object header messages & chunks */
    do {
        /* Reset 'rescan chunks' flag */
        rescan_header = FALSE;

        /* Scan for messages that can be moved earlier in chunks */
        result = H5O_move_msgs_forward(f, oh, dxpl_id);
        if(result < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTPACK, FAIL, "can't move header messages forward")
        if(result > 0)
            rescan_header = TRUE;

        /* Scan for adjacent null messages & merge them */
        result = H5O_merge_null(f, oh);
        if(result < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTPACK, FAIL, "can't pack null header messages")
        if(result > 0)
            rescan_header = TRUE;

        /* Scan for empty chunks to remove */
        result = H5O_remove_empty_chunks(f, oh, dxpl_id);
        if(result < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTPACK, FAIL, "can't remove empty chunk")
        if(result > 0)
            rescan_header = TRUE;
    } while(rescan_header);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O_condense_header() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5O_alloc_extend_chunk
 *
 * Purpose:     Attempt to extend a chunk that is allocated on disk.
 *
 *              If the extension is successful, and if the last message
 *		of the chunk is the null message, then that message will
 *		be extended with the chunk.  Otherwise a new null message
 *		is created.
 *
 *              f is the file in which the chunk will be written.  It is
 *              included to ensure that there is enough space to extend
 *              this chunk.
 *
 * Return:      TRUE:		The chunk has been extended, and *msg_idx
 *				contains the message index for null message
 *				which is large enough to hold size bytes.
 *
 *		FALSE:		The chunk cannot be extended, and *msg_idx
 *				is undefined.
 *
 *		FAIL:		Some internal error has been detected.
 *
 * Programmer:  John Mainzer -- 8/16/05
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5O_alloc_extend_chunk(H5F_t *f,
                       H5O_t *oh,
                       unsigned chunkno,
                       size_t size,
                       unsigned * msg_idx)
{
    size_t      delta;          /* Change in chunk's size */
    size_t      aligned_size = H5O_ALIGN(size);
    uint8_t     *old_image;     /* Old address of chunk's image in memory */
    size_t      old_size;       /* Old size of chunk */
    htri_t      tri_result;     /* Result from checking if chunk can be extended */
    int         extend_msg = -1;/* Index of null message to extend */
    unsigned    u;              /* Local index variable */
    htri_t      ret_value = TRUE; 	/* return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_alloc_extend_chunk)

    /* check args */
    HDassert(f != NULL);
    HDassert(oh != NULL);
    HDassert(chunkno < oh->nchunks);
    HDassert(size > 0);
    HDassert(msg_idx != NULL);

    if(!H5F_addr_defined(oh->chunk[chunkno].addr))
        HGOTO_ERROR(H5E_OHDR, H5E_NOSPACE, FAIL, "chunk isn't on disk")

    /* Test to see if the specified chunk ends with a null messages.
     * If successful, set the index of the the null message in extend_msg.
     */
    for(u = 0; u < oh->nmesgs; u++) {
        /* Check for null message at end of proper chunk */
        if(oh->mesg[u].chunkno == chunkno && H5O_NULL_ID == oh->mesg[u].type->id &&
                (oh->mesg[u].raw + oh->mesg[u].raw_size == oh->chunk[chunkno].image + oh->chunk[chunkno].size)) {

            extend_msg = u;
            break;
        } /* end if */
    } /* end for */

    /* If we can extend an existing null message, adjust the delta appropriately */
    if(extend_msg >= 0)
        delta = MAX(H5O_MIN_SIZE, aligned_size - oh->mesg[extend_msg].raw_size);
    else
        delta = MAX(H5O_MIN_SIZE, aligned_size + H5O_SIZEOF_MSGHDR(f));
    delta = H5O_ALIGN(delta);

    /* determine whether the chunk can be extended */
    tri_result = H5MF_can_extend(f, H5FD_MEM_OHDR, oh->chunk[chunkno].addr,
                                 (hsize_t)(oh->chunk[chunkno].size), (hsize_t)delta);
    if(tri_result == FALSE) { /* can't extend -- we are done */
        HGOTO_DONE(FALSE);
    } else if(tri_result != TRUE) { /* system error */
        HGOTO_ERROR(H5E_RESOURCE, H5E_SYSTEM, FAIL, "can't tell if we can extend chunk")
    }

    /* If we get this far, we should be able to extend the chunk */
    if(H5MF_extend(f, H5FD_MEM_OHDR, oh->chunk[chunkno].addr, (hsize_t)(oh->chunk[chunkno].size), (hsize_t)delta) < 0 )
        HGOTO_ERROR(H5E_RESOURCE, H5E_SYSTEM, FAIL, "can't extend chunk")

    /* If we can extend an existing null message, take take of that */
    if(extend_msg >= 0) {
        /* Adjust message size of existing null message */
        oh->mesg[extend_msg].dirty = TRUE;
        oh->mesg[extend_msg].raw_size += delta;
    } /* end if */
    /* Create new null message for end of chunk */
    else {
        /* Create a new null message */
        if(oh->nmesgs >= oh->alloc_nmesgs) {
            if(H5O_alloc_msgs(oh, (size_t)0) < 0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate more space for messages")
        } /* end if */

        /* Set extension message */
        extend_msg = oh->nmesgs++;

        /* Initialize new null message */
        oh->mesg[extend_msg].type = H5O_MSG_NULL;
        oh->mesg[extend_msg].dirty = TRUE;
        oh->mesg[extend_msg].native = NULL;
        oh->mesg[extend_msg].raw = oh->chunk[chunkno].image +
                            oh->chunk[chunkno].size + H5O_SIZEOF_MSGHDR(f);
        oh->mesg[extend_msg].raw_size = delta - H5O_SIZEOF_MSGHDR(f);
        oh->mesg[extend_msg].chunkno = chunkno;
    } /* end else */

    /* Allocate more memory space for chunk's image */
    old_image = oh->chunk[chunkno].image;
    old_size = oh->chunk[chunkno].size;
    oh->chunk[chunkno].size += delta;
    oh->chunk[chunkno].image = H5FL_BLK_REALLOC(chunk_image, old_image, oh->chunk[chunkno].size);
    oh->chunk[chunkno].dirty = TRUE;
    if(NULL == oh->chunk[chunkno].image)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    /* Wipe new space for chunk */
    HDmemset(oh->chunk[chunkno].image + old_size, 0, oh->chunk[chunkno].size - old_size);

    /* Spin through existing messages, adjusting them */
    for(u = 0; u < oh->nmesgs; u++) {
        /* Adjust raw addresses for messages in this chunk to reflect to 'image' address */
        if(oh->mesg[u].chunkno == chunkno)
            oh->mesg[u].raw = oh->chunk[chunkno].image + (oh->mesg[u].raw - old_image);

        /* Find continuation message which points to this chunk and adjust chunk's size */
        /* (Chunk 0 doesn't have a continuation message that points to it and
         * it's size is directly encoded in the object header) */
        if(chunkno > 0 && (H5O_CONT_ID == oh->mesg[u].type->id) &&
                (((H5O_cont_t *)(oh->mesg[u].native))->chunkno == chunkno)) {
            /* Adjust size of continuation message */
            HDassert(((H5O_cont_t *)(oh->mesg[u].native))->size == old_size);
            ((H5O_cont_t *)(oh->mesg[u].native))->size = oh->chunk[chunkno].size;

            /* Flag continuation message as dirty */
            oh->mesg[u].dirty = TRUE;
        } /* end if */
    } /* end for */

    /* Set return value */
    *msg_idx = extend_msg;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O_alloc_extend_chunk() */


/*-------------------------------------------------------------------------
 * Function:    H5O_alloc_new_chunk
 *
 * Purpose:     Allocates a new chunk for the object header, including
 *		file space.
 *
 *              One of the other chunks will get an object continuation
 *		message.  If there isn't room in any other chunk for the
 *		object continuation message, then some message from
 *		another chunk is moved into this chunk to make room.
 *
 *              SIZE need not be aligned.
 *
 * Return:      Success:        Index number of the null message for the
 *                              new chunk.  The null message will be at
 *                              least SIZE bytes not counting the message
 *                              ID or size fields.
 *
 *              Failure:        Negative
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Aug  7 1997
 *
 *-------------------------------------------------------------------------
 */
static unsigned
H5O_alloc_new_chunk(H5F_t *f,
                    hid_t dxpl_id,
                    H5O_t *oh,
                    size_t size)
{
    size_t      cont_size;              /*continuation message size     */
    int         found_null = (-1);      /*best fit null message         */
    int         found_attr = (-1);      /*best fit attribute message    */
    int         found_link = (-1);      /*best fit link message         */
    int         found_other = (-1);     /*best fit other message        */
    unsigned    idx;                    /*message number */
    uint8_t     *p = NULL;              /*ptr into new chunk            */
    H5O_cont_t  *cont = NULL;           /*native continuation message   */
    int chunkno;
    unsigned    u;
    haddr_t	new_chunk_addr;
    unsigned    ret_value;              /*return value  */

    FUNC_ENTER_NOAPI_NOINIT(H5O_alloc_new_chunk)

    /* check args */
    HDassert(oh);
    HDassert(size > 0);
    size = H5O_ALIGN(size);

    /*
     * Find the smallest null message that will hold an object
     * continuation message.  Failing that, find the smallest message
     * that could be moved to make room for the continuation message.
     * Don't ever move continuation message from one chunk to another.
     * Prioritize attribute and link messages moving to later chunks,
     * instead of more "important" messages.
     */
    cont_size = H5O_ALIGN (H5F_SIZEOF_ADDR(f) + H5F_SIZEOF_SIZE(f));
    for(u = 0; u < oh->nmesgs; u++) {
        int msg_id = oh->mesg[u].type->id;      /* Temp. copy of message type ID */

	if(H5O_NULL_ID == msg_id) {
	    if(cont_size == oh->mesg[u].raw_size) {
		found_null = u;
		break;
	    } else if(oh->mesg[u].raw_size >= cont_size &&
                   (found_null < 0 ||
                    oh->mesg[u].raw_size < oh->mesg[found_null].raw_size)) {
		found_null = u;
	    }
	} else if(H5O_CONT_ID == msg_id) {
	    /*don't consider continuation messages */
	} else if(H5O_ATTR_ID == msg_id) {
            if(oh->mesg[u].raw_size >= cont_size &&
                   (found_attr < 0 ||
                    oh->mesg[u].raw_size < oh->mesg[found_attr].raw_size))
                found_attr = u;
#ifdef H5_GROUP_REVISION
	} else if(H5O_LINK_ID == msg_id) {
            if(oh->mesg[u].raw_size >= cont_size &&
                   (found_link < 0 ||
                    oh->mesg[u].raw_size < oh->mesg[found_link].raw_size))
                found_link = u;
#endif /* H5_GROUP_REVISION */
	} else {
            if(oh->mesg[u].raw_size >= cont_size &&
                   (found_other < 0 ||
                    oh->mesg[u].raw_size < oh->mesg[found_other].raw_size))
                found_other = u;
	} /* end else */
    } /* end for */
    HDassert(found_null >= 0 || found_attr >= 0 || found_link >= 0 || found_other >= 0);

    /*
     * If we must move some other message to make room for the null
     * message, then make sure the new chunk has enough room for that
     * other message.
     *
     * Move attributes first, then link messages, then other messages.
     *
     */
    if(found_null < 0) {
        if(found_attr >= 0)
            found_other = found_attr;
        else if(found_link >= 0)
            found_other = found_link;

        HDassert(found_other >= 0);
        size += H5O_SIZEOF_MSGHDR(f) + oh->mesg[found_other].raw_size;
    } /* end if */

    /*
     * The total chunk size must include the requested space plus enough
     * for the message header.  This must be at least some minimum and a
     * multiple of the alignment size.
     */
    size = MAX(H5O_MIN_SIZE, size + H5O_SIZEOF_MSGHDR(f));
    HDassert(size == H5O_ALIGN (size));

    /* allocate space in file to hold the new chunk */
    new_chunk_addr = H5MF_alloc(f, H5FD_MEM_OHDR, dxpl_id, (hsize_t)size);
    if(HADDR_UNDEF == new_chunk_addr)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, UFAIL, "unable to allocate space for new chunk")

    /*
     * Create the new chunk giving it a file address.
     */
    if(oh->nchunks >= oh->alloc_nchunks) {
        unsigned na = oh->alloc_nchunks + H5O_NCHUNKS;
        H5O_chunk_t *x = H5FL_SEQ_REALLOC (H5O_chunk_t, oh->chunk, (size_t)na);

        if(!x)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, UFAIL, "memory allocation failed")
        oh->alloc_nchunks = na;
        oh->chunk = x;
    } /* end if */

    chunkno = oh->nchunks++;
    oh->chunk[chunkno].dirty = TRUE;
    oh->chunk[chunkno].addr = new_chunk_addr;
    oh->chunk[chunkno].size = size;
    if (NULL==(oh->chunk[chunkno].image = p = H5FL_BLK_CALLOC(chunk_image,size)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, UFAIL, "memory allocation failed")

    /*
     * Make sure we have enough space for all possible new messages
     * that could be generated below.
     */
    if(oh->nmesgs + 3 > oh->alloc_nmesgs) {
        if(H5O_alloc_msgs(oh, (size_t)3) < 0)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, UFAIL, "can't allocate more space for messages")
    } /* end if */

    /*
     * Describe the messages of the new chunk.
     */
    if(found_null < 0) {
        found_null = u = oh->nmesgs++;
        oh->mesg[u].type = H5O_MSG_NULL;
        oh->mesg[u].dirty = TRUE;
        oh->mesg[u].native = NULL;
        oh->mesg[u].raw = oh->mesg[found_other].raw;
        oh->mesg[u].raw_size = oh->mesg[found_other].raw_size;
        oh->mesg[u].chunkno = oh->mesg[found_other].chunkno;

        oh->mesg[found_other].dirty = TRUE;
        /* Copy the message to the new location */
        HDmemcpy(p + H5O_SIZEOF_MSGHDR(f),
                 oh->mesg[found_other].raw,
                 oh->mesg[found_other].raw_size);
        oh->mesg[found_other].raw = p + H5O_SIZEOF_MSGHDR(f);
        oh->mesg[found_other].chunkno = chunkno;
        p += H5O_SIZEOF_MSGHDR(f) + oh->mesg[found_other].raw_size;
        size -= H5O_SIZEOF_MSGHDR(f) + oh->mesg[found_other].raw_size;
    } /* end if */
    idx = oh->nmesgs++;
    oh->mesg[idx].type = H5O_MSG_NULL;
    oh->mesg[idx].dirty = TRUE;
    oh->mesg[idx].native = NULL;
    oh->mesg[idx].raw = p + H5O_SIZEOF_MSGHDR(f);
    oh->mesg[idx].raw_size = size - H5O_SIZEOF_MSGHDR(f);
    oh->mesg[idx].chunkno = chunkno;

    /*
     * If the null message that will receive the continuation message
     * is larger than the continuation message, then split it into
     * two null messages.
     */
    if(oh->mesg[found_null].raw_size > cont_size) {
        u = oh->nmesgs++;
        oh->mesg[u].type = H5O_MSG_NULL;
        oh->mesg[u].dirty = TRUE;
        oh->mesg[u].native = NULL;
        oh->mesg[u].raw = oh->mesg[found_null].raw +
                          cont_size +
                          H5O_SIZEOF_MSGHDR(f);
        oh->mesg[u].raw_size = oh->mesg[found_null].raw_size -
                               (cont_size + H5O_SIZEOF_MSGHDR(f));
        oh->mesg[u].chunkno = oh->mesg[found_null].chunkno;

        oh->mesg[found_null].dirty = TRUE;
        oh->mesg[found_null].raw_size = cont_size;
    } /* end if */

    /*
     * Initialize the continuation message.
     */
    oh->mesg[found_null].type = H5O_MSG_CONT;
    oh->mesg[found_null].dirty = TRUE;
    if (NULL==(cont = H5FL_MALLOC(H5O_cont_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, UFAIL, "memory allocation failed")
    cont->addr = oh->chunk[chunkno].addr;
    cont->size = oh->chunk[chunkno].size;
    cont->chunkno = chunkno;
    oh->mesg[found_null].native = cont;

    /* Set return value */
    ret_value = idx;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O_alloc_new_chunk() */


/*-------------------------------------------------------------------------
 * Function:    H5O_alloc
 *
 * Purpose:     Allocate enough space in the object header for this message.
 *
 * Return:      Success:        Index of message
 *
 *              Failure:        Negative
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Aug  6 1997
 *
 *-------------------------------------------------------------------------
 */
static unsigned
H5O_alloc(H5F_t *f,
          hid_t dxpl_id,
          H5O_t *oh,
          const H5O_msg_class_t *type,
          size_t size,
          unsigned * oh_flags_ptr)
{
    H5O_mesg_t *msg;            /* Pointer to newly allocated message */
    size_t      aligned_size = H5O_ALIGN(size);
    unsigned    idx;            /* Index of message which fits allocation */
    unsigned    ret_value;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_alloc)

    /* check args */
    HDassert(oh);
    HDassert(type);
    HDassert(oh_flags_ptr);

    /* look for a null message which is large enough */
    for(idx = 0; idx < oh->nmesgs; idx++) {
        if(H5O_NULL_ID == oh->mesg[idx].type->id &&
                oh->mesg[idx].raw_size >= aligned_size)
            break;
    } /* end for */

#ifdef LATER
    /*
     * Perhaps if we join adjacent null messages we could make one
     * large enough... we leave this as an exercise for future
     * programmers :-)  This isn't a high priority because when an
     * object header is read from disk the null messages are combined
     * anyway.
     */
#endif

    /* if we didn't find one, then allocate more header space */
    if(idx >= oh->nmesgs) {
        unsigned        chunkno;

        /* check to see if we can extend one of the chunks.  If we can,
         * do so.  Otherwise, we will have to allocate a new chunk.
         *
         * Note that in this new version of this function, all chunks
         * must have file space allocated to them.
         */
        for(chunkno = 0; chunkno < oh->nchunks; chunkno++) {
            htri_t	tri_result;

            HDassert(H5F_addr_defined(oh->chunk[chunkno].addr));

            tri_result = H5O_alloc_extend_chunk(f, oh, chunkno, size, &idx);
            if(tri_result == TRUE)
		break;
            else if(tri_result == FALSE)
                idx = UFAIL;
            else
                HGOTO_ERROR(H5E_OHDR, H5E_SYSTEM, UFAIL, "H5O_alloc_extend_chunk failed unexpectedly")
        } /* end for */

        /* if idx is still UFAIL, we were not able to extend a chunk.
         * Create a new one.
         */
        if(idx == UFAIL)
            if((idx = H5O_alloc_new_chunk(f, dxpl_id, oh, size)) == UFAIL)
                HGOTO_ERROR(H5E_OHDR, H5E_NOSPACE, UFAIL, "unable to create a new object header data chunk")
    } /* end if */

    /* Set pointer to newly allocated message */
    msg = &(oh->mesg[idx]);

    /* do we need to split the null message? */
    if(msg->raw_size > aligned_size) {
        H5O_mesg_t *null_msg;       /* Pointer to null message */
        size_t  mesg_size = aligned_size + H5O_SIZEOF_MSGHDR(f); /* Total size of newly allocated message */

        HDassert(msg->raw_size - aligned_size >= H5O_SIZEOF_MSGHDR(f));

        /* Check if we need to extend message table */
        if(oh->nmesgs >= oh->alloc_nmesgs) {
            if(H5O_alloc_msgs(oh, (size_t)0) < 0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, UFAIL, "can't allocate more space for messages")

            /* "Retarget" local 'msg' pointer into newly allocated array of messages */
            msg = &oh->mesg[idx];
        } /* end if */
        null_msg = &(oh->mesg[oh->nmesgs++]);
        null_msg->type = H5O_MSG_NULL;
        null_msg->dirty = TRUE;
        null_msg->native = NULL;
        null_msg->raw = msg->raw + mesg_size;
        null_msg->raw_size = msg->raw_size - mesg_size;
        null_msg->chunkno = msg->chunkno;
        msg->raw_size = aligned_size;
    } /* end if */

    /* initialize the new message */
    msg->type = type;
    msg->dirty = TRUE;
    msg->native = NULL;

    *oh_flags_ptr |= H5AC__DIRTIED_FLAG;

    /* Set return value */
    ret_value = idx;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O_alloc() */

#ifdef NOT_YET

/*-------------------------------------------------------------------------
 * Function:	H5O_share
 *
 * Purpose:	Writes a message to the global heap.
 *
 * Return:	Success:	Non-negative, and HOBJ describes the global heap
 *				object.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Thursday, April  2, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_share (H5F_t *f, hid_t dxpl_id, const H5O_msg_class_t *type, const void *mesg,
	   H5HG_t *hobj/*out*/)
{
    size_t	size;
    void	*buf = NULL;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_share);

    /* Check args */
    assert (f);
    assert (type);
    assert (mesg);
    assert (hobj);

    /* Encode the message put it in the global heap */
    if ((size = (type->raw_size)(f, mesg))>0) {
	if (NULL==(buf = H5MM_malloc (size)))
	    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
	if ((type->encode)(f, buf, mesg) < 0)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTENCODE, FAIL, "unable to encode message");
	if (H5HG_insert (f, dxpl_id, size, buf, hobj) < 0)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to store message in global heap");
    }

done:
    if(buf)
        H5MM_xfree (buf);

    FUNC_LEAVE_NOAPI(ret_value);
}
#endif /* NOT_YET */


/*-------------------------------------------------------------------------
 * Function:	H5O_raw_size
 *
 * Purpose:	Call the 'raw_size' method for a
 *              particular class of object header.
 *
 * Return:	Size of message on success, 0 on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 13 2003
 *
 *-------------------------------------------------------------------------
 */
size_t
H5O_raw_size(unsigned type_id, const H5F_t *f, const void *mesg)
{
    const H5O_msg_class_t *type;            /* Actual H5O class type for the ID */
    size_t      ret_value;       /* Return value */

    FUNC_ENTER_NOAPI(H5O_raw_size,0)

    /* Check args */
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type=H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);
    HDassert(type->raw_size);
    HDassert(f);
    HDassert(mesg);

    /* Compute the raw data size for the mesg */
    if ((ret_value = (type->raw_size)(f, mesg))==0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTCOUNT, 0, "unable to determine size of message")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_raw_size() */


/*-------------------------------------------------------------------------
 * Function:	H5O_mesg_size
 *
 * Purpose:	Calculate the final size of an encoded message in an object
 *              header.
 *
 * Return:	Size of message on success, 0 on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Sep  6 2005
 *
 *-------------------------------------------------------------------------
 */
size_t
H5O_mesg_size(unsigned type_id, const H5F_t *f, const void *mesg)
{
    const H5O_msg_class_t *type;            /* Actual H5O class type for the ID */
    size_t      ret_value;       /* Return value */

    FUNC_ENTER_NOAPI(H5O_mesg_size,0)

    /* Check args */
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type=H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);
    HDassert(type->raw_size);
    HDassert(f);
    HDassert(mesg);

    /* Compute the raw data size for the mesg */
    if((ret_value = (type->raw_size)(f, mesg)) == 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTCOUNT, 0, "unable to determine size of message")

    /* Adjust size for alignment, if necessary */
    ret_value = H5O_ALIGN(ret_value);

    /* Add space for message header */
    ret_value += H5O_SIZEOF_MSGHDR(f);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_mesg_size() */


/*-------------------------------------------------------------------------
 * Function:	H5O_get_share
 *
 * Purpose:	Call the 'get_share' method for a
 *              particular class of object header.
 *
 * Return:	Success:	Non-negative, and SHARE describes the shared
 *				object.
 *
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Oct  2 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_get_share(unsigned type_id, H5F_t *f, const void *mesg, H5O_shared_t *share)
{
    const H5O_msg_class_t *type;    /* Actual H5O class type for the ID */
    herr_t ret_value;           /* Return value */

    FUNC_ENTER_NOAPI(H5O_get_share,FAIL)

    /* Check args */
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);
    HDassert(type->get_share);
    HDassert(f);
    HDassert(mesg);
    HDassert(share);

    /* Compute the raw data size for the mesg */
    if((ret_value = (type->get_share)(f, mesg, share)) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "unable to retrieve shared message information")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_get_share() */


/*-------------------------------------------------------------------------
 * Function:	H5O_delete
 *
 * Purpose:	Delete an object header from a file.  This frees the file
 *              space used for the object header (and it's continuation blocks)
 *              and also walks through each header message and asks it to
 *              remove all the pieces of the file referenced by the header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 19 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_delete(H5F_t *f, hid_t dxpl_id, haddr_t addr)
{
    H5O_t *oh = NULL;           /* Object header information */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(H5O_delete,FAIL)

    /* Check args */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));

    /* Get the object header information */
    if(NULL == (oh = H5AC_protect(f, dxpl_id, H5AC_OHDR, addr, NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* Delete object */
    if(H5O_delete_oh(f, dxpl_id, oh) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, FAIL, "can't delete object from file")

done:
    if(oh && H5AC_unprotect(f, dxpl_id, H5AC_OHDR, addr, oh, H5AC__DIRTIED_FLAG | H5C__DELETED_FLAG) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_delete() */


/*-------------------------------------------------------------------------
 * Function:	H5O_delete_oh
 *
 * Purpose:	Internal function to:
 *              Delete an object header from a file.  This frees the file
 *              space used for the object header (and it's continuation blocks)
 *              and also walks through each header message and asks it to
 *              remove all the pieces of the file referenced by the header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 19 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_delete_oh(H5F_t *f, hid_t dxpl_id, H5O_t *oh)
{
    H5O_mesg_t *curr_msg;       /* Pointer to current message being operated on */
    unsigned	u;
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_delete_oh)

    /* Check args */
    HDassert(f);
    HDassert(oh);

    /* Walk through the list of object header messages, asking each one to
     * delete any file space used
     */
    for(u = 0, curr_msg = &oh->mesg[0]; u < oh->nmesgs; u++, curr_msg++) {
        /* Free any space referred to in the file from this message */
        if(H5O_delete_mesg(f, dxpl_id, curr_msg, TRUE) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, FAIL, "unable to delete file space for object header message")
    } /* end for */

    /* Free main (first) object header "chunk" */
    if(H5MF_xfree(f, H5FD_MEM_OHDR, dxpl_id, (oh->chunk[0].addr - H5O_SIZEOF_HDR(f)), (hsize_t)(oh->chunk[0].size + H5O_SIZEOF_HDR(f))) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTFREE, FAIL, "unable to free object header")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_delete_oh() */


/*-------------------------------------------------------------------------
 * Function:	H5O_delete_mesg
 *
 * Purpose:	Internal function to:
 *              Delete an object header message from a file.  This frees the file
 *              space used for anything referred to in the object header message.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		September 26 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_delete_mesg(H5F_t *f, hid_t dxpl_id, H5O_mesg_t *mesg, hbool_t adj_link)
{
    const H5O_msg_class_t	*type;  /* Type of object to free */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_delete_mesg)

    /* Check args */
    HDassert(f);
    HDassert(mesg);

    /* Get the message to free's type */
    if(mesg->flags & H5O_FLAG_SHARED)
        type = H5O_MSG_SHARED;
    else
        type = mesg->type;

    /* Check if there is a file space deletion callback for this type of message */
    if(type->del) {
        /*
         * Decode the message if necessary.
         */
        if(NULL == mesg->native) {
            HDassert(type->decode);
            mesg->native = (type->decode) (f, dxpl_id, mesg->raw);
            if(NULL == mesg->native)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTDECODE, FAIL, "unable to decode message")
        } /* end if */

        if((type->del)(f, dxpl_id, mesg->native, adj_link) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, FAIL, "unable to delete file space for object header message")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_delete_msg() */


/*-------------------------------------------------------------------------
 * Function:	H5O_get_info
 *
 * Purpose:	Retrieve information about an object header
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Oct  7 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_get_info(H5O_loc_t *loc, H5O_stat_t *ostat, hid_t dxpl_id)
{
    H5O_t *oh = NULL;           /* Object header information */
    H5O_mesg_t *curr_msg;       /* Pointer to current message being operated on */
    hsize_t total_size;         /* Total amount of space used in file */
    hsize_t free_space;         /* Free space in object header */
    unsigned u;                 /* Local index variable */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(H5O_get_info,FAIL)

    /* Check args */
    HDassert(loc);
    HDassert(ostat);

    /* Get the object header information */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* Iterate over all the messages, accumulating the total size & free space */
    total_size = H5O_SIZEOF_HDR(loc->file);
    free_space = 0;
    for(u = 0, curr_msg = &oh->mesg[0]; u < oh->nmesgs; u++, curr_msg++) {
        /* Accumulate the size for this message */
        total_size += H5O_SIZEOF_MSGHDR(loc->file) + curr_msg->raw_size;

        /* Check for this message being free space */
	if(H5O_NULL_ID == curr_msg->type->id)
            free_space+= H5O_SIZEOF_MSGHDR(loc->file) + curr_msg->raw_size;
    } /* end for */

    /* Set the information for this object header */
    ostat->size = total_size;
    ostat->free = free_space;
    ostat->nmesgs = oh->nmesgs;
    ostat->nchunks = oh->nchunks;

done:
    if(oh && H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, H5AC__NO_FLAGS_SET) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_get_info() */


/*-------------------------------------------------------------------------
 * Function:	H5O_encode
 *
 * Purpose:	Encode an object(data type and simple data space only)
 *              description into a buffer.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Raymond Lu
 *		slu@ncsa.uiuc.edu
 *		July 13, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_encode(H5F_t *f, unsigned char *buf, void *obj, unsigned type_id)
{
    const H5O_msg_class_t   *type;            /* Actual H5O class type for the ID */
    herr_t 	        ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI(H5O_encode,FAIL);

    /* check args */
    assert(type_id < NELMTS(H5O_msg_class_g));
    type=H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    assert(type);

    /* Encode */
    if ((type->encode)(f, buf, obj) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTENCODE, FAIL, "unable to encode message");

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_decode
 *
 * Purpose:	Decode a binary object(data type and data space only)
 *              description and return a new object handle.
 *
 * Return:	Success:        Pointer to object(data type or space)
 *
 *		Failure:	NULL
 *
 * Programmer:	Raymond Lu
 *		slu@ncsa.uiuc.edu
 *		July 14, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void*
H5O_decode(H5F_t *f, const unsigned char *buf, unsigned type_id)
{
    const H5O_msg_class_t   *type;            /* Actual H5O class type for the ID */
    void 	        *ret_value=NULL;       /* Return value */

    FUNC_ENTER_NOAPI(H5O_decode,NULL);

    /* check args */
    assert(type_id < NELMTS(H5O_msg_class_g));
    type=H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    assert(type);

    /* decode */
    if((ret_value = (type->decode)(f, 0, buf))==NULL)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTDECODE, NULL, "unable to decode message");

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_iterate
 *
 * Purpose:	Iterate through object headers of a certain type.
 *
 * Return:	Returns a negative value if something is wrong, the return
 *      value of the last operator if it was non-zero, or zero if all
 *      object headers were processed.
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Nov 19 2004
 *
 * Description:
 *      This function interates over the object headers of an object
 *  specified with 'ent' of type 'type_id'.  For each object header of the
 *  object, the 'op_data' and some additional information (specified below) are
 *  passed to the 'op' function.
 *      The operation receives a pointer to the object header message for the
 *  object being iterated over ('mesg'), and the pointer to the operator data
 *  passed in to H5O_iterate ('op_data').  The return values from an operator
 *  are:
 *      A. Zero causes the iterator to continue, returning zero when all
 *          object headers of that type have been processed.
 *      B. Positive causes the iterator to immediately return that positive
 *          value, indicating short-circuit success.
 *      C. Negative causes the iterator to immediately return that value,
 *          indicating failure.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_iterate(const H5O_loc_t *loc, unsigned type_id, H5O_operator_t app_op,
    void *op_data, hid_t dxpl_id)
{
    const H5O_msg_class_t *type;    /* Actual H5O class type for the ID */
    H5O_mesg_operator_t op;         /* Wrapper for operator */
    herr_t ret_value;               /* Return value */

    FUNC_ENTER_NOAPI(H5O_iterate, FAIL)

    /* check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));
    HDassert(type_id < NELMTS(H5O_msg_class_g));
    type = H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    HDassert(type);

    /* Call the "real" iterate routine */
    op.app_op = app_op;
    if((ret_value = H5O_iterate_real(loc, type, H5AC_READ, FALSE, op, op_data, dxpl_id)) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_BADITER, FAIL, "unable to iterate over object header messages")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_iterate() */


/*-------------------------------------------------------------------------
 * Function:	H5O_iterate_real
 *
 * Purpose:	Iterate through object headers of a certain type.
 *
 * Return:	Returns a negative value if something is wrong, the return
 *      value of the last operator if it was non-zero, or zero if all
 *      object headers were processed.
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Sep  6 2005
 *
 * Description:
 *      This function interates over the object headers of an object
 *  specified with 'ent' of type 'type_id'.  For each object header of the
 *  object, the 'op_data' and some additional information (specified below) are
 *  passed to the 'op' function.
 *      The operation receives a pointer to the object header message for the
 *  object being iterated over ('mesg'), and the pointer to the operator data
 *  passed in to H5O_iterate ('op_data').  The return values from an operator
 *  are:
 *      A. Zero causes the iterator to continue, returning zero when all
 *          object headers of that type have been processed.
 *      B. Positive causes the iterator to immediately return that positive
 *          value, indicating short-circuit success.
 *      C. Negative causes the iterator to immediately return that value,
 *          indicating failure.
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_iterate_real(const H5O_loc_t *loc, const H5O_msg_class_t *type, H5AC_protect_t prot,
    hbool_t internal, H5O_mesg_operator_t op, void *op_data, hid_t dxpl_id)
{
    H5O_t		*oh = NULL;     /* Pointer to actual object header */
    unsigned            oh_flags = H5AC__NO_FLAGS_SET;  /* Start iteration with no flags set on object header */
    unsigned		idx;            /* Absolute index of current message in all messages */
    unsigned		sequence;       /* Relative index of current message for messages of type */
    H5O_mesg_t         *idx_msg;        /* Pointer to current message */
    herr_t              ret_value = H5O_ITER_CONT;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_iterate_real)

    /* check args */
    HDassert(loc);
    HDassert(loc->file);
    HDassert(H5F_addr_defined(loc->addr));
    HDassert(type);
    HDassert(op.app_op);

    /* Protect the object header to iterate over */
    if (NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, prot)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header");

    /* Iterate over messages */
    for(sequence = 0, idx = 0, idx_msg = &oh->mesg[0]; idx < oh->nmesgs && !ret_value; idx++, idx_msg++) {
	if(type->id == idx_msg->type->id) {
            /*
             * Decode the message if necessary.  If the message is shared then decode
             * a shared message, ignoring the message type.
             */
            LOAD_NATIVE(loc->file, dxpl_id, idx_msg, FAIL)

            /* Check for making an "internal" (i.e. within the H5O package) callback */
            if(internal) {
                /* Call the "internal" iterator callback */
                if((ret_value = (op.lib_op)(idx_msg, sequence, &oh_flags, op_data)) != 0)
                    break;
            } /* end if */
            else {
                /* Call the iterator callback */
                if((ret_value = (op.app_op)(idx_msg->native, sequence, op_data)) != 0)
                    break;
            } /* end else */

            /* Check for error from iterator */
            if(ret_value < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "iterator function failed")

            /* Increment sequence value for message type */
            sequence++;
        } /* end if */
    } /* end for */

done:
    if(oh) {
        /* Check if object header was modified */
        if(oh_flags & H5AC__DIRTIED_FLAG) {
            /* Shouldn't be able to modify object header if we don't have write access */
            HDassert(prot == H5AC_WRITE);

            /* Try to condense object header info */
            if(H5O_condense_header(loc->file, oh, dxpl_id) < 0)
                HDONE_ERROR(H5E_OHDR, H5E_CANTPACK, FAIL, "can't pack object header")

            H5O_touch_oh(loc->file, dxpl_id, oh, FALSE, &oh_flags);
        } /* end if */

        if(H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, oh_flags) < 0)
            HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_iterate_real() */


/*-------------------------------------------------------------------------
 * Function:	H5O_obj_type
 *
 * Purpose:	Returns the type of object pointed to by `loc'.
 *
 * Return:	Success:	An object type defined in H5Gpublic.h
 *		Failure:	H5G_UNKNOWN
 *
 * Programmer:	Robb Matzke
 *              Wednesday, November  4, 1998
 *
 *-------------------------------------------------------------------------
 */
H5G_obj_t
H5O_obj_type(H5O_loc_t *loc, hid_t dxpl_id)
{
    H5O_t	*oh = NULL;                     /* Object header for location */
    H5G_obj_t   ret_value = H5G_UNKNOWN;        /* Return value */

    FUNC_ENTER_NOAPI(H5O_obj_type, H5G_UNKNOWN)

    /* Load the object header */
    if(NULL == (oh = H5AC_protect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, H5G_UNKNOWN, "unable to load object header")

    /* Test whether entry qualifies as a particular type of object */
    if((ret_value = H5O_obj_type_real(oh)) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, H5G_UNKNOWN, "unable to determine object type")

done:
    if(oh && H5AC_unprotect(loc->file, dxpl_id, H5AC_OHDR, loc->addr, oh, H5AC__NO_FLAGS_SET) != SUCCEED)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, H5G_UNKNOWN, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_obj_type() */


/*-------------------------------------------------------------------------
 * Function:	H5O_obj_type_real
 *
 * Purpose:	Returns the type of object pointed to by `oh'.
 *
 * Note:        Same algorithm as H5O_obj_class()
 *
 * Return:	Success:	An object type defined in H5Gpublic.h
 *		Failure:	H5G_UNKNOWN
 *
 * Programmer:	Quincey Koziol
 *              Monday, November 21, 2005
 *
 *-------------------------------------------------------------------------
 */
static H5G_obj_t
H5O_obj_type_real(H5O_t *oh)
{
    size_t	i;                              /* Local index variable */
    H5G_obj_t   ret_value = H5G_UNKNOWN;        /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_obj_type_real)

    /* Sanity check */
    HDassert(oh);

    /* Test whether entry qualifies as a particular type of object */
    /* (Note: loop is in reverse order, to test specific objects first) */
    for(i = NELMTS(H5O_obj_class_g); i > 0; --i) {
        htri_t	isa;            /* Is entry a particular type? */

	if((isa = (H5O_obj_class_g[i - 1]->isa)(oh)) < 0)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, H5G_UNKNOWN, "unable to determine object type")
	else if(isa)
	    HGOTO_DONE(H5O_obj_class_g[i - 1]->type)
    } /* end for */

    if(0 == i)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, H5G_UNKNOWN, "unable to determine object type")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_obj_type_real() */


/*-------------------------------------------------------------------------
 * Function:	H5O_obj_class
 *
 * Purpose:	Returns the class of object pointed to by `oh'.
 *
 * Note:        Same algorithm as H5O_obj_type_real()
 *
 * Return:	Success:	An object class
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Monday, November 21, 2005
 *
 *-------------------------------------------------------------------------
 */
static const H5O_obj_class_t *
H5O_obj_class(H5O_t *oh)
{
    size_t	i;                      /* Local index variable */
    const H5O_obj_class_t *ret_value;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_obj_class)

    /* Sanity check */
    HDassert(oh);

    /* Test whether entry qualifies as a particular type of object */
    /* (Note: loop is in reverse order, to test specific objects first) */
    for(i = NELMTS(H5O_obj_class_g); i > 0; --i) {
        htri_t	isa;            /* Is entry a particular type? */

	if((isa = (H5O_obj_class_g[i - 1]->isa)(oh)) < 0)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, NULL, "unable to determine object type")
	else if(isa)
	    HGOTO_DONE(H5O_obj_class_g[i - 1])
    } /* end for */

    if(0 == i)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, NULL, "unable to determine object type")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_obj_class() */


/*-------------------------------------------------------------------------
 * Function:	H5O_loc_reset
 *
 * Purpose:	Reset a object location to an empty state
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, September 19, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_loc_reset(H5O_loc_t *loc)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_loc_reset)

    /* Check arguments */
    HDassert(loc);

    /* Clear the object location to an empty state */
    HDmemset(loc, 0, sizeof(H5O_loc_t));
    loc->addr = HADDR_UNDEF;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_loc_reset() */


/*-------------------------------------------------------------------------
 * Function:    H5O_loc_copy
 *
 * Purpose:     Copy object location information
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              koziol@ncsa.uiuc.edu
 *              Monday, September 19, 2005
 *
 * Notes:       'depth' parameter determines how much of the group entry
 *              structure we want to copy.  The values are:
 *                  H5_COPY_SHALLOW - Copy all the field values from the source
 *                      to the destination, but not copying objects pointed to.
 *                      (Destination "takes ownership" of objects pointed to)
 *                  H5_COPY_DEEP - Copy all the fields from the source to
 *                      the destination, deep copying objects pointed to.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_loc_copy(H5O_loc_t *dst, const H5O_loc_t *src, H5_copy_depth_t depth)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_loc_copy)

    /* Check arguments */
    HDassert(src);
    HDassert(dst);
    HDassert(depth == H5_COPY_SHALLOW || depth == H5_COPY_DEEP);

    /* Copy the top level information */
    HDmemcpy(dst, src, sizeof(H5O_loc_t));

    /* Deep copy the names */
    if(depth == H5_COPY_DEEP) {
        /* If the original entry has holding open the file, this one should
         * hold it open, too.
         */
        if(src->holding_file)
            dst->file->nopen_objs++;
        ;
    } else if(depth == H5_COPY_SHALLOW) {
        /* Discarding 'const' qualifier OK - QAK */
        H5O_loc_reset((H5O_loc_t *)src);
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_loc_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5O_loc_hold_file
 *
 * Purpose:	Have this object header hold a file open until it is
 *              released.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	James Laird
 *              Wednesday, August 16, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_loc_hold_file(H5O_loc_t *loc)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_loc_hold_file)

    /* Check arguments */
    HDassert(loc);
    HDassert(loc->file);

    /* If this location is not already holding its file open, do so. */
    if(!loc->holding_file)
    {
        loc->file->nopen_objs++;
        loc->holding_file = TRUE;
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_loc_hold_file() */


/*-------------------------------------------------------------------------
 * Function:	H5O_loc_free
 *
 * Purpose:	Release resources used by this object header location.
 *              Not to be confused with H5O_close; this is used on
 *              locations that don't correspond to open objects.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	James Laird
 *              Wednesday, August 16, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_loc_free(H5O_loc_t *loc)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5O_loc_free)

    /* Check arguments */
    HDassert(loc);

    /* If this location is holding its file open try to close the file. */
    if(loc->holding_file)
    {
        loc->file->nopen_objs--;
        loc->holding_file = FALSE;
        if(loc->file->nopen_objs <= 0) {
          if(H5F_try_close(loc->file) < 0)
              HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "can't close file");
        }
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_loc_free() */


/*-------------------------------------------------------------------------
 * Function:    H5O_copy_mesg_file
 *
 * Purpose:     Copies a message to file.  If MESG is is the null pointer then a null
 *              pointer is returned with no error.
 *
 * Return:      Success:        Ptr to the new message
 *
 *              Failure:        NULL
 *
 * Programmer:  Peter Cao
 *              June 4, 2005
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_copy_mesg_file(const H5O_msg_class_t *type, H5F_t *file_src, void *native_src,
    H5F_t *file_dst, hid_t dxpl_id, H5O_copy_t *cpy_info, void *udata)
{
    void        *ret_value;

    FUNC_ENTER_NOAPI_NOINIT(H5O_copy_mesg_file)

    /* check args */
    HDassert(type);
    HDassert(type->copy_file);
    HDassert(file_src);
    HDassert(native_src);
    HDassert(file_dst);
    HDassert(cpy_info);

    if(NULL == (ret_value = (type->copy_file)(file_src, native_src, file_dst, dxpl_id, cpy_info, udata)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, NULL, "unable to copy object header message to file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_copy_mesg_file() */


/*-------------------------------------------------------------------------
 * Function:    H5O_copy_header_real
 *
 * Purpose:     copy header object from one location to another.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Peter Cao
 *              May 30, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_copy_header_real(const H5O_loc_t *oloc_src, H5O_loc_t *oloc_dst /*out */,
    hid_t dxpl_id, H5O_copy_t *cpy_info)
{
    H5O_addr_map_t         *addr_map = NULL;       /* Address mapping of object copied */
    H5O_t                  *oh_src = NULL;         /* Object header for source object */
    H5O_t                  *oh_dst = NULL;         /* Object header for destination object */
    unsigned               chunkno = 0, mesgno = 0;
    size_t                 hdr_size;
    haddr_t                addr_new = HADDR_UNDEF;
    H5O_mesg_t             *mesg_src;               /* Message in source object header */
    H5O_mesg_t             *mesg_dst;               /* Message in source object header */
    const H5O_msg_class_t  *copy_type;              /* Type of message to use for copying */
    const H5O_obj_class_t  *obj_class = NULL;       /* Type of object we are copying */
    void                   *udata = NULL;           /* User data for passing to message callbacks */
    herr_t                 ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5O_copy_header_real)

    HDassert(oloc_src);
    HDassert(oloc_src->file);
    HDassert(H5F_addr_defined(oloc_src->addr));
    HDassert(oloc_dst->file);
    HDassert(cpy_info);

    /* Get source object header */
    if(NULL == (oh_src = H5AC_protect(oloc_src->file, dxpl_id, H5AC_OHDR, oloc_src->addr, NULL, NULL, H5AC_READ)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* Get pointer to object class for this object */
    if(NULL == (obj_class = H5O_obj_class(oh_src)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to determine object type")

    /* Retrieve user data for particular type of object to copy */
    if(obj_class->get_copy_file_udata &&
            (NULL == (udata = (obj_class->get_copy_file_udata)())))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to retrieve copy user data")

    /* Flush any dirty messages in source object header to update the header chunks */
    if(H5O_flush_msgs(oloc_src->file, oh_src) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTFLUSH, FAIL, "unable to flush object header messages")

    /* get the size of the file header of the destination file */
    hdr_size = H5O_SIZEOF_HDR(oloc_dst->file);

    /* Allocate the destination object header and fill in header fields */
    if(NULL == (oh_dst = H5FL_MALLOC(H5O_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    /* Initialize header information */
    oh_dst->version = H5O_VERSION;
    oh_dst->nlink = 0;

    /* Initialize size of chunk array */
    oh_dst->alloc_nchunks = oh_dst->nchunks = oh_src->nchunks;

    /* Allocate memory for the chunk array */
    if(NULL == (oh_dst->chunk = H5FL_SEQ_MALLOC(H5O_chunk_t, oh_dst->alloc_nchunks)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    /* need to allocate all the chunks for the destination before copy the chunk message
       because continuation chunk message will need to know the chunk address of address of
       continuation block.
     */
    for(chunkno = 0; chunkno < oh_src->nchunks; chunkno++) {
        size_t chunk_size = oh_src->chunk[chunkno].size;

        /* '0th' chunk is preceded by object header prefix */
        if(0 == chunkno) {
            /* Allocate file space for the first chunk & object header prefix */
            if(HADDR_UNDEF == (addr_new = H5MF_alloc(oloc_dst->file, H5FD_MEM_OHDR, dxpl_id, (hsize_t)hdr_size + chunk_size)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "file allocation failed for object header")

            /* Set first chunk's address */
            oh_dst->chunk[0].addr = addr_new + (hsize_t)hdr_size;
        } /* end if */
        else {
            if(HADDR_UNDEF == (oh_dst->chunk[chunkno].addr = H5MF_alloc(oloc_dst->file, H5FD_MEM_OHDR, dxpl_id, (hsize_t)chunk_size)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "file allocation failed for object header")
        } /* end else */

        /* Create memory image for the new chunk */
        if(NULL == (oh_dst->chunk[chunkno].image = H5FL_BLK_MALLOC(chunk_image, chunk_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

        /* Copy the chunk image from source to destination in memory */
        /* (This copies over all the messages which don't require special
         * callbacks to fix them up.)
         */
        HDmemcpy(oh_dst->chunk[chunkno].image, oh_src->chunk[chunkno].image, chunk_size);

        /* Set dest. chunk information */
        oh_dst->chunk[chunkno].dirty = TRUE;
        oh_dst->chunk[chunkno].size = chunk_size;
    } /* end for */


    /* Initialize size of message list */
    oh_dst->alloc_nmesgs = oh_dst->nmesgs = oh_src->nmesgs;

    /* Allocate memory for message array */
    if(NULL == (oh_dst->mesg = H5FL_SEQ_CALLOC(H5O_mesg_t, oh_dst->alloc_nmesgs)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    /* Copy basic information about messages */
    HDmemcpy(oh_dst->mesg, oh_src->mesg, oh_dst->alloc_nmesgs * sizeof(H5O_mesg_t));

    /* "pre copy" pass over messages, to gather information for actual message copy operation */
    /* (for messages which depend on information from other messages) */
    /* (also resets various destination message information) */
    for(mesgno = 0; mesgno < oh_src->nmesgs; mesgno++) {
        /* Set up convenience variables */
        mesg_src = &(oh_src->mesg[mesgno]);
        mesg_dst = &(oh_dst->mesg[mesgno]);

        /* Sanity check */
        HDassert(!mesg_src->dirty);     /* Should be cleared by earlier call to flush messages */

        /* Fix up destination message pointers */
        mesg_dst->native = NULL;
        mesg_dst->raw = oh_dst->chunk[mesg_dst->chunkno].image + (mesg_src->raw - oh_src->chunk[mesg_src->chunkno].image);

        /* Check for shared message to operate on */
        if(mesg_src->flags & H5O_FLAG_SHARED)
            copy_type = H5O_MSG_SHARED;
        else
            copy_type = mesg_src->type;

        HDassert(copy_type);
        if(copy_type->pre_copy_file) {
            hbool_t deleted = FALSE;    /* Flag to indicate that the message should be deleted from the destination */

            /*
             * Decode the message if necessary.  If the message is shared then do
             * a shared message, ignoring the message type.
             */
            if(NULL == mesg_src->native) {
                /* Decode the message if necessary */
                HDassert(copy_type->decode);
                if(NULL == (mesg_src->native = (copy_type->decode)(oloc_src->file, dxpl_id, mesg_src->raw)))
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTDECODE, FAIL, "unable to decode a message")
            } /* end if (NULL == mesg_src->native) */

            /* Perform "pre copy" operation on messge */
            if((copy_type->pre_copy_file)(oloc_src->file, mesg_src->type, mesg_src->native, &deleted, cpy_info, udata) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to perform 'pre copy' operation on message")

            /* Check if the message should be deleted in the destination */
            if(deleted) {
                /* Change ID of destination message to NULL */
                mesg_dst->type = H5O_MSG_NULL;

                /* Clear message data to 0 */
                HDmemset(mesg_dst->raw, 0, mesg_dst->raw_size);

                /* Clear message flags */
                mesg_dst->flags = 0;

                /* Mark message as dirty */
                mesg_dst->dirty = TRUE;
            } /* end if */
        } /* end if */
    } /* end for */

    /* "copy" pass over messages, to perform main message copying */
    for(mesgno = 0; mesgno < oh_src->nmesgs; mesgno++) {
        /* Set up convenience variables */
        mesg_src = &(oh_src->mesg[mesgno]);
        mesg_dst = &(oh_dst->mesg[mesgno]);

        /* Check for shared message to operate on */
        /* (Use destination message, in case the message has been removed (i.e
         *      converted to a nil message) in the destination -QAK)
         */
        if (mesg_dst->flags & H5O_FLAG_SHARED)
            copy_type = H5O_MSG_SHARED;
        else
            copy_type = mesg_dst->type;

        /* copy this message into destination file */
        HDassert(copy_type);
        if(copy_type->copy_file) {
            /*
             * Decode the message if necessary.  If the message is shared then do
             * a shared message, ignoring the message type.
             */
            if(NULL == mesg_src->native) {
                /* Decode the message if necessary */
                HDassert(copy_type->decode);
                if(NULL == (mesg_src->native = (copy_type->decode)(oloc_src->file, dxpl_id, mesg_src->raw)))
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTDECODE, FAIL, "unable to decode a message")
            } /* end if (NULL == mesg_src->native) */

            /* Copy the source message */
            if(H5O_CONT_ID == mesg_src->type->id) {
                if((mesg_dst->native = H5O_copy_mesg_file(copy_type, oloc_src->file, mesg_src->native,
                        oloc_dst->file, dxpl_id, cpy_info, oh_dst->chunk)) == NULL)
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, FAIL, "unable to copy object header message")
             } /* end if */
             else {
                if((mesg_dst->native = H5O_copy_mesg_file(copy_type, oloc_src->file, mesg_src->native,
                        oloc_dst->file, dxpl_id, cpy_info, udata)) == NULL)
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, FAIL, "unable to copy object header message")
             } /* end else */

            /*
             * Encode the message.  If the message is shared then we
             * encode a Shared Object message instead of the object
             * which is being shared.
             */
            if((copy_type->encode)(oloc_dst->file, mesg_dst->raw, mesg_dst->native) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTENCODE, FAIL, "unable to encode object header message")
        } /* end if (mesg_src->type->copy_file) */
    } /* end of mesgno loop */


    /* Set the dest. object location to the first chunk address */
    HDassert(H5F_addr_defined(addr_new));
    oloc_dst->addr = addr_new;

    /* Allocate space for the address mapping of the object copied */
    if(NULL == (addr_map = H5FL_MALLOC(H5O_addr_map_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    /* Insert the address mapping for the new object into the copied list */
    /* (Do this here, because "post copy" possibly checks it) */
    addr_map->src_addr = oloc_src->addr;
    addr_map->dst_addr = oloc_dst->addr;
    addr_map->is_locked = TRUE;                 /* We've locked the object currently */
    addr_map->inc_ref_count = 0;                /* Start with no additional ref counts to add */

    if(H5SL_insert(cpy_info->map_list, addr_map, &(addr_map->src_addr)) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINSERT, FAIL, "can't insert object into skip list")

    /* "post copy" loop over messages, to fix up any messages which require a complete
     * object header for destination object
     */
    for(mesgno = 0; mesgno < oh_src->nmesgs; mesgno++) {
        /* Set up convenience variables */
        mesg_src = &(oh_src->mesg[mesgno]);
        mesg_dst = &(oh_dst->mesg[mesgno]);

        /* Check for shared message to operate on */
        /* (Use destination message, in case the message has been removed (i.e
         *      converted to a nil message) in the destination -QAK)
         */
        if(mesg_dst->flags & H5O_FLAG_SHARED)
            copy_type = H5O_MSG_SHARED;
        else
            copy_type = mesg_dst->type;

        HDassert(copy_type);
        if(copy_type->post_copy_file && mesg_src->native) {
            hbool_t modified = FALSE;

            /* Get destination message */
            HDassert(mesg_dst->type == mesg_src->type);

            /* Make certain the destination's native info is available */
            LOAD_NATIVE(oloc_dst->file, dxpl_id, mesg_dst, FAIL)

            /* Perform "post copy" operation on messge */
            if((copy_type->post_copy_file)(oloc_src, mesg_src->native, oloc_dst,
                    mesg_dst->native, &modified, dxpl_id, cpy_info) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to perform 'post copy' operation on message")

            /* Mark message and header as dirty if the destination message was modified */
            if(modified)
                mesg_dst->dirty = TRUE;
        } /* end if */
    } /* end for */

    /* Indicate that the destination address will no longer be locked */
    addr_map->is_locked = FALSE;

    /* Increment object header's reference count, if any descendents have created links to link to this object */
    if(addr_map->inc_ref_count) {
        H5_CHECK_OVERFLOW(addr_map->inc_ref_count, hsize_t, int);
        oh_dst->nlink += (int)addr_map->inc_ref_count;
    } /* end if */

    /* Insert destination object header in cache */
    if(H5AC_set(oloc_dst->file, dxpl_id, H5AC_OHDR, oloc_dst->addr, oh_dst, H5AC__DIRTIED_FLAG) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to cache object header")

done:
    /* Release pointer to source object header and it's derived objects */
    if(oh_src != NULL) {
        /* Unprotect the source object header */
        if(H5AC_unprotect(oloc_src->file, dxpl_id, H5AC_OHDR, oloc_src->addr, oh_src, H5AC__NO_FLAGS_SET) < 0)
            HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")
    } /* end if */

    /* Release pointer to destination object header */
    if(ret_value < 0 && oh_dst) {
        if(H5O_dest(oloc_dst->file, oh_dst) < 0)
	    HDONE_ERROR(H5E_OHDR, H5E_CANTFREE, FAIL, "unable to destroy object header data")
    } /* end if */

    /* Release user data for particular type of object to copy */
    if(udata) {
        HDassert(obj_class);
        HDassert(obj_class->free_copy_file_udata);
        (obj_class->free_copy_file_udata)(udata);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_copy_header_real() */


/*-------------------------------------------------------------------------
 * Function:    H5O_copy_header_map
 *
 * Purpose:     Copy header object from one location to another, detecting
 *              already mapped objects, etc.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              November 1, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_copy_header_map(const H5O_loc_t *oloc_src, H5O_loc_t *oloc_dst /*out */,
    hid_t dxpl_id, H5O_copy_t *cpy_info, hbool_t inc_depth)
{
    H5O_addr_map_t      *addr_map;              /* Address mapping of object copied */
    hbool_t             inc_link;               /* Whether to increment the link count for the object */
    herr_t             ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5O_copy_header_map, FAIL)

    /* Sanity check */
    HDassert(oloc_src);
    HDassert(oloc_dst);
    HDassert(oloc_dst->file);
    HDassert(cpy_info);

    /* Look up the address of the object to copy in the skip list */
    addr_map = (H5O_addr_map_t *)H5SL_search(cpy_info->map_list, &(oloc_src->addr));

    /* Check if address is already in list of objects copied */
    if(addr_map == NULL) {
        /* Copy object for the first time */

        /* Check for incrementing the depth of copy */
        /* (Can't do this for all copies, since shared datatypes should always be copied) */
        if(inc_depth)
            cpy_info->curr_depth++;

        /* Copy object referred to */
        if(H5O_copy_header_real(oloc_src, oloc_dst, dxpl_id, cpy_info) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, FAIL, "unable to copy object")

        /* Check for incrementing the depth of copy */
        if(inc_depth)
            cpy_info->curr_depth--;

        /* When an object is copied for the first time, increment it's link */
        inc_link = TRUE;

        /* indicate that a new object is created */
        ret_value++;
    } /* end if */
    else {
        /* Object has already been copied, set it's address in destination file */
        oloc_dst->addr = addr_map->dst_addr;

        /* If the object is locked currently (because we are copying a group
         * hierarchy and this is a link to a group higher in the hierarchy),
         * increment it's deferred reference count instead of incrementing the
         * reference count now.
         */
        if(addr_map->is_locked) {
            addr_map->inc_ref_count++;
            inc_link = FALSE;
        } /* end if */
        else
            inc_link = TRUE;
    } /* end else */

    /* Increment destination object's link count, if allowed */
    if(inc_link)
        if(H5O_link(oloc_dst, 1, dxpl_id) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to increment object link count")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_copy_header_map() */


/*--------------------------------------------------------------------------
 NAME
    H5O_copy_free_addrmap_cb
 PURPOSE
    Internal routine to free address maps from the skip list for copying objects
 USAGE
    herr_t H5O_copy_free_addrmap_cb(item, key, op_data)
        void *item;             IN/OUT: Pointer to addr
        void *key;              IN/OUT: (unused)
        void *op_data;          IN: (unused)
 RETURNS
    Returns zero on success, negative on failure.
 DESCRIPTION
        Releases the memory for the address.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5O_copy_free_addrmap_cb(void *item, void UNUSED *key, void UNUSED *op_data)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_copy_free_addrmap_cb)

    HDassert(item);

    /* Release the item */
    H5FL_FREE(H5O_addr_map_t, item);

    FUNC_LEAVE_NOAPI(0)
}   /* H5O_copy_free_addrmap_cb() */


/*-------------------------------------------------------------------------
 * Function:    H5O_copy_header
 *
 * Purpose:     copy header object from one location to another.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Peter Cao
 *              May 30, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_copy_header(const H5O_loc_t *oloc_src, H5O_loc_t *oloc_dst /*out */,
    hid_t dxpl_id, unsigned cpy_option)
{
    H5O_copy_t  cpy_info;               /* Information for copying object */
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5O_copy_header, FAIL)

    HDassert(oloc_src);
    HDassert(oloc_src->file);
    HDassert(H5F_addr_defined(oloc_src->addr));
    HDassert(oloc_dst->file);

    /* Convert copy flags into copy struct */
    HDmemset(&cpy_info, 0, sizeof(H5O_copy_t));
    if((cpy_option & H5G_COPY_SHALLOW_HIERARCHY_FLAG) > 0) {
        cpy_info.copy_shallow = TRUE;
        cpy_info.max_depth = 1;
    } /* end if */
    else
        cpy_info.max_depth = -1;        /* Current default is for full, recursive hier. copy */
    cpy_info.curr_depth = 0;
    if((cpy_option & H5G_COPY_EXPAND_SOFT_LINK_FLAG) > 0)
        cpy_info.expand_soft_link = TRUE;
    if((cpy_option & H5G_COPY_EXPAND_EXT_LINK_FLAG) > 0)
        cpy_info.expand_ext_link = TRUE;
    if((cpy_option & H5G_COPY_EXPAND_REFERENCE_FLAG) > 0)
        cpy_info.expand_ref = TRUE;
    if((cpy_option & H5G_COPY_WITHOUT_ATTR_FLAG) > 0)
        cpy_info.copy_without_attr = TRUE;

    /* Create a skip list to keep track of which objects are copied */
    if((cpy_info.map_list = H5SL_create(H5SL_TYPE_HADDR, 0.5, 16)) == NULL)
        HGOTO_ERROR(H5E_SLIST, H5E_CANTCREATE, FAIL, "cannot make skip list")

    /* copy the object from the source file to the destination file */
    if(H5O_copy_header_real(oloc_src, oloc_dst, dxpl_id, &cpy_info) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, FAIL, "unable to copy object")

done:
    if(cpy_info.map_list)
        H5SL_destroy(cpy_info.map_list, H5O_copy_free_addrmap_cb, NULL);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_copy_header() */


/*-------------------------------------------------------------------------
 * Function:    H5O_copy_obj_by_ref
 *
 * Purpose:     Copy the object pointed by _src_ref.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Peter Cao       
 *              Aug 7 2006 
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_copy_obj_by_ref(H5O_loc_t *src_oloc, hid_t dxpl_id, H5O_loc_t *dst_oloc,
    H5G_loc_t *dst_root_loc, H5O_copy_t *cpy_info)
{
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5O_copy_obj_by_ref, FAIL)

    HDassert(src_oloc);
    HDassert(dst_oloc);

    /* Perform the copy, or look up existing copy */
    if((ret_value = H5O_copy_header_map(src_oloc, dst_oloc, dxpl_id, cpy_info, FALSE)) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, FAIL, "unable to copy object")

    /* Check if a new valid object is copied to the destination */
    if(H5F_addr_defined(dst_oloc->addr) && (ret_value > SUCCEED)) {
        char    tmp_obj_name[80];
        H5G_name_t      new_path;
        H5O_loc_t       new_oloc;
        H5G_loc_t       new_loc;

        /* Set up group location for new object */
        new_loc.oloc = &new_oloc;
        new_loc.path = &new_path;
        H5G_loc_reset(&new_loc);
        new_oloc.file = dst_oloc->file;
        new_oloc.addr = dst_oloc->addr;

        /* Pick a default name for the new object */
        sprintf(tmp_obj_name, "~obj_pointed_by_%llu", (unsigned long_long)dst_oloc->addr);

        /* Create a link to the newly copied object */
        if(H5L_link(dst_root_loc, tmp_obj_name, &new_loc, H5P_DEFAULT, H5P_DEFAULT, dxpl_id) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to insert link")

        H5G_loc_free(&new_loc);
    } /* if (H5F_addr_defined(dst_oloc.addr)) */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_copy_obj_by_ref() */


/*-------------------------------------------------------------------------
 * Function:	H5O_copy_expand_ref
 *
 * Purpose:	Copy the object pointed by _src_ref.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Peter Cao	
 *		Aug 7 2006 
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_copy_expand_ref(H5F_t *file_src, void *_src_ref, hid_t dxpl_id,
    H5F_t *file_dst, void *_dst_ref, size_t ref_count, H5R_type_t ref_type,
    H5O_copy_t *cpy_info)
{
    H5O_loc_t 	dst_oloc;         	/* Copied object object location */
    H5O_loc_t	src_oloc;          	/* Temporary object location for source object */
    H5G_loc_t   dst_root_loc;           /* The location of root group of the destination file */
    uint8_t     *p;                     /* Pointer to OID to store */
    size_t      i;                      /* Local index variable */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5O_copy_expand_ref, FAIL)

    /* Sanity checks */
    HDassert(file_src);
    HDassert(_src_ref);
    HDassert(file_dst);
    HDassert(_dst_ref);
    HDassert(ref_count);
    HDassert(cpy_info);

    /* Initialize object locations */
    H5O_loc_reset(&src_oloc);
    H5O_loc_reset(&dst_oloc);
    src_oloc.file = file_src;
    dst_oloc.file = file_dst;

    /* Set up the root group in the destination file */
    if(NULL == (dst_root_loc.oloc = H5G_oloc(H5G_rootof(file_dst))))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get object location for root group")
    if(NULL == (dst_root_loc.path = H5G_nameof(H5G_rootof(file_dst))))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get path for root group")

    /* Copy object references */
    if(H5R_OBJECT == ref_type) {
        hobj_ref_t *src_ref = (hobj_ref_t *)_src_ref;
        hobj_ref_t *dst_ref = (hobj_ref_t *)_dst_ref;

        /* Making equivalent references in the destination file */
        for(i = 0; i < ref_count; i++) {
            /* Set up for the object copy for the reference */
            p = (uint8_t *)(&src_ref[i]);
            H5F_addr_decode(src_oloc.file, (const uint8_t **)&p, &(src_oloc.addr));
            dst_oloc.addr = HADDR_UNDEF;

            /* Attempt to copy object from source to destination file */
            if(H5O_copy_obj_by_ref(&src_oloc, dxpl_id, &dst_oloc, &dst_root_loc, cpy_info) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, FAIL, "unable to copy object")

            /* Set the object reference info for the destination file */
            p = (uint8_t *)(&dst_ref[i]);
            H5F_addr_encode(dst_oloc.file, &p, dst_oloc.addr);
	} /* end for */
    }  /* end if */
    /* Copy region references */
    else if(H5R_DATASET_REGION == ref_type) {
        hdset_reg_ref_t *src_ref = (hdset_reg_ref_t *)_src_ref;
        hdset_reg_ref_t *dst_ref = (hdset_reg_ref_t *)_dst_ref;
        uint8_t *buf;           /* Buffer to store serialized selection in */
        H5HG_t hobjid;          /* Heap object ID */
        size_t buf_size;        /* Length of object in heap */
  
        /* Making equivalent references in the destination file */
        for(i = 0; i < ref_count; i++) {
            /* Get the heap ID for the dataset region */
            p = (uint8_t *)(&src_ref[i]);
            H5F_addr_decode(src_oloc.file, (const uint8_t **)&p, &(hobjid.addr));
            INT32DECODE(p, hobjid.idx);

            /* Get the dataset region from the heap (allocate inside routine) */
            if((buf = H5HG_read(src_oloc.file, dxpl_id, &hobjid, NULL, &buf_size)) == NULL)
                HGOTO_ERROR(H5E_REFERENCE, H5E_READERROR, FAIL, "Unable to read dataset region information")

            /* Get the object oid for the dataset */
            p = (uint8_t *)buf;
            H5F_addr_decode(src_oloc.file, (const uint8_t **)&p, &(src_oloc.addr));
            dst_oloc.addr = HADDR_UNDEF;

            /* copy the object pointed by the ref to the destination */
            if(H5O_copy_obj_by_ref(&src_oloc, dxpl_id, &dst_oloc, &dst_root_loc, cpy_info) < 0) {
                H5MM_xfree(buf);
                HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, FAIL, "unable to copy object")
            } /* end if */

            /* Serialize object ID */
            p = (uint8_t *)buf;
            H5F_addr_encode(dst_oloc.file, &p, dst_oloc.addr);

            /* Save the serialized buffer to the destination */
            if(H5HG_insert(dst_oloc.file, dxpl_id, buf_size, buf, &hobjid) < 0) {
                H5MM_xfree(buf);
                HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, FAIL, "Unable to write dataset region information")
            } /* end if */

            /* Set the dataset region reference info for the destination file */
            p = (uint8_t *)(&dst_ref[i]);
            H5F_addr_encode(dst_oloc.file, &p, hobjid.addr);
            INT32ENCODE(p, hobjid.idx);

            /* Free the buffer allocated in H5HG_read() */
            H5MM_xfree(buf);
        } /* end for */
    } /* end if */
    else
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_copy_expand_ref() */


/*-------------------------------------------------------------------------
 * Function:	H5O_debug_id
 *
 * Purpose:	Act as a proxy for calling the 'debug' method for a
 *              particular class of object header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 13 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_debug_id(unsigned type_id, H5F_t *f, hid_t dxpl_id, const void *mesg, FILE *stream, int indent, int fwidth)
{
    const H5O_msg_class_t *type;            /* Actual H5O class type for the ID */
    herr_t      ret_value;       /* Return value */

    FUNC_ENTER_NOAPI(H5O_debug_id,FAIL);

    /* Check args */
    assert(type_id < NELMTS(H5O_msg_class_g));
    type=H5O_msg_class_g[type_id];    /* map the type ID to the actual type object */
    assert(type);
    assert(type->debug);
    assert(f);
    assert(mesg);
    assert(stream);
    assert(indent >= 0);
    assert(fwidth >= 0);

    /* Call the debug method in the class */
    if ((ret_value = (type->debug)(f, dxpl_id, mesg, stream, indent, fwidth)) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_BADTYPE, FAIL, "unable to debug message");

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5O_debug_id() */


/*-------------------------------------------------------------------------
 * Function:	H5O_debug_real
 *
 * Purpose:	Prints debugging info about an object header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  6 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_debug_real(H5F_t *f, hid_t dxpl_id, H5O_t *oh, haddr_t addr, FILE *stream, int indent, int fwidth)
{
    unsigned	i, chunkno;
    size_t	mesg_total = 0, chunk_total = 0;
    int		*sequence;
    haddr_t	tmp_addr;
    void	*(*decode)(H5F_t*, hid_t, const uint8_t*);
    herr_t      (*debug)(H5F_t*, hid_t, const void*, FILE*, int, int)=NULL;
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5O_debug_real, FAIL)

    /* check args */
    HDassert(f);
    HDassert(oh);
    HDassert(H5F_addr_defined(addr));
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    /* debug */
    HDfprintf(stream, "%*sObject Header...\n", indent, "");

    HDfprintf(stream, "%*s%-*s %d\n", indent, "", fwidth,
	      "Dirty:",
	      (int) (oh->cache_info.is_dirty));
    HDfprintf(stream, "%*s%-*s %d\n", indent, "", fwidth,
	      "Version:",
	      (int) (oh->version));
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Header size (in bytes):",
	      (unsigned) H5O_SIZEOF_HDR(f));
    HDfprintf(stream, "%*s%-*s %d\n", indent, "", fwidth,
	      "Number of links:",
	      (int) (oh->nlink));
    HDfprintf(stream, "%*s%-*s %u (%u)\n", indent, "", fwidth,
	      "Number of messages (allocated):",
	      (unsigned) (oh->nmesgs), (unsigned) (oh->alloc_nmesgs));
    HDfprintf(stream, "%*s%-*s %u (%u)\n", indent, "", fwidth,
	      "Number of chunks (allocated):",
	      (unsigned) (oh->nchunks), (unsigned) (oh->alloc_nchunks));

    /* debug each chunk */
    for (i=0, chunk_total=0; i < oh->nchunks; i++) {
	chunk_total += oh->chunk[i].size;
	HDfprintf(stream, "%*sChunk %d...\n", indent, "", i);

	HDfprintf(stream, "%*s%-*s %d\n", indent + 3, "", MAX(0, fwidth - 3),
		  "Dirty:",
		  (int) (oh->chunk[i].dirty));

	HDfprintf(stream, "%*s%-*s %a\n", indent + 3, "", MAX(0, fwidth - 3),
		  "Address:", oh->chunk[i].addr);

	tmp_addr = addr + (hsize_t)H5O_SIZEOF_HDR(f);
	if (0 == i && H5F_addr_ne(oh->chunk[i].addr, tmp_addr))
	    HDfprintf(stream, "*** WRONG ADDRESS!\n");
	HDfprintf(stream, "%*s%-*s %lu\n", indent + 3, "", MAX(0, fwidth - 3),
		  "Size in bytes:",
		  (unsigned long) (oh->chunk[i].size));
    }

    /* debug each message */
    if (NULL==(sequence = H5MM_calloc(NELMTS(H5O_msg_class_g)*sizeof(int))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    for (i=0, mesg_total=0; i < oh->nmesgs; i++) {
	mesg_total += H5O_SIZEOF_MSGHDR(f) + oh->mesg[i].raw_size;
	HDfprintf(stream, "%*sMessage %d...\n", indent, "", i);

	/* check for bad message id */
	if (oh->mesg[i].type->id >= (int)NELMTS(H5O_msg_class_g)) {
	    HDfprintf(stream, "*** BAD MESSAGE ID 0x%04x\n",
		      oh->mesg[i].type->id);
	    continue;
	}

	/* message name and size */
	HDfprintf(stream, "%*s%-*s 0x%04x `%s' (%d)\n",
		  indent + 3, "", MAX(0, fwidth - 3),
		  "Message ID (sequence number):",
		  (unsigned) (oh->mesg[i].type->id),
		  oh->mesg[i].type->name,
		  sequence[oh->mesg[i].type->id]++);
	HDfprintf (stream, "%*s%-*s %d\n", indent+3, "", MAX (0, fwidth-3),
		   "Dirty:",
		   (int)(oh->mesg[i].dirty));
	HDfprintf (stream, "%*s%-*s %s\n", indent+3, "", MAX (0, fwidth-3),
		   "Shared:",
		   (oh->mesg[i].flags & H5O_FLAG_SHARED) ? "Yes" : "No");
	HDfprintf(stream, "%*s%-*s %s\n", indent + 3, "", MAX(0, fwidth - 3),
		  "Constant:",
		  (oh->mesg[i].flags & H5O_FLAG_CONSTANT) ? "Yes" : "No");
	if (oh->mesg[i].flags & ~H5O_FLAG_BITS) {
	    HDfprintf (stream, "%*s%-*s 0x%02x\n", indent+3,"",MAX(0,fwidth-3),
		       "*** ADDITIONAL UNKNOWN FLAGS --->",
		       oh->mesg[i].flags & ~H5O_FLAG_BITS);
	}
	HDfprintf(stream, "%*s%-*s %lu bytes\n", indent+3, "", MAX(0,fwidth-3),
		  "Raw size in obj header:",
		  (unsigned long) (oh->mesg[i].raw_size));
	HDfprintf(stream, "%*s%-*s %d\n", indent + 3, "", MAX(0, fwidth - 3),
		  "Chunk number:",
		  (int) (oh->mesg[i].chunkno));
	chunkno = oh->mesg[i].chunkno;
	if (chunkno >= oh->nchunks)
	    HDfprintf(stream, "*** BAD CHUNK NUMBER\n");
	HDfprintf(stream, "%*s%-*s %u\n", indent + 3, "", MAX(0, fwidth - 3),
		  "Raw data offset in chunk:",
		  (unsigned) (oh->mesg[i].raw - oh->chunk[chunkno].image));

	/* check the size */
	if ((oh->mesg[i].raw + oh->mesg[i].raw_size >
                 oh->chunk[chunkno].image + oh->chunk[chunkno].size) ||
                (oh->mesg[i].raw < oh->chunk[chunkno].image)) {
	    HDfprintf(stream, "*** BAD MESSAGE RAW ADDRESS\n");
	}

	/* decode the message */
	if (oh->mesg[i].flags & H5O_FLAG_SHARED) {
	    decode = H5O_MSG_SHARED->decode;
	    debug = H5O_MSG_SHARED->debug;
	} else {
	    decode = oh->mesg[i].type->decode;
	    debug = oh->mesg[i].type->debug;
	}
	if (NULL==oh->mesg[i].native && decode)
	    oh->mesg[i].native = (decode)(f, dxpl_id, oh->mesg[i].raw);
	if (NULL==oh->mesg[i].native)
	    debug = NULL;

	/* print the message */
	HDfprintf(stream, "%*s%-*s\n", indent + 3, "", MAX(0, fwidth - 3),
		  "Message Information:");
	if (debug)
	    (debug)(f, dxpl_id, oh->mesg[i].native, stream, indent+6, MAX(0, fwidth-6));
	else
	    HDfprintf(stream, "%*s<No info for this message>\n", indent + 6, "");

	/* If the message is shared then also print the pointed-to message */
	if (oh->mesg[i].flags & H5O_FLAG_SHARED) {
	    H5O_shared_t *shared = (H5O_shared_t*)(oh->mesg[i].native);
	    void *mesg = NULL;

            mesg = H5O_read_real(&(shared->oloc), oh->mesg[i].type, 0, NULL, dxpl_id);
	    if (oh->mesg[i].type->debug)
		(oh->mesg[i].type->debug)(f, dxpl_id, mesg, stream, indent+3, MAX (0, fwidth-3));
	    H5O_free_real(oh->mesg[i].type, mesg);
	}
    }
    sequence = H5MM_xfree(sequence);

    if (mesg_total != chunk_total)
	HDfprintf(stream, "*** TOTAL SIZE DOES NOT MATCH ALLOCATED SIZE!\n");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_debug_real() */


/*-------------------------------------------------------------------------
 * Function:	H5O_debug
 *
 * Purpose:	Prints debugging info about an object header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  6 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr, FILE *stream, int indent, int fwidth)
{
    H5O_t	*oh = NULL;
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5O_debug, FAIL)

    /* check args */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    if(NULL == (oh = H5AC_protect(f, dxpl_id, H5AC_OHDR, addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* debug */
    H5O_debug_real(f, dxpl_id, oh, addr, stream, indent, fwidth);

done:
    if(oh && H5AC_unprotect(f, dxpl_id, H5AC_OHDR, addr, oh, H5AC__NO_FLAGS_SET) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_debug() */

