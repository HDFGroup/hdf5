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

#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */
#define H5G_PACKAGE		/*suppress error about including H5Gpkg   */
#define H5L_PACKAGE		/*suppress error about including H5Lpkg   */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5L_init_interface

#include "H5private.h"          /* Generic Functions                    */
#include "H5Lpkg.h"             /* Links                                */
#include "H5Fpkg.h"             /* File access                          */
#include "H5Eprivate.h"         /* Error handling                       */
#include "H5Iprivate.h"         /* IDs                                  */
#include "H5MMprivate.h"        /* Memory management                    */
#include "H5Oprivate.h"         /* File objects                         */
#include "H5Dprivate.h"         /* Datasets                             */
#include "H5Pprivate.h"         /* Property lists                       */
#include "H5Gpkg.h"             /* Groups                               */

/* Local typedefs */
#define H5L_MOVE_OP 1
#define H5L_RENAME_OP 2

/* User data for path traversal routine for getting link metadata */
typedef struct {
    H5L_linkinfo_t  *linfo;                  /* Buffer to return to user */
    hid_t           dxpl_id;                 /* dxpl to use in callback */
} H5L_trav_ud1_t;

/* User data for path traversal callback to creating a link */
typedef struct {
    H5F_t *file;                              /* Pointer to the file */
    hid_t dxpl_id;                            /* Dataset transfer property list */
    hid_t lcpl_id;                            /* Link creation property list */
    H5G_name_t *path;                         /* Path to object being linked */
    H5O_link_t *lnk;                          /* Pointer to link information to insert */
} H5L_trav_ud3_t;

/* User data for path traversal routine for moving and renaming a link */
typedef struct {
    const char *dst_name;                     /* Destination name for moving object */
    H5T_cset_t cset;                          /* Char set for new name */
    H5G_loc_t  *dst_loc;		      /* Destination location for moving object */
    hbool_t copy;                             /* TRUE if this is a copy operation */
    hid_t lapl_id;                            /* lapl to use in callback */
    hid_t dxpl_id;                            /* dxpl to use in callback */
} H5L_trav_ud4_t;

/* User data for path traversal routine for getting soft link value */
typedef struct {
    size_t size;                              /* Size of user buffer */
    char *buf;                                /* User buffer */
} H5L_trav_ud5_t;

/* User data for path traversal routine for removing link (i.e. unlink) */
typedef struct {
    hid_t dxpl_id;                            /* Dataset transfer property list */
} H5L_trav_ud6_t;

/* User data for path traversal routine for retrieving link creation property list */
typedef struct {
    H5P_genplist_t *lcpl;                     /* Copy of default property list to be set */
} H5L_trav_ud8_t;

/* User data for path traversal routine for moving and renaming an object */
typedef struct {
    H5F_t *file;                              /* Pointer to the file */
    H5O_link_t *lnk;                          /* Pointer to link information to insert */
    hbool_t copy;                             /* TRUE if this is a copy operation */
    hid_t dxpl_id;                            /* Dataset transfer property list */
} H5L_trav_ud10_t;

/* Package variables */

/* Local variables */

/* Private prototypes */
static herr_t H5L_link_cb(H5G_loc_t *grp_loc/*in*/, const char *name,
    const H5O_link_t *lnk, H5G_loc_t *obj_loc, void *_udata/*in,out*/,
    H5G_own_loc_t *own_loc/*out*/);
static herr_t H5L_create_real(H5G_loc_t *link_loc, const char *link_name,
    H5G_name_t *obj_path, H5F_t *obj_file, H5O_link_t *lnk, hid_t lcpl_id,
    hid_t lapl_id, hid_t dxpl_id);
static herr_t H5L_create_hard(H5G_loc_t *cur_loc, const char *cur_name,
    H5G_loc_t *link_loc, const char *link_name, hid_t lcpl_id,
    hid_t lapl_id, hid_t dxpl_id);
static herr_t H5L_create_soft(const char *target_path, H5G_loc_t *cur_loc,
    const char *cur_name, hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id);
static herr_t H5L_linkval_cb(H5G_loc_t *grp_loc/*in*/, const char *name,
    const H5O_link_t *lnk, H5G_loc_t *obj_loc, void *_udata/*in,out*/,
    H5G_own_loc_t *own_loc/*out*/);
static herr_t H5L_linkval(H5G_loc_t *loc, const char *name, size_t size,
    char *buf/*out*/, hid_t lapl_id, hid_t dxpl_id);
static herr_t H5L_unlink_cb(H5G_loc_t *grp_loc/*in*/, const char *name,
    const H5O_link_t *lnk, H5G_loc_t *obj_loc, void *_udata/*in,out*/,
    H5G_own_loc_t *own_loc/*out*/);
static herr_t H5L_unlink(H5G_loc_t *loc, const char *name, hid_t lapl_id,
    hid_t dxpl_id);
static herr_t H5L_move(H5G_loc_t *src_loc, const char *src_name,
    H5G_loc_t *dst_loc, const char *dst_name, hbool_t copy_flag,
    hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id);
static herr_t H5L_move_cb(H5G_loc_t *grp_loc/*in*/, const char *name,
    const H5O_link_t *lnk, H5G_loc_t *obj_loc, void *_udata/*in,out*/,
    H5G_own_loc_t *own_loc/*out*/);
static herr_t H5L_move_dest_cb(H5G_loc_t *grp_loc/*in*/,
    const char *name, const H5O_link_t *lnk, H5G_loc_t *obj_loc, void *_udata/*in,out*/,
    H5G_own_loc_t *own_loc/*out*/);
static herr_t H5L_get_linfo_cb(H5G_loc_t UNUSED *grp_loc/*in*/, const char UNUSED *name,
    const H5O_link_t *lnk, H5G_loc_t UNUSED *obj_loc, void *_udata/*in,out*/,
    H5G_own_loc_t *own_loc/*out*/);
static int H5L_find_class_idx(H5L_link_t id);


/* Information about user-defined links */
static size_t             H5L_table_alloc_g = 0;
static size_t             H5L_table_used_g = 0;
static H5L_link_class_t  *H5L_table_g = NULL;



#define H5L_MIN_TABLE_SIZE 32 /* Minimum size of the user-defined link type table if it is allocated */



/*-------------------------------------------------------------------------
 * Function:	H5L_init
 *
 * Purpose:	Initialize the interface from some other package.
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	James Laird
 *              Thursday, July 13, 2006
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5L_init(void)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5L_init, FAIL);
    /* FUNC_ENTER() does all the work */

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Lregister
 *
 * Purpose:	Registers a class of user-defined links, or changes the
 *              behavior of an existing class.
 *
 *              The link class passed in will override any existing link
 *              class for the specified link class ID. It must at least
 *              include a H5L_link_class_t version (which should be
 *              H5L_LINK_CLASS_T_VERS), a link class ID, and a traversal
 *              function.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Monday, July 10, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Lregister(const H5L_link_class_t *cls)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Lregister, FAIL)
    H5TRACE1("e","*x",cls);

    /* Check args */
    if (cls==NULL)
	HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "invalid link class")

    /* Check H5L_link_class_t version number; this is where a function to convert
     * from an outdated version should be called.
     */
    if(cls->version != H5L_LINK_CLASS_T_VERS)
      HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid H5L_link_class_t version number");

    if (cls->id<H5L_LINK_UD_MIN || cls->id>H5L_LINK_MAX)
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "invalid link identification number")
    if (cls->trav_func==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no traversal function specified")

    /* Do it */
    if (H5L_register (cls)<0)
        HGOTO_ERROR (H5E_LINK, H5E_NOTREGISTERED, FAIL, "unable to register link type")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5L_find_class_idx
 *
 * Purpose:	Given a link class ID, return the offset in the global array
 *              that holds all the registered link classes.
 *
 * Return:	Success:	Non-negative index of entry in global
 *                              link class table.
 *		Failure:	Negative
 *
 * Programmer:	James Laird
 *              Monday, July 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
H5L_find_class_idx(H5L_link_t id)
{
    size_t i;                   /* Local index variable */
    int ret_value=FAIL;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5L_find_class_idx)

    for (i=0; i<H5L_table_used_g; i++)
	if (H5L_table_g[i].id == id)
	    HGOTO_DONE((int)i)

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5L_find_class_idx */


/*-------------------------------------------------------------------------
 * Function:	H5L_find_class
 *
 * Purpose:	Given a link class ID return a pointer to a global struct that
 *		defines the link class.
 *
 * Return:	Success:	Ptr to entry in global link class table.
 *		Failure:	NULL
 *
 * Programmer:	James Laird
 *              Monday, July 10, 2006
 *
 *-------------------------------------------------------------------------
 */
const H5L_link_class_t *
H5L_find_class(H5L_link_t id)
{
    int	idx;                            /* Filter index in global table */
    H5L_link_class_t *ret_value=NULL;   /* Return value */

    FUNC_ENTER_NOAPI(H5L_find_class, NULL)

    /* Get the index in the global table */
    if((idx=H5L_find_class_idx(id))<0)
        HGOTO_ERROR(H5E_LINK, H5E_NOTREGISTERED, NULL, "unable to find link class")

    /* Set return value */
    ret_value=H5L_table_g+idx;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5L_find_class */


/*-------------------------------------------------------------------------
 * Function:	H5L_init_interface
 *
 * Purpose:	Initialize information specific to H5L interface.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Tuesday, January 24, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5L_init_interface(void)
{
    H5P_genclass_t  *crt_pclass;
    H5P_genclass_t  *acc_pclass;
    size_t          nprops;                 /* Number of properties */
    unsigned        intmd_group = H5L_CRT_INTERMEDIATE_GROUP_DEF;
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5L_init_interface)

    /* =========Link Creation Property Class Initialization========= */
    /* Register the default attribute creation properties */
    assert(H5P_CLS_LINK_CREATE_g!=(-1));

    /* Get the pointer to the link creation class */
    if (NULL == (crt_pclass = H5I_object(H5P_CLS_LINK_CREATE_g)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list class")

    /* Get the number of properties in the class */
    if(H5P_get_nprops_pclass(crt_pclass,&nprops,FALSE)<0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "can't query number of properties")

    /* Assume that if there are properties in the class, they are the default ones */
    if(nprops==0) {
        /* Register create intermediate groups property */
        if(H5P_register(crt_pclass,H5L_CRT_INTERMEDIATE_GROUP_NAME,H5L_CRT_INTERMEDIATE_GROUP_SIZE,
                 &intmd_group,NULL,NULL,NULL,NULL,NULL,NULL,NULL)<0)
             HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")
    }

    /* Only register the default property list if it hasn't been created yet */
    if(H5P_LST_LINK_CREATE_g==(-1)) {
        /* Register the default link creation property list */
        if ((H5P_LST_LINK_CREATE_g = H5P_create_id (crt_pclass))<0)
            HGOTO_ERROR (H5E_PLIST, H5E_CANTREGISTER, FAIL, "can't register default property list")
    } /* end if */

    /* ========== Link Access Property Class Initialization ============*/
    assert(H5P_CLS_LINK_ACCESS_g!=-1);

    /* Get the pointer to link access class */
    if(NULL == (acc_pclass = H5I_object(H5P_CLS_LINK_ACCESS_g)))
         HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list class")

    /* Only register the default property list if it hasn't been created yet */
    if(H5P_LST_LINK_ACCESS_g == (-1)) {
        /* Register the default link access property list */
        if((H5P_LST_LINK_ACCESS_g = H5P_create_id(acc_pclass))<0)
             HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "can't register default LAPL")
    } /* end if */


    /* Initialize user-defined link classes */
    if(H5L_register_external() <0)
        HGOTO_ERROR(H5E_LINK, H5E_NOTREGISTERED, FAIL, "unable to register external link class")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5L_term_interface
 *
 * Purpose:	Terminate any resources allocated in H5L_init_interface.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Tuesday, January 24, 2006
 *
 *-------------------------------------------------------------------------
 */
int
H5L_term_interface(void)
{
    int	n=0;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5L_term_interface)

    /* Free the table of link types */
    H5L_table_g = H5MM_xfree(H5L_table_g);
    H5L_table_used_g = H5L_table_alloc_g = 0;

    FUNC_LEAVE_NOAPI(n)
}

/*-------------------------------------------------------------------------
 * Function:	H5Lmove
 *
 * Purpose:	Renames an object within an HDF5 file and moves it to a new
 *              group.  The original name SRC is unlinked from the group graph
 *              and the inserted with the new name DST (which can specify a
 *              new path for the object) as an atomic operation. The names
 *              are interpreted relative to SRC_LOC_ID and
 *              DST_LOC_ID, which are either file IDs or group ID.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Wednesday, March 29, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Lmove(hid_t src_loc_id, const char *src_name, hid_t dst_loc_id,
              const char *dst_name, hid_t lcpl_id, hid_t lapl_id)
{
    H5G_loc_t	src_loc, *src_loc_p;
    H5G_loc_t	dst_loc, *dst_loc_p;
    herr_t      ret_value=SUCCEED;              /* Return value */

    FUNC_ENTER_API(H5Lmove, FAIL)
    H5TRACE6("e","isisii",src_loc_id,src_name,dst_loc_id,dst_name,lcpl_id,
             lapl_id);

    /* Check arguments */
    if(src_loc_id != H5L_SAME_LOC && H5G_loc(src_loc_id, &src_loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(dst_loc_id != H5L_SAME_LOC && H5G_loc(dst_loc_id, &dst_loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!src_name || !*src_name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no current name specified")
    if(!dst_name || !*dst_name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no destination name specified")

    /* Set up src & dst location pointers */
    src_loc_p = &src_loc;
    dst_loc_p = &dst_loc;
    if(src_loc_id == H5L_SAME_LOC && dst_loc_id == H5L_SAME_LOC)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "source and destination should not both be H5L_SAME_LOC")
    else if(src_loc_id == H5L_SAME_LOC)
        src_loc_p = dst_loc_p;
    else if(dst_loc_id == H5L_SAME_LOC)
        dst_loc_p = src_loc_p;

    if(lcpl_id != H5P_DEFAULT && (TRUE != H5P_isa_class(lcpl_id, H5P_LINK_CREATE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a link creation property list")

    if(H5L_move(src_loc_p, src_name, dst_loc_p, dst_name,
                                FALSE, lcpl_id, lapl_id, H5AC_dxpl_id) < 0)
	HGOTO_ERROR(H5E_LINK, H5E_CANTMOVE, FAIL, "unable to move link")

done:    
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Lcopy
 *
 * Purpose:	Creates an identical copy of a link with the same creation
 *              time and target.  The new link can have a different name
 *              and be in a different location than the original.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Wednesday, March 29, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Lcopy(hid_t src_loc_id, const char *src_name, hid_t dst_loc_id,
              const char *dst_name, hid_t lcpl_id, hid_t lapl_id)
{
    H5G_loc_t	src_loc, *src_loc_p;
    H5G_loc_t	dst_loc, *dst_loc_p;
    herr_t      ret_value=SUCCEED;              /* Return value */

    FUNC_ENTER_API(H5Lcopy, FAIL)
    H5TRACE6("e","isisii",src_loc_id,src_name,dst_loc_id,dst_name,lcpl_id,
             lapl_id);

    /* Check arguments */
    if(src_loc_id != H5L_SAME_LOC && H5G_loc(src_loc_id, &src_loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(dst_loc_id != H5L_SAME_LOC && H5G_loc(dst_loc_id, &dst_loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!src_name || !*src_name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no current name specified")
    if(!dst_name || !*dst_name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no destination name specified")

    /* Set up src & dst location pointers */
    src_loc_p = &src_loc;
    dst_loc_p = &dst_loc;
    if(src_loc_id == H5L_SAME_LOC && dst_loc_id == H5L_SAME_LOC)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "source and destination should not both be H5L_SAME_LOC")
    else if(src_loc_id == H5L_SAME_LOC)
        src_loc_p = dst_loc_p;
    else if(dst_loc_id == H5L_SAME_LOC)
        dst_loc_p = src_loc_p;

    if(lcpl_id != H5P_DEFAULT && (TRUE != H5P_isa_class(lcpl_id, H5P_LINK_CREATE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a link creation property list")

    if(H5L_move(src_loc_p, src_name, dst_loc_p, dst_name, TRUE, lcpl_id,
                lapl_id, H5AC_dxpl_id) < 0)
	HGOTO_ERROR(H5E_LINK, H5E_CANTMOVE, FAIL, "unable to move link")

done:    
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Llink
 *
 * Purpose:	Creates a hard link from NEW_NAME to the object specified
 *		by OBJ_ID using properties defined in the Link Creation
 *              Property List LCPL.
 *
 *		This function should be used to link objects that have just
 *              been created.
 *
 *		CUR_NAME and NEW_NAME are interpreted relative to
 *		CUR_LOC_ID and NEW_LOC_ID, which is either a file ID or a
 *		group ID.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Tuesday, December 13, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Llink(hid_t new_loc_id, const char *new_name, hid_t obj_id, hid_t lcpl_id,
    hid_t lapl_id)
{
    H5G_loc_t	new_loc;
    H5G_loc_t	obj_loc;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Llink, FAIL)
    H5TRACE5("e","isiii",new_loc_id,new_name,obj_id,lcpl_id,lapl_id);

    /* Check arguments */
    if(new_loc_id == H5L_SAME_LOC)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "cannot use H5L_SAME_LOC when only one location is specified")
    if(H5G_loc(new_loc_id, &new_loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(H5G_loc(obj_id, &obj_loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!new_name || !*new_name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified")

    if(lcpl_id != H5P_DEFAULT && (TRUE != H5P_isa_class(lcpl_id, H5P_LINK_CREATE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a link creation property list")

    if(H5L_link(&new_loc, new_name, &obj_loc, lcpl_id, lapl_id, H5AC_dxpl_id) < 0)
        HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "unable to create link")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Llink() */


/*-------------------------------------------------------------------------
 * Function:	H5Lcreate_soft
 *
 * Purpose:	Creates a soft link from NEW_NAME to TARGET_PATH.
 *
 * 		TARGET_PATH can be anything and is interpreted at lookup
 *              time relative to the group which contains the final component
 *              of NEW_NAME.  For instance, if TARGET_PATH is `./foo' and
 *              NEW_NAME is `./x/y/bar' and a request is made for `./x/y/bar'
 *              then the actual object looked up is `./x/y/./foo'.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Monday, April  6, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Lcreate_soft(const char *target_path,
	 hid_t cur_loc, const char *cur_name, hid_t lcpl_id, hid_t lapl_id)
{
    H5G_loc_t	new_loc, *new_loc_p;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Lcreate_soft, FAIL)
    H5TRACE5("e","sisii",target_path,cur_loc,cur_name,lcpl_id,lapl_id);

    /* Check arguments */
    if(H5G_loc(cur_loc, &new_loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!target_path || !*target_path)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no target specified")
    if(!cur_name || !*cur_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no new name specified")

    if(lcpl_id != H5P_DEFAULT && (TRUE != H5P_isa_class(lcpl_id, H5P_LINK_CREATE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a link creation property list")

    new_loc_p = &new_loc;

    if(H5L_create_soft(target_path, new_loc_p, cur_name,
                lcpl_id, lapl_id, H5AC_dxpl_id) < 0)
	HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "unable to create link")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Lcreate_soft() */


/*-------------------------------------------------------------------------
 * Function:	H5Lcreate_hard
 *
 * Purpose:	Creates a hard link from NEW_NAME to CUR_NAME.
 *
 *		CUR_NAME must name an existing object.  CUR_NAME and
 *              NEW_NAME are interpreted relative to CUR_LOC_ID and
 *              NEW_LOC_ID, which are either file IDs or group IDs.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Monday, April  6, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Lcreate_hard(hid_t cur_loc_id, const char *cur_name,
	 hid_t new_loc_id, const char *new_name, hid_t lcpl_id, hid_t lapl_id)
{
    H5G_loc_t	cur_loc, *cur_loc_p;
    H5G_loc_t	new_loc, *new_loc_p;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Lcreate_hard, FAIL)
    H5TRACE6("e","isisii",cur_loc_id,cur_name,new_loc_id,new_name,lcpl_id,
             lapl_id);

    /* Check arguments */
    if(cur_loc_id != H5L_SAME_LOC && H5G_loc(cur_loc_id, &cur_loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(new_loc_id != H5L_SAME_LOC && H5G_loc(new_loc_id, &new_loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!cur_name || !*cur_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no current name specified")
    if(!new_name || !*new_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no new name specified")

    if(lcpl_id != H5P_DEFAULT && (TRUE != H5P_isa_class(lcpl_id, H5P_LINK_CREATE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a link creation property list")

    /* Set up current & new location pointers */
    cur_loc_p = &cur_loc;
    new_loc_p = &new_loc;
    if(cur_loc_id == H5L_SAME_LOC && new_loc_id == H5L_SAME_LOC)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "source and destination should not be both H5L_SAME_LOC")
    else if(cur_loc_id == H5L_SAME_LOC)
        cur_loc_p = new_loc_p;
    else if(new_loc_id == H5L_SAME_LOC)
   	new_loc_p = cur_loc_p;
    else if(cur_loc_p->oloc->file != new_loc_p->oloc->file)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "source and destination should be in the same file.")

    if(H5L_create_hard(cur_loc_p, cur_name, new_loc_p, new_name,
                lcpl_id, lapl_id, H5AC_dxpl_id) < 0)
	HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "unable to create link")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Lcreate_hard() */



/*-------------------------------------------------------------------------
 * Function:	H5Lcreate_ud
 *
 * Purpose:	Creates a user-defined link of type LINK_TYPE named LINK_NAME
 *              with user-specified data UDATA.
 *
 *		The format of the information pointed to by UDATA is
 *              defined by the user. UDATA_SIZE holds the size of this buffer.
 *
 *		LINK_NAME is interpreted relative to LINK_LOC_ID.
 *
 *		The property list specified by LCPL_ID holds properties used
 *              to create the link.
 *
 *              The link class of the new link must already be registered
 *              with the library.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Tuesday, December 13, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Lcreate_ud(hid_t link_loc_id, const char *link_name, H5L_link_t link_type, void *udata, size_t udata_size, hid_t lcpl_id, hid_t lapl_id)
{
    H5G_loc_t	link_loc;
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Lcreate_ud, FAIL)
    H5TRACE7("e","isLlxzii",link_loc_id,link_name,link_type,udata,udata_size,
             lcpl_id,lapl_id);

    /* Check arguments */
    if(H5G_loc(link_loc_id, &link_loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!link_name || !*link_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no link name specified")

    /* Create external link */
    if(H5L_create_ud(&link_loc, link_name, udata, udata_size, link_type,
                H5G_TARGET_NORMAL, lcpl_id, lapl_id, H5AC_dxpl_id) < 0)
	HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "unable to create link")

done:
    FUNC_LEAVE_API(ret_value);
} /* end H5Lcreate_ud */


/*-------------------------------------------------------------------------
 * Function:	H5Lunlink
 *
 * Purpose:	Removes the specified NAME from the group graph and
 *		decrements the link count for the object to which NAME
 *		points.  If the link count reaches zero then all file-space
 *		associated with the object will be reclaimed (but if the
 *		object is open, then the reclamation of the file space is
 *		delayed until all handles to the object are closed).
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Monday, April  6, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Lunlink(hid_t loc_id, const char *name, hid_t lapl_id)
{
    H5G_loc_t	loc;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Lunlink, FAIL)
    H5TRACE3("e","isi",loc_id,name,lapl_id);

    /* Check arguments */
    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")

    /* Unlink */
    if(H5L_unlink(&loc, name, lapl_id, H5AC_dxpl_id) < 0)
	HGOTO_ERROR(H5E_LINK, H5E_CANTDELETE, FAIL, "unable to unlink object")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Lunlink() */


/*-------------------------------------------------------------------------
 * Function:	H5Lget_linkval
 *
 * Purpose:	Returns the link value of a link whose name is NAME.  For
 *              symbolic links, this is the path to which the link points,
 *              including the null terminator.  For user-defined links, it
 *              is the link buffer.
 *
 *              At most SIZE bytes are copied to the BUF result buffer.
 *
 * Return:	Success:	Non-negative with the link value in BUF.
 *
 * 		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Monday, April 13, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Lget_linkval(hid_t loc_id, const char *name, size_t size, char *buf/*out*/,
               hid_t lapl_id)
{
    H5G_loc_t	loc;
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Lget_linkval, FAIL)
    H5TRACE5("e","iszxi",loc_id,name,size,buf,lapl_id);

    /* Check arguments */
    if(H5G_loc(loc_id, &loc))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified")

    /* Get the link value */
    if(H5L_linkval(&loc, name, size, buf, lapl_id, H5AC_ind_dxpl_id) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to get link value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Lget_linkval() */


/*-------------------------------------------------------------------------
 * Function:	H5Lget_linkinfo
 *
 * Purpose:	Gets metadata for a link.
 *
 * Return:	Success:	Non-negative with information in LINKBUF
 *
 * 		Failure:	Negative
 *
 * Programmer:	James Laird
 *              Wednesday, June 21, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Lget_linkinfo(hid_t loc_id, const char *name, H5L_linkinfo_t *linkbuf /*out*/,
    hid_t lapl_id)
{
    H5G_loc_t	loc;
    herr_t ret_value = SUCCEED;
    FUNC_ENTER_API(H5Lget_linkinfo, FAIL)
    H5TRACE4("e","isxi",loc_id,name,linkbuf,lapl_id);

    /* Check arguments */
    if(H5G_loc(loc_id, &loc))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified")

    /* Get the creation time */
    if(H5L_get_linkinfo(&loc, name, linkbuf, lapl_id, H5AC_ind_dxpl_id) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to get link info")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Lunregister
 *
 * Purpose:	Unregisters a class of user-defined links, preventing them
 *              from being traversed, queried, moved, etc.
 *
 *              A link class can be re-registered using H5Lregister().
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Monday, July 10, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Lunregister(H5L_link_t id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Lunregister, FAIL)
    H5TRACE1("e","Ll",id);

    /* Check args */
    if (id<0 || id>H5L_LINK_MAX)
	HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "invalid link type")

    /* Do it */
    if (H5L_unregister (id)<0)
	HGOTO_ERROR (H5E_LINK, H5E_NOTREGISTERED, FAIL, "unable to unregister link type")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Lunregister() */


/*-------------------------------------------------------------------------
 * Function:	H5Lis_registered
 *
 * Purpose:	Tests whether a user-defined link class has been registered
 *              or not.
 *
 * Return:	Positive if the link class has been registered
 *              Zero if it is unregistered
 *              Negative on error (if the class is not a valid UD class ID)
 *
 * Programmer:	James Laird
 *              Monday, July 10, 2006
 *
 *-------------------------------------------------------------------------
 */
htri_t H5Lis_registered(H5L_link_t id)
{
    size_t i;                   /* Local index variable */
    htri_t ret_value=FALSE;     /* Return value */

    FUNC_ENTER_API(H5Lis_registered, FAIL)

    /* Check args */
    if(id<0 || id>H5L_LINK_MAX)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid link type id number")

    /* Is the link class already registered? */
    for(i=0; i<H5L_table_used_g; i++)
	if(H5L_table_g[i].id==id) {
            ret_value=TRUE;
            break;
        } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
}
/* end H5Lis_registered */

/*
 *-------------------------------------------------------------------------
 *-------------------------------------------------------------------------
 *   N O   A P I   F U N C T I O N S   B E Y O N D   T H I S   P O I N T
 *-------------------------------------------------------------------------
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * Function:	H5L_register
 *
 * Purpose:	Registers a class of user-defined links, or changes the
 *              behavior of an existing class.
 *
 *              See H5Lregister for full documentation.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Monday, July 10, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5L_register (const H5L_link_class_t *cls)
{
    size_t      i;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5L_register, FAIL)

    assert (cls);
    assert (cls->id>=0 && cls->id<=H5L_LINK_MAX);

    /* Is the link type already registered? */
    for (i=0; i<H5L_table_used_g; i++)
	if (H5L_table_g[i].id==cls->id)
            break;

    /* Filter not already registered */
    if (i>=H5L_table_used_g) {
	if (H5L_table_used_g>=H5L_table_alloc_g) {
	    size_t n = MAX(H5L_MIN_TABLE_SIZE, 2*H5L_table_alloc_g);
	    H5L_link_class_t *table = H5MM_realloc(H5L_table_g,
					      n*sizeof(H5L_link_class_t));
            if (!table)
		HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to extend link type table")
	    H5L_table_g = table;
            H5L_table_alloc_g = n;
	} /* end if */

	/* Initialize */
	i = H5L_table_used_g++;
	HDmemcpy(H5L_table_g+i, cls, sizeof(H5L_link_class_t));
    } /* end if */
    /* Filter already registered */
    else {
	/* Replace old contents */
	HDmemcpy(H5L_table_g+i, cls, sizeof(H5L_link_class_t));
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5L_register */


/*-------------------------------------------------------------------------
 * Function:	H5L_unregister
 *
 * Purpose:	Unregisters a class of user-defined links.
 *
 *              See H5Lunregister for full documentation.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Monday, July 10, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5L_unregister (H5L_link_t id)
{
    size_t i;                   /* Local index variable */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5L_unregister,FAIL)

    assert (id>=0 && id<=H5L_LINK_MAX);

    /* Is the filter already registered? */
    for (i=0; i<H5L_table_used_g; i++)
	if (H5L_table_g[i].id==id)
            break;

    /* Fail if filter not found */
    if (i>=H5L_table_used_g)
        HGOTO_ERROR(H5E_LINK, H5E_NOTREGISTERED, FAIL, "link class is not registered")

    /* Remove filter from table */
    /* Don't worry about shrinking table size (for now) */
    HDmemmove(&H5L_table_g[i],&H5L_table_g[i+1],sizeof(H5L_link_class_t)*((H5L_table_used_g-1)-i));
    H5L_table_used_g--;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5L_unregister() */


/*-------------------------------------------------------------------------
 * Function:	H5L_link
 *
 * Purpose:	Creates a link from OBJ_ID to CUR_NAME.  See H5Llink() for
 *		full documentation.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Tuesday, December 13, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5L_link(H5G_loc_t *new_loc, const char *new_name, H5G_loc_t *obj_loc,
           hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id)
{
    H5F_t *file = NULL;                 /* File link will be in */
    H5O_link_t lnk;                     /* Link to insert */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5L_link)

    /* Check args */
    HDassert(new_loc);
    HDassert(obj_loc);
    HDassert(new_name && *new_name);

    /* The link callback will check that the object isn't being hard linked
     * into a different file, so we don't need to do it here (there could be
     * external links along the path).
     */

    /* Construct link information for eventual insertion */
    lnk.type = H5L_LINK_HARD;
    lnk.u.hard.addr = obj_loc->oloc->addr;

    /* Create the link */
    if( H5L_create_real(new_loc, new_name, obj_loc->path, obj_loc->oloc->file, &lnk, lcpl_id, lapl_id, dxpl_id) <0) 
        HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "unable to create new link to object")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5L_link() */


/*-------------------------------------------------------------------------
 * Function:	H5L_link_cb
 *
 * Purpose:	Callback for creating a link to an object.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Monday, September 19, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5L_link_cb(H5G_loc_t *grp_loc/*in*/, const char *name, const H5O_link_t UNUSED *lnk,
    H5G_loc_t *obj_loc, void *_udata/*in,out*/, H5G_own_loc_t *own_loc/*out*/)
{
    H5L_trav_ud3_t *udata = (H5L_trav_ud3_t *)_udata;   /* User data passed in */
    H5G_t *grp=NULL;                /* H5G_t for this group, opened to pass to user callback */
    hid_t grp_id = FAIL;            /* Id for this group (passed to user callback */
    H5G_loc_t temp_loc;             /* For UD callback */
    hbool_t temp_loc_init = FALSE;
    herr_t ret_value = SUCCEED;              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5L_link_cb)

    /* Check if the name in this group resolved to a valid location */
    /* (which is not what we want) */
    if(obj_loc != NULL)
        HGOTO_ERROR(H5E_SYM, H5E_EXISTS, FAIL, "name already exists")

    /* Check for crossing file boundaries with a new hard link */
    if(udata->lnk->type == H5L_LINK_HARD) {
        /* Check that both objects are in same file */
        if(grp_loc->oloc->file->shared != udata->file->shared)
            HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "interfile hard links are not allowed")
    } /* end if */

    /* Set the link's name correctly */
    /* Casting away const OK -QAK */
    udata->lnk->name = name;

    /* Insert link into group */
    if(H5G_obj_insert(grp_loc->oloc, name, udata->lnk, (hbool_t)(udata->lnk->type == H5L_LINK_HARD ? TRUE : FALSE), udata->dxpl_id) < 0)
        HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "unable to create new link for object")

    /* Set object's path if it has been passed in and is not set */
    if(udata->path != NULL && udata->path->user_path_r == NULL)
    {
      if(H5G_name_set(grp_loc->path, udata->path, name) < 0)
         HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "cannot set name")
    }

    /* If link is a user-defined link, trigger its creation callback if it has one*/
    if(udata->lnk->type >= H5L_LINK_UD_MIN)
    {
        const H5L_link_class_t   *link_class;         /* User-defined link class */
        H5O_loc_t           temp_oloc;
        H5G_name_t          temp_path;

        /* Get the link class for this type of link. */
        if(NULL == (link_class = H5L_find_class(udata->lnk->type)))
            HGOTO_ERROR(H5E_LINK, H5E_NOTREGISTERED, FAIL, "unable to get class of UD link")

        if(link_class->create_func != NULL)
        {
            /* Create a temporary location (or else H5G_open will do a shallow
             * copy and wipe out grp_loc)
             */
            H5G_name_reset(&temp_path);
            if(H5O_loc_copy(&temp_oloc, grp_loc->oloc, H5_COPY_DEEP) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTCOPY, FAIL, "unable to copy object location")

            temp_loc.oloc = &temp_oloc;
            temp_loc.path = &temp_path;
            temp_loc_init = TRUE;

            /* Set up location for user-defined callback */
            if((grp = H5G_open(&temp_loc, udata->dxpl_id)) == NULL)
                HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open group")
            if((grp_id = H5I_register(H5I_GROUP, grp)) < 0)
                HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register ID for group")

            if((link_class->create_func)(name, grp_id, udata->lnk->u.ud.udata, udata->lnk->u.ud.size, H5P_DEFAULT) < 0)
                HGOTO_ERROR(H5E_LINK, H5E_CALLBACK, FAIL, "link creation callback failed")
        }
    }

done:
    /* Close the location given to the user callback if it was created */
    if(grp_id >= 0)
    {
        if(H5I_dec_ref(grp_id) <0)
            HDONE_ERROR(H5E_ATOM, H5E_CANTRELEASE, FAIL, "unable to close atom from UD callback")
    }
    else if(grp != NULL)
    {
        if(H5G_close(grp) <0)
            HDONE_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "unable to close group given to UD callback")
    }
    else if(temp_loc_init)
        H5G_loc_free(&temp_loc);

    /* Indicate that this callback didn't take ownership of the group *
     * location for the object */
    *own_loc = H5G_OWN_NONE;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5L_link_cb() */


/*-------------------------------------------------------------------------
 * Function:    H5L_create_real
 *
 * Purpose:     Creates a link at a path location
 *
 *              lnk should have linkclass-specific information already
 *              set, but this function will take care of setting
 *              creation time and name.
 *
 *              obj_path can be NULL if the object's path doesn't need to
 *              be set, and obj_file can be NULL if the object is not a
 *              hard link.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Monday, December  5, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5L_create_real(H5G_loc_t *link_loc, const char *link_name, H5G_name_t *obj_path,
    H5F_t *obj_file, H5O_link_t *lnk, hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id)
{
    char *norm_link_name = NULL;        /* Pointer to normalized link name */
    unsigned target_flags = H5G_TARGET_NORMAL; /* Flags to pass to group traversal function */
    H5T_cset_t char_encoding = H5F_CRT_DEFAULT_CSET; /* Character encoding for link */
    H5P_genplist_t* lc_plist;           /* Link creation property list */
    H5L_trav_ud3_t udata;               /* User data for callback */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5L_create_real)

    /* Check args */
    HDassert(lnk);
    HDassert(link_loc);
    HDassert(link_name && *link_name);
    HDassert(lnk->type >= H5L_LINK_HARD && lnk->type <= H5L_LINK_MAX);

    /* Get normalized link name */
    if((norm_link_name = H5G_normalize(link_name)) == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "can't normalize name")

    /* Check for flags present in creation property list */
    if(lcpl_id != H5P_DEFAULT)
    {
      unsigned crt_intmd_group;

      if(NULL == (lc_plist = H5I_object(lcpl_id)))
          HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")

      /* Get intermediate group creation property */
      if(H5P_get(lc_plist, H5L_CRT_INTERMEDIATE_GROUP_NAME, &crt_intmd_group) < 0)
          HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for creating missing groups")

      if (crt_intmd_group > 0)
          target_flags |= H5G_CRT_INTMD_GROUP;

      /* Get character encoding property */
      if(H5P_get(lc_plist, H5P_CHAR_ENCODING_NAME, &char_encoding) < 0)
          HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for character encoding")
    } /* end if */

    /* Pass the lcpl to the link creation callback */
    udata.lcpl_id = lcpl_id;

    /* Fill in common data for the link struct */
    lnk->cset = char_encoding;
#ifdef H5_HAVE_GETTIMEOFDAY
    {
        struct timeval now_tv;

        HDgettimeofday(&now_tv, NULL);
        lnk->ctime = now_tv.tv_sec;
    }
#else /* H5_HAVE_GETTIMEOFDAY */
    lnk->ctime = HDtime(NULL);
#endif /* H5_HAVE_GETTIMEOFDAY */

    /* Set up user data
     * file is used to make sure that hard links don't cross files, and
     * should be NULL for other link types.
     * lnk is the link struct passed into this function.  At this point all
     * of its fields should be populated except for name, which is set when
     * inserting it in the callback.
     * dxpl_id is the dxpl ID that needs to be used during writes and reads.
     * path is a pointer to the path of the object being inserted if this is
     * a hard link; this is used to set the paths to objects when they are
     * created.  For other link types, this is NULL.
     */
    udata.file = obj_file;
    udata.lnk = lnk;
    udata.dxpl_id = dxpl_id;
    udata.path = obj_path;

    /* Traverse the destination path & create new link */
    if(H5G_traverse(link_loc, link_name, target_flags, H5L_link_cb, &udata, lapl_id, dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, FAIL, "can't insert link")

done:
    /* Free the normalized path name */
    if(norm_link_name)
        H5MM_xfree(norm_link_name);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5L_create_real() */


/*-------------------------------------------------------------------------
 * Function:	H5L_create_hard
 *
 * Purpose:	Creates a hard link from NEW_NAME to CUR_NAME.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Monday, April  6, 1998
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5L_create_hard(H5G_loc_t *cur_loc, const char *cur_name,
    H5G_loc_t *link_loc, const char *link_name, hid_t lcpl_id, hid_t lapl_id,
    hid_t dxpl_id)
{
    char *norm_cur_name = NULL;	        /* Pointer to normalized current name */
    H5F_t *link_file = NULL;            /* Pointer to file to link to */
    H5O_link_t lnk;                     /* Link to insert */
    H5G_loc_t obj_loc;                  /* Location of object to link to */
    H5G_name_t path;                    /* obj_loc's path*/
    H5O_loc_t oloc;                     /* obj_loc's oloc */
    hbool_t loc_valid = FALSE;
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5L_create_hard)

    /* Check args */
    HDassert(cur_loc);
    HDassert(link_loc);
    HDassert(cur_name && *cur_name);
    HDassert(link_name && *link_name);

    /* Get normalized copy of the current name */
    if((norm_cur_name = H5G_normalize(cur_name)) == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "can't normalize name")

    /* Set up link data specific to hard links */
    lnk.type = H5L_LINK_HARD;

    /* Get object location for object pointed to */
    obj_loc.path = &path;
    obj_loc.oloc = &oloc;
    H5G_loc_reset(&obj_loc);
    if(H5G_loc_find(cur_loc, norm_cur_name, &obj_loc, lapl_id, dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "source object not found")
    loc_valid = TRUE;

    /* Construct link information for eventual insertion */
    lnk.u.hard.addr = obj_loc.oloc->addr;

    /* Set destination's file information */
    link_file = obj_loc.oloc->file;

    /* Create actual link to the object.  Pass in NULL for the path, since this
     * function shouldn't change an object's user path. */
    if(H5L_create_real(link_loc, link_name, NULL, link_file, &lnk, lcpl_id,
                       lapl_id, dxpl_id) < 0)
        HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "unable to create new link to object")

done:
    /* Free the object header location */
    if(loc_valid)    {
        if(H5G_loc_free(&obj_loc) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "unable to free location")
    }

    /* Free the normalized path name */
    if(norm_cur_name)
        H5MM_xfree(norm_cur_name);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5L_create_hard() */


/*-------------------------------------------------------------------------
 * Function:	H5L_create_soft
 *
 * Purpose:	Creates a soft link from NEW_NAME to CUR_NAME.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Monday, April  6, 1998
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5L_create_soft( const char *target_path, H5G_loc_t *link_loc,
                const char *link_name, hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id)
{
    char *norm_target = NULL;	        /* Pointer to normalized current name */
    H5O_link_t lnk;                     /* Link to insert */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5L_create_soft)

    /* Check args */
    HDassert(link_loc);
    HDassert(target_path && *target_path);
    HDassert(link_name && *link_name);

    /* Get normalized copy of the link target */
    if((norm_target = H5G_normalize(target_path)) == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "can't normalize name")

    /* Set up link data specific to soft links */
    lnk.type = H5L_LINK_SOFT;
    lnk.u.soft.name = norm_target;

    /* Create actual link to the object */
    if(H5L_create_real(link_loc, link_name, NULL, NULL, &lnk, lcpl_id,
                       lapl_id, dxpl_id) < 0)
        HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "unable to create new link to object")

done:
    /* Free the normalized target name */
    if(norm_target)
        H5MM_xfree(norm_target);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5L_create_soft() */


/*-------------------------------------------------------------------------
 * Function:	H5L_create_ud
 *
 * Purpose:	Creates a user-defined link. See H5Lcreate_ud for
 *              full documentation.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Friday, May 19, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5L_create_ud(H5G_loc_t *link_loc, const char *link_name, void * ud_data,
    size_t ud_data_size, H5L_link_t type, unsigned traverse_flags,
    hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id)
{
    H5O_link_t lnk;                     /* Link to insert */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5L_create_ud)

    /* Check args */
    HDassert(type >= H5L_LINK_UD_MIN && type <= H5L_LINK_MAX);
    HDassert(link_loc);
    HDassert(link_name && *link_name);
    HDassert(ud_data_size >= 0);
    HDassert(ud_data_size == 0 || ud_data);

    /* Make sure that this link class is registered */
    if(H5L_find_class_idx(type) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "link class has not been registered with library")

    /* Fill in UD link-specific information in the link struct*/
    if(ud_data_size > 0)
    {
        lnk.u.ud.udata = H5MM_malloc((size_t) ud_data_size);
        HDmemcpy(lnk.u.ud.udata, ud_data, (size_t) ud_data_size);
    }
    else
        lnk.u.ud.udata = NULL;

    lnk.u.ud.size = ud_data_size;
    lnk.type = type;

    /* Create actual link to the object */
    if(H5L_create_real(link_loc, link_name, NULL, NULL, &lnk, lcpl_id,
                       lapl_id, dxpl_id) < 0)
        HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "unable to register new name for object")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5L_create_ud() */


/*-------------------------------------------------------------------------
 * Function:	H5L_linkval_cb
 *
 * Purpose:	Callback for retrieving link value or udata.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, September 20, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5L_linkval_cb(H5G_loc_t UNUSED *grp_loc/*in*/, const char UNUSED *name, const H5O_link_t *lnk,
    H5G_loc_t UNUSED *obj_loc, void *_udata/*in,out*/, H5G_own_loc_t *own_loc/*out*/)
{
    H5L_trav_ud5_t *udata = (H5L_trav_ud5_t *)_udata;   /* User data passed in */
    const H5L_link_class_t *link_class;             /* User-defined link class */
    herr_t ret_value = SUCCEED;               /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5L_linkval_cb)

    /* Check if the name in this group resolved to a valid link */
    if(lnk == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "name doesn't exist")

    if(H5L_LINK_SOFT == lnk->type)
    {
        /* Copy to output buffer */
        if(udata->size > 0 && udata->buf) {
            HDstrncpy(udata->buf, lnk->u.soft.name, udata->size);
            if(HDstrlen(lnk->u.soft.name) >= udata->size)
                udata->buf[udata->size - 1] = '\0';
        } /* end if */
    }
    else if(lnk->type >= H5L_LINK_UD_MIN)
    {
        /* Get the link class for this type of link.  It's okay if the class isn't registered, though--we
        * just can't give any more information about it */
        link_class = H5L_find_class(lnk->type);

        if(link_class != NULL && link_class->query_func != NULL) {
            if((link_class->query_func)(lnk->name, lnk->u.ud.udata, lnk->u.ud.size, udata->buf, udata->size) < 0)
              HGOTO_ERROR(H5E_LINK, H5E_CALLBACK, FAIL, "query callback returned failure")
        }
        else if(udata->buf && udata->size > 0)
            udata->buf[0] = '\0';
    }
    else
        HGOTO_ERROR(H5E_LINK, H5E_BADTYPE, FAIL, "object is not a symbolic or user-defined link")

done:
    /* Indicate that this callback didn't take ownership of the group *
     * location for the object */
    *own_loc = H5G_OWN_NONE;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5L_linkval_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5L_linkval
 *
 * Purpose:	Returns the value of a symbolic link or the udata for a
 *              user-defined link.
 *
 * Return:	Success:	Non-negative, with at most SIZE bytes of the
 *				link value copied into the BUF buffer.  If the
 *				link value is larger than SIZE characters
 *				counting the null terminator then the BUF
 *				result will not be null terminated.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Monday, April 13, 1998
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5L_linkval(H5G_loc_t *loc, const char *name, size_t size, char *buf/*out*/, hid_t lapl_id, hid_t dxpl_id)
{
    H5L_trav_ud5_t udata;           /* User data for callback */
    herr_t ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5L_linkval)

    /* Set up user data for retrieving information */
    udata.size = size;
    udata.buf = buf;

    /* Traverse the group hierarchy to locate the object to get info about */
    if(H5G_traverse(loc, name, H5G_TARGET_SLINK | H5G_TARGET_UDLINK, H5L_linkval_cb, &udata, lapl_id, dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "name doesn't exist")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5L_linkval() */


/*-------------------------------------------------------------------------
 * Function:	H5L_unlink_cb
 *
 * Purpose:	Callback for unlinking an object.  This routine
 *              deletes the link
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Monday, September 19, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5L_unlink_cb(H5G_loc_t *grp_loc/*in*/, const char *name, const H5O_link_t UNUSED *lnk,
    H5G_loc_t *obj_loc, void *_udata/*in,out*/, H5G_own_loc_t *own_loc/*out*/)
{
    H5G_t *grp=NULL;                /* H5G_t for this group, opened to pass to user callback */
    hid_t grp_id = FAIL;            /* Id for this group (passed to user callback */
    H5L_trav_ud6_t *udata = (H5L_trav_ud6_t *)_udata;   /* User data passed in */
    H5G_loc_t           temp_loc;   /* For UD callback */
    hbool_t             temp_loc_init = FALSE;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5L_unlink_cb)

    /* Check if the name in this group resolved to a valid link */
    if(obj_loc == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "name doesn't exist")

    /* Check for removing '.' */
    if(lnk == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDELETE, FAIL, "can't delete self")

    /* If there is a user-defined callback, call it before deleting the link */
    if(lnk->type >= H5L_LINK_UD_MIN)
    {
        const H5L_link_class_t   *link_class;         /* User-defined link class */
        H5O_loc_t           temp_oloc;
        H5G_name_t          temp_path;

        /* Get the link class for this type of link. */
        if(NULL == (link_class = H5L_find_class(lnk->type)))
            HGOTO_ERROR(H5E_LINK, H5E_NOTREGISTERED, FAIL, "link class not registered")

        if(link_class->del_func != NULL)
        {
            H5G_name_reset(&temp_path);

            if(H5O_loc_copy(&temp_oloc, grp_loc->oloc, H5_COPY_DEEP) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTCOPY, FAIL, "unable to copy object location")

            temp_loc.oloc = &temp_oloc;
            temp_loc.path = &temp_path;
            temp_loc_init = TRUE;

            /* Set up location for user-defined callback */
            if((grp = H5G_open(&temp_loc, udata->dxpl_id)) == NULL)
                HGOTO_ERROR(H5E_FILE, H5E_CANTOPENOBJ, FAIL, "unable to open group")
            if((grp_id = H5I_register(H5I_GROUP, grp)) < 0)
                HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register group")

            if((link_class->del_func)(name, grp_id, lnk->u.ud.udata, lnk->u.ud.size) < 0)
                HGOTO_ERROR(H5E_LINK, H5E_CALLBACK, FAIL, "link deletion callback returned failure")
        }
    }

    /* Remove the link from the group */
    if(H5G_loc_remove(grp_loc, name, obj_loc, udata->dxpl_id) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTDELETE, FAIL, "unable to unlink name from group")

done:
    /* Close the location given to the user callback if it was created */
    if(grp_id >= 0)
        H5I_dec_ref(grp_id);
    else if(grp != NULL)
        H5G_close(grp);
    else if(temp_loc_init)
        H5G_loc_free(&temp_loc);

    /* Indicate that this callback didn't take ownership of the group *
     * location for the object */
    *own_loc = H5G_OWN_NONE;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5L_unlink_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5L_unlink
 *
 * Purpose:	Unlink a name from a group.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Thursday, September 17, 1998
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5L_unlink(H5G_loc_t *loc, const char *name, hid_t lapl_id, hid_t dxpl_id)
{
    H5L_trav_ud6_t      udata;                  /* User data for callback */
    char		*norm_name = NULL;	/* Pointer to normalized name */
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5L_unlink)

    /* Sanity check */
    HDassert(loc);
    HDassert(name && *name);

    /* Get normalized copy of the name */
    if((norm_name = H5G_normalize(name)) == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "can't normalize name")

    /* Set up user data for unlink operation */
    udata.dxpl_id = dxpl_id;

    if(H5G_traverse(loc, norm_name, H5G_TARGET_SLINK|H5G_TARGET_UDLINK|H5G_TARGET_MOUNT, H5L_unlink_cb, &udata, lapl_id, dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_EXISTS, FAIL, "name doesn't exist")

done:
    /* Free the normalized path name */
    if(norm_name)
        H5MM_xfree(norm_name);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5L_unlink() */


/*-------------------------------------------------------------------------
 * Function:	H5L_move_dest_cb
 *
 * Purpose:	Second callback for moving and renaming an object.  This routine
 *              inserts a new link into the group returned by the traversal.
 *              It is called by H5L_move_cb.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Monday, April 3, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5L_move_dest_cb(H5G_loc_t *grp_loc/*in*/, const char *name, const H5O_link_t *lnk,
    H5G_loc_t *obj_loc, void *_udata/*in,out*/, H5G_own_loc_t *own_loc/*out*/)
{
    H5L_trav_ud10_t *udata = (H5L_trav_ud10_t *)_udata;   /* User data passed in */
    H5RS_str_t *dst_name_r = NULL;      /* Ref-counted version of dest name */
    H5G_t *grp=NULL;                    /* H5G_t for this group, opened to pass to user callback */
    hid_t grp_id = FAIL;                /* Id for this group (passed to user callback */
    H5G_loc_t temp_loc;                 /* For UD callback */
    hbool_t temp_loc_init = FALSE;
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5L_move_dest_cb)

    /* Make sure an object with this name doesn't already exist */
    if(obj_loc != NULL)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "an object with that name already exists")

    /* Check for crossing file boundaries with a new hard link */
    if(udata->lnk->type == H5L_LINK_HARD) {
        /* Check that both objects are in same file */
        if(grp_loc->oloc->file->shared != udata->file->shared)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "moving a link across files is not allowed")
    } /* end if */

    /* Give the object its new name */
    /* Casting away const okay -JML */
    udata->lnk->name = H5MM_xfree(udata->lnk->name);
    udata->lnk->name= (char *)name;

    /* Insert the link into the group */
    if(H5G_obj_insert(grp_loc->oloc, name, udata->lnk, (hbool_t)(udata->lnk->type == H5L_LINK_HARD ? TRUE : FALSE), udata->dxpl_id) < 0)
        HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "unable to create new link to object")

    /* If the link was a user-defined link, call its move callback if it has one */
    if(udata->lnk->type >= H5L_LINK_UD_MIN)
    {
        const H5L_link_class_t   *link_class;         /* User-defined link class */
        H5O_loc_t           temp_oloc;
        H5G_name_t          temp_path;

        /* Get the link class for this type of link. */
        if(NULL == (link_class = H5L_find_class(udata->lnk->type)))
            HGOTO_ERROR(H5E_LINK, H5E_NOTREGISTERED, FAIL, "link class is not registered")

        if((!udata->copy && link_class->move_func != NULL) || (udata->copy && link_class->move_func))
        {
            /* Create a temporary location (or else H5G_open will do a shallow
             * copy and wipe out grp_loc)
             */
            H5G_name_reset(&temp_path);
            if(H5O_loc_copy(&temp_oloc, grp_loc->oloc, H5_COPY_DEEP) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTCOPY, FAIL, "unable to copy object location")

            temp_loc.oloc = &temp_oloc;
            temp_loc.path = &temp_path;
            temp_loc_init = TRUE;

            /* Set up location for user-defined callback */
            if((grp = H5G_open(&temp_loc, udata->dxpl_id)) == NULL)
                HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open group")
            if((grp_id = H5I_register(H5I_GROUP, grp)) < 0)
                HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register group ID")

            if(udata->copy)
            {
                if((link_class->copy_func)(udata->lnk->name, grp_id, udata->lnk->u.ud.udata, udata->lnk->u.ud.size) < 0)
                    HGOTO_ERROR(H5E_LINK, H5E_CALLBACK, FAIL, "UD copy callback returned error")
            }
            else
            {
                if((link_class->move_func)(udata->lnk->name, grp_id, udata->lnk->u.ud.udata, udata->lnk->u.ud.size) < 0)
                    HGOTO_ERROR(H5E_LINK, H5E_CALLBACK, FAIL, "UD move callback returned error")
            }
        }
    }
done:
    /* Close the location given to the user callback if it was created */
    if(grp_id >= 0)
    {
        if(H5I_dec_ref(grp_id) <0)
            HDONE_ERROR(H5E_ATOM, H5E_CANTRELEASE, FAIL, "unable to close atom from UD callback")
    }
    else if(grp != NULL)
    {
        if(H5G_close(grp) <0)
            HDONE_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "unable to close group given to UD callback")
    }
    else if(temp_loc_init)
        H5G_loc_free(&temp_loc);

    /* Indicate that this callback didn't take ownership of the group *
     * location for the object */
    *own_loc = H5G_OWN_NONE;
    if(dst_name_r)
        H5RS_decr(dst_name_r);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5L_move_dest_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5L_move_cb
 *
 * Purpose:	Callback for moving and renaming an object.  This routine
 *              replaces the names of open objects with the moved object
 *              in the path
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Friday, April 3, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5L_move_cb(H5G_loc_t *grp_loc/*in*/, const char *name, const H5O_link_t *lnk,
    H5G_loc_t *obj_loc, void *_udata/*in,out*/, H5G_own_loc_t *own_loc/*out*/)
{
    H5L_trav_ud4_t *udata = (H5L_trav_ud4_t *)_udata;   /* User data passed in */
    H5L_trav_ud10_t udata_out;    /* User data for H5L_move_dest_cb traversal */
    H5G_obj_t type;               /* Type of object being moved */
    H5RS_str_t *dst_name_r = NULL;      /* Ref-counted version of dest name */
    char * orig_name = NULL;            /* The name of the link in this group */
    herr_t ret_value = SUCCEED;              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5L_move_cb)

    /* Check if the name in this group resolved to a valid link */
    if(obj_loc == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "name doesn't exist")

    /* Check for operations on '.' */
    if(lnk == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "the name of a link must be supplied to move or copy")

    /* Get object type */
    switch(lnk->type) {
        case H5L_LINK_HARD:
          if(H5G_UNKNOWN == (type = H5O_obj_type(obj_loc->oloc, udata->dxpl_id)))
                HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "unable to get object type to move")
            break;

        case H5L_LINK_SOFT:
            type = H5G_LINK;
            break;

        default:
            if(lnk->type < H5L_LINK_UD_MIN)
                HGOTO_ERROR(H5E_LINK, H5E_BADTYPE, FAIL, "unrecognized link type")
            type = H5G_UDLINK;
    } /* end switch */

    /* Set up user data for move_dest_cb */
    if((udata_out.lnk = H5O_link_copy(lnk, NULL, 0)) == NULL)
        HGOTO_ERROR(H5E_LINK, H5E_CANTCOPY, FAIL, "unable to copy link to be moved");
    udata_out.lnk->cset = udata->cset;
    udata_out.file = grp_loc->oloc->file;
    udata_out.copy = udata->copy;
    udata_out.dxpl_id = udata->dxpl_id;

    /* Remember the link's original name (in case it's changed by H5G_name_replace) */
    orig_name = H5MM_xstrdup(name);

    /* Insert the link into its new location */
    if(H5G_traverse(udata->dst_loc, udata->dst_name, H5G_TARGET_NORMAL, H5L_move_dest_cb, &udata_out, udata->lapl_id, udata->dxpl_id) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to follow symbolic link")

    /* If this is a move and not a copy operation, change the object's name and remove the old link */
    if(!udata->copy)
    {
        /* Fix names up */
        dst_name_r = H5RS_wrap(udata->dst_name);
        HDassert(dst_name_r);
        if(H5G_name_replace(type, obj_loc, dst_name_r, udata->dst_loc, H5G_NAME_MOVE) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to replace name ")

        /* Remove the old link */
        if(H5G_obj_remove(grp_loc->oloc, orig_name, &type, udata->dxpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to remove old name")
    }

done:
        /* Cleanup */
    if(orig_name)
        H5MM_xfree(orig_name);

    /* Indicate that this callback didn't take ownership of the group *
     * location for the object */
    *own_loc = H5G_OWN_NONE;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5L_move_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5L_move
 *
 * Purpose:	Atomically move or copy a link.
 *
 *              Creates a copy of a link in a new destination with a new name.
 *              SRC_LOC and SRC_NAME together define the link's original
 *              location, while DST_LOC and DST_NAME together define its
 *              final location.
 *
 *              If copy_flag is FALSE, the original link is removed
 *              (effectively moving the link).
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Monday, May 1, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5L_move(H5G_loc_t *src_loc, const char *src_name, H5G_loc_t *dst_loc,
                const char *dst_name, hbool_t copy_flag, hid_t lcpl_id,
                hid_t lapl_id, hid_t dxpl_id)
{
    unsigned    target_flags = H5G_TARGET_MOUNT|H5G_TARGET_SLINK|H5G_TARGET_UDLINK;
    H5T_cset_t char_encoding = H5F_CRT_DEFAULT_CSET; /* Character encoding for link */
    H5P_genplist_t* lc_plist;           /* Link creation property list */
    H5P_genplist_t* la_plist;           /* Link access property list */
    H5L_trav_ud4_t      udata;          /* User data for traversal */
    hid_t               lapl_copy;      /* Copy of lapl for this function */
    herr_t              ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5L_move)

    /* Sanity check */
    HDassert(src_loc);
    HDassert(dst_loc);
    HDassert(src_name && *src_name);
    HDassert(dst_name && *dst_name);

    /* Check for flags present in creation property list */
    if(lcpl_id != H5P_DEFAULT)
    {
      unsigned crt_intmd_group;

      if(NULL == (lc_plist = H5I_object(lcpl_id)))
          HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")

      /* Get intermediate group creation property */
      if(H5P_get(lc_plist, H5L_CRT_INTERMEDIATE_GROUP_NAME, &crt_intmd_group) < 
0)
          HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for creating missing groups")

      if (crt_intmd_group > 0)
          target_flags |= H5G_CRT_INTMD_GROUP;

      /* Get character encoding property */
      if(H5P_get(lc_plist, H5P_CHAR_ENCODING_NAME, &char_encoding) < 0)
          HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for character encoding")
    } /* end if */

    /* Copy the link access property list because traversing UD links will
     * decrease the NLINKS property. HDF5 should have NLINKS traversals to
     * get to the source and NLINKS more to get to the destination. */
    if(lapl_id == H5P_DEFAULT) {
        lapl_copy = lapl_id;
    }
    else {
        if (NULL==(la_plist=H5I_object(lapl_id)))
            HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a valid access PL")
        if((lapl_copy=H5P_copy_plist(la_plist)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "unable to copy access properties")
    }

    /* Set up user data */
    udata.dst_loc = dst_loc;
    udata.dst_name= dst_name;
    udata.cset = char_encoding;
    udata.copy = copy_flag;
    udata.lapl_id = lapl_copy;
    udata.dxpl_id = dxpl_id;

    /* Do the move */
    if(H5G_traverse(src_loc, src_name, target_flags, H5L_move_cb, &udata, lapl_id, dxpl_id) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to find link")    

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5L_move() */


/*-------------------------------------------------------------------------
 * Function:	H5L_get_linfo_cb
 *
 * Purpose:	Callback for retrieving a link's metadata
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Monday, April 17 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5L_get_linfo_cb(H5G_loc_t UNUSED *grp_loc/*in*/, const char UNUSED *name, const H5O_link_t *lnk,
    H5G_loc_t *obj_loc, void *_udata/*in,out*/, H5G_own_loc_t *own_loc/*out*/)
{
    H5L_trav_ud1_t *udata = (H5L_trav_ud1_t *)_udata;   /* User data passed in */
    H5L_linkinfo_t *linfo = udata->linfo;
    const H5L_link_class_t *link_class;       /* User-defined link class */
    ssize_t cb_ret;          /* Return value from UD callback */
    herr_t ret_value = SUCCEED;              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5L_get_linfo_cb)

    /* Check if the name in this group resolved to a valid link */
    if(lnk == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "name doesn't exist")

    /* Get information from the link */
    if(linfo)
    {
        linfo->cset = lnk->cset;
        linfo->ctime = lnk->ctime;
        linfo->linkclass = lnk->type;

        switch(lnk->type)
        {
          case H5L_LINK_HARD:
              linfo->u.address = lnk->u.hard.addr;
              break;

          case H5L_LINK_SOFT:
              linfo->u.link_size = HDstrlen(lnk->u.soft.name) + 1; /*count the null terminator*/
              break;

          default:
            if(lnk->type < H5L_LINK_UD_MIN || lnk->type > H5L_LINK_MAX)
                HGOTO_ERROR(H5E_LINK, H5E_BADTYPE, FAIL, "unknown link class")

            /* User-defined link; call its query function to get the link udata size. */
            /* Get the link class for this type of link.  It's okay if the class
            * isn't registered, though--we just can't give any more information
            * about it
            */
              link_class = H5L_find_class(lnk->type);

              if(link_class != NULL && link_class->query_func != NULL) {
                  if((cb_ret = (link_class->query_func)(lnk->name, lnk->u.ud.udata, lnk->u.ud.size, NULL, 0)) < 0)
                    HGOTO_ERROR(H5E_LINK, H5E_CALLBACK, FAIL, "query buffer size callback returned failure")

                  linfo->u.link_size = cb_ret;
              }
              else {
                  linfo->u.link_size = 0;
              }
        } /* end switch */
    } /* end if */

done:
    /* Indicate that this callback didn't take ownership of the group *
     * location for the object */
    *own_loc = H5G_OWN_NONE;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5L_get_linfo_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5L_get_linkinfo
 *
 * Purpose:	Returns metadata about a link.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Monday, April 17 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5L_get_linkinfo(const H5G_loc_t *loc, const char *name, H5L_linkinfo_t *linkbuf/*out*/, hid_t lapl_id, hid_t dxpl_id)
{
    H5L_trav_ud1_t udata;
    herr_t ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5L_get_linkinfo)

    udata.linfo = linkbuf;
    udata.dxpl_id = dxpl_id;

    /* Traverse the group hierarchy to locate the object to get info about */
    if(H5G_traverse(loc, name, H5G_TARGET_SLINK|H5G_TARGET_UDLINK, H5L_get_linfo_cb, &udata, lapl_id, dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_EXISTS, FAIL, "name doesn't exist")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5L_get_linkinfo() */


/*-------------------------------------------------------------------------
 * Function:	H5L_get_default_lcpl
 *
 * Purpose:	Accessor for the default Link Creation Property List
 *
 * Return:	Success:	ID of the deafult lcpl
 *
 * 		Failure:	Negative
 *
 * Programmer:	James Laird
 *              Tuesday, July 4, 2006
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5L_get_default_lcpl()
{
    hid_t ret_value = FAIL;       /* Return value */

    FUNC_ENTER_NOAPI(H5L_get_default_lcpl, FAIL)

    ret_value = H5P_LINK_CREATE_DEFAULT;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5L_get_default_lcpl */

