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
 * Created:		H5Gname.c
 *			Sep 12 2005
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Functions for handling group hierarchy paths.
 *
 *-------------------------------------------------------------------------
 */
#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */
#define H5G_PACKAGE		/*suppress error about including H5Gpkg	  */


/* Packages needed by this file... */
#include "H5private.h"		/* Generic Functions			*/
#include "H5Dprivate.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"		/* File access				*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory wrappers			*/

/* Private typedefs */

/* Struct used by change name callback function */
typedef struct H5G_names_t {
    H5G_names_op_t op;                  /* Operation performed on file */
    H5G_loc_t	*loc;                   /* [src] Location affected */
    H5F_t       *top_loc_file;          /* Top file in src location's mounted file hier. */
    H5G_loc_t	*dst_loc;               /* Destination location */
    H5RS_str_t  *dst_name;              /* Name of object relative to destination location */
} H5G_names_t;

/* Info to pass to the iteration functions */
typedef struct iter_t {
        char *container;  /* full name of the container object */
	hid_t file; 		/* File id where it came from */
	haddr_t obj; 		/* The address what we're looking for */
} iter_t;

typedef struct {
    haddr_t objno;      /* Object ID (i.e. address) */
    const char *path;   /* Object path */
} ref_path_node_t;

static H5SL_t *ref_path_table = NULL;   /* the "table" (implemented with a skip list) */


/* Private macros */

/* Local variables */

/* Declare extern the PQ free list for the wrapped strings */
H5FL_BLK_EXTERN(str_buf);

/* PRIVATE PROTOTYPES */
static htri_t H5G_common_path(const H5RS_str_t *fullpath_r, const H5RS_str_t *prefix_r);
static H5RS_str_t *H5G_build_fullpath(const char *prefix, const char *name);
static H5RS_str_t *H5G_build_fullpath_refstr_refstr(const H5RS_str_t *prefix_r, const H5RS_str_t *name_r);
static H5RS_str_t *H5G_build_fullpath_refstr_str(H5RS_str_t *path_r, const char *name);
static herr_t H5G_name_move_path(H5RS_str_t **path_r_ptr,
    const char *full_suffix, const char *src_path, const char *dst_path);
static int H5G_name_replace_cb(void *obj_ptr, hid_t obj_id, void *key);
static ssize_t H5G_get_refobj_name(haddr_t id, hid_t file, char* name, size_t size);
static herr_t H5G_refname_iterator (hid_t group, const char *name, void *_iter);
static hbool_t H5G_ref_path_table_find(haddr_t objno);
static int H5G_term_ref_path_table(void);
static int H5G_init_ref_path_table(hid_t fid);
static herr_t H5G_free_ref_path_info(void *item, void UNUSED *key, void UNUSED *operator_data/*in,out*/);
static int H5G_ref_path_table_put(const char *path, haddr_t objno);


/*-------------------------------------------------------------------------
 * Function: H5G_common_path
 *
 * Purpose: Determine if one path is a valid prefix of another path
 *
 * Return: TRUE for valid prefix, FALSE for not a valid prefix, FAIL
 *              on error
 *
 * Programmer: Quincey Koziol, koziol@ncsa.uiuc.edu
 *
 * Date: September 24, 2002
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5G_common_path(const H5RS_str_t *fullpath_r, const H5RS_str_t *prefix_r)
{
    const char *fullpath;       /* Pointer to actual fullpath string */
    const char *prefix;         /* Pointer to actual prefix string */
    size_t  nchars1,nchars2;    /* Number of characters in components */
    htri_t ret_value=FALSE;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_common_path)

    /* Get component of each name */
    fullpath=H5RS_get_str(fullpath_r);
    assert(fullpath);
    fullpath=H5G_component(fullpath,&nchars1);
    assert(fullpath);
    prefix=H5RS_get_str(prefix_r);
    assert(prefix);
    prefix=H5G_component(prefix,&nchars2);
    assert(prefix);

    /* Check if we have a real string for each component */
    while(*fullpath && *prefix) {
        /* Check that the components we found are the same length */
        if(nchars1==nchars2) {
            /* Check that the two components are equal */
            if(HDstrncmp(fullpath,prefix,nchars1)==0) {
                /* Advance the pointers in the names */
                fullpath+=nchars1;
                prefix+=nchars2;

                /* Get next component of each name */
                fullpath=H5G_component(fullpath,&nchars1);
                assert(fullpath);
                prefix=H5G_component(prefix,&nchars2);
                assert(prefix);
            } /* end if */
            else
                HGOTO_DONE(FALSE)
        } /* end if */
        else
            HGOTO_DONE(FALSE)
    } /* end while */

    /* If we reached the end of the prefix path to check, it must be a valid prefix */
    if(*prefix=='\0')
        ret_value=TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_common_path() */


/*-------------------------------------------------------------------------
 * Function: H5G_build_fullpath
 *
 * Purpose: Build a full path from a prefix & base pair of strings
 *
 * Return: Pointer to reference counted string on success, NULL on error
 *
 * Programmer: Quincey Koziol, koziol@ncsa.uiuc.edu
 *
 * Date: August 19, 2005
 *
 *-------------------------------------------------------------------------
 */
static H5RS_str_t *
H5G_build_fullpath(const char *prefix, const char *name)
{
    char *full_path;            /* Full user path built */
    size_t path_len;            /* Length of the path */
    unsigned need_sep;          /* Flag to indicate if separator is needed */
    H5RS_str_t *ret_value;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_build_fullpath)

    /* Sanity check */
    HDassert(prefix);
    HDassert(name);

    /* Get the length of the prefix */
    path_len = HDstrlen(prefix);

    /* Determine if there is a trailing separator in the name */
    if(prefix[path_len - 1] == '/')
        need_sep = 0;
    else
        need_sep = 1;

    /* Add in the length needed for the '/' separator and the relative path */
    path_len += HDstrlen(name) + need_sep;

    /* Allocate space for the path */
    if(NULL == (full_path = (char *)H5FL_BLK_MALLOC(str_buf, path_len + 1)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Build full path */
    HDstrcpy(full_path, prefix);
    if(need_sep)
        HDstrcat(full_path, "/");
    HDstrcat(full_path, name);

    /* Create reference counted string for path */
    if((ret_value = H5RS_own(full_path)) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_build_fullpath() */


/*-------------------------------------------------------------------------
 * Function:	H5G_build_fullpath_refstr_str
 *
 * Purpose:     Append an object path to an existing ref-counted path
 *
 * Return:	Success:	Non-NULL, combined path
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol, koziol@ncsa.uiuc.edu
 *              Tuesday, October 11, 2005
 *
 *-------------------------------------------------------------------------
 */
static H5RS_str_t *
H5G_build_fullpath_refstr_str(H5RS_str_t *prefix_r, const char *name)
{
    const char *prefix;         /* Pointer to raw string for path */
    H5RS_str_t *ret_value;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_build_fullpath_refstr_str)

    HDassert(prefix_r);
    HDassert(name);

    /* Get the raw string for the user path */
    prefix = H5RS_get_str(prefix_r);
    HDassert(prefix);

    /* Create reference counted string for path */
    ret_value = H5G_build_fullpath(prefix, name);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_build_fullpath_refstr_str() */


/*-------------------------------------------------------------------------
 * Function: H5G_name_build_refstr_refstr
 *
 * Purpose: Build a full path from a prefix & base pair of reference counted
 *              strings
 *
 * Return: Pointer to reference counted string on success, NULL on error
 *
 * Programmer: Quincey Koziol, koziol@ncsa.uiuc.edu
 *
 * Date: August 19, 2005
 *
 *-------------------------------------------------------------------------
 */
static H5RS_str_t *
H5G_build_fullpath_refstr_refstr(const H5RS_str_t *prefix_r, const H5RS_str_t *name_r)
{
    const char *prefix;         /* Pointer to raw string of prefix */
    const char *name;           /* Pointer to raw string of name */
    H5RS_str_t *ret_value;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_build_fullpath_refstr_refstr)

    /* Get the pointer to the prefix */
    prefix = H5RS_get_str(prefix_r);

    /* Get the pointer to the raw src user path */
    name = H5RS_get_str(name_r);

    /* Create reference counted string for path */
    ret_value = H5G_build_fullpath(prefix, name);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_build_fullpath_refstr_refstr() */


/*-------------------------------------------------------------------------
 * Function:    H5G_name_init
 *
 * Purpose:     Set the initial path for a group hierarchy name
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, September 12, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_name_init(H5G_name_t *name, const char *path)
{
    FUNC_ENTER_NOAPI_NOFUNC(H5G_name_init)

    /* Check arguments */
    HDassert(name);

    /* Set the initial paths for a name object */
    name->full_path_r = H5RS_create(path);
    HDassert(name->full_path_r);
    name->user_path_r = H5RS_create(path);
    HDassert(name->user_path_r);
    name->obj_hidden = 0;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5G_name_init() */


/*-------------------------------------------------------------------------
 * Function:	H5G_name_set
 *
 * Purpose:     Set the name of a symbol entry OBJ, located at LOC
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Pedro Vicente, pvn@ncsa.uiuc.edu
 *              Thursday, August 22, 2002
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_name_set(H5G_name_t *loc, H5G_name_t *obj, const char *name)
{
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5G_name_set, FAIL)

    HDassert(loc);
    HDassert(obj);
    HDassert(name);

    /* Free & reset the object's previous paths info (if they exist) */
    H5G_name_free(obj);

    /* Create the object's full path, if a full path exists in the location */
    if(loc->full_path_r) {
        /* Go build the new full path */
        if((obj->full_path_r = H5G_build_fullpath_refstr_str(loc->full_path_r, name)) == NULL)
            HGOTO_ERROR(H5E_SYM, H5E_PATH, FAIL, "can't build user path name")
    } /* end if */

    /* Create the object's user path, if a user path exists in the location */
    if(loc->user_path_r) {
        /* Go build the new user path */
        if((obj->user_path_r = H5G_build_fullpath_refstr_str(loc->user_path_r, name)) == NULL)
            HGOTO_ERROR(H5E_SYM, H5E_PATH, FAIL, "can't build user path name")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_name_set() */


/*-------------------------------------------------------------------------
 * Function:    H5G_name_copy
 *
 * Purpose:     Do a copy of group hier. names
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, September 12, 2005
 *
 * Notes:       'depth' parameter determines how much of the group entry
 *              structure we want to copy.  The depths are:
 *                  H5_COPY_SHALLOW - Copy all the fields from the source
 *                      to the destination, including the user path and
 *                      canonical path. (Destination "takes ownership" of
 *                      user and canonical paths)
 *                  H5_COPY_DEEP - Copy all the fields from the source to
 *                      the destination, deep copying the user and canonical
 *                      paths.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_name_copy(H5G_name_t *dst, const H5G_name_t *src, H5_copy_depth_t depth)
{
    FUNC_ENTER_NOAPI_NOFUNC(H5G_name_copy)

    /* Check arguments */
    HDassert(src);
    HDassert(dst);
#if defined(H5_USING_PURIFY) || !defined(NDEBUG)
    HDassert(dst->full_path_r == NULL);
    HDassert(dst->user_path_r == NULL);
#endif /* H5_USING_PURIFY */
    HDassert(depth == H5_COPY_SHALLOW || depth == H5_COPY_DEEP);

    /* Copy the top level information */
    HDmemcpy(dst, src, sizeof(H5G_name_t));

    /* Deep copy the names */
    if(depth == H5_COPY_DEEP) {
        dst->full_path_r = H5RS_dup(src->full_path_r);
        dst->user_path_r = H5RS_dup(src->user_path_r);
    } else {
        /* Discarding 'const' qualifier OK - QAK */
        H5G_name_reset((H5G_name_t *)src);
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5G_name_copy() */


/*-------------------------------------------------------------------------
 * Function:    H5G_get_name
 *
 * Purpose:     Gets a name of an object from its ID.
 *
 * Notes:	Internal routine for H5Iget_name().

 * Return:	Success:	Non-negative, length of name
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, December 13, 2005
 *
 * Modifications: Leon Arber
 * 		  Oct. 18, 2006
 * 		  Added functionality to get the name for a reference.
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5G_get_name(hid_t id, char *name/*out*/, size_t size)
{
    H5G_loc_t     loc;          /* Object location */
    ssize_t       ret_value = FAIL;
    

    FUNC_ENTER_NOAPI(H5G_get_name, FAIL)

    /* get object location */
    if(H5G_loc(id, &loc) >= 0) {
        size_t        len = 0;

        if(loc.path->user_path_r != NULL && loc.path->obj_hidden == 0) {
            len = H5RS_len(loc.path->user_path_r);

            if(name) {
                HDstrncpy(name, H5RS_get_str(loc.path->user_path_r), MIN(len + 1, size));
                if(len >= size)
                    name[size-1] = '\0';
            } /* end if */
        }
	else
	{
	    hid_t	  file;

	    if((file = H5I_get_file_id(id)) < 0)
		HGOTO_ERROR(H5E_ATOM, H5E_CANTGET, FAIL, "can't retrieve file ID")


	    len = H5G_get_refobj_name((loc.oloc)->addr, file, name, size);
	
	    H5Fclose(file);
	}
        /* Set return value */
        ret_value = (ssize_t)len;
    } /* end if */

done: 
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_get_name() */


/*-------------------------------------------------------------------------
 * Function:	H5G_name_reset
 *
 * Purpose:	Reset a group hierarchy name to an empty state
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, September 12, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_name_reset(H5G_name_t *name)
{
    FUNC_ENTER_NOAPI_NOFUNC(H5G_name_reset)

    /* Check arguments */
    HDassert(name);

    /* Clear the group hier. name to an empty state */
    HDmemset(name, 0, sizeof(H5G_name_t));

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5G_name_reset() */


/*-------------------------------------------------------------------------
 * Function:	H5G_name_free
 *
 * Purpose:	Free the 'ID to name' buffers.
 *
 * Return:	Success
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: August 22, 2002
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_name_free(H5G_name_t *name)
{
    FUNC_ENTER_NOAPI_NOFUNC(H5G_name_free)

    /* Check args */
    HDassert(name);

    if(name->full_path_r) {
        H5RS_decr(name->full_path_r);
        name->full_path_r = NULL;
    } /* end if */
    if(name->user_path_r) {
        H5RS_decr(name->user_path_r);
        name->user_path_r = NULL;
    } /* end if */
    name->obj_hidden = 0;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5G_name_free() */


/*-------------------------------------------------------------------------
 * Function:    H5G_name_move_path
 *
 * Purpose:     Update a user or canonical path after an object moves
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, December 13, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_name_move_path(H5RS_str_t **path_r_ptr, const char *full_suffix, const char *src_path,
    const char *dst_path)
{
    const char *path;                   /* Path to update */
    size_t path_len;                    /* Length of path */
    size_t full_suffix_len;             /* Length of full suffix */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_name_move_path)

    /* Check arguments */
    HDassert(path_r_ptr && *path_r_ptr);
    HDassert(full_suffix);
    HDassert(src_path);
    HDassert(dst_path);

    /* Get pointer to path to update */
    path = H5RS_get_str(*path_r_ptr);
    HDassert(path);

    /* Check if path needs to be updated */
    full_suffix_len = HDstrlen(full_suffix);
    path_len = HDstrlen(path);
    if(full_suffix_len < path_len) {
        const char *dst_suffix;         /* Destination suffix that changes */
        const char *src_suffix;         /* Source suffix that changes */
        const char *path_prefix;        /* Prefix for path */
        size_t path_prefix_len;         /* Length of path prefix */
        const char *path_prefix2;       /* 2nd prefix for path */
        size_t path_prefix2_len;        /* Length of 2nd path prefix */
        const char *common_prefix;      /* Common prefix for src & dst paths */
        size_t common_prefix_len;       /* Length of common prefix */
        char *new_path;                 /* Pointer to new path */
        size_t new_path_len;            /* Length of new path */


        /* Compute path prefix before full suffix*/
        path_prefix = path;
        path_prefix_len = path_len - full_suffix_len;

        /* Determine the common prefix for src & dst paths */
        common_prefix = src_path;
        common_prefix_len = 0;
        /* Find first character that is different */
        while(*(src_path + common_prefix_len) == *(dst_path + common_prefix_len))
            common_prefix_len++;
        /* Back up to previous '/' */
        while(*(common_prefix + common_prefix_len) != '/')
            common_prefix_len--;
        /* Include '/' */
        common_prefix_len++;

        /* Determine source suffix */
        src_suffix = src_path + (common_prefix_len - 1);

        /* Determine destination suffix */
        dst_suffix = dst_path + (common_prefix_len - 1);

        /* Compute path prefix before src suffix*/
        path_prefix2 = path;
        path_prefix2_len = path_prefix_len - HDstrlen(src_suffix);

        /* Allocate space for the new path */
        new_path_len = path_prefix2_len + HDstrlen(dst_suffix) + full_suffix_len;
        if(NULL == (new_path = (char *)H5FL_BLK_MALLOC(str_buf, new_path_len + 1)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

        /* Create the new path */
        if(path_prefix2_len > 0) {
            HDstrncpy(new_path, path_prefix2, path_prefix2_len);
            HDstrcpy(new_path + path_prefix2_len, dst_suffix);
        } /* end if */
        else
            HDstrcpy(new_path, dst_suffix);
        if(full_suffix_len > 0)
            HDstrcat(new_path, full_suffix);

        /* Release previous path */
        H5RS_decr(*path_r_ptr);

        /* Take ownership of the new full path */
        *path_r_ptr = H5RS_own(new_path);
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_name_move_path() */


/*-------------------------------------------------------------------------
 * Function: H5G_name_replace_cb
 *
 * Purpose: H5I_search callback function to replace group entry names
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: June 5, 2002
 *
 *-------------------------------------------------------------------------
 */
static int
H5G_name_replace_cb(void *obj_ptr, hid_t obj_id, void *key)
{
    const H5G_names_t *names = (const H5G_names_t *)key;        /* Get operation's information */
    H5O_loc_t *oloc;            /* Object location for object that the ID refers to */
    H5G_name_t *obj_path;       /* Pointer to group hier. path for obj */
    H5F_t *top_obj_file;        /* Top file in object's mounted file hier. */
    hbool_t obj_in_child = FALSE;   /* Flag to indicate that the object is in the child mount hier. */
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_name_replace_cb)

    HDassert(obj_ptr);

    /* Get the symbol table entry */
    switch(H5I_get_type(obj_id)) {
        case H5I_GROUP:
            oloc = H5G_oloc((H5G_t *)obj_ptr);
            obj_path = H5G_nameof((H5G_t *)obj_ptr);
            break;

        case H5I_DATASET:
            oloc = H5D_oloc((H5D_t *)obj_ptr);
            obj_path = H5D_nameof((H5D_t *)obj_ptr);
            break;

        case H5I_DATATYPE:
            /* Avoid non-named datatypes */
            if(!H5T_is_named((H5T_t *)obj_ptr))
                HGOTO_DONE(SUCCEED)     /* Do not exit search over IDs */

            oloc = H5T_oloc((H5T_t *)obj_ptr);
            obj_path = H5T_nameof((H5T_t *)obj_ptr);
            break;

        default:
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "unknown data object")
    } /* end switch */
    HDassert(oloc);
    HDassert(obj_path);

    /* Check if the object has a full path still */
    if(!obj_path->full_path_r)
        HGOTO_DONE(SUCCEED)     /* No need to look at object, it's path is already invalid */

    /* Find the top file in object's mount hier. */
    if(oloc->file->mtab.parent) {
        /* Check if object is in child file (for mount & unmount operations) */
        if(names->dst_loc && oloc->file->shared == names->dst_loc->oloc->file->shared)
            obj_in_child = TRUE;

        /* Find the "top" file in the chain of mounted files */
        top_obj_file = oloc->file->mtab.parent;
        while(top_obj_file->mtab.parent != NULL) {
            /* Check if object is in child mount hier. (for mount & unmount operations) */
            if(names->dst_loc && top_obj_file->shared == names->dst_loc->oloc->file->shared)
                obj_in_child = TRUE;

            top_obj_file = top_obj_file->mtab.parent;
        } /* end while */
    } /* end if */
    else
        top_obj_file = oloc->file;

    /* Check if object is in top of child mount hier. (for mount & unmount operations) */
    if(names->dst_loc && top_obj_file->shared == names->dst_loc->oloc->file->shared)
        obj_in_child = TRUE;

    /* Check if the object is in same file mount hier. */
    if(top_obj_file->shared != names->top_loc_file->shared)
        HGOTO_DONE(SUCCEED)     /* No need to look at object, it's path is already invalid */

    switch(names->op) {
        /*-------------------------------------------------------------------------
        * H5G_NAME_MOUNT
        *-------------------------------------------------------------------------
        */
        case H5G_NAME_MOUNT:
            /* Check if object is in child mount hier. */
            if(obj_in_child) {
                const char *full_path;      /* Full path of current object */
                const char *src_path;       /* Full path of source object */
                char *new_full_path;        /* New full path of object */
                size_t new_full_len;        /* Length of new full path */

                /* Get pointers to paths of interest */
                full_path = H5RS_get_str(obj_path->full_path_r);
                src_path = H5RS_get_str(names->loc->path->full_path_r);

                /* Build new full path */

                /* Allocate space for the new full path */
                new_full_len = HDstrlen(src_path) + HDstrlen(full_path);
                if(NULL == (new_full_path = (char *)H5FL_BLK_MALLOC(str_buf, new_full_len + 1)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

                /* Create the new full path */
                HDstrcpy(new_full_path, src_path);
                HDstrcat(new_full_path, full_path);

                /* Release previous full path */
                H5RS_decr(obj_path->full_path_r);

                /* Take ownership of the new full path */
                obj_path->full_path_r = H5RS_own(new_full_path);
            } /* end if */
            /* Object must be in parent mount file hier. */
            else {
                /* Check if the source is along the entry's path */
                /* (But not actually the entry itself) */
                if(H5G_common_path(obj_path->full_path_r, names->loc->path->full_path_r) &&
                        H5RS_cmp(obj_path->full_path_r, names->loc->path->full_path_r)) {
                    /* Hide the user path */
                    (obj_path->obj_hidden)++;
                } /* end if */
            } /* end else */
            break;

        /*-------------------------------------------------------------------------
        * H5G_NAME_UNMOUNT
        *-------------------------------------------------------------------------
        */
        case H5G_NAME_UNMOUNT:
            if(obj_in_child) {
                const char *full_path;      /* Full path of current object */
                const char *full_suffix;    /* Full path after source path */
                const char *src_path;       /* Full path of source object */
                char *new_full_path;        /* New full path of object */

                /* Get pointers to paths of interest */
                full_path = H5RS_get_str(obj_path->full_path_r);
                src_path = H5RS_get_str(names->loc->path->full_path_r);

                /* Construct full path suffix */
                full_suffix = full_path + HDstrlen(src_path);

                /* Build new full path */

                /* Create the new full path */
                if(NULL == (new_full_path = (char *)H5FL_BLK_MALLOC(str_buf, HDstrlen(full_suffix) + 1)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
                HDstrcpy(new_full_path, full_suffix);

                /* Release previous full path */
                H5RS_decr(obj_path->full_path_r);

                /* Take ownership of the new full path */
                obj_path->full_path_r = H5RS_own(new_full_path);

                /* Check if the object's user path should be invalidated */
                if(obj_path->user_path_r && HDstrlen(new_full_path) < (size_t)H5RS_len(obj_path->user_path_r)) {
                    /* Free user path */
                    H5RS_decr(obj_path->user_path_r);
                    obj_path->user_path_r = NULL;
                } /* end if */
            } /* end if */
            else {
                /* Check if file being unmounted was hiding the object */
                if(H5G_common_path(obj_path->full_path_r, names->loc->path->full_path_r) &&
                        H5RS_cmp(obj_path->full_path_r, names->loc->path->full_path_r)) {
                    /* Un-hide the user path */
                    (obj_path->obj_hidden)--;
                } /* end if */
            } /* end else */
            break;

        /*-------------------------------------------------------------------------
        * H5G_NAME_UNLINK
        *-------------------------------------------------------------------------
        */
        case H5G_NAME_UNLINK:
            /* Check if the location being unlinked is in the path for the current object */
            if(H5G_common_path(obj_path->full_path_r, names->loc->path->full_path_r)) {
                /* Free paths for object */
                H5G_name_free(obj_path);
            } /* end if */
            break;

        /*-------------------------------------------------------------------------
        * H5G_NAME_MOVE
        *-------------------------------------------------------------------------
        */
        case H5G_NAME_MOVE: /* H5Gmove case, check for relative names case */
            /* Check if the src object moved is in the current object's path */
            if(H5G_common_path(obj_path->full_path_r, names->loc->path->full_path_r)) {
                const char *full_path;      /* Full path of current object */
                const char *full_suffix;    /* Suffix of full path, after src_path */
                char *new_full_path;        /* New full path of object */
                size_t new_full_len;        /* Length of new full path */
                H5RS_str_t *src_path_r;     /* Full path of source name */
                const char *src_path;       /* Full path of source object */
                H5RS_str_t *dst_path_r;     /* Full path of destination name */
                const char *dst_path;       /* Full path of destination object */

                /* Sanity check */
                HDassert(*(H5RS_get_str(names->loc->path->full_path_r)) == '/');
                HDassert(names->dst_name);

                /* Make certain that the source and destination names are full (not relative) paths */
                src_path_r = H5RS_dup(names->loc->path->full_path_r);
                if(*(H5RS_get_str(names->dst_name)) != '/') {
                    HDassert(names->dst_loc && names->dst_loc->path);
                    /* Create reference counted string for full dst path */
                    if((dst_path_r = H5G_build_fullpath_refstr_refstr(names->dst_loc->path->full_path_r, names->dst_name)) == NULL)
                        HGOTO_ERROR(H5E_SYM, H5E_PATH, FAIL, "can't build destination path name")
                } /* end if */
                else
                    dst_path_r = H5RS_dup(names->dst_name);

                /* Get pointers to paths of interest */
                full_path = H5RS_get_str(obj_path->full_path_r);
                src_path = H5RS_get_str(src_path_r);
                dst_path = H5RS_get_str(dst_path_r);

                /* Get pointer to "full suffix" */
                full_suffix = full_path + HDstrlen(src_path);

                /* Update the user path, if one exists */
                if(obj_path->user_path_r)
                    if(H5G_name_move_path(&(obj_path->user_path_r), full_suffix, src_path, dst_path) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_PATH, FAIL, "can't build user path name")

                /* Build new full path */

                /* Allocate space for the new full path */
                new_full_len = HDstrlen(dst_path) + HDstrlen(full_suffix);
                if(NULL == (new_full_path = (char *)H5FL_BLK_MALLOC(str_buf, new_full_len + 1)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

                /* Create the new full path */
                HDstrcpy(new_full_path, dst_path);
                HDstrcat(new_full_path, full_suffix);

                /* Release previous full path */
                H5RS_decr(obj_path->full_path_r);

                /* Take ownership of the new full path */
                obj_path->full_path_r = H5RS_own(new_full_path);

                /* Release source & destination full paths */
                H5RS_decr(src_path_r);
                H5RS_decr(dst_path_r);
            } /* end if */
            break;

        default:
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid operation")
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5G_name_replace_cb() */


/*-------------------------------------------------------------------------
 * Function: H5G_name_replace
 *
 * Purpose: Search the list of open IDs and replace names according to a
 *              particular operation.  The operation occured on the LOC
 *              entry.  The new name (if there is one) is DST_NAME.
 *              Additional entry location information (currently only needed
 *              for the 'move' operation) is passed in DST_LOC.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: June 11, 2002
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_name_replace(H5G_obj_t type, H5G_loc_t *loc,
    H5RS_str_t *dst_name, H5G_loc_t *dst_loc, H5G_names_op_t op)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5G_name_replace, FAIL)

    /* Check arguments */
    HDassert(loc && loc->path);

    /* Check if the object we are manipulating has a path */
    if(loc->path->full_path_r) {
        unsigned search_group = 0;  /* Flag to indicate that groups are to be searched */
        unsigned search_dataset = 0;  /* Flag to indicate that datasets are to be searched */
        unsigned search_datatype = 0; /* Flag to indicate that datatypes are to be searched */

        /* Determine which types of IDs need to be operated on */
        switch(type) {
            /* Object is a group  */
            case H5G_GROUP:
                /* Search and replace names through group IDs */
                search_group = 1;
                break;

            /* Object is a dataset */
            case H5G_DATASET:
                /* Search and replace names through dataset IDs */
                search_dataset = 1;
                break;

            /* Object is a named datatype */
            case H5G_TYPE:
                /* Search and replace names through datatype IDs */
                search_datatype = 1;
                break;

            case H5G_UNKNOWN:   /* We pass H5G_UNKNOWN as object type when we need to search all IDs */
            case H5G_LINK:      /* Symbolic links might resolve to any object, so we need to search all IDs */
                /* Check if we will need to search groups */
                if(H5I_nmembers(H5I_GROUP) > 0)
                    search_group = 1;

                /* Check if we will need to search datasets */
                if(H5I_nmembers(H5I_DATASET) > 0)
                    search_dataset = 1;

                /* Check if we will need to search datatypes */
                if(H5I_nmembers(H5I_DATATYPE) > 0)
                    search_datatype = 1;
                break;

            case H5G_UDLINK:
              /* User-defined links automatically wipe out names (because it
               * would be too much work to track them), so there's no point
               * in searching them.
               */
              break;

            default:
                HGOTO_ERROR(H5E_SYM, H5E_BADTYPE, FAIL, "not valid object type")
        } /* end switch */

        /* Check if we need to operate on the objects affected */
        if(search_group || search_dataset || search_datatype) {
            H5G_names_t names;          /* Structure to hold operation information for callback */
            H5F_t *top_loc_file;        /* Top file in src location's mounted file hier. */

            /* Find top file in src location's mount hierarchy */
            if(loc->oloc->file->mtab.parent) {
                /* Find the "top" file in the chain of mounted files for the location */
                top_loc_file = loc->oloc->file->mtab.parent;
                while(top_loc_file->mtab.parent != NULL)
                    top_loc_file = top_loc_file->mtab.parent;
            } /* end if */
            else
                top_loc_file = loc->oloc->file;

            /* Set up common information for callback */
            names.loc = loc;
            names.top_loc_file = top_loc_file;
            names.dst_loc = dst_loc;
            names.dst_name = dst_name;
            names.op = op;

            /* Search through group IDs */
            if(search_group)
                H5I_search(H5I_GROUP, H5G_name_replace_cb, &names);

            /* Search through dataset IDs */
            if(search_dataset)
                H5I_search(H5I_DATASET, H5G_name_replace_cb, &names);

            /* Search through datatype IDs */
            if(search_datatype)
                H5I_search(H5I_DATATYPE, H5G_name_replace_cb, &names);
        } /* end if */
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_name_replace() */


/*-------------------------------------------------------------------------
 * Function:    H5G_ref_path_table_put
 *
 * Purpose:     Enter the 'obj' with 'path' in the table (assumes its not
 *              already there)
 *
 *              Create an object reference, pte, and store them
 *              in the table.
 *
 *              NOTE: Takes ownership of the path name string passed in!
 *
 * Return:      Non-negative on success, negative on failure
 *
 * Programmer:  REMcG
 *
 * Modifications:
  * 		Leon Arber, Oct. 25, 2006.  Moved into H5G from h5ls
 * 		tools lib for looking up path to reference.
*
 *-------------------------------------------------------------------------
 */
static int
H5G_ref_path_table_put(const char *path, haddr_t objno)
{
    ref_path_node_t *new_node;
    int ret_value = 0;

    HDassert(ref_path_table);
    HDassert(path);

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_ref_path_table_put)
	
    if((new_node = H5MM_malloc(sizeof(ref_path_node_t))) == NULL)
	ret_value = -1;

    new_node->objno = objno;
    new_node->path = path;

    ret_value = H5SL_insert(ref_path_table, new_node, &(new_node->objno));

    FUNC_LEAVE_NOAPI(ret_value)
}



/*-------------------------------------------------------------------------
 * Function:    H5G_init_ref_path_table
 *
 * Purpose:     Enter the root group ("/") into the path table
 *
 * Return:      Non-negative on success, negative on failure
 *
 * Programmer:  Quincey Koziol
 *
 * Modifications:
  * 		Leon Arber, Oct. 25, 2006.  Moved into H5G from h5ls
 * 		tools lib for looking up path to reference.
*
 *-------------------------------------------------------------------------
 */
static int
H5G_init_ref_path_table(hid_t fid)
{
    H5G_stat_t sb;
    haddr_t objno;              /* Compact form of object's location */
    char *root_path;
    int ret_value = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_init_ref_path_table)

    /* Create skip list to store reference path information */
    if((ref_path_table = H5SL_create(H5SL_TYPE_HADDR, 0.5, (size_t)16))==NULL)
        ret_value = -1;

    if((root_path = HDstrdup("/")) == NULL)
        ret_value = -1;

    if(H5Gget_objinfo(fid, "/", TRUE, &sb)<0) {
        /* fatal error? */
        HDfree(root_path);
        ret_value = -1;
    }
    objno = (haddr_t)sb.objno[0] | ((haddr_t)sb.objno[1] << (8 * sizeof(long)));

    /* Insert into table (takes ownership of path) */
    H5G_ref_path_table_put(root_path, objno);
    
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:    H5G_free_ref_path_info
 *
 * Purpose:     Free the key for a reference path table node
 *
 * Return:      Non-negative on success, negative on failure
 *
 * Programmer:  Quincey Koziol
 *
 * Modifications:
 * 		Leon Arber, Oct. 25, 2006.  Moved into H5G from h5ls
 * 		tools lib for looking up path to reference.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_free_ref_path_info(void *item, void UNUSED *key, void UNUSED *operator_data/*in,out*/)
{
    ref_path_node_t *node = (ref_path_node_t *)item;
    
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_free_ref_path_table)

    H5MM_xfree(node->path);
    H5MM_xfree(node);

    FUNC_LEAVE_NOAPI(SUCCEED)
}

/*-------------------------------------------------------------------------
 * Function:    H5G_term_ref_path_table
 *
 * Purpose:     Terminate the reference path table
 *
 * Return:      Non-negative on success, negative on failure
 *
 * Programmer:  Quincey Koziol
 *
 * Modifications:
 * * 		Leon Arber, Oct. 25, 2006.  Moved into H5G from h5ls
 * 		tools lib for looking up path to reference.

 *-------------------------------------------------------------------------
 */
static int
H5G_term_ref_path_table(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_term_ref_path_table)

    /* Destroy reference path table, freeing all memory */
    if(ref_path_table)
        H5SL_destroy(ref_path_table, H5G_free_ref_path_info, NULL);

    FUNC_LEAVE_NOAPI(0);
}



/*-------------------------------------------------------------------------
 * Function:    H5G_ref_path_table_find
 *
 * Purpose:     Looks up a table entry given a object number.
 *              Used during construction of the table.
 *
 * Return:      TRUE/FALSE on success, can't fail
 *
 * Programmer:  Quincey Koziol
 *
 * Modifications:
  * 		Leon Arber, Oct. 25, 2006.  Moved into H5G from h5ls
 * 		tools lib for looking up path to reference.
*
 *-------------------------------------------------------------------------
 */
static hbool_t
H5G_ref_path_table_find(haddr_t objno)
{
    hbool_t ret_value;

    HDassert(ref_path_table);
    
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_ref_path_table_find)

    if(H5SL_search(ref_path_table, &objno) == NULL)
        ret_value = FALSE;
    else
        ret_value = TRUE;

    FUNC_LEAVE_NOAPI(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    H5G_refname_iterator
 *
 * Purpose:     The iterator which traverses all objects in a file looking for
 * 		one that matches the object that is being looked for.
 *
 * Return:      1 on success, 0 to continue searching, negative on failure.
 *
 * Programmer:  Leon Arber, Nov 1, 2006.
 *
 * Modifications:
*
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_refname_iterator(hid_t group, const char *name, void *_iter)
{
    H5G_stat_t sb;
    iter_t *iter = (iter_t*)_iter;
    H5G_loc_t   loc;
    int ret_value = 0, last_obj;
    
    FUNC_ENTER_NOAPI_NOINIT(H5G_refname_iterator)
    
    if(H5G_loc(group, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")

    if(H5G_get_objinfo(&loc, name, 0, &sb, H5AC_ind_dxpl_id) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_CANTINIT, FAIL, "cannot stat object")

    if (sb.type<0 || sb.type>=H5G_NTYPES) {
	/* We hit an unknown type during iteration.  Let's continue anyway */
	ret_value = 0;
    }
    else
    {
	haddr_t objno = (haddr_t)sb.objno[0] | ((haddr_t)sb.objno[1] << (8 * sizeof(long)));

	/* See if we've seen this object before */
	if (H5G_ref_path_table_find(objno)) 
	    HGOTO_DONE(ret_value)
	else
	{
	    /* If not, we add it to the list of visited objects if it's ref count is > 1*/
	    if(H5I_get_ref(group) > 1)
	    {
		/* We don't really care about the path to this object in the table, since we're only going
		 * to be matching on objno */
		if(H5G_ref_path_table_put(NULL, objno) < 0)
		    HGOTO_ERROR(H5E_ARGS, H5E_CANTINIT, FAIL, "couldn't insert object into ref path table")
	    }

	    ret_value = 0;
	}

	if(iter->obj == objno)
	{
	    /* We found a match so we return immediately */
	    iter->container = H5MM_realloc(iter->container, HDstrlen(iter->container)+HDstrlen(name)+2);
	    iter->container = HDstrcat(iter->container,name); 

	    ret_value = 1;
	}
	else if(sb.type == H5G_GROUP)
	{
	    /* It's a group, so we recurse into it */
	    int len = HDstrlen(iter->container);
	    iter->container = H5MM_realloc(iter->container, HDstrlen(iter->container)+HDstrlen(name)+2);
	    iter->container = HDstrcat(iter->container,name); 
	    HDstrcpy(iter->container+HDstrlen(iter->container), "/");
		
	    ret_value = H5G_obj_iterate(iter->file, iter->container, H5_ITER_INC, 0, &last_obj, H5G_refname_iterator, _iter, H5AC_ind_dxpl_id);
	    
	    /* If we didn't find the object, truncate the name to not include it anymore */
	    if(!ret_value)
		iter->container[len] = '\0'; 
	}
    }
    /* Close the object. */
done:
    FUNC_LEAVE_NOAPI(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    H5G_get_refobj_name
 *
 * Purpose:     Tries to figure out the path to a reference.
 *
 * Return:      returns size of path name, and copies it into buffer
 * 		pointed to by name if that buffer is big enough.
 * 		0 if it cannot find the path
 * 		negative on failure.
 *
 * Programmer:  Leon Arber, Nov 1, 2006.
 *
 * Modifications:
*
 *-------------------------------------------------------------------------
 */

static ssize_t
H5G_get_refobj_name(haddr_t id, hid_t file, char* name, size_t size)
{
    const char  *oname = "/";
    iter_t iter;
    ssize_t ret_value=0;
    int last_obj = 0;

    FUNC_ENTER_NOAPI_NOINIT(H5G_get_refobj_name)

    H5G_init_ref_path_table(file);
    iter.container = H5MM_malloc(strlen(oname)+1);
    HDstrcpy(iter.container, oname);
    iter.file = file;
    iter.obj = id;
    
    if((ret_value = H5G_obj_iterate(file, oname, H5_ITER_INC, 0, &last_obj, H5G_refname_iterator, &iter, H5AC_ind_dxpl_id))< 0)
        HGOTO_ERROR(H5E_SYM, H5E_BADITER, FAIL, "group iteration failed while looking for object name")
    else if(ret_value == 0)
	HGOTO_DONE(ret_value)
    else if(HDstrlen(iter.container) >= size)
	ret_value = HDstrlen(iter.container) + 1;
    else
    {
	HDstrncpy(name, iter.container, size);
	ret_value = HDstrlen(iter.container) + 1;
    }
   
done:    
    H5G_term_ref_path_table();
    H5MM_xfree(iter.container);

    FUNC_LEAVE_NOAPI(ret_value)
}

