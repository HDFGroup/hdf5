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
#include "H5MMprivate.h"	/* Memory management			*/

/* Private typedefs */

/* Struct used by change name callback function */
typedef struct H5G_names_t {
    H5G_loc_t	*loc;
    H5RS_str_t *src_name;
    H5G_loc_t	*src_loc;
    H5RS_str_t *dst_name;
    H5G_loc_t	*dst_loc;
    H5G_names_op_t op;
} H5G_names_t;

/* Private macros */

/* Local variables */

/* Declare extern the PQ free list for the wrapped strings */
H5FL_BLK_EXTERN(str_buf);

/* PRIVATE PROTOTYPES */
static htri_t H5G_common_path(const H5RS_str_t *fullpath_r, const H5RS_str_t *prefix_r);
static H5RS_str_t *H5G_build_fullpath(const char *prefix, const char *name);
static H5RS_str_t *H5G_build_fullpath_refstr_refstr(const H5RS_str_t *prefix_r, const H5RS_str_t *name_r);
static H5RS_str_t *H5G_build_fullpath_refstr_str(H5RS_str_t *path_r, const char *name);
static int H5G_name_replace_cb(void *obj_ptr, hid_t obj_id, void *key);


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
    if(NULL == (full_path = H5FL_BLK_MALLOC(str_buf, path_len + 1)))
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
 * Function: H5G_name_concat_refstr_refstr
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
    name->user_path_r=H5RS_create(path);
    HDassert(name->user_path_r);
    name->canon_path_r=H5RS_create(path);
    HDassert(name->canon_path_r);
    name->user_path_hidden=0;

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

    /* Create the object's user path, if a user path exists in the location */
    if(loc->user_path_r) {
        /* Go build the new user path */
        if((obj->user_path_r = H5G_build_fullpath_refstr_str(loc->user_path_r, name)) == NULL)
            HGOTO_ERROR(H5E_SYM, H5E_PATH, FAIL, "can't build user path name")
    } /* end if */

    /* Create the object's canonical path, if a canonical path exists in the location */
    if(loc->canon_path_r) {
        /* Go build the new canonical path */
        if((obj->canon_path_r = H5G_build_fullpath_refstr_str(loc->canon_path_r, name)) == NULL)
            HGOTO_ERROR(H5E_SYM, H5E_PATH, FAIL, "can't build canonical path name")
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
 *              structure we want to copy.  The new depths are:
 *                  H5G_COPY_SHALLOW - Copy all the fields from the source
 *                      to the destination, including the user path and
 *                      canonical path. (Destination "takes ownership" of
 *                      user and canonical paths)
 *                  H5G_COPY_CANON - Deep copy the canonical path and leave
 *                      the user path alone
 *                  H5G_COPY_DEEP - Copy all the fields from the source to
 *                      the destination, deep copying the user and canonical
 *                      paths.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_name_copy(H5G_name_t *dst, const H5G_name_t *src, H5G_copy_depth_t depth)
{
    H5RS_str_t *tmp_user_path_r = NULL;   /* Temporary string pointer for entry's user path */

    FUNC_ENTER_NOAPI_NOFUNC(H5G_name_copy)

    /* Check arguments */
    HDassert(src);
    HDassert(dst);
    HDassert(depth == H5G_COPY_SHALLOW || depth == H5G_COPY_DEEP || depth == H5G_COPY_CANON);

    /* If only copying the canonical path, keep the old user path */
    if(depth == H5G_COPY_CANON) {
        tmp_user_path_r = dst->user_path_r;
        if(dst->canon_path_r)
            H5RS_decr(dst->canon_path_r);
    } /* end if */

    /* Copy the top level information */
    HDmemcpy(dst, src, sizeof(H5G_name_t));

    /* Deep copy the names */
    if(depth == H5G_COPY_DEEP) {
        dst->user_path_r = H5RS_dup(src->user_path_r);
        dst->canon_path_r = H5RS_dup(src->canon_path_r);
    } else if(depth == H5G_COPY_CANON) {
        dst->user_path_r = tmp_user_path_r;
        dst->canon_path_r = H5RS_dup(src->canon_path_r);
    } else {
        /* Discarding 'const' qualifier OK - QAK */
        H5G_name_reset((H5G_name_t *)src);
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5G_name_copy() */


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

    if(name->user_path_r) {
        H5RS_decr(name->user_path_r);
        name->user_path_r=NULL;
    } /* end if */
    if(name->canon_path_r) {
        H5RS_decr(name->canon_path_r);
        name->canon_path_r=NULL;
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5G_name_free() */


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
    H5F_t *top_ent_file;        /* Top file in entry's mounted file chain */
    H5F_t *top_loc_file;        /* Top file in location's mounted file chain */
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

    switch(names->op) {
        /*-------------------------------------------------------------------------
        * OP_MOUNT
        *-------------------------------------------------------------------------
        */
        case OP_MOUNT:
	    if(obj_path->user_path_r) {
		if(oloc->file->mtab.parent && H5RS_cmp(obj_path->user_path_r, obj_path->canon_path_r)) {
		    /* Find the "top" file in the chain of mounted files */
		    top_ent_file = oloc->file->mtab.parent;
		    while(top_ent_file->mtab.parent != NULL)
			top_ent_file = top_ent_file->mtab.parent;
		} /* end if */
		else
		    top_ent_file = oloc->file;

		/* Check for entry being in correct file (or mounted file) */
		if(top_ent_file->shared == names->loc->oloc->file->shared) {
		    /* Check if the source is along the entry's path */
		    /* (But not actually the entry itself) */
		    if(H5G_common_path(obj_path->user_path_r, names->src_name) &&
			    H5RS_cmp(obj_path->user_path_r, names->src_name) != 0) {
			/* Hide the user path */
			(obj_path->user_path_hidden)++;
		    } /* end if */
		} /* end if */
	    } /* end if */
            break;

        /*-------------------------------------------------------------------------
        * OP_UNMOUNT
        *-------------------------------------------------------------------------
        */
        case OP_UNMOUNT:
	    if(obj_path->user_path_r) {
		if(oloc->file->mtab.parent) {
		    /* Find the "top" file in the chain of mounted files for the entry */
		    top_ent_file = oloc->file->mtab.parent;
		    while(top_ent_file->mtab.parent != NULL)
			top_ent_file=top_ent_file->mtab.parent;
		} /* end if */
		else
		    top_ent_file = oloc->file;

		if(names->loc->oloc->file->mtab.parent) {
		    /* Find the "top" file in the chain of mounted files for the location */
		    top_loc_file = names->loc->oloc->file->mtab.parent;
		    while(top_loc_file->mtab.parent != NULL)
			top_loc_file = top_loc_file->mtab.parent;
		} /* end if */
		else
		    top_loc_file = names->loc->oloc->file;

		/* If the ID's entry is not in the file we operated on, skip it */
		if(top_ent_file->shared == top_loc_file->shared) {
		    if(obj_path->user_path_hidden) {
			if(H5G_common_path(obj_path->user_path_r, names->src_name)) {
			    /* Un-hide the user path */
			    (obj_path->user_path_hidden)--;
			} /* end if */
		    } /* end if */
		    else {
			if(H5G_common_path(obj_path->user_path_r, names->src_name)) {
			    /* Free user path */
			    H5RS_decr(obj_path->user_path_r);
			    obj_path->user_path_r = NULL;
			} /* end if */
		    } /* end else */
		} /* end if */
	    } /* end if */
            break;

        /*-------------------------------------------------------------------------
        * OP_UNLINK
        *-------------------------------------------------------------------------
        */
        case OP_UNLINK:
            /* If the ID's entry is not in the file we operated on, skip it */
            if(oloc->file->shared == names->loc->oloc->file->shared && 
                    names->loc->path->canon_path_r && obj_path->canon_path_r && obj_path->user_path_r) {
                /* Check if we are referring to the same object */
                if(H5F_addr_eq(oloc->addr, names->loc->oloc->addr)) {
                    /* Check if the object was opened with the same canonical path as the one being moved */
                    if(H5RS_cmp(obj_path->canon_path_r, names->loc->path->canon_path_r) == 0) {
                        /* Free user path */
			H5RS_decr(obj_path->user_path_r);
			obj_path->user_path_r=NULL;
                    } /* end if */
                } /* end if */
                else {
                    /* Check if the location being unlinked is in the canonical path for the current object */
                    if(H5G_common_path(obj_path->canon_path_r, names->loc->path->canon_path_r)) {
                        /* Free user path */
			H5RS_decr(obj_path->user_path_r);
			obj_path->user_path_r=NULL;
                    } /* end if */
                } /* end else */
            } /* end if */
            break;

        /*-------------------------------------------------------------------------
        * OP_MOVE
        *-------------------------------------------------------------------------
        */
        case OP_MOVE: /* H5Gmove case, check for relative names case */
            /* If the ID's entry is not in the file we operated on, skip it */
            if(oloc->file->shared == names->loc->oloc->file->shared) {
		if(obj_path->user_path_r && names->loc->path->user_path_r &&
			names->src_loc->path->user_path_r && names->dst_loc->path->user_path_r) {
		    H5RS_str_t *src_path_r; /* Full user path of source name */
		    H5RS_str_t *dst_path_r; /* Full user path of destination name */
		    H5RS_str_t *canon_src_path_r;   /* Copy of canonical part of source path */
		    H5RS_str_t *canon_dst_path_r;   /* Copy of canonical part of destination path */

		    /* Sanity check */
		    HDassert(names->src_name);
		    HDassert(names->dst_name);

		    /* Make certain that the source and destination names are full (not relative) paths */
		    if(*(H5RS_get_str(names->src_name)) != '/') {
			/* Create reference counted string for full src path */
			if((src_path_r = H5G_build_fullpath_refstr_refstr(names->src_loc->path->user_path_r, names->src_name)) == NULL)
			    HGOTO_ERROR(H5E_SYM, H5E_PATH, FAIL, "can't build source path name")
		    } /* end if */
		    else
                        src_path_r = H5RS_dup(names->src_name);
		    if(*(H5RS_get_str(names->dst_name)) != '/') {
			/* Create reference counted string for full dst path */
			if((dst_path_r = H5G_build_fullpath_refstr_refstr(names->dst_loc->path->user_path_r, names->dst_name)) == NULL)
			    HGOTO_ERROR(H5E_SYM, H5E_PATH, FAIL, "can't build destination path name")
		    } /* end if */
		    else
                        dst_path_r = H5RS_dup(names->dst_name);

		    /* Get the canonical parts of the source and destination names */

		    /* Check if the object being moved was accessed through a mounted file */
		    if(H5RS_cmp(names->loc->path->user_path_r, names->loc->path->canon_path_r) != 0) {
			size_t non_canon_name_len;   /* Length of non-canonical part of name */

			/* Get current string lengths */
			non_canon_name_len = H5RS_len(names->loc->path->user_path_r) - H5RS_len(names->loc->path->canon_path_r);

			canon_src_path_r = H5RS_create(H5RS_get_str(src_path_r) + non_canon_name_len);
			canon_dst_path_r = H5RS_create(H5RS_get_str(dst_path_r) + non_canon_name_len);
		    } /* end if */
		    else {
			canon_src_path_r = H5RS_dup(src_path_r);
			canon_dst_path_r = H5RS_dup(dst_path_r);
		    } /* end else */

		    /* Check if the link being changed in the file is along the canonical path for this object */
		    if(H5G_common_path(obj_path->canon_path_r, canon_src_path_r)) {
			size_t user_dst_len;    /* Length of destination user path */
			size_t canon_dst_len;   /* Length of destination canonical path */
			const char *old_user_path;    /* Pointer to previous user path */
			char *new_user_path;    /* Pointer to new user path */
			char *new_canon_path;   /* Pointer to new canonical path */
			const char *tail_path;  /* Pointer to "tail" of path */
			size_t tail_len;    /* Pointer to "tail" of path */
			char *src_canon_prefix; /* Pointer to source canonical path prefix of component which is moving */
			size_t src_canon_prefix_len;/* Length of the source canonical path prefix */
			char *dst_canon_prefix; /* Pointer to destination canonical path prefix of component which is moving */
			size_t dst_canon_prefix_len;/* Length of the destination canonical path prefix */
			char *user_prefix;      /* Pointer to user path prefix of component which is moving */
			size_t user_prefix_len; /* Length of the user path prefix */
			char *src_comp;         /* The source name of the component which is actually changing */
			char *dst_comp;         /* The destination name of the component which is actually changing */
			const char *canon_src_path;   /* pointer to canonical part of source path */
			const char *canon_dst_path;   /* pointer to canonical part of destination path */

			/* Get the pointers to the raw strings */
			canon_src_path = H5RS_get_str(canon_src_path_r);
			canon_dst_path = H5RS_get_str(canon_dst_path_r);

			/* Get the source & destination components */
			src_comp = HDstrrchr(canon_src_path, '/');
			HDassert(src_comp);
			dst_comp = HDstrrchr(canon_dst_path, '/');
			HDassert(dst_comp);

			/* Find the canonical prefixes for the entry */
			src_canon_prefix_len = HDstrlen(canon_src_path) - HDstrlen(src_comp);
			if(NULL == (src_canon_prefix = H5MM_malloc(src_canon_prefix_len + 1)))
			    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
			HDstrncpy(src_canon_prefix, canon_src_path, src_canon_prefix_len);
			src_canon_prefix[src_canon_prefix_len] = '\0';

			dst_canon_prefix_len = HDstrlen(canon_dst_path) - HDstrlen(dst_comp);
			if(NULL == (dst_canon_prefix = H5MM_malloc(dst_canon_prefix_len + 1)))
			    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
			HDstrncpy(dst_canon_prefix, canon_dst_path, dst_canon_prefix_len);
			dst_canon_prefix[dst_canon_prefix_len] = '\0';

			/* Hold this for later use */
			old_user_path = H5RS_get_str(obj_path->user_path_r);

			/* Find the user prefix for the entry */
			user_prefix_len = HDstrlen(old_user_path) - H5RS_len(obj_path->canon_path_r);
			if(NULL == (user_prefix = H5MM_malloc(user_prefix_len + 1)))
			    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
			HDstrncpy(user_prefix, old_user_path, user_prefix_len);
			user_prefix[user_prefix_len] = '\0';

			/* Set the tail path info */
			tail_path = old_user_path+user_prefix_len + src_canon_prefix_len + HDstrlen(src_comp);
			tail_len = HDstrlen(tail_path);

			/* Get the length of the destination paths */
			user_dst_len = user_prefix_len + dst_canon_prefix_len + HDstrlen(dst_comp) + tail_len;
			canon_dst_len = dst_canon_prefix_len + HDstrlen(dst_comp) + tail_len;

			/* Allocate space for the new user path */
			if(NULL == (new_user_path = H5FL_BLK_MALLOC(str_buf, user_dst_len + 1)))
			    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

			/* Allocate space for the new canonical path */
			if(NULL == (new_canon_path = H5FL_BLK_MALLOC(str_buf, canon_dst_len + 1)))
			    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

			/* Create the new names */
			HDstrcpy(new_user_path, user_prefix);
			HDstrcat(new_user_path, dst_canon_prefix);
			HDstrcat(new_user_path, dst_comp);
			HDstrcat(new_user_path, tail_path);
			HDstrcpy(new_canon_path, dst_canon_prefix);
			HDstrcat(new_canon_path, dst_comp);
			HDstrcat(new_canon_path, tail_path);

			/* Release the old user & canonical paths */
			H5RS_decr(obj_path->user_path_r);
			H5RS_decr(obj_path->canon_path_r);

			/* Take ownership of the new user & canonical paths */
			obj_path->user_path_r = H5RS_own(new_user_path);
			obj_path->canon_path_r = H5RS_own(new_canon_path);

			/* Free the extra paths allocated */
			H5MM_xfree(src_canon_prefix);
			H5MM_xfree(dst_canon_prefix);
			H5MM_xfree(user_prefix);
		    } /* end if */


		    /* Free the extra paths allocated */
		    H5RS_decr(src_path_r);
		    H5RS_decr(dst_path_r);
		    H5RS_decr(canon_src_path_r);
		    H5RS_decr(canon_dst_path_r);
		} /* end if */
		else {
		    /* Release the old user path */
		    if(obj_path->user_path_r) {
			H5RS_decr(obj_path->user_path_r);
			obj_path->user_path_r = NULL;
		    } /* end if */
		} /* end else */
            } /* end if */
            break;

        default:
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid call")
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5G_name_replace_cb() */


/*-------------------------------------------------------------------------
 * Function: H5G_name_replace
 *
 * Purpose: Search the list of open IDs and replace names according to a
 *              particular operation.  The operation occured on the LOC
 *              entry, which had SRC_NAME previously.  The new name (if there
 *              is one) is DST_NAME.  Additional entry location information
 *              (currently only needed for the 'move' operation) is passed
 *              in SRC_LOC and DST_LOC.
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
    H5RS_str_t *src_name, H5G_loc_t *src_loc,
    H5RS_str_t *dst_name, H5G_loc_t *dst_loc, H5G_names_op_t op)
{
    H5G_names_t names;          /* Structure to hold operation information for callback */
    unsigned search_group = 0;  /* Flag to indicate that groups are to be searched */
    unsigned search_dataset = 0;  /* Flag to indicate that datasets are to be searched */
    unsigned search_datatype = 0; /* Flag to indicate that datatypes are to be searched */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5G_name_replace, FAIL)

    /* Set up common information for callback */
    names.src_name = src_name;
    names.dst_name = dst_name;
    names.loc = loc;
    names.src_loc = src_loc;
    names.dst_loc = dst_loc;
    names.op = op;

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

        default:
            HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "not valid object type");
    } /* end switch */

    /* Search through group IDs */
    if(search_group)
        H5I_search(H5I_GROUP, H5G_name_replace_cb, &names);

    /* Search through dataset IDs */
    if(search_dataset)
        H5I_search(H5I_DATASET, H5G_name_replace_cb, &names);

    /* Search through datatype IDs */
    if(search_datatype)
        H5I_search(H5I_DATATYPE, H5G_name_replace_cb, &names);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_name_replace() */

