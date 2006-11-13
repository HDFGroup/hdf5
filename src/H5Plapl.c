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
 * Created:		H5Plapl.c
 *			July 14 2006
 *			James Laird <jlaird@ncsa.uiuc.edu>
 *
 * Purpose:		Link access property list class routines
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/
#define H5P_PACKAGE		/*suppress error about including H5Ppkg	  */

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Lprivate.h"		/* Links		  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Ppkg.h"		/* Property lists		  	*/


/****************/
/* Local Macros */
/****************/

/* ========  Link access properties ======== */
/* Definitions for number of soft links to traverse */
#define H5L_ACS_NLINKS_SIZE        sizeof(size_t)
#define H5L_ACS_NLINKS_DEF         H5L_NUM_LINKS /*max symlinks to follow per lookup  */
/* Definitions for external link prefix */
#define H5L_ACS_ELINK_PREFIX_SIZE        sizeof(char *)
#define H5L_ACS_ELINK_PREFIX_DEF         NULL /*default is no prefix */
#define H5L_ACS_ELINK_PREFIX_DEL         H5P_lacc_elink_pref_del
#define H5L_ACS_ELINK_PREFIX_COPY        H5P_lacc_elink_pref_copy
#define H5L_ACS_ELINK_PREFIX_CLOSE       H5P_lacc_elink_pref_close

/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Property class callbacks */
static herr_t H5P_lacc_reg_prop(H5P_genclass_t *pclass);

/* Property list callbacks */
static herr_t H5P_lacc_elink_pref_del(hid_t prop_id, const char* name, size_t size, void* value);
static herr_t H5P_lacc_elink_pref_copy(const char* name, size_t size, void* value);
static herr_t H5P_lacc_elink_pref_close(const char* name, size_t size, void* value);


/*********************/
/* Package Variables */
/*********************/

/* Dataset creation property list class library initialization object */
const H5P_libclass_t H5P_CLS_LACC[1] = {{
    "link access",		/* Class name for debugging     */
    &H5P_CLS_ROOT_g,		/* Parent class ID              */
    &H5P_CLS_LINK_ACCESS_g,	/* Pointer to class ID          */
    &H5P_LST_LINK_ACCESS_g,	/* Pointer to default property list ID */
    H5P_lacc_reg_prop,		/* Default property registration routine */
    NULL,		        /* Class creation callback      */
    NULL,		        /* Class creation callback info */
    NULL,			/* Class copy callback          */
    NULL,		        /* Class copy callback info     */
    NULL,			/* Class close callback         */
    NULL 		        /* Class close callback info    */
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:    H5P_lacc_reg_prop
 *
 * Purpose:     Register the dataset creation property list class's properties
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              October 31, 2006
 *-------------------------------------------------------------------------
 */
static herr_t
H5P_lacc_reg_prop(H5P_genclass_t *pclass)
{
    size_t nlinks = H5L_ACS_NLINKS_DEF; /* Default number of soft links to traverse */
    char *elink_prefix = H5L_ACS_ELINK_PREFIX_DEF; /* Default external link prefix string */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5P_lacc_reg_prop)

    /* Register property for number of links traversed */
    if(H5P_register(pclass, H5L_ACS_NLINKS_NAME, H5L_ACS_NLINKS_SIZE,
             &nlinks, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register property for external link prefix */
    if(H5P_register(pclass, H5L_ACS_ELINK_PREFIX_NAME, H5L_ACS_ELINK_PREFIX_SIZE,
             &elink_prefix, NULL, NULL, NULL, H5L_ACS_ELINK_PREFIX_DEL, H5L_ACS_ELINK_PREFIX_COPY, NULL, H5L_ACS_ELINK_PREFIX_CLOSE) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_lacc_reg_prop() */


/*-------------------------------------------------------------------------
 * Function:    H5P_lacc_elink_pref_del
 *
 * Purpose:     Frees memory used to store the external link prefix string
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              November 2, 2006
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
static herr_t
H5P_lacc_elink_pref_del(hid_t UNUSED prop_id, const char UNUSED *name, size_t UNUSED size, void *value)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5P_lacc_elink_pref_del)

    HDassert(value);

    H5MM_xfree(*(void **)value);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5P_lacc_elink_pref_del() */


/*-------------------------------------------------------------------------
 * Function:    H5P_lacc_elink_pref_copy
 *
 * Purpose:     Creates a copy of the external link prefix string
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              November 2, 2006
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
static herr_t
H5P_lacc_elink_pref_copy(const char UNUSED *name, size_t UNUSED size, void *value)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5P_lacc_elink_pref_copy)

    HDassert(value);

    *(char **)value = H5MM_xstrdup(*(const char **)value);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5P_lacc_elink_pref_copy() */


/*-------------------------------------------------------------------------
 * Function:    H5P_lacc_elink_pref_close
 *
 * Purpose:     Frees memory used to store the external link prefix string
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              November 2, 2006
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
static herr_t
H5P_lacc_elink_pref_close(const char UNUSED *name, size_t UNUSED size, void *value)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5P_lacc_elink_pref_close)

    HDassert(value);

    H5MM_xfree(*(void **)value);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5P_lacc_elink_pref_close() */


/*-------------------------------------------------------------------------
 * Function:    H5Pset_nlinks
 *
 * Purpose:     Set the number of soft or UD link traversals allowed before
 *              the library assumes it has found a cycle and aborts the
 *              traversal.
 *
 *              The limit on soft or UD link traversals is designed to
 *              terminate link traversal if one or more links form a cycle.
 *              However, users may have a file with a legitimate path
 *              formed of a large number of soft or user-defined links.
 *              This property can be used to allow traversal of as many
 *              links as desired.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Friday, July 14, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_nlinks(hid_t plist_id, size_t nlinks)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(H5Pset_nlinks, FAIL)
    H5TRACE2("e","iz",plist_id,nlinks);

    if(nlinks <= 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "number of links must be positive");

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_LINK_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set number of links */
    if(H5P_set(plist, H5L_ACS_NLINKS_NAME, &nlinks) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set nlink info")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_nlinks() */


/*-------------------------------------------------------------------------
 * Function:    H5Pget_nlinks
 *
 * Purpose:	Gets the number of soft or user-defined links that can be
 *              traversed before a failure occurs.
 *
 *              Retrieves the current setting for the nlinks property on
 *              the given property list.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Friday, July 14, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_nlinks(hid_t plist_id, size_t *nlinks)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(H5Pget_nlinks, FAIL)
    H5TRACE2("e","i*z",plist_id,nlinks);

    if(!nlinks)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid pointer passed in");

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_LINK_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get the current number of links */
    if(H5P_get(plist, H5L_ACS_NLINKS_NAME, nlinks) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get number of links")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:    H5Pset_elink_prefix
 *
 * Purpose:     Set a prefix to be applied to the path of any external links
 *              traversed.  The prefix is appended to the filename stored
 *              in the external link.
 * 
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Thursday, August 3, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_elink_prefix(hid_t plist_id, const char *prefix)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    char *my_prefix;                    /* Copy of prefix string */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(H5Pset_elink_prefix, FAIL)
    H5TRACE2("e","is",plist_id,prefix);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_LINK_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get current prefix value */
    if(H5P_get(plist, H5L_ACS_ELINK_PREFIX_NAME, &my_prefix) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get prefix info")

    /* Free existing prefix, if there is one */
    H5MM_xfree(my_prefix);

    /* Make a copy of the user's prefix string */
    if(NULL == (my_prefix = H5MM_xstrdup(prefix)))
        HGOTO_ERROR(H5E_PLIST, H5E_CANTCOPY, FAIL, "can't copy prefix")

    /* Set prefix */
    if(H5P_set(plist, H5L_ACS_ELINK_PREFIX_NAME, &my_prefix) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set prefix info")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_elink_prefix() */


/*-------------------------------------------------------------------------
 * Function:    H5Pget_elink_prefix
 *
 * Purpose:	Gets the prefix to be applied to any external link
 *              traversals made using this property list.
 *
 *              If the pointer is not NULL, it points to a user-allocated
 *              buffer.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Thursday, August 3, 2006
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Pget_elink_prefix(hid_t plist_id, char *prefix, size_t size)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    char *my_prefix;                    /* Library's copy of the prefix */
    size_t	len;                    /* Length of prefix string */
    ssize_t 	ret_value;              /* Return value */

    FUNC_ENTER_API(H5Pget_elink_prefix, FAIL)
    H5TRACE3("Zs","isz",plist_id,prefix,size);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_LINK_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get the current prefix */
    if(H5P_get(plist, H5L_ACS_ELINK_PREFIX_NAME, &my_prefix) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get external link prefix")

    /* Check for prefix being set */
    if(my_prefix) {
        /* Copy to user's buffer, if given */
        len = HDstrlen(my_prefix);
        if(prefix) {
            HDstrncpy(prefix, my_prefix, MIN(len + 1, size));
            if(len >= size)
                prefix[size - 1] = '\0';
        } /* end if */
    } /* end if */
    else
        len = 0;

    /* Set return value */
    ret_value = (ssize_t)len;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_elink_prefix() */

