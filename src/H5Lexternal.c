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

#define H5L_PACKAGE		/*suppress error about including H5Lpkg   */
#define H5G_PACKAGE		/*suppress error about including H5Gpkg   */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5L_init_extern_interface

#include "H5private.h"          /* Generic Functions                    */
#include "H5Lpkg.h"             /* Links                                */
#include "H5Eprivate.h"         /* Error handling                       */
#include "H5MMprivate.h"        /* Memory management                    */
#include "H5Opublic.h"         /* File objects                         */
#include "H5Ppublic.h"         /* Property lists                       */
#include "H5Gpkg.h"             /* Groups                               */

static hid_t H5L_extern_traverse(const char UNUSED *link_name, hid_t cur_group,
    void * udata, size_t UNUSED udata_size, hid_t lapl_id);
static ssize_t H5L_extern_query(const char UNUSED * link_name, void * udata,
    size_t udata_size, void * buf /*out*/, size_t buf_size);

/* Default External Link link class */
const H5L_class_t H5L_EXTERN_LINK_CLASS[1] = {{
    H5L_LINK_CLASS_T_VERS,      /* H5L_class_t version            */
    H5L_TYPE_EXTERNAL,		/* Link type id number            */
    "external_link",            /* Link name for debugging        */
    NULL,                       /* Creation callback              */
    NULL,                       /* Move callback                  */
    NULL,                       /* Copy callback                  */
    H5L_extern_traverse,        /* The actual traversal function  */
    NULL,                       /* Deletion callback              */
    H5L_extern_query            /* Query callback                 */
}};


/*--------------------------------------------------------------------------
NAME
   H5L_init_extern_interface -- Initialize interface-specific information
USAGE
    herr_t H5L_init_extern_interface()

RETURNS
    Non-negative on success/Negative on failure

DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5L_init currently).

--------------------------------------------------------------------------*/
static herr_t
H5L_init_extern_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5L_init_extern_interface)

    FUNC_LEAVE_NOAPI(H5L_init())
} /* H5L_init_extern_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5L_extern_traverse
 *
 * Purpose:	Default traversal function for external links. This can
 *              be overridden using H5Lregister().
 *
 *              Given a filename and path packed into the link udata,
 *              attempts to open an object within an external file.
 *              If the H5L_ELINK_PREFIX_PROP property is set in the
 *              link access property list, appends that prefix to the
 *              filename being opened.
 *
 * Return:	ID of the opened object on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Monday, July 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static hid_t
H5L_extern_traverse(const char UNUSED *link_name, hid_t cur_group,
    void * udata, size_t UNUSED udata_size, hid_t lapl_id)
{
    hid_t         fid;
    char         *file_name;
    char         *obj_name;
    char         *prefix;
    size_t        fname_len;
    hbool_t       fname_alloc = FALSE;
    unsigned      intent;
    hid_t         fapl_id;
    hid_t         ret_value = -1;

    file_name = (char *) udata;
    fname_len = HDstrlen(file_name);
    obj_name = ((char *) udata) + fname_len + 1;

    /* See if the external link prefix property is set */
    if(H5Pget_elink_prefix(lapl_id, &prefix) < 0)
        goto error;

    /* If so, prepend it to the filename */
    if(prefix != NULL)
    {
        size_t buf_size;

        buf_size = HDstrlen(prefix);

        /* Allocate a buffer to hold the filename plus prefix */
        file_name = H5MM_malloc(buf_size + fname_len + 1);
        fname_alloc = TRUE;

        /* Add the external link's filename to the prefix supplied */
        HDstrcpy(file_name, prefix);
        HDstrcat(file_name, udata);
    }

    /* Whatever access properties and intent the user used on the old file,
     * use the same ones to open the new file.  If this is a bad default,
     * users can override this callback using H5Lregister.
     */
    if((fid = H5Iget_file_id(cur_group)) < 0)
        goto error;
    if(H5Fget_intent(fid, &intent) < 0)
        goto error;
    if((fapl_id = H5Fget_access_plist(fid)) < 0)
        goto error;
    if(H5Fclose(fid) < 0)
        goto error;

    if((fid = H5Fopen(file_name, intent, fapl_id)) < 0)
        goto error;

    ret_value = H5Oopen(fid, obj_name, lapl_id); /* If this fails, our return value will be negative. */
    if(H5Pclose(fapl_id) < 0)
        goto error;
    if(H5Fclose(fid) < 0)
        goto error;

    /* Free file_name if it's been allocated */
    if(fname_alloc)
        H5MM_xfree(file_name);

    return ret_value;

error:
    /* Free file_name if it's been allocated */
    if(fname_alloc)
        H5MM_xfree(file_name);

    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	H5L_extern_query
 *
 * Purpose:	Default query function for external links. This can
 *              be overridden using H5Lregister().
 *
 *              Returns the size of the link's user data. If a buffer of
 *              is provided, copies at most buf_size bytes of the udata
 *              into it.
 *
 * Return:	Size of buffer on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Monday, July 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static ssize_t
H5L_extern_query(const char UNUSED * link_name, void * udata,
    size_t udata_size, void * buf /*out*/, size_t buf_size)
{
    size_t        ret_value;

    /* If the buffer is NULL, skip writng anything in it and just return
     * the size needed */
    if(buf) {
        if(udata_size < buf_size)
            buf_size = udata_size;

        /* Copy the udata verbatim up to udata_size*/
        HDmemcpy(buf, udata, udata_size);
    }

    ret_value = udata_size;

    return ret_value;
}


/*-------------------------------------------------------------------------
 * Function:	H5Lcreate_external
 *
 * Purpose:	Creates an external link from LINK_NAME to OBJ_NAME.
 *
 *              External links are links to objects in other HDF5 files.  They
 *              are allowed to "dangle" like soft links internal to a file.
 *              FILE_NAME is the name of the file that OBJ_NAME is is contained
 *              within.  If OBJ_NAME is given as a relative path name, the
 *              path will be relative to the root group of FILE_NAME.
 *		LINK_NAME is interpreted relative to LINK_LOC_ID, which is
 *              either a file ID or a group ID.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, May 18, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Lcreate_external(const char *file_name, const char *obj_name,
        hid_t link_loc_id, const char *link_name, hid_t lcpl_id, hid_t lapl_id)
{
    H5G_loc_t	link_loc;
    char       *temp_name = NULL;
    size_t      buf_size;
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Lcreate_external, FAIL)
    H5TRACE6("e","ssisii",file_name,obj_name,link_loc_id,link_name,lcpl_id,
             lapl_id);

    /* Check arguments */
    if(!file_name || !*file_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no file name specified")
    if(!obj_name || !*obj_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no object name specified")
    if(H5G_loc(link_loc_id, &link_loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!link_name || !*link_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no link name specified")

    /* Combine the filename and link name into a single buffer to give to the UD link */
    buf_size = HDstrlen(file_name) + HDstrlen(obj_name) + 2;
    if(NULL == (temp_name = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to allocate udata buffer")
    HDstrcpy(temp_name, file_name);
    HDstrcpy(temp_name + (HDstrlen(file_name) + 1), obj_name);

    /* Create an external link */
    if(H5L_create_ud(&link_loc, link_name, temp_name, buf_size, H5L_TYPE_EXTERNAL, lcpl_id, lapl_id, H5AC_dxpl_id) < 0)
        HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "unable to create link")

done:
    if(temp_name != NULL)
        H5MM_free(temp_name);
    FUNC_LEAVE_API(ret_value);
} /* end H5Lcreate_external() */


/*-------------------------------------------------------------------------
 * Function: H5L_register_external
 *
 * Purpose: Registers default "External Link" link class.
 *              Use during library initialization or to restore the default
 *              after users change it.
 *
 * Return: Non-negative on success/ negative on failure
 *
 * Programmer:  James Laird
 *              Monday, July 17, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5L_register_external(void)
{
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5L_register_external, FAIL)

    if(H5L_register(H5L_EXTERN_LINK_CLASS) < 0)
        HGOTO_ERROR(H5E_LINK, H5E_NOTREGISTERED, FAIL, "unable to register external link class")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function: H5Lunpack_elink_val
 *
 * Purpose: Given a buffer holding the "link value" from an external link,
 *              gets pointers to the filename and object path within the
 *              link value buffer.
 *
 *              External link linkvalues are two NULL-terminated strings
 *              one after the other.
 *
 *              FILENAME and OBJ_PATH will be set to pointers within
 *              ext_linkval unless they are NULL.
 *
 *              Using this function on strings that aren't external link
 *              udata buffers can result in segmentation faults.
 *
 * Return: Non-negative on success/ Negative on failure
 *
 * Programmer:  James Laird
 *              Monday, July 17, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Lunpack_elink_val(char *ext_linkval, size_t link_size, char **filename,
                    char **obj_path)
{
    size_t      len;                /* Length of the filename in the linkval*/
    herr_t      ret_value=SUCCEED;  /* Return value */

    FUNC_ENTER_API(H5Lunpack_elink_val, FAIL)
    H5TRACE3("e","s*s*s",ext_linkval,filename,obj_path);

    if(ext_linkval == NULL )
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not an external link linkval buffer")
    if(link_size <= 1)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a valid external link buffer")

    /* Try to do some error checking.  If the last character in the linkval
     * (the last character of obj_path) isn't NULL, then something's wrong.
     */
    if(ext_linkval[link_size-1] != '\0')
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "linkval buffer is not NULL-terminated")

    /* We're now guaranteed that HDstrlen won't segfault, since the buffer has
     * at least one NULL in it.
     */
    len = HDstrlen(ext_linkval);

    /* If the first NULL we found was at the very end of the buffer, then
     * this external link value has no object name and is invalid.
     */
    if(len + 1>= link_size)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "linkval buffer doesn't contain an object path")

    /* If we got here then the buffer contains (at least) two strings packed
     * in the correct way.  Assume it's correct and return pointers to the
     * filename and object path.
     */
    if(filename != NULL)
        *filename = ext_linkval;

    if(obj_path != NULL)
        *obj_path = ext_linkval + len + 1;  /* Add one for NULL terminator */

done:
    FUNC_LEAVE_API(ret_value)
}

