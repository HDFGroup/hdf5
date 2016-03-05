/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose:     Reference routines.
 */

/****************/
/* Module Setup */
/****************/

#include "H5Rmodule.h"      /* This source code file is part of the H5R module */

/***********/
/* Headers */
/***********/
#include "H5private.h"      /* Generic Functions    */
#include "H5Rpkg.h"         /* References           */
#include "H5Eprivate.h"     /* Error handling       */
#include "H5Iprivate.h"     /* IDs                  */
#include "H5MMprivate.h"    /* Memory management    */
#include "H5Aprivate.h"     /* Attributes           */
#include "H5Dprivate.h"     /* Datasets             */
#include "H5Gprivate.h"     /* Groups               */
#include "H5Sprivate.h"     /* Dataspaces           */


/****************/
/* Local Macros */
/****************/
#define H5R_MAX_ATTR_REF_NAME_LEN (64 * 1024)

/******************/
/* Local Typedefs */
/******************/


/********************/
/* Local Prototypes */
/********************/

static htri_t H5R__equal(href_t _ref1, href_t _ref2);
static ssize_t H5R__get_file_name(href_t ref, char *name, size_t size);


/*********************/
/* Package Variables */
/*********************/

/* Package initialization variable */
hbool_t H5_PKG_INIT_VAR = FALSE;


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Reference ID class */
static const H5I_class_t H5I_REFERENCE_CLS[1] = {{
    H5I_REFERENCE,              /* ID class value */
    0,                          /* Class flags */
    0,                          /* # of reserved IDs for class */
    (H5I_free_t) H5R_destroy    /* Callback routine for closing objects of this class */
}};

/* Flag indicating "top" of interface has been initialized */
static hbool_t H5R_top_package_initialize_s = FALSE;

/*--------------------------------------------------------------------------
NAME
   H5R__init_package -- Initialize interface-specific information
USAGE
    herr_t H5R__init_package()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
herr_t
H5R__init_package(void)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Initialize the atom group for the file IDs */
    if(H5I_register_type(H5I_REFERENCE_CLS) < 0)
	HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable to initialize interface")

    /* Mark "top" of interface as initialized, too */
    H5R_top_package_initialize_s = TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5R__init_package() */

/*--------------------------------------------------------------------------
 NAME
    H5R_top_term_package
 PURPOSE
    Terminate various H5R objects
 USAGE
    void H5R_top_term_package()
 RETURNS
    void
 DESCRIPTION
    Release IDs for the atom group, deferring full interface shutdown
    until later (in H5R_term_package).
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
int
H5R_top_term_package(void)
{
    int	n = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(H5R_top_package_initialize_s) {
	if(H5I_nmembers(H5I_REFERENCE) > 0) {
	    (void)H5I_clear_type(H5I_REFERENCE, FALSE, FALSE);
            n++; /*H5I*/
	} /* end if */

        /* Mark closed */
        if(0 == n)
            H5R_top_package_initialize_s = FALSE;
    } /* end if */

    FUNC_LEAVE_NOAPI(n)
} /* end H5R_top_term_package() */

/*--------------------------------------------------------------------------
 NAME
    H5R_term_package
 PURPOSE
    Terminate various H5R objects
 USAGE
    void H5R_term_package()
 RETURNS
    void
 DESCRIPTION
    Release the atom group and any other resources allocated.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...

     Finishes shutting down the interface, after H5R_top_term_package()
     is called
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
int
H5R_term_package(void)
{
    int	n = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(H5_PKG_INIT_VAR) {
        /* Sanity checks */
        HDassert(0 == H5I_nmembers(H5I_REFERENCE));
        HDassert(FALSE == H5R_top_package_initialize_s);

        /* Destroy the reference id group */
        n += (H5I_dec_type_ref(H5I_REFERENCE) > 0);

        /* Mark closed */
        if(0 == n)
            H5_PKG_INIT_VAR = FALSE;
    } /* end if */

    FUNC_LEAVE_NOAPI(n)
} /* end H5R_term_package() */

/*-------------------------------------------------------------------------
 * Function:    H5R_create_object
 *
 * Purpose: Creates an object reference. The LOC and NAME are used to locate
 * the object pointed to.
 *
 * Return:  Success:    Reference created
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
href_t
H5R_create_object(H5G_loc_t *loc, const char *name, hid_t dxpl_id)
{
    H5G_loc_t obj_loc;          /* Group hier. location of object */
    H5G_name_t path;            /* Object group hier. path */
    H5O_loc_t oloc;             /* Object object location */
    hbool_t obj_found = FALSE;  /* Object location found */
    struct href_t *ret_value = NULL; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(loc);
    HDassert(name);

    /* Set up object location to fill in */
    obj_loc.oloc = &oloc;
    obj_loc.path = &path;
    H5G_loc_reset(&obj_loc);

    /* Find the object */
    if(H5G_loc_find(loc, name, &obj_loc, H5P_DEFAULT, dxpl_id) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_NOTFOUND, NULL, "object not found")
    obj_found = TRUE;

    if(NULL == (ret_value = (struct href_t *)H5MM_malloc(sizeof(struct href_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "Cannot allocate memory for reference")

    ret_value->ref_type = H5R_OBJECT;
    ret_value->ref.addr = obj_loc.oloc->addr;

done:
    if(obj_found)
        H5G_loc_free(&obj_loc);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5R_create_object() */

/*-------------------------------------------------------------------------
 * Function:    H5R_create_region
 *
 * Purpose: Creates a region reference. The LOC and NAME are used to locate
 * the object pointed to and the SPACE is used to choose the region pointed to.
 *
 * Return:  Success:    Reference created
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
href_t
H5R_create_region(H5G_loc_t *loc, const char *name, hid_t dxpl_id, H5S_t *space)
{
    H5G_loc_t obj_loc;          /* Group hier. location of object */
    H5G_name_t path;            /* Object group hier. path */
    H5O_loc_t oloc;             /* Object object location */
    hbool_t obj_found = FALSE;  /* Object location found */
    hssize_t buf_size;          /* Size of buffer needed to serialize selection */
    uint8_t *p;                 /* Pointer to OID to store */
    struct href_t *ref = NULL;         /* Reference to be returned */
    struct href_t *ret_value = NULL;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(loc);
    HDassert(name);
    HDassert(space);

    /* Set up object location to fill in */
    obj_loc.oloc = &oloc;
    obj_loc.path = &path;
    H5G_loc_reset(&obj_loc);

    /* Find the object */
    if(H5G_loc_find(loc, name, &obj_loc, H5P_DEFAULT, dxpl_id) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_NOTFOUND, NULL, "object not found")
    obj_found = TRUE;

    /* Get the amount of space required to serialize the selection */
    if((buf_size = H5S_SELECT_SERIAL_SIZE(space)) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, NULL, "Invalid amount of space for serializing selection")

    /* Increase buffer size to allow for the dataset OID */
    buf_size += (hssize_t)sizeof(haddr_t);

    /* Allocate the space to store the serialized information */
    H5_CHECK_OVERFLOW(buf_size, hssize_t, size_t);
    if(NULL == (ref = (struct href_t *)H5MM_malloc(sizeof(struct href_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "Cannot allocate memory for reference")
    if(NULL == (ref->ref.serial.buf = H5MM_malloc((size_t) buf_size)))
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTALLOC, NULL, "Cannot allocate buffer to serialize selection")
    ref->ref.serial.buf_size = (size_t) buf_size;
    ref->ref_type = H5R_REGION;

    /* Serialize information for dataset OID into buffer */
    p = (uint8_t *)ref->ref.serial.buf;
    H5F_addr_encode(loc->oloc->file, &p, obj_loc.oloc->addr);

    /* Serialize the selection into buffer */
    if(H5S_SELECT_SERIALIZE(space, &p) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTCOPY, NULL, "Unable to serialize selection")

    ret_value = ref;

done:
    if(obj_found)
        H5G_loc_free(&obj_loc);
    if(NULL == ret_value) {
        H5MM_free(ref->ref.serial.buf);
        H5MM_free(ref);
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5R_create_region */

/*-------------------------------------------------------------------------
 * Function:    H5R_create_attr
 *
 * Purpose: Creates an attribute reference. The LOC, NAME and ATTR_NAME are
 * used to locate the attribute pointed to.
 *
 * Return:  Success:    Reference created
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
href_t
H5R_create_attr(H5G_loc_t *loc, const char *name, hid_t dxpl_id, const char *attr_name)
{
    H5G_loc_t obj_loc;          /* Group hier. location of object */
    H5G_name_t path;            /* Object group hier. path */
    H5O_loc_t oloc;             /* Object object location */
    hbool_t obj_found = FALSE;  /* Object location found */
    size_t buf_size;            /* Size of buffer needed to serialize attribute */
    size_t attr_name_len;       /* Length of the attribute name */
    uint8_t *p;                 /* Pointer to OID to store */
    struct href_t *ref = NULL;         /* Reference to be returned */
    struct href_t *ret_value = NULL;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(loc);
    HDassert(name);
    HDassert(attr_name);

    /* Set up object location to fill in */
    obj_loc.oloc = &oloc;
    obj_loc.path = &path;
    H5G_loc_reset(&obj_loc);

    /* Find the object */
    if(H5G_loc_find(loc, name, &obj_loc, H5P_DEFAULT, dxpl_id) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_NOTFOUND, NULL, "object not found")
    obj_found = TRUE;

    /* Get the amount of space required to serialize the attribute name */
    attr_name_len = HDstrlen(attr_name);
    if(attr_name_len >= H5R_MAX_ATTR_REF_NAME_LEN)
        HGOTO_ERROR(H5E_REFERENCE, H5E_ARGS, NULL, "attribute name too long")

    /* Compute buffer size, allow for the attribute name length and object's OID */
    buf_size = attr_name_len + sizeof(uint16_t) + sizeof(haddr_t);

    /* Allocate the space to store the serialized information */
    if(NULL == (ref = (struct href_t *)H5MM_malloc(sizeof(struct href_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "Cannot allocate memory for reference")
    if (NULL == (ref->ref.serial.buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTALLOC, NULL, "Cannot allocate buffer to serialize selection")
    ref->ref.serial.buf_size = buf_size;
    ref->ref_type = H5R_ATTR;

    /* Serialize information for object's OID into buffer */
    p = (uint8_t *)ref->ref.serial.buf;
    H5F_addr_encode(loc->oloc->file, &p, obj_loc.oloc->addr);

    /* Serialize information for attribute name length into the buffer */
    UINT16ENCODE(p, attr_name_len);

    /* Copy the attribute name into the buffer */
    HDmemcpy(p, attr_name, attr_name_len);

    /* Sanity check */
    HDassert((size_t)((p + attr_name_len) - (uint8_t *)ref->ref.serial.buf) == buf_size);

    ret_value = ref;

done:
    if(obj_found)
        H5G_loc_free(&obj_loc);
    if(NULL == ret_value) {
        H5MM_free(ref->ref.serial.buf);
        H5MM_free(ref);
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5R_create_attr */

/*-------------------------------------------------------------------------
 * Function:    H5R_create_ext_object
 *
 * Purpose: Creates an external object reference. The FILENAME and PATHNAME
 * are used to locate the object pointed to.
 *
 * Return:  Success:    Reference created
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
href_t
H5R_create_ext_object(const char *filename, const char *pathname)
{
    size_t filename_len;        /* Length of the file name */
    size_t pathname_len;        /* Length of the obj path name */
    size_t buf_size;            /* Size of buffer needed to serialize reference */
    uint8_t *p;                 /* Pointer to OID to store */
    struct href_t *ref = NULL;         /* Reference to be returned */
    struct href_t *ret_value = NULL;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(filename);
    HDassert(pathname);

    /* Need to add name of the file */
    filename_len = HDstrlen(filename);

    /* Need to add object path name */
    pathname_len = HDstrlen(pathname);

    /* Compute buffer size, allow for the file name and object path name lengths */
    buf_size = filename_len + pathname_len + 2 * sizeof(uint16_t);

    if(NULL == (ref = (struct href_t *)H5MM_malloc(sizeof(struct href_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "Cannot allocate memory for reference")
    if (NULL == (ref->ref.serial.buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTALLOC, NULL, "Cannot allocate buffer to serialize selection")
    ref->ref.serial.buf_size = buf_size;
    ref->ref_type = H5R_EXT_OBJECT;

    /* Serialize information for file name length into the buffer */
    p = (uint8_t *)ref->ref.serial.buf;
    UINT16ENCODE(p, filename_len);

    /* Copy the file name into the buffer */
    HDmemcpy(p, filename, filename_len);
    p += filename_len;

    /* Serialize information for object path name length into the buffer */
    UINT16ENCODE(p, pathname_len);

    /* Copy the object path name into the buffer */
    HDmemcpy(p, pathname, pathname_len);

    /* Sanity check */
    HDassert((size_t)((p + pathname_len) - (uint8_t *)ref->ref.serial.buf) == buf_size);

    ret_value = ref;

done:
    if(NULL == ret_value) {
        H5MM_free(ref->ref.serial.buf);
        H5MM_free(ref);
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5R_create_ext_object() */

/*-------------------------------------------------------------------------
 * Function:    H5R_create_ext_region
 *
 * Purpose: Creates an external region reference. The FILENAME and PATHNAME
 * are used to locate the object pointed to and the SPACE is used to choose
 * the region pointed to.
 *
 * Return:  Success:    Reference created
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
href_t
H5R_create_ext_region(const char *filename, const char *pathname, H5S_t *space)
{
    size_t filename_len;        /* Length of the file name */
    size_t pathname_len;        /* Length of the obj path name */
    size_t buf_size;            /* Size of buffer needed to serialize selection */
    size_t space_buf_size;      /* Size of buffer needed to serialize selection */
    uint8_t *p = NULL;          /* Pointer to OID to store */
    struct href_t *ref = NULL;         /* Reference to be returned */
    struct href_t *ret_value = NULL;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(filename);
    HDassert(pathname);
    HDassert(space);

    /* Need to add name of the file */
    filename_len = HDstrlen(filename);

    /* Need to add object path name */
    pathname_len = HDstrlen(pathname);

    /* Get the amount of space required to serialize the selection */
    if (H5S_encode(space, &p, &space_buf_size) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTCOPY, NULL, "Unable to serialize selection")

    /* Compute buffer size, allow for the file name and object path name lengths */
    buf_size = space_buf_size + filename_len + pathname_len + 2 * sizeof(uint16_t);

    /* Allocate the space to store the serialized information */
    if(NULL == (ref = (struct href_t *)H5MM_malloc(sizeof(struct href_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "Cannot allocate memory for reference")
    if (NULL == (ref->ref.serial.buf = H5MM_malloc((size_t) buf_size)))
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTALLOC, NULL, "Cannot allocate buffer to serialize selection")
    ref->ref.serial.buf_size = buf_size;
    ref->ref_type = H5R_EXT_REGION;

    /* Serialize information for file name length into the buffer */
    p = (uint8_t *)ref->ref.serial.buf;
    UINT16ENCODE(p, filename_len);

    /* Copy the file name into the buffer */
    HDmemcpy(p, filename, filename_len);
    p += filename_len;

    /* Serialize information for object path name length into the buffer */
    UINT16ENCODE(p, pathname_len);

    /* Copy the object path name into the buffer */
    HDmemcpy(p, pathname, pathname_len);
    p += pathname_len;

    /* Serialize the selection into buffer */
    if (H5S_encode(space, &p, &space_buf_size) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTCOPY, NULL, "Unable to serialize selection")

    /* Sanity check */
    HDassert((size_t)(p - (uint8_t *)ref->ref.serial.buf) == (size_t)buf_size);

    ret_value = ref;

done:
    if(NULL == ret_value) {
        H5MM_free(ref->ref.serial.buf);
        H5MM_free(ref);
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5R_create_ext_region */

/*-------------------------------------------------------------------------
 * Function:    H5R_create_ext_attr
 *
 * Purpose: Creates an external attribute reference. The FILENAME, PATHNAME
 * and ATTR_NAME are used to locate the attribute pointed to.
 *
 * Return:  Success:    Reference created
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
href_t
H5R_create_ext_attr(const char *filename, const char *pathname, const char *attr_name)
{
    size_t filename_len;        /* Length of the file name */
    size_t pathname_len;        /* Length of the obj path name */
    size_t attr_name_len;       /* Length of the attribute name */
    size_t buf_size;            /* Size of buffer needed to serialize reference */
    uint8_t *p;                 /* Pointer to OID to store */
    struct href_t *ref = NULL;         /* Reference to be returned */
    struct href_t *ret_value = NULL;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(filename);
    HDassert(pathname);
    HDassert(attr_name);

    /* Need to add name of the file */
    filename_len = HDstrlen(filename);

    /* Need to add object path name */
    pathname_len = HDstrlen(pathname);

    /* Get the amount of space required to serialize the attribute name */
    attr_name_len = HDstrlen(attr_name);
    if(attr_name_len >= H5R_MAX_ATTR_REF_NAME_LEN)
        HGOTO_ERROR(H5E_REFERENCE, H5E_ARGS, NULL, "attribute name too long")

    /* Compute buffer size, allow for the file name and object path name lengths */
    buf_size = filename_len + pathname_len + attr_name_len + 3 * sizeof(uint16_t);

    /* Allocate the space to store the serialized information */
    if(NULL == (ref = (struct href_t *)H5MM_malloc(sizeof(struct href_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "Cannot allocate memory for reference")
    if (NULL == (ref->ref.serial.buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTALLOC, NULL, "Cannot allocate buffer to serialize selection")
    ref->ref.serial.buf_size = buf_size;
    ref->ref_type = H5R_EXT_ATTR;

    /* Serialize information for file name length into the buffer */
    p = (uint8_t *)ref->ref.serial.buf;
    UINT16ENCODE(p, filename_len);

    /* Copy the file name into the buffer */
    HDmemcpy(p, filename, filename_len);
    p += filename_len;

    /* Serialize information for object path name length into the buffer */
    UINT16ENCODE(p, pathname_len);

    /* Copy the object path name into the buffer */
    HDmemcpy(p, pathname, pathname_len);
    p += pathname_len;

    /* Serialize information for attribute name length into the buffer */
    UINT16ENCODE(p, attr_name_len);

    /* Copy the attribute name into the buffer */
    HDmemcpy(p, attr_name, attr_name_len);

    /* Sanity check */
    HDassert((size_t)((p + attr_name_len) - (uint8_t *)ref->ref.serial.buf) == buf_size);

    ret_value = ref;

done:
    if(NULL == ret_value) {
        H5MM_free(ref->ref.serial.buf);
        H5MM_free(ref);
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5R_create_ext_attr */

/*-------------------------------------------------------------------------
 * Function:    H5Rcreate_object
 *
 * Purpose: Creates an object reference. The LOC_ID and NAME are used to locate
 * the object pointed to.
 *
 * Return:  Success:    Reference created
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
href_t
H5Rcreate_object(hid_t loc_id, const char *name)
{
    H5G_loc_t loc; /* File location */
    href_t    ret_value = NULL; /* Return value */

    FUNC_ENTER_API(NULL)

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a location")
    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "no name given")

    if(NULL == (ret_value = H5R_create_object(&loc, name, H5AC_dxpl_id)))
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, NULL, "unable to create object reference")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Rcreate_object() */

/*-------------------------------------------------------------------------
 * Function:    H5Rcreate_region
 *
 * Purpose: Creates a region reference. The LOC_ID and NAME are used to locate
 * the object pointed to and the SPACE_ID is used to choose the region pointed
 * to.
 *
 * Return:  Success:    Reference created
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
href_t
H5Rcreate_region(hid_t loc_id, const char *name, hid_t space_id)
{
    H5G_loc_t loc; /* File location */
    H5S_t      *space = NULL;   /* Pointer to dataspace containing region */
    href_t     ret_value = NULL; /* Return value */

    FUNC_ENTER_API(NULL)

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a location")
    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "no name given")
    if(space_id == H5I_BADID)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "reference region dataspace id must be valid")
    if(space_id != H5I_BADID && (NULL == (space = (H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE))))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a dataspace")

    if(NULL == (ret_value = H5R_create_region(&loc, name, H5AC_dxpl_id, space)))
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, NULL, "unable to create region reference")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Rcreate_region() */

/*-------------------------------------------------------------------------
 * Function:    H5Rcreate_attr
 *
 * Purpose: Creates an attribute reference. The LOC_ID, NAME and ATTR_NAME are
 * used to locate the attribute pointed to.
 *
 * Return:  Success:    Reference created
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
href_t
H5Rcreate_attr(hid_t loc_id, const char *name, const char *attr_name)
{
    H5G_loc_t loc; /* File location */
    href_t    ret_value = NULL; /* Return value */

    FUNC_ENTER_API(NULL)

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a location")
    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "no name given")
    if(!attr_name || !*attr_name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "no attribute name given")

    if(NULL == (ret_value = H5R_create_attr(&loc, name, H5AC_dxpl_id, attr_name)))
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, NULL, "unable to create atribute reference")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Rcreate_attr() */

/*-------------------------------------------------------------------------
 * Function:    H5Rcreate_ext_object
 *
 * Purpose: Creates an external object reference. The FILENAME and PATHNAME
 * are used to locate the object pointed to.
 *
 * Return:  Success:    Reference created
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
href_t
H5Rcreate_ext_object(const char *filename, const char *pathname)
{
    href_t    ret_value = NULL; /* Return value */

    FUNC_ENTER_API(NULL)

    /* Check args */
    if(!filename || !*filename)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "no filename given")
    if(!pathname || !*pathname)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "no pathname given")

    if(NULL == (ret_value = H5R_create_ext_object(filename, pathname)))
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, NULL, "unable to create object reference")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Rcreate_ext_object() */

/*-------------------------------------------------------------------------
 * Function:    H5Rcreate_ext_region
 *
 * Purpose: Creates an external region reference. The FILENAME and PATHNAME
 * are used to locate the object pointed to and the SPACE_ID is used to choose
 * the region pointed to.
 *
 * Return:  Success:    Reference created
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
href_t
H5Rcreate_ext_region(const char *filename, const char *pathname, hid_t space_id)
{
    H5S_t      *space = NULL;   /* Pointer to dataspace containing region */
    href_t     ret_value = NULL; /* Return value */

    FUNC_ENTER_API(NULL)

    /* Check args */
    if(!filename || !*filename)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "no filename given")
    if(!pathname || !*pathname)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "no pathname given")
    if(space_id == H5I_BADID)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "reference region dataspace id must be valid")
    if(space_id != H5I_BADID && (NULL == (space = (H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE))))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a dataspace")

    if(NULL == (ret_value = H5R_create_ext_region(filename, pathname, space)))
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, NULL, "unable to create region reference")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Rcreate_ext_region() */

/*-------------------------------------------------------------------------
 * Function:    H5Rcreate_ext_attr
 *
 * Purpose: Creates an external attribute reference. The FILENAME, PATHNAME
 * and ATTR_NAME are used to locate the attribute pointed to.
 *
 * Return:  Success:    Reference created
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
href_t
H5Rcreate_ext_attr(const char *filename, const char *pathname, const char *attr_name)
{
    href_t    ret_value = NULL; /* Return value */

    FUNC_ENTER_API(NULL)

    /* Check args */
    if(!filename || !*filename)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "no filename given")
    if(!pathname || !*pathname)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "no pathname given")
    if(!attr_name || !*attr_name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "no attribute name given")

    if(NULL == (ret_value = H5R_create_ext_attr(filename, pathname, attr_name)))
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, NULL, "unable to create atribute reference")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Rcreate_ext_attr() */

/*-------------------------------------------------------------------------
 * Function:    H5R_destroy
 *
 * Purpose: Destroy reference and free resources allocated during H5R_create.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5R_destroy(href_t _ref)
{
    struct href_t *ref = (struct href_t *) _ref; /* Reference */
    herr_t    ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(ref);

    if(H5R_OBJECT != ref->ref_type)
        H5MM_free(ref->ref.serial.buf);
    H5MM_free(ref);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5R_destroy() */

/*-------------------------------------------------------------------------
 * Function:    H5Rdestroy
 *
 * Purpose: Destroy reference and free resources allocated during H5Rcreate.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Rdestroy(href_t ref)
{
    herr_t    ret_value = FAIL; /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(NULL == ref)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid reference pointer")

    if(FAIL == (ret_value = H5R_destroy(ref)))
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable to destroy reference")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Rdestroy() */

/*-------------------------------------------------------------------------
 * Function:    H5R_get_type
 *
 * Purpose: Given a reference to some object, return the type of that reference.
 *
 * Return:  Type of the reference
 *
 *-------------------------------------------------------------------------
 */
H5R_type_t
H5R_get_type(href_t _ref)
{
    struct href_t *ref = (struct href_t *) _ref; /* Reference */
    H5R_type_t ret_value = H5R_BADTYPE;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(ref);
    ret_value = ref->ref_type;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5R_get_type() */

/*-------------------------------------------------------------------------
 * Function:    H5Rget_type
 *
 * Purpose: Given a reference to some object, return the type of that reference.
 *
 * Return:  Reference type/H5R_BADTYPE on failure
 *
 *-------------------------------------------------------------------------
 */
H5R_type_t
H5Rget_type(href_t ref)
{
    H5R_type_t ret_value;

    FUNC_ENTER_API(H5R_BADTYPE)

    /* Check args */
    if(ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5R_BADTYPE, "invalid reference pointer")

    /* Create reference */
    ret_value = H5R_get_type(ref);
    if((ret_value <= H5R_BADTYPE) || (ret_value >= H5R_MAXTYPE))
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTGET, H5R_BADTYPE, "invalid reference type")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Rget_type() */

/*-------------------------------------------------------------------------
 * Function:    H5R__equal
 *
 * Purpose: Compare two references
 *
 * Return:  TRUE if equal, FALSE if unequal, FAIL if error
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5R__equal(href_t _ref1, href_t _ref2)
{
    struct href_t *ref1 = (struct href_t *) _ref1; /* Reference */
    struct href_t *ref2 = (struct href_t *) _ref2; /* Reference */
    htri_t ret_value = FAIL;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(ref1);
    HDassert(ref2);

    if (ref1->ref_type != ref2->ref_type)
        HGOTO_DONE(FALSE);

    switch (ref1->ref_type) {
        case H5R_OBJECT:
            if (ref1->ref.addr != ref2->ref.addr)
                HGOTO_DONE(FALSE);
            break;
        case H5R_REGION:
        case H5R_ATTR:
        case H5R_EXT_OBJECT:
        case H5R_EXT_REGION:
        case H5R_EXT_ATTR:
            if (ref1->ref.serial.buf_size != ref2->ref.serial.buf_size)
                HGOTO_DONE(FALSE);
            if (0 != HDstrcmp(ref1->ref.serial.buf, ref2->ref.serial.buf))
                HGOTO_DONE(FALSE);
            break;
        default:
            HDassert("unknown reference type" && 0);
            HGOTO_ERROR(H5E_REFERENCE, H5E_UNSUPPORTED, FAIL, "internal error (unknown reference type)")
    }

    ret_value = TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5R__equal() */

/*-------------------------------------------------------------------------
 * Function:    H5Requal
 *
 * Purpose: Compare two references
 *
 * Return:  TRUE if equal, FALSE if unequal, FAIL if error
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5Requal(href_t ref1, href_t ref2)
{
    htri_t ret_value;

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(!ref1 || !ref2)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")

    /* Create reference */
    if (FAIL == (ret_value = H5R__equal(ref1, ref2)))
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTCOMPARE, FAIL, "cannot compare references")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Requal() */

/*-------------------------------------------------------------------------
 * Function:    H5R__get_object
 *
 * Purpose: Given a reference to some object, open that object and return an
 * ID for that object.
 *
 * Return:  Valid ID on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5R__get_object(H5F_t *file, hid_t oapl_id, hid_t dxpl_id, href_t _ref, hbool_t app_ref)
{
    H5O_loc_t oloc;                 /* Object location */
    H5G_name_t path;                /* Path of object */
    H5G_loc_t loc;                  /* Group location */
    char *pathname = NULL;          /* Path name of the object (for external references) */
    unsigned rc;                    /* Reference count of object */
    H5O_type_t obj_type;            /* Type of object */
    hbool_t ext_ref = FALSE;        /* External reference */
    const uint8_t *p = NULL;        /* Pointer to OID to store */
    struct href_t *ref = (struct href_t *) _ref; /* Reference */
    hid_t ret_value = H5I_BADID;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file);
    HDassert(ref);
    HDassert(ref->ref_type > H5R_BADTYPE && ref->ref_type < H5R_MAXTYPE);

    /* Initialize the object location */
    H5O_loc_reset(&oloc);
    oloc.file = file;

    /* Construct a group location for opening the object */
    H5G_name_reset(&path);
    loc.oloc = &oloc;
    loc.path = &path;

    /* Point to reference buffer now */
    p = (const uint8_t *)ref->ref.serial.buf;

    /* Decode reference */
    switch (ref->ref_type) {
        case H5R_OBJECT:
            oloc.addr = ref->ref.addr;
            if(!H5F_addr_defined(oloc.addr) || oloc.addr == 0)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Undefined reference pointer")
            break;
        case H5R_REGION:
        case H5R_ATTR:
            /* Get the object oid for the dataset */
            H5F_addr_decode(oloc.file, &p, &(oloc.addr));
            break;
        case H5R_EXT_OBJECT:
        case H5R_EXT_REGION:
        case H5R_EXT_ATTR:
        {
            size_t filename_len; /* Length of the file name */
            size_t pathname_len; /* Length of the obj path name */

            /* Get the file name length */
            UINT16DECODE(p, filename_len);
            HDassert(filename_len < H5R_MAX_ATTR_REF_NAME_LEN);

            /* Skip the file name */
            p += filename_len;

            /* Get the path name length */
            UINT16DECODE(p, pathname_len);

            /* Allocate space for the path name */
            if(NULL == (pathname = (char *)H5MM_malloc(pathname_len + 1)))
                HGOTO_ERROR(H5E_REFERENCE, H5E_CANTALLOC, FAIL, "memory allocation failed")

            /* Get the path name */
            HDmemcpy(pathname, p, pathname_len);
            pathname[pathname_len] = '\0';

            /* Skip the path name */
            p += pathname_len;

            /* Mark as external reference */
            ext_ref = TRUE;
        }
            break;
        case H5R_BADTYPE:
        case H5R_MAXTYPE:
        default:
            HDassert("unknown reference type" && 0);
            HGOTO_ERROR(H5E_REFERENCE, H5E_UNSUPPORTED, FAIL, "internal error (unknown reference type)")
    }

    /* Get object location */
    if (ext_ref) {
        H5G_loc_t file_loc;  /* File root group location */

        /* Construct a group location for root group of the file */
        if (FAIL == H5G_root_loc(file, &file_loc))
            HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "unable to create location for file")

        /* Find the object's location */
        if (H5G_loc_find(&file_loc, pathname, &loc/*out*/, H5P_LINK_ACCESS_DEFAULT, dxpl_id) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, FAIL, "object not found")

        /* Find the object's type */
        if (H5O_obj_type(loc.oloc, &obj_type, dxpl_id) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "cannot get object type")
    } else {
        /* Get the # of links for object, and its type */
        /* (To check to make certain that this object hasn't been deleted since the reference was created) */
        if(H5O_get_rc_and_type(&oloc, dxpl_id, &rc, &obj_type) < 0 || 0 == rc)
            HGOTO_ERROR(H5E_REFERENCE, H5E_LINKCOUNT, FAIL, "dereferencing deleted object")
    }

    /* Open the object */
    switch(obj_type) {
        case H5O_TYPE_GROUP:
        {
            H5G_t *group;               /* Pointer to group to open */

            if(NULL == (group = H5G_open(&loc, dxpl_id)))
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "not found")

            /* Create an atom for the group */
            if((ret_value = H5I_register(H5I_GROUP, group, app_ref)) < 0) {
                H5G_close(group);
                HGOTO_ERROR(H5E_SYM, H5E_CANTREGISTER, FAIL, "can't register group")
            } /* end if */
        } /* end case */
        break;

        case H5O_TYPE_NAMED_DATATYPE:
        {
            H5T_t *type;                /* Pointer to datatype to open */

            if(NULL == (type = H5T_open(&loc, dxpl_id)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_NOTFOUND, FAIL, "not found")

            /* Create an atom for the datatype */
            if((ret_value = H5I_register(H5I_DATATYPE, type, app_ref)) < 0) {
                H5T_close(type);
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL, "can't register datatype")
            } /* end if */
        } /* end case */
        break;

        case H5O_TYPE_DATASET:
        {
            H5D_t *dset;                /* Pointer to dataset to open */

            /* Get correct property list */
            if(H5P_DEFAULT == oapl_id)
                oapl_id = H5P_DATASET_ACCESS_DEFAULT;
            else if(TRUE != H5P_isa_class(oapl_id, H5P_DATASET_ACCESS))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not dataset access property list")

            /* Open the dataset */
            if(NULL == (dset = H5D_open(&loc, oapl_id, dxpl_id)))
                HGOTO_ERROR(H5E_DATASET, H5E_NOTFOUND, FAIL, "not found")

            /* Create an atom for the dataset */
            if((ret_value = H5I_register(H5I_DATASET, dset, app_ref)) < 0) {
                H5D_close(dset);
                HGOTO_ERROR(H5E_DATASET, H5E_CANTREGISTER, FAIL, "can't register dataset")
            } /* end if */

            /* TODO Keep ID of the dataset */
//                  dset->shared->dset_id = ret_value;

        } /* end case */
        break;

        case H5O_TYPE_UNKNOWN:
        case H5O_TYPE_NTYPES:
        default:
            HGOTO_ERROR(H5E_REFERENCE, H5E_BADTYPE, FAIL, "can't identify type of object referenced")
    } /* end switch */

done:
    H5MM_xfree(pathname);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5R__get_object() */

/*-------------------------------------------------------------------------
 * Function:    H5Rget_object
 *
 * Purpose: Given a reference to some object, open that object and return an
 * ID for that object.
 *
 * Return:  Valid ID on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Rget_object(hid_t obj_id, hid_t oapl_id, href_t _ref)
{
    struct href_t *ref = (struct href_t *) _ref; /* Reference */
    H5G_loc_t loc;      /* Group location */
    hid_t ret_value;

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(H5G_loc(obj_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(oapl_id < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")
    if(ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")
    if(ref->ref_type <= H5R_BADTYPE || ref->ref_type >= H5R_MAXTYPE)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type")

    /* get_object */
    if((ret_value = H5R__get_object(loc.oloc->file, oapl_id, H5AC_ind_dxpl_id, ref, TRUE)) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable to get_object object")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Rget_object() */

/*-------------------------------------------------------------------------
 * Function:    H5R__get_region
 *
 * Purpose: Given a reference to some object, creates a copy of the dataset
 * pointed to's dataspace and defines a selection in the copy which is the
 * region pointed to.
 *
 * Return:  Pointer to the dataspace on success/NULL on failure
 *
 *-------------------------------------------------------------------------
 */
H5S_t *
H5R__get_region(H5F_t *file, hid_t dxpl_id, href_t _ref)
{
    H5O_loc_t oloc;             /* Object location */
    const uint8_t *p = NULL;    /* Pointer to OID to store */
    struct href_t *ref = (struct href_t *) _ref; /* Reference */
    H5S_t *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file);
    HDassert(ref);
    HDassert((ref->ref_type == H5R_REGION) || (ref->ref_type == H5R_EXT_REGION));

    /* Initialize the object location */
    H5O_loc_reset(&oloc);
    oloc.file = file;

    /* Point to reference buffer now */
    p = (const uint8_t *)ref->ref.serial.buf;

    if (ref->ref_type == H5R_EXT_REGION) {
        size_t filename_len; /* Length of the file name */
        size_t pathname_len; /* Length of the obj path name */

        /* Get the file name length */
        UINT16DECODE(p, filename_len);
        HDassert(filename_len < H5R_MAX_ATTR_REF_NAME_LEN);

        /* Skip the file name */
        p += filename_len;

        /* Get the path name length */
        UINT16DECODE(p, pathname_len);

        /* Skip the path name */
        p += pathname_len;

        /* Unserialize the selection */
        if (NULL == (ret_value = H5S_decode(&p)))
            HGOTO_ERROR(H5E_REFERENCE, H5E_CANTDECODE, NULL, "can't deserialize selection")
    } else {
        /* Get the object oid for the dataset */
        H5F_addr_decode(oloc.file, &p, &(oloc.addr));

        /* Open and copy the dataset's dataspace */
        if((ret_value = H5S_read(&oloc, dxpl_id)) == NULL)
            HGOTO_ERROR(H5E_DATASPACE, H5E_NOTFOUND, NULL, "not found")

        /* Unserialize the selection */
        if(H5S_SELECT_DESERIALIZE(&ret_value, &p) < 0)
            HGOTO_ERROR(H5E_REFERENCE, H5E_CANTDECODE, NULL, "can't deserialize selection")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5R__get_region() */

/*-------------------------------------------------------------------------
 * Function:    H5Rget_region2
 *
 * Purpose: Given a reference to some object, creates a copy of the dataset
 * pointed to's dataspace and defines a selection in the copy which is the
 * region pointed to.
 *
 * Return:  Valid ID on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Rget_region2(hid_t loc_id, href_t _ref)
{
    H5G_loc_t loc;          /* Object's group location */
    H5S_t *space = NULL;    /* Dataspace object */
    struct href_t *ref = (struct href_t *) _ref; /* Reference */
    hid_t ret_value;

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")
    if(ref->ref_type != H5R_REGION && ref->ref_type != H5R_EXT_REGION)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type")

    /* Get the dataspace with the correct region selected */
    if((space = H5R__get_region(loc.oloc->file, H5AC_ind_dxpl_id, ref)) == NULL)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTCREATE, FAIL, "unable to create dataspace")

    /* Atomize */
    if((ret_value = H5I_register (H5I_DATASPACE, space, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register dataspace atom")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Rget_region2() */

/*-------------------------------------------------------------------------
 * Function:    H5R__get_attr
 *
 * Purpose: Given a reference to some attribute, open that attribute and
 * return an ID for that attribute.
 *
 * Return:  Valid ID on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
H5A_t *
H5R__get_attr(H5F_t *file, hid_t dxpl_id, href_t _ref)
{
    H5O_loc_t oloc;                 /* Object location */
    H5G_name_t path;                /* Path of object */
    H5G_loc_t loc;                  /* Group location */
    char *pathname = NULL;          /* Path name of the object (for external references) */
    size_t attr_name_len;           /* Length of the attribute name */
    char *attr_name = NULL;         /* Attribute name */
    const uint8_t *p = NULL;        /* Pointer to OID to store */
    struct href_t *ref = (struct href_t *) _ref; /* Reference */
    H5A_t *ret_value = NULL;        /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file);
    HDassert(ref);
    HDassert((ref->ref_type == H5R_ATTR) || (ref->ref_type == H5R_EXT_ATTR));

    /* Initialize the object location */
    H5O_loc_reset(&oloc);
    oloc.file = file;

    /* Construct a group location for opening the object */
    H5G_name_reset(&path);
    loc.oloc = &oloc;
    loc.path = &path;

    /* Point to reference buffer now */
    p = (const uint8_t *)ref->ref.serial.buf;

    if (ref->ref_type == H5R_EXT_ATTR) {
        H5G_loc_t file_loc;  /* File root group location */
        size_t filename_len; /* Length of the file name */
        size_t pathname_len; /* Length of the obj path name */

        /* Get the file name length */
        UINT16DECODE(p, filename_len);

        /* Skip the file name */
        p += filename_len;

        /* Get the path name length */
        UINT16DECODE(p, pathname_len);

        /* Allocate space for the path name */
        if(NULL == (pathname = (char *)H5MM_malloc(pathname_len + 1)))
            HGOTO_ERROR(H5E_REFERENCE, H5E_CANTALLOC, NULL, "memory allocation failed")

        /* Get the path name */
        HDmemcpy(pathname, p, pathname_len);
        pathname[pathname_len] = '\0';

        /* Skip the path name */
        p += pathname_len;

        /* Construct a group location for root group of the file */
        if (FAIL == H5G_root_loc(file, &file_loc))
            HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, NULL, "unable to create location for file")

        /* Find the object's location */
        if (H5G_loc_find(&file_loc, pathname, &loc/*out*/, H5P_LINK_ACCESS_DEFAULT, dxpl_id) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, NULL, "object not found")
    } else {
        /* Get the object oid for the dataset */
        H5F_addr_decode(oloc.file, &p, &(oloc.addr));
    }

    /* Get the attribute name length */
    UINT16DECODE(p, attr_name_len);
    HDassert(attr_name_len < H5R_MAX_ATTR_REF_NAME_LEN);

    /* Allocate space for the attribute name */
    if(NULL == (attr_name = (char *)H5MM_malloc(attr_name_len + 1)))
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTALLOC, NULL, "memory allocation failed")

    /* Get the attribute name */
    HDmemcpy(attr_name, p, attr_name_len);
    attr_name[attr_name_len] = '\0';

    /* Open the attribute */
    if(NULL == (ret_value = H5A_open(&loc, attr_name, dxpl_id)))
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTOPENOBJ, NULL, "can't open attribute")

done:
    H5MM_xfree(pathname);
    H5MM_xfree(attr_name);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5R__get_attr() */

/*-------------------------------------------------------------------------
 * Function:    H5Rget_attr
 *
 * Purpose: Given a reference to some attribute, open that attribute and
 * return an ID for that attribute.
 *
 * Return:  Valid ID on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Rget_attr(hid_t loc_id, href_t _ref)
{
    struct href_t *ref = (struct href_t *) _ref; /* Reference */
    H5G_loc_t loc;      /* Group location */
    H5A_t *attr;        /* Attribute */
    hid_t ret_value;

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")
    if(ref->ref_type != H5R_ATTR && ref->ref_type != H5R_EXT_ATTR)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type")

    /* Get the attribute */
    if((attr = H5R__get_attr(loc.oloc->file, H5AC_ind_dxpl_id, ref)) == NULL)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTCREATE, FAIL, "unable to open attribute")

    /* Atomize */
    if((ret_value = H5I_register (H5I_ATTR, attr, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register attribute atom")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Rget_attr() */

/*-------------------------------------------------------------------------
 * Function:    H5R__get_obj_type
 *
 * Purpose: Given a reference to some object, this function returns the type
 * of object pointed to.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5R__get_obj_type(H5F_t *file, hid_t dxpl_id, href_t _ref, H5O_type_t *obj_type)
{
    H5O_loc_t oloc;                 /* Object location */
    H5G_name_t path;                /* Path of object */
    H5G_loc_t loc;                  /* Group location */
    char *pathname = NULL;          /* Path name of the object (for external references) */
    unsigned rc;                    /* Reference count of object */
    hbool_t ext_ref = FALSE;        /* External reference */
    const uint8_t *p = NULL;        /* Pointer to OID to store */
    struct href_t *ref = (struct href_t *) _ref; /* Reference */
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file);
    HDassert(ref);
    HDassert(ref->ref_type > H5R_BADTYPE && ref->ref_type < H5R_MAXTYPE);
    HDassert(obj_type);

    /* Initialize the object location */
    H5O_loc_reset(&oloc);
    oloc.file = file;

    /* Construct a group location for opening the object */
    H5G_name_reset(&path);
    loc.oloc = &oloc;
    loc.path = &path;

    /* Point to reference buffer now */
    p = (const uint8_t *)ref->ref.serial.buf;

    switch(ref->ref_type) {
        case H5R_OBJECT:
            /* Get the object oid */
            oloc.addr = ref->ref.addr;
            break;

        case H5R_REGION:
        case H5R_ATTR:
            /* Get the object oid for the dataset */
            H5F_addr_decode(oloc.file, &p, &(oloc.addr));
            break;

        case H5R_EXT_OBJECT:
        case H5R_EXT_REGION:
        case H5R_EXT_ATTR:
        {
            size_t filename_len; /* Length of the file name */
            size_t pathname_len; /* Length of the obj path name */

            /* Get the file name length */
            UINT16DECODE(p, filename_len);
            HDassert(filename_len < H5R_MAX_ATTR_REF_NAME_LEN);

            /* Skip the file name */
            p += filename_len;

            /* Get the path name length */
            UINT16DECODE(p, pathname_len);

            /* Allocate space for the path name */
            if(NULL == (pathname = (char *)H5MM_malloc(pathname_len + 1)))
                HGOTO_ERROR(H5E_REFERENCE, H5E_CANTALLOC, FAIL, "memory allocation failed")

            /* Get the path name */
            HDmemcpy(pathname, p, pathname_len);
            pathname[pathname_len] = '\0';

            /* Skip the path name */
            p += pathname_len;

            /* Mark as external reference */
            ext_ref = TRUE;
            break;
        } /* end case */

        case H5R_BADTYPE:
        case H5R_MAXTYPE:
        default:
            HDassert("unknown reference type" && 0);
            HGOTO_ERROR(H5E_REFERENCE, H5E_UNSUPPORTED, FAIL, "internal error (unknown reference type)")
    } /* end switch */

    if (ext_ref) {
        H5G_loc_t file_loc;  /* File root group location */

        /* Construct a group location for root group of the file */
        if (FAIL == H5G_root_loc(file, &file_loc))
            HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "unable to create location for file")

        /* Find the object's location */
        if (H5G_loc_find(&file_loc, pathname, &loc/*out*/, H5P_LINK_ACCESS_DEFAULT, dxpl_id) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, FAIL, "object not found")

        /* Find the object's type */
        if (H5O_obj_type(loc.oloc, obj_type, dxpl_id) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "cannot get object type")
    } else {
        /* Get the # of links for object, and its type */
        /* (To check to make certain that this object hasn't been deleted since the reference was created) */
        if(H5O_get_rc_and_type(&oloc, dxpl_id, &rc, obj_type) < 0 || 0 == rc)
            HGOTO_ERROR(H5E_REFERENCE, H5E_LINKCOUNT, FAIL, "dereferencing deleted object")
    }

done:
    H5MM_xfree(pathname);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5R__get_obj_type() */

/*-------------------------------------------------------------------------
 * Function:    H5Rget_obj_type3
 *
 * Purpose: Given a reference to some object, this function returns the type
 * of object pointed to.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Rget_obj_type3(hid_t loc_id, href_t _ref, H5O_type_t *obj_type)
{
    H5G_loc_t loc;              /* Object location */
    struct href_t *ref = (struct href_t *) _ref; /* Reference */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")
    if(ref->ref_type <= H5R_BADTYPE || ref->ref_type >= H5R_MAXTYPE)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type")

    /* Get the object information */
    if(H5R__get_obj_type(loc.oloc->file, H5AC_ind_dxpl_id, ref, obj_type) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable to determine object type")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Rget_obj_type3() */

/*-------------------------------------------------------------------------
 * Function:    H5R__get_obj_name
 *
 * Purpose: Given a reference to some object, determine a path to the object
 * referenced in the file.
 *
 * Return:  Non-negative length of the path on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5R__get_obj_name(H5F_t *f, hid_t lapl_id, hid_t dxpl_id, href_t _ref,
    char *name, size_t size)
{
    hid_t file_id = H5I_BADID;  /* ID for file that the reference is in */
    H5O_loc_t oloc;             /* Object location describing object for reference */
    ssize_t ret_value = -1;     /* Return value */
    hbool_t ext_ref = FALSE;    /* External reference */
    const uint8_t *p = NULL;    /* Pointer to reference to decode */
    struct href_t *ref = (struct href_t *) _ref; /* Reference */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check args */
    HDassert(f);
    HDassert(ref);

    /* Initialize the object location */
    H5O_loc_reset(&oloc);
    oloc.file = f;

    /* Point to reference buffer now */
    p = (const uint8_t *)ref->ref.serial.buf;

    /* Get address for reference */
    switch(ref->ref_type) {
        case H5R_OBJECT:
            oloc.addr = ref->ref.addr;
            break;

        case H5R_REGION:
        case H5R_ATTR:
        {
            /* Get the object oid for the dataset */
            H5F_addr_decode(oloc.file, &p, &(oloc.addr));
        } /* end case */
        break;

        case H5R_EXT_OBJECT:
        case H5R_EXT_REGION:
        case H5R_EXT_ATTR:
        {
            size_t filename_len; /* Length of the file name */
            size_t pathname_len; /* Length of the obj path name */
            size_t copy_len;

            /* Get the file name length */
            UINT16DECODE(p, filename_len);
            HDassert(filename_len < H5R_MAX_ATTR_REF_NAME_LEN);

            /* Skip the file name */
            p += filename_len;

            /* Get the path name length */
            UINT16DECODE(p, pathname_len);
            copy_len = MIN(pathname_len, size - 1);

            /* Get the path name */
            if (name) {
                HDmemcpy(name, p, copy_len);
                name[copy_len] = '\0';
            }

            ret_value = (ssize_t)(copy_len + 1);

            /* Mark as external reference */
            ext_ref = TRUE;
        }
            break;

        case H5R_BADTYPE:
        case H5R_MAXTYPE:
        default:
            HDassert("unknown reference type" && 0);
            HGOTO_ERROR(H5E_REFERENCE, H5E_UNSUPPORTED, FAIL, "internal error (unknown reference type)")
    } /* end switch */

    /* Get object location */
    if (!ext_ref) {
        /* Retrieve file ID for name search */
        if((file_id = H5F_get_id(f, FALSE)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTGET, FAIL, "can't get file ID")

        /* Get name, length, etc. */
        if((ret_value = H5G_get_name_by_addr(file_id, lapl_id, dxpl_id, &oloc, name, size)) < 0)
            HGOTO_ERROR(H5E_REFERENCE, H5E_CANTGET, FAIL, "can't determine name")
    }

done:
    /* Close file ID used for search */
    if(file_id > 0 && H5I_dec_ref(file_id) < 0)
        HDONE_ERROR(H5E_REFERENCE, H5E_CANTDEC, FAIL, "can't decrement ref count of temp ID")
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5R__get_obj_name() */

/*-------------------------------------------------------------------------
 * Function:    H5Rget_obj_name
 *
 * Purpose: Given a reference to some object, determine a path to the object
 * referenced in the file.
 *
 * Return:  Non-negative length of the path on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Rget_obj_name(hid_t loc_id, href_t _ref, char *name, size_t size)
{
    H5G_loc_t loc;      /* Group location */
    H5F_t *file;        /* File object */
    struct href_t *ref = (struct href_t *) _ref; /* Reference */
    ssize_t ret_value;  /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")
    if(ref->ref_type <= H5R_BADTYPE || ref->ref_type >= H5R_MAXTYPE)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type")

    /* Get the file pointer from the entry */
    file = loc.oloc->file;

    /* Get name */
    if((ret_value = H5R__get_obj_name(file, H5P_DEFAULT, H5AC_ind_dxpl_id, loc_id, ref, name, size)) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable to determine object path")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Rget_obj_name() */

/*-------------------------------------------------------------------------
 * Function:    H5R__get_attr_name
 *
 * Purpose: Given a reference to some attribute, determine its name.
 *
 * Return:  Non-negative length of the path on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5R__get_attr_name(H5F_t *f, href_t _ref, char *name, size_t size)
{
    H5O_loc_t oloc;             /* Object location describing object for reference */
    ssize_t ret_value = -1;     /* Return value */
    const uint8_t *p = NULL;    /* Pointer to reference to decode */
    size_t attr_name_len;       /* Length of the attribute name */
    size_t copy_len;
    struct href_t *ref = (struct href_t *) _ref; /* Reference */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Check args */
    HDassert(f);
    HDassert(ref);
    HDassert((ref->ref_type == H5R_ATTR) || (ref->ref_type == H5R_EXT_ATTR));

    /* Initialize the object location */
    H5O_loc_reset(&oloc);
    oloc.file = f;

    /* Point to reference buffer now */
    p = (const uint8_t *)ref->ref.serial.buf;

    if (ref->ref_type == H5R_EXT_ATTR) {
        size_t filename_len; /* Length of the file name */
        size_t pathname_len; /* Length of the obj path name */

        /* Get the file name length */
        UINT16DECODE(p, filename_len);
        HDassert(filename_len < H5R_MAX_ATTR_REF_NAME_LEN);

        /* Skip the file name */
        p += filename_len;

        /* Get the path name length */
        UINT16DECODE(p, pathname_len);

        /* Skip the path name */
        p += pathname_len;
    } else {
        /* Get the object oid for the dataset */
        H5F_addr_decode(oloc.file, &p, &(oloc.addr));
    }

    /* Get the attribute name length */
    UINT16DECODE(p, attr_name_len);
    HDassert(attr_name_len < H5R_MAX_ATTR_REF_NAME_LEN);
    copy_len = MIN(attr_name_len, size - 1);

    /* Get the attribute name */
    if (name) {
        HDmemcpy(name, p, copy_len);
        name[copy_len] = '\0';
    }

    ret_value = (ssize_t)(copy_len + 1);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5R__get_attr_name() */

/*-------------------------------------------------------------------------
 * Function:    H5Rget_attr_name
 *
 * Purpose: Given a reference to some attribute, determine its name.
 *
 * Return:  Non-negative length of the path on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Rget_attr_name(hid_t loc_id, href_t _ref, char *name, size_t size)
{
    H5G_loc_t loc;      /* Group location */
    H5F_t *file;        /* File object */
    struct href_t *ref = (struct href_t *) _ref; /* Reference */
    ssize_t ret_value;  /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")
    if((ref->ref_type != H5R_ATTR) && (ref->ref_type != H5R_EXT_ATTR))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type")

    /* Get the file pointer from the entry */
    file = loc.oloc->file;

    /* Get name */
    if((ret_value = H5R__get_attr_name(file, ref, name, size)) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable to determine object path")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Rget_attr_name() */

/*-------------------------------------------------------------------------
 * Function:    H5R__get_file_name
 *
 * Purpose: Given a reference to some object, determine a file name of the
 * object located into.
 *
 * Return:  Non-negative length of the path on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static ssize_t
H5R__get_file_name(href_t _ref, char *name, size_t size)
{
    struct href_t *ref = (struct href_t *) _ref; /* Reference */
    ssize_t ret_value = -1;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check args */
    HDassert(ref);

    switch (ref->ref_type) {
        case H5R_EXT_OBJECT:
        case H5R_EXT_REGION:
        case H5R_EXT_ATTR:
        {
            const uint8_t *p;    /* Pointer to reference to decode */
            size_t filename_len; /* Length of the file name */
            size_t copy_len;

            /* Get the file name length */
            p = (const uint8_t *)ref->ref.serial.buf;
            UINT16DECODE(p, filename_len);
            copy_len = MIN(filename_len, size - 1);

            /* Get the attribute name */
            if (name) {
                HDmemcpy(name, p, copy_len);
                name[copy_len] = '\0';
            }
            ret_value = (ssize_t)(copy_len + 1);
        }
            break;
        case H5R_OBJECT:
        case H5R_REGION:
        case H5R_ATTR:
        case H5R_BADTYPE:
        case H5R_MAXTYPE:
        default:
            HDassert("unknown reference type" && 0);
            HGOTO_ERROR(H5E_REFERENCE, H5E_UNSUPPORTED, FAIL, "internal error (unknown reference type)")
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5R__get_file_name() */

/*-------------------------------------------------------------------------
 * Function:    H5Rget_file_name
 *
 * Purpose: Given a reference to some object, determine a file name of the
 * object located into.
 *
 * Return:  Non-negative length of the path on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Rget_file_name(href_t _ref, char *name, size_t size)
{
    struct href_t *ref = (struct href_t *) _ref; /* Reference */
    ssize_t ret_value;  /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Check args */
    if(ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")
    if(ref->ref_type <= H5R_BADTYPE || ref->ref_type >= H5R_MAXTYPE)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type")

    /* Get name */
    if((ret_value = H5R__get_file_name(ref, name, size)) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable to retrieve file name")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Rget_file_name() */
