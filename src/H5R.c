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

/****************/
/* Module Setup */
/****************/

#include "H5Rmodule.h"          /* This source code file is part of the H5R module */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5ACprivate.h"        /* Metadata cache                       */
#include "H5Aprivate.h"		/* Attributes				*/
#include "H5Dprivate.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Gprivate.h"		/* Groups				*/
#include "H5HGprivate.h"	/* Global Heaps				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Rpkg.h"		/* References				*/
#include "H5Sprivate.h"		/* Dataspaces 				*/


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

static H5S_t * H5R_get_region(H5F_t *file, hid_t dxpl_id, H5R_type_t ref_type, const void *_ref);
static ssize_t H5R_get_name(H5F_t *file, hid_t lapl_id, hid_t dxpl_id, hid_t id,
    H5R_type_t ref_type, const void *_ref, char *name, size_t size);


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
    H5I_REFERENCE,		/* ID class value */
    0,				/* Class flags */
    0,				/* # of reserved IDs for class */
    NULL			/* Callback routine for closing objects of this class */
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


/*--------------------------------------------------------------------------
 NAME
    H5R_create
 PURPOSE
    Creates a particular kind of reference for the user
 USAGE
    herr_t H5R_create(ref, ref_type, ...)
        void *ref;              OUT: Reference created
        H5R_type_t ref_type;    IN: Type of reference to create
        Additional arguments:
            H5G_loc_t *loc;     IN: File location used to locate object pointed to
            const char *name;   IN: Name of object at location of object pointed to
            hid_t dxpl_id;      IN: Property list ID
            H5S_t *space;       IN: Dataspace with selection, used for Dataset
                                    Region references.
            const char *attr_name;   IN: Name of attribute pointed to, used for
                                     Attribute references.

 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Creates a particular type of reference specified with REF_TYPE, in the
    space pointed to by REF.  The LOC_ID and NAME are used to locate the object
    pointed to and the SPACE_ID is used to choose the region pointed to (for
    Dataset Region references) or ATTR_NAME is used to choose tha attribute
    pointed to (for Attribute references).
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5R_create(void *_ref, H5R_type_t ref_type, ...)
{
    H5G_loc_t obj_loc;          /* Group hier. location of object */
    H5G_name_t path;            /* Object group hier. path */
    H5O_loc_t oloc;             /* Object object location */
    uint8_t *buf = NULL;        /* Buffer to store serialized selection in */
    hbool_t obj_found = FALSE;  /* Object location found */
    va_list ap;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(ref_type > H5R_BADTYPE && ref_type < H5R_MAXTYPE);

    va_start(ap, ref_type);

    /* Set up object location to fill in */
    obj_loc.oloc = &oloc;
    obj_loc.path = &path;
    H5G_loc_reset(&obj_loc);

    switch(ref_type) {
        case H5R_OBJECT:
        {
            H5G_loc_t *loc;
            const char *name;
            hid_t dxpl_id;
            hobj_ref_t *ref = (hobj_ref_t *)_ref; /* Get pointer to correct type of reference struct */

            /* Get arguments */
            loc = va_arg(ap, H5G_loc_t *);
            name = va_arg(ap, const char *);
            dxpl_id = va_arg(ap, hid_t);

            HDassert(ref);
            HDassert(loc);
            HDassert(name);

            /* Find the object */
            if(H5G_loc_find(loc, name, &obj_loc, H5P_DEFAULT, dxpl_id) < 0)
                HGOTO_ERROR(H5E_REFERENCE, H5E_NOTFOUND, FAIL, "object not found")
            obj_found = TRUE;

            *ref = obj_loc.oloc->addr;
            break;
        }

        case H5R_DATASET_REGION:
        {
            H5G_loc_t *loc;
            const char *name;
            hid_t dxpl_id;
            H5HG_t hobjid;      /* Heap object ID */
            hdset_reg_ref_t *ref = (hdset_reg_ref_t *)_ref; /* Get pointer to correct type of reference struct */
            hssize_t buf_size;  /* Size of buffer needed to serialize selection */
            uint8_t *p;       /* Pointer to OID to store */
            hbool_t heapid_found;  /* Flag for non-zero heap ID found */
            unsigned u;        /* local index */
            H5S_t *space;

            /* Get arguments */
            loc = va_arg(ap, H5G_loc_t *);
            name = va_arg(ap, const char *);
            dxpl_id = va_arg(ap, hid_t);
            space = va_arg(ap, H5S_t *);

            HDassert(ref);
            HDassert(loc);
            HDassert(name);
            HDassert(space);

            /* Find the object */
            if(H5G_loc_find(loc, name, &obj_loc, H5P_DEFAULT, dxpl_id) < 0)
                HGOTO_ERROR(H5E_REFERENCE, H5E_NOTFOUND, FAIL, "object not found")
            obj_found = TRUE;

            /* Set up information for dataset region */

            /* Return any previous heap block to the free list if we are garbage collecting */
            if(H5F_GC_REF(loc->oloc->file)) {
                /* Check for an existing heap ID in the reference */
                for(u = 0, heapid_found = FALSE, p = (uint8_t *)ref; u < H5R_DSET_REG_REF_BUF_SIZE; u++)
                    if(p[u] != 0) {
                        heapid_found = TRUE;
                        break;
                    } /* end if */

                if(heapid_found) {
/* Return heap block to free list */
                } /* end if */
            } /* end if */

            /* Zero the heap ID out, may leak heap space if user is re-using reference and doesn't have garbage collection on */
            HDmemset(ref, 0, H5R_DSET_REG_REF_BUF_SIZE);

            /* Get the amount of space required to serialize the selection */
            if((buf_size = H5S_SELECT_SERIAL_SIZE(space)) < 0)
                HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "Invalid amount of space for serializing selection")

            /* Increase buffer size to allow for the dataset OID */
            buf_size += (hssize_t)sizeof(haddr_t);

            /* Allocate the space to store the serialized information */
            H5_CHECK_OVERFLOW(buf_size, hssize_t, size_t);
            if(NULL == (buf = (uint8_t *)H5MM_malloc((size_t)buf_size)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

            /* Serialize information for dataset OID into heap buffer */
            p = (uint8_t *)buf;
            H5F_addr_encode(loc->oloc->file, &p, obj_loc.oloc->addr);

            /* Serialize the selection into heap buffer */
            if(H5S_SELECT_SERIALIZE(space, &p) < 0)
                HGOTO_ERROR(H5E_REFERENCE, H5E_CANTCOPY, FAIL, "Unable to serialize selection")

            /* Save the serialized buffer for later */
            if(H5HG_insert(loc->oloc->file, dxpl_id, (size_t)buf_size, buf, &hobjid) < 0)
                HGOTO_ERROR(H5E_REFERENCE, H5E_WRITEERROR, FAIL, "Unable to serialize selection")

            /* Serialize the heap ID and index for storage in the file */
            p = (uint8_t *)ref;
            H5F_addr_encode(loc->oloc->file, &p, hobjid.addr);
            UINT32ENCODE(p, hobjid.idx);

            break;
        }

        case H5R_REGION:
        {
            H5G_loc_t *loc;
            const char *name;
            hid_t dxpl_id;
            hreg_ref_t *ref = (hreg_ref_t *)_ref; /* Get pointer to correct type of reference struct */
            hssize_t buf_size;    /* Size of buffer needed to serialize selection */
            uint8_t *p;         /* Pointer to OID to store */
            H5S_t *space;

            /* Get arguments */
            loc = va_arg(ap, H5G_loc_t *);
            name = va_arg(ap, const char *);
            dxpl_id = va_arg(ap, hid_t);
            space = va_arg(ap, H5S_t *);

            HDassert(ref);
            HDassert(loc);
            HDassert(name);
            HDassert(space);

            /* Find the object */
            if(H5G_loc_find(loc, name, &obj_loc, H5P_DEFAULT, dxpl_id) < 0)
                HGOTO_ERROR(H5E_REFERENCE, H5E_NOTFOUND, FAIL, "object not found")
            obj_found = TRUE;

            /* Get the amount of space required to serialize the selection */
            if((buf_size = H5S_SELECT_SERIAL_SIZE(space)) < 0)
                HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "Invalid amount of space for serializing selection")

            /* Increase buffer size to allow for the dataset OID */
            buf_size += (hssize_t)sizeof(haddr_t);

            /* Allocate the space to store the serialized information */
            H5_CHECK_OVERFLOW(buf_size, hssize_t, size_t);

            if(!ref->buf || ref->buf_size < (size_t) buf_size) {
                ref->buf_size = (size_t)buf_size;
            } else {
                /* Serialize information for dataset OID into buffer */
                p = (uint8_t *)ref->buf;
                H5F_addr_encode(loc->oloc->file, &p, obj_loc.oloc->addr);

                /* Serialize the selection into buffer */
                if(H5S_SELECT_SERIALIZE(space, &p) < 0)
                    HGOTO_ERROR(H5E_REFERENCE, H5E_CANTCOPY, FAIL, "Unable to serialize selection")
            }
        }
            break;

        case H5R_ATTR:
        {
            H5G_loc_t *loc;
            const char *name;
            hid_t dxpl_id;
            H5HG_t hobjid;      /* Heap object ID */
            hattr_ref_t *ref = (hattr_ref_t *)_ref; /* Get pointer to correct type of reference struct */
            size_t buf_size;    /* Size of buffer needed to serialize selection */
            size_t attr_name_len; /* Length of the attribute name */
            uint8_t *p;         /* Pointer to OID to store */
            hbool_t heapid_found;  /* Flag for non-zero heap ID found */
            unsigned u;         /* local index */
            const char *attr_name;

            /* Get arguments */
            loc = va_arg(ap, H5G_loc_t *);
            name = va_arg(ap, const char *);
            dxpl_id = va_arg(ap, hid_t);
            attr_name = va_arg(ap, const char *);

            HDassert(ref);
            HDassert(loc);
            HDassert(name);
            HDassert(attr_name);

            /* Find the object */
            if(H5G_loc_find(loc, name, &obj_loc, H5P_DEFAULT, dxpl_id) < 0)
                HGOTO_ERROR(H5E_REFERENCE, H5E_NOTFOUND, FAIL, "object not found")
            obj_found = TRUE;

            /* Set up information for attribute */

            /* Return any previous heap block to the free list if we are garbage collecting */
            if(H5F_GC_REF(loc->oloc->file)) {
                /* Check for an existing heap ID in the reference */
                for(u = 0, heapid_found = FALSE, p = (uint8_t *)ref; u < H5R_ATTR_REF_BUF_SIZE; u++)
                    if(p[u] != 0) {
                        heapid_found = TRUE;
                        break;
                    } /* end if */

                if(heapid_found) {
/* Return heap block to free list */
                } /* end if */
            } /* end if */

            /* Zero the heap ID out, may leak heap space if user is re-using reference and doesn't have garbage collection on */
            HDmemset(ref, 0, H5R_ATTR_REF_BUF_SIZE);

            /* Get the amount of space required to serialize the attribute name */
            attr_name_len = HDstrlen(attr_name);
            if(attr_name_len >= H5R_MAX_ATTR_REF_NAME_LEN)
                HGOTO_ERROR(H5E_REFERENCE, H5E_ARGS, FAIL, "attribute name too long")

            /* Compute buffer size, allow for the attribute name length and object's OID */
            buf_size = attr_name_len + 2 + sizeof(haddr_t);

            /* Allocate the space to store the serialized information */
            if(NULL == (buf = (uint8_t *)H5MM_malloc(buf_size)))
                HGOTO_ERROR(H5E_REFERENCE, H5E_CANTALLOC, FAIL, "memory allocation failed")

            /* Serialize information for object's OID into heap buffer */
            p = (uint8_t *)buf;
            H5F_addr_encode(loc->oloc->file, &p, obj_loc.oloc->addr);

            /* Serialize information for attribute name length into the buffer */
            UINT16ENCODE(p, attr_name_len);

            /* Copy the attribute name into the buffer */
            HDmemcpy(p, attr_name, attr_name_len);

            /* Sanity check */
            HDassert((size_t)((p + attr_name_len) - buf) == buf_size);

            /* Save the serialized buffer for later */
            if(H5HG_insert(loc->oloc->file, dxpl_id, buf_size, buf, &hobjid) < 0)
                HGOTO_ERROR(H5E_REFERENCE, H5E_WRITEERROR, FAIL, "Unable to serialize selection")

            /* Serialize the heap ID and index for storage in the file */
            p = (uint8_t *)ref;
            H5F_addr_encode(loc->oloc->file, &p, hobjid.addr);
            UINT32ENCODE(p, hobjid.idx);

            break;
        }

        case H5R_BADTYPE:
        case H5R_MAXTYPE:
        default:
            HDassert("unknown reference type" && 0);
            HGOTO_ERROR(H5E_REFERENCE, H5E_UNSUPPORTED, FAIL, "internal error (unknown reference type)")
    } /* end switch */

done:
    if(obj_found)
        H5G_loc_free(&obj_loc);

    H5MM_free(buf);

    va_end(ap);

    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5R_create() */


/*--------------------------------------------------------------------------
 NAME
    H5Rcreate
 PURPOSE
    Creates a particular kind of reference for the user
 USAGE
    herr_t H5Rcreate(ref, ref_type, ...)
        void *ref;          OUT: Reference created
        H5R_type_t ref_type;    IN: Type of reference to create
        Additional arguments:
            hid_t loc_id;       IN: Location ID used to locate object pointed to
            const char *name;   IN: Name of object at location LOC_ID of object
                                    pointed to
            hid_t space_id;     IN: Dataspace ID with selection, used for Dataset
                                    Region references.
            const char *attr_name;   IN: Name of attribute pointed to, used for
                                     Attribute references.

 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Creates a particular type of reference specified with REF_TYPE, in the
    space pointed to by REF.  The LOC_ID and NAME are used to locate the object
    pointed to and the SPACE_ID is used to choose the region pointed to (for
    Dataset Region references).
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5Rcreate(void *ref, H5R_type_t ref_type, ...)
{
    H5S_t      *space = NULL;   /* Pointer to dataspace containing region */
    va_list     ap;
    herr_t      ret_value = FAIL; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "*xRt", ref, ref_type);

    va_start(ap, ref_type);

    /* Check args */
    if(ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")

    switch (ref_type) {
        case H5R_OBJECT:
        {
            hid_t loc_id;
            H5G_loc_t loc; /* File location */
            const char *name;

            /* Get arguments */
            loc_id = va_arg(ap, hid_t);
            name = va_arg(ap, const char *);

            if(H5G_loc(loc_id, &loc) < 0)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
            if(!name || !*name)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name given")

            if((ret_value = H5R_create(ref, ref_type, &loc, name, H5AC_dxpl_id)) < 0)
                HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable to create reference")
        }
            break;
        case H5R_REGION:
        case H5R_DATASET_REGION:
        {
            hid_t loc_id;
            H5G_loc_t loc; /* File location */
            const char *name;
            hid_t space_id;

            /* Get arguments */
            loc_id = va_arg(ap, hid_t);
            name = va_arg(ap, const char *);
            space_id = va_arg(ap, hid_t);

            if(H5G_loc(loc_id, &loc) < 0)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
            if(!name || !*name)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name given")
            if(space_id == H5I_BADID)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "reference region dataspace id must be valid")
            if(space_id != H5I_BADID && (NULL == (space = (H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE))))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace")
            if((ret_value = H5R_create(ref, ref_type, &loc, name, H5AC_dxpl_id, space)) < 0)
                HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable to create reference")
        }
            break;
        case H5R_ATTR:
        {
            hid_t loc_id;
            H5G_loc_t loc; /* File location */
            const char *name;
            const char *attr_name;

            /* Get arguments */
            loc_id = va_arg(ap, hid_t);
            name = va_arg(ap, const char *);
            attr_name = va_arg(ap, const char *);

            if(H5G_loc(loc_id, &loc) < 0)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
            if(!name || !*name)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name given")
            if(!attr_name || !*attr_name)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no attribute name given")
            if((ret_value = H5R_create(ref, ref_type, &loc, name, H5AC_dxpl_id, attr_name)) < 0)
                HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable to create reference")
        }
            break;
        case H5R_MAXTYPE:
        case H5R_BADTYPE:
        default:
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type")
    }

done:
    va_end(ap);

    FUNC_LEAVE_API(ret_value)
}   /* end H5Rcreate() */


/*--------------------------------------------------------------------------
 NAME
    H5R_dereference
 PURPOSE
    Opens the HDF5 object referenced.
 USAGE
    hid_t H5R_dereference(ref)
        H5F_t *file;        IN: File the object being dereferenced is within
        H5R_type_t ref_type;    IN: Type of reference
        void *ref;          IN: Reference to open.

 RETURNS
    Valid ID on success, Negative on failure
 DESCRIPTION
    Given a reference to some object, open that object and return an ID for
    that object.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    Currently only set up to work with references to datasets
 EXAMPLES
 REVISION LOG
    Raymond Lu
    13 July 2011
    I added the OAPL_ID parameter for the object being referenced.  It only
    supports dataset access property list currently.

    M. Scot Breitenfeld
    3 March 2015
    Added a check for undefined reference pointer.
--------------------------------------------------------------------------*/
hid_t
H5R_dereference(H5F_t *file, hid_t oapl_id, hid_t dxpl_id, H5R_type_t ref_type, const void *_ref, hbool_t app_ref)
{
    H5O_loc_t oloc;             /* Object location */
    H5G_name_t path;            /* Path of object */
    H5G_loc_t loc;              /* Group location */
    char *attr_name = NULL;     /* Name of the attribute (for attribute references) */
    unsigned rc;		/* Reference count of object */
    H5O_type_t obj_type;        /* Type of object */
    hid_t ret_value = H5I_INVALID_HID;  /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(_ref);
    HDassert(ref_type > H5R_BADTYPE && ref_type < H5R_MAXTYPE);
    HDassert(file);

    /* Initialize the object location */
    H5O_loc_reset(&oloc);
    oloc.file = file;

    switch(ref_type) {
        case H5R_OBJECT:
            oloc.addr = *(const hobj_ref_t *)_ref; /* Only object references currently supported */
	    if(!H5F_addr_defined(oloc.addr) || oloc.addr == 0)
	      HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Undefined reference pointer")
	      break;
            
        case H5R_DATASET_REGION:
        {
            H5HG_t hobjid;  /* Heap object ID */
            uint8_t *buf;   /* Buffer to store serialized selection in */
            const uint8_t *p;           /* Pointer to OID to store */

            /* Get the heap ID for the dataset region */
            p = (const uint8_t *)_ref;
            H5F_addr_decode(oloc.file, &p, &(hobjid.addr));
            UINT32DECODE(p, hobjid.idx);

            if(!H5F_addr_defined(hobjid.addr) || hobjid.addr == 0)
	      HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Undefined reference pointer")

            /* Get the dataset region from the heap (allocate inside routine) */
            if(NULL == (buf = (uint8_t *)H5HG_read(oloc.file, dxpl_id, &hobjid, NULL, NULL)))
                HGOTO_ERROR(H5E_REFERENCE, H5E_READERROR, FAIL, "Unable to read dataset region information")

            /* Get the object oid for the dataset */
            p = buf;
            H5F_addr_decode(oloc.file, &p, &(oloc.addr));

            /* Free the buffer allocated in H5HG_read() */
            H5MM_xfree(buf);
        } /* end case */
        break;

        case H5R_ATTR:
        {
            H5HG_t hobjid;      /* Heap object ID */
            uint8_t *buf;       /* Buffer to store serialized selection in */
            const uint8_t *p;   /* Pointer to OID to store */
            size_t attr_name_len;       /* Length of the attribute name */

            /* Get the heap ID for the dataset region */
            p = (const uint8_t *)_ref;
            H5F_addr_decode(oloc.file, &p, &(hobjid.addr));
            UINT32DECODE(p, hobjid.idx);

            if(!H5F_addr_defined(hobjid.addr) || hobjid.addr == 0)
	      HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Undefined reference pointer")

            /* Get the dataset region from the heap (allocate inside routine) */
            if(NULL == (buf = (uint8_t *)H5HG_read(oloc.file, dxpl_id, &hobjid, NULL, NULL)))
                HGOTO_ERROR(H5E_REFERENCE, H5E_READERROR, FAIL, "Unable to read dataset region information")

            /* Get the object oid for the dataset */
            p = buf;
            H5F_addr_decode(oloc.file, &p, &(oloc.addr));

            /* Get the attribute name length */
            UINT16DECODE(p, attr_name_len);
            HDassert(attr_name_len < H5R_MAX_ATTR_REF_NAME_LEN);

            /* Allocate space for the attribute name */
            if(NULL == (attr_name = (char *)H5MM_malloc(attr_name_len)))
                HGOTO_ERROR(H5E_REFERENCE, H5E_CANTALLOC, FAIL, "memory allocation failed")

            /* Get the attribute name */
            HDmemcpy(attr_name, p, attr_name_len);
            attr_name[attr_name_len] = '\0';

            /* Free the buffer allocated in H5HG_read() */
            H5MM_xfree(buf);
        } /* end case */
        break;

        case H5R_REGION:
        {
            const hreg_ref_t *ref = (const hreg_ref_t *)_ref; /* Get pointer to correct type of reference struct */
            const uint8_t *p; /* Pointer to OID to store */

            /* Get the object oid for the dataset */
            p = (const uint8_t *)ref->buf;
            H5F_addr_decode(oloc.file, &p, &(oloc.addr));
        } /* end case */
        break;

        case H5R_BADTYPE:
        case H5R_MAXTYPE:
        default:
            HDassert("unknown reference type" && 0);
            HGOTO_ERROR(H5E_REFERENCE, H5E_UNSUPPORTED, FAIL, "internal error (unknown reference type)")
    } /* end switch */

    /* Get the # of links for object, and its type */
    /* (To check to make certain that this object hasn't been deleted since the reference was created) */
    if(H5O_get_rc_and_type(&oloc, dxpl_id, &rc, &obj_type) < 0 || 0 == rc)
        HGOTO_ERROR(H5E_REFERENCE, H5E_LINKCOUNT, FAIL, "dereferencing deleted object")

    /* Construct a group location for opening the object */
    H5G_name_reset(&path);
    loc.oloc = &oloc;
    loc.path = &path;

    /* Open an attribute */
    if(H5R_ATTR == ref_type) {
        H5A_t *attr;            /* Attribute opened */

        /* Open the attribute */
        if(NULL == (attr = H5A_open(&loc, attr_name, dxpl_id)))
            HGOTO_ERROR(H5E_REFERENCE, H5E_CANTOPENOBJ, FAIL, "can't open attribute")

        /* Create an atom for the attribute */
        if((ret_value = H5I_register(H5I_ATTR, attr, app_ref)) < 0) {
            H5A_close(attr);
            HGOTO_ERROR(H5E_REFERENCE, H5E_CANTREGISTER, FAIL, "can't register attribute")
        } /* end if */
    } /* end if */
    else {
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
    } /* end else */

done:
    if(attr_name)
        H5MM_xfree(attr_name);

    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5R_dereference() */


/*--------------------------------------------------------------------------
 NAME
    H5Rdereference2
 PURPOSE
    Opens the HDF5 object referenced.
 USAGE
    hid_t H5Rdereference2(ref)
        hid_t id;       IN: Dataset reference object is in or location ID of
                            object that the dataset is located within.
        hid_t oapl_id;  IN: Property list of the object being referenced.
        H5R_type_t ref_type;    IN: Type of reference to create
        void *ref;      IN: Reference to open.

 RETURNS
    Valid ID on success, Negative on failure
 DESCRIPTION
    Given a reference to some object, open that object and return an ID for
    that object.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
    Raymond Lu
    13 July 2011
    I added the OAPL_ID parameter for the object being referenced.  It only
    supports dataset access property list currently.
--------------------------------------------------------------------------*/
hid_t
H5Rdereference2(hid_t obj_id, hid_t oapl_id, H5R_type_t ref_type, const void *_ref)
{
    H5G_loc_t loc;      /* Group location */
    hid_t ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE4("i", "iiRt*x", obj_id, oapl_id, ref_type, _ref);

    /* Check args */
    if(H5G_loc(obj_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(oapl_id < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")
    if(ref_type <= H5R_BADTYPE || ref_type >= H5R_MAXTYPE)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type")
    if(_ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")

    /* Create reference */
    if((ret_value = H5R_dereference(loc.oloc->file, oapl_id, H5AC_ind_dxpl_id, ref_type, _ref, TRUE)) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable to dereference object")

done:
    FUNC_LEAVE_API(ret_value)
}   /* end H5Rdereference2() */


/*--------------------------------------------------------------------------
 NAME
    H5R_get_region
 PURPOSE
    Retrieves a dataspace with the region pointed to selected.
 USAGE
    H5S_t *H5R_get_region(file, dxpl_id, ref_type, ref)
        H5F_t *file;            IN: File the object being dereferenced is within
        hid_t dxpl_id;          IN: Property list ID
        H5R_type_t ref_type;    IN: Type of reference to get region of
        void *ref;              IN: Reference to open.

 RETURNS
    Pointer to the dataspace on success, NULL on failure
 DESCRIPTION
    Given a reference to some object, creates a copy of the dataset pointed
    to's dataspace and defines a selection in the copy which is the region
    pointed to.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static H5S_t *
H5R_get_region(H5F_t *file, hid_t dxpl_id, H5R_type_t ref_type, const void *_ref)
{
    H5O_loc_t oloc;             /* Object location */
    const uint8_t *p;           /* Pointer to OID to store */
    H5HG_t hobjid;              /* Heap object ID */
    uint8_t *buf = NULL;        /* Buffer to store serialized selection in */
    H5S_t *ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(_ref);
    HDassert(file);

    /* Initialize the object location */
    H5O_loc_reset(&oloc);
    oloc.file = file;

    if (ref_type == H5R_DATASET_REGION) {
        /* Get the heap ID for the dataset region */
        p = (const uint8_t *)_ref;
        H5F_addr_decode(oloc.file, &p, &(hobjid.addr));
        UINT32DECODE(p, hobjid.idx);

        /* Get the dataset region from the heap (allocate inside routine) */
        if((buf = (uint8_t *)H5HG_read(oloc.file, dxpl_id, &hobjid, NULL, NULL)) == NULL)
            HGOTO_ERROR(H5E_REFERENCE, H5E_READERROR, NULL, "Unable to read dataset region information")

        /* Point to heap buffer now */
        p = buf;
    } else {
        const hreg_ref_t *ref = (const hreg_ref_t *)_ref; /* Get pointer to correct type of reference struct */

        /* Point to reference buffer now */
        p = (const uint8_t *)ref->buf;
    }

    /* Get the object oid for the dataset */
    H5F_addr_decode(oloc.file, &p, &(oloc.addr));

    /* Open and copy the dataset's dataspace */
    if((ret_value = H5S_read(&oloc, dxpl_id)) == NULL)
        HGOTO_ERROR(H5E_DATASPACE, H5E_NOTFOUND, NULL, "not found")

    /* Unserialize the selection */
    if(H5S_SELECT_DESERIALIZE(&ret_value, &p) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTDECODE, NULL, "can't deserialize selection")

done:
    /* Free the buffer allocated in H5HG_read() */
    H5MM_free(buf);

    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5R_get_region() */


/*--------------------------------------------------------------------------
 NAME
    H5Rget_region
 PURPOSE
    Retrieves a dataspace with the region pointed to selected.
 USAGE
    hid_t H5Rget_region(id, ref_type, ref)
        hid_t id;       IN: Dataset reference object is in or location ID of
                            object that the dataset is located within.
        H5R_type_t ref_type;    IN: Type of reference to get region of
        void *ref;        IN: Reference to open.

 RETURNS
    Valid ID on success, Negative on failure
 DESCRIPTION
    Given a reference to some object, creates a copy of the dataset pointed
    to's dataspace and defines a selection in the copy which is the region
    pointed to.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hid_t
H5Rget_region(hid_t id, H5R_type_t ref_type, const void *ref)
{
    H5G_loc_t loc;          /* Object's group location */
    H5S_t *space = NULL;    /* Dataspace object */
    hid_t ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("i", "iRt*x", id, ref_type, ref);

    /* Check args */
    if(H5G_loc(id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(ref_type != H5R_DATASET_REGION && ref_type != H5R_REGION)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type")
    if(ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")

    /* Get the dataspace with the correct region selected */
    if((space = H5R_get_region(loc.oloc->file, H5AC_ind_dxpl_id, ref_type, ref)) == NULL)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTCREATE, FAIL, "unable to create dataspace")

    /* Atomize */
    if((ret_value = H5I_register (H5I_DATASPACE, space, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register dataspace atom")

done:
    FUNC_LEAVE_API(ret_value)
}   /* end H5Rget_region() */


/*--------------------------------------------------------------------------
 NAME
    H5R_get_obj_type
 PURPOSE
    Retrieves the type of object that an object reference points to
 USAGE
    H5O_type_t H5R_get_obj_type(file, ref_type, ref)
        H5F_t *file;        IN: File the object being dereferenced is within
        H5R_type_t ref_type;    IN: Type of reference to query
        void *ref;          IN: Reference to query.

 RETURNS
    Success:	An object type defined in H5Gpublic.h
    Failure:	H5G_UNKNOWN
 DESCRIPTION
    Given a reference to some object, this function returns the type of object
    pointed to.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5R_get_obj_type(H5F_t *file, hid_t dxpl_id, H5R_type_t ref_type,
    const void *_ref, H5O_type_t *obj_type)
{
    H5O_loc_t oloc;             /* Object location */
    unsigned rc;		/* Reference count of object    */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file);
    HDassert(_ref);

    /* Initialize the symbol table entry */
    H5O_loc_reset(&oloc);
    oloc.file = file;

    switch(ref_type) {
        case H5R_OBJECT:
            /* Get the object oid */
            oloc.addr = *(const hobj_ref_t *)_ref; /* Only object references currently supported */
            break;

        case H5R_DATASET_REGION:
        {
            H5HG_t hobjid;      /* Heap object ID */
            const uint8_t *p;   /* Pointer to reference to decode */
            uint8_t *buf;       /* Buffer to store serialized selection in */

            /* Get the heap ID for the dataset region */
            p = (const uint8_t *)_ref;
            H5F_addr_decode(oloc.file, &p, &(hobjid.addr));
            UINT32DECODE(p, hobjid.idx);

            /* Get the dataset region from the heap (allocate inside routine) */
            if((buf = (uint8_t *)H5HG_read(oloc.file, dxpl_id, &hobjid, NULL, NULL)) == NULL)
                HGOTO_ERROR(H5E_REFERENCE, H5E_READERROR, FAIL, "Unable to read dataset region information")

            /* Get the object oid for the dataset */
            p = buf;
            H5F_addr_decode(oloc.file, &p, &(oloc.addr));

            /* Free the buffer allocated in H5HG_read() */
            H5MM_xfree(buf);
            break;
        } /* end case */

        case H5R_ATTR:
        {
            H5HG_t hobjid;      /* Heap object ID */
            const uint8_t *p;   /* Pointer to reference to decode */
            uint8_t *buf;       /* Buffer to store serialized selection in */

            /* Get the heap ID for the dataset region */
            p = (const uint8_t *)_ref;
            H5F_addr_decode(oloc.file, &p, &(hobjid.addr));
            UINT32DECODE(p, hobjid.idx);

            /* Get the dataset region from the heap (allocate inside routine) */
            if((buf = (uint8_t *)H5HG_read(oloc.file, dxpl_id, &hobjid, NULL, NULL)) == NULL)
                HGOTO_ERROR(H5E_REFERENCE, H5E_READERROR, FAIL, "Unable to read attribute reference information")

            /* Get the object oid for the dataset */
            p = buf;
            H5F_addr_decode(oloc.file, &p, &(oloc.addr));

            /* Free the buffer allocated in H5HG_read() */
            H5MM_xfree(buf);
            break;
        } /* end case */

        case H5R_BADTYPE:
        case H5R_MAXTYPE:
        default:
            HDassert("unknown reference type" && 0);
            HGOTO_ERROR(H5E_REFERENCE, H5E_UNSUPPORTED, FAIL, "internal error (unknown reference type)")
    } /* end switch */

    /* Get the # of links for object, and its type */
    /* (To check to make certain that this object hasn't been deleted since the reference was created) */
    if(H5O_get_rc_and_type(&oloc, dxpl_id, &rc, obj_type) < 0 || 0 == rc)
        HGOTO_ERROR(H5E_REFERENCE, H5E_LINKCOUNT, FAIL, "dereferencing deleted object")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5R_get_obj_type() */


/*--------------------------------------------------------------------------
 NAME
    H5Rget_obj_type2
 PURPOSE
    Retrieves the type of object that an object reference points to
 USAGE
    herr_t H5Rget_obj_type2(id, ref_type, ref, obj_type)
        hid_t id;       IN: Dataset reference object is in or location ID of
                            object that the dataset is located within.
        H5R_type_t ref_type;    IN: Type of reference to query
        void *ref;          IN: Reference to query.
        H5O_type_t *obj_type;   OUT: Type of object reference points to

 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Given a reference to some object, this function retrieves the type of
    object pointed to.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5Rget_obj_type2(hid_t id, H5R_type_t ref_type, const void *ref,
    H5O_type_t *obj_type)
{
    H5G_loc_t loc;              /* Object location */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE4("e", "iRt*x*Ot", id, ref_type, ref, obj_type);

    /* Check args */
    if(H5G_loc(id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(ref_type <= H5R_BADTYPE || ref_type >= H5R_MAXTYPE)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type")
    if(ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")

    /* Get the object information */
    if(H5R_get_obj_type(loc.oloc->file, H5AC_ind_dxpl_id, ref_type, ref, obj_type) < 0)
	HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable to determine object type")

done:
    FUNC_LEAVE_API(ret_value)
}   /* end H5Rget_obj_type2() */


/*--------------------------------------------------------------------------
 NAME
    H5R_get_name
 PURPOSE
    Internal routine to determine a name for the object referenced
 USAGE
    ssize_t H5R_get_name(f, dxpl_id, ref_type, ref, name, size)
        H5F_t *f;       IN: Pointer to the file that the reference is pointing
                            into
        hid_t lapl_id;  IN: LAPL to use for operation
        hid_t dxpl_id;  IN: DXPL to use for operation
        hid_t id;       IN: Location ID given for reference
        H5R_type_t ref_type;    IN: Type of reference
        void *ref;      IN: Reference to query.
        char *name;     OUT: Buffer to place name of object referenced
        size_t size;    IN: Size of name buffer

 RETURNS
    Non-negative length of the path on success, Negative on failure
 DESCRIPTION
    Given a reference to some object, determine a path to the object
    referenced in the file.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    This may not be the only path to that object.
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static ssize_t
H5R_get_name(H5F_t *f, hid_t lapl_id, hid_t dxpl_id, hid_t id, H5R_type_t ref_type,
    const void *_ref, char *name, size_t size)
{
    hid_t file_id = H5I_INVALID_HID;    /* ID for file that the reference is in */
    H5O_loc_t oloc;             /* Object location describing object for reference */
    ssize_t ret_value = -1;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check args */
    HDassert(f);
    HDassert(_ref);

    /* Initialize the object location */
    H5O_loc_reset(&oloc);
    oloc.file = f;

    /* Get address for reference */
    switch(ref_type) {
        case H5R_OBJECT:
            oloc.addr = *(const hobj_ref_t *)_ref;
            break;

        case H5R_DATASET_REGION:
        {
            H5HG_t hobjid;  /* Heap object ID */
            uint8_t *buf;   /* Buffer to store serialized selection in */
            const uint8_t *p;           /* Pointer to OID to store */

            /* Get the heap ID for the dataset region */
            p = (const uint8_t *)_ref;
            H5F_addr_decode(oloc.file, &p, &(hobjid.addr));
            UINT32DECODE(p, hobjid.idx);

            /* Get the dataset region from the heap (allocate inside routine) */
            if((buf = (uint8_t *)H5HG_read(oloc.file, dxpl_id, &hobjid, NULL, NULL)) == NULL)
                HGOTO_ERROR(H5E_REFERENCE, H5E_READERROR, FAIL, "Unable to read dataset region information")

            /* Get the object oid for the dataset */
            p = buf;
            H5F_addr_decode(oloc.file, &p, &(oloc.addr));

            /* Free the buffer allocated in H5HG_read() */
            H5MM_xfree(buf);
        } /* end case */
        break;
        case H5R_ATTR:
        {
            /* TODO */
        }
        break;
        case H5R_BADTYPE:
        case H5R_MAXTYPE:
        default:
            HDassert("unknown reference type" && 0);
            HGOTO_ERROR(H5E_REFERENCE, H5E_UNSUPPORTED, FAIL, "internal error (unknown reference type)")
    } /* end switch */

    /* Retrieve file ID for name search */
    if((file_id = H5I_get_file_id(id, FALSE)) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTGET, FAIL, "can't retrieve file ID")

    /* Get name, length, etc. */
    if((ret_value = H5G_get_name_by_addr(file_id, lapl_id, dxpl_id, &oloc, name, size)) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTGET, FAIL, "can't determine name")

done:
    /* Close file ID used for search */
    if(file_id > 0 && H5I_dec_ref(file_id) < 0)
        HDONE_ERROR(H5E_REFERENCE, H5E_CANTDEC, FAIL, "can't decrement ref count of temp ID")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5R_get_name() */


/*--------------------------------------------------------------------------
 NAME
    H5Rget_name
 PURPOSE
    Determines a name for the object referenced
 USAGE
    ssize_t H5Rget_name(loc_id, ref_type, ref, name, size)
        hid_t loc_id;   IN: Dataset reference object is in or location ID of
                            object that the dataset is located within.
        H5R_type_t ref_type;    IN: Type of reference
        void *ref;      IN: Reference to query.
        char *name;     OUT: Buffer to place name of object referenced. If NULL
	                     then this call will return the size in bytes of name.
        size_t size;    IN: Size of name buffer (user needs to include NULL terminator
                            when passing in the size)

 RETURNS
    Non-negative length of the path on success, Negative on failure
 DESCRIPTION
    Given a reference to some object, determine a path to the object
    referenced in the file.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    This may not be the only path to that object.
 EXAMPLES
 REVISION LOG
    M. Scot Breitenfeld
    22 January 2014
    Changed the behavior for the returned value of the function when name is NULL.
    If name is NULL then size is ignored and the function returns the size 
    of the name buffer (not including the NULL terminator), it still returns
    negative on failure.
--------------------------------------------------------------------------*/
ssize_t
H5Rget_name(hid_t id, H5R_type_t ref_type, const void *_ref, char *name,
    size_t size)
{
    H5G_loc_t loc;      /* Group location */
    H5F_t *file;        /* File object */
    ssize_t ret_value;  /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("Zs", "iRt*x*sz", id, ref_type, _ref, name, size);

    /* Check args */
    if(H5G_loc(id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(ref_type <= H5R_BADTYPE || ref_type >= H5R_MAXTYPE)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference type")
    if(_ref == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid reference pointer")

    /* Get the file pointer from the entry */
    file = loc.oloc->file;

    /* Get name */
    if((ret_value = H5R_get_name(file, H5P_DEFAULT, H5AC_ind_dxpl_id, id, ref_type, _ref, name, size)) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTINIT, FAIL, "unable to determine object path")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Rget_name() */

