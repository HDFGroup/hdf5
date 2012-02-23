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
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 * Purpose:     The null file driver is a non-functional stackable driver. All
 *              calls are delegated to the underlying driver; it provides
 *              absolutely no new functionality. The main purpose of this driver
 *              is to serve as a template for future stackable drivers. In
 *              particular, other transparent drivers (those that add a feature
 *              that does not affect file structure, like external logging or 
 *              caching) will be easy to build on top of this driver.
 */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC  H5FD_null_init_interface


#include "H5private.h"      /* Generic Functions    */
#include "H5Eprivate.h"     /* Error handling       */
#include "H5Fprivate.h"     /* File access          */
#include "H5FDprivate.h"    /* File drivers         */
#include "H5FDnull.h"       /* Null file driver     */
#include "H5Iprivate.h"     /* IDs                  */
#include "H5MMprivate.h"    /* Memory management    */
#include "H5Pprivate.h"     /* Property lists       */

/* The driver identification number, initialized at runtime */
static hid_t H5FD_NULL_g = 0;

/* The description of a file belonging to this driver. */
typedef struct H5FD_null_t {
    H5FD_t  pub;            /* public stuff, must be first      */
    hid_t   inner_fapl_id;  /* FAPL of the underlying driver     */
    H5FD_t  *inner_file;    /* Pointer to the underlying file   */
} H5FD_null_t;

/* Driver-specific file access properties */
typedef struct H5FD_null_fapl_t {
    hid_t   inner_fapl_id;  /* FAPL of the underlying driver */
} H5FD_null_fapl_t;

/* Driver specific data transfer properties */
typedef struct H5FD_null_dxpl_t {
    hid_t   inner_dxpl_id;  /*data xfer property list of the inner file */
} H5FD_null_dxpl_t;

/* Callback prototypes */
static herr_t H5FD_null_term(void);
static void *H5FD_null_fapl_get(H5FD_t *_file);
static void *H5FD_null_fapl_copy(const void *_old_fa);
static herr_t H5FD_null_fapl_free(void *_fa);
static void *H5FD_null_dxpl_copy(const void *_old_dx);
static herr_t H5FD_null_dxpl_free(void *_dx);
static hsize_t H5FD_null_sb_size(H5FD_t *_file);
static herr_t H5FD_null_sb_encode(H5FD_t *_file, char *name/*out*/,
                unsigned char *buf/*out*/);
static herr_t H5FD_null_sb_decode(H5FD_t *_file, const char *name,
                const unsigned char *buf);
static herr_t H5FD_null_sb_verify(H5FD_t *_file, const char *driver_id);
static H5FD_t *H5FD_null_open(const char *name, unsigned flags,
                hid_t fapl_id, haddr_t maxaddr);
static herr_t H5FD_null_close(H5FD_t *_file);
static int H5FD_null_cmp(const H5FD_t *_f1, const H5FD_t *_f2);
static herr_t H5FD_null_query(const H5FD_t *_f1, unsigned long *flags);
static herr_t H5FD_null_get_type_map(const H5FD_t *_file, H5FD_mem_t *type_map);
static haddr_t H5FD_null_alloc(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, 
                hsize_t size);
static herr_t H5FD_null_free(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, 
                hsize_t size);
static haddr_t H5FD_null_get_eoa(const H5FD_t *_file, H5FD_mem_t type);
static herr_t H5FD_null_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t eoa);
static haddr_t H5FD_null_get_eof(const H5FD_t *_file);
static herr_t  H5FD_null_get_handle(H5FD_t *_file, hid_t fapl, void** file_handle);
static herr_t H5FD_null_read(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr,
                size_t size, void *_buf/*out*/);
static herr_t H5FD_null_write(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr,
                size_t size, const void *_buf);
static herr_t H5FD_null_flush(H5FD_t *_file, hid_t dxpl_id, unsigned closing);
static herr_t H5FD_null_truncate(H5FD_t *_file, hid_t dxpl_id, unsigned closing);

/* The class struct */
static const H5FD_class_t H5FD_null_g = {
    "null",                     /* name         */
    HADDR_MAX,                  /* maxaddr      */
    H5F_CLOSE_WEAK,             /* fc_degree    */
    H5FD_null_term,             /* terminate    */
    H5FD_null_sb_size,          /* sb_size      */
    H5FD_null_sb_encode,        /* sb_encode    */
    H5FD_null_sb_decode,        /* sb_decode    */
    H5FD_null_sb_verify,        /* sb_verify    */
    sizeof(H5FD_null_fapl_t),   /* fapl_size    */
    H5FD_null_fapl_get,         /* fapl_get     */
    H5FD_null_fapl_copy,        /* fapl_copy    */
    H5FD_null_fapl_free,        /* fapl_free    */
    sizeof(H5FD_null_dxpl_t),   /* dxpl_size    */
    H5FD_null_dxpl_copy,        /* dxpl_copy    */
    H5FD_null_dxpl_free,        /* dxpl_free    */
    H5FD_null_open,             /* open         */
    H5FD_null_close,            /* close        */
    H5FD_null_cmp,              /* cmp          */
    H5FD_null_query,            /* query        */
    H5FD_null_get_type_map,     /* get_type_map */
    H5FD_null_alloc,            /* alloc        */
    H5FD_null_free,             /* free         */
    H5FD_null_get_eoa,          /* get_eoa      */
    H5FD_null_set_eoa,          /* set_eoa      */
    H5FD_null_get_eof,          /* get_eof      */
    H5FD_null_get_handle,       /* get_handle   */
    H5FD_null_read,             /* read         */
    H5FD_null_write,            /* write        */
    H5FD_null_flush,            /* flush        */
    H5FD_null_truncate,         /* truncate     */
    NULL,                       /* lock         */
    NULL,                       /* unlock       */
    H5FD_FLMAP_SINGLE           /* fl_map       */
};


/*-------------------------------------------------------------------------
 * Function:    H5FD_null_init_interface
 *
 * Purpose:     Initialize interface-specific information
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_null_init_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_null_init_interface)

    FUNC_LEAVE_NOAPI(H5FD_null_init())
} /* H5FD_null_init_interface() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_null_init
 *
 * Purpose:     Initialize this driver by registering the driver with the
 *              library.
 *
 * Return:      Success:    The driver ID for the null driver.
 *              Failure:    Negative
 *
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5FD_null_init(void)
{
    hid_t ret_value=H5FD_NULL_g;   /* Return value */

    FUNC_ENTER_NOAPI(H5FD_null_init, FAIL)

    if (H5I_VFL!=H5Iget_type(H5FD_NULL_g))
        H5FD_NULL_g = H5FD_register(&H5FD_null_g,sizeof(H5FD_class_t),FALSE);

    /* Set return value */
    ret_value=H5FD_NULL_g;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD_null_init */


/*---------------------------------------------------------------------------
 * Function:    H5FD_null_term
 *
 * Purpose:     Shut down the VFD
 *
 * Returns:     Success: Non-negative
 *              Failure: Negative
 *
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 *---------------------------------------------------------------------------
 */
static herr_t
H5FD_null_term(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_null_term)

    /* Reset VFL ID */
    H5FD_NULL_g=0;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD_null_term() */


/*-------------------------------------------------------------------------
 * Function:    H5Pset_fapl_null
 *
 * Purpose:     Sets the file access property list FAPL_ID to use the null
 *              driver. The inner_fapl_id is the FAPL of the underlying 
 *              property list. 
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_null(hid_t fapl_id, hid_t inner_fapl_id)
{
    herr_t ret_value;
    H5FD_null_fapl_t fa={0};
    H5P_genplist_t *plist;      /* Property list pointer */

    FUNC_ENTER_API(H5Pset_fapl_null, FAIL)
    H5TRACE2("e", "ii", fapl_id, inner_fapl_id);

    /* Check arguments */
    if(TRUE != H5P_isa_class(fapl_id, H5P_FILE_ACCESS))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")
    
    if(H5P_DEFAULT == inner_fapl_id)
        inner_fapl_id = H5P_FILE_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(inner_fapl_id, H5P_FILE_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access list")

    /*
     * Initialize driver specific information.
     */
    fa.inner_fapl_id = inner_fapl_id;

    if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")
    ret_value= H5P_set_driver(plist, H5FD_NULL, &fa);

done:
    FUNC_LEAVE_API(ret_value)
} /*H5Pset_fapl_null*/


/*-------------------------------------------------------------------------
 * Function:    H5Pget_fapl_null
 *
 * Purpose:     Returns information about the null file access property
 *              list though the function arguments. The inner_fapl_id is a
 *              pointer to the FAPL of the underlying driver.
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_fapl_null(hid_t fapl_id, hid_t *inner_fapl_id/*out*/)
{
    H5FD_null_fapl_t *fa;
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_API(H5Pget_fapl_null, FAIL)
    H5TRACE2("e", "ix", fapl_id, inner_fapl_id);

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(fapl_id,H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access list")
    if(H5FD_NULL != H5P_get_driver(plist))
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "incorrect VFL driver")
    if(NULL == (fa = (H5FD_null_fapl_t *)H5P_get_driver_info(plist)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "bad VFL driver info")

    if(inner_fapl_id) {
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(fa->inner_fapl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access list")
        *inner_fapl_id = H5P_copy_plist(plist, TRUE);
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /*H5Pget_fapl_null*/


/*-------------------------------------------------------------------------
 * Function:    H5FD_null_fapl_get
 *
 * Purpose:     Gets a file access property list which could be used to
 *              create an identical file.
 *
 * Return:      Success:    Ptr to new file access property list.
 *              Failure:    NULL
 *
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5FD_null_fapl_get(H5FD_t *_file)
{
    H5FD_null_t *file = (H5FD_null_t*)_file;
    H5FD_null_fapl_t    *fa = NULL;
    H5P_genplist_t *plist; /* Property list pointer */
    void *ret_value;       /* Return value */

    FUNC_ENTER_NOAPI(H5FD_null_fapl_get, NULL)

    /* Check agruments */
    if(NULL == (fa = (H5FD_null_fapl_t *)H5MM_calloc(sizeof(H5FD_null_fapl_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    if(NULL == (plist = (H5P_genplist_t *)H5I_object(file->inner_fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
    fa->inner_fapl_id = H5P_copy_plist(plist, FALSE);

    /* Set return value */
    ret_value=fa;

done:
    if(ret_value==NULL) {
        if(fa!=NULL)
            H5MM_xfree(fa);
    } /* end if */
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD_null_fapl_get */


/*-------------------------------------------------------------------------
 * Function:    H5FD_null_fapl_copy
 *
 * Purpose:     Copies the null-specific file access properties.
 *
 * Return:      Success:    Ptr to a new property list
 *              Failure:    NULL
 *
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5FD_null_fapl_copy(const void *_old_fa)
{
    const H5FD_null_fapl_t *old_fa = (const H5FD_null_fapl_t*)_old_fa;
    H5FD_null_fapl_t *new_fa = NULL;
    H5P_genplist_t *plist; /* Property list pointer */
    void *ret_value;       /* Return value */

    FUNC_ENTER_NOAPI(H5FD_null_fapl_copy, NULL)

    /* Allocate memory for copy */
    if(NULL == (new_fa = (H5FD_null_fapl_t *)H5MM_malloc(sizeof(H5FD_null_fapl_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Copy the fields of the structure */
    memcpy(new_fa, old_fa, sizeof(H5FD_null_fapl_t));

    /* Deep copy the property list objects in the structure */
    if(old_fa->inner_fapl_id==H5P_FILE_ACCESS_DEFAULT) {
        if(H5I_inc_ref(new_fa->inner_fapl_id, FALSE)<0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTINC, NULL, "unable to increment ref count on VFL driver")
    } /* end if */
    else {
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(old_fa->inner_fapl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
        new_fa->inner_fapl_id = H5P_copy_plist(plist, FALSE);
    } /* end else */

    /* Set return value */
    ret_value=new_fa;

done:
    if(ret_value==NULL) {
        if(new_fa!=NULL)
            H5MM_xfree(new_fa);
    } /* end if */
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD_null_fapl_copy */


/*-------------------------------------------------------------------------
 * Function:    H5FD_null_fapl_free
 *
 * Purpose:     Frees the null-specific file access properties.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_null_fapl_free(void *_fa)
{
    H5FD_null_fapl_t *fa = (H5FD_null_fapl_t*)_fa;
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5FD_null_fapl_free, FAIL)

    /* Free the iner FAPL */
    if(H5I_dec_ref(fa->inner_fapl_id) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTDEC, FAIL, "can't close driver ID")
    H5MM_xfree(fa);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD_null_fapl_free */


/*-------------------------------------------------------------------------
 * Function:    H5FD_null_dxpl_copy
 *
 * Purpose:     Copies the null-specific data transfer properties.
 *
 * Return:      Success:    Pointertr to the new property list
 *              Failure:    NULL
 *
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5FD_null_dxpl_copy(const void *_old_dx)
{
    const H5FD_null_dxpl_t *old_dx = (const H5FD_null_dxpl_t*)_old_dx;
    H5FD_null_dxpl_t *new_dx = NULL;
    H5P_genplist_t *plist;  /* Property list pointer */
    void *ret_value;        /* Return value */

    FUNC_ENTER_NOAPI(H5FD_null_dxpl_copy, NULL)

    /* Allocate memoryfor copy */
    if(NULL == (new_dx = (H5FD_null_dxpl_t *)H5MM_malloc(sizeof(H5FD_null_dxpl_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Copy the DXPL */
    HDmemcpy(new_dx, old_dx, sizeof(H5FD_null_dxpl_t));

    if(old_dx->inner_dxpl_id == H5P_DATASET_XFER_DEFAULT) {
        if(H5I_inc_ref(new_dx->inner_dxpl_id, FALSE)<0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTINC, NULL, "unable to increment ref count on VFL driver")
    } /* end if */
    else {
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(old_dx->inner_dxpl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
        new_dx->inner_dxpl_id = H5P_copy_plist(plist, FALSE);
    } /* end else */

    /* Set return value */
    ret_value=new_dx;

done:
    if(ret_value==NULL) {
        if(new_dx!=NULL)
            H5MM_xfree(new_dx);
    } /* end if */
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD_null_dxpl_copy */


/*-------------------------------------------------------------------------
 * Function:    H5FD_null_dxpl_free
 *
 * Purpose:     Frees the null-specific data transfer properties.
 *
 * Return:      Success:    0
 *              Failure:    -1
 
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_null_dxpl_free(void *_dx)
{
    H5FD_null_dxpl_t  *dx = (H5FD_null_dxpl_t*)_dx;
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5FD_null_dxpl_free, FAIL)

    /* Free the DXPL */
    if(H5I_dec_ref(dx->inner_dxpl_id) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTDEC, FAIL, "can't close driver ID")
    H5MM_xfree(dx);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD_null_dxpl_free */


/*-------------------------------------------------------------------------
 * Function:    H5FD_null_sb_size
 *
 * Purpose:     Returns the size of the private information to be stored
 *              in the superblock. This function is delegated to the 
 *              inner driver and the result is returned without
 *              modification.
 *
 * Return:      Success:    The super block driver data size.
 *              Failure:    0 if an error occurs or if the driver has no
 *                          data to store in the superblock.
 *
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hsize_t
H5FD_null_sb_size(H5FD_t *_file)
{
    H5FD_null_t *file = (H5FD_null_t*)_file;
    hsize_t     ret_value = 0; /*size of header*/

    FUNC_ENTER_NOAPI(H5FD_null_sb_size, UFAIL)

    HDassert(file);
    
    /* Delegate to inner file */
    ret_value = H5FD_sb_size(file->inner_file);
    
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD_null_sb_size */


/*-------------------------------------------------------------------------
 * Function:    H5FD_null_sb_encode
 *
 * Purpose:     Encode driver information for the superblock. This
 *              is delegated to the inner driver.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_null_sb_encode(H5FD_t *_file, char *name/*out*/, unsigned char *buf/*out*/)
{
    H5FD_null_t *file = (H5FD_null_t*)_file;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5FD_null_sb_encode, FAIL)

    HDassert(file);

    /* Delegate to inner file */
    if(H5FD_sb_encode(file->inner_file, name, buf) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "inner driver sb_encode failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD_null_sb_encode() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_null_sb_decode
 *
 * Purpose:     Decode the superblock information for this driver. This
 *              is delegated to the inner driver.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_null_sb_decode(H5FD_t *_file, const char *name, const unsigned char *buf)
{
    H5FD_null_t *file = (H5FD_null_t*)_file;
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5FD_null_sb_decode, FAIL)
    
    HDassert(file);
    
    /* Delegate to the inner driver */
    if(H5FD_sb_decode(file->inner_file, name, buf) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "inner driver sb_decode failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD_null_sb_decode() */



/*-------------------------------------------------------------------------
 * Function:    H5FD_null_sb_verify
 *
 * Purpose:     Verify that the inner driver is compatable with the driver
 *              that created the file. driver_id is the driver identifier
 *              field stored in the superblock. This is called when
 *              reopening a file and ensures that the driver is able to
 *              decode the superblock info.
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 * Programmer:  Jacob Gruber
 *              Friday, January 13, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_null_sb_verify(H5FD_t *_file, const char *driver_id)
{
    H5FD_null_t *file = (H5FD_null_t*)_file;
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5FD_null_sb_verify, FAIL)
    
    HDassert(file);
    
    /* Delegate to the inner driver */
    if(H5FD_sb_verify(file->inner_file, driver_id) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "inner driver sb_verify failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}



/*-------------------------------------------------------------------------
 * Function:    H5FD_null_open
 *
 * Purpose: Creates and/or opens a file with the null driver.
 *
 * Return:  Success: A pointer to a new file data structure. The
 *              public fields will be initialized by the
 *              caller, which is always H5FD_open().
 *          Failure: NULL
 *
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5FD_t *
H5FD_null_open(const char *name, unsigned flags, hid_t fapl_id,
            haddr_t maxaddr)
{
    H5FD_null_t *file=NULL;
    H5FD_t      *ret_value=NULL;

    FUNC_ENTER_NOAPI(H5FD_null_open, NULL)

    /* Check arguments */
    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid file name")
    if(0 == maxaddr || HADDR_UNDEF == maxaddr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, NULL, "bogus maxaddr")

    /* Initialize file from file access properties */
    if(NULL == (file = (H5FD_null_t *)H5MM_calloc(sizeof(H5FD_null_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "unable to allocate file struct")
    if(H5P_FILE_ACCESS_DEFAULT==fapl_id) {
        file->inner_fapl_id = H5P_FILE_ACCESS_DEFAULT;
        if(H5I_inc_ref(file->inner_fapl_id, FALSE) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTINC, NULL, "unable to increment ref count on VFL driver")
    } /* end if */
    else {
        H5P_genplist_t      *plist;      /* Property list pointer */
        H5FD_null_fapl_t *fa;

        /* Get FAPL */
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
        fa = (H5FD_null_fapl_t *)H5P_get_driver_info(plist);
        HDassert(fa);

        /* Get inner FAPL */
        if(fa->inner_fapl_id==H5P_FILE_ACCESS_DEFAULT) {
            if(H5I_inc_ref(fa->inner_fapl_id, FALSE)<0)
                HGOTO_ERROR(H5E_VFL, H5E_CANTINC, NULL, "unable to increment ref count on VFL driver")
            file->inner_fapl_id = fa->inner_fapl_id;
        } /* end if */
        else {
            if(NULL == (plist = (H5P_genplist_t *)H5I_object(fa->inner_fapl_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
            file->inner_fapl_id = H5P_copy_plist(plist, FALSE);
        } /* end else */
    } /* end else */

    /* Open the inner file */
    file->inner_file = H5FD_open(name, flags, file->inner_fapl_id, HADDR_UNDEF);    
    if(NULL == file->inner_file)
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open inner file")
    ret_value=(H5FD_t *)file;

done:
    /* Cleanup and fail */
    if(ret_value == NULL && file != NULL) {

        if(file->inner_file) {
            if (H5FD_close(file->inner_file) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, NULL, "unable to close inner file")
         }
          
        if(H5I_dec_ref(file->inner_fapl_id) < 0)
            HDONE_ERROR(H5E_VFL, H5E_CANTDEC, NULL, "can't close driver ID")
        H5MM_xfree(file);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD_null_open() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_null_close
 *
 * Purpose:     Closes a file opened through the null driver
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_null_close(H5FD_t *_file)
{
    H5FD_null_t *file = (H5FD_null_t*)_file;
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5FD_null_close, FAIL)

    /* Close the inner file */
    if (H5FD_close(file->inner_file) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "unable to close inner file")
    
    /* Decrease the reference count on the inner FAPL */
    if(H5I_dec_ref(file->inner_fapl_id) < 0)
        HDONE_ERROR(H5E_VFL, H5E_CANTDEC, FAIL, "can't close driver ID")

    /* Free the file */
    H5MM_xfree(file);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD_null_close() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_null_cmp
 *
 * Purpose:     Compares two null driver files. Delegated to the inner
 *              driver.
 *
 * Return:      Success:    Like strcmp()
 *              Failure:    Never fails (arguments were checked by the caller).
 *
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
H5FD_null_cmp(const H5FD_t *_f1, const H5FD_t *_f2)
{
    const H5FD_null_t   *f1 = (const H5FD_null_t*)_f1;
    const H5FD_null_t   *f2 = (const H5FD_null_t*)_f2;
    int ret_value = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_null_cmp)
    
    HDassert(f1);
    HDassert(f2);

    /* Delegate to the inner driver */
    ret_value = H5FDcmp(f1->inner_file, f2->inner_file);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD_null_cmp() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_null_query
 *
 * Purpose:     Set the flags that this VFL driver is capable of supporting
 *              (listed in H5FDpublic.h). Delegated to the inner driver.  
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_null_query(const H5FD_t * _file, unsigned long *flags /* out */)
{
    const H5FD_null_t *file = (const H5FD_null_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_null_query)

    /* Query the inner driver */
    if(flags) {
        HDassert(file);
        H5FD_query(file->inner_file, flags);
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD_null_query() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_null_get_type_map
 *
 * Purpose:     Get the type mapping of the underlying driver.
 *              Only used for the multi driver.
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_null_get_type_map(const H5FD_t *_file, H5FD_mem_t *type_map)
{
    const H5FD_null_t *file = (const H5FD_null_t*)_file;
    herr_t ret_value;

    FUNC_ENTER_NOAPI(H5FD_null_get_type_map, FAIL)

    HDassert(file);
    
    /* Delegate to the underlying driver */
    ret_value = H5FD_get_fs_type_map(file->inner_file, type_map);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:    H5FD_null_alloc
 *
 * Purpose:     Allocate file memory in the inner driver.
 *              
 * Return:  Success:    Non-negative
 *          Failure:    Negative
 *
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_null_alloc(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, hsize_t size)
{
    const H5FD_null_t *file = (const H5FD_null_t*)_file;
    hid_t inner_dxpl_id = H5P_DATASET_XFER_DEFAULT;
    herr_t ret_value;

    FUNC_ENTER_NOAPI(H5FD_null_alloc, FAIL)

    /*
     * Get the inner data transfer property list.
     */
    if(H5P_DATASET_XFER_DEFAULT != dxpl_id) {
        H5P_genplist_t *plist;      /* Property list pointer */
        H5FD_null_dxpl_t *dx;       /* Null-specific info */
        
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data xfer property list")
        
        dx = (H5FD_null_dxpl_t *)H5P_get_driver_info(plist);

        HDassert(TRUE == H5P_isa_class(dxpl_id, H5P_DATASET_XFER));
        HDassert(dx);
        inner_dxpl_id = dx->inner_dxpl_id;
    } /* end if */

    HDassert(file);

    /* Delegate to the inner file */
    ret_value = H5FDalloc(file->inner_file, type, inner_dxpl_id, size);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:    H5FD_null_free
 *
 * Purpose:     Free file memory in the inner file.
 *              
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_null_free(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, hsize_t size)
{
    const H5FD_null_t *file = (const H5FD_null_t*)_file;
    hid_t inner_dxpl_id = H5P_DATASET_XFER_DEFAULT;
    herr_t ret_value;

    FUNC_ENTER_NOAPI(H5FD_null_free, FAIL)
   
    /*
     * Get the inner data transfer property list.
     */
    if(H5P_DATASET_XFER_DEFAULT != dxpl_id) {
        H5P_genplist_t *plist;      /* Property list pointer */
        H5FD_null_dxpl_t *dx;       /* Null-specific info */
        
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data xfer property list")
        
        dx = (H5FD_null_dxpl_t *)H5P_get_driver_info(plist);

        HDassert(TRUE == H5P_isa_class(dxpl_id, H5P_DATASET_XFER));
        HDassert(dx);
        inner_dxpl_id = dx->inner_dxpl_id;
    } /* end if */

    HDassert(file);

    /* Delegate to the inner file */
    ret_value = H5FDfree(file->inner_file, type, inner_dxpl_id, addr, size);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:    H5FD_null_get_eoa
 *
 * Purpose:     Returns the end-of-address marker for the inner file. The
 *              marker is the first address past the last byte allocated in
 *              the format address space.
 *
 * Return:      Success:    The end-of-address-marker
 *              Failure:    HADDR_UNDEF
 *
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_null_get_eoa(const H5FD_t *_file, H5FD_mem_t type)
{
    const H5FD_null_t   *file = (const H5FD_null_t*)_file;
    haddr_t ret_value;   /* Return value */

    FUNC_ENTER_NOAPI(H5FD_null_get_eoa, HADDR_UNDEF)
   
    HDassert(file);
    
    /* Delegate to the inner file */
    ret_value = H5FD_get_eoa(file->inner_file, type);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:    H5FD_null_set_eoa
 *
 * Purpose:     Set the end-of-address marker for the inner file.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_null_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t abs_eoa)
{
    H5FD_null_t *file = (H5FD_null_t*)_file;
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5FD_null_set_eoa, FAIL)

    HDassert(file);

    /* Delegate to the inner file */
    ret_value = H5FD_set_eoa(file->inner_file, type, abs_eoa);
    
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD_null_set_eof */


/*-------------------------------------------------------------------------
 * Function:    H5FD_null_get_eof
 *
 * Purpose:     Returns the end-of-file marker in the inner file.
 *
 * Return:      Success:    End of file address.
 *              Failure:    HADDR_UNDEF
 *
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_null_get_eof(const H5FD_t *_file)
{
    const H5FD_null_t *file = (const H5FD_null_t*)_file;
    haddr_t ret_value;   /* Return value */

    FUNC_ENTER_NOAPI(H5FD_null_get_eof, HADDR_UNDEF)

    HDassert(file);

    /* Delegate to the inner file */
    ret_value = H5FD_get_eof(file->inner_file);
   
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD_null_get_eof */


/*-------------------------------------------------------------------------
 * Function:    H5FD_null_get_handle
 *
 * Purpose:     Returns the file handle of inner file driver.
 *
 * Returns:     Non-negative if succeed or negative if fails.
 *
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_null_get_handle(H5FD_t *_file, hid_t UNUSED fapl, void** file_handle)
{
    H5FD_null_t       *file = (H5FD_null_t *)_file;
    herr_t            ret_value;

    FUNC_ENTER_NOAPI(H5FD_null_get_handle, FAIL)

    HDassert(file);
    
    /* Delegate to the inner file */
    ret_value = H5FD_get_vfd_handle(file->inner_file, file->inner_fapl_id, file_handle);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:    H5FD_null_read
 *
 * Purpose:     Reads from the inner file.
 *
 * Return:      Success:    Zero. Result is stored in caller-supplied buffer.
 *              Failure:    -1, contents of buffer BUF are undefined.
 *
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_null_read(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, size_t size, 
            void *_buf/*out*/)
{
    H5FD_null_t *file = (H5FD_null_t*)_file;
    unsigned char *buf = (unsigned char*)_buf;
    hid_t inner_dxpl_id = H5P_DATASET_XFER_DEFAULT;
    herr_t              ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5FD_null_read, FAIL)

    /*
     * Get the inner data transfer property list.
     */
    if(H5P_DATASET_XFER_DEFAULT != dxpl_id) {
        H5P_genplist_t *plist;      /* Property list pointer */
        H5FD_null_dxpl_t *dx;       /* Null-specific info */
        
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data xfer property list")
        
        dx = (H5FD_null_dxpl_t *)H5P_get_driver_info(plist);

        HDassert(TRUE == H5P_isa_class(dxpl_id, H5P_DATASET_XFER));
        HDassert(dx);
        inner_dxpl_id = dx->inner_dxpl_id;
    } /* end if */
    
    /* Read from inner file */
    if (H5FDread(file->inner_file, type, inner_dxpl_id, addr, size, buf)<0)
            HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "inner file read failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD_null_read */


/*-------------------------------------------------------------------------
 * Function:    H5FD_null_write
 *
 * Purpose:     Writes to the inner file.
 *
 * Return:      Success:    Zero
 *              Failure:    -1
 *
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_null_write(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, size_t size,
            const void *_buf)
{
    H5FD_null_t *file = (H5FD_null_t*)_file;
    const unsigned char *buf = (const unsigned char*)_buf;
    hid_t inner_dxpl_id = H5P_DATASET_XFER_DEFAULT;
    herr_t ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5FD_null_write, FAIL)

    /*
     * Get the inner data transfer property list.
     */
    if(H5P_DATASET_XFER_DEFAULT != dxpl_id) {
        H5P_genplist_t *plist;      /* Property list pointer */
        H5FD_null_dxpl_t *dx;       /* Null-specific info */
        
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data xfer property list")
        
        dx = (H5FD_null_dxpl_t *)H5P_get_driver_info(plist);

        HDassert(TRUE == H5P_isa_class(dxpl_id, H5P_DATASET_XFER));
        HDassert(dx);
        inner_dxpl_id = dx->inner_dxpl_id;
    } /* end if */

    /* Delegate write to the inner file */
    HDassert(file);
    if (H5FDwrite(file->inner_file, type, inner_dxpl_id, addr, size, buf)<0)
        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "member file write failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD_null_write */


/*-------------------------------------------------------------------------
 * Function:    H5FD_null_flush
 *
 * Purpose:     Flushes the inner file.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_null_flush(H5FD_t *_file, hid_t dxpl_id, unsigned closing)
{
    H5FD_null_t   *file = (H5FD_null_t*)_file;
    hid_t inner_dxpl_id = H5P_DATASET_XFER_DEFAULT;
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5FD_null_flush, FAIL)

    /*
     * Get the inner data transfer property list.
     */
    if(H5P_DATASET_XFER_DEFAULT != dxpl_id) {
        H5P_genplist_t *plist;      /* Property list pointer */
        H5FD_null_dxpl_t *dx;       /* Null driver property list */
        
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data xfer property list")
        
        dx = (H5FD_null_dxpl_t *)H5P_get_driver_info(plist);

        HDassert(TRUE == H5P_isa_class(dxpl_id, H5P_DATASET_XFER));
        HDassert(dx);
        inner_dxpl_id = dx->inner_dxpl_id;
    } /* end if */
    
    /* Delegate to the inner driver */
    HDassert(file);
    if (H5FD_flush(file->inner_file, inner_dxpl_id, closing) < 0)
        HGOTO_ERROR(H5E_IO, H5E_BADVALUE, FAIL, "unable to flush inner file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_null_flush() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_null_truncate
 *
 * Purpose:     Truncates the inner file.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 * Programmer:  Jacob Gruber
 *              Thursday, January 5, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_null_truncate(H5FD_t *_file, hid_t dxpl_id, unsigned closing)
{
    H5FD_null_t     *file = (H5FD_null_t*)_file;
    hid_t inner_dxpl_id = H5P_DATASET_XFER_DEFAULT;
    herr_t          ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5FD_null_truncate, FAIL)

    /*
     * Get the inner data transfer property list.
     */
    if(H5P_DATASET_XFER_DEFAULT != dxpl_id) {
        H5P_genplist_t *plist;      /* Property list pointer */
        H5FD_null_dxpl_t *dx;       /* Null driver property list */
        
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data xfer property list")
        
        dx = (H5FD_null_dxpl_t *)H5P_get_driver_info(plist);

        HDassert(TRUE == H5P_isa_class(dxpl_id, H5P_DATASET_XFER));
        HDassert(dx);
        inner_dxpl_id = dx->inner_dxpl_id;
    } /* end if */

    /* Delegate to the underlying driver */
    HDassert(file);
    if(H5FD_truncate(file->inner_file, inner_dxpl_id, closing) < 0)
        HGOTO_ERROR(H5E_IO, H5E_BADVALUE, FAIL, "unable to truncate inner file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_null_truncate() */

