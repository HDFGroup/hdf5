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
 * Programmer:	Robb Matzke <matzke@llnl.gov>
 *		Monday, November 10, 1997
 *
 * Purpose:	Implements a family of files that acts as a single hdf5
 *		file.  The purpose is to be able to split a huge file on a
 *		64-bit platform, transfer all the <2GB members to a 32-bit
 *		platform, and then access the entire huge file on the 32-bit
 *		platform.
 *
 *		All family members are logically the same size although their
 *		physical sizes may vary.  The logical member size is
 *		determined by looking at the physical size of the first member
 *		when the file is opened.  When creating a file family, the
 *		first member is created with a predefined physical size
 *		(actually, this happens when the file family is flushed, and
 *		can be quite time consuming on file systems that don't
 *		implement holes, like nfs).
 *
 */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5FD_family_init_interface


#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"		/* File access				*/
#include "H5FDprivate.h"	/* File drivers				*/
#include "H5FDfamily.h"         /* Family file driver 			*/
#include "H5FLprivate.h"        /* Free Lists                           */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"		/* Property lists			*/

#undef MAX
#define MAX(X,Y)	((X)>(Y)?(X):(Y))
#undef MIN
#define MIN(X,Y)	((X)<(Y)?(X):(Y))



/* Type definitions of the data structure used to store the aio control 
 * block(s) returned by any underlying file driver(s) that support AIO, 
 * along with the information needed to route the control block to the 
 * appropriate driver when a family aio control block is passed down to 
 * the family driver.
 *
 * The magic field of H5FD_family_aio_ctlblk_t must always be set to 
 * H5FD_FAMILY_AIO_CTLBLK_T__MAGIC. That of H5FD_family_aio_subctlblk_t
 * must be set to H5FD_FAMILY_AIO_SUBCTLBLK_T__MAGIC.
 *
 * While AIO reads and writes will usually only involve a single 
 * underlying file, they can, in pricipal, involve all files in the
 * family file.  Further, in general, aio fsync operations will involve
 * all open files in the family file.  Thus the aio control block returned 
 * by the family file driver must be able to accomodate an arbitrarily
 * large list of driver/aio control block pairs.  Do this by allocating
 * an array of struct H5FD_family_aio_subctlblk_t and storing the base 
 * address of the array in the instance of H5FD_family_aio_ctlblk_t whose
 * address is passed back to the caller.  Initially set the size of the
 * array to H5FD_FAMILY_AIO_SUBCTLBLK_INIT_ARRAY_SIZE, and double its
 * size each time we run out of space. Store the number of array entries
 * currently in use in num_subctlblks, and the current size of the 
 * array in array_len.
 *
 * In each element of the array of H5FD_family_aio_subctlblk_t, we 
 * maintain a a pointer to the associated instance of H5FD_t, a 
 * pointer to the AIO control block, and booleans to store the done 
 * and finished status of each sub operation.  
 *
 * The driver, ctlblk, done, and finished fields of each entry
 * in the array are set to NULL, NULL, FALSE, and FALSE respectively,
 * and retain those values when not in use.
 */

#define H5FD_FAMILY_AIO_CTLBLK_T__MAGIC		  0x00000000 /* 'FACB' */
#define H5FD_FAMILY_AIO_SUBCTLBLK_T__MAGIC	  0x00000000 /* 'FSCB' */
#define H5FD_FAMILY_AIO_SUBCTLBLK_INIT_ARRAY_SIZE 1

typedef struct H5FD_family_aio_subctlblk_t {

    uint32_t   magic;
    H5FD_t   * driver;
    void     * ctlblk;
    hbool_t    done;
    hbool_t    finished;

} H5FD_family_aio_subctlblk_t;

typedef struct H5FD_family_aio_ctlblk_t {

    uint32_t                      magic;
    int			          array_len;
    int                           num_subctlblks;
    H5FD_family_aio_subctlblk_t * subctlblks;

} H5FD_family_aio_ctlblk_t;

/* declare a free list to manage top level aio control blocks */
H5FL_DEFINE_STATIC(H5FD_family_aio_ctlblk_t);

/* declare a free list to manage singleton aio sub control blocks.
 * This is the common case -- when we ever need more than one we will
 * just do a malloc.
 */
H5FL_DEFINE_STATIC(H5FD_family_aio_subctlblk_t);


/* The driver identification number, initialized at runtime */
static hid_t H5FD_FAMILY_g = 0;

/* The description of a file belonging to this driver. */
typedef struct H5FD_family_t {
    H5FD_t	pub;		/*public stuff, must be first		*/
    hid_t	memb_fapl_id;	/*file access property list for members	*/
    hsize_t	memb_size;	/*actual size of each member file	*/
    hsize_t	pmem_size;	/*member size passed in from property	*/
    unsigned	nmembs;		/*number of family members		*/
    unsigned	amembs;		/*number of member slots allocated	*/
    H5FD_t	**memb;		/*dynamic array of member pointers	*/
    haddr_t	eoa;		/*end of allocated addresses		*/
    char	*name;		/*name generator printf format		*/
    unsigned	flags;		/*flags for opening additional members	*/

    /* Information from properties set by 'h5repart' tool */
    hsize_t	mem_newsize;	/*new member size passed in as private
                                 * property. It's used only by h5repart */
    hbool_t     repart_members; /* Whether to mark the superblock dirty
                                 * when it is loaded, so that the family
                                 * member sizes can be re-encoded       */
} H5FD_family_t;

/* Driver-specific file access properties */
typedef struct H5FD_family_fapl_t {
    hsize_t	memb_size;	/*size of each member			*/
    hid_t	memb_fapl_id;	/*file access property list of each memb*/
} H5FD_family_fapl_t;

/* Driver specific data transfer properties */
typedef struct H5FD_family_dxpl_t {
    hid_t	memb_dxpl_id;	/*data xfer property list of each memb	*/
} H5FD_family_dxpl_t;

/* Callback prototypes */
static void *H5FD_family_fapl_get(H5FD_t *_file);
static void *H5FD_family_fapl_copy(const void *_old_fa);
static herr_t H5FD_family_fapl_free(void *_fa);
static void *H5FD_family_dxpl_copy(const void *_old_dx);
static herr_t H5FD_family_dxpl_free(void *_dx);
static hsize_t H5FD_family_sb_size(H5FD_t *_file);
static herr_t H5FD_family_sb_encode(H5FD_t *_file, char *name/*out*/,
		     unsigned char *buf/*out*/);
static herr_t H5FD_family_sb_decode(H5FD_t *_file, const char *name,
                    const unsigned char *buf);
static H5FD_t *H5FD_family_open(const char *name, unsigned flags,
				hid_t fapl_id, haddr_t maxaddr);
static herr_t H5FD_family_close(H5FD_t *_file);
static int H5FD_family_cmp(const H5FD_t *_f1, const H5FD_t *_f2);
static herr_t H5FD_family_query(const H5FD_t *_f1, unsigned long *flags);
static haddr_t H5FD_family_get_eoa(const H5FD_t *_file, H5FD_mem_t type);
static herr_t H5FD_family_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t eoa);
static haddr_t H5FD_family_get_eof(const H5FD_t *_file);
static herr_t  H5FD_family_get_handle(H5FD_t *_file, hid_t fapl, void** file_handle);
static herr_t H5FD_family_read(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr,
			       size_t size, void *_buf/*out*/);
static herr_t H5FD_family_write(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr,
				size_t size, const void *_buf);
static herr_t H5FD_family_flush(H5FD_t *_file, hid_t dxpl_id, unsigned closing);
static herr_t H5FD_family_truncate(H5FD_t *_file, hid_t dxpl_id, unsigned closing);
static herr_t H5FD_family_aio_alloc_ctlblk(int init_array_len, 
                                     H5FD_family_aio_ctlblk_t **ctlblk_ptr_ptr);
static herr_t H5FD_family_aio_discard_ctlblk(
                                     H5FD_family_aio_ctlblk_t *ctlblk_ptr);
static herr_t H5FD_family_aio_extend_ctlblk(
                                     H5FD_family_aio_ctlblk_t *ctlblk_ptr);
static herr_t H5FD_family_aio_read(H5FD_t *file, 
                                   H5FD_mem_t type, 
                                   hid_t dxpl_id,
                                   haddr_t addr, 
                                   size_t size, 
                                   void *buffer,
                                   void **ctlblk_ptr_ptr);
static herr_t H5FD_family_aio_write(H5FD_t *file, 
                                    H5FD_mem_t type, 
                                    hid_t dxpl_id,
                                    haddr_t addr, 
                                    size_t size, 
                                    void *buffer,
                                    void **ctlblk_ptr_ptr);
static herr_t H5FD_family_aio_test(hbool_t *done_ptr, void *ctlblk_ptr);
static herr_t H5FD_family_aio_wait(void *ctlblk_ptr);
static herr_t H5FD_family_aio_finish(int *errno_ptr, void *ctlblk_ptr);
static herr_t H5FD_family_aio_fsync(H5FD_t *file, void **ctlblk_ptr_ptr);
static herr_t H5FD_family_aio_cancel(void *ctlblk_ptr);
static herr_t H5FD_family_fsync(H5FD_t *file, hid_t dxpl_id);

/* The class struct */
static const H5FD_class_t H5FD_family_g = {
    "family",					/*name			*/
    HADDR_MAX,					/*maxaddr		*/
    H5F_CLOSE_WEAK,				/*fc_degree		*/
    H5FD_family_sb_size,			/*sb_size		*/
    H5FD_family_sb_encode,			/*sb_encode		*/
    H5FD_family_sb_decode,			/*sb_decode		*/
    sizeof(H5FD_family_fapl_t),			/*fapl_size		*/
    H5FD_family_fapl_get,			/*fapl_get		*/
    H5FD_family_fapl_copy,			/*fapl_copy		*/
    H5FD_family_fapl_free,			/*fapl_free		*/
    sizeof(H5FD_family_dxpl_t),			/*dxpl_size		*/
    H5FD_family_dxpl_copy,			/*dxpl_copy		*/
    H5FD_family_dxpl_free,			/*dxpl_free		*/
    H5FD_family_open,				/*open			*/
    H5FD_family_close,				/*close			*/
    H5FD_family_cmp,				/*cmp			*/
    H5FD_family_query,		                /*query			*/
    NULL,					/*get_type_map		*/
    NULL,					/*alloc			*/
    NULL,					/*free			*/
    H5FD_family_get_eoa,			/*get_eoa		*/
    H5FD_family_set_eoa,			/*set_eoa		*/
    H5FD_family_get_eof,			/*get_eof		*/
    H5FD_family_get_handle,                     /*get_handle            */
    H5FD_family_read,				/*read			*/
    H5FD_family_write,				/*write			*/
    H5FD_family_flush,				/*flush			*/
    H5FD_family_truncate,			/*truncate		*/
    NULL,                                       /*lock                  */
    NULL,                                       /*unlock                */
    H5FD_family_aio_read,                       /*aio_read              */
    H5FD_family_aio_write,                      /*aio_write             */
    H5FD_family_aio_test,                       /*aio_test              */
    H5FD_family_aio_wait,                       /*aio_wait              */
    H5FD_family_aio_finish,                     /*aio_finish            */
    H5FD_family_aio_fsync,                      /*aio_fsync             */
    H5FD_family_aio_cancel,                     /*aio_cancel            */
    H5FD_family_fsync,				/*fsync			*/
    H5FD_FLMAP_SINGLE 				/*fl_map		*/
};


/*--------------------------------------------------------------------------
NAME
   H5FD_family_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5FD_family_init_interface()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5FD_family_init currently).

--------------------------------------------------------------------------*/
static herr_t
H5FD_family_init_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_family_init_interface)

    FUNC_LEAVE_NOAPI(H5FD_family_init())
} /* H5FD_family_init_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_init
 *
 * Purpose:	Initialize this driver by registering the driver with the
 *		library.
 *
 * Return:	Success:	The driver ID for the family driver.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5FD_family_init(void)
{
    hid_t ret_value=H5FD_FAMILY_g;   /* Return value */

    FUNC_ENTER_NOAPI(H5FD_family_init, FAIL)

    if (H5I_VFL!=H5Iget_type(H5FD_FAMILY_g))
        H5FD_FAMILY_g = H5FD_register(&H5FD_family_g,sizeof(H5FD_class_t),FALSE);

    /* Set return value */
    ret_value=H5FD_FAMILY_g;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*---------------------------------------------------------------------------
 * Function:	H5FD_family_term
 *
 * Purpose:	Shut down the VFD
 *
 * Return:	<none>
 *
 * Programmer:  Quincey Koziol
 *              Friday, Jan 30, 2004
 *
 * Modification:
 *
 *---------------------------------------------------------------------------
 */
void
H5FD_family_term(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_family_term)

    /* Reset VFL ID */
    H5FD_FAMILY_g=0;

    FUNC_LEAVE_NOAPI_VOID
} /* end H5FD_family_term() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_fapl_family
 *
 * Purpose:	Sets the file access property list FAPL_ID to use the family
 *		driver. The MEMB_SIZE is the size in bytes of each file
 *		member (used only when creating a new file) and the
 *		MEMB_FAPL_ID is a file access property list to be used for
 *		each family member.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *
 *		Raymond Lu
 * 		Tuesday, Oct 23, 2001
 *		Changed the file access list to the new generic property
 *		list.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_family(hid_t fapl_id, hsize_t msize, hid_t memb_fapl_id)
{
    herr_t ret_value;
    H5FD_family_fapl_t	fa={0, -1};
    H5P_genplist_t *plist;      /* Property list pointer */

    FUNC_ENTER_API(H5Pset_fapl_family, FAIL)
    H5TRACE3("e", "ihi", fapl_id, msize, memb_fapl_id);


    /* Check arguments */
    if(TRUE != H5P_isa_class(fapl_id, H5P_FILE_ACCESS))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")
    if(H5P_DEFAULT == memb_fapl_id)
        memb_fapl_id = H5P_FILE_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(memb_fapl_id, H5P_FILE_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access list")

    /*
     * Initialize driver specific information. No need to copy it into the FA
     * struct since all members will be copied by H5P_set_driver().
     */
    fa.memb_size = msize;
    fa.memb_fapl_id = memb_fapl_id;

    if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")
    ret_value= H5P_set_driver(plist, H5FD_FAMILY, &fa);

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_fapl_family
 *
 * Purpose:	Returns information about the family file access property
 *		list though the function arguments.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *
 *		Raymond Lu
 * 		Tuesday, Oct 23, 2001
 *		Changed the file access list to the new generic property
 *		list.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_fapl_family(hid_t fapl_id, hsize_t *msize/*out*/,
		   hid_t *memb_fapl_id/*out*/)
{
    H5FD_family_fapl_t	*fa;
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Pget_fapl_family, FAIL)
    H5TRACE3("e", "ixx", fapl_id, msize, memb_fapl_id);

    if(NULL == (plist = H5P_object_verify(fapl_id,H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access list")
    if(H5FD_FAMILY != H5P_get_driver(plist))
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "incorrect VFL driver")
    if(NULL == (fa = (H5FD_family_fapl_t *)H5P_get_driver_info(plist)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "bad VFL driver info")
    if(msize)
        *msize = fa->memb_size;
    if(memb_fapl_id) {
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(fa->memb_fapl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access list")
        *memb_fapl_id = H5P_copy_plist(plist, TRUE);
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_fapl_get
 *
 * Purpose:	Gets a file access property list which could be used to
 *		create an identical file.
 *
 * Return:	Success:	Ptr to new file access property list.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Friday, August 13, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5FD_family_fapl_get(H5FD_t *_file)
{
    H5FD_family_t	*file = (H5FD_family_t*)_file;
    H5FD_family_fapl_t	*fa = NULL;
    H5P_genplist_t *plist;      /* Property list pointer */
    void *ret_value;       /* Return value */

    FUNC_ENTER_NOAPI(H5FD_family_fapl_get, NULL)

    if(NULL == (fa = (H5FD_family_fapl_t *)H5MM_calloc(sizeof(H5FD_family_fapl_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    fa->memb_size = file->memb_size;
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(file->memb_fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
    fa->memb_fapl_id = H5P_copy_plist(plist, FALSE);

    /* Set return value */
    ret_value=fa;

done:
    if(ret_value==NULL) {
        if(fa!=NULL)
            H5MM_xfree(fa);
    } /* end if */
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_fapl_copy
 *
 * Purpose:	Copies the family-specific file access properties.
 *
 * Return:	Success:	Ptr to a new property list
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5FD_family_fapl_copy(const void *_old_fa)
{
    const H5FD_family_fapl_t *old_fa = (const H5FD_family_fapl_t*)_old_fa;
    H5FD_family_fapl_t *new_fa = NULL;
    H5P_genplist_t *plist;      /* Property list pointer */
    void *ret_value;       /* Return value */

    FUNC_ENTER_NOAPI(H5FD_family_fapl_copy, NULL)

    if(NULL == (new_fa = (H5FD_family_fapl_t *)H5MM_malloc(sizeof(H5FD_family_fapl_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Copy the fields of the structure */
    memcpy(new_fa, old_fa, sizeof(H5FD_family_fapl_t));

    /* Deep copy the property list objects in the structure */
    if(old_fa->memb_fapl_id==H5P_FILE_ACCESS_DEFAULT) {
        if(H5I_inc_ref(new_fa->memb_fapl_id, FALSE)<0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTINC, NULL, "unable to increment ref count on VFL driver")
    } /* end if */
    else {
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(old_fa->memb_fapl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
        new_fa->memb_fapl_id = H5P_copy_plist(plist, FALSE);
    } /* end else */

    /* Set return value */
    ret_value=new_fa;

done:
    if(ret_value==NULL) {
        if(new_fa!=NULL)
            H5MM_xfree(new_fa);
    } /* end if */
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_fapl_free
 *
 * Purpose:	Frees the family-specific file access properties.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_family_fapl_free(void *_fa)
{
    H5FD_family_fapl_t	*fa = (H5FD_family_fapl_t*)_fa;
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5FD_family_fapl_free, FAIL)

    if(H5I_dec_ref(fa->memb_fapl_id, FALSE)<0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTDEC, FAIL, "can't close driver ID")
    H5MM_xfree(fa);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_dxpl_copy
 *
 * Purpose:	Copes the family-specific data transfer properties.
 *
 * Return:	Success:	Ptr to new property list
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5FD_family_dxpl_copy(const void *_old_dx)
{
    const H5FD_family_dxpl_t *old_dx = (const H5FD_family_dxpl_t*)_old_dx;
    H5FD_family_dxpl_t *new_dx = NULL;
    H5P_genplist_t *plist;      /* Property list pointer */
    void *ret_value;       /* Return value */

    FUNC_ENTER_NOAPI(H5FD_family_dxpl_copy, NULL)

    if(NULL == (new_dx = (H5FD_family_dxpl_t *)H5MM_malloc(sizeof(H5FD_family_dxpl_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    HDmemcpy(new_dx, old_dx, sizeof(H5FD_family_dxpl_t));

    if(old_dx->memb_dxpl_id == H5P_DATASET_XFER_DEFAULT) {
        if(H5I_inc_ref(new_dx->memb_dxpl_id, FALSE)<0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTINC, NULL, "unable to increment ref count on VFL driver")
    } /* end if */
    else {
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(old_dx->memb_dxpl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
        new_dx->memb_dxpl_id = H5P_copy_plist(plist, FALSE);
    } /* end else */

    /* Set return value */
    ret_value=new_dx;

done:
    if(ret_value==NULL) {
        if(new_dx!=NULL)
            H5MM_xfree(new_dx);
    } /* end if */
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_dxpl_free
 *
 * Purpose:	Frees the family-specific data transfer properties.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_family_dxpl_free(void *_dx)
{
    H5FD_family_dxpl_t	*dx = (H5FD_family_dxpl_t*)_dx;
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5FD_family_dxpl_free, FAIL)

    if(H5I_dec_ref(dx->memb_dxpl_id, FALSE)<0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTDEC, FAIL, "can't close driver ID")
    H5MM_xfree(dx);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_sb_size
 *
 * Purpose:	Returns the size of the private information to be stored in
 *		the superblock.
 *
 * Return:	Success:	The super block driver data size.
 *
 *		Failure:	never fails
 *
 * Programmer:	Raymond Lu
 *              Tuesday, May 10, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hsize_t
H5FD_family_sb_size(H5FD_t UNUSED *_file)
{
    hsize_t		ret_value = 0; /*size of header*/

    FUNC_ENTER_NOAPI(H5FD_family_sb_size, UFAIL)

    /* 8 bytes field for the size of member file size field should be
     * enough for now. */
    ret_value += 8;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_sb_encode
 *
 * Purpose:	Encode driver information for the superblock. The NAME
 *		argument is a nine-byte buffer which will be initialized with
 *		an eight-character name/version number and null termination.
 *
 *		The encoding is the member file size and name template.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *              Tuesday, May 10, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_family_sb_encode(H5FD_t *_file, char *name/*out*/, unsigned char *buf/*out*/)
{
    H5FD_family_t	*file = (H5FD_family_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_family_sb_encode)

    /* Name and version number */
    HDstrncpy(name, "NCSAfami", (size_t)8);
    name[8] = '\0';

    /* Store member file size.  Use the member file size from the property here.
     * This is to guarantee backward compatibility.  If a file is created with
     * v1.6 library and the driver info isn't saved in the superblock.  We open
     * it with v1.8, the FILE->MEMB_SIZE will be the actual size of the first
     * member file (see H5FD_family_open).  So it isn't safe to use FILE->MEMB_SIZE.
     * If the file is created with v1.8, the correctness of FILE->PMEM_SIZE is
     * checked in H5FD_family_sb_decode. SLU - 2009/3/21
     */
    UINT64ENCODE(buf, (uint64_t)file->pmem_size);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD_family_sb_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_sb_decode
 *
 * Purpose:	This function has 2 seperate purpose.  One is to decodes the
 *              superblock information for this driver. The NAME argument is
 *              the eight-character (plus null termination) name stored in i
 *              the file.  The FILE argument is updated according to the
 *              information in the superblock.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *              Tuesday, May 10, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_family_sb_decode(H5FD_t *_file, const char UNUSED *name, const unsigned char *buf)
{
    H5FD_family_t	*file = (H5FD_family_t*)_file;
    uint64_t            msize;
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5FD_family_sb_decode, FAIL)

    /* Read member file size. Skip name template for now although it's saved. */
    UINT64DECODE(buf, msize);

    /* For h5repart only. Private property of new member size is used to signal
     * h5repart is being used to change member file size.  h5repart will open
     * files for read and write.  When the files are closed, metadata will be
     * flushed to the files and updated to this new size */
    if(file->mem_newsize) {
        file->memb_size = file->pmem_size = file->mem_newsize;
        HGOTO_DONE(ret_value)
    } /* end if */

    /* Default - use the saved member size */
    if(file->pmem_size == H5F_FAMILY_DEFAULT)
       file->pmem_size = msize;

    /* Check if member size from file access property is correct */
    if(msize != file->pmem_size) {
        char                err_msg[128];

        sprintf(err_msg, "Family member size should be %lu.  But the size from file access property is %lu", (unsigned long)msize, (unsigned long)file->pmem_size);
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, err_msg)
    } /* end if */

    /* Update member file size to the size saved in the superblock.
     * That's the size intended to be. */
    file->memb_size = msize;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_family_sb_decode() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_open
 *
 * Purpose:	Creates and/or opens a family of files as an HDF5 file.
 *
 * Return:	Success:	A pointer to a new file dat structure. The
 *				public fields will be initialized by the
 *				caller, which is always H5FD_open().
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *              Raymond Lu
 *              Thursday, November 18, 2004
 *              When file is re-opened, member size passed in from access property
 *              is checked to see if it's reasonable.  If there is only 1 member
 *              file, member size can't be smaller than current member size.
 *              If there are at least 2 member files, member size can only be equal
 *              the 1st member size.
 *
 *              Raymond Lu
 *              Tuesday, May 24, 2005
 *              The modification described above has been changed.  The major checking
 *              is done in H5F_read_superblock.  Member file size is saved in the
 *              superblock now.  H5F_read_superblock() reads this saved size and compare
 *              to the size passed in from file access property.  Wrong size will
 *              result in a failure.
 *
 *-------------------------------------------------------------------------
 */
static H5FD_t *
H5FD_family_open(const char *name, unsigned flags, hid_t fapl_id,
		 haddr_t maxaddr)
{
    H5FD_family_t	*file=NULL;
    H5FD_t     		*ret_value=NULL;
    char		memb_name[4096], temp[4096];
    hsize_t		eof=HADDR_UNDEF;
    unsigned		t_flags = flags & ~H5F_ACC_CREAT;

    FUNC_ENTER_NOAPI(H5FD_family_open, NULL)

    /* Check arguments */
    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid file name")
    if(0 == maxaddr || HADDR_UNDEF == maxaddr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, NULL, "bogus maxaddr")

    /* Initialize file from file access properties */
    if(NULL == (file = (H5FD_family_t *)H5MM_calloc(sizeof(H5FD_family_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "unable to allocate file struct")
    if(H5P_FILE_ACCESS_DEFAULT==fapl_id) {
        file->memb_fapl_id = H5P_FILE_ACCESS_DEFAULT;
        if(H5I_inc_ref(file->memb_fapl_id, FALSE) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTINC, NULL, "unable to increment ref count on VFL driver")
        file->memb_size = 1024 * 1024 * 1024; /*1GB. Actual member size to be updated later */
        file->pmem_size = 1024 * 1024 * 1024; /*1GB. Member size passed in through property */
        file->mem_newsize = 0;            /*New member size used by h5repart only       */
    } /* end if */
    else {
        H5P_genplist_t      *plist;      /* Property list pointer */
        H5FD_family_fapl_t *fa;

        if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
        fa = (H5FD_family_fapl_t *)H5P_get_driver_info(plist);
        HDassert(fa);

        /* Check for new family file size. It's used by h5repart only. */
        if(H5P_exist_plist(plist, H5F_ACS_FAMILY_NEWSIZE_NAME) > 0) {
            hsize_t fam_newsize = 0;        /* New member size, when repartitioning */

            /* Get the new family file size */
            if(H5P_get(plist, H5F_ACS_FAMILY_NEWSIZE_NAME, &fam_newsize) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get new family member size")

            /* Store information for later */
            file->mem_newsize = fam_newsize; /* New member size passed in through property */
            file->repart_members = TRUE;
        } /* end if */

        if(fa->memb_fapl_id==H5P_FILE_ACCESS_DEFAULT) {
            if(H5I_inc_ref(fa->memb_fapl_id, FALSE)<0)
                HGOTO_ERROR(H5E_VFL, H5E_CANTINC, NULL, "unable to increment ref count on VFL driver")
            file->memb_fapl_id = fa->memb_fapl_id;
        } /* end if */
        else {
            if(NULL == (plist = (H5P_genplist_t *)H5I_object(fa->memb_fapl_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
            file->memb_fapl_id = H5P_copy_plist(plist, FALSE);
        } /* end else */
        file->memb_size = fa->memb_size; /* Actual member size to be updated later */
        file->pmem_size = fa->memb_size; /* Member size passed in through property */
    } /* end else */
    file->name = H5MM_strdup(name);
    file->flags = flags;

    /* Check that names are unique */
    sprintf(memb_name, name, 0);
    sprintf(temp, name, 1);
    if(!HDstrcmp(memb_name, temp))
        HGOTO_ERROR(H5E_FILE, H5E_FILEEXISTS, NULL, "file names not unique")

    /* Open all the family members */
    while(1) {
        sprintf(memb_name, name, file->nmembs);

        /* Enlarge member array */
        if(file->nmembs >= file->amembs) {
            unsigned n = MAX(64, 2 * file->amembs);
            H5FD_t **x;

            HDassert(n > 0);
            if(NULL == (x = (H5FD_t **)H5MM_realloc(file->memb, n * sizeof(H5FD_t *))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "unable to reallocate members")
            file->amembs = n;
            file->memb = x;
        } /* end if */

        /*
         * Attempt to open file. If the first file cannot be opened then fail;
         * otherwise an open failure means that we've reached the last member.
         * Allow H5F_ACC_CREAT only on the first family member.
         */
        H5E_BEGIN_TRY {
            file->memb[file->nmembs] = H5FDopen(memb_name,
                (0==file->nmembs ? flags : t_flags), file->memb_fapl_id, HADDR_UNDEF);
        } H5E_END_TRY;
        if (!file->memb[file->nmembs]) {
            if (0==file->nmembs)
                HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open member file")
            H5E_clear_stack(NULL);
            break;
        }
        file->nmembs++;
    }

    /* If the file is reopened and there's only one member file existing, this file maybe
     * smaller than the size specified through H5Pset_fapl_family().  Update the actual
     * member size.
     */
    if ((eof=H5FDget_eof(file->memb[0]))) file->memb_size = eof;

    ret_value=(H5FD_t *)file;

done:
    /* Cleanup and fail */
    if (ret_value==NULL && file!=NULL) {
        unsigned nerrors=0;     /* Number of errors closing member files */
        unsigned u;             /* Local index variable */

        /* Close as many members as possible. Use private function here to avoid clearing
         * the error stack. We need the error message to indicate wrong member file size. */
        for (u=0; u<file->nmembs; u++)
            if (file->memb[u])
                if (H5FD_close(file->memb[u])<0)
                    nerrors++;
        if (nerrors)
            HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, NULL, "unable to close member files")

        if (file->memb)
            H5MM_xfree(file->memb);
        if(H5I_dec_ref(file->memb_fapl_id, FALSE)<0)
            HDONE_ERROR(H5E_VFL, H5E_CANTDEC, NULL, "can't close driver ID")
        if (file->name)
            H5MM_xfree(file->name);
        H5MM_xfree(file);
    }
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_close
 *
 * Purpose:	Closes a family of files.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative with as many members closed as
 *				possible. The only subsequent operation
 *				permitted on the file is a close operation.
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_family_close(H5FD_t *_file)
{
    H5FD_family_t *file = (H5FD_family_t*)_file;
    unsigned	nerrors = 0;    /* Number of errors while closing member files */
    unsigned	u;              /* Local index variable */
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5FD_family_close, FAIL)

    /* Close as many members as possible. Use private function here to avoid clearing
     * the error stack. We need the error message to indicate wrong member file size. */
    for(u = 0; u < file->nmembs; u++) {
        if(file->memb[u]) {
            if(H5FD_close(file->memb[u]) < 0)
                nerrors++;
            else
                file->memb[u] = NULL;
        } /* end if */
    } /* end for */
    if(nerrors)
        /* Push error, but keep going*/
        HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "unable to close member files")

    /* Clean up other stuff */
    if(H5I_dec_ref(file->memb_fapl_id, FALSE) < 0)
        /* Push error, but keep going*/
        HDONE_ERROR(H5E_VFL, H5E_CANTDEC, FAIL, "can't close driver ID")
    H5MM_xfree(file->memb);
    H5MM_xfree(file->name);
    H5MM_xfree(file);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_family_close() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_cmp
 *
 * Purpose:	Compares two file families to see if they are the same. It
 *		does this by comparing the first member of the two families.
 *
 * Return:	Success:	like strcmp()
 *
 *		Failure:	never fails (arguments were checked by the
 *				caller).
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
H5FD_family_cmp(const H5FD_t *_f1, const H5FD_t *_f2)
{
    const H5FD_family_t	*f1 = (const H5FD_family_t*)_f1;
    const H5FD_family_t	*f2 = (const H5FD_family_t*)_f2;
    int ret_value=(H5FD_VFD_DEFAULT);

    FUNC_ENTER_NOAPI(H5FD_family_cmp, H5FD_VFD_DEFAULT)

    assert(f1->nmembs>=1 && f1->memb[0]);
    assert(f2->nmembs>=1 && f2->memb[0]);

    ret_value= H5FDcmp(f1->memb[0], f2->memb[0]);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_query
 *
 * Purpose:	Set the flags that this VFL driver is capable of supporting.
 *              (listed in H5FDpublic.h)
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Friday, August 25, 2000
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
static herr_t
H5FD_family_query(const H5FD_t * _file, unsigned long *flags /* out */)
{
    const H5FD_family_t	*file = (const H5FD_family_t*)_file;    /* Family VFD info */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_family_query)

    /* Set the VFL feature flags that this driver supports */
    if(flags) {
        *flags = 0;
        *flags |= H5FD_FEAT_AGGREGATE_METADATA; /* OK to aggregate metadata allocations */
        *flags |= H5FD_FEAT_ACCUMULATE_METADATA; /* OK to accumulate metadata for faster writes. */
        *flags |= H5FD_FEAT_DATA_SIEVE;       /* OK to perform data sieving for faster raw data reads & writes */
        *flags |= H5FD_FEAT_AGGREGATE_SMALLDATA; /* OK to aggregate "small" raw data allocations */

        /* Check for flags that are set by h5repart */
        if(file->repart_members)
            *flags |= H5FD_FEAT_DIRTY_SBLK_LOAD; /* Mark the superblock dirty when it is loaded (so the family member sizes are rewritten) */
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD_family_query() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_get_eoa
 *
 * Purpose:	Returns the end-of-address marker for the file. The EOA
 *		marker is the first address past the last byte allocated in
 *		the format address space.
 *
 * Return:	Success:	The end-of-address-marker
 *
 *		Failure:	HADDR_UNDEF
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *              Raymond Lu
 *              21 Dec. 2006
 *              Added the parameter TYPE.  It's only used for MULTI driver.
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_family_get_eoa(const H5FD_t *_file, H5FD_mem_t UNUSED type)
{
    const H5FD_family_t	*file = (const H5FD_family_t*)_file;
    haddr_t ret_value;   /* Return value */

    FUNC_ENTER_NOAPI(H5FD_family_get_eoa, HADDR_UNDEF)

    /* Set return value */
    ret_value=file->eoa;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_set_eoa
 *
 * Purpose:	Set the end-of-address marker for the file.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *              Raymond Lu
 *              21 Dec. 2006
 *              Added the parameter TYPE.  It's only used for MULTI driver.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_family_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t abs_eoa)
{
    H5FD_family_t	*file = (H5FD_family_t*)_file;
    haddr_t		addr = abs_eoa;
    char		memb_name[4096];
    unsigned		u;                      /* Local index variable */
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5FD_family_set_eoa, FAIL)

    for(u = 0; addr || u < file->nmembs; u++) {

        /* Enlarge member array */
        if(u >= file->amembs) {
            unsigned n = MAX(64, 2 * file->amembs);
            H5FD_t **x = (H5FD_t **)H5MM_realloc(file->memb, n * sizeof(H5FD_t *));

            if(!x)
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to allocate memory block")
            file->amembs = n;
            file->memb = x;
            file->nmembs = u;
        } /* end if */

        /* Create another file if necessary */
        if(u >= file->nmembs || !file->memb[u]) {
            file->nmembs = MAX(file->nmembs, u+1);
            sprintf(memb_name, file->name, u);
            H5E_BEGIN_TRY {
                H5_CHECK_OVERFLOW(file->memb_size, hsize_t, haddr_t);
                file->memb[u] = H5FDopen(memb_name, file->flags | H5F_ACC_CREAT,
                             file->memb_fapl_id, (haddr_t)file->memb_size);
            } H5E_END_TRY;
            if(NULL == file->memb[u])
                HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to open member file")
        } /* end if */

        /* Set the EOA marker for the member */
        /* (Note compensating for base address addition in internal routine) */
        H5_CHECK_OVERFLOW(file->memb_size, hsize_t, haddr_t);
        if(addr > (haddr_t)file->memb_size) {
            if(H5FD_set_eoa(file->memb[u], type, ((haddr_t)file->memb_size - file->pub.base_addr)) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to set file eoa")
            addr -= file->memb_size;
        } /* end if */
        else {
            if(H5FD_set_eoa(file->memb[u], type, (addr - file->pub.base_addr)) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to set file eoa")
            addr = 0;
        } /* end else */
    } /* end for */

    file->eoa = abs_eoa;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_get_eof
 *
 * Purpose:	Returns the end-of-file marker, which is the greater of
 *		either the total family size or the current EOA marker.
 *
 * Return:	Success:	End of file address, the first address past
 *				the end of the family of files or the current
 *				EOA, whichever is larger.
 *
 *		Failure:      	HADDR_UNDEF
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_family_get_eof(const H5FD_t *_file)
{
    const H5FD_family_t	*file = (const H5FD_family_t*)_file;
    haddr_t		eof=0;
    int			i;      /* Local index variable */
    haddr_t ret_value;   /* Return value */

    FUNC_ENTER_NOAPI(H5FD_family_get_eof, HADDR_UNDEF)

    /*
     * Find the last member that has a non-zero EOF and break out of the loop
     * with `i' equal to that member. If all members have zero EOF then exit
     * loop with i==0.
     */
    HDassert(file->nmembs > 0);
    for(i = (int)file->nmembs - 1; i >= 0; --i) {
        if((eof = H5FD_get_eof(file->memb[i])) != 0)
            break;
        if(0 == i)
            break;
    } /* end for */

    /* Adjust for base address for file */
    eof += file->pub.base_addr;

    /*
     * The file size is the number of members before the i'th member plus the
     * size of the i'th member.
     */
    eof += ((unsigned)i)*file->memb_size;

    /* Set return value */
    ret_value = MAX(eof, file->eoa);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:       H5FD_family_get_handle
 *
 * Purpose:        Returns the file handle of FAMILY file driver.
 *
 * Returns:        Non-negative if succeed or negative if fails.
 *
 * Programmer:     Raymond Lu
 *                 Sept. 16, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_family_get_handle(H5FD_t *_file, hid_t fapl, void** file_handle)
{
    H5FD_family_t       *file = (H5FD_family_t *)_file;
    H5P_genplist_t      *plist;
    hsize_t             offset;
    int                 memb;
    herr_t              ret_value;

    FUNC_ENTER_NOAPI(H5FD_family_get_handle, FAIL)

    /* Get the plist structure and family offset */
    if(NULL == (plist = H5P_object_verify(fapl, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
    if(H5P_get(plist, H5F_ACS_FAMILY_OFFSET_NAME, &offset) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get offset for family driver")

    if(offset > (file->memb_size * file->nmembs))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "offset is bigger than file size")
    memb = (int)(offset/file->memb_size);

    ret_value = H5FD_get_vfd_handle(file->memb[memb], fapl, file_handle);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_read
 *
 * Purpose:	Reads SIZE bytes of data from FILE beginning at address ADDR
 *		into buffer BUF according to data transfer properties in
 *		DXPL_ID.
 *
 * Return:	Success:	Zero. Result is stored in caller-supplied
 *				buffer BUF.
 *
 *		Failure:	-1, contents of buffer BUF are undefined.
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_family_read(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, size_t size,
		 void *_buf/*out*/)
{
    H5FD_family_t	*file = (H5FD_family_t*)_file;
    unsigned char	*buf = (unsigned char*)_buf;
    hid_t		memb_dxpl_id = H5P_DATASET_XFER_DEFAULT;
    haddr_t		sub;
    size_t		req;
    hsize_t             tempreq;
    unsigned		u;              /* Local index variable */
    H5P_genplist_t      *plist;      /* Property list pointer */
    herr_t              ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5FD_family_read, FAIL)

    /*
     * Get the member data transfer property list. If the transfer property
     * list does not belong to this driver then assume defaults
     */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")
    if(H5P_DATASET_XFER_DEFAULT != dxpl_id && H5FD_FAMILY == H5P_get_driver(plist)) {
        H5FD_family_dxpl_t *dx = (H5FD_family_dxpl_t *)H5P_get_driver_info(plist);

        HDassert(TRUE == H5P_isa_class(dxpl_id, H5P_DATASET_XFER));
        assert(dx);
        memb_dxpl_id = dx->memb_dxpl_id;
    } /* end if */

    /* Read from each member */
    while(size > 0) {
        H5_ASSIGN_OVERFLOW(u,addr /file->memb_size,hsize_t,unsigned);

        sub = addr % file->memb_size;

	/* This check is for mainly for IA32 architecture whose size_t's size
	 * is 4 bytes, to prevent overflow when user application is trying to
	 * write files bigger than 4GB. */
        tempreq = file->memb_size-sub;
  	if(tempreq > SIZET_MAX)
	    tempreq = SIZET_MAX;
        req = MIN(size, (size_t)tempreq);

        assert(u<file->nmembs);

        if (H5FDread(file->memb[u], type, memb_dxpl_id, sub, req, buf)<0)
            HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "member file read failed")

        addr += req;
        buf += req;
        size -= req;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_write
 *
 * Purpose:	Writes SIZE bytes of data to FILE beginning at address ADDR
 *		from buffer BUF according to data transfer properties in
 *		DXPL_ID.
 *
 * Return:	Success:	Zero
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_family_write(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, size_t size,
		  const void *_buf)
{
    H5FD_family_t	*file = (H5FD_family_t*)_file;
    const unsigned char	*buf = (const unsigned char*)_buf;
    hid_t		memb_dxpl_id = H5P_DATASET_XFER_DEFAULT;
    haddr_t		sub;
    size_t		req;
    hsize_t             tempreq;
    unsigned		u;      /* Local index variable */
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5FD_family_write, FAIL)

    /*
     * Get the member data transfer property list. If the transfer property
     * list does not belong to this driver then assume defaults.
     */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")
    if(H5P_DATASET_XFER_DEFAULT != dxpl_id && H5FD_FAMILY == H5P_get_driver(plist)) {
        H5FD_family_dxpl_t *dx = (H5FD_family_dxpl_t *)H5P_get_driver_info(plist);

        HDassert(TRUE == H5P_isa_class(dxpl_id, H5P_DATASET_XFER));
        HDassert(dx);
        memb_dxpl_id = dx->memb_dxpl_id;
    } /* end if */

    /* Write to each member */
    while (size>0) {
        H5_ASSIGN_OVERFLOW(u,addr /file->memb_size,hsize_t,unsigned);

        sub = addr % file->memb_size;

        /* This check is for mainly for IA32 architecture whose size_t's size
         * is 4 bytes, to prevent overflow when user application is trying to
         * write files bigger than 4GB. */
        tempreq = file->memb_size-sub;
	if(tempreq > SIZET_MAX)
	    tempreq = SIZET_MAX;
        req = MIN(size, (size_t)tempreq);

        assert(u<file->nmembs);

        if (H5FDwrite(file->memb[u], type, memb_dxpl_id, sub, req, buf)<0)
            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "member file write failed")

        addr += req;
        buf += req;
        size -= req;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_flush
 *
 * Purpose:	Flushes all family members.
 *
 * Return:	Success:	0
 *		Failure:	-1, as many files flushed as possible.
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_family_flush(H5FD_t *_file, hid_t dxpl_id, unsigned closing)
{
    H5FD_family_t	*file = (H5FD_family_t*)_file;
    unsigned		u, nerrors = 0;
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5FD_family_flush, FAIL)

    for(u = 0; u < file->nmembs; u++)
        if(file->memb[u] && H5FD_flush(file->memb[u], dxpl_id, closing) < 0)
            nerrors++;

    if(nerrors)
        HGOTO_ERROR(H5E_IO, H5E_BADVALUE, FAIL, "unable to flush member files")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_family_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_truncate
 *
 * Purpose:	Truncates all family members.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1, as many files truncated as possible.
 *
 * Programmer:	Quincey Koziol
 *              Saturday, February 23, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_family_truncate(H5FD_t *_file, hid_t dxpl_id, unsigned closing)
{
    H5FD_family_t	*file = (H5FD_family_t*)_file;
    unsigned		u, nerrors = 0;
    herr_t      	ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5FD_family_truncate, FAIL)

    for(u = 0; u < file->nmembs; u++)
        if(file->memb[u] && H5FD_truncate(file->memb[u], dxpl_id, closing) < 0)
            nerrors++;

    if(nerrors)
        HGOTO_ERROR(H5E_IO, H5E_BADVALUE, FAIL, "unable to flush member files")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_family_truncate() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_family_aio_alloc_ctlblk
 *
 * Purpose:     Allocate a control block for use in an asynchronous
 *              read, write, or fsync.  Allocate the array of sub aio 
 *		control blocks with initial size init_array_len.
 *
 *              If successful, return a pointer to the newly allocated
 *              and initialized control block in *ctlblk_ptr_ptr.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        Negative
 *
 * Programmer:  John Mainzer
 *              6/20/10
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5FD_family_aio_alloc_ctlblk(int init_array_len,
                             H5FD_family_aio_ctlblk_t **ctlblk_ptr_ptr)
{
    herr_t                     ret_value = SUCCEED;       /* Return value */
    int                        i;
    H5FD_family_aio_ctlblk_t * ctlblk_ptr = NULL;

    FUNC_ENTER_NOAPI(H5FD_family_aio_alloc_ctlblk, FAIL)

    if ( ( ctlblk_ptr_ptr == NULL ) ||
         ( *ctlblk_ptr_ptr != NULL ) ||
         ( init_array_len <= 0 ) ) {

	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bad param(s) on entry")
    }

    ctlblk_ptr = H5FL_CALLOC(H5FD_family_aio_ctlblk_t);

    if ( ctlblk_ptr == NULL ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "memory allocation failed(1)")
    }

    ctlblk_ptr->magic          = H5FD_FAMILY_AIO_CTLBLK_T__MAGIC;
    ctlblk_ptr->array_len      = init_array_len;
    ctlblk_ptr->num_subctlblks = 0;
    ctlblk_ptr->subctlblks     = NULL;


    /* for the most common case of only one underlying file associated
     * with the operation, use the free list of instances of 
     * H5FD_family_aio_subctlblk_t.  For larger numbers of underlying 
     * files, just malloc an array of the desired size. 
     */
    if ( init_array_len == 1 ) {

        ctlblk_ptr->subctlblks =  H5FL_CALLOC(H5FD_family_aio_subctlblk_t);

        if ( ctlblk_ptr->subctlblks == NULL ) {

            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                        "memory allocation failed(2)")
        }
    } else {

        HDassert( init_array_len > 1 );

        ctlblk_ptr->subctlblks = (H5FD_family_aio_subctlblk_t *)
		H5MM_malloc(((size_t)init_array_len) * 
                            sizeof(H5FD_family_aio_subctlblk_t));


        if ( ctlblk_ptr->subctlblks == NULL ) {

            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                        "memory allocation failed(3)")
        }
    }

    for ( i = 0; i < ctlblk_ptr->array_len; i++ )
    {
        (ctlblk_ptr->subctlblks[i]).magic = H5FD_FAMILY_AIO_SUBCTLBLK_T__MAGIC;
        (ctlblk_ptr->subctlblks[i]).driver   = NULL;
        (ctlblk_ptr->subctlblks[i]).ctlblk   = NULL;
        (ctlblk_ptr->subctlblks[i]).done     = FALSE;
        (ctlblk_ptr->subctlblks[i]).finished = FALSE;
    }

    *ctlblk_ptr_ptr = ctlblk_ptr;

done:

    if ( ret_value != SUCCEED ) {

        if ( ctlblk_ptr != NULL ) {

            /* only way we should be able to get here is if the 
             * allocation of the H5FD_family_aio_ctlblk_t succeeded,
             * but the allocation of the array of 
             * H5FD_family_aio_subctlblk_t failed.  Thus, all we need to
             * do is discard the instance of H5FD_family_aio_ctlblk_t
             * before we return.
             */
            HDassert( ctlblk_ptr->subctlblks == NULL );

            ctlblk_ptr = H5FL_FREE(H5FD_family_aio_ctlblk_t, ctlblk_ptr);

            if ( ctlblk_ptr != NULL ) {

                HDONE_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                            "base ctlblk de-allocation failed")
            }

        }
    }

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5FD_family_aio_alloc_ctlblk() */


/*-------------------------------------------------------------------------
 *
 * Purpose:     Free the control block pointed to by ctlblk_ptr, marking
 *              it as invalid in passing.
 *
 * Return:      Success:        zero
 *
 *              Failure:        Negative
 *
 * Programmer:  John Mainzer
 *              6/20/10
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5FD_family_aio_discard_ctlblk(H5FD_family_aio_ctlblk_t *ctlblk_ptr)
{
    herr_t                        ret_value = SUCCEED;  /* Return value */
    hbool_t		          bad_subctlblk_magic = FALSE;
    int                           i;
    int				  array_len = 0;
    H5FD_family_aio_subctlblk_t * subctlblks = NULL;

    FUNC_ENTER_NOAPI(H5FD_family_aio_discard_ctlblk, FAIL)

    if ( ctlblk_ptr == NULL ) {

	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, \
                    "ctlblk_ptr NULL on entry")
    }

    if ( ctlblk_ptr->magic != H5FD_FAMILY_AIO_CTLBLK_T__MAGIC ) {

	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, \
                    "bad ctlblk magic")
    }

    if ( ( ctlblk_ptr->subctlblks == NULL ) ||
         (  ctlblk_ptr->array_len <= 1 ) ) {

	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, \
                    "subctlblks fields corrupt?")
    }

    array_len = ctlblk_ptr->array_len;
    subctlblks = ctlblk_ptr->subctlblks;

    for ( i = 0; i < array_len; i++ )
    {
        if ( subctlblks[i].magic != H5FD_FAMILY_AIO_SUBCTLBLK_T__MAGIC ) {

            bad_subctlblk_magic = TRUE;
        }
    }

    if ( bad_subctlblk_magic ) {

	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, \
                    "bad subctlblk magic(s)")
    }

    /* mark the control block as invalid, and null out its fields */

    ctlblk_ptr->magic          = 0;
    ctlblk_ptr->array_len      = 0;
    ctlblk_ptr->num_subctlblks = 0;
    ctlblk_ptr->subctlblks     = NULL;

    for ( i = 0; i < array_len; i++ )
    {
        (subctlblks[i]).magic    = 0;
        (subctlblks[i]).driver   = NULL;
        (subctlblks[i]).ctlblk   = NULL;
        (subctlblks[i]).done     = FALSE;
        (subctlblks[i]).finished = FALSE;
    }

    /* now free the data */

    ctlblk_ptr = H5FL_FREE(H5FD_family_aio_ctlblk_t, ctlblk_ptr);

    /* recall that we use the free list for singleton instaces of 
     * H5FD_family_aio_subctlblk_t, and malloc to allocate larger 
     * arrays.
     */
    if ( array_len == 1 ) {

        subctlblks = H5FL_FREE(H5FD_family_aio_subctlblk_t, subctlblks);

    } else {

	subctlblks = (H5FD_family_aio_subctlblk_t *)H5MM_xfree(subctlblks);
    }

    if ( ( ctlblk_ptr != NULL ) ||
         ( subctlblks != NULL ) ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                    "control block free(s) failed")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5FD_family_aio_discard_ctlblk */


/*-------------------------------------------------------------------------
 * Function:    H5FD_family_aio_extend_ctlblk
 *
 * Purpose:     Double the size of the array of sub aio control blocks
 *		in the control block.  Ensure that the entries currently
 *		in use contain the same data after the doubling as before,
 *		and that new entries are all correctly initialized.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        Negative
 *
 * Programmer:  John Mainzer
 *              6/20/10
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5FD_family_aio_extend_ctlblk(H5FD_family_aio_ctlblk_t *ctlblk_ptr)
{
    herr_t                        ret_value = SUCCEED;  /* Return value */
    hbool_t		          bad_subctlblk_magic = FALSE;
    int                           i;
    int			          old_array_len;
    int			          new_array_len;
    H5FD_family_aio_subctlblk_t * old_subctlblks = NULL;
    H5FD_family_aio_subctlblk_t * new_subctlblks = NULL;

    FUNC_ENTER_NOAPI(H5FD_family_aio_extend_ctlblk, FAIL)

    if ( ctlblk_ptr == NULL ) {

	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, \
                    "ctlblk_ptr NULL on entry")
    }

    if ( ctlblk_ptr->magic != H5FD_FAMILY_AIO_CTLBLK_T__MAGIC ) {

	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, \
                    "bad ctlblk magic")
    }

    if ( ( ctlblk_ptr->subctlblks == NULL ) ||
         (  ctlblk_ptr->array_len <= 1 ) ) {

	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, \
                    "subctlblks fields corrupt?")
    }

    old_array_len = ctlblk_ptr->array_len;
    old_subctlblks = ctlblk_ptr->subctlblks;

    for ( i = 0; i < old_array_len; i++ )
    {
        if ( old_subctlblks[i].magic != H5FD_FAMILY_AIO_SUBCTLBLK_T__MAGIC ) {

            bad_subctlblk_magic = TRUE;
        }
    }

    if ( bad_subctlblk_magic ) {

	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, \
                    "bad subctlblk magic(s)")
    }

    /* everything looks good.  Allocate the new array of sub control blocks */
    new_array_len = 2 * old_array_len;
    HDassert( new_array_len > 1);
    new_subctlblks = (H5FD_family_aio_subctlblk_t *)
		     H5MM_malloc(((size_t)new_array_len) * 
                                 sizeof(H5FD_family_aio_subctlblk_t));

    if ( ctlblk_ptr->subctlblks == NULL ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "memory allocation failed")
    }

    /* copy existing data from old_subctlblks to new_sub_cltblks, and 
     * initialize the remaining new fields.
     */
    for ( i = 0; i < old_array_len; i++ ) {

        new_subctlblks[i].magic    = old_subctlblks[i].magic;
        new_subctlblks[i].driver   = old_subctlblks[i].driver;
        new_subctlblks[i].ctlblk   = old_subctlblks[i].ctlblk;
        new_subctlblks[i].done     = old_subctlblks[i].done;
        new_subctlblks[i].finished = old_subctlblks[i].finished;
    }

    for ( i = old_array_len; i < new_array_len; i++ ) {

        new_subctlblks[i].magic    = H5FD_FAMILY_AIO_SUBCTLBLK_T__MAGIC;
        new_subctlblks[i].driver   = NULL;
        new_subctlblks[i].ctlblk   = NULL;
        new_subctlblks[i].done     = FALSE;
        new_subctlblks[i].finished = FALSE;
    }

    /* replace the old subctlblks array for the new */
    ctlblk_ptr->array_len  = new_array_len;
    ctlblk_ptr->subctlblks = new_subctlblks;

    /* null out the old subctlblks array prior to discarding it */
    for ( i = 0; i < old_array_len; i++ )
    {
        old_subctlblks[i].magic    = 0;
        old_subctlblks[i].driver   = NULL;
        old_subctlblks[i].ctlblk   = NULL;
        old_subctlblks[i].done     = FALSE;
        old_subctlblks[i].finished = FALSE;
    }

    /* free the old sub control blocks array */
    if ( old_array_len == 1 ) {

        old_subctlblks = H5FL_FREE(H5FD_family_aio_subctlblk_t, old_subctlblks);

    } else {

	old_subctlblks = 
		(H5FD_family_aio_subctlblk_t *)H5MM_xfree(old_subctlblks);
    }

    if ( old_subctlblks != NULL ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                    "old sub control blocks free failed")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5FD_family_aio_extend_ctlblk() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_aio_read
 *
 * Purpose:     Initiate an asynchronous read from the indicated file of
 *              the specified number of bytes starting at the specified
 *              offset and loading the data  into the provided buffer.
 *
 *              The buffer must be large enough to contain the requested
 *              data, and is undefined and must not be read or modified
 *              until the read completes successfully.  Completion is
 *              determined via either a call to H5FD_family_aio_test() or a
 *              call to H5FD_family_aio_wait(), and success via a call to
 *              H5FD_family_aio_finish().
 *
 *              If successful, the H5FD_family_aio_read routine will return 
 *		a pointer to an internal control block in *ctlblk_ptr_ptr.  
 *		This pointer must be used in all subsequent 
 *		H5FD_family_aio_test() / H5FD_family_aio_wait() / 
 *		H5FD_family_aio_finish() calls referring to this request.
 *
 *              Note that a successful return from this function does not
 *              imply a successful read -- simply that no errors were
 *              immediately evident.  
 *
 *		As the family file driver does no direct file I/O itself,
 *		it must pass the aio read call through to the appropriate 
 *		file driver.  As all calls to the underlying files will be
 *              make through the public H5FD interface, which will fake
 *              AIO if it is not supported, this function presumes that 
 *		AIO is supported.
 *
 *		If the call is successful, it returns a pointer to an 
 *		instance of H5FD_sec2_family_discard_ctlblk, loaded 
 *		with the address(s) of the target driver's instance of 
 *		H5FD_t, along with the address(s) of the aio control block(s)
 *		returned by the driver(s).
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	John Mainzer
 *              6/12/10
 *
 * Changes:	None.
 *
 *-------------------------------------------------------------------------
 */

static herr_t 
H5FD_family_aio_read(H5FD_t *file, 
                     H5FD_mem_t type, 
                     hid_t dxpl_id,
                     haddr_t addr, 
                     size_t size, 
                     void *buffer,
                     void **ctlblk_ptr_ptr)
{
    herr_t                     ret_value = SUCCEED;  /* Return value */
    herr_t		       result;
    hbool_t		       success = FALSE;
    int                        i;
    unsigned                   sub_file_num;         /* Local index variable */
    hid_t                      memb_dxpl_id = H5P_DATASET_XFER_DEFAULT;
    size_t		       size_remaining;
    size_t		       subfile_size;
    hsize_t		       temp_subfile_size;
    haddr_t                    addr_of_remainder;
    haddr_t		       subfile_addr;
    H5P_genplist_t           * plist;      /* Property list pointer */
    H5FD_family_t            * family_file;
    void                     * buffer_remaining;
    void                     * subctlblk_ptr = NULL;
    H5FD_family_aio_ctlblk_t * ctlblk_ptr = NULL;

    FUNC_ENTER_NOAPI(H5FD_family_aio_read, FAIL)

    if ( ( file == NULL ) ||
         ( file->cls == NULL ) ||
         ( addr == HADDR_UNDEF ) ||
         ( size <= 0 ) ||
         ( buffer == NULL ) ||
         ( ctlblk_ptr_ptr == NULL ) ||
         ( *ctlblk_ptr_ptr != NULL ) ) {

	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bad arg(s) on entry")
    }

    family_file = (H5FD_family_t *)file;

    /* Get the member data transfer property list. If the transfer property
     * list does not belong to this driver then assume defaults
     */
    plist = (H5P_genplist_t *)H5I_object(dxpl_id);

    if ( plist == NULL ) {

        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, \
                    "not a file access property list")
    }

    if ( ( dxpl_id != H5P_DATASET_XFER_DEFAULT ) &&
         ( H5P_get_driver(plist) == H5FD_FAMILY ) ) {

        H5FD_family_dxpl_t *dx;

	dx = (H5FD_family_dxpl_t *)H5P_get_driver_info(plist);

        HDassert( H5P_isa_class(dxpl_id, H5P_DATASET_XFER) == TRUE );
        HDassert( dx != NULL ); 

        memb_dxpl_id = dx->memb_dxpl_id;
    }

    /* allocate the family file AIO control block */
    result = H5FD_family_aio_alloc_ctlblk
             (
	       H5FD_FAMILY_AIO_SUBCTLBLK_INIT_ARRAY_SIZE,
               &ctlblk_ptr
             );

    if ( result != SUCCEED ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "can't allocate aio control block")

    } else if ( ( ctlblk_ptr == NULL ) ||
                ( ctlblk_ptr->magic != H5FD_FAMILY_AIO_CTLBLK_T__MAGIC ) ) {

        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                    "NULL ctlblk_ptr or bad ctlblk magic")

    } else if ( ( ctlblk_ptr->array_len <= 1 ) ||
                ( ctlblk_ptr->num_subctlblks != 0 ) ||
                ( ctlblk_ptr->subctlblks == NULL ) ||
                ( (ctlblk_ptr->subctlblks)[0].magic != 
                  H5FD_FAMILY_AIO_SUBCTLBLK_T__MAGIC ) ) {

        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                    "bad sub control block array")
    }


    /* Queue async reads for each member that handles a chunk of the data */

    size_remaining = size;
    addr_of_remainder = addr;
    buffer_remaining = buffer;

    while ( size_remaining > 0 ) {

	H5_ASSIGN_OVERFLOW(sub_file_num, \
                           addr_of_remainder/family_file->memb_size, \
                           hsize_t, unsigned);

        subfile_addr = addr_of_remainder % family_file->memb_size;

        /* This check is for mainly for IA32 architecture whose size_t's size
         * is 4 bytes, to prevent overflow when user application is trying to
         * write files bigger than 4GB. */

        temp_subfile_size = family_file->memb_size - subfile_addr;

        if ( temp_subfile_size > SIZET_MAX ) {

            temp_subfile_size = SIZET_MAX;
        }

        subfile_size = MIN(size_remaining, (size_t)temp_subfile_size);

        HDassert( sub_file_num < family_file->nmembs );

        /* check to see if we have a slot in the aio control block 
         * for this sub file -- if not, extend the control block.
         */
        if ( ctlblk_ptr->array_len <= ctlblk_ptr->num_subctlblks ) {

	    result = H5FD_family_aio_extend_ctlblk(ctlblk_ptr);

            if ( result != SUCCEED ) {

                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                            "can't extend aio control block")

            } else if ( ctlblk_ptr->array_len <= ctlblk_ptr->num_subctlblks ) {

                HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                            "sub control block array still full?!?")
            }
        }

        result = H5FDaio_read(family_file->memb[sub_file_num], type, 
                              memb_dxpl_id, subfile_addr, subfile_size, 
                              buffer_remaining, &subctlblk_ptr);

	if ( result < 0 ) {

            HGOTO_ERROR(H5E_IO, H5E_AIOREADERROR, FAIL, \
                        "member file aio read failed")
        }

        i = ctlblk_ptr->num_subctlblks;

        (ctlblk_ptr->subctlblks[i]).driver   = family_file->memb[sub_file_num];
        (ctlblk_ptr->subctlblks[i]).ctlblk   = subctlblk_ptr;
        (ctlblk_ptr->subctlblks[i]).done     = FALSE;
        (ctlblk_ptr->subctlblks[i]).finished = FALSE;

        (ctlblk_ptr->num_subctlblks)++;

        HDassert( ctlblk_ptr->num_subctlblks <= ctlblk_ptr->array_len );

        addr_of_remainder += subfile_size;
        size_remaining    -= subfile_size;

        buffer_remaining = 
		(void *)(((char *)(buffer_remaining)) + subfile_size );
    }

    /* pass back the address of the control block */
    *ctlblk_ptr_ptr = ctlblk_ptr;

    /* make note of success */
    success = TRUE;

done:

    /* discard the control block if not successful */
    if ( ( ctlblk_ptr != NULL ) && ( ! success ) ) {

        result = H5FD_family_aio_discard_ctlblk(ctlblk_ptr);

        if ( result != SUCCEED ) {

            HDONE_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "ctlblk de-allocation failed")
        }

        ctlblk_ptr = NULL;
    }


    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_family_aio_read() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_aio_write
 *
 * Purpose:     Initiate an asynchronous write to the indicated file of
 *              the specified number of bytes from the supplied  buffer
 *              to the indicated location.
 *
 *              The buffer must not be discarded or modified until the
 *              write completes successfully.  Completion is determined
 *              via either H5FD_family_aio_test() or H5FD_family_aio_wait(), 
 *		and success via H5FD_family_aio_finish().
 *
 *              Note that a successful return from this function does not
 *              imply a successful read -- simply that no errors were
 *              immediately evident.  
 *
 *		As the family file driver does no direct file I/O itself,
 *		it must pass the aio write call through to the appropriate 
 *		file driver.  As all calls to the underlying files will be
 *              make through the public H5FD interface, which will fake
 *              AIO if it is not supported, this function presumes that 
 *		AIO is supported.
 *
 *		If the call is successful, it returns a pointer to an 
 *		instance of H5FD_family_aio_ctlblk_t, loaded 
 *		with the address(s) of the target driver's instances of 
 *		H5FD_t, along with the address(s) of the aio control block(s)
 *		returned by the driver(s).
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	John Mainzer
 *              6/12/10
 *
 * Changes: 	None.
 *
 *-------------------------------------------------------------------------
 */

static herr_t 
H5FD_family_aio_write(H5FD_t *file, 
                     H5FD_mem_t type, 
                     hid_t dxpl_id,
                     haddr_t addr, 
                     size_t size, 
                     void *buffer,
                     void **ctlblk_ptr_ptr)
{
    herr_t                     ret_value = SUCCEED;  /* Return value */
    herr_t		       result;
    hbool_t		       success = FALSE;
    int                        i;
    unsigned                   sub_file_num;         /* Local index variable */
    hid_t                      memb_dxpl_id = H5P_DATASET_XFER_DEFAULT;
    size_t		       size_remaining;
    size_t		       subfile_size;
    hsize_t		       temp_subfile_size;
    haddr_t                    addr_of_remainder;
    haddr_t		       subfile_addr;
    H5P_genplist_t           * plist;      /* Property list pointer */
    H5FD_family_t            * family_file;
    void                     * buffer_remaining;
    void                     * subctlblk_ptr = NULL;
    H5FD_family_aio_ctlblk_t * ctlblk_ptr = NULL;

    FUNC_ENTER_NOAPI(H5FD_family_aio_write, FAIL)

    if ( ( file == NULL ) ||
         ( file->cls == NULL ) ||
         ( addr == HADDR_UNDEF ) ||
         ( size <= 0 ) ||
         ( buffer == NULL ) ||
         ( ctlblk_ptr_ptr == NULL ) ||
         ( *ctlblk_ptr_ptr != NULL ) ) {

	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bad arg(s) on entry")
    }

    family_file = (H5FD_family_t *)file;

    /* Get the member data transfer property list. If the transfer property
     * list does not belong to this driver then assume defaults
     */
    plist = (H5P_genplist_t *)H5I_object(dxpl_id);

    if ( plist == NULL ) {

        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, \
                    "not a file access property list")
    }

    if ( ( dxpl_id != H5P_DATASET_XFER_DEFAULT ) &&
         ( H5P_get_driver(plist) == H5FD_FAMILY ) ) {

        H5FD_family_dxpl_t *dx;

	dx = (H5FD_family_dxpl_t *)H5P_get_driver_info(plist);

        HDassert( H5P_isa_class(dxpl_id, H5P_DATASET_XFER) == TRUE );
        HDassert( dx != NULL ); 

        memb_dxpl_id = dx->memb_dxpl_id;
    }

    /* allocate the family file AIO control block */
    result = H5FD_family_aio_alloc_ctlblk
             (
	       H5FD_FAMILY_AIO_SUBCTLBLK_INIT_ARRAY_SIZE,
               &ctlblk_ptr
             );

    if ( result != SUCCEED ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "can't allocate aio control block")

    } else if ( ( ctlblk_ptr == NULL ) ||
                ( ctlblk_ptr->magic != H5FD_FAMILY_AIO_CTLBLK_T__MAGIC ) ) {

        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                    "NULL ctlblk_ptr or bad ctlblk magic")

    } else if ( ( ctlblk_ptr->array_len <= 1 ) ||
                ( ctlblk_ptr->num_subctlblks != 0 ) ||
                ( ctlblk_ptr->subctlblks == NULL ) ||
                ( (ctlblk_ptr->subctlblks)[0].magic != 
                  H5FD_FAMILY_AIO_SUBCTLBLK_T__MAGIC ) ) {

        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                    "bad sub control block array")
    }


    /* Queue async writes for each member that handles a chunk of the data */

    size_remaining = size;
    addr_of_remainder = addr;
    buffer_remaining = buffer;

    while ( size_remaining > 0 ) {

	H5_ASSIGN_OVERFLOW(sub_file_num, \
                           addr_of_remainder/family_file->memb_size, \
                           hsize_t, unsigned);

        subfile_addr = addr_of_remainder % family_file->memb_size;

        /* This check is for mainly for IA32 architecture whose size_t's size
         * is 4 bytes, to prevent overflow when user application is trying to
         * write files bigger than 4GB. */

        temp_subfile_size = family_file->memb_size - subfile_addr;

        if ( temp_subfile_size > SIZET_MAX ) {

            temp_subfile_size = SIZET_MAX;
        }

        subfile_size = MIN(size_remaining, (size_t)temp_subfile_size);

        HDassert( sub_file_num < family_file->nmembs );

        /* check to see if we have a slot in the aio control block 
         * for this sub file -- if not, extend the control block.
         */
        if ( ctlblk_ptr->array_len <= ctlblk_ptr->num_subctlblks ) {

	    result = H5FD_family_aio_extend_ctlblk(ctlblk_ptr);

            if ( result != SUCCEED ) {

                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                            "can't extend aio control block")

            } else if ( ctlblk_ptr->array_len <= ctlblk_ptr->num_subctlblks ) {

                HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                            "sub control block array still full?!?")
            }
        }

        result = H5FDaio_write(family_file->memb[sub_file_num], type, 
                               memb_dxpl_id, subfile_addr, subfile_size, 
                               buffer_remaining, &subctlblk_ptr);

	if ( result < 0 ) {

            HGOTO_ERROR(H5E_IO, H5E_AIOWRITEERROR, FAIL, \
                        "member file aio write failed")
        }

        i = ctlblk_ptr->num_subctlblks;

        (ctlblk_ptr->subctlblks[i]).driver   = family_file->memb[sub_file_num];
        (ctlblk_ptr->subctlblks[i]).ctlblk   = subctlblk_ptr;
        (ctlblk_ptr->subctlblks[i]).done     = FALSE;
        (ctlblk_ptr->subctlblks[i]).finished = FALSE;

        (ctlblk_ptr->num_subctlblks)++;

        HDassert( ctlblk_ptr->num_subctlblks <= ctlblk_ptr->array_len );

        addr_of_remainder += subfile_size;
        size_remaining    -= subfile_size;

        buffer_remaining = 
		(void *)(((char *)(buffer_remaining)) + subfile_size );
    }

    /* pass back the address of the control block */
    *ctlblk_ptr_ptr = ctlblk_ptr;

    /* make note of success */
    success = TRUE;

done:

    /* discard the control block if not successful */
    if ( ( ctlblk_ptr != NULL ) && ( ! success ) ) {

        result = H5FD_family_aio_discard_ctlblk(ctlblk_ptr);

        if ( result != SUCCEED ) {

            HDONE_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "ctlblk de-allocation failed")
        }

        ctlblk_ptr = NULL;
    }

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_family_aio_write() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_aio_test
 *
 * Purpose:	This function is used to determine if the asynchronous
 *		operation associated with the supplied control block 
 *		pointer is done.  If it is, *done_ptr should be set
 *		to TRUE, if it isn't, *done_ptr should be set to FALSE.
 *		In all cases, there function should return immediately.
 *
 *		Note that the return value only reflects errors in the 
 *		process of testing whether the operation is complete.
 *
 *		After the operation is done, a call to 
 *		H5FD_family_aio_finish() must be made to determine whether 
 *		the operation completed successfully and to allow the 
 *		driver to tidy its data structures.
 *
 *		Note that all calls to the underlying files will be 
 *		through the public H5FD interface, which will fake 
 *		AIO if it is not supported.  Thus, this function 
 *		presumes that AIO is supported.
 *
 *		However, it is possible that multiple underlying 
 *		files may be involved.
 *
 *		For each underlying file / driver first check the 
 *		control block, and see if that driver is done with
 *		the operation.  If it is, go on to the next file / 
 *		driver pair.
 *
 *		If the file / driver pair is not marked as being 
 *		done, call H5FDaio_test with the target driver.  If the
 *		call fails, fail.
 *
 *		If the call indicates that the aio operation on the 
 *		target file/driver pair is still in progress, set *done_ptr
 *		to FALSE, and return.  Otherwise go onto the next 
 *		file / driver pair and repeat.  If all underlying 
 *		file / driver pairs report the operation complete,
 *		set *done_ptr to TRUE and then return.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              6/12/10
 *
 * Changes:	None.
 *
 *-------------------------------------------------------------------------
 */

static herr_t 
H5FD_family_aio_test(hbool_t *done_ptr, 
                     void *ctlblk_ptr)
{
    herr_t                      ret_value = SUCCEED;  /* Return value */
    herr_t			result;
    hbool_t			done = TRUE;
    hbool_t			tgt_done;
    hbool_t			already_done = TRUE;
    int                         i = 0;
    H5FD_family_aio_ctlblk_t  * family_ctlblk_ptr;
    H5FD_t                    * tgt_file;
    void                      * subctlblk_ptr;

    FUNC_ENTER_NOAPI(H5FD_family_aio_test, FAIL)

    if ( ( done_ptr == NULL ) ||
         ( ctlblk_ptr == NULL ) ) {

	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bad arg(s) on entry")

    } 

    family_ctlblk_ptr = (H5FD_family_aio_ctlblk_t *)ctlblk_ptr;

    if ( family_ctlblk_ptr->magic != H5FD_FAMILY_AIO_CTLBLK_T__MAGIC ) {

	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bad ctlblk magic")

    } else if ( family_ctlblk_ptr->num_subctlblks < 1 ) {


	HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                     "empty control block on entry.")
    }

    while ( ( done ) && ( i < family_ctlblk_ptr->num_subctlblks ) ) {

        if ( (family_ctlblk_ptr->subctlblks[i]).magic != 
	     H5FD_FAMILY_AIO_SUBCTLBLK_T__MAGIC ) {

	    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, \
                        "bad subctlblk magic")
        }

        tgt_file      = (family_ctlblk_ptr->subctlblks[i]).driver;
        subctlblk_ptr = (family_ctlblk_ptr->subctlblks[i]).ctlblk;

        if ( ( tgt_file == NULL ) ||
             ( subctlblk_ptr == NULL ) ) {

	    HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                        "NULL tgt file or sub ctl blk.")
        }

        if ( (family_ctlblk_ptr->subctlblks[i]).finished ) {

	    HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                        "sub op already finished?!?!")
        }

        if ( ! ((family_ctlblk_ptr->subctlblks[i]).done) ) {

            already_done = FALSE;

            tgt_done = FALSE;

            result = H5FDaio_test(tgt_file, &tgt_done, subctlblk_ptr);

            if ( result < 0 ) {

	        HGOTO_ERROR(H5E_VFL, H5E_AIOTESTFAIL, FAIL, \
                            "aio test sub-request failed")
            }

            if ( tgt_done ) {

                (family_ctlblk_ptr->subctlblks[i]).done = TRUE;
                
            } else {

                done = FALSE;

            }
        }

        i++;
    }

    if ( already_done ) {

        HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                    "operation already done?!?!?")
    }

    *done_ptr = done;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_family_aio_test() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_aio_wait
 *
 * Purpose:	Wait until the asynchronous read, write, or fsync operation 
 *		indicated by *ctlblk_ptr has completed (successfully or 
 *		otherwise).
 *
 *		Note that the error code returned refers only to the 
 *		operation of waiting until the read/write/fsync is
 *		complete -- Success does not imply that the read, write,
 *		or fsync operation completed successfully, only that
 *		no error was encountered while waiting for the operation 
 *		to finish.
 *
 *		Note that all calls to the underlying files will be 
 *		through the public H5FD interface, which will fake 
 *		AIO if it is not supported.  Thus, this function 
 *		presumes that AIO is supported.
 *
 *		However, it is possible that multiple underlying 
 *		files may be involved.
 *
 *		For each underlying file / driver listed in the control
 *		block, first check the control block particular to the 
 *		file / driver pair, and then wait until the associated
 *		operation is done -- if it is not done already.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              6/12/10
 *
 * Changes:	None.
 *
 *-------------------------------------------------------------------------
 */

static herr_t 
H5FD_family_aio_wait(void *ctlblk_ptr)
{
    herr_t                      ret_value = SUCCEED;  /* Return value */
    herr_t			result;
    hbool_t			already_done = TRUE;
    int                         i;
    H5FD_family_aio_ctlblk_t  * family_ctlblk_ptr;
    H5FD_t                    * tgt_file;
    void                      * subctlblk_ptr;

    FUNC_ENTER_NOAPI(H5FD_family_aio_wait, FAIL)

    if ( ctlblk_ptr == NULL ) {

	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL ctlblk_ptr on entry")

    } 

    family_ctlblk_ptr = (H5FD_family_aio_ctlblk_t *)ctlblk_ptr;

    if ( family_ctlblk_ptr->magic != H5FD_FAMILY_AIO_CTLBLK_T__MAGIC ) {

	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bad ctlblk magic")

    } else if ( family_ctlblk_ptr->num_subctlblks < 1 ) {


	HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                     "empty control block on entry.")
    }

    i = 0;

    while ( i < family_ctlblk_ptr->num_subctlblks ) {

        if ( (family_ctlblk_ptr->subctlblks[i]).magic != 
	     H5FD_FAMILY_AIO_SUBCTLBLK_T__MAGIC ) {

	    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, \
                        "bad subctlblk magic")
        }

        tgt_file      = (family_ctlblk_ptr->subctlblks[i]).driver;
        subctlblk_ptr = (family_ctlblk_ptr->subctlblks[i]).ctlblk;

        if ( ( tgt_file == NULL ) ||
             ( subctlblk_ptr == NULL ) ) {

	    HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                        "NULL tgt file or sub ctl blk.")
        }

        if ( (family_ctlblk_ptr->subctlblks[i]).finished ) {

	    HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                        "sub op already finished?!?!")
        }

        if ( ! ((family_ctlblk_ptr->subctlblks[i]).done) ) {

            already_done = FALSE;

            result = H5FDaio_wait(tgt_file, subctlblk_ptr);

            if ( result < 0 ) {

	        HGOTO_ERROR(H5E_VFL, H5E_AIOTESTFAIL, FAIL, \
                            "aio wait sub-request failed")
            }

            (family_ctlblk_ptr->subctlblks[i]).done = TRUE;
        }

        i++;
    }

    if ( already_done ) {

        HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                    "operation already done?!?!?")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_family_aio_wait() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_aio_finish
 *
 * Purpose:	Determine whether the read, write, or fsync operation 
 *		indicated by *ctlblk_ptr completed successfully.  If it 
 *		did, set *errno_ptr to 0.  If if didn't, set *errno_ptr 
 *		to the appropriate error code.
 *
 *		Return SUCCEED if successful, and the appropriate error 
 *		code if not.
 *
 *		Note that the returned error code only refers to the 
 *		success or failure of the finish operation.  The caller 
 *		must examine *errno_ptr to determine if the underlying 
 *		asynchronous operation succeeded.
 *
 *		Note that all calls to the underlying files will be 
 *		through the public H5FD interface, which will fake 
 *		AIO if it is not supported.  Thus, this function 
 *		presumes that AIO is supported.
 *
 *		However, it is possible that multiple underlying 
 *		files may be involved.
 *
 *		For each underlying file / driver listed in the control
 *		block, first check the control block particular to the 
 *		file / driver pair and verify that it is listed as being
 *		done but not finished.  Then call the associated aio 
 *		finish function for each file -- set *errno_ptr to 0
 *		if no errors are detected, and to the last non-zero
 *		errno reported if any error is reported.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              6/12/10
 *
 *-------------------------------------------------------------------------
 */

static herr_t 
H5FD_family_aio_finish(int *errno_ptr, 
                       void *ctlblk_ptr)
{
    herr_t                      ret_value = SUCCEED;  /* Return value */
    herr_t			result;
    int                         i;
    int				error_num = 0;
    int				sub_errno;
    H5FD_family_aio_ctlblk_t  * family_ctlblk_ptr;
    H5FD_t                    * tgt_file;
    void                      * subctlblk_ptr;

    FUNC_ENTER_NOAPI(H5FD_family_aio_finish, FAIL)

    if ( ( errno_ptr == NULL ) ||
         ( ctlblk_ptr == NULL ) ) {

	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bad arg(s) on entry")

    } 

    family_ctlblk_ptr = (H5FD_family_aio_ctlblk_t *)ctlblk_ptr;

    if ( family_ctlblk_ptr->magic != H5FD_FAMILY_AIO_CTLBLK_T__MAGIC ) {

	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bad ctlblk magic")

    } else if ( family_ctlblk_ptr->num_subctlblks < 1 ) {


	HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                     "empty control block on entry.")
    }


    /* finish all the sub operations */

    i = 0;
    while ( i < family_ctlblk_ptr->num_subctlblks ) {

        if ( (family_ctlblk_ptr->subctlblks[i]).magic != 
	     H5FD_FAMILY_AIO_SUBCTLBLK_T__MAGIC ) {

	    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, \
                        "bad subctlblk magic")
        }

        tgt_file      = (family_ctlblk_ptr->subctlblks[i]).driver;
        subctlblk_ptr = (family_ctlblk_ptr->subctlblks[i]).ctlblk;

        if ( ( tgt_file == NULL ) ||
             ( subctlblk_ptr == NULL ) ) {

	    HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                        "NULL tgt file or sub ctl blk.")
        }

        if ( ! ((family_ctlblk_ptr->subctlblks[i]).done) ) {

	    HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                        "sub op not done?!?!")
        }

        if ( (family_ctlblk_ptr->subctlblks[i]).finished ) {

	    HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                        "sub op already finished?!?!")
        }

        sub_errno = 0;

        result = H5FDaio_finish(tgt_file, &sub_errno, subctlblk_ptr);

        if ( result < 0 ) {

	    HGOTO_ERROR(H5E_VFL, H5E_AIOTESTFAIL, FAIL, \
                        "aio finish sub-request failed")
        }

        if ( sub_errno != 0 ) {

            error_num = sub_errno;
        }

        (family_ctlblk_ptr->subctlblks[i]).finished = TRUE;

        i++;
    }

    /* discard the control block */

    result = H5FD_family_aio_discard_ctlblk(family_ctlblk_ptr);

    if ( result < 0 ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                    "Attempt to discard control block failed")
    }

    *errno_ptr = error_num;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_family_aio_finish() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_aio_fsync
 *
 * Purpose:	Queue a sync of all asynchronous writes outstanding as of 
 *		the time this function is called.  Return zero if no 
 *		errors are encountered, but note that a good error return 
 *		from H5FD_family_aio_fsync() does not imply a successful 
 *		operation, only that no immediate errors were detected.
 *
 *		The sync is not known to be successful until reported 
 *		complete by either H5FD_family_aio_test or 
 *		H5FD_family_aio_wait, and reported successful by 
 *		H5FD_family_aio_finish.
 *
 *		Note that all calls to the underlying files will be 
 *		through the public H5FD interface, which will fake 
 *		AIO if it is not supported.  Thus, this function 
 *		presumes that AIO is supported.
 *
 *		To implement the aio_fsync, we must send an aio fsync
 *		to each of the underlying files, construct a control
 *		block containing pointers to all the sub control block
 *		returned, and return the constructed control block 
 *		to the users.
 *
 *		To do this, first allocate a control block with sufficient
 *		space to contain references to all underlying files.
 *
 *		Then, pass the aio_fsync to each underlying file, 
 *		recording the pointer to the associated instance of 
 *		H5FD_t and the returned aio control block for each 
 *		underlying file in the family file aio control block.
 *
 *		If all calls complete without error, return the 
 *		address of the control block in *ctlblk_ptr_ptr.
 *
 *		If any errors are detected, simply fail.
 *		
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              6/12/10
 *
 *-------------------------------------------------------------------------
 */

static herr_t 
H5FD_family_aio_fsync(H5FD_t *file, 
                      void **ctlblk_ptr_ptr)
{
    herr_t                     ret_value = SUCCEED;  /* Return value */
    herr_t	 	       result;
    hbool_t                    success = FALSE;
    int                        i;
    int		 	       num_sub_files;
    H5FD_family_t            * family_file;
    H5FD_t                   * tgt_file;
    H5FD_family_aio_ctlblk_t * ctlblk_ptr = NULL;
    void                     * subctlblk_ptr;

    FUNC_ENTER_NOAPI(H5FD_family_aio_finish, FAIL)

    if ( ( file == NULL ) ||
         ( file->cls == NULL ) ||
         ( ctlblk_ptr_ptr == NULL ) ||
         ( *ctlblk_ptr_ptr != NULL ) ) {

	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bad arg(s) on entry")

    } 

    family_file = (H5FD_family_t *)file;

    num_sub_files = family_file->nmembs;

    /* allocate an aio control block large enough to contain refereces to
     * all the members of the family file.
     */
    result = H5FD_family_aio_alloc_ctlblk(num_sub_files, &ctlblk_ptr);

    if ( result != SUCCEED ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "can't allocate aio control block")

    } else if ( ( ctlblk_ptr == NULL ) ||
                ( ctlblk_ptr->magic != H5FD_FAMILY_AIO_CTLBLK_T__MAGIC ) ) {

        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                    "NULL ctlblk_ptr or bad ctlblk magic")

    } else if ( ( ctlblk_ptr->array_len != num_sub_files ) ||
                ( ctlblk_ptr->num_subctlblks != 0 ) ||
                ( ctlblk_ptr->subctlblks == NULL ) ||
                ( (ctlblk_ptr->subctlblks)[0].magic != 
                  H5FD_FAMILY_AIO_SUBCTLBLK_T__MAGIC ) ) {

        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                    "bad sub control block array")
    }

    for ( i = i; i < num_sub_files; i++ ) {

        if ( (ctlblk_ptr->subctlblks[i]).magic != 
	     H5FD_FAMILY_AIO_SUBCTLBLK_T__MAGIC ) {

	    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, \
                        "bad subctlblk magic")
        }

        tgt_file      = family_file->memb[i];
        subctlblk_ptr = NULL;

        result = H5FDaio_fsync(tgt_file, &subctlblk_ptr);

        if ( result < 0 ) {

            HGOTO_ERROR(H5E_VFL, H5E_AIOSYNCFAIL, FAIL, \
                        "sub aio fsync request failed.")
        }

        (ctlblk_ptr->subctlblks[i]).driver   = tgt_file;
        (ctlblk_ptr->subctlblks[i]).ctlblk   = subctlblk_ptr;
        (ctlblk_ptr->subctlblks[i]).done     = FALSE;
        (ctlblk_ptr->subctlblks[i]).finished = FALSE;

        (ctlblk_ptr->num_subctlblks)++;

        HDassert( ctlblk_ptr->num_subctlblks <= ctlblk_ptr->array_len );
    }

    HDassert( ctlblk_ptr->num_subctlblks == ctlblk_ptr->array_len );

    *ctlblk_ptr_ptr = (void *)ctlblk_ptr;

    success = TRUE;

done:

    /* discard the control block if not successful */
    if ( ( ctlblk_ptr != NULL ) && ( ! success ) ) {

        result = H5FD_family_aio_discard_ctlblk(ctlblk_ptr);

        if ( result != SUCCEED ) {

            HDONE_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "ctlblk de-allocation failed")
        }

        ctlblk_ptr = NULL;
    }

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_family_aio_fsync() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_fmily_aio_cancel
 *
 * Purpose:	Attempt to cancel the asynchronous operation associated 
 *		with the control block pointed to by ctlblk_ptr.  
 *
 *		Note that this operation may have completed, but it is 
 *		an error if H5FD_family_aio_finish() has been called on 
 *		it.
 *
 *		As part of the cancel, free the associated control blocks.
 *
 *		Return SUCCEED if successful, and the appropriate error 
 *		code otherwise.
 *
 *		Note that all calls to the underlying files will be 
 *		through the public H5FD interface, which will fake 
 *		AIO if it is not supported.  Thus, this function 
 *		presumes that AIO is supported.
 *
 *		However, it is possible that multiple underlying 
 *		files may be involved.
 *
 *		For each underlying file / driver listed in the control
 *		block, first check the control block particular to the 
 *		file / driver pair and verify that it is not listed as 
 *		being finished.  Then call the associated aio cancel 
 *		function for each file.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              6/12/10
 *
 *-------------------------------------------------------------------------
 */

static herr_t 
H5FD_family_aio_cancel(void *ctlblk_ptr)
{
    herr_t                       ret_value = SUCCEED;  /* Return value */
    herr_t			 result;
    int                          i;
    H5FD_family_aio_ctlblk_t   * family_ctlblk_ptr;
    H5FD_t                     * tgt_file;
    void                       * subctlblk_ptr;

    FUNC_ENTER_NOAPI(H5FD_family_aio_cancel, FAIL)

    if ( ctlblk_ptr == NULL ) {

	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL ctlblk_ptr on entry")

    } 

    family_ctlblk_ptr = (H5FD_family_aio_ctlblk_t *)ctlblk_ptr;

    if ( family_ctlblk_ptr->magic != H5FD_FAMILY_AIO_CTLBLK_T__MAGIC ) {

	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bad ctlblk magic")

    } else if ( family_ctlblk_ptr->num_subctlblks < 1 ) {


	HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                     "empty control block on entry.")
    }

    i = 0;

    while ( i < family_ctlblk_ptr->num_subctlblks ) {

        if ( (family_ctlblk_ptr->subctlblks[i]).magic != 
	     H5FD_FAMILY_AIO_SUBCTLBLK_T__MAGIC ) {

	    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, \
                        "bad subctlblk magic")
        }

        tgt_file      = (family_ctlblk_ptr->subctlblks[i]).driver;
        subctlblk_ptr = (family_ctlblk_ptr->subctlblks[i]).ctlblk;

        if ( ( tgt_file == NULL ) ||
             ( subctlblk_ptr == NULL ) ) {

	    HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                        "NULL tgt file or sub ctl blk.")
        }

        if ( (family_ctlblk_ptr->subctlblks[i]).finished ) {

	    HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                        "sub op already finished?!?!")
        }

        result = H5FDaio_cancel(tgt_file, subctlblk_ptr);

        if ( result < 0 ) {

	    HGOTO_ERROR(H5E_VFL, H5E_AIOTESTFAIL, FAIL, \
                        "aio cance sub-request failed")
        }

	(family_ctlblk_ptr->subctlblks[i]).driver   = NULL;
	(family_ctlblk_ptr->subctlblks[i]).ctlblk   = NULL;
	(family_ctlblk_ptr->subctlblks[i]).done     = FALSE;
	(family_ctlblk_ptr->subctlblks[i]).finished = FALSE;

        i++;
    }

    /* discard the control block */

    result = H5FD_family_aio_discard_ctlblk(family_ctlblk_ptr);

    if ( result < 0 ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                    "Attempt to discard control block failed")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_family_aio_cancel() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_fsync
 *
 * Purpose:	Sync out all underlying files.
 *		
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              6/12/10
 *
 *-------------------------------------------------------------------------
 */

static herr_t 
H5FD_family_fsync(H5FD_t *file, 
		  hid_t dxpl_id)
{
    herr_t                     ret_value = SUCCEED;  /* Return value */
    herr_t	 	       result;
    int                        i;
    int		 	       num_sub_files;
    H5FD_family_t            * family_file;
    H5FD_t                   * tgt_file;

    FUNC_ENTER_NOAPI(H5FD_family_fsync, FAIL)

    if ( file == NULL ) {

	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bad arg(s) on entry")

    } 

    family_file = (H5FD_family_t *)file;

    num_sub_files = family_file->nmembs;

    for ( i = i; i < num_sub_files; i++ ) {

        tgt_file = family_file->memb[i];

	result = H5FDfsync(tgt_file, dxpl_id);

        if ( result < 0 ) {

            HGOTO_ERROR(H5E_VFL, H5E_SYNCFAIL, FAIL, \
                        "family sub fsync request failed.")
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_family_fsync() */
