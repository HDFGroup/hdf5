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
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.org>
 *              Septemeber 11, 2012
 *
 * Purpose:	This is the MDS driver.
 */

#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */
#define H5P_PACKAGE		/*suppress error about including H5Ppkg	  */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5FD_mds_init_interface


#include "H5private.h"		/* Generic Functions			*/
#include "H5Dprivate.h"		/* Dataset functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"		/* File access				*/
#include "H5Fpkg.h"             /* File pkg                             */
#include "H5FDprivate.h"	/* File drivers				*/
#include "H5FDmpi.h"            /* MPI-based file drivers		*/
#include "H5FDmds.h"            /* MDS file driver        		*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"         /* Property lists                       */
#include "H5Ppkg.h"             /* Property lists                       */
#include "H5VLmdserver.h"       /* MDS helper routines			*/

#ifdef H5_HAVE_PARALLEL

/*
 * The driver identification number, initialized at runtime if H5_HAVE_PARALLEL
 * is defined. This allows applications to still have the H5FD_MDS
 * "constants" in their source code.
 */
static hid_t H5FD_MDS_g = 0;

/*
 * The description of a file belonging to this driver.
 * The EOF value is only used just after the file is opened in order for the
 * library to determine whether the file is empty, truncated, or okay. The MDS
 * driver doesn't bother to keep it updated since it's an expensive operation.
 */
typedef struct H5FD_mds_t {
    H5FD_t	pub;		/*public stuff, must be first		*/
    haddr_t     raw_eoa; /*EOA for individual files   */
} H5FD_mds_t;

/* Callbacks */
static herr_t H5FD_mds_term(void);
static H5FD_t *H5FD_mds_open(const char *name, unsigned flags, hid_t fapl_id,
			      haddr_t maxaddr);
static herr_t H5FD_mds_close(H5FD_t *_file);
static herr_t H5FD_mds_query(const H5FD_t *_f1, unsigned long *flags);
static haddr_t H5FD_mds_get_eoa(const H5FD_t *_file, H5FD_mem_t type);
static herr_t H5FD_mds_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t addr);
static haddr_t H5FD_mds_get_eof(const H5FD_t *_file);
static herr_t H5FD_mds_read(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr,
            size_t size, void *buf);
static herr_t H5FD_mds_write(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr,
            size_t size, const void *buf);
static herr_t H5FD_mds_flush(H5FD_t *_file, hid_t dxpl_id, unsigned closing);
static herr_t H5FD_mds_truncate(H5FD_t *_file, hid_t dxpl_id, hbool_t closing);


/* The MDS file driver information */
static const H5FD_class_t H5FD_mds_g = {
    "mds",					/*name			*/
    HADDR_MAX,					/*maxaddr		*/
    H5F_CLOSE_SEMI,				/*fc_degree		*/
    H5FD_mds_term,                              /*terminate             */
    NULL,					/*sb_size		*/
    NULL,					/*sb_encode		*/
    NULL,					/*sb_decode		*/
    0,			                        /*fapl_size		*/
    NULL,				        /*fapl_get		*/
    NULL,			                /*fapl_copy		*/
    NULL, 			                /*fapl_free		*/
    0,		                		/*dxpl_size		*/
    NULL,					/*dxpl_copy		*/
    NULL,					/*dxpl_free		*/
    H5FD_mds_open,				/*open			*/
    H5FD_mds_close,				/*close			*/
    NULL,					/*cmp			*/
    H5FD_mds_query,		                /*query			*/
    NULL,					/*get_type_map		*/
    NULL,					/*alloc			*/
    NULL,					/*free			*/
    H5FD_mds_get_eoa,				/*get_eoa		*/
    H5FD_mds_set_eoa, 				/*set_eoa		*/
    H5FD_mds_get_eof,     			/*get_eof		*/
    NULL,                                       /*get_handle            */
    H5FD_mds_read,				/*read			*/
    H5FD_mds_write,				/*write			*/
    H5FD_mds_flush,				/*flush			*/
    H5FD_mds_truncate,				/*truncate		*/
    NULL,                                       /*lock                  */
    NULL,                                       /*unlock                */
    H5FD_FLMAP_SINGLE                           /*fl_map                */
};


/*--------------------------------------------------------------------------
NAME
   H5FD_mds_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5FD_mds_init_interface()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5FD_mds_init currently).

--------------------------------------------------------------------------*/
static herr_t
H5FD_mds_init_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(H5FD_mds_init())
} /* H5FD_mds_init_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_mds_init
 *
 * Purpose:	Initialize this driver by registering the driver with the
 *		library.
 *
 * Return:	Success:	The driver ID for the mds driver.
 *
 *		Failure:	Negative.
 *
 * Programmer:	Mohamad Chaarawi
 *              September, 2012
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5FD_mds_init(void)
{
    hid_t ret_value;        	/* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if (H5I_VFL!=H5I_get_type(H5FD_MDS_g))
        H5FD_MDS_g = H5FD_register((const H5FD_class_t *)&H5FD_mds_g,sizeof(H5FD_class_mpi_t),FALSE);

    /* Set return value */
    ret_value=H5FD_MDS_g;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*---------------------------------------------------------------------------
 * Function:	H5FD_mds_term
 *
 * Purpose:	Shut down the VFD
 *
 * Returns:     Non-negative on success or negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              September, 2012
 *
 *---------------------------------------------------------------------------
 */
static herr_t
H5FD_mds_term(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Reset VFL ID */
    H5FD_MDS_g=0;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD_mds_term() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_fapl_mds
 *
 * Purpose:	
 *
 * Return:	Success:	Non-negative
 *
 * 		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              September, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5P_set_fapl_mds(hid_t fapl_id)
{
    H5P_genplist_t      *plist;      /* Property list pointer */
    herr_t              ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    if(fapl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a file access list")

    ret_value= H5P_set_driver(plist, H5FD_MDS, NULL);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5P_set_fapl_mds */


/*-------------------------------------------------------------------------
 * Function:    H5FD_mds_open
 *
 * Purpose:     Opens the metadata file that is controlled by the MDS process.
 *
 * Return:      Success:        A new file pointer.
 *
 *              Failure:        NULL
 *
 * Programmer:	Mohamad Chaarawi
 *              September, 2012
 *
 *-------------------------------------------------------------------------
 */
static H5FD_t *
H5FD_mds_open(const char UNUSED *name, unsigned UNUSED flags, hid_t UNUSED fapl_id,
              haddr_t UNUSED maxaddr)
{
    H5FD_mds_t	       	*file=NULL;
    H5FD_t		*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (file = (H5FD_mds_t *)calloc((size_t)1, sizeof(H5FD_mds_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    file->raw_eoa = 0;

    /* Set return value */
    ret_value=(H5FD_t*)file;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:    H5FD_mds_close
 *
 * Purpose:     Closes the metadata file
 *
 * Return:      Success:	Non-negative
 *
 * 		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              September, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mds_close(H5FD_t *_file)
{
    H5FD_mds_t	*file = (H5FD_mds_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    free(file);

    FUNC_LEAVE_NOAPI(SUCCEED)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mds_query
 *
 * Purpose:	Set the flags that this VFL driver is capable of supporting.
 *              (listed in H5FDpublic.h)
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              September, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mds_query(const H5FD_t UNUSED *_file, unsigned long *flags /* out */)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Set the VFL feature flags that this driver supports */
    if(flags) {
        *flags = 0;
        *flags |= H5FD_FEAT_AGGREGATE_METADATA;     /* OK to aggregate metadata allocations                             */
        *flags |= H5FD_FEAT_ACCUMULATE_METADATA;    /* OK to accumulate metadata for faster writes                      */
        *flags |= H5FD_FEAT_ALLOCATE_EARLY;           /* Allocate space early instead of late */
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mds_get_eoa
 *
 * Purpose:	Gets the end-of-address marker for the file. The EOA marker
 *		is the first address past the last byte allocated in the
 *		format address space.
 *
 * Return:	Success:	The end-of-address marker.
 *
 *		Failure:	HADDR_UNDEF
 *
 * Programmer:	Mohamad Chaarawi
 *              September, 2012
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_mds_get_eoa(const H5FD_t *_file, H5FD_mem_t type)
{
    const H5FD_mds_t	*file = (const H5FD_mds_t*)_file;
    haddr_t ret_value = HADDR_UNDEF;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(H5FD_MEM_DRAW == type); 

    ret_value = file->raw_eoa;
    HDassert(HADDR_UNDEF != file->raw_eoa);

    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mds_set_eoa
 *
 * Purpose:	Set the end-of-address marker for the file. This function is
 *		called shortly after an existing HDF5 file is opened in order
 *		to tell the driver where the end of the HDF5 data is located.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Mohamad Chaarawi
 *              September, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mds_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t eoa)
{
    H5FD_mds_t	*file = (H5FD_mds_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(H5FD_MEM_DRAW == type); 

    file->raw_eoa = eoa;
    FUNC_LEAVE_NOAPI(SUCCEED)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mds_get_eof
 *
 * Purpose:	Gets the end-of-file marker for the file. The EOF marker
 *		is the real size of the file.
 *
 * Return:	Success:	The end-of-address marker.
 *
 *		Failure:	HADDR_UNDEF
 *
 * Programmer:	Mohamad Chaarawi
 *              September, 2012
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_mds_get_eof(const H5FD_t UNUSED *_file)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR
    FUNC_LEAVE_NOAPI(0)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mds_read
 *
 * Purpose:	Read metadata from the metadata file. call the underlying 
 *              VFD member read . If this is raw data, return an error.
 *
 * Return:	Success:	Zero. Result is stored in caller-supplied
 *				buffer BUF.
 *
 *		Failure:	-1, Contents of buffer BUF are undefined.
 *
 * Programmer:	Mohamad Chaarawi
 *              September, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mds_read(H5FD_t UNUSED *_file, H5FD_mem_t UNUSED type, hid_t UNUSED dxpl_id, haddr_t UNUSED addr, 
              size_t UNUSED size, void UNUSED *buf)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR
    printf("READ -- should not be here\n");
    FUNC_LEAVE_NOAPI(FAIL)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mds_write
 *
 * Purpose:	Writes metadata to the metadata file. Calls the underlying
 *              write VFD call.
 *
 * Return:	Success:	Zero
 *
 *		Failure:	-1
 *
 * Programmer:	Mohamad Chaarawi
 *              September, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mds_write(H5FD_t UNUSED *_file, H5FD_mem_t UNUSED type, hid_t UNUSED dxpl_id, haddr_t UNUSED addr,
		size_t UNUSED size, const void UNUSED *buf)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR
    printf("WRITE -- should not be here\n");
    FUNC_LEAVE_NOAPI(FAIL)
} /* end H5FD_mds_write() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_mds_flush
 *
 * Purpose:     Makes sure that all data is on disk.  This is collective.
 *
 * Return:      Success:	Non-negative
 *
 * 		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              September, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mds_flush(H5FD_t UNUSED *_file, hid_t UNUSED dxpl_id, unsigned UNUSED closing)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR
    FUNC_LEAVE_NOAPI(SUCCEED)
}


/*-------------------------------------------------------------------------
 * Function:    H5FD_mds_truncate
 *
 * Purpose:     Make certain the file's size matches it's allocated size
 *
 * Return:      Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:  Quincey Koziol
 *              January 31, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mds_truncate(H5FD_t UNUSED *_file, hid_t UNUSED dxpl_id, hbool_t UNUSED closing)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR
    FUNC_LEAVE_NOAPI(SUCCEED)
}

#endif /* H5_HAVE_PARALLEL */
