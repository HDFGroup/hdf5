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
 * Purpose:	This is the MDC driver.
 */

#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */
#define H5P_PACKAGE		/*suppress error about including H5Ppkg	  */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5FD_mdc_init_interface


#include "H5private.h"		/* Generic Functions			*/
#include "H5Dprivate.h"		/* Dataset functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"		/* File access				*/
#include "H5FDprivate.h"	/* File drivers				*/
#include "H5Fpkg.h"             /* File pkg                             */
#include "H5FDmpi.h"            /* MPI-based file drivers		*/
#include "H5FDmdc.h"            /* MDC file driver       		*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MDprivate.h"	/* MDS server private			*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"         /* Property lists                       */
#include "H5Ppkg.h"             /* Property lists                       */

#ifdef H5_HAVE_PARALLEL

/*
 * The driver identification number, initialized at runtime if H5_HAVE_PARALLEL
 * is defined. This allows applications to still have the H5FD_MDC
 * "constants" in their source code.
 */
static hid_t H5FD_MDC_g = 0;

/*
 * The description of a file belonging to this driver.
 * The EOF value is only used just after the file is opened in order for the
 * library to determine whether the file is empty, truncated, or okay. The MDC
 * driver doesn't bother to keep it updated since it's an expensive operation.
 */
typedef struct H5FD_mdc_t {
    H5FD_t	pub;		/*public stuff, must be first		*/
    H5FD_t     *memb;	        /*member pointer         		*/
    hid_t       mdfile_id;      /* file id of the metadata file created by the MDS */
} H5FD_mdc_t;

/* MDC specific file access properties */
typedef struct H5FD_mdc_fapl_t {
    hid_t	memb_fapl;                 /* underlying fapl    	*/
    char       *memb_name;                 /* metadata file name	*/
    haddr_t	memb_addr;                 /* starting addr     	*/
    hid_t       mdfile_id;                 /* metadata file ID          */
} H5FD_mdc_fapl_t;

/* Private Prototypes */

/* Callbacks */
static herr_t H5FD_mdc_term(void);
static void *H5FD_get_fapl_mdc(H5FD_t *_file);
static void *H5FD_mdc_fapl_copy(const void *_old_fa);
static herr_t H5FD_mdc_fapl_free(void *_fa);
static H5FD_t *H5FD_mdc_open(const char *name, unsigned flags, hid_t fapl_id, haddr_t maxaddr);
static herr_t H5FD_mdc_close(H5FD_t *_file);
static herr_t H5FD_mdc_query(const H5FD_t *_f1, unsigned long *flags);
static haddr_t H5FD_mdc_alloc(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, hsize_t size);
static haddr_t H5FD_mdc_get_eoa(const H5FD_t *_file, H5FD_mem_t type);
static herr_t H5FD_mdc_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t addr);
static haddr_t H5FD_mdc_get_eof(const H5FD_t *_file);
static herr_t H5FD_mdc_read(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr,
                            size_t size, void *buf);
static herr_t H5FD_mdc_write(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr,
                             size_t size, const void *buf);
static herr_t H5FD_mdc_flush(H5FD_t *_file, hid_t dxpl_id, unsigned closing);
static herr_t H5FD_mdc_truncate(H5FD_t *_file, hid_t dxpl_id, hbool_t closing);
static int H5FD_mdc_mpi_rank(const H5FD_t *_file);
static int H5FD_mdc_mpi_size(const H5FD_t *_file);
static int H5FD_mdc_communicator(const H5FD_t *_file);

/* The MDC file driver information */
static const H5FD_class_mpi_t H5FD_mdc_g = {
    {  /* Start of superclass information */
    "mdc",					/*name			*/
    HADDR_MAX,					/*maxaddr		*/
    H5F_CLOSE_SEMI,				/* fc_degree		*/
    H5FD_mdc_term,                             /*terminate             */
    NULL,					/*sb_size		*/
    NULL,					/*sb_encode		*/
    NULL,					/*sb_decode		*/
    sizeof(H5FD_mdc_fapl_t),			/*fapl_size		*/
    H5FD_get_fapl_mdc,				/*fapl_get		*/
    H5FD_mdc_fapl_copy,     			/*fapl_copy		*/
    H5FD_mdc_fapl_free, 			/*fapl_free		*/
    0,		                		/*dxpl_size		*/
    NULL,					/*dxpl_copy		*/
    NULL,					/*dxpl_free		*/
    H5FD_mdc_open,				/*open			*/
    H5FD_mdc_close,				/*close			*/
    NULL,					/*cmp			*/
    H5FD_mdc_query,		                /*query			*/
    NULL,					/*get_type_map		*/
    H5FD_mdc_alloc,				/*alloc			*/
    NULL,					/*free			*/
    H5FD_mdc_get_eoa,				/*get_eoa		*/
    H5FD_mdc_set_eoa, 				/*set_eoa		*/
    H5FD_mdc_get_eof,  				/*get_eof		*/
    NULL,                                       /*get_handle            */
    H5FD_mdc_read,				/*read			*/
    H5FD_mdc_write,				/*write			*/
    H5FD_mdc_flush,				/*flush			*/
    H5FD_mdc_truncate,				/*truncate		*/
    NULL,                                       /*lock                  */
    NULL,                                       /*unlock                */
    H5FD_FLMAP_SINGLE                           /*fl_map                */
    },
    H5FD_mdc_mpi_rank,         /* get_rank         */
    H5FD_mdc_mpi_size,         /* get_size         */
    H5FD_mdc_communicator      /* get_comm         */
};


/*--------------------------------------------------------------------------
NAME
   H5FD_mdc_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5FD_mdc_init_interface()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5FD_mdc_init currently).

--------------------------------------------------------------------------*/
static herr_t
H5FD_mdc_init_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(H5FD_mdc_init())
} /* H5FD_mdc_init_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_mdc_init
 *
 * Purpose:	Initialize this driver by registering the driver with the
 *		library.
 *
 * Return:	Success:	The driver ID for the mdc driver.
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
H5FD_mdc_init(void)
{
    hid_t ret_value;        	/* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if (H5I_VFL!=H5I_get_type(H5FD_MDC_g))
        H5FD_MDC_g = H5FD_register((const H5FD_class_t *)&H5FD_mdc_g,sizeof(H5FD_class_mpi_t),FALSE);

    /* Set return value */
    ret_value=H5FD_MDC_g;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*---------------------------------------------------------------------------
 * Function:	H5FD_mdc_term
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
H5FD_mdc_term(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Reset VFL ID */
    H5FD_MDC_g=0;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD_mdc_term() */


/*---------------------------------------------------------------------------
 * Function:	H5FD_mdc_set_mdfile
 *
 * Purpose:	Set the mds file ID in the MDC file struct. This is called from
 *              H5VL_mds_file_create.
 *
 * Returns:     Non-negative on success or negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              September, 2012
 *
 *---------------------------------------------------------------------------
 */
herr_t 
H5FD_mdc_set_mdfile(H5F_t *file, hid_t mdfile_id)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    ((H5FD_mdc_t *)(file->shared->lf))->mdfile_id = mdfile_id;

    FUNC_LEAVE_NOAPI(SUCCEED)
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_fapl_mdc
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
H5P_set_fapl_mdc(hid_t fapl_id, const char *name, hid_t plist_id, hid_t mdfile_id)
{
    H5FD_mdc_fapl_t	fa;
    H5P_genplist_t      *plist;      /* Property list pointer */
    herr_t              ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    if(fapl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a file access list")

    if(H5P_DEFAULT != plist_id)
        fa.memb_fapl = plist_id;
    else
        fa.memb_fapl = H5Pcreate(H5P_FILE_ACCESS);
    fa.memb_name = H5MM_strdup(name);
    fa.memb_addr = 0;
    fa.mdfile_id = mdfile_id;

    ret_value= H5P_set_driver(plist, H5FD_MDC, &fa);

    H5MM_free(fa.memb_name);
done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_get_fapl_mdc
 *
 * Purpose:	Returns a file access property list which indicates how the
 *		specified file is being accessed. The return list could be
 *		used to access another file the same way.
 *
 * Return:	Success:	Ptr to new file access property list with all
 *				members copied from the file struct.
 *
 *		Failure:	NULL
 *
 * Programmer:	Mohamad Chaarawi
 *              September, 2012
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5FD_get_fapl_mdc(H5FD_t *_file)
{
    H5FD_mdc_t	*file = (H5FD_mdc_t*)_file;
    H5FD_mdc_fapl_t *fa = NULL;
    void *ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file);
    HDassert(H5FD_MDC == file->pub.driver_id);

    if(NULL == (fa = (H5FD_mdc_fapl_t *)H5MM_calloc(sizeof(H5FD_mdc_fapl_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* MSC - Need to think more about that */
    
    /* Set return value */
    ret_value = (void *)fa;

done:
    //FUNC_LEAVE_NOAPI(H5FD_mdc_fapl_copy(&(file->fa)))
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mdc_fapl_copy
 *
 * Purpose:	Copies the mdc-specific file access properties.
 *
 * Return:	Success:	Ptr to a new property list
 *
 *		Failure:	NULL
 *
 * Programmer:	Mohamad Chaarawi
 *              September, 2012
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5FD_mdc_fapl_copy(const void *_old_fa)
{
    const H5FD_mdc_fapl_t *old_fa = (const H5FD_mdc_fapl_t*)_old_fa;
    H5FD_mdc_fapl_t *new_fa = (H5FD_mdc_fapl_t *)malloc(sizeof(H5FD_mdc_fapl_t));
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == new_fa)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    memcpy(new_fa, old_fa, sizeof(H5FD_mdc_fapl_t));

    if (old_fa->memb_fapl >= 0) {
        new_fa->memb_fapl = H5Pcopy(old_fa->memb_fapl);
        if (new_fa->memb_fapl<0) 
            HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, NULL, "can't copy plist") 
    }
    if (old_fa->memb_name) {
        if(NULL == (new_fa->memb_name = (char *)malloc(strlen(old_fa->memb_name)+1)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
        strcpy(new_fa->memb_name, old_fa->memb_name);
    }
    new_fa->mdfile_id = old_fa->mdfile_id;

    ret_value = (void *)new_fa;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mdc_fapl_free
 *
 * Purpose:	Frees the mdc-specific file access properties.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Mohamad Chaarawi
 *              September, 2012
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mdc_fapl_free(void *_fa)
{
    H5FD_mdc_fapl_t	*fa = (H5FD_mdc_fapl_t*)_fa;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT
    if (fa->memb_fapl >= 0)
        if(H5Pclose(fa->memb_fapl)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTCLOSEOBJ, FAIL, "can't close plist") 
    if (fa->memb_name)
        free(fa->memb_name);

    free(fa);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:    H5FD_mdc_open
 *
 * Purpose:     Opens the raw data file of the client(s). This is collective.
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
H5FD_mdc_open(const char UNUSED *name, unsigned flags, hid_t fapl_id,
              haddr_t UNUSED maxaddr)
{
    H5FD_mdc_fapl_t     *fa;
    H5FD_mdc_t	        *file=NULL;
    H5FD_t		*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (file = (H5FD_mdc_t *)calloc((size_t)1, sizeof(H5FD_mdc_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    fa = (H5FD_mdc_fapl_t *)H5Pget_driver_info(fapl_id);

    /* set the metadata file id in the FD struct */
    file->mdfile_id = fa->mdfile_id;

    /* open the file from the underlying VFD */
    file->memb = H5FDopen(fa->memb_name, flags, fa->memb_fapl, HADDR_UNDEF);
    if (!file->memb)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "error opening member file")

    /* Set return value */
    ret_value=(H5FD_t*)file;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:    H5FD_mdc_close
 *
 * Purpose:     Closes the raw data file. This is collective.
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
H5FD_mdc_close(H5FD_t *_file)
{
    H5FD_mdc_t	*file = (H5FD_mdc_t*)_file;
    MPI_Comm    comm;
    int         my_rank;
    herr_t      ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* get the communicator of the file */
    comm = H5FD_mpi_get_comm(file->memb);
    /* get the rank of the process */
    my_rank = H5FD_mpi_get_rank(file->memb);

    /* Get all processes that opened the file here so they all have sent their requests to the
       MDS already so that one process would not terminate the MDS while other processes have
       more work to do before closing */
    MPI_Barrier(comm);

    /* the first process in the communicator will tell the MDS process to close the metadata file */
    if (0 == my_rank) {
        size_t buf_size;
        void *send_buf = NULL;

        buf_size = 1 /* request type */ + sizeof(int) /* metadata file id */;

        /* allocate the buffer for encoding the parameters */
        if(NULL == (send_buf = H5MM_malloc(buf_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

        /* encode file close params */
        if(H5VL__encode_file_close_params(send_buf, &buf_size, file->mdfile_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to encode file close params")

        MPI_Pcontrol(0);
        /* send the request to the MDS process and recieve the metadata file ID */
        if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                       &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                       MPI_COMM_WORLD, MPI_STATUS_IGNORE))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message")
        MPI_Pcontrol(1);
        H5MM_free(send_buf);
    }

    if (file->memb)
        if (H5FDclose(file->memb) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTCLOSEFILE, FAIL, "can't close member file")

                /*
    if (file->fa.memb_fapl >= 0)
        if(H5Pclose(file->fa.memb_fapl) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTCLOSEOBJ, FAIL, "can't close plist") 
    if (file->fa.memb_name) 
        free(file->fa.memb_name);
                */
    free(file);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mdc_query
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
H5FD_mdc_query(const H5FD_t UNUSED*_file, unsigned long *flags /* out */)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Set the VFL feature flags that this driver supports */
    if(flags) {
        *flags=0;
        *flags|=H5FD_FEAT_AGGREGATE_METADATA;  /* OK to aggregate metadata allocations */
        *flags|=H5FD_FEAT_AGGREGATE_SMALLDATA; /* OK to aggregate "small" raw data allocations */
        *flags|=H5FD_FEAT_HAS_MPI;             /* This driver uses MPI */
        *flags|=H5FD_FEAT_ALLOCATE_EARLY;      /* Allocate space early instead of late */
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mdc_alloc
 *
 * Purpose:	Gets the end-of-address marker for the file. The request 
 *              will be sent to the MDS server because it is the one
 *              responsible for managing this value.
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
H5FD_mdc_alloc(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, hsize_t size)
{
    const H5FD_mdc_t *file = (const H5FD_mdc_t*)_file;
    size_t buf_size;
    uint8_t *p = NULL;
    void *send_buf = NULL;
    size_t dxpl_size = 0;
    H5P_genplist_t *dxpl;
    haddr_t ret_value = HADDR_UNDEF;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(H5FD_MEM_DRAW == type);

    if(NULL == (dxpl = (H5P_genplist_t *)H5I_object_verify(dxpl_id, H5I_GENPROP_LST)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, HADDR_UNDEF, "not a property list");

    /* get property list size */
    if(H5P_DATASET_XFER_DEFAULT != dxpl_id) {
        if(H5P__encode(dxpl, FALSE, NULL, &dxpl_size) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, HADDR_UNDEF, "unable to encode property list");
    }

    buf_size = 1 + /* request type */ 
        sizeof(int) + /* metadata file id */
        sizeof(H5FD_mem_t) + /* requested type of space */
        H5V_limit_enc_size((uint64_t)dxpl_size) + dxpl_size + 
        H5V_limit_enc_size((uint64_t)size);

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, HADDR_UNDEF, "memory allocation failed")
    p = (uint8_t *)send_buf;    /* Temporary pointer to encoding buffer */

    /* encode request type */
    *p++ = (uint8_t)H5VL_ALLOC;

    /* encode the object id */
    INT32ENCODE(p, file->mdfile_id);
    *p++ = (uint8_t)type;

    /* encode the plist size */
    UINT64ENCODE_VARLEN(p, dxpl_size);
    /* encode property lists if they are not default*/
    if(H5P_DATASET_XFER_DEFAULT != dxpl_id) {
        if(H5P__encode(dxpl, FALSE, p, &dxpl_size) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, HADDR_UNDEF, "unable to encode property list");
        p += dxpl_size;
    }

    /* encode size requested */
    UINT64ENCODE_VARLEN(p, size);

    MPI_Pcontrol(0);
    /* send the request to the MDS process and recieve the metadata file ID */
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                   &ret_value, sizeof(uint64_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HADDR_UNDEF, "failed to send message")
    MPI_Pcontrol(1);

    H5MM_free(send_buf);
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD_mdc_alloc */


/*-------------------------------------------------------------------------
 * Function:	H5FD_mdc_get_eoa
 *
 * Purpose:	Gets the end-of-address marker for the file. The request 
 *              will be sent to the MDS server because it is the one
 *              responsible for managing this value.
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
H5FD_mdc_get_eoa(const H5FD_t *_file, H5FD_mem_t type)
{
    const H5FD_mdc_t	*file = (const H5FD_mdc_t*)_file;
    size_t buf_size;
    uint8_t *p = NULL;
    void *send_buf = NULL;
    haddr_t ret_value = HADDR_UNDEF;

    FUNC_ENTER_NOAPI_NOINIT

    buf_size = 1 /* request type*/ + sizeof(hid_t) /* metadata file id */ + sizeof(H5FD_mem_t);

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, HADDR_UNDEF, "memory allocation failed")

    p = (uint8_t *)send_buf;    /* Temporary pointer to encoding buffer */

    /* encode request type */
    *p++ = (uint8_t)H5VL_GET_EOA;

    /* encode the object id */
    INT32ENCODE(p, file->mdfile_id);
    *p++ = (uint8_t)type;

    MPI_Pcontrol(0);
    /* send the EOA request */
    if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, 
                                H5MD_LISTEN_TAG, MPI_COMM_WORLD))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HADDR_UNDEF, "failed to send message")

    /* recieve the current EOA */
    if(MPI_SUCCESS != MPI_Recv(&ret_value, sizeof(uint64_t), MPI_UINT64_T, MDS_RANK, 
                               H5MD_RETURN_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, HADDR_UNDEF, "failed to receive message")
    MPI_Pcontrol(1);

    H5MM_free(send_buf);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mdc_set_eoa
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
H5FD_mdc_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t eoa)
{
    const H5FD_mdc_t	*file = (const H5FD_mdc_t*)_file;
    size_t buf_size;
    uint8_t *p = NULL;
    void *send_buf = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    buf_size = 1 /* request type*/ + sizeof(hid_t) /* metadata file id */ + sizeof(H5FD_mem_t) +
        sizeof(uint64_t);

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    p = (uint8_t *)send_buf;    /* Temporary pointer to encoding buffer */

    /* encode request type */
    *p++ = (uint8_t)H5VL_SET_EOA;
    /* encode the object id */
    INT32ENCODE(p, file->mdfile_id);
    /* VFD memory type */
    *p++ = (uint8_t)type;
    /* eoa value */
    UINT64ENCODE(p, eoa);

    MPI_Pcontrol(0);
    /* send the EOA set request & recieve the set confirmation*/
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                   &ret_value, sizeof(herr_t), MPI_BYTE, MDS_RANK, H5MD_RETURN_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message")
    MPI_Pcontrol(1);

    H5MM_free(send_buf);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mdc_get_eof
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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_mdc_get_eof(const H5FD_t *_file)
{
    const H5FD_mdc_t	*file = (const H5FD_mdc_t*)_file;
    haddr_t ret_value = HADDR_UNDEF;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    ret_value = H5FDget_eof(file->memb);
    HDassert(HADDR_UNDEF != ret_value);

    FUNC_LEAVE_NOAPI(ret_value + HADDR_MAX/2)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mdc_read
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
H5FD_mdc_read(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, size_t size, void *buf)
{
    H5FD_mdc_t			*file = (H5FD_mdc_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(type == H5FD_MEM_DRAW);

    FUNC_LEAVE_NOAPI(H5FDread(file->memb, type, dxpl_id, addr - HADDR_MAX/2, size, buf))
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mdc_write
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
H5FD_mdc_write(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, 
               size_t size, const void *buf)
{
    H5FD_mdc_t			*file = (H5FD_mdc_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(type == H5FD_MEM_DRAW);

    FUNC_LEAVE_NOAPI(H5FDwrite(file->memb, type, dxpl_id, addr - HADDR_MAX/2, size, buf))
} /* end H5FD_mdc_write() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_mdc_flush
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
H5FD_mdc_flush(H5FD_t *_file, hid_t dxpl_id, unsigned closing)
{
    H5FD_mdc_t			*file = (H5FD_mdc_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(H5FDflush(file->memb, dxpl_id, closing))
}


/*-------------------------------------------------------------------------
 * Function:    H5FD_mdc_truncate
 *
 * Purpose:     Make certain the file's size matches it's allocated size
 *
 * Return:      Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              September, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mdc_truncate(H5FD_t *_file, hid_t dxpl_id, hbool_t closing)
{
    H5FD_mdc_t			*file = (H5FD_mdc_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(H5FDtruncate(file->memb, dxpl_id, closing))
}


/*-------------------------------------------------------------------------
 * Function:    H5FD_mdc_mpi_rank
 *
 * Purpose:     Returns the MPI rank for a process
 *
 * Return:      MPI rank.  Cannot report failure.
 *
 * Programmer:	Mohamad Chaarawi
 *              September, 2012
 *
 *-------------------------------------------------------------------------
 */
static int
H5FD_mdc_mpi_rank(const H5FD_t *_file)
{
    const H5FD_mdc_t *file = (const H5FD_mdc_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(H5FD_mpi_get_rank(file->memb))
} /* end H5FD_mdc_mpi_rank() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_mdc_mpi_size
 *
 * Purpose:     Returns the number of MPI processes
 *
 * Return:      The number of MPI processes.  Cannot report failure.
 *
 * Programmer:	Mohamad Chaarawi
 *              September, 2012
 *
 *-------------------------------------------------------------------------
 */
static int
H5FD_mdc_mpi_size(const H5FD_t *_file)
{
    const H5FD_mdc_t *file = (const H5FD_mdc_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(H5FD_mpi_get_size(file->memb))
} /* end H5FD_mdc_mpi_size() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_mdc_communicator
 *
 * Purpose:     Returns the MPI communicator for the file.
 *
 * Return:      The MPI communicator.  Cannot report failure.
 *
 * Programmer:	Mohamad Chaarawi
 *              September, 2012
 *
 *-------------------------------------------------------------------------
 */
static MPI_Comm
H5FD_mdc_communicator(const H5FD_t *_file)
{
    const H5FD_mdc_t *file = (const H5FD_mdc_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(H5FD_mpi_get_comm(file->memb))
} /* end H5FD_mdc_communicator() */

#endif /*H5_HAVE_PARALLEL*/
