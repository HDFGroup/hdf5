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
#include "H5VLmds.h"            /* MDS VOL plugin			*/
#include "H5VLmdserver.h"       /* MDS helper routines			*/

/* Loop through all mapped files */
#define UNIQUE_MEMBERS(MAP,LOOPVAR) {					      \
    H5FD_mem_t _unmapped, LOOPVAR;					      \
    hbool_t _seen[H5FD_MEM_NTYPES];					      \
									      \
    memset(_seen, 0, sizeof _seen);					      \
    for (_unmapped=H5FD_MEM_SUPER; _unmapped<H5FD_MEM_NTYPES; _unmapped=(H5FD_mem_t)(_unmapped+1)) {  \
	LOOPVAR = MAP[_unmapped];					      \
	if (H5FD_MEM_DEFAULT==LOOPVAR) LOOPVAR=_unmapped;		      \
	assert(LOOPVAR>0 && LOOPVAR<H5FD_MEM_NTYPES);			      \
	if (_seen[LOOPVAR]++) continue;

#define ALL_MEMBERS(LOOPVAR) {						      \
    H5FD_mem_t LOOPVAR;							      \
    for (LOOPVAR=H5FD_MEM_DEFAULT; LOOPVAR<H5FD_MEM_NTYPES; LOOPVAR=(H5FD_mem_t)(LOOPVAR+1)) {

#define END_MEMBERS	}}
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
    H5FD_t	*memb;	                   /*member pointer		*/
    haddr_t     memb_eoa[H5FD_MEM_NTYPES]; /*EOA for individual files   */
} H5FD_mds_t;

/* MDS specific file access properties */
typedef struct H5FD_mds_fapl_t {
    H5FD_mem_t	memb_map[H5FD_MEM_NTYPES]; /* memory usage map		*/
    hid_t	memb_fapl;                 /* underlying fapl    	*/
    char	*memb_name;                /* metadata file name	*/
    haddr_t	memb_addr;                 /* starting addr     	*/
} H5FD_mds_fapl_t;

/* Private Prototypes */

/* Callbacks */
static herr_t H5FD_mds_term(void);
static void *H5FD_get_fapl_mds(H5FD_t *_file);
static void *H5FD_mds_fapl_copy(const void *_old_fa);
static herr_t H5FD_mds_fapl_free(void *_fa);
static H5FD_t *H5FD_mds_open(const char *name, unsigned flags, hid_t fapl_id,
			      haddr_t maxaddr);
static herr_t H5FD_mds_close(H5FD_t *_file);
static herr_t H5FD_mds_query(const H5FD_t *_f1, unsigned long *flags);
static haddr_t H5FD_mds_get_eoa(const H5FD_t *_file, H5FD_mem_t type);
static herr_t H5FD_mds_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t addr);
static haddr_t H5FD_mds_get_eof(const H5FD_t *_file);
static herr_t  H5FD_mds_get_handle(H5FD_t *_file, hid_t fapl, void** file_handle);
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
    H5F_CLOSE_SEMI,				/* fc_degree		*/
    H5FD_mds_term,                             /*terminate             */
    NULL,					/*sb_size		*/
    NULL,					/*sb_encode		*/
    NULL,					/*sb_decode		*/
    sizeof(H5FD_mds_fapl_t),			/*fapl_size		*/
    H5FD_get_fapl_mds,				/*fapl_get		*/
    H5FD_mds_fapl_copy,			/*fapl_copy		*/
    H5FD_mds_fapl_free, 			/*fapl_free		*/
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
    NULL,        				/*get_eof		*/
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
H5P_set_fapl_mds(hid_t fapl_id, const char *name, hid_t plist_id)
{
    H5FD_mds_fapl_t	fa;
    H5P_genplist_t      *plist;      /* Property list pointer */
    herr_t              ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    if(fapl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a file access list")

    /* Initialize */
    ALL_MEMBERS(mt) {
	/* Treat global heap as raw data, not metadata */
	fa.memb_map[mt] = ((mt == H5FD_MEM_DRAW || mt == H5FD_MEM_GHEAP) ? H5FD_MEM_DRAW : H5FD_MEM_SUPER);
    } END_MEMBERS;
    if(H5P_DEFAULT != plist_id)
        fa.memb_fapl = plist_id;
    else
        fa.memb_fapl = H5Pcreate(H5P_FILE_ACCESS);
    fa.memb_name = H5MM_strdup(name);
    fa.memb_addr = 0;

    ret_value= H5P_set_driver(plist, H5FD_MDS, &fa);

    H5MM_free(fa.memb_name);
done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_get_fapl_mds
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
H5FD_get_fapl_mds(H5FD_t *_file)
{
    H5FD_mds_t	*file = (H5FD_mds_t*)_file;
    H5FD_mds_fapl_t *fa = NULL;
    void *ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file);
    HDassert(H5FD_MDS == file->pub.driver_id);

    if(NULL == (fa = (H5FD_mds_fapl_t *)H5MM_calloc(sizeof(H5FD_mds_fapl_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* MSC - Need to think more about that */
    
    /* Set return value */
    ret_value = (void *)fa;

done:
    //FUNC_LEAVE_NOAPI(H5FD_mds_fapl_copy(&(file->fa)))
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mds_fapl_copy
 *
 * Purpose:	Copies the mds-specific file access properties.
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
H5FD_mds_fapl_copy(const void *_old_fa)
{
    const H5FD_mds_fapl_t *old_fa = (const H5FD_mds_fapl_t*)_old_fa;
    H5FD_mds_fapl_t *new_fa = (H5FD_mds_fapl_t *)malloc(sizeof(H5FD_mds_fapl_t));
    void *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == new_fa)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    memcpy(new_fa, old_fa, sizeof(H5FD_mds_fapl_t));

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

    ret_value = (void *)new_fa;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mds_fapl_free
 *
 * Purpose:	Frees the mds-specific file access properties.
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
H5FD_mds_fapl_free(void *_fa)
{
    H5FD_mds_fapl_t	*fa = (H5FD_mds_fapl_t*)_fa;
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
H5FD_mds_open(const char UNUSED *name, unsigned flags, hid_t fapl_id,
              haddr_t UNUSED maxaddr)
{
    H5FD_mds_fapl_t     *fa = NULL;
    H5FD_mds_t	       	*file=NULL;
    H5FD_t		*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (file = (H5FD_mds_t *)calloc((size_t)1, sizeof(H5FD_mds_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    fa = (H5FD_mds_fapl_t *)H5Pget_driver_info(fapl_id);

    file->memb = H5FDopen(fa->memb_name, flags, fa->memb_fapl, HADDR_UNDEF);
    if (!file->memb)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "error opening member file")

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
    herr_t      ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

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
H5FD_mds_query(const H5FD_t *_file, unsigned long *flags /* out */)
{
    const H5FD_mds_t	*file = (const H5FD_mds_t*)_file;
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    ret_value = H5FDquery(file->memb, flags);

    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mds_get_type_map
 *
 * Purpose:	Retrieve the memory type mapping for this file
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Mohamad Chaarawi
 *              September, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mds_get_type_map(const H5FD_t *_file, H5FD_mem_t *type_map)
{
    const H5FD_mds_t	*file = (const H5FD_mds_t*)_file;

    /* MSC - need to figure this out - Copy file's free space type mapping 
    memcpy(type_map, file->fa.memb_map, sizeof(file->fa.memb_map));
    */
    return(0);
} /* end H5FD_mds_get_type_map() */


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
    H5FD_mem_t mmt = file->fa.memb_map[type];
    haddr_t ret_value;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if (H5FD_MEM_DEFAULT == mmt) 
        mmt = type;

    if (H5FD_MEM_DRAW != mmt) {
        H5E_BEGIN_TRY {
            ret_value = H5FDget_eoa(file->memb, mmt);
        } H5E_END_TRY;

        if (HADDR_UNDEF == ret_value)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, HADDR_UNDEF, "BAD EOA")
    } 
    else {
        ret_value = file->raw_eoa;
        assert(HADDR_UNDEF != file->raw_eoa);
    }

done:
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
    H5FD_mem_t   mmt;
    herr_t       ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    mmt = file->fa.memb_map[type];
    if(H5FD_MEM_DEFAULT == mmt)
        mmt = type;

    if (H5FD_MEM_DRAW != mmt) {
        if(ret_value = H5FDset_eoa(file->memb, mmt, eoa) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "can't set member EOA")
    }
    else
        file->raw_eoa = eoa;

done:
    FUNC_LEAVE_NOAPI(ret_value)
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
H5FD_mds_get_eof(const H5FD_t *_file)
{
    const H5FD_mds_t	*file = (const H5FD_mds_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(H5FDget_eof(file->memb))
}


/*-------------------------------------------------------------------------
 * Function:       H5FD_mds_get_handle
 *
 * Purpose:        Returns the file handle of MDS file driver.
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
H5FD_mds_get_handle(H5FD_t *_file, hid_t UNUSED fapl, void** file_handle)
{
    H5FD_mds_t         *file = (H5FD_mds_t *)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(file_handle);

    FUNC_LEAVE_NOAPI(H5FDget_handle(file->memb, fapl, file_handle))
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
H5FD_mds_read(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, size_t size,
	       void *buf/*out*/)
{
    H5FD_mds_t			*file = (H5FD_mds_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(type != H5FD_MEM_DRAW);

    FUNC_LEAVE_NOAPI(H5FDread(file->memb, type, dxpl_id, addr, size, buf))
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
H5FD_mds_write(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr,
		size_t size, const void *buf)
{
    H5FD_mds_t			*file = (H5FD_mds_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(type != H5FD_MEM_DRAW);

    FUNC_LEAVE_NOAPI(H5FDwrite(file->memb, type, dxpl_id, addr, size, buf);)
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
H5FD_mds_flush(H5FD_t *_file, hid_t dxpl_id, unsigned closing)
{
    H5FD_mds_t			*file = (H5FD_mds_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(H5FDflush(file->memb, dxpl_id, closing))
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
H5FD_mds_truncate(H5FD_t *_file, hid_t dxpl_id, hbool_t closing)
{
    H5FD_mds_t			*file = (H5FD_mds_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(H5FDtruncate(file->memb, dxpl_id, closing))
}
