/****************************************************************************
* NCSA HDF                                                                  *
* Software Development Group                                                *
* National Center for Supercomputing Applications                           *
* University of Illinois at Urbana-Champaign                                *
* 605 E. Springfield, Champaign IL 61820                                    *
*                                                                           *
* For conditions of distribution and use, see the accompanying              *
* hdf/COPYING file.                                                         *
*                                                                           *
****************************************************************************/

/* $Id$ */

#define H5S_PACKAGE		/*suppress error about including H5Spkg	  */

#include "H5private.h"		/* Generic Functions			*/
#include "H5Dprivate.h"		/* Dataset functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FDprivate.h"	/* File drivers				*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5Gprivate.h"		/* Group headers		  	*/
#include "H5HLprivate.h"	/* Name heap				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Oprivate.h"		/* Object headers		  	*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5Spkg.h"		/* Dataspace functions			*/
#include "H5Vprivate.h"		/* Vector and array functions		*/
#include "H5Zprivate.h"		/* Data filters				*/

/*#define H5D_DEBUG*/

/*
 * The MPIO & MPIPOSIX drivers are needed because there are kludges in this
 * file and places where we check for things that aren't handled by these
 * drivers.
 */
#include "H5FDmpio.h"
#include "H5FDmpiposix.h"

#ifdef H5_HAVE_PARALLEL
/* Remove this if H5R_DATASET_REGION is no longer used in this file */
#   include "H5Rpublic.h"
#endif /*H5_HAVE_PARALLEL*/

#define PABLO_MASK	H5D_mask

/*
 * A dataset is the following struct.
 */
struct H5D_t {
    H5G_entry_t		ent;		/* cached object header stuff	*/
    H5T_t		*type;		/* datatype of this dataset	*/
    hid_t               dcpl_id;        /* dataset creation property id */
    H5O_layout_t	layout;		/* data layout			*/
};


static int interface_initialize_g = 0;
#define INTERFACE_INIT H5D_init_interface
static herr_t H5D_init_interface(void);
static herr_t H5D_init_storage(H5D_t *dataset, const H5S_t *space);
H5D_t * H5D_new(hid_t dcpl_id);
static herr_t H5D_fill(const void *fill, const H5T_t *fill_type, void *buf,
    const H5T_t *buf_type, const H5S_t *space);
static herr_t H5D_get_space_status(H5D_t *dset, H5D_space_status_t *allocation);

/* Declare a free list to manage the H5D_t struct */
H5FL_DEFINE_STATIC(H5D_t);

/* Declare a free list to manage blocks of type conversion data */
H5FL_BLK_DEFINE_STATIC(type_conv);

/* Declare a free list to manage blocks of single datatype element data */
H5FL_BLK_DEFINE(type_elem);

/* Declare a free list to manage blocks of VL data */
H5FL_BLK_DEFINE_STATIC(vlen_vl_buf);

/* Declare a free list to manage other blocks of VL data */
H5FL_BLK_DEFINE_STATIC(vlen_fl_buf);


/*-------------------------------------------------------------------------
 * Function:	H5D_init
 *
 * Purpose:	Initialize the interface from some other layer.
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Saturday, March 4, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_init(void)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5D_init, FAIL);
    /* FUNC_ENTER() does all the work */

done:
    FUNC_LEAVE(ret_value);
}


/*--------------------------------------------------------------------------
NAME
   H5D_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5D_init_interface()
   
RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.
NOTES
    Care must be taken when using the H5P functions, since they can cause
    a deadlock in the library when the library is attempting to terminate -QAK

--------------------------------------------------------------------------*/
static herr_t
H5D_init_interface(void)
{
    /* Dataset Transfer property class variables.  In sequence, they are,
     * - Transfer Property list class to modify
     * - Default value for maximum temp buffer size
     * - Default value for type conversion buffer
     * - Default value for background buffer
     * - Default value for B-tree node split ratios
     * - Default value for hyperslab caching
     * - Default value for hyperslab cache limit
     * - Default value for vlen allocation function
     * - Default value for vlen allocation information
     * - Default value for vlen free function
     * - Default value for vlen free information
     * - Default value for file driver ID
     * - Default value for file driver info
     * - Default value for 'gather reads' property
     * - Default value for vector size
     */
    H5P_genclass_t  *xfer_pclass;   
    size_t          def_max_temp_buf         = H5D_XFER_MAX_TEMP_BUF_DEF;
    void            *def_tconv_buf           = H5D_XFER_TCONV_BUF_DEF;
    void            *def_bkgr_buf            = H5D_XFER_BKGR_BUF_DEF;   
    H5T_bkg_t       def_bkgr_buf_type        = H5D_XFER_BKGR_BUF_TYPE_DEF;     
    double          def_btree_split_ratio[3] = H5D_XFER_BTREE_SPLIT_RATIO_DEF;
#ifdef H5_WANT_H5_V1_4_COMPAT
    unsigned        def_hyper_cache          = H5D_XFER_HYPER_CACHE_DEF;     
    unsigned        def_hyper_cache_lim      = H5D_XFER_HYPER_CACHE_LIM_DEF;   
#endif /* H5_WANT_H5_V1_4_COMPAT */
    H5MM_allocate_t def_vlen_alloc           = H5D_XFER_VLEN_ALLOC_DEF;     
    void            *def_vlen_alloc_info     = H5D_XFER_VLEN_ALLOC_INFO_DEF;
    H5MM_free_t     def_vlen_free            = H5D_XFER_VLEN_FREE_DEF;    
    void            *def_vlen_free_info      = H5D_XFER_VLEN_FREE_INFO_DEF;
    hid_t           def_vfl_id               = H5D_XFER_VFL_ID_DEF;     
    void            *def_vfl_info            = H5D_XFER_VFL_INFO_DEF;    
    size_t          def_hyp_vec_size         = H5D_XFER_HYPER_VECTOR_SIZE_DEF; 

    /* Dataset creation property class variables.  In sequence, they are,
     * - Creation property list class to modify
     * - Default value for storage layout property
     * - Default value for chunk dimensionality property
     * - Default value for chunk size
     * - Default value for fill value
     * - Default value for external file list
     * - Default value for data filter pipeline
     */
    H5P_genclass_t  *crt_pclass;
    H5D_layout_t    layout                   = H5D_CRT_LAYOUT_DEF;
    int             chunk_ndims              = H5D_CRT_CHUNK_DIM_DEF;
    hsize_t         chunk_size[32]           = H5D_CRT_CHUNK_SIZE_DEF;
    H5O_fill_t      fill                     = H5D_CRT_FILL_VALUE_DEF;
    H5D_space_time_t    space_time           = H5D_CRT_SPACE_TIME_DEF;
    H5D_fill_time_t     fill_time            = H5D_CRT_FILL_TIME_DEF;   
    H5O_efl_t       efl                      = H5D_CRT_EXT_FILE_LIST_DEF;
    H5O_pline_t     pline                    = H5D_CRT_DATA_PIPELINE_DEF;

    size_t          nprops;                 /* Number of properties */
    herr_t          ret_value                = SUCCEED;   /* Return value */

    FUNC_ENTER_NOINIT(H5D_init_interface);

    /* Initialize the atom group for the dataset IDs */
    if (H5I_init_group(H5I_DATASET, H5I_DATASETID_HASHSIZE, H5D_RESERVED_ATOMS, (H5I_free_t)H5D_close)<0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize interface");

    /* =========Dataset Transfer Property Class Initialization========= */    
    /* Register the default dataset transfer properties */
    assert(H5P_CLS_DATASET_XFER_g!=(-1));

    /* Get the pointer to the dataset transfer class */
    if (NULL == (xfer_pclass = H5I_object_verify(H5P_CLS_DATASET_XFER_g, H5I_GENPROP_CLS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list class");

    /* Get the number of properties in the class */
    if(H5P_get_nprops_pclass(xfer_pclass,&nprops)<0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "can't query number of properties");

    /* Assume that if there are properties in the class, they are the default ones */
    if(nprops==0) {
        /* Register the max. temp buffer size property */
        if(H5P_register(xfer_pclass,H5D_XFER_MAX_TEMP_BUF_NAME,H5D_XFER_MAX_TEMP_BUF_SIZE,&def_max_temp_buf,NULL,NULL,NULL,NULL,NULL,NULL)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class");

        /* Register the type conversion buffer property */
        if(H5P_register(xfer_pclass,H5D_XFER_TCONV_BUF_NAME,H5D_XFER_TCONV_BUF_SIZE,&def_tconv_buf,NULL,NULL,NULL,NULL,NULL,NULL)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class");

        /* Register the background buffer property */
        if(H5P_register(xfer_pclass,H5D_XFER_BKGR_BUF_NAME,H5D_XFER_BKGR_BUF_SIZE,&def_bkgr_buf,NULL,NULL,NULL,NULL,NULL,NULL)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class");

        /* Register the background buffer type property */
        if(H5P_register(xfer_pclass,H5D_XFER_BKGR_BUF_TYPE_NAME,H5D_XFER_BKGR_BUF_TYPE_SIZE,&def_bkgr_buf_type,NULL,NULL,NULL,NULL,NULL,NULL)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class");

        /* Register the B-Tree node splitting ratios property */
        if(H5P_register(xfer_pclass,H5D_XFER_BTREE_SPLIT_RATIO_NAME,H5D_XFER_BTREE_SPLIT_RATIO_SIZE,&def_btree_split_ratio,NULL,NULL,NULL,NULL,NULL,NULL)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class");

#ifdef H5_WANT_H5_V1_4_COMPAT
        /* Register the hyperslab caching property */
        if(H5P_register(xfer_pclass,H5D_XFER_HYPER_CACHE_NAME,H5D_XFER_HYPER_CACHE_SIZE,&def_hyper_cache,NULL,NULL,NULL,NULL,NULL,NULL)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class");

        /* Register the hyperslab cache limit property */
        if(H5P_register(xfer_pclass,H5D_XFER_HYPER_CACHE_LIM_NAME,H5D_XFER_HYPER_CACHE_LIM_SIZE,&def_hyper_cache_lim,NULL,NULL,NULL,NULL,NULL,NULL)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class");
#endif /* H5_WANT_H5_V1_4_COMPAT */

        /* Register the vlen allocation function property */
        if(H5P_register(xfer_pclass,H5D_XFER_VLEN_ALLOC_NAME,H5D_XFER_VLEN_ALLOC_SIZE,&def_vlen_alloc,NULL,NULL,NULL,NULL,NULL,NULL)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class");

        /* Register the vlen allocation information property */
        if(H5P_register(xfer_pclass,H5D_XFER_VLEN_ALLOC_INFO_NAME,H5D_XFER_VLEN_ALLOC_INFO_SIZE,&def_vlen_alloc_info,NULL,NULL,NULL,NULL,NULL,NULL)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class");

        /* Register the vlen free function property */
        if(H5P_register(xfer_pclass,H5D_XFER_VLEN_FREE_NAME,H5D_XFER_VLEN_FREE_SIZE,&def_vlen_free,NULL,NULL,NULL,NULL,NULL,NULL)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class");

        /* Register the vlen free information property */
        if(H5P_register(xfer_pclass,H5D_XFER_VLEN_FREE_INFO_NAME,H5D_XFER_VLEN_FREE_INFO_SIZE,&def_vlen_free_info,NULL,NULL,NULL,NULL,NULL,NULL)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class");

        /* Register the file driver ID property */
        if(H5P_register(xfer_pclass,H5D_XFER_VFL_ID_NAME,H5D_XFER_VFL_ID_SIZE,&def_vfl_id,NULL,NULL,NULL,NULL,NULL,NULL)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class");

        /* Register the file driver info property */
        if(H5P_register(xfer_pclass,H5D_XFER_VFL_INFO_NAME,H5D_XFER_VFL_INFO_SIZE,&def_vfl_info,NULL,NULL,NULL,NULL,NULL,NULL)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class");

        /* Register the vector size property */
        if(H5P_register(xfer_pclass,H5D_XFER_HYPER_VECTOR_SIZE_NAME,H5D_XFER_HYPER_VECTOR_SIZE_SIZE,&def_hyp_vec_size,NULL,NULL,NULL,NULL,NULL,NULL)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class");
    } /* end if */

    /* Only register the default property list if it hasn't been created yet */
    if(H5P_LST_DATASET_XFER_g==(-1)) {
        /* Register the default data transfer property list */
        if ((H5P_LST_DATASET_XFER_g = H5P_create_id (xfer_pclass))<0)
            HGOTO_ERROR (H5E_PLIST, H5E_CANTREGISTER, FAIL, "can't register default property list");
    } /* end if */

    /* =========Dataset Creation Property Class Initialization========== */
    /* Register the default dataset creation properties */
    assert(H5P_CLS_DATASET_CREATE_g != -1);
    
    /* Get the pointer to the dataset creation class */
    if(NULL == (crt_pclass = H5I_object_verify(H5P_CLS_DATASET_CREATE_g, H5I_GENPROP_CLS)))
       HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list class");
        
    /* Get the number of properties in the class */
    if(H5P_get_nprops_pclass(crt_pclass,&nprops)<0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "can't query number of properties");

    /* Assume that if there are properties in the class, they are the default ones */
    if(nprops==0) {
        /* Register the storage layout property */ 
        if(H5P_register(crt_pclass, H5D_CRT_LAYOUT_NAME, H5D_CRT_LAYOUT_SIZE, &layout, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
           HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class");
        
        /* Register the chunking dimensionality property */
        if(H5P_register(crt_pclass, H5D_CRT_CHUNK_DIM_NAME, H5D_CRT_CHUNK_DIM_SIZE, &chunk_ndims, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
           HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class");

        /* Register the chunking size property */ 
        if(H5P_register(crt_pclass, H5D_CRT_CHUNK_SIZE_NAME, H5D_CRT_CHUNK_SIZE_SIZE, chunk_size, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
           HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class");
      
        /* Register the fill value property */
        if(H5P_register(crt_pclass, H5D_CRT_FILL_VALUE_NAME, H5D_CRT_FILL_VALUE_SIZE, &fill, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
           HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class");

        /* Register the space allocation time property */
        if(H5P_register(crt_pclass, H5D_CRT_SPACE_TIME_NAME, H5D_CRT_SPACE_TIME_SIZE, &space_time, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class");

        /* Register the fill value writing time property */
        if(H5P_register(crt_pclass, H5D_CRT_FILL_TIME_NAME, H5D_CRT_FILL_TIME_SIZE, &fill_time, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class");

        /* Register the external file list property */
        if(H5P_register(crt_pclass, H5D_CRT_EXT_FILE_LIST_NAME, H5D_CRT_EXT_FILE_LIST_SIZE, &efl, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
           HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class");
       
        /* Register the data pipeline property */
        if(H5P_register(crt_pclass, H5D_CRT_DATA_PIPELINE_NAME, H5D_CRT_DATA_PIPELINE_SIZE, &pline, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
           HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class");
    } /* end if */

    /* Only register the default property list if it hasn't been created yet */
    if(H5P_LST_DATASET_CREATE_g==(-1)) {
        /* Register the default data transfer property list */
        if ((H5P_LST_DATASET_CREATE_g = H5P_create_id (crt_pclass))<0)
            HGOTO_ERROR (H5E_PLIST, H5E_CANTREGISTER, FAIL, "can't register default property list");
    } /* end if */

done:
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5D_term_interface
 *
 * Purpose:	Terminate this interface.
 *
 * Return:	Success:	Positive if anything was done that might
 *				affect other interfaces; zero otherwise.
 *
 * 		Failure:	Negative.
 *
 * Programmer:	Robb Matzke
 *              Friday, November 20, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5D_term_interface(void)
{
    int		n=0;

    FUNC_ENTER_NOINIT(H5D_term_interface);

    if (interface_initialize_g) {
	if ((n=H5I_nmembers(H5I_DATASET))) {
	    H5I_clear_group(H5I_DATASET, FALSE);
	} else {
	    H5I_destroy_group(H5I_DATASET);
	    interface_initialize_g = 0;
	    n = 1; /*H5I*/
	}
    }
    FUNC_LEAVE(n);
}


/*-------------------------------------------------------------------------
 * Function:       H5D_crt_copy
 *
 * Purpose:        Callback routine which is called whenever any dataset 
 *                 creation property list is copied.  This routine copies
 *                 the properties from the old list to the new list.
 *
 * Return:         Success:        Non-negative
 *
 *                 Failure:        Negative
 *
 * Programmer:     Raymond Lu
 *                 Tuesday, October 2, 2001
 *
 * Modification: 
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_crt_copy(hid_t new_plist_id, hid_t old_plist_id, void UNUSED *copy_data)
{
    H5O_fill_t     src_fill, dst_fill;
    H5O_efl_t      src_efl, dst_efl;
    H5O_pline_t    src_pline, dst_pline;
    H5P_genplist_t *old_plist;
    H5P_genplist_t *new_plist;
    herr_t         ret_value=SUCCEED;

    FUNC_ENTER_NOAPI(H5D_crt_copy, FAIL);

    /* Verify property list ID */
    if (NULL == (new_plist = H5P_object_verify(new_plist_id,H5P_DATASET_CREATE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation property list");
    if (NULL == (old_plist = H5P_object_verify(old_plist_id,H5P_DATASET_CREATE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation property list");

    /* Get the fill value, external file list, and data pipeline properties
     * from the old property list */
    HDmemset(&src_fill,0,sizeof(H5O_fill_t));
    if(H5P_get(old_plist, H5D_CRT_FILL_VALUE_NAME, &src_fill) < 0) 
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get fill value");
    HDmemset(&src_efl,0,sizeof(H5O_efl_t));
    if(H5P_get(old_plist, H5D_CRT_EXT_FILE_LIST_NAME, &src_efl) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get external file list");
    HDmemset(&src_pline,0,sizeof(H5O_pline_t));
    if(H5P_get(old_plist, H5D_CRT_DATA_PIPELINE_NAME, &src_pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline");

    /* Make copies of fill value, external file list, and data pipeline */
    HDmemset(&dst_fill,0,sizeof(H5O_fill_t));
    if(src_fill.buf && (NULL==H5O_copy(H5O_FILL, &src_fill, &dst_fill))) {
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "can't copy fill value");
    }
    else if (!src_fill.buf) {
	dst_fill.type = dst_fill.buf = NULL;
	dst_fill.size = src_fill.size;
    }
    HDmemset(&dst_efl,0,sizeof(H5O_efl_t));
    if(NULL==H5O_copy(H5O_EFL, &src_efl, &dst_efl)) 
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "can't copy external file list");
    HDmemset(&dst_pline,0,sizeof(H5O_pline_t));
    if(NULL==H5O_copy(H5O_PLINE, &src_pline, &dst_pline)) 
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "can't copy data pipeline");

    /* Set the fill value, external file list, and data pipeline property 
     * for the new property list */
    if(H5P_set(new_plist, H5D_CRT_FILL_VALUE_NAME, &dst_fill) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set fill value");
    if(H5P_set(new_plist, H5D_CRT_EXT_FILE_LIST_NAME, &dst_efl) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set external file list");
    if(H5P_set(new_plist, H5D_CRT_DATA_PIPELINE_NAME, &dst_pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set pipeline");

done:
    FUNC_LEAVE(ret_value);    
}


/*-------------------------------------------------------------------------
 * Function:	H5D_crt_close
 *
 * Purpose:	Callback routine which is called whenever any dataset create
 *              property list is closed.  This routine performs any generic
 *              cleanup needed on the properties the library put into the list.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, July 11, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_crt_close(hid_t dcpl_id, void UNUSED *close_data)
{
    H5O_fill_t     fill;
    H5O_efl_t      efl;
    H5O_pline_t    pline;
    H5P_genplist_t *plist;      /* Property list */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5D_crt_close, FAIL);

    /* Check arguments */
    if (NULL == (plist = H5P_object_verify(dcpl_id,H5P_DATASET_CREATE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation property list");

    /* Get the fill value, external file list, and data pipeline properties
     * from the old property list */
    HDmemset(&fill,0,sizeof(H5O_fill_t));
    if(H5P_get(plist, H5D_CRT_FILL_VALUE_NAME, &fill) < 0) 
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get fill value");
    HDmemset(&efl,0,sizeof(H5O_efl_t));
    if(H5P_get(plist, H5D_CRT_EXT_FILE_LIST_NAME, &efl) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get external file list");
    HDmemset(&pline,0,sizeof(H5O_pline_t));
    if(H5P_get(plist, H5D_CRT_DATA_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline");

    /* Clean up any values set for the fill-value, external file-list and
     * data pipeline */
    H5O_reset(H5O_FILL, &fill);
    H5O_reset(H5O_EFL, &efl);
    H5O_reset(H5O_PLINE, &pline);

done:
    FUNC_LEAVE(ret_value);
} /* end H5D_crt_close() */


/*-------------------------------------------------------------------------
 * Function:	H5D_xfer_create
 *
 * Purpose:	Callback routine which is called whenever any dataset transfer
 *              property list is created.  This routine performs any generic
 *              initialization needed on the properties the library put into
 *              the list.
 *              Right now, it's just allocating the driver-specific dataset
 *              transfer information.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, August 2, 2001
 *
 * Notes:       This same routine is currently used for the 'copy' callback.
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_xfer_create(hid_t dxpl_id, void UNUSED *create_data)
{
    hid_t driver_id;            /* VFL driver ID */
    void *driver_info;          /* VFL driver info */
    H5P_genplist_t *plist;      /* Property list */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5D_xfer_create, FAIL);

    /* Check arguments */
    if (NULL == (plist = H5P_object_verify(dxpl_id,H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset transfer property list");

    /* Get the driver information */
    if(H5P_get(plist, H5D_XFER_VFL_ID_NAME, &driver_id)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve VFL driver ID");
    if(H5P_get(plist, H5D_XFER_VFL_INFO_NAME, &driver_info)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve VFL driver info");

    /* Check if we have a valid driver ID */
    if(driver_id>0) {
        /* Increment the reference count on the driver and copy the driver info */
        if(H5I_inc_ref(driver_id)<0)
            HGOTO_ERROR (H5E_DATASET, H5E_CANTINC, FAIL, "Can't increment VFL driver ID");
        if((driver_info = H5FD_dxpl_copy(driver_id, driver_info))==NULL)
            HGOTO_ERROR (H5E_DATASET, H5E_CANTCOPY, FAIL, "Can't copy VFL driver");
        
        /* Set the driver information for the new property list */
        if(H5P_set(plist, H5D_XFER_VFL_ID_NAME, &driver_id)<0)
            HGOTO_ERROR (H5E_PLIST, H5E_CANTSET, FAIL, "Can't set VFL driver ID");
        if(H5P_set(plist, H5D_XFER_VFL_INFO_NAME, &driver_info)<0)
            HGOTO_ERROR (H5E_PLIST, H5E_CANTSET, FAIL, "Can't set VFL driver info");
    } /* end if */

done:
    FUNC_LEAVE(ret_value);
} /* end H5D_xfer_create() */


/*-------------------------------------------------------------------------
 * Function:       H5D_xfer_copy
 *
 * Purpose:        Callback routine which is called whenever any dataset 
 *                 transfer property list is copied.  This routine copies
 *                 the properties from the old list to the new list.
 *
 * Return:         Success:        Non-negative
 *
 *                 Failure:        Negative
 *
 * Programmer:     Raymond Lu
 *                 Tuesday, October 2, 2001
 *
 * Modification: 
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_xfer_copy(hid_t new_plist_id, hid_t UNUSED old_plist_id, 
                void *copy_data)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5D_xfer_copy, FAIL);

    if(H5D_xfer_create(new_plist_id, copy_data) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "can't copy property list");

done:
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5D_xfer_close
 *
 * Purpose:	Callback routine which is called whenever any dataset transfer
 *              property list is closed.  This routine performs any generic
 *              cleanup needed on the properties the library put into the list.
 *              Right now, it's just freeing the driver-specific dataset
 *              transfer information.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, July 11, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_xfer_close(hid_t dxpl_id, void UNUSED *close_data)
{
    hid_t driver_id;            /* VFL driver ID */
    void *driver_info;          /* VFL driver info */
    H5P_genplist_t *plist;      /* Property list */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5D_xfer_close, FAIL);

    /* Check arguments */
    if (NULL == (plist = H5P_object_verify(dxpl_id,H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset transfer property list");

    if(H5P_get(plist, H5D_XFER_VFL_ID_NAME, &driver_id)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve VFL driver ID");
    if(H5P_get(plist, H5D_XFER_VFL_INFO_NAME, &driver_info)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve VFL driver info");
    if(driver_id>0) {
        if(H5FD_dxpl_free(driver_id, driver_info)<0)
            HGOTO_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "Can't free VFL driver");
        if(H5I_dec_ref(driver_id)<0)
            HGOTO_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "Can't decrement VFL driver ID");
    } /* end if */

done:
    FUNC_LEAVE(ret_value);
} /* end H5D_xfer_close() */


/*-------------------------------------------------------------------------
 * Function:	H5Dcreate
 *
 * Purpose:	Creates a new dataset named NAME at LOC_ID, opens the
 *		dataset for access, and associates with that dataset constant
 *		and initial persistent properties including the type of each
 *		datapoint as stored in the file (TYPE_ID), the size of the
 *		dataset (SPACE_ID), and other initial miscellaneous
 *		properties (PLIST_ID).
 *
 *		All arguments are copied into the dataset, so the caller is
 *		allowed to derive new types, data spaces, and creation
 *		parameters from the old ones and reuse them in calls to
 *		create other datasets.
 *
 * Return:	Success:	The object ID of the new dataset.  At this
 *				point, the dataset is ready to receive its
 *				raw data.  Attempting to read raw data from
 *				the dataset will probably return the fill
 *				value.	The dataset should be closed when
 *				the caller is no longer interested in it.
 *
 *		Failure:	FAIL
 *
 * Errors:
 *		ARGS	  BADTYPE	Not a data space. 
 *		ARGS	  BADTYPE	Not a dataset creation plist. 
 *		ARGS	  BADTYPE	Not a file. 
 *		ARGS	  BADTYPE	Not a type. 
 *		ARGS	  BADVALUE	No name. 
 *		DATASET	  CANTINIT	Can't create dataset. 
 *		DATASET	  CANTREGISTER	Can't register dataset. 
 *
 * Programmer:	Robb Matzke
 *		Wednesday, December  3, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Dcreate(hid_t loc_id, const char *name, hid_t type_id, hid_t space_id,
	  hid_t plist_id)
{
    H5G_entry_t		   *loc = NULL;
    H5T_t		   *type = NULL;
    H5S_t		   *space = NULL;
    H5D_t		   *new_dset = NULL;
    hid_t		    ret_value;

    FUNC_ENTER_API(H5Dcreate, FAIL);
    H5TRACE5("i","isiii",loc_id,name,type_id,space_id,plist_id);

    /* Check arguments */
    if (NULL == (loc = H5G_loc(loc_id)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location");
    if (!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name");
    if (NULL == (type = H5I_object_verify(type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a type");
    if (NULL == (space = H5I_object_verify(space_id, H5I_DATASPACE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    if(H5P_DEFAULT == plist_id)
        plist_id = H5P_DATASET_CREATE_DEFAULT;
    if(TRUE != H5P_isa_class(plist_id, H5P_DATASET_CREATE))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not dataset create property list");

    /* build and open the new dataset */
    if (NULL == (new_dset = H5D_create(loc, name, type, space, plist_id)))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create dataset");

    /* Register the new dataset to get an ID for it */
    if ((ret_value = H5I_register(H5I_DATASET, new_dset)) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTREGISTER, FAIL, "unable to register dataset");

done:
    if(ret_value<0) {
        if(new_dset!=NULL)
            H5D_close(new_dset);
    } /* end if */

    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Dopen
 *
 * Purpose:	Finds a dataset named NAME at LOC_ID, opens it, and returns
 *		its ID.	 The dataset should be close when the caller is no
 *		longer interested in it.
 *
 * Return:	Success:	A new dataset ID
 *
 *		Failure:	FAIL
 *
 * Errors:
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Dopen(hid_t loc_id, const char *name)
{
    H5G_entry_t	*loc = NULL;		/*location holding the dataset	*/
    H5D_t	*dataset = NULL;	/*the dataset			*/
    hid_t	ret_value;

    FUNC_ENTER_API(H5Dopen, FAIL);
    H5TRACE2("i","is",loc_id,name);

    /* Check args */
    if (NULL == (loc = H5G_loc(loc_id)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location");
    if (!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name");
    
    /* Find the dataset */
    if (NULL == (dataset = H5D_open(loc, name)))
	HGOTO_ERROR(H5E_DATASET, H5E_NOTFOUND, FAIL, "dataset not found");
    
    /* Create an atom for the dataset */
    if ((ret_value = H5I_register(H5I_DATASET, dataset)) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTREGISTER, FAIL, "can't register dataset");

done:
    if(ret_value<0) {
        if(dataset!=NULL)
            H5D_close(dataset);
    } /* end if */

    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Dclose
 *
 * Purpose:	Closes access to a dataset (DATASET_ID) and releases
 *		resources used by it. It is illegal to subsequently use that
 *		same dataset ID in calls to other dataset functions.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Errors:
 *		ARGS	  BADTYPE	Not a dataset. 
 *		DATASET	  CANTINIT	Can't free. 
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dclose(hid_t dset_id)
{
    H5D_t	*dset = NULL;	        /* Dataset object to release */
    herr_t       ret_value=SUCCEED;     /* Return value */

    FUNC_ENTER_API(H5Dclose, FAIL);
    H5TRACE1("e","i",dset_id);

    /* Check args */
    if (NULL == (dset = H5I_object_verify(dset_id, H5I_DATASET)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
    if (NULL == dset->ent.file)
	HGOTO_ERROR(H5E_DATASET, H5E_BADTYPE, FAIL, "not a dataset");

    /*
     * Decrement the counter on the dataset.  It will be freed if the count
     * reaches zero.
     */
    if (H5I_dec_ref(dset_id) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't free");

done:
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Dget_space
 *
 * Purpose:	Returns a copy of the file data space for a dataset.
 *
 * Return:	Success:	ID for a copy of the data space.  The data
 *				space should be released by calling
 *				H5Sclose().
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January 28, 1998
 *
 * Modifications:
 *	Robb Matzke, 9 Jun 1998
 *	The data space is not constant and is no longer cached by the dataset
 *	struct.
 *-------------------------------------------------------------------------
 */
hid_t
H5Dget_space(hid_t dset_id)
{
    H5D_t	*dset = NULL;
    H5S_t	*space = NULL;
    hid_t	ret_value;
    
    FUNC_ENTER_API(H5Dget_space, FAIL);
    H5TRACE1("i","i",dset_id);

    /* Check args */
    if (NULL==(dset=H5I_object_verify(dset_id, H5I_DATASET)))
	HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");

    /* Read the data space message and return a data space object */
    if (NULL==(space=H5D_get_space (dset)))
	HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to get data space");

    /* Create an atom */
    if ((ret_value=H5I_register (H5I_DATASPACE, space))<0)
	HGOTO_ERROR (H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register data space");

done:
    if(ret_value<0) {
        if(space!=NULL)
            H5S_close(space);
    } /* end if */

    FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5D_get_space
 *
 * Purpose:	Returns the data space associated with the dataset.
 *
 * Return:	Success:	Ptr to a copy of the data space.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, August 25, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5S_t *
H5D_get_space(H5D_t *dset)
{
    H5S_t	*space;
    H5S_t	*ret_value;     /* Return value */
    
    FUNC_ENTER_NOAPI(H5D_get_space, NULL);
    assert(dset);

    if (NULL==(space=H5S_read(&(dset->ent))))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to load space info from dataset header");

    /* Set return value */
    ret_value=space;

done:
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Dget_space_status
 *
 * Purpose:	Returns the status of data space allocation.
 *
 * Return:	
 *		Success:	Non-negative
 *		
 *		Failture:	Negative
 *
 * Programmer:	Raymond Lu
 *
 * Modification:
 *
 *-------------------------------------------------------------------------
 */
herr_t H5Dget_space_status(hid_t dset_id, H5D_space_status_t *allocation)
{
    H5D_t 	*dset = NULL;
    herr_t 	ret_value = SUCCEED;

    FUNC_ENTER_API(H5Dget_space_status, FAIL);

    /* Check arguments */
    if(NULL==(dset=H5I_object_verify(dset_id, H5I_DATASET)))
	HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");

    /* Read data space address and return */
    if(FAIL==(ret_value=H5D_get_space_status(dset, allocation)))
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to get space status");

done:
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:    H5D_get_space_status
 *
 * Purpose:     Returns the status of data space allocation.
 *
 * Return:
 *              Success:        Non-negative
 *
 *              Failture:       Negative
 *
 * Programmer:  Raymond Lu
 *
 * Modification:
 *
 *-------------------------------------------------------------------------
 */
static herr_t H5D_get_space_status(H5D_t *dset, H5D_space_status_t *allocation)
{
    H5S_t      *space=NULL;         /* Dataset's dataspace */
    hsize_t     space_allocated;    /* The number of bytes allocated for chunks */
    hssize_t    total_elem;         /* The total number of elements in dataspace */
    size_t      type_size;          /* The size of the datatype for the dataset */
    hsize_t     full_size;          /* The number of bytes in the dataset when fully populated */
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_NOINIT(H5D_get_space_status);

    /* Get the dataset's dataspace */
    if((space=H5D_get_space(dset))==NULL)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to get dataspace");

    /* Get the total number of elements in dataset's dataspace */
    if((total_elem=H5S_get_simple_extent_npoints(space))<0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTCOUNT, FAIL, "unable to get # of dataspace elements");

    /* Get the size of the dataset's datatype */
    if((type_size=H5T_get_size(dset->type))==0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTCOUNT, FAIL, "unable to get size of datatype");

    /* Compute the maximum size of the dataset in bytes */
    H5_CHECK_OVERFLOW(total_elem,hssize_t,hsize_t);
    full_size=((hsize_t)total_elem)*type_size;

    /* Difficult to error check, since the error value is 0 and 0 is a valid value... :-/ */
    space_allocated=H5D_get_storage_size(dset);

    /* Decide on how much of the space is allocated */
    if(space_allocated==0)
        *allocation = H5D_SPACE_STATUS_NOT_ALLOCATED;
    else if(space_allocated==full_size)
        *allocation = H5D_SPACE_STATUS_ALLOCATED;
    else {
        /* Should only happen for chunked datasets currently */
        assert(dset->layout.type==H5D_CHUNKED);

        *allocation = H5D_SPACE_STATUS_PART_ALLOCATED;
    } /* end else */

done:
    if(space)
	H5S_close(space);
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Dget_type
 *
 * Purpose:	Returns a copy of the file data type for a dataset.
 *
 * Return:	Success:	ID for a copy of the data type.	 The data
 *				type should be released by calling
 *				H5Tclose().
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, February  3, 1998
 *
 * Modifications:
 *
 * 	Robb Matzke, 1 Jun 1998
 *	If the dataset has a named data type then a handle to the opened data
 *	type is returned.  Otherwise the returned data type is read-only.  If
 *	atomization of the data type fails then the data type is closed.
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Dget_type(hid_t dset_id)
{
    
    H5D_t	*dset = NULL;
    H5T_t	*copied_type = NULL;
    hid_t	ret_value = FAIL;
    
    FUNC_ENTER_API(H5Dget_type, FAIL);
    H5TRACE1("i","i",dset_id);

    /* Check args */
    if (NULL==(dset=H5I_object_verify(dset_id, H5I_DATASET)))
	HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");

    /* Copy the data type and mark it read-only */
    if (NULL==(copied_type=H5T_copy (dset->type, H5T_COPY_REOPEN)))
	HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to copy the data type");

    /* Mark any VL datatypes as being in memory now */
    if (H5T_vlen_mark(copied_type, NULL, H5T_VLEN_MEMORY)<0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "invalid VL location");

    /* Unlock copied type */
    if (H5T_lock (copied_type, FALSE)<0)
	HGOTO_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to lock transient data type");
    
    /* Create an atom */
    if ((ret_value=H5I_register (H5I_DATATYPE, copied_type))<0)
	HGOTO_ERROR (H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register data type");

done:
    if(ret_value<0) {
        if(copied_type!=NULL)
            H5T_close (copied_type);
    } /* end if */

    FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Dget_create_plist
 *
 * Purpose:	Returns a copy of the dataset creation property list.
 *
 * Return:	Success:	ID for a copy of the dataset creation
 *				property list.  The template should be
 *				released by calling H5P_close().
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, February  3, 1998
 *
 * Modifications:
 * 
 *              Raymond Lu
 *              Tuesday, October 2, 2001
 *              The way to retrieve and set property is changed for the 
 *              generic property list.
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Dget_create_plist(hid_t dset_id)
{
    H5D_t		*dset = NULL;
    H5O_fill_t          copied_fill={NULL,0,NULL};
    H5P_genplist_t      *dcpl_plist;
    H5P_genplist_t      *new_plist;
    hid_t		new_dcpl_id = FAIL;
    hid_t		ret_value = FAIL;
    
    FUNC_ENTER_API(H5Dget_create_plist, FAIL);
    H5TRACE1("i","i",dset_id);

    /* Check args */
    if (NULL==(dset=H5I_object_verify(dset_id, H5I_DATASET)))
	HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
    if (NULL == (dcpl_plist = H5I_object(dset->dcpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't get property list");

    /* Copy the creation property list */
    if((new_dcpl_id = H5P_copy_plist(dcpl_plist)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "unable to copy the creation property list");
    if (NULL == (new_plist = H5I_object(new_dcpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't get property list");

    /* Get the fill value property */
    if(H5P_get(new_plist, H5D_CRT_FILL_VALUE_NAME, &copied_fill) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get fill value");

    /* Copy the dataset type into the fill value message */
    if(copied_fill.type==NULL)
        if(NULL==(copied_fill.type=H5T_copy(dset->type, H5T_COPY_TRANSIENT)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to copy dataset data type for fill value");

    /* Set back the fill value property to property list */
    if(H5P_set(new_plist, H5D_CRT_FILL_VALUE_NAME, &copied_fill) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "unable to set property list fill value");

    /* Set the return value */
    ret_value=new_dcpl_id;

done:
    if(ret_value<0) {
        if(new_dcpl_id>0)
            H5Pclose(new_dcpl_id);
    } /* end if */

    FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Dread
 *
 * Purpose:	Reads (part of) a DSET from the file into application
 *		memory BUF. The part of the dataset to read is defined with
 *		MEM_SPACE_ID and FILE_SPACE_ID.	 The data points are
 *		converted from their file type to the MEM_TYPE_ID specified. 
 *		Additional miscellaneous data transfer properties can be
 *		passed to this function with the PLIST_ID argument.
 *
 *		The FILE_SPACE_ID can be the constant H5S_ALL which indicates
 *		that the entire file data space is to be referenced.
 *
 *		The MEM_SPACE_ID can be the constant H5S_ALL in which case
 *		the memory data space is the same as the file data space
 *		defined when the dataset was created.
 *
 *		The number of elements in the memory data space must match
 *		the number of elements in the file data space.
 *
 *		The PLIST_ID can be the constant H5P_DEFAULT in which
 *		case the default data transfer properties are used.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Errors:
 *		ARGS	  BADTYPE	Not a data space. 
 *		ARGS	  BADTYPE	Not a data type. 
 *		ARGS	  BADTYPE	Not a dataset. 
 *		ARGS	  BADTYPE	Not xfer parms. 
 *		ARGS	  BADVALUE	No output buffer. 
 *		DATASET	  READERROR	Can't read data. 
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dread(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
	hid_t file_space_id, hid_t plist_id, void *buf/*out*/)
{
    H5D_t		   *dset = NULL;
    const H5T_t		   *mem_type = NULL;
    const H5S_t		   *mem_space = NULL;
    const H5S_t		   *file_space = NULL;
    herr_t                  ret_value=SUCCEED;  /* Return value */

    FUNC_ENTER_API(H5Dread, FAIL);
    H5TRACE6("e","iiiiix",dset_id,mem_type_id,mem_space_id,file_space_id,
             plist_id,buf);

    /* check arguments */
    if (NULL == (dset = H5I_object_verify(dset_id, H5I_DATASET)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
    if (NULL == dset->ent.file)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
    if (NULL == (mem_type = H5I_object_verify(mem_type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    if (H5S_ALL != mem_space_id) {
	if (NULL == (mem_space = H5I_object_verify(mem_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");

	/* Check for valid selection */
	if((*mem_space->select.is_valid)(mem_space)!=TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent");
    }
    if (H5S_ALL != file_space_id) {
	if (NULL == (file_space = H5I_object_verify(file_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");

	/* Check for valid selection */
	if((*file_space->select.is_valid)(file_space)!=TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent");
    }

    /* Get the default dataset transfer property list if the user didn't provide one */
    if (H5P_DEFAULT == plist_id)
        plist_id= H5P_DATASET_XFER_DEFAULT;

    if (TRUE!=H5P_isa_class(plist_id,H5P_DATASET_XFER))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms");
    if (!buf)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer");

    /* read raw data */
    if (H5D_read(dset, mem_type, mem_space, file_space, plist_id, buf/*out*/) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data");

done:
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Dwrite
 *
 * Purpose:	Writes (part of) a DSET from application memory BUF to the
 *		file.  The part of the dataset to write is defined with the
 *		MEM_SPACE_ID and FILE_SPACE_ID arguments. The data points
 *		are converted from their current type (MEM_TYPE_ID) to their
 *		file data type.	 Additional miscellaneous data transfer
 *		properties can be passed to this function with the
 *		PLIST_ID argument.
 *
 *		The FILE_SPACE_ID can be the constant H5S_ALL which indicates
 *		that the entire file data space is to be referenced.
 *
 *		The MEM_SPACE_ID can be the constant H5S_ALL in which case
 *		the memory data space is the same as the file data space
 *		defined when the dataset was created.
 *
 *		The number of elements in the memory data space must match
 *		the number of elements in the file data space.
 *
 *		The PLIST_ID can be the constant H5P_DEFAULT in which
 *		case the default data transfer properties are used.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Errors:
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dwrite(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
	 hid_t file_space_id, hid_t plist_id, const void *buf)
{
    H5D_t		   *dset = NULL;
    const H5T_t		   *mem_type = NULL;
    const H5S_t		   *mem_space = NULL;
    const H5S_t		   *file_space = NULL;
    herr_t                  ret_value=SUCCEED;  /* Return value */

    FUNC_ENTER_API(H5Dwrite, FAIL);
    H5TRACE6("e","iiiiix",dset_id,mem_type_id,mem_space_id,file_space_id,
             plist_id,buf);

    /* check arguments */
    if (NULL == (dset = H5I_object_verify(dset_id, H5I_DATASET)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
    if (NULL == dset->ent.file)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
    if (NULL == (mem_type = H5I_object_verify(mem_type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    if (H5S_ALL != mem_space_id) {
	if (NULL == (mem_space = H5I_object_verify(mem_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");

	/* Check for valid selection */
	if ((*mem_space->select.is_valid)(mem_space)!=TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent");
    }
    if (H5S_ALL != file_space_id) {
	if (NULL == (file_space = H5I_object_verify(file_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");

	/* Check for valid selection */
	if ((*file_space->select.is_valid)(file_space)!=TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent");
    }

    /* Get the default dataset transfer property list if the user didn't provide one */
    if (H5P_DEFAULT == plist_id)
        plist_id= H5P_DATASET_XFER_DEFAULT;

    if (TRUE!=H5P_isa_class(plist_id,H5P_DATASET_XFER))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms");
    if (!buf)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer");

    /* write raw data */
    if (H5D_write(dset, mem_type, mem_space, file_space, plist_id, buf) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data");

done:
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Dextend
 *
 * Purpose:	This function makes sure that the dataset is at least of size
 *		SIZE. The dimensionality of SIZE is the same as the data
 *		space of the dataset being changed.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Friday, January 30, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dextend(hid_t dset_id, const hsize_t *size)
{
    H5D_t	*dset = NULL;
    herr_t       ret_value=SUCCEED;  /* Return value */
    
    FUNC_ENTER_API(H5Dextend, FAIL);
    H5TRACE2("e","i*h",dset_id,size);

    /* Check args */
    if (NULL==(dset=H5I_object_verify(dset_id, H5I_DATASET)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
    if (!size)
	HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "no size specified");

    /* Increase size */
    if (H5D_extend (dset, size)<0)
	HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to extend dataset");

done:
    FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5D_new
 *
 * Purpose:	Creates a new, empty dataset structure
 *
 * Return:	Success:	Pointer to a new dataset descriptor.
 *
 *		Failure:	NULL
 *
 * Errors:
 *
 * Programmer:	Quincey Koziol
 *		Monday, October 12, 1998
 *
 * Modifications:
 *
 *              Raymond Lu
 *              Tuesday, October 2, 2001
 *              Changed the way to query and inialization for generic
 *              property list.
 *
 *-------------------------------------------------------------------------
 */
H5D_t *
H5D_new(hid_t dcpl_id)
{
    H5P_genplist_t	*plist;    /* Property list created */
    H5D_t	*new_dset = NULL;  /* New dataset object */
    H5D_t	*ret_value;	   /* Return value */
    
    FUNC_ENTER_NOAPI(H5D_new, NULL);

    if (NULL==(new_dset = H5FL_ALLOC(H5D_t,1)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
    if(H5P_DEFAULT == dcpl_id)
        dcpl_id = H5P_DATASET_CREATE_DEFAULT;

    /* Get the property list */
    if (NULL == (plist = H5P_object_verify(dcpl_id,H5P_DATASET_CREATE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a property list");

    new_dset->dcpl_id = H5P_copy_plist(plist);
    new_dset->ent.header = HADDR_UNDEF;

    /* Set return value */
    ret_value=new_dset;

done:
    if(ret_value==NULL) {
        if(new_dset!=NULL)
            H5FL_FREE(H5D_t,new_dset);
    } /* end if */

    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5D_create
 *
 * Purpose:	Creates a new dataset with name NAME in file F and associates
 *		with it a datatype TYPE for each element as stored in the
 *		file, dimensionality information or dataspace SPACE, and
 *		other miscellaneous properties CREATE_PARMS.  All arguments
 *		are deep-copied before being associated with the new dataset,
 *		so the caller is free to subsequently modify them without
 *		affecting the dataset.
 *
 * Return:	Success:	Pointer to a new dataset
 *
 *		Failure:	NULL
 *
 * Errors:
 *		DATASET	  CANTINIT	Can't update dataset header. 
 *		DATASET	  CANTINIT	Problem with the dataset name. 
 *		DATASET	  CANTINIT	Fail in file space allocation for
 *					chunks
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 *	Robb Matzke, 9 Jun 1998
 *	The data space message is no longer cached in the dataset struct.
 *
 * 	Robb Matzke, 27 Jul 1998
 *	Added the MTIME message to the dataset object header.
 *
 * 	Robb Matzke, 1999-10-14
 *	The names for the external file list are entered into the heap hear
 *	instead of when the efl message is encoded, preventing a possible
 *	infinite recursion situation.
 *
 *      Raymond Lu
 *      Tuesday, October 2, 2001
 *      Changed the way to retrieve and set property for generic property 
 *      list.
 *
 *	Raymond Lu, 26 Feb 2002
 *	A new fill value message is added.  Two properties, space allocation
 *	time and fill value writing time, govern space allocation and fill
 *      value writing.   
 *	
 *-------------------------------------------------------------------------
 */
H5D_t *
H5D_create(H5G_entry_t *loc, const char *name, const H5T_t *type, 
           const H5S_t *space, hid_t dcpl_id)
{
    H5D_t		*new_dset = NULL;
    H5D_t		*ret_value = NULL;
    int		        i, ndims;
    hsize_t 		comp_data_size;
    unsigned		u;
    hsize_t		max_dim[H5O_LAYOUT_NDIMS]={0};
    H5O_efl_t           efl;
    H5F_t		*f = NULL;
    H5O_pline_t         dcpl_pline;
    H5D_layout_t        dcpl_layout;
    int                 chunk_ndims = 0;
    hsize_t             chunk_size[32]={0};
    H5D_space_time_t    space_time;
    H5D_fill_time_t	fill_time;
    H5O_fill_t		fill_prop={NULL,0,NULL};
    H5O_fill_new_t      fill={NULL, 0, NULL, H5D_SPACE_ALLOC_LATE, H5D_FILL_TIME_ALLOC, TRUE};
    H5D_fill_value_t	fill_status;
    H5P_genplist_t 	*plist;      /* Property list */
    H5P_genplist_t 	*new_plist;  /* New Property list */

    FUNC_ENTER_NOAPI(H5D_create, NULL);

    /* check args */
    assert (loc);
    assert (name && *name);
    assert (type);
    assert (space);

    /* Get property list object */
    if (NULL == (plist = H5P_object_verify(dcpl_id,H5P_DATASET_CREATE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "can't get dataset creation property list");

    if(H5P_get(plist, H5D_CRT_DATA_PIPELINE_NAME, &dcpl_pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't retrieve pipeline filter");
    if(H5P_get(plist, H5D_CRT_LAYOUT_NAME, &dcpl_layout) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't retrieve layout");
    if(H5P_get(plist, H5D_CRT_SPACE_TIME_NAME, &space_time) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't retrieve space allocation time");
    if(dcpl_pline.nfilters > 0 && H5D_CHUNKED != dcpl_layout)
        HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, NULL, "filters can only be used with chunked layout");
    if(dcpl_layout==H5D_COMPACT && space_time==H5D_SPACE_ALLOC_LATE)
        HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, NULL, "compact dataset doesn't support late space allocation");

    /* What file is the dataset being added to? */
    if (NULL==(f=H5G_insertion_file(loc, name)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to locate insertion point");

#ifdef H5_HAVE_PARALLEL
    /* If MPIO or MPIPOSIX is used, no filter support yet. */
    if((IS_H5FD_MPIO(f) || IS_H5FD_MPIPOSIX(f)) && dcpl_pline.nfilters > 0) 
        HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, NULL, "Parallel I/O does not support filters yet");
#endif /*H5_HAVE_PARALLEL*/
   
    if(H5P_get(plist, H5D_CRT_FILL_TIME_NAME, &fill_time) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't retrieve fill time");

    if(fill_time==H5D_FILL_TIME_NEVER && H5T_detect_class(type, H5T_VLEN))
        HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, NULL, "Dataset doesn't support VL datatype when fill value is not defined");
 
    /* Initialize the dataset object */
    if(NULL == (new_dset = H5D_new(dcpl_id)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Check if the datatype is "sensible" for use in a dataset */
    if(H5T_is_sensible(type)!=TRUE)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "datatype is not sensible");

    /* Copy datatype for dataset */
    if((new_dset->type = H5T_copy(type, H5T_COPY_ALL))==NULL)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCOPY, NULL, "can't copy datatype");

    /* Mark any VL datatypes as being on disk now */
    if (H5T_vlen_mark(new_dset->type, f, H5T_VLEN_DISK)<0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "invalid VL location");

    /* Get new dataset's property list object */
    if (NULL == (new_plist = H5I_object(new_dset->dcpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "can't get dataset creation property list");

    if(H5P_get(new_plist, H5D_CRT_CHUNK_DIM_NAME, &chunk_ndims) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't retrieve layout");
    
    if(H5P_get(new_plist, H5D_CRT_EXT_FILE_LIST_NAME, &efl) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't retrieve external file list");

    /* Total raw data size */
    if(H5P_get(new_plist, H5D_CRT_LAYOUT_NAME, &(new_dset->layout.type)) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't retrieve layout");
    new_dset->layout.ndims = H5S_get_simple_extent_ndims(space) + 1;
    assert((unsigned)(new_dset->layout.ndims) <= NELMTS(new_dset->layout.dim));
    new_dset->layout.dim[new_dset->layout.ndims-1] = H5T_get_size(new_dset->type);    

    switch (new_dset->layout.type) {
        case H5D_CONTIGUOUS:
            /*
             * The maximum size of the dataset cannot exceed the storage size.
             * Also, only the slowest varying dimension of a simple data space
             * can be extendible.
             */
	    if ((ndims=H5S_get_simple_extent_dims(space, new_dset->layout.dim, max_dim))<0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to initialize contiguous storage");
            for (i=1; i<ndims; i++) {
                if (max_dim[i]>new_dset->layout.dim[i])
                    HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, NULL, "only the first dimension can be extendible");
            }
            if (efl.nused>0) {
                hsize_t max_points = H5S_get_npoints_max (space);
                hsize_t max_storage = H5O_efl_total_size (&efl);

                if (H5S_UNLIMITED==max_points) {
                    if (H5O_EFL_UNLIMITED!=max_storage)
                        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, NULL, "unlimited data space but finite storage");
                } else if (max_points * H5T_get_size (type) < max_points) {
                    HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, NULL, "data space * type size overflowed");
                } else if (max_points * H5T_get_size (type) > max_storage) {
                    HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, NULL, "data space size exceeds external storage size");
                }
            } else if (ndims>0 && max_dim[0]>new_dset->layout.dim[0]) {
                HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, NULL, "extendible contiguous non-external dataset");
            }
            break;

        case H5D_CHUNKED:
            /*
             * Chunked storage allows any type of data space extension, so we
             * don't even bother checking.
             */
            if(chunk_ndims != H5S_get_simple_extent_ndims(space))
                HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, NULL, "dimensionality of chunks doesn't match the data space");
            if (efl.nused>0)
                HGOTO_ERROR (H5E_DATASET, H5E_BADVALUE, NULL, "external storage not supported with chunked layout");

            /*
             * The chunk size of a dimension with a fixed size cannot exceed
             * the maximum dimension size 
             */
            if(H5P_get(new_plist, H5D_CRT_CHUNK_SIZE_NAME, chunk_size) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't retrieve chunk size");

            if (H5S_get_simple_extent_dims(space, NULL, max_dim)<0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to query maximum dimensions");
            for (u=0; u<new_dset->layout.ndims-1; u++) {
	        if(max_dim[u] != H5S_UNLIMITED && max_dim[u] < chunk_size[u])
                    HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, NULL, "chunk size must be <= maximum dimension size for fixed-sized dimensions");
            }

            /* Set the dataset's chunk sizes from the property list's chunk sizes */
            for (u=0; u<new_dset->layout.ndims-1; u++)
                new_dset->layout.dim[u] = chunk_size[u];
            break;

        case H5D_COMPACT:
            /*
             * Compact dataset is stored in dataset object header message of 
             * layout.
             */
            new_dset->layout.size = H5S_get_simple_extent_npoints(space) *
                                    H5T_get_size(type);
	    /* Verify data size is smaller than maximum header message size
	     * (64KB) minus other layout message fields.
	     */
            comp_data_size=H5O_MAX_SIZE-H5O_layout_meta_size(f, &(new_dset->layout));
            if(new_dset->layout.size > comp_data_size)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "compact dataset size is bigger than header message maximum size");
            if ((ndims=H5S_get_simple_extent_dims(space, new_dset->layout.dim, max_dim))<0) 
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to initialize dimension size of compact dataset storage");
            /* remember to check if size is small enough to fit header message */
            break;
            
        default:
            HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, NULL, "not implemented yet");
    } /* end switch */

    /* Create (open for write access) an object header */
    if (H5O_create(f, 256, &(new_dset->ent)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to create dataset object header");

    /* Retrieve properties of fill value and others.  Copy them into new fill
     * value struct.  Convert the fill value to the dataset type and write 
     * the message */
    if(H5P_get(new_plist, H5D_CRT_SPACE_TIME_NAME, &space_time) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't retrieve space allocation time");
    if(H5P_get(new_plist, H5D_CRT_FILL_TIME_NAME, &fill_time) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't retrieve fill time");
    if(H5P_fill_value_defined(new_plist, &fill_status)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't tell if fill value defined");
    if(fill_status== H5D_FILL_VALUE_DEFAULT || fill_status==H5D_FILL_VALUE_USER_DEFINED) {
	if(H5P_get(new_plist, H5D_CRT_FILL_VALUE_NAME, &fill_prop) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't retrieve fill value");
        if(NULL==H5O_copy(H5O_FILL, &fill_prop, &fill))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT,NULL, "unable to copy fill value");
        if (fill_prop.buf && fill_prop.size>0 && H5O_fill_convert(&fill, new_dset->type) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to convert fill value to dataset type");
	fill.fill_defined = TRUE;
    } else if(fill_status==H5D_FILL_VALUE_UNDEFINED) {
	fill.size = -1;
 	fill.type = fill.buf = NULL;
 	fill.fill_defined = FALSE;
    } else {
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, NULL, "unable to determine if fill value is defined");
    } /* end else */
    fill.space_time = space_time;
    fill.fill_time   = fill_time;
   
    if(fill.fill_defined == FALSE && fill_time != H5D_FILL_TIME_NEVER)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT,NULL, "unable to create dataset");

    /* Write new fill value message */
    if (H5O_modify(&(new_dset->ent), H5O_FILL_NEW, 0, H5O_FLAG_CONSTANT, &fill) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to update fill value header message");        
    H5O_reset(H5O_FILL, &fill_prop);
    if(fill.buf && (NULL==H5O_copy(H5O_FILL, &fill, &fill_prop)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT,NULL,"unable to copy fill value");
    H5O_reset(H5O_FILL_NEW, &fill);

    /* Write old fill value */
    if (fill_prop.buf && H5O_modify(&(new_dset->ent), H5O_FILL, 0, H5O_FLAG_CONSTANT, &fill_prop) < 0)        
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to update fill value header message");
    if(H5P_set(new_plist, H5D_CRT_FILL_VALUE_NAME, &fill_prop) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, NULL, "can't set fill value");  

    /* Update the type and space header messages */
    if (H5O_modify(&(new_dset->ent), H5O_DTYPE, 0,
            H5O_FLAG_CONSTANT|H5O_FLAG_SHARED, new_dset->type)<0 ||
            H5S_modify(&(new_dset->ent), space) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to update type or space header messages");

    /* Update the filters message */
    if(H5P_get(new_plist, H5D_CRT_DATA_PIPELINE_NAME, &dcpl_pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "Can't retrieve pipeline filter");
    if (dcpl_pline.nfilters>0 && H5O_modify (&(new_dset->ent), H5O_PLINE, 0, H5O_FLAG_CONSTANT, &dcpl_pline) < 0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, NULL, "unable to update filter header message");

    /*
     * Add a modification time message.
     */
    if (H5O_touch(&(new_dset->ent), TRUE)<0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to update modification time message");
    
    /* Give the dataset a name */
    if (H5G_insert(loc, name, &(new_dset->ent)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to name dataset");

    /*
     * Allocate storage.  We assume that external storage is already
     * allocated by the caller, or at least will be before I/O is performed.
     * For parallelization, space is always allocated now except using 
     * external storage. For contiguous layout, space is allocated now if 
     * space allocate time is early; otherwise delay allocation until H5Dwrite.
     * For compact dataset, space is allocated regardless of space allocation
     * time.
     */
#ifdef H5_HAVE_PARALLEL
    if (0==efl.nused) {
        if(H5F_arr_create(f, &(new_dset->layout))<0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to initialize storage");
    } /* end if */
    else
	new_dset->layout.addr = HADDR_UNDEF;
#else /*H5_HAVE_PARALLEL*/
    if (0==efl.nused) {
        if(dcpl_layout==H5D_CHUNKED || dcpl_layout==H5D_COMPACT || 
            (dcpl_layout==H5D_CONTIGUOUS && space_time==H5D_SPACE_ALLOC_EARLY)) {
            if (H5F_arr_create(f, &(new_dset->layout))<0)
            	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to initialize storage"); 
        } /* end if */
        else
	    new_dset->layout.addr = HADDR_UNDEF;
    } /* end if */
    else
        new_dset->layout.addr = HADDR_UNDEF;
#endif /*H5_HAVE_PARALLEL*/

    /* Update external storage message */
    if (efl.nused>0) {
        size_t heap_size = H5HL_ALIGN (1);

        for (i=0; i<efl.nused; i++)
            heap_size += H5HL_ALIGN (HDstrlen (efl.slot[i].name)+1);
        if (H5HL_create (f, heap_size, &(efl.heap_addr)/*out*/)<0 ||
                (size_t)(-1)==H5HL_insert(f, efl.heap_addr, 1, ""))
            HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, NULL, "unable to create external file list name heap");
        for (i=0; i<efl.nused; i++) {
            size_t offset = H5HL_insert(f, efl.heap_addr,
                        HDstrlen(efl.slot[i].name)+1, efl.slot[i].name);
            assert(0==efl.slot[i].name_offset);
            if ((size_t)(-1)==offset)
                HGOTO_ERROR(H5E_EFL, H5E_CANTINIT, NULL, "unable to insert URL into name heap");
            efl.slot[i].name_offset = offset;
        } /* end for */
        if (H5O_modify (&(new_dset->ent), H5O_EFL, 0, H5O_FLAG_CONSTANT, &efl)<0)
            HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, NULL, "unable to update external file list message");
    } /* end if */

    /* Initialize the raw data when it's 
     *  1. Parallel I/O
     *  2. layout is contiguous, space allocation time is early, fill value 
     *     writing time is upon allocation 
     *  3. external space is treated the same as internal except it's always
     *     assumed as early for space allocation time
     *  4. compact dataset and fill value writing time is upon allocation
     */
#ifdef H5_HAVE_PARALLEL
    if (H5D_init_storage(new_dset, space)<0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to initialize storage");
#else /*H5_HAVE_PARALLEL*/
    if(fill_time==H5D_FILL_TIME_ALLOC) {
        if((dcpl_layout==H5D_CONTIGUOUS && space_time==H5D_SPACE_ALLOC_EARLY)
            || (dcpl_layout==H5D_CHUNKED && space_time==H5D_SPACE_ALLOC_EARLY) 
            || dcpl_layout==H5D_COMPACT ) {
	    if (H5D_init_storage(new_dset, space)<0) {
	        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to initialize storage");
            }
        }
     }
#endif /*H5_HAVE_PARALLEL*/

    /* Update layout message */
    if (H5D_COMPACT != new_dset->layout.type && H5O_modify (&(new_dset->ent), H5O_LAYOUT, 0, 0, &(new_dset->layout))<0)
         HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, NULL, "unable to update layout"); 
    if (H5D_COMPACT == new_dset->layout.type)
         new_dset->layout.dirty = TRUE;
    
    /* Success */
    ret_value = new_dset;

done:
    if (!ret_value && new_dset) {
        if (new_dset->type)
            H5T_close(new_dset->type);
        if (H5F_addr_defined(new_dset->ent.header))
            H5O_close(&(new_dset->ent));
        new_dset->ent.file = NULL;
        H5FL_FREE(H5D_t,new_dset);
    }

    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5D_isa
 *
 * Purpose:	Determines if an object has the requisite messages for being
 *		a dataset.
 *
 * Return:	Success:	TRUE if the required dataset messages are
 *				present; FALSE otherwise.
 *
 *		Failure:	FAIL if the existence of certain messages
 *				cannot be determined.
 *
 * Programmer:	Robb Matzke
 *              Monday, November  2, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5D_isa(H5G_entry_t *ent)
{
    htri_t	exists;
    htri_t	ret_value=TRUE;         /* Return value */
    
    FUNC_ENTER_NOAPI(H5D_isa, FAIL);

    assert(ent);

    /* Data type */
    if ((exists=H5O_exists(ent, H5O_DTYPE, 0))<0) {
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to read object header");
    } else if (!exists) {
	HGOTO_DONE(FALSE);
    }

    /* Layout */
    if ((exists=H5O_exists(ent, H5O_LAYOUT, 0))<0) {
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to read object header");
    } else if (!exists) {
	HGOTO_DONE(FALSE);
    }
    

done:
    FUNC_LEAVE(ret_value);
}


/*
 *------------------------------------------------------------------------- 
 * Function:	H5D_open
 *
 * Purpose:	Finds a dataset named NAME in file F and builds a descriptor
 *		for it, opening it for access.
 *
 * Return:	Success:	Pointer to a new dataset descriptor.
 *
 *		Failure:	NULL
 *
 * Errors:
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 * 	Robb Matzke, 9 Jun 1998
 *	The data space message is no longer cached in the dataset struct.
 *	
 *  	Quincey Koziol, 12 Oct 1998
 *  	Moved guts of function into H5D_open_oid
 *
 *-------------------------------------------------------------------------
 */
H5D_t *
H5D_open(H5G_entry_t *loc, const char *name)
{
    H5D_t	*dataset = NULL;	/*the dataset which was found	*/
    H5D_t	*ret_value = NULL;	/*return value			*/
    H5G_entry_t ent;            	/*dataset symbol table entry	*/
    
    FUNC_ENTER_NOAPI(H5D_open, NULL);

    /* check args */
    assert (loc);
    assert (name && *name);
    
    /* Find the dataset object */
    if (H5G_find(loc, name, NULL, &ent) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_NOTFOUND, NULL, "not found");

    /* Open the dataset object */
    if ((dataset=H5D_open_oid(&ent)) ==NULL)
        HGOTO_ERROR(H5E_DATASET, H5E_NOTFOUND, NULL, "not found");

    /* Success */
    ret_value = dataset;

done:
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5D_open_oid
 *
 * Purpose:	Opens a dataset for access.
 *
 * Return:	Dataset pointer on success, NULL on failure
 *
 * Errors:
 *
 * Programmer:	Quincey Koziol
 *		Monday, October 12, 1998
 *
 * Modifications:
 *
 *              Raymond Lu
 *              Tuesday, October 2, 2001
 *              Changed the way to set property for generic property list.
 *
 *		Raymond Lu
 *		Feb 26, 2002
 *		A new fill value message and two new properties are added.
 *
 *-------------------------------------------------------------------------
 */
H5D_t *
H5D_open_oid(H5G_entry_t *ent)
{
    H5D_t 	*dataset = NULL;	/*new dataset struct 		*/
    H5D_t 	*ret_value = NULL;	/*return value			*/
    H5S_t	*space = NULL;		/*data space			*/
    H5O_fill_new_t  fill = {NULL, 0, NULL, H5D_SPACE_ALLOC_LATE, H5D_FILL_TIME_ALLOC, TRUE}; 
    H5O_fill_t      fill_prop = {NULL, 0, NULL};
    H5O_pline_t pline;                  /* I/O pipeline information */
    H5O_efl_t   efl;                    /* External file information */
    H5D_layout_t layout;                /* Dataset layout */
    int         chunk_ndims;
    H5P_genplist_t *plist;      /* Property list */
    
    FUNC_ENTER_NOAPI(H5D_open_oid, NULL);

    /* check args */
    assert (ent);
    
    /* Allocate the dataset structure */
    if(NULL==(dataset = H5D_new(H5P_DEFAULT)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Copy over the symbol table information if it's provided */
    HDmemcpy(&(dataset->ent),ent,sizeof(H5G_entry_t));

    /* Find the dataset object */
    if (H5O_open(&(dataset->ent)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, NULL, "unable to open");
    
    /* Get the type and space */
    if (NULL==(dataset->type=H5O_read(&(dataset->ent), H5O_DTYPE, 0, NULL)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to load type info from dataset header");
    if (NULL==(space=H5S_read (&(dataset->ent))))
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, NULL, "unable to read data space info from dataset header");

    /* Get dataset creation property list object */
    if (NULL == (plist = H5I_object(dataset->dcpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "can't get dataset creation property list");

    /* Retrieve & release the previous fill-value settings */
    if(H5P_get(plist, H5D_CRT_FILL_VALUE_NAME, &fill_prop) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, NULL, "can't get fill value");
    H5O_reset(H5O_FILL, &fill_prop);

    /* Get the new fill value message */
    if(NULL == H5O_read(&(dataset->ent), H5O_FILL_NEW, 0, &fill)) {
        H5E_clear();
        HDmemset(&fill, 0, sizeof(fill));
    }
    if(fill.fill_defined) {
        if(NULL==H5O_copy(H5O_FILL, &fill, &fill_prop))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "can't copy fill value");
    } else {
	/* For compatibility with v1.4.  Retrieve the old fill value message.
 	 * If size is 0, make it -1 for undefined. */
        if(NULL == H5O_read(&(dataset->ent), H5O_FILL, 0, &fill_prop)) {
            H5E_clear();
            HDmemset(&fill_prop, 0, sizeof(fill_prop));
        }
        if(fill_prop.size == 0) {
	    fill_prop.type = fill_prop.buf = NULL;
	    fill_prop.size = (size_t)-1;
	}
    } /* end else */
	
    /* Set fill value properties */ 
    if(H5P_set(plist, H5D_CRT_FILL_VALUE_NAME, &fill_prop) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, NULL, "can't set fill value");
    if(H5P_set(plist, H5D_CRT_SPACE_TIME_NAME, &fill.space_time) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, NULL, "can't set fill value");
    if(H5P_set(plist, H5D_CRT_FILL_TIME_NAME, &fill.fill_time) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, NULL, "can't set fill value");

    /* Get the optional filters message */
    HDmemset(&pline,0,sizeof(H5O_pline_t));
    if(NULL == H5O_read(&(dataset->ent), H5O_PLINE, 0, &pline)) {
        H5E_clear();
        HDmemset(&pline, 0, sizeof(pline));
    }
    if(H5P_set(plist, H5D_CRT_DATA_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, NULL, "can't set pipeline");

#ifdef H5_HAVE_PARALLEL
    /* If MPIO or MPIPOSIX is used, no filter support yet. */
    if((IS_H5FD_MPIO(dataset->ent.file) || IS_H5FD_MPIPOSIX(dataset->ent.file)) && pline.nfilters > 0)
        HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, NULL, "Parallel IO does not support filters yet");
#endif /*H5_HAVE_PARALLEL*/
    
    /*
     * Get the raw data layout info.  It's actually stored in two locations:
     * the storage message of the dataset (dataset->storage) and certain
     * values are copied to the dataset create plist so the user can query
     * them.
     */
    if (NULL==H5O_read(&(dataset->ent), H5O_LAYOUT, 0, &(dataset->layout)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to read data layout message");
    switch (dataset->layout.type) {
        case H5D_CONTIGUOUS:
            layout = H5D_CONTIGUOUS; 
            if(H5P_set(plist, H5D_CRT_LAYOUT_NAME, &layout) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, NULL, "can't set layout"); 
            break;

        case H5D_CHUNKED:
        /*
         * Chunked storage.  The creation plist's dimension is one less than
         * the chunk dimension because the chunk includes a dimension for the
         * individual bytes of the data type.
         */
            layout = H5D_CHUNKED;
            chunk_ndims  = dataset->layout.ndims - 1;
   
            if(H5P_set(plist, H5D_CRT_LAYOUT_NAME, &layout) < 0)
                 HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, NULL, "can't set layout");
            if(H5P_set(plist, H5D_CRT_CHUNK_DIM_NAME, &chunk_ndims) < 0)
                 HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, NULL, "can't set chunk dimensions");
            if(H5P_set(plist, H5D_CRT_CHUNK_SIZE_NAME, dataset->layout.dim) < 0)
                 HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, NULL, "can't set chunk size");
            break;
            
        case H5D_COMPACT:
            layout = H5D_COMPACT;
            if(H5P_set(plist, H5D_CRT_LAYOUT_NAME, &layout) < 0)
                 HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, NULL, "can't set layout"); 
            break;
        default:
            HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, NULL, "not implemented yet");
    } /* end switch */

    /* Get the external file list message, which might not exist.  Space is
     * also undefined when space allocate time is H5D_SPACE_ALLOC_LATE. */
    if( !H5F_addr_defined(dataset->layout.addr)) {
        HDmemset(&efl,0,sizeof(H5O_efl_t));
        if(NULL != H5O_read(&(dataset->ent), H5O_EFL, 0, &efl))
            if(H5P_set(plist, H5D_CRT_EXT_FILE_LIST_NAME, &efl) < 0)
            	HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, NULL, "can't set external file list");
    }
    /*
     * Make sure all storage is properly initialized for chunked datasets.
     * This is especially important for parallel I/O where the B-tree must
     * be fully populated before I/O can happen.
     */
    if ((H5F_get_intent(dataset->ent.file) & H5F_ACC_RDWR) && H5D_CHUNKED==dataset->layout.type) {
        if (H5D_init_storage(dataset, space)<0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to initialize file storage");
    }

    /* Success */
    ret_value = dataset;

done:
    if (space)
        H5S_close(space);
    if (ret_value==NULL && dataset) {
        if (H5F_addr_defined(dataset->ent.header))
            H5O_close(&(dataset->ent));
        if (dataset->type)
            H5T_close(dataset->type);
        dataset->ent.file = NULL;
        H5FL_FREE(H5D_t,dataset);
    }
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5D_close
 *
 * Purpose:	Insures that all data has been saved to the file, closes the
 *		dataset object header, and frees all resources used by the
 *		descriptor.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Errors:
 *		DATASET	  CANTINIT	Couldn't free the type or space,
 *					but the dataset was freed anyway. 
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 *	Robb Matzke, 9 Jun 1998
 *	The data space message is no longer cached in the dataset struct.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_close(H5D_t *dataset)
{
    unsigned		    free_failed;
    herr_t                  ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5D_close, FAIL);

    /* check args */
    assert(dataset && dataset->ent.file);

    /*
     * Release data type and creation property list -- there isn't much we
     * can do if one of these fails, so we just continue.
     */
    free_failed = (H5T_close(dataset->type) < 0 ||
			H5I_dec_ref(dataset->dcpl_id) < 0);

    /*Update header message of layout for compact dataset.*/
    if(dataset->layout.type==H5D_COMPACT && dataset->layout.dirty) {
        if(H5O_modify(&(dataset->ent), H5O_LAYOUT, 0, 0, &(dataset->layout))<0)
            HRETURN_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to update layout message");
        dataset->layout.dirty = FALSE;
    }        

    /* Close the dataset object */
    H5O_close(&(dataset->ent));

    /*
     * Free memory.  Before freeing the memory set the file pointer to NULL.
     * We always check for a null file pointer in other H5D functions to be
     * sure we're not accessing an already freed dataset (see the assert()
     * above).
     */
    dataset->ent.file = NULL;
    H5FL_FREE(H5D_t,dataset);

    if (free_failed)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "couldn't free the type or creation property list, but the dataset was freed anyway.");

done:
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5D_read
 *
 * Purpose:	Reads (part of) a DATASET into application memory BUF. See
 *		H5Dread() for complete details.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 *	Robb Matzke, 1998-06-09
 *	The data space is no longer cached in the dataset struct.
 *
 * 	Robb Matzke, 1998-08-11
 *	Added timing calls around all the data space I/O functions.
 *
 * 	rky, 1998-09-18
 *	Added must_convert to do non-optimized read when necessary.
 *
 *  	Quincey Koziol, 1999-07-02
 *	Changed xfer_parms parameter to xfer plist parameter, so it
 *	could be passed to H5T_convert.
 *
 *	Albert Cheng, 2000-11-21
 *	Added the code that when it detects it is not safe to process a
 *	COLLECTIVE read request without hanging, it changes it to
 *	INDEPENDENT calls.
 *
 *	Albert Cheng, 2000-11-27
 *	Changed to use the optimized MPIO transfer for Collective calls only.
 *
 *      Raymond Lu, 2001-10-2
 *      Changed the way to retrieve property for generic property list.
 *
 *	Raymond Lu, 2002-2-26
 *	For the new fill value design, data space can either be allocated 
 *	or not allocated at this stage.  Fill value or data from space is
 *	returned to outgoing buffer.
 *
 *      QAK - 2002/04/02
 *      Removed the must_convert parameter and move preconditions to
 *      H5S_<foo>_opt_possible() routine
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_read(H5D_t *dataset, const H5T_t *mem_type, const H5S_t *mem_space,
	 const H5S_t *file_space, hid_t dxpl_id, void *buf/*out*/)
{
    hsize_t	nelmts;                 /*total number of elmts	*/
    hsize_t	smine_start;		/*strip mine start loc	*/
    hsize_t	n, smine_nelmts;	/*elements per strip	*/
    uint8_t	*tconv_buf = NULL;	/*data type conv buffer	*/
    uint8_t	*bkg_buf = NULL;	/*background buffer	*/
    H5T_path_t	*tpath = NULL;		/*type conversion info	*/
    hid_t	src_id = -1, dst_id = -1;/*temporary type atoms */
    H5S_conv_t	*sconv=NULL;	        /*space conversion funcs*/
    H5S_sel_iter_t mem_iter;            /*memory selection iteration info*/
    H5S_sel_iter_t bkg_iter;            /*background iteration info*/
    H5S_sel_iter_t file_iter;           /*file selection iteration info*/
    herr_t	ret_value = SUCCEED;	/*return value		*/
    herr_t	status;                 /*function return status*/
    size_t	src_type_size;		/*size of source type	*/
    size_t	dst_type_size;	        /*size of destination type*/
    size_t	target_size;		/*desired buffer size	*/
    hsize_t	request_nelmts;		/*requested strip mine	*/
    H5T_bkg_t	need_bkg;               /*type of background buf*/
    H5S_t	*free_this_space=NULL;  /*data space to free	*/
#ifdef H5_HAVE_PARALLEL
    H5FD_mpio_dxpl_t *dx = NULL;
    H5FD_mpio_xfer_t xfer_mode=H5FD_MPIO_INDEPENDENT;	/*xfer_mode for this request */
    hbool_t	xfer_mode_changed=0;	/*xfer_mode needs restore */
    hbool_t	doing_mpio=0;		/*This is an MPIO access */
#endif /*H5_HAVE_PARALLEL*/
#ifdef H5S_DEBUG
    H5_timer_t	timer;
#endif
    H5O_efl_t   efl;                    /* External File List info */
    H5O_fill_t  fill;                   /* Fill value info */
    H5D_fill_time_t	fill_time;      /* When to write the fill values */
    H5D_fill_value_t	fill_status;    /* Whether/How the fill value is defined */
    H5P_genplist_t *dx_plist=NULL;      /* Data transfer property list */
    H5P_genplist_t *dc_plist;           /* Dataset creation roperty list */
    unsigned	sconv_flags=0;	        /* Flags for the space conversion */

    FUNC_ENTER_NOAPI(H5D_read, FAIL);

    /* check args */
    assert(dataset && dataset->ent.file);
    assert(mem_type);
    assert(buf);

    /* Initialize these before any errors can occur */
    HDmemset(&mem_iter,0,sizeof(H5S_sel_iter_t));
    HDmemset(&bkg_iter,0,sizeof(H5S_sel_iter_t));
    HDmemset(&file_iter,0,sizeof(H5S_sel_iter_t));

    /* Get the dataset's creation property list */
    if (NULL == (dc_plist = H5I_object(dataset->dcpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation property list");

    /* Get the dataset transfer property list */
    if (NULL == (dx_plist = H5P_object_verify(dxpl_id,H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation property list");

    if (!file_space) {
        if (NULL==(free_this_space=H5S_read (&(dataset->ent))))
            HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to read data space from dataset header");
        file_space = free_this_space;
    } /* end if */
    if (!mem_space)
        mem_space = file_space;
    nelmts = (*mem_space->select.get_npoints)(mem_space);

#ifdef H5_HAVE_PARALLEL
    /* Collect Parallel I/O information for possible later use */
    if (H5FD_MPIO==H5P_peek_hid_t(dx_plist,H5D_XFER_VFL_ID_NAME)) {
	doing_mpio++;
	if (NULL == (dx=H5P_peek_voidp(dx_plist,H5D_XFER_VFL_INFO_NAME))) {
	    HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to retrieve data xfer info");
        } else {
	    xfer_mode = dx->xfer_mode;
        }
    } /* end if */
    /* Collective access is not permissible without the MPIO or MPIPOSIX driver */
    if (doing_mpio && xfer_mode==H5FD_MPIO_COLLECTIVE &&
            !(IS_H5FD_MPIO(dataset->ent.file) || IS_H5FD_MPIPOSIX(dataset->ent.file)))
        HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL, "collective access for MPIO & MPIPOSIX drivers only");

    /* Set the "parallel I/O possible" flag, for H5S_find() */
    if (H5S_mpi_opt_types_g && IS_H5FD_MPIO(dataset->ent.file)) {
	/* Only collective write should call this since it eventually
	 * calls MPI_File_set_view which is a collective call.
	 * See H5S_mpio_spaces_xfer() for details.
	 */
	if (doing_mpio && xfer_mode==H5FD_MPIO_COLLECTIVE)
            sconv_flags |= H5S_CONV_PAR_IO_POSSIBLE;
    } /* end if */
#endif /*H5_HAVE_PARALLEL*/

    /* Make certain that the number of elements in each selection is the same */
    if (nelmts!=(*file_space->select.get_npoints) (file_space))
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "src and dest data spaces have different sizes");

    /* Retrieve dataset properties */
    if(H5P_fill_value_defined(dc_plist, &fill_status)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't tell if fill value defined");
    if((fill_status==H5D_FILL_VALUE_DEFAULT || fill_status==H5D_FILL_VALUE_USER_DEFINED)
            && H5P_get(dc_plist, H5D_CRT_FILL_VALUE_NAME, &fill) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL,"can't retrieve fill value");
    if(H5P_get(dc_plist, H5D_CRT_FILL_TIME_NAME, &fill_time) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL,"can't retrieve fill time");
    if(H5P_get(dc_plist, H5D_CRT_EXT_FILE_LIST_NAME, &efl) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't retrieve external file list");

    /* If space hasn't been allocated and not using external storage, 
     * return fill value to buffer if fill time is upon allocation, or
     * do nothing if fill time is never.  If the dataset is compact and 
     * fill time is NEVER, there is no way to tell whether part of data
     * has been overwritten.  So just proceed in reading.     
     */ 
    if(nelmts > 0 && efl.nused==0 && dataset->layout.type==H5D_CONTIGUOUS 
            && dataset->layout.addr==HADDR_UNDEF) {

        /* Should be impossible, but check anyway... */
        if(fill_status == H5D_FILL_VALUE_UNDEFINED && fill_time == H5D_FILL_TIME_ALLOC)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "read failed: dataset doesn't exist, no data can be read");

        /* If we're never going to fill this dataset, just leave the junk in the user's buffer */
        if(fill_time == H5D_FILL_TIME_NEVER)
            HGOTO_DONE(SUCCEED);

        /* Go fill the user's selection with the dataset's fill value */
        if(H5D_fill(fill.buf,fill.type,buf,mem_type,mem_space)<0) {
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "filling buf failed");
        } else
            HGOTO_DONE(SUCCEED);
    } /* end if */

    /*
     * Locate the type conversion function and data space conversion
     * functions, and set up the element numbering information. If a data
     * type conversion is necessary then register data type atoms. Data type
     * conversion is necessary if the user has set the `need_bkg' to a high
     * enough value in xfer_parms since turning off data type conversion also
     * turns off background preservation.
     */
    if (NULL==(tpath=H5T_path_find(dataset->type, mem_type, NULL, NULL))) {
        HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unable to convert between src and dest data types");
    } else if (!H5T_IS_NOOP(tpath)) {
        if ((src_id=H5I_register(H5I_DATATYPE, H5T_copy(dataset->type, H5T_COPY_ALL)))<0 ||
                (dst_id=H5I_register(H5I_DATATYPE, H5T_copy(mem_type, H5T_COPY_ALL)))<0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTREGISTER, FAIL, "unable to register types for conversion");
    } /* end if */

    /* Set the storage flags for the space conversion check */
    switch(dataset->layout.type) {
        case H5D_COMPACT:
            sconv_flags |= H5S_CONV_STORAGE_COMPACT;
            break;

        case H5D_CONTIGUOUS:
            sconv_flags |= H5S_CONV_STORAGE_CONTIGUOUS;
            break;

        case H5D_CHUNKED:
            sconv_flags |= H5S_CONV_STORAGE_CHUNKED;
            break;

        default:
            assert(0 && "Unhandled layout type!");
    } /* end switch */

    /* Get dataspace functions */
    if (NULL==(sconv=H5S_find(mem_space, file_space, sconv_flags)))
        HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unable to convert from file to memory data space");

    /*
     * If there is no type conversion then read directly into the
     * application's buffer.  This saves at least one mem-to-mem copy.
     */
    if (H5T_IS_NOOP(tpath)) {
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
  	/* Sanity check dataset, then read it */
        assert(dataset->layout.addr!=HADDR_UNDEF || efl.nused>0 || dataset->layout.type==H5D_COMPACT);
        status = (sconv->read)(dataset->ent.file, &(dataset->layout), 
                     dc_plist, H5T_get_size(dataset->type), 
                     file_space, mem_space, dxpl_id, buf/*out*/);
#ifdef H5S_DEBUG
        H5_timer_end(&(sconv->stats[1].read_timer), &timer);
        sconv->stats[1].read_nbytes += nelmts * H5T_get_size(dataset->type);
        sconv->stats[1].read_ncalls++;
#endif

        /* Check return value from optimized read */
        if (status<0) {
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "optimized read failed");
        } else
	    /* direct xfer accomplished successfully */
            HGOTO_DONE(SUCCEED);
    } /* end if */
	
#ifdef H5_HAVE_PARALLEL
    /* The following may not handle a collective call correctly
     * since it does not ensure all processes can handle the read
     * request according to the MPI collective specification.
     * Do the collective request via independent mode.
     */
    if (doing_mpio && xfer_mode==H5FD_MPIO_COLLECTIVE) {
	/* Kludge: change the xfer_mode to independent, handle the request,
	 * then xfer_mode before return.
	 * Better way is to get a temporary data_xfer property with
	 * INDEPENDENT xfer_mode and pass it downwards.
	 */
	dx->xfer_mode = H5FD_MPIO_INDEPENDENT;
	xfer_mode_changed++;	/* restore it before return */
#ifdef H5D_DEBUG
	if (H5DEBUG(D)) {
	    fprintf(H5DEBUG(D),
		"H5D: Cannot handle this COLLECTIVE read request.  Do it via INDEPENDENT calls\n"
		"dx->xfermode was %d, changed to %d\n",
		xfer_mode, dx->xfer_mode);
	} /* end if */
#endif
    } /* end if */
#endif /*H5_HAVE_PARALLEL*/
    /*
     * This is the general case.
     */

    /* Compute element sizes and other parameters */
    src_type_size = H5T_get_size(dataset->type);
    dst_type_size = H5T_get_size(mem_type);
    target_size = H5P_peek_size_t(dx_plist,H5D_XFER_MAX_TEMP_BUF_NAME);
    request_nelmts = target_size / MAX(src_type_size, dst_type_size);

    /* Figure out the strip mine size. */
    if ((*file_space->select.iter_init)(file_space, src_type_size, &file_iter)<0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize file selection information");
    if ((*mem_space->select.iter_init)(mem_space, dst_type_size, &mem_iter)<0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize memory selection information");
    if ((*mem_space->select.iter_init)(mem_space, dst_type_size, &bkg_iter)<0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize background selection information");

    /* Sanity check elements in temporary buffer */
    if (request_nelmts<=0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "temporary buffer max size is too small");

    /*
     * Get a temporary buffer for type conversion unless the app has already
     * supplied one through the xfer properties. Instead of allocating a
     * buffer which is the exact size, we allocate the target size.  The
     * malloc() is usually less resource-intensive if we allocate/free the
     * same size over and over.
     */
    if (tpath->cdata.need_bkg) {
        /* Retrieve the bkgr buffer property */
        if(H5P_get(dx_plist, H5D_XFER_BKGR_BUF_TYPE_NAME, &need_bkg)<0)
            HGOTO_ERROR (H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve background buffer type");
        need_bkg = MAX(tpath->cdata.need_bkg, need_bkg);
    } else {
        need_bkg = H5T_BKG_NO; /*never needed even if app says yes*/
    } /* end else */
    if (NULL==(tconv_buf=H5P_peek_voidp(dx_plist,H5D_XFER_TCONV_BUF_NAME))) {
        /* Allocate temporary buffer */
        if((tconv_buf=H5FL_BLK_ALLOC(type_conv,target_size,0))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for type conversion");
    } /* end if */
    if (need_bkg && NULL==(bkg_buf=H5P_peek_voidp(dx_plist,H5D_XFER_BKGR_BUF_NAME))) {
        /* Allocate background buffer */
        H5_CHECK_OVERFLOW((request_nelmts*dst_type_size),hsize_t,size_t);
        if((bkg_buf=H5FL_BLK_ALLOC(type_conv,(size_t)(request_nelmts*dst_type_size),0))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for background conversion");
    } /* end if */

    /* Start strip mining... */
    for (smine_start=0; smine_start<nelmts; smine_start+=smine_nelmts) {
        /* Go figure out how many elements to read from the file */
        assert((*file_space->select.iter_nelmts)(&file_iter)==(nelmts-smine_start));
        smine_nelmts = MIN(request_nelmts, (nelmts-smine_start));
	
        /*
         * Gather the data from disk into the data type conversion
         * buffer. Also gather data from application to background buffer
         * if necessary.
         */
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
	/* Sanity check that space is allocated, then read data from it */ 
        assert(dataset->layout.addr!=HADDR_UNDEF || efl.nused > 0);
        n = H5S_select_fgath(dataset->ent.file, &(dataset->layout), 
                dc_plist, src_type_size, file_space, 
                &file_iter, smine_nelmts, dxpl_id, tconv_buf/*out*/);

#ifdef H5S_DEBUG
	H5_timer_end(&(sconv->stats[1].gath_timer), &timer);
	sconv->stats[1].gath_nbytes += n * src_type_size;
	sconv->stats[1].gath_ncalls++;
#endif
	if (n!=smine_nelmts)
            HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "file gather failed");
	
        if (need_bkg) {
#ifdef H5S_DEBUG
            H5_timer_begin(&timer);
#endif
            n = H5S_select_mgath(buf, dst_type_size, mem_space, &bkg_iter,
				 smine_nelmts, dxpl_id, bkg_buf/*out*/);
#ifdef H5S_DEBUG
            H5_timer_end(&(sconv->stats[1].bkg_timer), &timer);
            sconv->stats[1].bkg_nbytes += n * dst_type_size;
            sconv->stats[1].bkg_ncalls++;
#endif
            if (n!=smine_nelmts)
                HGOTO_ERROR (H5E_IO, H5E_READERROR, FAIL, "mem gather failed");
        } /* end if */
	
        /*
         * Perform data type conversion.
         */
        if (H5T_convert(tpath, src_id, dst_id, smine_nelmts, 0, 0, tconv_buf, bkg_buf, dxpl_id)<0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "data type conversion failed");

        /*
         * Scatter the data into memory.
         */
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
        status = H5S_select_mscat(tconv_buf, dst_type_size, mem_space,
                          &mem_iter, smine_nelmts, dxpl_id, buf/*out*/);
#ifdef H5S_DEBUG
	H5_timer_end(&(sconv->stats[1].scat_timer), &timer);
	sconv->stats[1].scat_nbytes += smine_nelmts * dst_type_size;
	sconv->stats[1].scat_ncalls++;
#endif
	if (status<0)
            HGOTO_ERROR (H5E_DATASET, H5E_READERROR, FAIL, "scatter failed");
	
    } /* end for */
    
done:
#ifdef H5_HAVE_PARALLEL
    /* restore xfer_mode due to the kludge */
    if (doing_mpio && xfer_mode_changed) {
#ifdef H5D_DEBUG
	if (H5DEBUG(D)) {
	    fprintf (H5DEBUG(D), "H5D: dx->xfermode was %d, restored to %d\n",
		dx->xfer_mode, xfer_mode);
	} /* end if */
#endif
	dx->xfer_mode = xfer_mode;
    } /* end if */
#endif /*H5_HAVE_PARALLEL*/
    /* Release selection iterators */
    (*file_space->select.iter_release)(&file_iter);
    (*mem_space->select.iter_release)(&mem_iter);
    (*mem_space->select.iter_release)(&bkg_iter);

    if (src_id >= 0)
        H5I_dec_ref(src_id);
    if (dst_id >= 0)
        H5I_dec_ref(dst_id);
    assert(dx_plist);
    if (tconv_buf && NULL==H5P_peek_voidp(dx_plist,H5D_XFER_TCONV_BUF_NAME))
        H5FL_BLK_FREE(type_conv,tconv_buf);
    if (bkg_buf && NULL==H5P_peek_voidp(dx_plist,H5D_XFER_BKGR_BUF_NAME))
        H5FL_BLK_FREE(type_conv,bkg_buf);
    if (free_this_space)
        H5S_close(free_this_space);
    FUNC_LEAVE(ret_value);
} /* end H5D_read() */


/*-------------------------------------------------------------------------
 * Function:	H5D_write
 *
 * Purpose:	Writes (part of) a DATASET to a file from application memory
 *		BUF. See H5Dwrite() for complete details.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 * 	Robb Matzke, 9 Jun 1998
 *	The data space is no longer cached in the dataset struct.
 *
 * 	rky 980918
 *	Added must_convert to do non-optimized read when necessary.
 *
 *      Quincey Koziol, 2 July 1999
 *      Changed xfer_parms parameter to xfer plist parameter, so it could
 *      be passed to H5T_convert
 *
 *	Albert Cheng, 2000-11-21
 *	Added the code that when it detects it is not safe to process a
 *	COLLECTIVE write request without hanging, it changes it to
 *	INDEPENDENT calls.
 *      
 *	Albert Cheng, 2000-11-27
 *	Changed to use the optimized MPIO transfer for Collective calls only.
 *
 *      Raymond Lu, 2001-10-2
 *      Changed the way to retrieve property for generic property list.
 *
 *	Raymond Lu, 2002-2-26
 *	For the new fill value design, space may not be allocated until 
 *	this function is called.  Allocate and initialize space if it 
 *	hasn't been.
 *
 *      QAK - 2002/04/02
 *      Removed the must_convert parameter and move preconditions to
 *      H5S_<foo>_opt_possible() routine
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_write(H5D_t *dataset, const H5T_t *mem_type, const H5S_t *mem_space,
	  const H5S_t *file_space, hid_t dxpl_id, const void *buf)
{
    hsize_t	nelmts;                 /*total number of elmts	*/
    hsize_t	smine_start;		/*strip mine start loc	*/
    hsize_t	n, smine_nelmts;	/*elements per strip	*/
    uint8_t	*tconv_buf = NULL;	/*data type conv buffer	*/
    uint8_t	*bkg_buf = NULL;	/*background buffer	*/
    H5T_path_t	*tpath = NULL;		/*type conversion info	*/
    hid_t	src_id = -1, dst_id = -1;/*temporary type atoms */
    H5S_conv_t	*sconv=NULL;		/*space conversion funcs*/
    H5S_sel_iter_t mem_iter;            /*memory selection iteration info*/
    H5S_sel_iter_t bkg_iter;            /*background iteration info*/
    H5S_sel_iter_t file_iter;           /*file selection iteration info*/
    herr_t	ret_value = SUCCEED;	/*return value		*/
    herr_t	status;                 /*function return status*/
    size_t	src_type_size;		/*size of source type	*/
    size_t	dst_type_size;	        /*size of destination type*/
    size_t	target_size;		/*desired buffer size	*/
    hsize_t	request_nelmts;		/*requested strip mine	*/
    H5T_bkg_t	need_bkg;		/*type of background buf*/
    H5S_t	*free_this_space=NULL;	/*data space to free	*/
#ifdef H5_HAVE_PARALLEL
    H5FD_mpio_dxpl_t	*dx = NULL;
    H5FD_mpio_xfer_t xfer_mode=H5FD_MPIO_INDEPENDENT;	/*xfer_mode for this request */
    hbool_t	xfer_mode_changed=0;	/*xfer_mode needs restore */
    hbool_t	doing_mpio=0;		/*This is an MPIO access */
#endif /*H5_HAVE_PARALLEL*/
#ifdef H5S_DEBUG
    H5_timer_t	timer;
#endif
    H5O_efl_t   efl;                    /* External File List info */
    H5O_fill_t  fill;                   /* Fill value info */
    H5D_fill_time_t	fill_time;      /* When to write the fill values */
    H5P_genplist_t *dx_plist=NULL;      /* Data transfer property list */
    H5P_genplist_t *dc_plist;           /* Dataset creation roperty list */
    unsigned	sconv_flags=0;	        /* Flags for the space conversion */

    FUNC_ENTER_NOAPI(H5D_write, FAIL);

    /* check args */
    assert(dataset && dataset->ent.file);
    assert(mem_type);
    assert(buf);

    /* Initialize these before any errors can occur */
    HDmemset(&mem_iter,0,sizeof(H5S_sel_iter_t));
    HDmemset(&bkg_iter,0,sizeof(H5S_sel_iter_t));
    HDmemset(&file_iter,0,sizeof(H5S_sel_iter_t));

    /* Get the dataset's creation property list */
    if (NULL == (dc_plist = H5I_object(dataset->dcpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation property list");

    /* Get the dataset transfer property list */
    if (NULL == (dx_plist = H5P_object_verify(dxpl_id,H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation property list");

    if (!file_space) {
        if (NULL==(free_this_space=H5S_read (&(dataset->ent))))
            HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to read data space from dataset header");
        file_space = free_this_space;
    } /* end if */
    if (!mem_space)                                                                                                                      
        mem_space = file_space;                                                                                                          
    nelmts = (*mem_space->select.get_npoints)(mem_space);            

#ifdef H5_HAVE_PARALLEL
    /* If MPIO or MPIPOSIX is used, no VL datatype support yet. */
    /* This is because they use the global heap in the file and we don't */
    /* support parallel access of that yet */
    if ( (IS_H5FD_MPIO(dataset->ent.file) || IS_H5FD_MPIPOSIX(dataset->ent.file)) && H5T_get_class(mem_type)==H5T_VLEN)
        HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL, "Parallel IO does not support writing VL datatypes yet");
#endif /*H5_HAVE_PARALLEL*/
#ifdef H5_HAVE_PARALLEL
    /* If MPIO or MPIPOSIX is used, no dataset region reference datatype support yet. */
    /* This is because they use the global heap in the file and we don't */
    /* support parallel access of that yet */
    if ((IS_H5FD_MPIO(dataset->ent.file) || IS_H5FD_MPIPOSIX(dataset->ent.file)) &&
            H5T_get_class(mem_type)==H5T_REFERENCE &&
            H5T_get_ref_type(mem_type)==H5R_DATASET_REGION)
        HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL, "Parallel IO does not support writing region reference datatypes yet");
#endif /*H5_HAVE_PARALLEL*/

    if (0==(H5F_get_intent(dataset->ent.file) & H5F_ACC_RDWR))
	HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "no write intent on file");

#ifdef H5_HAVE_PARALLEL
    /* Collect Parallel I/O information for possible later use */
    if (H5FD_MPIO==H5P_peek_hid_t(dx_plist,H5D_XFER_VFL_ID_NAME)) {
	doing_mpio++;
	if (NULL==(dx=H5P_peek_voidp(dx_plist,H5D_XFER_VFL_INFO_NAME))) {
	    HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to retrieve data xfer info");
        } else {
	    xfer_mode = dx->xfer_mode;
        }
    } /* end if */
    
    /* Collective access is not permissible without the MPIO or MPIPOSIX driver */
    if (doing_mpio && xfer_mode==H5FD_MPIO_COLLECTIVE &&
            !(IS_H5FD_MPIO(dataset->ent.file) || IS_H5FD_MPIPOSIX(dataset->ent.file)))
        HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL, "collective access for MPIO driver only");

    /* If dataset is compact, collective access is only allowed when file space
     * selection is H5S_ALL */
    if(doing_mpio && xfer_mode==H5FD_MPIO_COLLECTIVE
        && dataset->layout.type==H5D_COMPACT) { 
        if(file_space->select.type != H5S_SEL_ALL)    
            HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL, "collective access to compact dataset doesn't support partial access");        
    }
    
    /* Set the "parallel I/O possible" flag, for H5S_find() */
    if (H5S_mpi_opt_types_g && IS_H5FD_MPIO(dataset->ent.file)) {
	/* Only collective write should call this since it eventually
	 * calls MPI_File_set_view which is a collective call.
	 * See H5S_mpio_spaces_xfer() for details.
	 */
	if (doing_mpio && xfer_mode==H5FD_MPIO_COLLECTIVE)
            sconv_flags |= H5S_CONV_PAR_IO_POSSIBLE;
    } /* end if */
#endif /*H5_HAVE_PARALLEL*/

    /* Make certain that the number of elements in each selection is the same */
    if (nelmts!=(*file_space->select.get_npoints) (file_space))
	HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "src and dest data spaces have different sizes");

    /* Retrieve dataset properties */
    if(H5P_get(dc_plist, H5D_CRT_FILL_VALUE_NAME, &fill) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't retrieve fill value");
    if(H5P_get(dc_plist, H5D_CRT_FILL_TIME_NAME, &fill_time) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't retrieve fill time");
    if(H5P_get(dc_plist, H5D_CRT_EXT_FILE_LIST_NAME, &efl) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't retrieve external file list");

    /* Allocate data space and initialize it if it hasn't been.  Also modify
     * header message for layout(this is only for forward compatibility). */
    if(nelmts > 0 && efl.nused==0 && dataset->layout.type==H5D_CONTIGUOUS &&
            dataset->layout.addr==HADDR_UNDEF) {
 	/* Allocate storage */
        if(H5F_arr_create(dataset->ent.file, &(dataset->layout))<0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize storage");

        /* Initialize raw data */
        if(fill_time==H5D_FILL_TIME_ALLOC)
            if(H5D_init_storage(dataset, file_space) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize dataset with fill value");

        /* Update layout message */
        if (H5O_modify (&(dataset->ent), H5O_LAYOUT, 0, H5O_FLAG_CONSTANT, 
	    		&(dataset->layout)) < 0)
            HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to update layout message");
    } /* end if */

    /*
     * Locate the type conversion function and data space conversion
     * functions, and set up the element numbering information. If a data
     * type conversion is necessary then register data type atoms. Data type
     * conversion is necessary if the user has set the `need_bkg' to a high
     * enough value in xfer_parms since turning off data type conversion also
     * turns off background preservation.
     */
    if (NULL==(tpath=H5T_path_find(mem_type, dataset->type, NULL, NULL))) {
	HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unable to convert between src and dest data types");
    } else if (!H5T_IS_NOOP(tpath)) {
	if ((src_id = H5I_register(H5I_DATATYPE, H5T_copy(mem_type, H5T_COPY_ALL)))<0 ||
                (dst_id = H5I_register(H5I_DATATYPE, H5T_copy(dataset->type, H5T_COPY_ALL)))<0)
	    HGOTO_ERROR(H5E_DATASET, H5E_CANTREGISTER, FAIL, "unable to register types for conversion");
    } /* end if */

    /* Set the storage flags for the space conversion check */
    switch(dataset->layout.type) {
        case H5D_COMPACT:
            sconv_flags |= H5S_CONV_STORAGE_COMPACT;
            break;

        case H5D_CONTIGUOUS:
            sconv_flags |= H5S_CONV_STORAGE_CONTIGUOUS;
            break;

        case H5D_CHUNKED:
            sconv_flags |= H5S_CONV_STORAGE_CHUNKED;
            break;

        default:
            assert(0 && "Unhandled layout type!");
    } /* end switch */

    /* Get dataspace functions */
    if (NULL==(sconv=H5S_find(mem_space, file_space, sconv_flags)))
	HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unable to convert from memory to file data space");

    /*
     * If there is no type conversion then write directly from the
     * application's buffer.  This saves at least one mem-to-mem copy.
     */
    if (H5T_IS_NOOP(tpath)) {
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
        status = (sconv->write)(dataset->ent.file, &(dataset->layout),
                        dc_plist, H5T_get_size(dataset->type),
                        file_space, mem_space, dxpl_id, buf);
#ifdef H5S_DEBUG
	    H5_timer_end(&(sconv->stats[0].write_timer), &timer);
	    sconv->stats[0].write_nbytes += nelmts * H5T_get_size(mem_type);
	    sconv->stats[0].write_ncalls++;
#endif

        /* Check return value from optimized write */
        if (status<0) {
	    HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "optimized write failed");
	} else
	    /* direct xfer accomplished successfully */
	    HGOTO_DONE(SUCCEED);
    } /* end if */

#ifdef H5_HAVE_PARALLEL
    /* The following may not handle a collective call correctly
     * since it does not ensure all processes can handle the write
     * request according to the MPI collective specification.
     * Do the collective request via independent mode.
     */
    if (doing_mpio && xfer_mode==H5FD_MPIO_COLLECTIVE) {
	/* Kludge: change the xfer_mode to independent, handle the request,
	 * then xfer_mode before return.
	 * Better way is to get a temporary data_xfer property with
	 * INDEPENDENT xfer_mode and pass it downwards.
	 */
	dx->xfer_mode = H5FD_MPIO_INDEPENDENT;
	xfer_mode_changed++;	/* restore it before return */
#ifdef H5D_DEBUG
	if (H5DEBUG(D)) {
	    fprintf(H5DEBUG(D),
		"H5D: Cannot handle this COLLECTIVE write request.  Do it via INDEPENDENT calls\n"
		"dx->xfermode was %d, changed to %d\n", xfer_mode, dx->xfer_mode);
	} /* end if */
#endif
    } /* end if */
#endif /*H5_HAVE_PARALLEL*/
    /*
     * This is the general case.
     */

    /* Compute element sizes and other parameters */
    src_type_size = H5T_get_size(mem_type);
    dst_type_size = H5T_get_size(dataset->type);
    target_size = H5P_peek_size_t(dx_plist,H5D_XFER_MAX_TEMP_BUF_NAME);
    request_nelmts = target_size / MAX (src_type_size, dst_type_size);

    /* Sanity check elements in temporary buffer */
    if (request_nelmts<=0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "temporary buffer max size is too small");

    /* Figure out the strip mine size. */
    if ((*file_space->select.iter_init)(file_space, dst_type_size, &file_iter)<0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize file selection information");
    if ((*mem_space->select.iter_init)(mem_space, src_type_size, &mem_iter)<0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize memory selection information");
    if ((*file_space->select.iter_init)(file_space, dst_type_size, &bkg_iter)<0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize background selection information");
     
    /*
     * Get a temporary buffer for type conversion unless the app has already
     * supplied one through the xfer properties.  Instead of allocating a
     * buffer which is the exact size, we allocate the target size. The
     * malloc() is usually less resource-intensive if we allocate/free the
     * same size over and over.
     */
    if (tpath->cdata.need_bkg) {
        /* Retrieve the bkgr buffer property */
        if(H5P_get(dx_plist, H5D_XFER_BKGR_BUF_TYPE_NAME, &need_bkg)<0)
            HGOTO_ERROR (H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve background buffer type");
        need_bkg = MAX (tpath->cdata.need_bkg, need_bkg);
    } else if(H5T_detect_class(dataset->type, H5T_VLEN)) {
	/* Old data is retrieved into background buffer for VL datatype.  The 
	 * data is used later for freeing heap objects. */
        need_bkg = H5T_BKG_YES;
    } else {
        need_bkg = H5T_BKG_NO; /*never needed even if app says yes*/
    } /* end else */
    if (NULL==(tconv_buf=H5P_peek_voidp(dx_plist,H5D_XFER_TCONV_BUF_NAME))) {
        /* Allocate temporary buffer */
        if((tconv_buf=H5FL_BLK_ALLOC(type_conv,target_size,0))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for type conversion");
    } /* end if */
    if (need_bkg && NULL==(bkg_buf=H5P_peek_voidp(dx_plist,H5D_XFER_BKGR_BUF_NAME))) {
        /* Allocate background buffer */
        H5_CHECK_OVERFLOW((request_nelmts*dst_type_size),hsize_t,size_t);
        if((bkg_buf=H5FL_BLK_ALLOC(type_conv,(size_t)(request_nelmts*dst_type_size),1))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for background conversion");
    } /* end if */

    /* Start strip mining... */
    for (smine_start=0; smine_start<nelmts; smine_start+=smine_nelmts) {
        /* Go figure out how many elements to read from the file */
        assert((*file_space->select.iter_nelmts)(&file_iter)==(nelmts-smine_start));
        smine_nelmts = MIN(request_nelmts, (nelmts-smine_start));
	
        /*
         * Gather data from application buffer into the data type conversion
         * buffer. Also gather data from the file into the background buffer
         * if necessary.
         */
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
        n = H5S_select_mgath(buf, src_type_size, mem_space, &mem_iter,
			     smine_nelmts, dxpl_id, tconv_buf/*out*/);
#ifdef H5S_DEBUG
	H5_timer_end(&(sconv->stats[0].gath_timer), &timer);
	sconv->stats[0].gath_nbytes += n * src_type_size;
	sconv->stats[0].gath_ncalls++;
#endif
        if (n!=smine_nelmts)
            HGOTO_ERROR (H5E_IO, H5E_WRITEERROR, FAIL, "mem gather failed");

        if (need_bkg) {
#ifdef H5S_DEBUG
            H5_timer_begin(&timer);
#endif
            n = H5S_select_fgath(dataset->ent.file, &(dataset->layout), 
                                 dc_plist, dst_type_size, file_space, 
                                 &bkg_iter, smine_nelmts, dxpl_id, 
                                 bkg_buf/*out*/); 

#ifdef H5S_DEBUG
            H5_timer_end(&(sconv->stats[0].bkg_timer), &timer);
            sconv->stats[0].bkg_nbytes += n * dst_type_size;
            sconv->stats[0].bkg_ncalls++;
#endif
            if (n!=smine_nelmts)
                HGOTO_ERROR (H5E_IO, H5E_WRITEERROR, FAIL, "file gather failed");
        } /* end if */
	
        /*
         * Perform data type conversion.
         */
        if (H5T_convert(tpath, src_id, dst_id, smine_nelmts, 0, 0, tconv_buf, bkg_buf, dxpl_id)<0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "data type conversion failed");

        /*
         * Scatter the data out to the file.
         */
#ifdef H5S_DEBUG
        H5_timer_begin(&timer);
#endif
        status = H5S_select_fscat(dataset->ent.file, &(dataset->layout), 
                                  dc_plist, dst_type_size, 
                                  file_space, &file_iter, smine_nelmts, 
                                  dxpl_id, tconv_buf);

#ifdef H5S_DEBUG
        H5_timer_end(&(sconv->stats[0].scat_timer), &timer);
        sconv->stats[0].scat_nbytes += smine_nelmts * dst_type_size;
        sconv->stats[0].scat_ncalls++;
#endif
        if (status<0)
            HGOTO_ERROR (H5E_IO, H5E_WRITEERROR, FAIL, "scatter failed");
    } /* end for */

    /*
     * Update modification time.  We have to do this explicitly because
     * writing to a dataset doesn't necessarily change the object header.
     */
    if (H5O_touch(&(dataset->ent), FALSE)<0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to update modification time");

done:
#ifdef H5_HAVE_PARALLEL
    /* restore xfer_mode due to the kludge */
    if (doing_mpio && xfer_mode_changed) {
#ifdef H5D_DEBUG
	if (H5DEBUG(D)) {
	    fprintf (H5DEBUG(D), "H5D: dx->xfermode was %d, restored to %d\n",
		dx->xfer_mode, xfer_mode);
	} /* end if */
#endif
	dx->xfer_mode = xfer_mode;
    } /* end if */
#endif /*H5_HAVE_PARALLEL*/
    /* Release selection iterators */
    (*file_space->select.iter_release)(&file_iter);
    (*mem_space->select.iter_release)(&mem_iter);
    (*file_space->select.iter_release)(&bkg_iter);

    if (src_id >= 0)
        H5I_dec_ref(src_id);
    if (dst_id >= 0)
        H5I_dec_ref(dst_id);
    assert(dx_plist);
    if (tconv_buf && NULL==H5P_peek_voidp(dx_plist,H5D_XFER_TCONV_BUF_NAME))
        H5FL_BLK_FREE(type_conv,tconv_buf);
    if (bkg_buf && NULL==H5P_peek_voidp(dx_plist,H5D_XFER_BKGR_BUF_NAME))
        H5FL_BLK_FREE(type_conv,bkg_buf);
    if (free_this_space)
        H5S_close(free_this_space);

    FUNC_LEAVE(ret_value);
} /* end H5D_write() */


/*-------------------------------------------------------------------------
 * Function:	H5D_extend
 *
 * Purpose:	Increases the size of a dataset.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Friday, January 30, 1998
 *
 * Modifications:
 *
 *              Raymond Lu
 *              Tuesday, October 2, 2001
 *              Changed the way to retrieve property for generic property 
 *              list.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_extend (H5D_t *dataset, const hsize_t *size)
{
    herr_t	changed, ret_value=SUCCEED;
    H5S_t	*space = NULL;
    H5O_fill_t  fill;
    H5P_genplist_t *plist;      /* Property list */

    FUNC_ENTER_NOAPI(H5D_extend, FAIL);

    /* Check args */
    assert (dataset);
    assert (size);

    /*
     * NOTE: Restrictions on extensions were checked when the dataset was
     *	     created.  All extensions are allowed here since none should be
     *	     able to muck things up.
     */

    /* Increase the size of the data space */
    if (NULL==(space=H5S_read (&(dataset->ent))))
	HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to read data space info from dataset header");
    if ((changed=H5S_extend (space, size))<0)
	HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to increase size of data space");

    if (changed>0){
	/* Save the new dataspace in the file if necessary */
	if (H5S_modify (&(dataset->ent), space)<0)
	    HGOTO_ERROR (H5E_DATASET, H5E_WRITEERROR, FAIL, "unable to update file with new dataspace");

	/* Initialize the new parts of the dataset */
#ifdef LATER
	if (H5S_select_all(space)<0 ||
                H5S_select_hyperslab(space, H5S_SELECT_DIFF, zero, NULL, old_dims, NULL)<0)
	    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to select new extents for fill value");
#else
	/*
	 * We don't have the H5S_SELECT_DIFF operator yet.  We really only
	 * need it for contiguous datasets because the chunked datasets will
	 * either fill on demand during I/O or attempt a fill of all chunks.
	 */
        if (NULL == (plist = H5I_object(dataset->dcpl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation property list");
        if(H5P_get(plist, H5D_CRT_FILL_VALUE_NAME, &fill) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get fill value");
        if(H5D_CONTIGUOUS == dataset->layout.type && fill.buf) 
            HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unable to select fill value region"); 
#endif
	if (H5D_init_storage(dataset, space)<0)
	    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize dataset with fill value");
    } /* end if */

done:
    H5S_close(space);
    FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5D_entof
 *
 * Purpose:	Returns a pointer to the entry for a dataset.
 *
 * Return:	Success:	Ptr to entry
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Friday, April 24, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5G_entry_t *
H5D_entof (H5D_t *dataset)
{
    /* Use FUNC_ENTER_NOINIT here to avoid performance issues */
    FUNC_ENTER_NOINIT(H5D_entof);

    FUNC_LEAVE( dataset ? &(dataset->ent) : NULL);
}


/*-------------------------------------------------------------------------
 * Function:	H5D_typeof
 *
 * Purpose:	Returns a pointer to the dataset's data type.  The data type
 *		is not copied.
 *
 * Return:	Success:	Ptr to the dataset's data type, uncopied.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Thursday, June  4, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5T_t *
H5D_typeof (H5D_t *dset)
{
    /* Use FUNC_ENTER_NOINIT here to avoid performance issues */
    FUNC_ENTER_NOINIT(H5D_typeof);

    assert (dset);
    assert (dset->type);

    FUNC_LEAVE (dset->type);
}


/*-------------------------------------------------------------------------
 * Function:  H5D_get_file
 *
 * Purpose:   Returns the dataset's file pointer.
 *
 * Return:    Success:        Ptr to the dataset's file pointer.
 *
 *            Failure:        NULL
 *
 * Programmer:        Quincey Koziol
 *              Thursday, October 22, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5F_t *
H5D_get_file (const H5D_t *dset)
{
    /* Use FUNC_ENTER_NOINIT here to avoid performance issues */
    FUNC_ENTER_NOINIT(H5D_get_file);

    assert (dset);
    assert (dset->ent.file);

    FUNC_LEAVE (dset->ent.file);
}


/*-------------------------------------------------------------------------
 * Function:	H5D_init_storage
 *
 * Purpose:	Initialize the data for a new dataset.  If a selection is
 *		defined for SPACE then initialize only that part of the
 *		dataset.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Monday, October  5, 1998
 *
 * Modifications:
 *
 *              Raymond Lu
 *              Tuesday, October 2, 2001
 *              Changed the way to retrieve property for generic property 
 *              list.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_init_storage(H5D_t *dset, const H5S_t *space)
{
    hssize_t            snpoints;       /* Number of points in space (for error checking) */
    size_t              npoints;        /* Number of points in space */
    size_t              ptsperbuf;      /* Maximum # of points which fit in the buffer */
    size_t		bufsize=64*1024;    /* Size of buffer to write */
    size_t		size;           /* Current # of points to write */
    hsize_t		addr;           /* Offset in dataset */
    void		*buf = NULL;    /* Buffer for fill value writing */
    H5O_fill_t          fill;           /* Fill value information */
    H5D_space_time_t    space_time;     /* When to allocate space */
    H5P_genplist_t     *plist;          /* Property list */
    herr_t		ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOINIT(H5D_init_storage);

    assert(dset);
    assert(space);

    /* Get fill value, external file list, and data pipeline properties */
    if (NULL == (plist = H5I_object(dset->dcpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation property list");
    if(H5P_get(plist, H5D_CRT_FILL_VALUE_NAME, &fill) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get fill value");
    if(H5P_get(plist, H5D_CRT_SPACE_TIME_NAME, &space_time) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't retrieve space allocation time");

    switch (dset->layout.type) {
        case H5D_COMPACT:
            /*
             * zero set data buf.  If fill value is defined, fall through 
             * the H5D_CONTIGUOUS case and initialize with fill value.
             */
            if(!fill.buf)
                HDmemset(dset->layout.buf, 0, dset->layout.size);
            
        case H5D_CONTIGUOUS:
            /*
             * If the fill value is set then write it to the entire extent
             * of the dataset.  Note: library default(fill.buf is NULL) is
             * not handled here.  How to do it?
             */
            snpoints = H5S_get_simple_extent_npoints(space);
            assert(snpoints>=0);
            H5_ASSIGN_OVERFLOW(npoints,snpoints,hssize_t,size_t);

            if (fill.buf) {
                /*
                 * Fill the entire current extent with the fill value.  We can do
                 * this quite efficiently by making sure we copy the fill value
                 * in relatively large pieces.
                 */
			
                ptsperbuf = MAX(1, bufsize/fill.size);
                bufsize = ptsperbuf*fill.size;

                /* Allocate temporary buffer */
                if ((buf=H5FL_BLK_ALLOC(type_conv,bufsize,0))==NULL)
                    HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for fill buffer");

                /* Fill the buffer with the fill value */
                H5V_array_fill(buf, fill.buf, fill.size, ptsperbuf);
                
                /* Start at the beginning of the dataset */
                addr = 0;

                /* Loop through writing the fill value to the dataset */
                while (npoints>0) {
                    size = MIN(ptsperbuf, npoints) * fill.size;
                    if (H5F_seq_write(dset->ent.file, H5P_DATASET_XFER_DEFAULT,
                            &(dset->layout), plist, space, fill.size, size, addr, buf)<0)
                        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to write fill value to dataset");
                    npoints -= MIN(ptsperbuf, npoints);
                    addr += size;
                } /* end while */
            } /* end if */
            break;

        case H5D_CHUNKED:
            /*
             * If the dataset is accessed via parallel I/O, allocate file space
             * for all chunks now and initialize each chunk with the fill value.
             */
            if (space_time==H5D_SPACE_ALLOC_EARLY
#ifdef H5_HAVE_PARALLEL
                || (IS_H5FD_MPIO(dset->ent.file) || IS_H5FD_MPIPOSIX(dset->ent.file))
#endif /*H5_HAVE_PARALLEL*/
                ) {
                /* We only handle simple data spaces so far */
                int		ndims;
                hsize_t		dim[H5O_LAYOUT_NDIMS];
                
                if ((ndims=H5S_get_simple_extent_dims(space, dim, NULL))<0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to get simple data space info");
                dim[ndims] = dset->layout.dim[ndims];
                ndims++;

                if (H5F_istore_allocate(dset->ent.file, H5P_DATASET_XFER_DEFAULT,
                        &(dset->layout), dim, plist)<0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to allocate all chunks of dataset");
            } /* end if */
            break;
    } /* end switch */

done:
    if (buf)
        H5FL_BLK_FREE(type_conv,buf);

    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Dget_storage_size
 *
 * Purpose:	Returns the amount of storage that is required for the
 *		dataset. For chunked datasets this is the number of allocated
 *		chunks times the chunk size.
 *
 * Return:	Success:	The amount of storage space allocated for the
 *				dataset, not counting meta data. The return
 *				value may be zero if no data has been stored.
 *
 *		Failure:	Zero
 *
 * Programmer:	Robb Matzke
 *              Wednesday, April 21, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hsize_t
H5Dget_storage_size(hid_t dset_id)
{
    H5D_t	*dset=NULL;
    hsize_t	ret_value;      /* Return value */
    
    FUNC_ENTER_API(H5Dget_storage_size, 0);
    H5TRACE1("h","i",dset_id);

    /* Check args */
    if (NULL==(dset=H5I_object_verify(dset_id, H5I_DATASET)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a dataset");

    /* Set return value */
    ret_value = H5D_get_storage_size(dset);

done:
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5D_get_storage_size
 *
 * Purpose:	Determines how much space has been reserved to store the raw
 *		data of a dataset.
 *
 * Return:	Success:	Number of bytes reserved to hold raw data.
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *              Wednesday, April 21, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hsize_t
H5D_get_storage_size(H5D_t *dset)
{
    unsigned	u;              /* Index variable */
    hsize_t	ret_value;
    
    FUNC_ENTER_NOAPI(H5D_get_storage_size, 0);

    switch(dset->layout.type) {
        case H5D_CHUNKED:
            ret_value = H5F_istore_allocated(dset->ent.file, dset->layout.ndims,
                                             dset->layout.addr);
            break;
        case H5D_CONTIGUOUS:
            /* Datasets which are not allocated yet are using no space on disk */
            if(dset->layout.addr == HADDR_UNDEF)
                 ret_value=0;
            else {
                 for (u=0, ret_value=1; u<dset->layout.ndims; u++)
                     ret_value *= dset->layout.dim[u];
            } /* end else */
            break;
        case H5D_COMPACT:
            ret_value = dset->layout.size;
            break;
        default:
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset type");
    }
     
done:
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Diterate
 *
 * Purpose:	This routine iterates over all the elements selected in a memory
 *      buffer.  The callback function is called once for each element selected
 *      in the dataspace.  The selection in the dataspace is modified so
 *      that any elements already iterated over are removed from the selection
 *      if the iteration is interrupted (by the H5D_operator_t function
 *      returning non-zero) in the "middle" of the iteration and may be
 *      re-started by the user where it left off.
 *
 *      NOTE: Until "subtracting" elements from a selection is implemented,
 *          the selection is not modified.
 *
 * Parameters:
 *      void *buf;          IN/OUT: Pointer to the buffer in memory containing
 *                              the elements to iterate over.
 *      hid_t type_id;      IN: Datatype ID for the elements stored in BUF.
 *      hid_t space_id;     IN: Dataspace ID for BUF, also contains the
 *                              selection to iterate over.
 *      H5D_operator_t op; IN: Function pointer to the routine to be
 *                              called for each element in BUF iterated over.
 *      void *operator_data;    IN/OUT: Pointer to any user-defined data
 *                              associated with the operation.
 *
 * Operation information:
 *      H5D_operator_t is defined as:
 *          typedef herr_t (*H5D_operator_t)(void *elem, hid_t type_id,
 *              hsize_t ndim, hssize_t *point, void *operator_data);
 *
 *      H5D_operator_t parameters:
 *          void *elem;         IN/OUT: Pointer to the element in memory containing
 *                                  the current point.
 *          hid_t type_id;      IN: Datatype ID for the elements stored in ELEM.
 *          hsize_t ndim;       IN: Number of dimensions for POINT array
 *          hssize_t *point;    IN: Array containing the location of the element
 *                                  within the original dataspace.
 *          void *operator_data;    IN/OUT: Pointer to any user-defined data
 *                                  associated with the operation.
 *
 *      The return values from an operator are: 
 *          Zero causes the iterator to continue, returning zero when all
 *              elements have been processed. 
 *          Positive causes the iterator to immediately return that positive
 *              value, indicating short-circuit success.  The iterator can be
 *              restarted at the next element. 
 *          Negative causes the iterator to immediately return that value,
 *              indicating failure. The iterator can be restarted at the next
 *              element.
 *
 * Return:	Returns the return value of the last operator if it was non-zero,
 *          or zero if all elements were processed. Otherwise returns a
 *          negative value.
 *
 * Programmer:	Quincey Koziol
 *              Friday, June 11, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Diterate(void *buf, hid_t type_id, hid_t space_id, H5D_operator_t op,
        void *operator_data)
{
    H5S_t		   *space = NULL;
    herr_t ret_value;

    FUNC_ENTER_API(H5Diterate, FAIL);
    H5TRACE5("e","xiixx",buf,type_id,space_id,op,operator_data);

    /* Check args */
    if (NULL==op)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid operator");
    if (buf==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid buffer");
    if (H5I_DATATYPE != H5I_get_type(type_id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid datatype");
    if (NULL == (space = H5I_object_verify(space_id, H5I_DATASPACE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid dataspace");

    ret_value=H5S_select_iterate(buf,type_id,space,op,operator_data);

done:
    FUNC_LEAVE(ret_value);
}   /* end H5Diterate() */


/*-------------------------------------------------------------------------
 * Function:	H5Dvlen_reclaim
 *
 * Purpose:	Frees the buffers allocated for storing variable-length data
 *      in memory.  Only frees the VL data in the selection defined in the
 *      dataspace.  The dataset transfer property list is required to find the
 *      correct allocation/free methods for the VL data in the buffer.
 *
 * Return:	Non-negative on success, negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, June 10, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dvlen_reclaim(hid_t type_id, hid_t space_id, hid_t plist_id, void *buf)
{
    herr_t ret_value;

    FUNC_ENTER_API(H5Dvlen_reclaim, FAIL);
    H5TRACE4("e","iiix",type_id,space_id,plist_id,buf);

    /* Check args */
    if (H5I_DATATYPE!=H5I_get_type(type_id) ||
            H5I_DATASPACE!=H5I_get_type(space_id) ||
            buf==NULL)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid argument");

    /* Get the default dataset transfer property list if the user didn't provide one */
    if (H5P_DEFAULT == plist_id)
        plist_id= H5P_DATASET_XFER_DEFAULT;

    if (TRUE!=H5P_isa_class(plist_id,H5P_DATASET_XFER))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms");

    /* Call H5Diterate with args, etc. */
    ret_value=H5Diterate(buf,type_id,space_id,H5T_vlen_reclaim,&plist_id);

done:
    FUNC_LEAVE(ret_value);
}   /* end H5Dvlen_reclaim() */


/*-------------------------------------------------------------------------
 * Function:	H5D_vlen_get_buf_size_alloc
 *
 * Purpose:	This routine makes certain there is enough space in the temporary
 *      buffer for the new data to read in.  All the VL data read in is actually
 *      placed in this buffer, overwriting the previous data.  Needless to say,
 *      this data is not actually usable.
 *
 * Return:	Non-negative on success, negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, August 17, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *
H5D_vlen_get_buf_size_alloc(size_t size, void *info)
{
    H5T_vlen_bufsize_t *vlen_bufsize=(H5T_vlen_bufsize_t *)info;
    void *ret_value;    /* Return value */

    FUNC_ENTER_NOAPI(H5D_vlen_get_buf_size_alloc, NULL);

    /* Get a temporary pointer to space for the VL data */
    if ((vlen_bufsize->vl_tbuf=H5FL_BLK_REALLOC(vlen_vl_buf,vlen_bufsize->vl_tbuf,size))!=NULL)
        vlen_bufsize->size+=size;

    /* Set return value */
    ret_value=vlen_bufsize->vl_tbuf;

done:
    FUNC_LEAVE(ret_value);
} /* end H5D_vlen_get_buf_size_alloc() */


/*-------------------------------------------------------------------------
 * Function:	H5D_vlen_get_buf_size
 *
 * Purpose:	This routine checks the number of bytes required to store a single
 *      element from a dataset in memory, creating a selection with just the
 *      single element selected to read in the element and using a custom memory
 *      allocator for any VL data encountered.
 *          The *size value is modified according to how many bytes are
 *      required to store the element in memory.
 *
 * Implementation: This routine actually performs the read with a custom
 *      memory manager which basically just counts the bytes requested and
 *      uses a temporary memory buffer (through the H5FL API) to make certain
 *      enough space is available to perform the read.  Then the temporary
 *      buffer is released and the number of bytes allocated is returned.
 *      Kinda kludgy, but easier than the other method of trying to figure out
 *      the sizes without actually reading the data in... - QAK
 *
 * Return:	Non-negative on success, negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, August 17, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_vlen_get_buf_size(void UNUSED *elem, hid_t type_id, hsize_t UNUSED ndim, hssize_t *point, void *op_data)
{
    H5T_vlen_bufsize_t *vlen_bufsize=(H5T_vlen_bufsize_t *)op_data;
    H5T_t	*dt = NULL;
    herr_t ret_value=0;         /* The correct return value, if this function succeeds */

    FUNC_ENTER_NOAPI(H5D_vlen_get_buf_size, FAIL);

    assert(op_data);
    assert(H5I_DATATYPE == H5I_get_type(type_id));

    /* Check args */
    if (NULL==(dt=H5I_object(type_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");

    /* Make certain there is enough fixed-length buffer available */
    if ((vlen_bufsize->fl_tbuf=H5FL_BLK_REALLOC(vlen_fl_buf,vlen_bufsize->fl_tbuf,H5T_get_size(dt)))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't resize tbuf");

    /* Select point to read in */
    if (H5Sselect_elements(vlen_bufsize->fspace_id,H5S_SELECT_SET,1,(const hssize_t **)point)<0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCREATE, FAIL, "can't select point");

    /* Read in the point (with the custom VL memory allocator) */
    if(H5Dread(vlen_bufsize->dataset_id,type_id,vlen_bufsize->mspace_id,vlen_bufsize->fspace_id,vlen_bufsize->xfer_pid,vlen_bufsize->fl_tbuf)<0)
        HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read point");

done:
    FUNC_LEAVE(ret_value);
}   /* end H5D_vlen_get_buf_size() */


/*-------------------------------------------------------------------------
 * Function:	H5Dvlen_get_buf_size
 *
 * Purpose:	This routine checks the number of bytes required to store the VL
 *      data from the dataset, using the space_id for the selection in the
 *      dataset on disk and the type_id for the memory representation of the
 *      VL data, in memory.  The *size value is modified according to how many
 *      bytes are required to store the VL data in memory.
 *
 * Implementation: This routine actually performs the read with a custom
 *      memory manager which basically just counts the bytes requested and
 *      uses a temporary memory buffer (through the H5FL API) to make certain
 *      enough space is available to perform the read.  Then the temporary
 *      buffer is released and the number of bytes allocated is returned.
 *      Kinda kludgy, but easier than the other method of trying to figure out
 *      the sizes without actually reading the data in... - QAK
 *
 * Return:	Non-negative on success, negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, August 11, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dvlen_get_buf_size(hid_t dataset_id, hid_t type_id, hid_t space_id,
        hsize_t *size)
{
    H5T_vlen_bufsize_t vlen_bufsize = {0, 0, 0, 0, 0, 0, 0};
    char bogus;         /* bogus value to pass to H5Diterate() */
    H5P_genclass_t  *pclass;    /* Property class */
    H5P_genplist_t  *plist;     /* Property list */
    herr_t ret_value=FAIL;

    FUNC_ENTER_API(H5Dvlen_get_buf_size, FAIL);
    H5TRACE4("e","iii*h",dataset_id,type_id,space_id,size);

    /* Check args */
    if (H5I_DATASET!=H5I_get_type(dataset_id) ||
            H5I_DATATYPE!=H5I_get_type(type_id) ||
            H5I_DATASPACE!=H5I_get_type(space_id) || size==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid argument");

    /* Save the dataset ID */
    vlen_bufsize.dataset_id=dataset_id;

    /* Get a copy of the dataspace ID */
    if((vlen_bufsize.fspace_id=H5Dget_space(dataset_id))<0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOPY, FAIL, "can't copy dataspace");
    
    /* Create a scalar for the memory dataspace */
    if((vlen_bufsize.mspace_id=H5Screate(H5S_SCALAR))<0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOPY, FAIL, "can't create dataspace");

    /* Grab the temporary buffers required */
    if((vlen_bufsize.fl_tbuf=H5FL_BLK_ALLOC(vlen_fl_buf,1,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "no temporary buffers available");
    if((vlen_bufsize.vl_tbuf=H5FL_BLK_ALLOC(vlen_vl_buf,1,0))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "no temporary buffers available");

    /* Get the pointer to the dataset transfer class */
    if (NULL == (pclass = H5I_object(H5P_CLS_DATASET_XFER_g)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list class");

    /* Change to the custom memory allocation routines for reading VL data */
    if((vlen_bufsize.xfer_pid=H5P_create_id(pclass))<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTCREATE, FAIL, "no dataset xfer plists available");

    /* Get the property list struct */
    if (NULL == (plist = H5I_object(vlen_bufsize.xfer_pid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset transfer property list");

    /* Set the memory manager to the special allocation routine */
    if(H5P_set_vlen_mem_manager(plist,H5D_vlen_get_buf_size_alloc,&vlen_bufsize,NULL,NULL)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "can't set VL data allocation routine");

    /* Set the initial number of bytes required */
    vlen_bufsize.size=0;

    /* Call H5Diterate with args, etc. */
    ret_value=H5Diterate(&bogus,type_id,space_id,H5D_vlen_get_buf_size,&vlen_bufsize);

    /* Get the size if we succeeded */
    if(ret_value>=0)
        *size=vlen_bufsize.size;

done:
    if(vlen_bufsize.fspace_id>0)
        H5Sclose(vlen_bufsize.fspace_id);
    if(vlen_bufsize.mspace_id>0)
        H5Sclose(vlen_bufsize.mspace_id);
    if(vlen_bufsize.fl_tbuf!=NULL)
        H5FL_BLK_FREE(vlen_fl_buf,vlen_bufsize.fl_tbuf);
    if(vlen_bufsize.vl_tbuf!=NULL)
        H5FL_BLK_FREE(vlen_vl_buf,vlen_bufsize.vl_tbuf);
    if(vlen_bufsize.xfer_pid>0)
        H5I_dec_ref(vlen_bufsize.xfer_pid);

    FUNC_LEAVE(ret_value);
}   /* end H5Dvlen_get_buf_size() */


/*--------------------------------------------------------------------------
 NAME
    H5D_fill
 PURPOSE
    Fill a selection in memory with a value (internal version)
 USAGE
    herr_t H5Dfill(fill, fill_type, buf, buf_type, space)
        const void *fill;       IN: Pointer to fill value to use
        H5T_t *fill_type;       IN: Datatype of the fill value
        void *buf;              IN/OUT: Memory buffer to fill selection within
        H5T_t *buf_type;        IN: Datatype of the elements in buffer
        H5S_t *space;           IN: Dataspace describing memory buffer &
                                    containing selection to use.
 RETURNS
    Non-negative on success/Negative on failure.
 DESCRIPTION
    Use the selection in the dataspace to fill elements in a memory buffer.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    If "fill" parameter is NULL, use all zeros as fill value.  If "fill_type"
    parameter is NULL, use "buf_type" for the fill value datatype.
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5D_fill(const void *fill, const H5T_t *fill_type, void *buf, const H5T_t *buf_type, const H5S_t *space)
{
    H5T_path_t *tpath = NULL;   /* Conversion information*/
    uint8_t *tconv_buf = NULL;  /* Data type conv buffer */
    uint8_t *bkg_buf = NULL;    /* Temp conversion buffer */
    hid_t src_id = -1, dst_id = -1;     /* Temporary type IDs */
    size_t src_type_size;       /* Size of source type	*/
    size_t dst_type_size;       /* Size of destination type*/
    size_t buf_size;            /* Desired buffer size	*/
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOINIT(H5D_fill);

    /* Check args */
    assert(buf);
    assert(buf_type);
    assert(space);

    /* Check for "default" fill value */
    if(fill_type==NULL)
        fill_type=buf_type;

    /* Get the memory and file datatype sizes */
    src_type_size = H5T_get_size(fill_type);
    dst_type_size = H5T_get_size(buf_type);

    /* Get the maximum buffer size needed and allocate it */
    buf_size=MAX(src_type_size,dst_type_size);
    if (NULL==(tconv_buf = H5FL_BLK_ALLOC(type_elem,buf_size,0)) || NULL==(bkg_buf = H5FL_BLK_ALLOC(type_elem,buf_size,1)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    /* Copy the user's data into the buffer for conversion */
    if(fill==NULL)
        HDmemset(tconv_buf,0,src_type_size);
    else
        HDmemcpy(tconv_buf,fill,src_type_size);

    /* Convert memory buffer into disk buffer */
    /* Set up type conversion function */
    if (NULL == (tpath = H5T_path_find(fill_type, buf_type, NULL, NULL))) {
        HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unable to convert between src and dest data types");
    } else if (!H5T_IS_NOOP(tpath)) {
        if ((src_id = H5I_register(H5I_DATATYPE, H5T_copy(fill_type, H5T_COPY_ALL)))<0 ||
                (dst_id = H5I_register(H5I_DATATYPE, H5T_copy(buf_type, H5T_COPY_ALL)))<0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTREGISTER, FAIL, "unable to register types for conversion");
    }

    /* Perform data type conversion */
    if (H5T_convert(tpath, src_id, dst_id, (hsize_t)1, 0, 0, tconv_buf, bkg_buf, H5P_DEFAULT)<0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCONVERT, FAIL, "data type conversion failed");

    /* Fill the selection in the memory buffer */
    if(H5S_select_fill(tconv_buf, dst_type_size, space, buf)<0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTENCODE, FAIL, "filling selection failed");

done:
    if (tconv_buf)
        H5FL_BLK_FREE(type_elem,tconv_buf);
    if (bkg_buf)
        H5FL_BLK_FREE(type_elem,bkg_buf);
    FUNC_LEAVE (ret_value);
}   /* H5D_fill() */


/*--------------------------------------------------------------------------
 NAME
    H5Dfill
 PURPOSE
    Fill a selection in memory with a value
 USAGE
    herr_t H5Dfill(fill, fill_type, space, buf, buf_type)
        const void *fill;       IN: Pointer to fill value to use
        hid_t fill_type_id;     IN: Datatype of the fill value
        void *buf;              IN/OUT: Memory buffer to fill selection within
        hid_t buf_type_id;      IN: Datatype of the elements in buffer
        hid_t space_id;         IN: Dataspace describing memory buffer &
                                    containing selection to use.
 RETURNS
    Non-negative on success/Negative on failure.
 DESCRIPTION
    Use the selection in the dataspace to fill elements in a memory buffer.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    If "fill" parameter is NULL, use all zeros as fill value
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5Dfill(const void *fill, hid_t fill_type_id, void *buf, hid_t buf_type_id, hid_t space_id)
{
    H5S_t *space;               /* Dataspace */
    H5T_t *fill_type;           /* Fill-value datatype */
    H5T_t *buf_type;            /* Buffer datatype */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_API(H5Dfill, FAIL);
    H5TRACE5("e","xixii",fill,fill_type_id,buf,buf_type_id,space_id);

    /* Check args */
    if (buf==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid buffer");
    if (NULL == (space=H5I_object_verify(space_id, H5I_DATASPACE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a dataspace");
    if (NULL == (fill_type=H5I_object_verify(fill_type_id, H5I_DATATYPE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a datatype");
    if (NULL == (buf_type=H5I_object_verify(buf_type_id, H5I_DATATYPE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a datatype");

    /* Fill the selection in the memory buffer */
    if(H5D_fill(fill,fill_type,buf,buf_type,space)<0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTENCODE, FAIL, "filling selection failed");

done:
    FUNC_LEAVE (ret_value);
}   /* H5Dfill() */


/*-------------------------------------------------------------------------
 * Function: H5Dset_extent
 *
 * Purpose: Modifies the dimensions of a dataset, based on H5Dextend. 
 *  Can change to a lower dimension.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *             Robb Matzke
 *
 * Date: April 9, 2002
 *
 * Comments: Public function, calls private H5D_set_extent
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dset_extent(hid_t dset_id, const hsize_t *size)
{
    H5D_t                  *dset = NULL;
    herr_t                  ret_value=SUCCEED;  /* Return value */

    FUNC_ENTER_API(H5Dset_extent, FAIL);
    H5TRACE2("e","i*h",dset_id,size);

    /* Check args */
    if(NULL == (dset = H5I_object_verify(dset_id, H5I_DATASET)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
    if(!size)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no size specified");

    /* Private function */
    if(H5D_set_extent(dset, size) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to set extend dataset");

done:
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function: H5D_set_extent
 *
 * Purpose: Based in H5D_extend, allows change to a lower dimension, 
 *  calls H5S_set_extent and H5F_istore_prune_by_extent instead
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *             Robb Matzke
 *
 * Date: April 9, 2002
 *
 * Comments: Private function
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_set_extent(H5D_t *dset, const hsize_t *size)
{
    hsize_t                 curr_dims[H5O_LAYOUT_NDIMS];	/* Current dimension sizes */
    int                     rank;	/* Dataspace # of dimensions */
    herr_t                  ret_value = SUCCEED;        /* Return value */
    H5S_t                  *space = NULL;
    H5P_genplist_t         *plist;
    int                     u;
    int                     shrink = 0;

    FUNC_ENTER_NOAPI(H5D_set_extent, FAIL);

    /* Check args */
    assert(dset);
    assert(size);

 /*-------------------------------------------------------------------------
  * Get the data space
  *-------------------------------------------------------------------------
  */
    if(NULL == (space = H5S_read(&(dset->ent))))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to read data space info from dset header");

 /*-------------------------------------------------------------------------
  * Check if we are shrinking in any of the dimensions
  *-------------------------------------------------------------------------
  */
    if((rank = H5S_get_simple_extent_dims(space, curr_dims, NULL)) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get dataset dimensions");

    for(u = 0; u < rank; u++) {
	if(size[u] < curr_dims[u]) {
	    shrink = 1;
	    break;
	}
    }

 /*-------------------------------------------------------------------------
  * Modify the size of the data space
  *-------------------------------------------------------------------------
  */
    if(H5S_set_extent(space, size) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to modify size of data space");

 /*-------------------------------------------------------------------------
  * Modify the dataset storage
  *-------------------------------------------------------------------------
  */
    /* Save the new dataspace in the file if necessary */
    if(H5S_modify(&(dset->ent), space) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "unable to update file with new dataspace");

    /* Initialize the new parts of the dset */
    if(H5D_init_storage(dset, space) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize dataset storage");


 /*-------------------------------------------------------------------------
  * Remove chunk information in the case of chunked datasets
  * This removal takes place only in case we are shrinking the dateset
  *-------------------------------------------------------------------------
  */
    if(shrink && H5D_CHUNKED == dset->layout.type) {
        /* Remove excess chunks */
	if(H5F_istore_prune_by_extent(dset->ent.file, &dset->layout, space) < 0)
	    HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "unable to remove chunks ");

        /* Get the dataset creation property list */
	if(NULL == (plist = H5I_object(dset->dcpl_id)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dset creation property list");

        /* Reset the elements outsize the new dimensions, but in existing chunks */
	if(H5F_istore_initialize_by_extent(dset->ent.file, &dset->layout, plist, space) < 0)
	    HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "unable to initialize chunks ");
    } /* end if */

done:
    if(space)
	H5S_close(space);
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:    H5D_flush
 *
 * Purpose:     Flush any compact datasets cached in memory
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 *
 * Programmer:  Ray Lu
 *
 * Date:        August 14, 2002
 *
 * Comments:    Private function
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_flush(H5F_t *f)
{
    unsigned            num_dsets;      /* Number of datasets in file   */
    hid_t               *id_list=NULL;  /* list of dataset IDs          */
    H5D_t               *dataset=NULL;  /* Dataset pointer              */
    herr_t              ret_value = SUCCEED;        /* Return value     */
    unsigned		j;              /* Index variable */

    FUNC_ENTER_NOAPI(H5D_flush, FAIL);

    /* Check args */
    assert(f);

    /* Update layout message for compact dataset */
    if(H5F_get_obj_count(f, H5F_OBJ_DATASET, &num_dsets)<0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to get dataset number");
    if(num_dsets>0) {
        if(NULL==(id_list=H5MM_malloc(num_dsets*sizeof(hid_t))))
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to allocate memory for ID list");
        if(H5F_get_obj_ids(f, H5F_OBJ_DATASET, id_list)<0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to get dataset ID list");
        for(j=0; j<num_dsets; j++) {
            if(NULL==(dataset=H5I_object_verify(id_list[j], H5I_DATASET)))
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to get dataset object");
            if(dataset->layout.type==H5D_COMPACT && dataset->layout.dirty)
                if(H5O_modify(&(dataset->ent), H5O_LAYOUT, 0, 0, &(dataset->layout))<0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to update layout message");
            dataset->layout.dirty = FALSE;
        }
    }

done:
    if(id_list!=NULL)
        H5MM_xfree(id_list);
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Ddebug
 *
 * Purpose:	Prints various information about a dataset.  This function is
 *		not to be documented in the API at this time.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Wednesday, April 28, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Ddebug(hid_t dset_id, unsigned UNUSED flags)
{
    H5D_t	*dset=NULL;
    herr_t      ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_API(H5Ddebug, FAIL);
    H5TRACE2("e","iIu",dset_id,flags);

    /* Check args */
    if (NULL==(dset=H5I_object_verify(dset_id, H5I_DATASET)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");

    /* Print B-tree information */
    if (H5D_CHUNKED==dset->layout.type) {
	H5F_istore_dump_btree(dset->ent.file, stdout, dset->layout.ndims, dset->layout.addr);
    } else if (H5D_CONTIGUOUS==dset->layout.type) {
	HDfprintf(stdout, "    %-10s %a\n", "Address:", dset->layout.addr);
    }
    
done:
    FUNC_LEAVE(ret_value);
}
