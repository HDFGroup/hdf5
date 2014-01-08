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

/*-------------------------------------------------------------------------
 *
 * Created:		H5Pfapl.c
 *			February 26 1998
 *			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		File access property list class routines
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
#include "H5ACprivate.h"	/* Metadata cache			*/
#include "H5Dprivate.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"		/* Files		  	*/
#include "H5FDprivate.h"	/* File drivers				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"        /* Memory Management    */
#include "H5Ppkg.h"		/* Property lists		  	*/

/* Includes needed to set as default file driver */
#include "H5FDsec2.h"		/* Posix unbuffered I/O	file driver	*/
#include "H5FDstdio.h"		/* Standard C buffered I/O		*/
#ifdef H5_HAVE_WINDOWS
#include "H5FDwindows.h"        /* Windows buffered I/O     */
#endif


/****************/
/* Local Macros */
/****************/

/* ========= File Access properties ============ */
/* Definitions for the initial metadata cache resize configuration */
#define H5F_ACS_META_CACHE_INIT_CONFIG_SIZE	sizeof(H5AC_cache_config_t)
#define H5F_ACS_META_CACHE_INIT_CONFIG_DEF	H5AC__DEFAULT_CACHE_CONFIG
#define H5F_ACS_META_CACHE_INIT_CONFIG_ENC	H5P__facc_cache_config_enc
#define H5F_ACS_META_CACHE_INIT_CONFIG_DEC	H5P__facc_cache_config_dec
#define H5F_ACS_META_CACHE_INIT_CONFIG_CMP      H5P__facc_cache_config_cmp
/* Definitions for size of raw data chunk cache(slots) */
#define H5F_ACS_DATA_CACHE_NUM_SLOTS_SIZE       sizeof(size_t)
#define H5F_ACS_DATA_CACHE_NUM_SLOTS_DEF        521
#define H5F_ACS_DATA_CACHE_NUM_SLOTS_ENC        H5P__encode_size_t
#define H5F_ACS_DATA_CACHE_NUM_SLOTS_DEC        H5P__decode_size_t
/* Definition for size of raw data chunk cache(bytes) */
#define H5F_ACS_DATA_CACHE_BYTE_SIZE_SIZE       sizeof(size_t)
#define H5F_ACS_DATA_CACHE_BYTE_SIZE_DEF        (1024*1024)
#define H5F_ACS_DATA_CACHE_BYTE_SIZE_ENC        H5P__encode_size_t
#define H5F_ACS_DATA_CACHE_BYTE_SIZE_DEC        H5P__decode_size_t
/* Definition for preemption read chunks first */
#define H5F_ACS_PREEMPT_READ_CHUNKS_SIZE        sizeof(double)
#define H5F_ACS_PREEMPT_READ_CHUNKS_DEF         0.75f
#define H5F_ACS_PREEMPT_READ_CHUNKS_ENC         H5P__encode_double
#define H5F_ACS_PREEMPT_READ_CHUNKS_DEC         H5P__decode_double
/* Definition for threshold for alignment */
#define H5F_ACS_ALIGN_THRHD_SIZE                sizeof(hsize_t)
#define H5F_ACS_ALIGN_THRHD_DEF                 1
#define H5F_ACS_ALIGN_THRHD_ENC                 H5P__encode_hsize_t
#define H5F_ACS_ALIGN_THRHD_DEC                 H5P__decode_hsize_t
/* Definition for alignment */
#define H5F_ACS_ALIGN_SIZE                      sizeof(hsize_t)
#define H5F_ACS_ALIGN_DEF                       1
#define H5F_ACS_ALIGN_ENC                       H5P__encode_hsize_t
#define H5F_ACS_ALIGN_DEC                       H5P__decode_hsize_t
/* Definition for minimum metadata allocation block size (when
   aggregating metadata allocations. */
#define H5F_ACS_META_BLOCK_SIZE_SIZE            sizeof(hsize_t)
#define H5F_ACS_META_BLOCK_SIZE_DEF             2048
#define H5F_ACS_META_BLOCK_SIZE_ENC             H5P__encode_hsize_t
#define H5F_ACS_META_BLOCK_SIZE_DEC             H5P__decode_hsize_t
/* Definition for maximum sieve buffer size (when data sieving
   is allowed by file driver */
#define H5F_ACS_SIEVE_BUF_SIZE_SIZE             sizeof(size_t)
#define H5F_ACS_SIEVE_BUF_SIZE_DEF              (64*1024)
#define H5F_ACS_SIEVE_BUF_SIZE_ENC              H5P__encode_size_t
#define H5F_ACS_SIEVE_BUF_SIZE_DEC              H5P__decode_size_t
/* Definition for minimum "small data" allocation block size (when
   aggregating "small" raw data allocations. */
#define H5F_ACS_SDATA_BLOCK_SIZE_SIZE           sizeof(hsize_t)
#define H5F_ACS_SDATA_BLOCK_SIZE_DEF            2048
#define H5F_ACS_SDATA_BLOCK_SIZE_ENC            H5P__encode_hsize_t
#define H5F_ACS_SDATA_BLOCK_SIZE_DEC            H5P__decode_hsize_t
/* Definition for garbage-collect references */
#define H5F_ACS_GARBG_COLCT_REF_SIZE            sizeof(unsigned)
#define H5F_ACS_GARBG_COLCT_REF_DEF             0
#define H5F_ACS_GARBG_COLCT_REF_ENC             H5P__encode_unsigned
#define H5F_ACS_GARBG_COLCT_REF_DEC             H5P__decode_unsigned
/* Definition for file driver ID */
#define H5F_ACS_FILE_DRV_ID_SIZE                sizeof(hid_t)
#define H5F_ACS_FILE_DRV_ID_DEF                 H5_DEFAULT_VFD
/* Definition for file driver info */
#define H5F_ACS_FILE_DRV_INFO_SIZE              sizeof(void*)
#define H5F_ACS_FILE_DRV_INFO_DEF               NULL
/* Definition for file close degree */
#define H5F_CLOSE_DEGREE_SIZE		        sizeof(H5F_close_degree_t)
#define H5F_CLOSE_DEGREE_DEF		        H5F_CLOSE_DEFAULT
#define H5F_CLOSE_DEGREE_ENC		        H5P__facc_fclose_degree_enc
#define H5F_CLOSE_DEGREE_DEC		        H5P__facc_fclose_degree_dec
/* Definition for offset position in file for family file driver */
#define H5F_ACS_FAMILY_OFFSET_SIZE              sizeof(hsize_t)
#define H5F_ACS_FAMILY_OFFSET_DEF               0
#define H5F_ACS_FAMILY_OFFSET_ENC               H5P__encode_hsize_t
#define H5F_ACS_FAMILY_OFFSET_DEC               H5P__decode_hsize_t
/* Definition for new member size of family driver. It's private
 * property only used by h5repart */
#define H5F_ACS_FAMILY_NEWSIZE_SIZE             sizeof(hsize_t)
#define H5F_ACS_FAMILY_NEWSIZE_DEF              0
/* Definition for whether to convert family to sec2 driver. It's private
 * property only used by h5repart */
#define H5F_ACS_FAMILY_TO_SEC2_SIZE             sizeof(hbool_t)
#define H5F_ACS_FAMILY_TO_SEC2_DEF              FALSE
/* Definition for data type in multi file driver */
#define H5F_ACS_MULTI_TYPE_SIZE                 sizeof(H5FD_mem_t)
#define H5F_ACS_MULTI_TYPE_DEF                  H5FD_MEM_DEFAULT
#define H5F_ACS_MULTI_TYPE_ENC                  H5P__facc_multi_type_enc
#define H5F_ACS_MULTI_TYPE_DEC                  H5P__facc_multi_type_dec
/* Definition for 'use latest format version' flag */
#define H5F_ACS_LATEST_FORMAT_SIZE              sizeof(hbool_t)
#define H5F_ACS_LATEST_FORMAT_DEF               FALSE
#define H5F_ACS_LATEST_FORMAT_ENC               H5P__encode_hbool_t
#define H5F_ACS_LATEST_FORMAT_DEC               H5P__decode_hbool_t
/* Definition for whether to query the file descriptor from the core VFD
 * instead of the memory address.  (Private to library)
 */
#define H5F_ACS_WANT_POSIX_FD_SIZE              sizeof(hbool_t)
#define H5F_ACS_WANT_POSIX_FD_DEF               FALSE
/* Definition for external file cache size */
#define H5F_ACS_EFC_SIZE_SIZE                   sizeof(unsigned)
#define H5F_ACS_EFC_SIZE_DEF                    0
#define H5F_ACS_EFC_SIZE_ENC                    H5P__encode_unsigned
#define H5F_ACS_EFC_SIZE_DEC                    H5P__decode_unsigned
/* Definition of pointer to initial file image info */
#define H5F_ACS_FILE_IMAGE_INFO_SIZE            sizeof(H5FD_file_image_info_t)
#define H5F_ACS_FILE_IMAGE_INFO_DEF             H5FD_DEFAULT_FILE_IMAGE_INFO
#define H5F_ACS_FILE_IMAGE_INFO_DEL             H5P_file_image_info_del
#define H5F_ACS_FILE_IMAGE_INFO_COPY            H5P_file_image_info_copy
#define H5F_ACS_FILE_IMAGE_INFO_CLOSE           H5P_file_image_info_close
/* Definition for # of metadata read attempts */
#define H5F_ACS_METADATA_READ_ATTEMPTS_SIZE	sizeof(unsigned)
#define H5F_ACS_METADATA_READ_ATTEMPTS_DEF     	0
#define H5F_ACS_METADATA_READ_ATTEMPTS_ENC     	H5P__encode_unsigned
#define H5F_ACS_METADATA_READ_ATTEMPTS_DEC     	H5P__decode_unsigned
/* Definition for object flush callback */
#define H5F_ACS_OBJECT_FLUSH_CB_SIZE		sizeof(H5F_object_flush_t)
#define H5F_ACS_OBJECT_FLUSH_CB_DEF             {NULL, NULL}
/* Definition for status_flags in the superblock */
#define H5F_ACS_CLEAR_STATUS_FLAGS_SIZE         sizeof(hbool_t)
#define H5F_ACS_CLEAR_STATUS_FLAGS_DEF          FALSE


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
static herr_t H5P_facc_reg_prop(H5P_genclass_t *pclass);
static herr_t H5P_facc_create(hid_t fapl_id, void *copy_data);
static herr_t H5P_facc_copy(hid_t new_plist_t, hid_t old_plist_t, void *copy_data);

/* File image info property callbacks */
static herr_t H5P_file_image_info_del(hid_t prop_id, const char *name, size_t size, void *value);
static herr_t H5P_file_image_info_copy(const char *name, size_t size, void *value);
static herr_t H5P_file_image_info_close(const char *name, size_t size, void *value);

/* encode & decode callbacks */
static herr_t H5P__facc_cache_config_enc(const void *value, void **_pp, size_t *size);
static herr_t H5P__facc_cache_config_dec(const void **_pp, void *value);
static int H5P__facc_cache_config_cmp(const void *value1, const void *value2, size_t size);
static herr_t H5P__facc_fclose_degree_enc(const void *value, void **_pp, size_t *size);
static herr_t H5P__facc_fclose_degree_dec(const void **pp, void *value);
static herr_t H5P__facc_multi_type_enc(const void *value, void **_pp, size_t *size);
static herr_t H5P__facc_multi_type_dec(const void **_pp, void *value);


/*********************/
/* Package Variables */
/*********************/

/* File access property list class library initialization object */
const H5P_libclass_t H5P_CLS_FACC[1] = {{
    "file access",		/* Class name for debugging     */
    H5P_TYPE_FILE_ACCESS,       /* Class type                   */
    &H5P_CLS_ROOT_g,		/* Parent class ID              */
    &H5P_CLS_FILE_ACCESS_g,	/* Pointer to class ID          */
    &H5P_LST_FILE_ACCESS_g,	/* Pointer to default property list ID */
    H5P_facc_reg_prop,		/* Default property registration routine */
    H5P_facc_create,		/* Class creation callback      */
    NULL,		        /* Class creation callback info */
    H5P_facc_copy,		/* Class copy callback          */
    NULL,		        /* Class copy callback info     */
    H5P_facc_close,		/* Class close callback         */
    NULL 		        /* Class close callback info    */
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Property value defaults */
static const H5AC_cache_config_t H5F_def_mdc_initCacheCfg_g = H5F_ACS_META_CACHE_INIT_CONFIG_DEF;  /* Default metadata cache settings */
static const size_t H5F_def_rdcc_nslots_g = H5F_ACS_DATA_CACHE_NUM_SLOTS_DEF;      /* Default raw data chunk cache # of slots */
static const size_t H5F_def_rdcc_nbytes_g = H5F_ACS_DATA_CACHE_BYTE_SIZE_DEF;      /* Default raw data chunk cache # of bytes */
static const double H5F_def_rdcc_w0_g = H5F_ACS_PREEMPT_READ_CHUNKS_DEF;           /* Default raw data chunk cache dirty ratio */
static const hsize_t H5F_def_threshold_g = H5F_ACS_ALIGN_THRHD_DEF;                /* Default allocation alignment threshold */
static const hsize_t H5F_def_alignment_g = H5F_ACS_ALIGN_DEF;                      /* Default allocation alignment value */
static const hsize_t H5F_def_meta_block_size_g = H5F_ACS_META_BLOCK_SIZE_DEF;      /* Default metadata allocation block size */
static const size_t H5F_def_sieve_buf_size_g = H5F_ACS_SIEVE_BUF_SIZE_DEF;         /* Default raw data I/O sieve buffer size */
static const hsize_t H5F_def_sdata_block_size_g = H5F_ACS_SDATA_BLOCK_SIZE_DEF;    /* Default small data allocation block size */
static const unsigned H5F_def_gc_ref_g = H5F_ACS_GARBG_COLCT_REF_DEF;              /* Default garbage collection for references setting */
static const void *H5F_def_driver_info_g = H5F_ACS_FILE_DRV_INFO_DEF;              /* Default VFL driver info */
static const H5F_close_degree_t H5F_def_close_degree_g = H5F_CLOSE_DEGREE_DEF;     /* Default file close degree */
static const hsize_t H5F_def_family_offset_g = H5F_ACS_FAMILY_OFFSET_DEF;          /* Default offset for family VFD */
static const hsize_t H5F_def_family_newsize_g = H5F_ACS_FAMILY_NEWSIZE_DEF;        /* Default size of new files for family VFD */
static const hbool_t H5F_def_family_to_sec2_g = H5F_ACS_FAMILY_TO_SEC2_DEF;        /* Default ?? for family VFD */
static const H5FD_mem_t H5F_def_mem_type_g = H5F_ACS_MULTI_TYPE_DEF;               /* Default file space type for multi VFD */
static const hbool_t H5F_def_latest_format_g = H5F_ACS_LATEST_FORMAT_DEF;          /* Default setting for "use the latest version of the format" flag */
static const hbool_t H5F_def_want_posix_fd_g = H5F_ACS_WANT_POSIX_FD_DEF;          /* Default setting for retrieving 'handle' from core VFD */
static const unsigned H5F_def_efc_size_g = H5F_ACS_EFC_SIZE_DEF;                   /* Default external file cache size */
static const H5FD_file_image_info_t H5F_def_file_image_info_g = H5F_ACS_FILE_IMAGE_INFO_DEF;  /* Default file image info and callbacks */
static const unsigned H5F_def_metadata_read_attempts_g = H5F_ACS_METADATA_READ_ATTEMPTS_DEF;  /* Default setting for the # of metadata read attempts */
static const H5F_object_flush_t H5F_def_object_flush_cb_g = H5F_ACS_OBJECT_FLUSH_CB_DEF;  	   /* Default setting for object flush callback */
static const hbool_t H5F_def_clear_status_flags_g = H5F_ACS_CLEAR_STATUS_FLAGS_DEF;             /* Default to clear the superblock status_flags */


/*-------------------------------------------------------------------------
 * Function:    H5P_facc_reg_prop
 *
 * Purpose:     Register the file access property list class's properties
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              October 31, 2006
 *-------------------------------------------------------------------------
 */
static herr_t
H5P_facc_reg_prop(H5P_genclass_t *pclass)
{
    const hid_t def_driver_id = H5F_ACS_FILE_DRV_ID_DEF;        /* Default VFL driver ID (initialized from a variable) */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Register the initial metadata cache resize configuration */
    if(H5P_register_real(pclass, H5F_ACS_META_CACHE_INIT_CONFIG_NAME, H5F_ACS_META_CACHE_INIT_CONFIG_SIZE, &H5F_def_mdc_initCacheCfg_g, 
            NULL, NULL, NULL, H5F_ACS_META_CACHE_INIT_CONFIG_ENC, H5F_ACS_META_CACHE_INIT_CONFIG_DEC, 
            NULL, NULL, H5F_ACS_META_CACHE_INIT_CONFIG_CMP, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the size of raw data chunk cache (elements) */
    if(H5P_register_real(pclass, H5F_ACS_DATA_CACHE_NUM_SLOTS_NAME, H5F_ACS_DATA_CACHE_NUM_SLOTS_SIZE, &H5F_def_rdcc_nslots_g, 
            NULL, NULL, NULL, H5F_ACS_DATA_CACHE_NUM_SLOTS_ENC, H5F_ACS_DATA_CACHE_NUM_SLOTS_DEC, 
            NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the size of raw data chunk cache(bytes) */
    if(H5P_register_real(pclass, H5F_ACS_DATA_CACHE_BYTE_SIZE_NAME, H5F_ACS_DATA_CACHE_BYTE_SIZE_SIZE, &H5F_def_rdcc_nbytes_g, 
            NULL, NULL, NULL, H5F_ACS_DATA_CACHE_BYTE_SIZE_ENC, H5F_ACS_DATA_CACHE_BYTE_SIZE_DEC, 
            NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the preemption for reading chunks */
    if(H5P_register_real(pclass, H5F_ACS_PREEMPT_READ_CHUNKS_NAME, H5F_ACS_PREEMPT_READ_CHUNKS_SIZE, &H5F_def_rdcc_w0_g, 
            NULL, NULL, NULL, H5F_ACS_PREEMPT_READ_CHUNKS_ENC, H5F_ACS_PREEMPT_READ_CHUNKS_DEC, 
            NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the threshold for alignment */
    if(H5P_register_real(pclass, H5F_ACS_ALIGN_THRHD_NAME, H5F_ACS_ALIGN_THRHD_SIZE, &H5F_def_threshold_g, 
            NULL, NULL, NULL, H5F_ACS_ALIGN_THRHD_ENC, H5F_ACS_ALIGN_THRHD_DEC, 
            NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the alignment */
    if(H5P_register_real(pclass, H5F_ACS_ALIGN_NAME, H5F_ACS_ALIGN_SIZE, &H5F_def_alignment_g, 
            NULL, NULL, NULL, H5F_ACS_ALIGN_ENC, H5F_ACS_ALIGN_DEC, 
            NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the minimum metadata allocation block size */
    if(H5P_register_real(pclass, H5F_ACS_META_BLOCK_SIZE_NAME, H5F_ACS_META_BLOCK_SIZE_SIZE, &H5F_def_meta_block_size_g, 
            NULL, NULL, NULL, H5F_ACS_META_BLOCK_SIZE_ENC, H5F_ACS_META_BLOCK_SIZE_DEC, 
            NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the maximum sieve buffer size */
    if(H5P_register_real(pclass, H5F_ACS_SIEVE_BUF_SIZE_NAME, H5F_ACS_SIEVE_BUF_SIZE_SIZE, &H5F_def_sieve_buf_size_g, 
            NULL, NULL, NULL, H5F_ACS_SIEVE_BUF_SIZE_ENC, H5F_ACS_SIEVE_BUF_SIZE_DEC, 
            NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the minimum "small data" allocation block size */
    if(H5P_register_real(pclass, H5F_ACS_SDATA_BLOCK_SIZE_NAME, H5F_ACS_SDATA_BLOCK_SIZE_SIZE, &H5F_def_sdata_block_size_g, 
            NULL, NULL, NULL, H5F_ACS_SDATA_BLOCK_SIZE_ENC, H5F_ACS_SDATA_BLOCK_SIZE_DEC, 
            NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the garbage collection reference */
    if(H5P_register_real(pclass, H5F_ACS_GARBG_COLCT_REF_NAME, H5F_ACS_GARBG_COLCT_REF_SIZE, &H5F_def_gc_ref_g, 
            NULL, NULL, NULL, H5F_ACS_GARBG_COLCT_REF_ENC, H5F_ACS_GARBG_COLCT_REF_DEC, 
            NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the file driver ID */
    /* (Note: this property should not have an encode/decode callback -QAK) */
    if(H5P_register_real(pclass, H5F_ACS_FILE_DRV_ID_NAME, H5F_ACS_FILE_DRV_ID_SIZE, &def_driver_id, 
            NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the file driver info */
    /* (Note: this property should not have an encode/decode callback -QAK) */
    if(H5P_register_real(pclass, H5F_ACS_FILE_DRV_INFO_NAME, H5F_ACS_FILE_DRV_INFO_SIZE, &H5F_def_driver_info_g, 
            NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the file close degree */
    if(H5P_register_real(pclass, H5F_ACS_CLOSE_DEGREE_NAME, H5F_CLOSE_DEGREE_SIZE, &H5F_def_close_degree_g, 
            NULL, NULL, NULL, H5F_CLOSE_DEGREE_ENC, H5F_CLOSE_DEGREE_DEC, 
            NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the offset of family driver info */
    if(H5P_register_real(pclass, H5F_ACS_FAMILY_OFFSET_NAME, H5F_ACS_FAMILY_OFFSET_SIZE, &H5F_def_family_offset_g, 
            NULL, NULL, NULL, H5F_ACS_FAMILY_OFFSET_ENC, H5F_ACS_FAMILY_OFFSET_DEC, 
            NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the private property of new family file size. It's used by h5repart only. */
    /* (Note: this property should not have an encode/decode callback -QAK) */
    if(H5P_register_real(pclass, H5F_ACS_FAMILY_NEWSIZE_NAME, H5F_ACS_FAMILY_NEWSIZE_SIZE, &H5F_def_family_newsize_g, 
            NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the private property of whether convert family to sec2 driver. It's used by h5repart only. */
    /* (Note: this property should not have an encode/decode callback -QAK) */
    if(H5P_register_real(pclass, H5F_ACS_FAMILY_TO_SEC2_NAME, H5F_ACS_FAMILY_TO_SEC2_SIZE, &H5F_def_family_to_sec2_g, 
            NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the data type of multi driver info */
    if(H5P_register_real(pclass, H5F_ACS_MULTI_TYPE_NAME, H5F_ACS_MULTI_TYPE_SIZE, &H5F_def_mem_type_g, 
            NULL, NULL, NULL, H5F_ACS_MULTI_TYPE_ENC, H5F_ACS_MULTI_TYPE_DEC, 
            NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the 'use the latest version of the format' flag */
    if(H5P_register_real(pclass, H5F_ACS_LATEST_FORMAT_NAME, H5F_ACS_LATEST_FORMAT_SIZE, &H5F_def_latest_format_g, 
            NULL, NULL, NULL, H5F_ACS_LATEST_FORMAT_ENC, H5F_ACS_LATEST_FORMAT_DEC, 
            NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the private property of whether to retrieve the file descriptor from the core VFD */
    /* (used internally to the library only) */
    /* (Note: this property should not have an encode/decode callback -QAK) */
    if(H5P_register_real(pclass, H5F_ACS_WANT_POSIX_FD_NAME, H5F_ACS_WANT_POSIX_FD_SIZE, &H5F_def_want_posix_fd_g, 
            NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the external file cache size */
    if(H5P_register_real(pclass, H5F_ACS_EFC_SIZE_NAME, H5F_ACS_EFC_SIZE_SIZE, &H5F_def_efc_size_g, 
            NULL, NULL, NULL, H5F_ACS_EFC_SIZE_ENC, H5F_ACS_EFC_SIZE_DEC, 
            NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the initial file image info */
    /* (Note: this property should not have an encode/decode callback -QAK) */
    if(H5P_register_real(pclass, H5F_ACS_FILE_IMAGE_INFO_NAME, H5F_ACS_FILE_IMAGE_INFO_SIZE, &H5F_def_file_image_info_g, 
            NULL, NULL, NULL, NULL, NULL, 
            H5F_ACS_FILE_IMAGE_INFO_DEL, H5F_ACS_FILE_IMAGE_INFO_COPY, NULL, H5F_ACS_FILE_IMAGE_INFO_CLOSE) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the # of read attempts */
    if(H5P_register_real(pclass, H5F_ACS_METADATA_READ_ATTEMPTS_NAME, H5F_ACS_METADATA_READ_ATTEMPTS_SIZE, &H5F_def_metadata_read_attempts_g, 
            NULL, NULL, NULL, H5F_ACS_METADATA_READ_ATTEMPTS_ENC, H5F_ACS_METADATA_READ_ATTEMPTS_DEC, 
            NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register object flush callback */
    if(H5P_register_real(pclass, H5F_ACS_OBJECT_FLUSH_CB_NAME, H5F_ACS_OBJECT_FLUSH_CB_SIZE, &H5F_def_object_flush_cb_g, 
            NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the private property of whether to clear the superblock status_flags. It's used by h5clear only. */
    if(H5P_register_real(pclass, H5F_ACS_CLEAR_STATUS_FLAGS_NAME, H5F_ACS_CLEAR_STATUS_FLAGS_SIZE, &H5F_def_clear_status_flags_g,
            NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")


done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_facc_reg_prop() */


/*----------------------------------------------------------------------------
 * Function:	H5P_facc_create
 *
 * Purpose:	Callback routine which is called whenever a file access
 *		property list is closed.  This routine performs any generic
 * 		initialization needed on the properties the library put into
 * 		the list.
 *
 * Return:	Success:		Non-negative
 * 		Failure:		Negative
 *
 * Programmer:	Raymond Lu
 *		Tuesday, Oct 23, 2001
 *
 *----------------------------------------------------------------------------
 */
/* ARGSUSED */
static herr_t
H5P_facc_create(hid_t fapl_id, void UNUSED *copy_data)
{
    hid_t          driver_id;
    H5P_genplist_t *plist;              /* Property list */
    herr_t         ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Check argument */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")

    /* Retrieve driver ID property */
    if(H5P_get(plist, H5F_ACS_FILE_DRV_ID_NAME, &driver_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get driver ID")

    if(driver_id > 0) {
        void *driver_info;

        /* Retrieve driver info property */
        if(H5P_get(plist, H5F_ACS_FILE_DRV_INFO_NAME, &driver_info) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get driver info")

        /* Set the driver for the property list */
        if(H5FD_fapl_open(plist, driver_id, driver_info) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set driver")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_facc_create() */


/*--------------------------------------------------------------------------
 * Function:	H5P_facc_copy
 *
 * Purpose:	Callback routine which is called whenever a file access
 * 		property list is copied.  This routine performs any generic
 * 	 	copy needed on the properties.
 *
 * Return:	Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:	Raymond Lu
 *		Tuesday, Oct 23, 2001
 *
 *--------------------------------------------------------------------------
 */
/* ARGSUSED */
static herr_t
H5P_facc_copy(hid_t dst_fapl_id, hid_t src_fapl_id, void UNUSED *copy_data)
{
    hid_t          driver_id;
    H5P_genplist_t *src_plist;              /* Source property list */
    herr_t         ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Get driver ID from source property list */
    if(NULL == (src_plist = (H5P_genplist_t *)H5I_object(src_fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't get property list")
    if(H5P_get(src_plist, H5F_ACS_FILE_DRV_ID_NAME, &driver_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get driver ID")

    if(driver_id > 0) {
        H5P_genplist_t *dst_plist;              /* Destination property list */
        void *driver_info;

        /* Get driver info from source property list */
        if(H5P_get(src_plist, H5F_ACS_FILE_DRV_INFO_NAME, &driver_info) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get driver info")

        /* Set the driver for the destination property list */
        if(NULL == (dst_plist = (H5P_genplist_t *)H5I_object(dst_fapl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't get property list")
        if(H5FD_fapl_open(dst_plist, driver_id, driver_info) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set driver")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_facc_copy() */


/*--------------------------------------------------------------------------
 * Function:	H5P_facc_close
 *
 * Purpose:	Callback routine which is called whenever a file access
 *		property list is closed.  This routine performs any generic
 *		cleanup needed on the properties.
 *
 * Return:	Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:	Raymond Lu
 *		Tuesday, Oct 23, 2001
 *
 *---------------------------------------------------------------------------
 */
/* ARGSUSED */
herr_t
H5P_facc_close(hid_t fapl_id, void UNUSED *close_data)
{
    hid_t      driver_id;
    H5P_genplist_t *plist;              /* Property list */
    herr_t     ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Check argument */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")

    /* Get driver ID property */
    if(H5P_get(plist, H5F_ACS_FILE_DRV_ID_NAME, &driver_id) < 0)
        HGOTO_DONE(FAIL) /* Can't return errors when library is shutting down */

    if(driver_id > 0) {
        void *driver_info;

        /* Get driver info property */
        if(H5P_get(plist, H5F_ACS_FILE_DRV_INFO_NAME, &driver_info) < 0)
            HGOTO_DONE(FAIL) /* Can't return errors when library is shutting down */

        /* Close the driver for the property list */
        if(H5FD_fapl_close(driver_id, driver_info) < 0)
            HGOTO_DONE(FAIL) /* Can't return errors when library is shutting down */
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_facc_close() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_alignment
 *
 * Purpose:	Sets the alignment properties of a file access property list
 *		so that any file object >= THRESHOLD bytes will be aligned on
 *		an address which is a multiple of ALIGNMENT.  The addresses
 *		are relative to the end of the user block; the alignment is
 *		calculated by subtracting the user block size from the
 *		absolute file address and then adjusting the address to be a
 *		multiple of ALIGNMENT.
 *
 *		Default values for THRESHOLD and ALIGNMENT are one, implying
 *		no alignment.  Generally the default values will result in
 *		the best performance for single-process access to the file.
 *		For MPI-IO and other parallel systems, choose an alignment
 *		which is a multiple of the disk block size.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Tuesday, June  9, 1998
 *
 * Modifications:
 *
 *		Raymond Lu
 *		Tuesday, Oct 23, 2001
 *		Changed file access property list mechanism to the new
 *		generic property list.
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_alignment(hid_t fapl_id, hsize_t threshold, hsize_t alignment)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED;   /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "ihh", fapl_id, threshold, alignment);

    /* Check args */
    if(alignment < 1)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "alignment must be positive")

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(fapl_id,H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set values */
    if(H5P_set(plist, H5F_ACS_ALIGN_THRHD_NAME, &threshold) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set threshold")
    if(H5P_set(plist, H5F_ACS_ALIGN_NAME, &alignment) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set alignment")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_alignment
 *
 * Purpose:	Returns the current settings for alignment properties from a
 *		file access property list.  The THRESHOLD and/or ALIGNMENT
 *		pointers may be null pointers.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Tuesday, June  9, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_alignment(hid_t fapl_id, hsize_t *threshold/*out*/,
    hsize_t *alignment/*out*/)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "ixx", fapl_id, threshold, alignment);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(fapl_id,H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get values */
    if(threshold)
        if(H5P_get(plist, H5F_ACS_ALIGN_THRHD_NAME, threshold) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get threshold")
    if(alignment)
        if(H5P_get(plist, H5F_ACS_ALIGN_NAME, alignment) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get alignment")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_alignment() */


/*-------------------------------------------------------------------------
 * Function:	H5P_set_driver
 *
 * Purpose:	Set the file driver (DRIVER_ID) for a file access 
 *		property list (PLIST_ID) and supply an optional
 *		struct containing the driver-specific properites
 *		(DRIVER_INFO).  The driver properties will be copied into the
 *		property list and the reference count on the driver will be
 *		incremented, allowing the caller to close the driver ID but
 *		still use the property list.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Tuesday, August  3, 1999
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5P_set_driver(H5P_genplist_t *plist, hid_t new_driver_id, const void *new_driver_info)
{
    hid_t driver_id;            /* VFL driver ID */
    void *driver_info;          /* VFL driver info */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == H5I_object_verify(new_driver_id, H5I_VFL))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file driver ID")

    if(TRUE == H5P_isa_class(plist->plist_id, H5P_FILE_ACCESS)) {
        /* Get the current driver information */
        if(H5P_get(plist, H5F_ACS_FILE_DRV_ID_NAME, &driver_id) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get driver ID")
        if(H5P_get(plist, H5F_ACS_FILE_DRV_INFO_NAME, &driver_info) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET,FAIL,"can't get driver info")

        /* Close the driver for the property list */
        if(H5FD_fapl_close(driver_id, driver_info) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't reset driver")

        /* Set the driver for the property list */
        if(H5FD_fapl_open(plist, new_driver_id, new_driver_info) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set driver")
    } /* end if */
    else
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_set_driver() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_driver
 *
 * Purpose:	Set the file driver (DRIVER_ID) for a file access 
 *		property list (PLIST_ID) and supply an optional
 *		struct containing the driver-specific properites
 *		(DRIVER_INFO).  The driver properties will be copied into the
 *		property list and the reference count on the driver will be
 *		incremented, allowing the caller to close the driver ID but
 *		still use the property list.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Tuesday, August  3, 1999
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_driver(hid_t plist_id, hid_t new_driver_id, const void *new_driver_info)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "ii*x", plist_id, new_driver_id, new_driver_info);

    /* Check arguments */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object_verify(plist_id, H5I_GENPROP_LST)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")
    if(NULL == H5I_object_verify(new_driver_id, H5I_VFL))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file driver ID")

    /* Set the driver */
    if(H5P_set_driver(plist, new_driver_id, new_driver_info) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set driver info")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_driver() */


/*-------------------------------------------------------------------------
 * Function:	H5P_get_driver
 *
 * Purpose:	Return the ID of the low-level file driver.  PLIST_ID should
 *		be a file access property list.
 *
 * Return:	Success:	A low-level driver ID which is the same ID
 *				used when the driver was set for the property
 *				list. The driver ID is only valid as long as
 *				the file driver remains registered.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Thursday, February 26, 1998
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5P_get_driver(H5P_genplist_t *plist)
{
    hid_t ret_value = FAIL;     /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Get the current driver ID */
    if(TRUE == H5P_isa_class(plist->plist_id, H5P_FILE_ACCESS)) {
        if(H5P_get(plist, H5F_ACS_FILE_DRV_ID_NAME, &ret_value) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get driver ID")
    } /* end if */
    else
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    if(H5FD_VFD_DEFAULT == ret_value)
        ret_value = H5_DEFAULT_VFD;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_get_driver() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_driver
 *
 * Purpose:	Return the ID of the low-level file driver.  PLIST_ID should
 *		be a file access property list.
 *
 * Return:	Success:	A low-level driver ID which is the same ID
 *				used when the driver was set for the property
 *				list. The driver ID is only valid as long as
 *				the file driver remains registered.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Thursday, February 26, 1998
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Pget_driver(hid_t plist_id)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    hid_t	ret_value;      /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("i", "i", plist_id);

    if(NULL == (plist = (H5P_genplist_t *)H5I_object_verify(plist_id, H5I_GENPROP_LST)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")

    /* Get the driver */
    if((ret_value = H5P_get_driver(plist)) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get driver")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_driver() */


/*-------------------------------------------------------------------------
 * Function:	H5P_get_driver_info
 *
 * Purpose:	Returns a pointer directly to the file driver-specific
 *		information of a file access.
 *
 * Return:	Success:	Ptr to *uncopied* driver specific data
 *				structure if any.
 *
 *		Failure:	NULL. Null is also returned if the driver has
 *				not registered any driver-specific properties
 *				although no error is pushed on the stack in
 *				this case.
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 *-------------------------------------------------------------------------
 */
void *
H5P_get_driver_info(H5P_genplist_t *plist)
{
    void *ret_value = NULL;     /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* Get the current driver info */
    if(TRUE == H5P_isa_class(plist->plist_id, H5P_FILE_ACCESS)) {
        if(H5P_get(plist, H5F_ACS_FILE_DRV_INFO_NAME, &ret_value) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get driver info")
    } /* end if */
    else
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_get_driver_info() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_driver_info
 *
 * Purpose:	Returns a pointer directly to the file driver-specific
 *		information of a file access.
 *
 * Return:	Success:	Ptr to *uncopied* driver specific data
 *				structure if any.
 *
 *		Failure:	NULL. Null is also returned if the driver has
 *				not registered any driver-specific properties
 *				although no error is pushed on the stack in
 *				this case.
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 *-------------------------------------------------------------------------
 */
void *
H5Pget_driver_info(hid_t plist_id)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    void *ret_value;            /* Return value */

    FUNC_ENTER_API(NULL)

    if(NULL == (plist = (H5P_genplist_t *)H5I_object_verify(plist_id, H5I_GENPROP_LST)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a property list")

    /* Get the driver info */
    if(NULL == (ret_value = H5P_get_driver_info(plist)))
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get driver info")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_driver_info() */


/*-------------------------------------------------------------------------
 * Function:    H5Pset_family_offset
 *
 * Purpose:     Set offset for family driver.  This file access property
 *              list will be passed to H5Fget_vfd_handle or H5FDget_vfd_handle
 *              to retrieve VFD file handle.
 *
 * Return:      Success:        Non-negative value.
 *              Failure:        Negative value.
 *
 * Programmer:  Raymond Lu
 *              Sep 17, 2002
 *
 *-------------------------------------------------------------------------
*/
herr_t
H5Pset_family_offset(hid_t fapl_id, hsize_t offset)
{
    H5P_genplist_t      *plist;                 /* Property list pointer */
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ih", fapl_id, offset);

    /* Get the plist structure */
    if(H5P_DEFAULT == fapl_id)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "can't modify default property list")
    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set value */
    if(H5P_set(plist, H5F_ACS_FAMILY_OFFSET_NAME, &offset) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set offset for family file")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_family_offset() */


/*-------------------------------------------------------------------------
 * Function:    H5Pget_family_offset
 *
 * Purpose:     Get offset for family driver.  This file access property
 *              list will be passed to H5Fget_vfd_handle or H5FDget_vfd_handle
 *              to retrieve VFD file handle.
 *
 * Return:      Success:        Non-negative value.
 *              Failure:        Negative value.
 *
 * Programmer:  Raymond Lu
 *              Sep 17, 2002
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_family_offset(hid_t fapl_id, hsize_t *offset)
{
    H5P_genplist_t      *plist;                 /* Property list pointer */
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*h", fapl_id, offset);

    /* Get the plist structure */
    if(H5P_DEFAULT == fapl_id)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "can't modify default property list")
    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get value */
    if(offset) {
        if(H5P_get(plist, H5F_ACS_FAMILY_OFFSET_NAME, offset) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set offset for family file")
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_family_offset() */


/*-------------------------------------------------------------------------
 * Function:    H5Pset_multi_type
 *
 * Purpose:     Set data type for multi driver.  This file access property
 *              list will be passed to H5Fget_vfd_handle or H5FDget_vfd_handle
 *              to retrieve VFD file handle.
 *
 * Return:      Success:        Non-negative value.
 *              Failure:        Negative value.
 *
 * Programmer:  Raymond Lu
 *              Sep 17, 2002
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_multi_type(hid_t fapl_id, H5FD_mem_t type)
{
    H5P_genplist_t      *plist;                 /* Property list pointer */
    herr_t              ret_value = SUCCEED;      /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iMt", fapl_id, type);

    /* Get the plist structure */
    if(H5P_DEFAULT == fapl_id)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "can't modify default property list")
    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set value */
     if(H5P_set(plist, H5F_ACS_MULTI_TYPE_NAME, &type) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set type for multi driver")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_multi_type() */


/*-------------------------------------------------------------------------
 * Function:    H5Pget_multi_type
 *
 * Purpose:     Get data type for multi driver.  This file access property
 *              list will be passed to H5Fget_vfd_handle or H5FDget_vfd_handle
 *              to retrieve VFD file handle.
 *
 * Return:      Success:        Non-negative value.
 *              Failure:        Negative value.
 *
 * Programmer:  Raymond Lu
 *              Sep 17, 2002
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_multi_type(hid_t fapl_id, H5FD_mem_t *type)
{
    H5P_genplist_t      *plist;                 /* Property list pointer */
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*Mt", fapl_id, type);

    /* Get the plist structure */
    if(H5P_DEFAULT == fapl_id)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "can't modify default property list")
    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get value */
    if(type) {
        if(H5P_get(plist, H5F_ACS_MULTI_TYPE_NAME, type) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't get type for multi driver")
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_multi_type() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_cache
 *
 * Purpose:	Set the number of objects in the meta data cache and the
 *		maximum number of chunks and bytes in the raw data chunk
 *		cache.
 *
 * 		The RDCC_W0 value should be between 0 and 1 inclusive and
 *		indicates how much chunks that have been fully read or fully
 *		written are favored for preemption.  A value of zero means
 *		fully read or written chunks are treated no differently than
 *		other chunks (the preemption is strictly LRU) while a value
 *		of one means fully read chunks are always preempted before
 *		other chunks.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Tuesday, May 19, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_cache(hid_t plist_id, int UNUSED mdc_nelmts,
	     size_t rdcc_nslots, size_t rdcc_nbytes, double rdcc_w0)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED;   /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("e", "iIszzd", plist_id, mdc_nelmts, rdcc_nslots, rdcc_nbytes,
             rdcc_w0);

    /* Check arguments */
    if(rdcc_w0 < 0.0 || rdcc_w0 > 1.0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "raw data cache w0 value must be between 0.0 and 1.0 inclusive")

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id,H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set sizes */
    if(H5P_set(plist, H5F_ACS_DATA_CACHE_NUM_SLOTS_NAME, &rdcc_nslots) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET,FAIL, "can't set data cache number of slots")
    if(H5P_set(plist, H5F_ACS_DATA_CACHE_BYTE_SIZE_NAME, &rdcc_nbytes) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET,FAIL, "can't set data cache byte size")
    if(H5P_set(plist, H5F_ACS_PREEMPT_READ_CHUNKS_NAME, &rdcc_w0) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET,FAIL, "can't set preempt read chunks")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_cache() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_cache
 *
 * Purpose:	Retrieves the maximum possible number of elements in the meta
 *		data cache and the maximum possible number of elements and
 *		bytes and the RDCC_W0 value in the raw data chunk cache.  Any
 *		(or all) arguments may be null pointers in which case the
 *		corresponding datum is not returned.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Tuesday, May 19, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_cache(hid_t plist_id, int *mdc_nelmts,
	     size_t *rdcc_nslots, size_t *rdcc_nbytes, double *rdcc_w0)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED;   /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("e", "i*Is*z*z*d", plist_id, mdc_nelmts, rdcc_nslots, rdcc_nbytes,
             rdcc_w0);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id,H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get sizes */

    /* the mdc_nelmts FAPL entry no longer exists, so just return a constant */
    if(mdc_nelmts)
        *mdc_nelmts = 0;

    if(rdcc_nslots)
        if(H5P_get(plist, H5F_ACS_DATA_CACHE_NUM_SLOTS_NAME, rdcc_nslots) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET,FAIL, "can't get data cache number of slots")
    if(rdcc_nbytes)
        if(H5P_get(plist, H5F_ACS_DATA_CACHE_BYTE_SIZE_NAME, rdcc_nbytes) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET,FAIL, "can't get data cache byte size")
    if(rdcc_w0)
        if(H5P_get(plist, H5F_ACS_PREEMPT_READ_CHUNKS_NAME, rdcc_w0) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET,FAIL, "can't get preempt read chunks")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_cache() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_mdc_config
 *
 * Purpose:	Set the initial metadata cache resize configuration in the
 *		target FAPL.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	J. Mainzer
 *              Thursday, April 7, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_mdc_config(hid_t plist_id, H5AC_cache_config_t *config_ptr)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED;   /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*x", plist_id, config_ptr);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id,H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* validate the new configuration */
    if(H5AC_validate_config(config_ptr) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid metadata cache configuration")

    /* set the modified config */

    /* If we ever support multiple versions of H5AC_cache_config_t, we
     * will have to test the version and do translation here.
     */

    if(H5P_set(plist, H5F_ACS_META_CACHE_INIT_CONFIG_NAME, config_ptr) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set metadata cache initial config")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Pset_mdc_config() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_mdc_config
 *
 * Purpose:	Retrieve the metadata cache initial resize configuration
 *		from the target FAPL.
 *
 *		Observe that the function will fail if config_ptr is
 *		NULL, or if config_ptr->version specifies an unknown
 *		version of H5AC_cache_config_t.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	J. Mainzer
 *              Thursday, April 7, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_mdc_config(hid_t plist_id, H5AC_cache_config_t *config_ptr)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED;   /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*x", plist_id, config_ptr);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id,H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* validate the config_ptr */
    if(config_ptr == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL config_ptr on entry.")

    if(config_ptr->version != H5AC__CURR_CACHE_CONFIG_VERSION)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Unknown config version.")

    /* If we ever support multiple versions of H5AC_cache_config_t, we
     * will have to get the cannonical version here, and then translate
     * to the version of the structure supplied.
     */

    /* Get the current initial metadata cache resize configuration */
    if(H5P_get(plist, H5F_ACS_META_CACHE_INIT_CONFIG_NAME, config_ptr) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET,FAIL, "can't get metadata cache initial resize config")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Pget_mdc_config() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_gc_references
 *
 * Purpose:	Sets the flag for garbage collecting references for the file.
 *		Dataset region references (and other reference types
 *		probably) use space in the file heap.  If garbage collection
 *		is on and the user passes in an uninitialized value in a
 *		reference structure, the heap might get corrupted.  When
 *		garbage collection is off however and the user re-uses a
 *		reference, the previous heap block will be orphaned and not
 *		returned to the free heap space.  When garbage collection is
 *		on, the user must initialize the reference structures to 0 or
 *		risk heap corruption.
 *
 *		Default value for garbage collecting references is off, just
 *		to be on the safe side.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		June, 1999
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
H5Pset_gc_references(hid_t plist_id, unsigned gc_ref)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED;   /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iIu", plist_id, gc_ref);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id,H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set values */
    if(H5P_set(plist, H5F_ACS_GARBG_COLCT_REF_NAME, &gc_ref) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set garbage collect reference")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_gc_references
 *
 * Purpose:	Returns the current setting for the garbage collection
 *		references property from a file access property list.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              June, 1999
 *
 * Modifications:
 *
 *		Raymond Lu
 *		Tuesday, Oct 23, 2001
 *		Changed the file access list to the new generic property
 *		list.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_gc_references(hid_t plist_id, unsigned *gc_ref/*out*/)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED;   /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ix", plist_id, gc_ref);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id,H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get values */
    if(gc_ref)
        if(H5P_get(plist, H5F_ACS_GARBG_COLCT_REF_NAME, gc_ref) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get garbage collect reference")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:    H5Pset_fclose_degree
 *
 * Purpose:     Sets the degree for the file close behavior.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Raymond Lu
 *              November, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fclose_degree(hid_t plist_id, H5F_close_degree_t degree)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED;   /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iFd", plist_id, degree);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id,H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set values */
    if(H5P_set(plist, H5F_ACS_CLOSE_DEGREE_NAME, &degree) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set file close degree")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_fclose_degree() */


/*-------------------------------------------------------------------------
 * Function:    H5Pget_fclose_degree
 *
 * Purpose:     Returns the degree for the file close behavior.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Raymond Lu
 *              November, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_fclose_degree(hid_t plist_id, H5F_close_degree_t *degree)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED;   /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*Fd", plist_id, degree);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    if(degree && H5P_get(plist, H5F_ACS_CLOSE_DEGREE_NAME, degree) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get file close degree")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_fclose_degree() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_meta_block_size
 *
 * Purpose:	Sets the minimum size of metadata block allocations when
 *      the H5FD_FEAT_AGGREGATE_METADATA is set by a VFL driver.
 *      Each "raw" metadata block is allocated to be this size and then
 *      specific pieces of metadata (object headers, local heaps, B-trees, etc)
 *      are sub-allocated from this block.
 *
 *		The default value is set to 2048 (bytes), indicating that metadata
 *      will be attempted to be bunched together in (at least) 2K blocks in
 *      the file.  Setting the value to 0 with this API function will
 *      turn off the metadata aggregation, even if the VFL driver attempts to
 *      use that strategy.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Friday, August 25, 2000
 *
 * Modifications:
 *
 *		Raymond Lu
 *		Tuesday, Oct 23, 2001
 *		Changed the file access list to the new generic property
 *		list.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_meta_block_size(hid_t plist_id, hsize_t size)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED;   /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ih", plist_id, size);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id,H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set values */
    if(H5P_set(plist, H5F_ACS_META_BLOCK_SIZE_NAME, &size) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set meta data block size")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_meta_block_size
 *
 * Purpose:	Returns the current settings for the metadata block allocation
 *      property from a file access property list.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Friday, August 29, 2000
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
H5Pget_meta_block_size(hid_t plist_id, hsize_t *size/*out*/)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED;   /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ix", plist_id, size);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id,H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get values */
    if(size) {
        if(H5P_get(plist, H5F_ACS_META_BLOCK_SIZE_NAME, size) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get meta data block size")
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_sieve_buf_size
 *
 * Purpose:	Sets the maximum size of the data seive buffer used for file
 *      drivers which are capable of using data sieving.  The data sieve
 *      buffer is used when performing I/O on datasets in the file.  Using a
 *      buffer which is large anough to hold several pieces of the dataset
 *      being read in for hyperslab selections boosts performance by quite a
 *      bit.
 *
 *		The default value is set to 64KB, indicating that file I/O for raw data
 *      reads and writes will occur in at least 64KB blocks.
 *      Setting the value to 0 with this API function will turn off the
 *      data sieving, even if the VFL driver attempts to use that strategy.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, September 21, 2000
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
H5Pset_sieve_buf_size(hid_t plist_id, size_t size)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED;   /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iz", plist_id, size);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id,H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set values */
    if(H5P_set(plist, H5F_ACS_SIEVE_BUF_SIZE_NAME, &size) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set sieve buffer size")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_sieve_buf_size() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_sieve_buf_size
 *
 * Purpose:	Returns the current settings for the data sieve buffer size
 *      property from a file access property list.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, September 21, 2000
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
H5Pget_sieve_buf_size(hid_t plist_id, size_t *size/*out*/)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED;   /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ix", plist_id, size);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id,H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get values */
    if(size)
        if(H5P_get(plist, H5F_ACS_SIEVE_BUF_SIZE_NAME, size) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get sieve buffer size")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_sieve_buf_size() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_small_data_block_size
 *
 * Purpose:	Sets the minimum size of "small" raw data block allocations
 *      when the H5FD_FEAT_AGGREGATE_SMALLDATA is set by a VFL driver.
 *      Each "small" raw data block is allocated to be this size and then
 *      pieces of raw data which are small enough to fit are sub-allocated from
 *      this block.
 *
 *	The default value is set to 2048 (bytes), indicating that raw data
 *      smaller than this value will be attempted to be bunched together in (at
 *      least) 2K blocks in the file.  Setting the value to 0 with this API
 *      function will turn off the "small" raw data aggregation, even if the
 *      VFL driver attempts to use that strategy.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, June 5, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_small_data_block_size(hid_t plist_id, hsize_t size)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED;   /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ih", plist_id, size);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id,H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set values */
    if(H5P_set(plist, H5F_ACS_SDATA_BLOCK_SIZE_NAME, &size) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set 'small data' block size")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_small_data_block_size() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_small_data_block_size
 *
 * Purpose:	Returns the current settings for the "small" raw data block
 *      allocation property from a file access property list.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, June 5, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_small_data_block_size(hid_t plist_id, hsize_t *size/*out*/)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED;   /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ix", plist_id, size);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id,H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get values */
    if(size) {
        if(H5P_get(plist, H5F_ACS_SDATA_BLOCK_SIZE_NAME, size) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get 'small data' block size")
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_small_data_block_size() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_libver_bounds
 *
 * Purpose:	Indicates which versions of the file format the library should
 *      use when creating objects.  LOW is the earliest version of the HDF5
 *      library that is guaranteed to be able to access the objects created
 *      (the format of some objects in an HDF5 file may not have changed between
 *      versions of the HDF5 library, possibly allowing earlier versions of the
 *      HDF5 library to access those objects) and HIGH is the latest version
 *      of the library required to access the objects created (later versions
 *      of the HDF5 library will also be able to access those objects).
 *
 *      LOW is used to require that objects use a more modern format and HIGH
 *      is used to restrict objects from using a more modern format.
 *
 *      The special values of H5F_FORMAT_EARLIEST and H5F_FORMAT_LATEST can be
 *      used in the following manner:  Setting LOW and HIGH to H5F_FORMAT_LATEST
 *      will produce files whose objects use the latest version of the file
 *      format available in the current HDF5 library for each object created.
 *      Setting LOW and HIGH to H5F_FORMAT_EARLIEST will produce files that that
 *      always require the use of the earliest version of the file format for
 *      each object created. [NOTE!  LOW=HIGH=H5F_FORMAT_EARLIEST is not
 *      implemented as of version 1.8.0 and setting LOW and HIGH to
 *      H5F_FORMAT_EARLIEST will produce an error currently].
 *
 *      Currently, the only two valid combinations for this routine are:
 *      LOW = H5F_FORMAT_EARLIEST and HIGH = H5F_FORMAT_LATEST (the default
 *      setting, which creates objects with the ealiest version possible for
 *      each object, but no upper limit on the version allowed to be created if
 *      a newer version of an object's format is required to support a feature
 *      requested with an HDF5 library API routine), and LOW = H5F_FORMAT_LATEST
 *      and HIGH = H5F_FORMAT_LATEST (which is described above).
 *
 *      The LOW and HIGH values set with this routine at imposed with each
 *      HDF5 library API call that creates objects in the file.  API calls that
 *      would violate the LOW or HIGH format bound will fail.
 *
 *      Setting the LOW and HIGH values will not affect reading/writing existing
 *      objects, only the creation of new objects.
 *
 * Note: Eventually we want to add more values to the H5F_libver_t
 *      enumerated type that indicate library release values where the file
 *      format was changed (like "H5F_FORMAT_1_2_0" for the file format changes
 *      in the 1.2.x release branch and possily even "H5F_FORMAT_1_4_2" for
 *      a change mid-way through the 1.4.x release branch, etc).
 *
 *      Adding more values will allow applications to make settings like the
 *      following:
 *          LOW = H5F_FORMAT_EARLIEST, HIGH = H5F_FORMAT_1_2_0 => Create objects
 *              with the earliest possible format and don't allow any objects
 *              to be created that require a library version greater than 1.2.x
 *              (This is the "make certain that <application> linked with v1.2.x
 *              of the library can read the file produced" use case)
 *
 *          LOW = H5F_FORMAT_1_4_2, HIGH = H5F_FORMAT_LATEST => create objects
 *              with at least the version of their format that the 1.4.2 library
 *              uses and allow any later version of the object's format
 *              necessary to represent features used.
 *              (This is the "make certain to take advantage of <new feature>
 *              in the file format" use case (maybe <new feature> is smaller
 *              or scales better than an ealier version, which would otherwise
 *              be used))
 *
 *         LOW = H5F_FORMAT_1_2_0, HIGH = H5F_FORMAT_1_6_0 => creates objects
 *              with at least the version of their format that the 1.2.x library
 *              uses and don't allow any objects to be created that require a
 *              library version greater than 1.6.x.
 *              (Not certain of a particular use case for these settings,
 *              although its probably just the logical combination of the
 *              previous two; it just falls out as possible/logical (if it turns
 *              out to be hard to implement in some way, we can always disallow
 *              it))
 *
 * Note #2:     We talked about whether to include enum values for only library
 *      versions where the format changed and decided it would be less confusing
 *      for application developers if we include enum values for _all_ library
 *      releases and then map down to the previous actual library release which
 *      had a format change.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Sunday, December 30, 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_libver_bounds(hid_t plist_id, H5F_libver_t low,
    H5F_libver_t high)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    hbool_t latest;             /* Whether to use the latest version or not */
    herr_t ret_value = SUCCEED;   /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "iFvFv", plist_id, low, high);

    /* Check args */
    /* (Note that this is _really_ restricted right now, we'll want to loosen
     *  this up more as we add features - QAK)
     */
    if(high != H5F_LIBVER_LATEST)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid high library version bound")

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set values */
    latest = (hbool_t)((low == H5F_LIBVER_LATEST) ? TRUE : FALSE);
    if(H5P_set(plist, H5F_ACS_LATEST_FORMAT_NAME, &latest) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set library version bounds")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_libver_bounds() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_libver_bounds
 *
 * Purpose:	Returns the current settings for the library version format bounds
 *      from a file access property list.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, January 3, 2008
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_libver_bounds(hid_t plist_id, H5F_libver_t *low/*out*/,
    H5F_libver_t *high/*out*/)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    hbool_t latest;             /* Whether to use the latest version or not */
    herr_t ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "ixx", plist_id, low, high);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get value */
    if(H5P_get(plist, H5F_ACS_LATEST_FORMAT_NAME, &latest) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get library version bounds")

    /* Check for setting values to return */
    /* (Again, this is restricted now, we'll need to open it up later -QAK) */
    if(low)
        *low = latest ? H5F_LIBVER_LATEST : H5F_LIBVER_EARLIEST;
    if(high)
        *high = H5F_LIBVER_LATEST;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_libver_bounds() */


/*-------------------------------------------------------------------------
 * Function:    H5Pset_elink_file_cache_size
 *
 * Purpose:     Sets the number of files opened through external links
 *              from the file associated with this fapl to be held open
 *              in that file's external file cache.  When the maximum
 *              number of files is reached, the least recently used file
 *              is closed (unless it is opened from somewhere else).
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              Friday, December 17, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_elink_file_cache_size(hid_t plist_id, unsigned efc_size)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED;   /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iIu", plist_id, efc_size);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set value */
    if(H5P_set(plist, H5F_ACS_EFC_SIZE_NAME, &efc_size) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set elink file cache size")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_elink_file_cache_size() */


/*-------------------------------------------------------------------------
 * Function:    H5Pget_elink_file_cache_size
 *
 * Purpose:     Gets the number of files opened through external links
 *              from the file associated with this fapl to be held open
 *              in that file's external file cache.  When the maximum
 *              number of files is reached, the least recently used file
 *              is closed (unless it is opened from somewhere else).
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              Friday, December 17, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_elink_file_cache_size(hid_t plist_id, unsigned *efc_size)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED;   /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*Iu", plist_id, efc_size);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get value */
    if(efc_size)
        if(H5P_get(plist, H5F_ACS_EFC_SIZE_NAME, efc_size) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get elink file cache size")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_elink_file_cache_size() */


/*-------------------------------------------------------------------------
 * Function: H5Pset_file_image
 *
 * Purpose:     Sets the initial file image. Some file drivers can initialize 
 *              the starting data in a file from a buffer. 
 *              
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Jacob Gruber
 *              Thurday, August 11, 2011
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_file_image(hid_t fapl_id, void *buf_ptr, size_t buf_len)
{
    H5P_genplist_t *fapl;               /* Property list pointer */
    H5FD_file_image_info_t image_info;  /* File image info */
    herr_t ret_value = SUCCEED;         /* Return value */
    
    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "i*xz", fapl_id, buf_ptr, buf_len);

    /* validate parameters */
    if(!(((buf_ptr == NULL) && (buf_len == 0)) || ((buf_ptr != NULL) && (buf_len > 0))))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "inconsistant buf_ptr and buf_len")
   
    /* Get the plist structure */
    if(NULL == (fapl = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")
        
    /* Get old image info */
    if(H5P_get(fapl, H5F_ACS_FILE_IMAGE_INFO_NAME, &image_info) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get old file image pointer")
        
    /* Release previous buffer, if it exists */
    if(image_info.buffer != NULL) {
        if(image_info.callbacks.image_free) {
            if(SUCCEED != image_info.callbacks.image_free(image_info.buffer, H5FD_FILE_IMAGE_OP_PROPERTY_LIST_SET, image_info.callbacks.udata))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, "image_free callback failed")
        } /* end if */
        else
            H5MM_xfree(image_info.buffer);
    } /* end if */  
    
    /* Update struct */
    if(buf_ptr) {
        /* Allocate memory */
        if(image_info.callbacks.image_malloc) {
            if(NULL == (image_info.buffer = image_info.callbacks.image_malloc(buf_len,
                    H5FD_FILE_IMAGE_OP_PROPERTY_LIST_SET, image_info.callbacks.udata)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "image malloc callback failed")
         } /* end if */
         else
            if(NULL == (image_info.buffer = H5MM_malloc(buf_len)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to allocate memory block")
    
        /* Copy data */
        if(image_info.callbacks.image_memcpy) {
            if(image_info.buffer != image_info.callbacks.image_memcpy(image_info.buffer, 
                   buf_ptr, buf_len, H5FD_FILE_IMAGE_OP_PROPERTY_LIST_SET, 
                   image_info.callbacks.udata))
	        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTCOPY, FAIL, "image_memcpy callback failed")
        } /* end if */
	else
            HDmemcpy(image_info.buffer, buf_ptr, buf_len);
    } /* end if */
    else
        image_info.buffer = NULL;

    image_info.size = buf_len;

    /* Set values */
    if(H5P_set(fapl, H5F_ACS_FILE_IMAGE_INFO_NAME, &image_info) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set file image info")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_file_image() */


/*-------------------------------------------------------------------------
 * Function: H5Pget_file_image
 *
 * Purpose:     If the file image exists and buf_ptr_ptr is not NULL, 
 *		allocate a buffer of the correct size, copy the image into 
 *		the new buffer, and return the buffer to the caller in 
 *		*buf_ptr_ptr.  Do this using the file image callbacks
 *		if defined.  
 *
 *		NB: It is the responsibility of the caller to free the 
 *		buffer whose address is returned in *buf_ptr_ptr.  Do
 *		this using free if the file image callbacks are not 
 *		defined, or with whatever method is appropriate if 
 *		the callbacks are defined.
 *
 *              If buf_ptr_ptr is not NULL, and no image exists, set 
 *		*buf_ptr_ptr to NULL.
 *
 *		If buf_len_ptr is not NULL, set *buf_len_ptr equal
 *		to the length of the file image if it exists, and 
 *		to 0 if it does not.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Jacob Gruber
 *              Thurday, August 11, 2011
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_file_image(hid_t fapl_id, void **buf_ptr_ptr, size_t *buf_len_ptr)
{
    H5P_genplist_t *fapl;               /* Property list pointer */
    H5FD_file_image_info_t image_info;  /* File image info */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "i**x*z", fapl_id, buf_ptr_ptr, buf_len_ptr);

    /* Get the plist structure */
    if(NULL == (fapl = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get values */
    if(H5P_get(fapl, H5F_ACS_FILE_IMAGE_INFO_NAME, &image_info) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get file image info")

    /* verify file image field consistancy */
    HDassert(((image_info.buffer != NULL) && (image_info.size > 0)) || 
             ((image_info.buffer == NULL) && (image_info.size == 0)));

    /* Set output size */
    if(buf_len_ptr != NULL)
        *buf_len_ptr = image_info.size;

    /* Duplicate the image if desired, using callbacks if available */
    if(buf_ptr_ptr != NULL) {
        void * copy_ptr = NULL;         /* Copy of memory image */

        if(image_info.buffer != NULL) {
            /* Allocate memory */
            if(image_info.callbacks.image_malloc) {
                if(NULL == (copy_ptr = image_info.callbacks.image_malloc(image_info.size,
                        H5FD_FILE_IMAGE_OP_PROPERTY_LIST_GET, image_info.callbacks.udata)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "image malloc callback failed")
            } /* end if */
            else
                if(NULL == (copy_ptr = H5MM_malloc(image_info.size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to allocate copy")
    
            /* Copy data */
            if(image_info.callbacks.image_memcpy) {
                if(copy_ptr != image_info.callbacks.image_memcpy(copy_ptr, image_info.buffer,
                        image_info.size, H5FD_FILE_IMAGE_OP_PROPERTY_LIST_GET, 
                        image_info.callbacks.udata))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTCOPY, FAIL, "image_memcpy callback failed")
            } /* end if */
	    else
                HDmemcpy(copy_ptr, image_info.buffer, image_info.size);
        } /* end if */

        *buf_ptr_ptr = copy_ptr;
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_file_image */


/*-------------------------------------------------------------------------
 * Function: H5Pset_file_image_callbacks
 *
 * Purpose:     Sets the callbacks for file images. Some file drivers allow
 *              the use of user-defined callbacks for allocating, freeing and
 *              copying the drivers internal buffer, potentially allowing a 
 *              clever user to do optimizations such as avoiding large mallocs
 *              and memcpys or to perform detailed logging.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Jacob Gruber
 *              Thurday, August 11, 2011
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_file_image_callbacks(hid_t fapl_id, H5FD_file_image_callbacks_t *callbacks_ptr)
{
    H5P_genplist_t *fapl;               /* Property list pointer */
    H5FD_file_image_info_t info;        /* File image info */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*x", fapl_id, callbacks_ptr);

    /* Get the plist structure */
    if(NULL == (fapl = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get old info */
    if(H5P_get(fapl, H5F_ACS_FILE_IMAGE_INFO_NAME, &info) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get old file image info")

    /* verify file image field consistancy */
    HDassert(((info.buffer != NULL) && (info.size > 0)) || 
             ((info.buffer == NULL) && (info.size == 0)));

    /* Make sure a file image hasn't already been set */
    if(info.buffer != NULL || info.size > 0)
        HGOTO_ERROR(H5E_PLIST, H5E_SETDISALLOWED, FAIL, "setting callbacks when an image is already set is forbidden. It could cause memory leaks.")

    /* verify that callbacks_ptr is not NULL */
    if(NULL == callbacks_ptr)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL callbacks_ptr")

    /* Make sure udata callbacks are going to be set if udata is going to be set */
    if(callbacks_ptr->udata)
        if(callbacks_ptr->udata_copy == NULL || callbacks_ptr->udata_free == NULL)
            HGOTO_ERROR(H5E_PLIST, H5E_SETDISALLOWED, FAIL, "udata callbacks must be set if udata is set")

    /* Release old udata if it exists */
    if(info.callbacks.udata != NULL) {
        HDassert(info.callbacks.udata_free);
        if(info.callbacks.udata_free(info.callbacks.udata) < 0)
	    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, "udata_free callback failed")
    } /* end if */

    /* Update struct */
    info.callbacks = *callbacks_ptr;

    if(callbacks_ptr->udata) {
        HDassert(callbacks_ptr->udata_copy);
        HDassert(callbacks_ptr->udata_free);
        if((info.callbacks.udata = callbacks_ptr->udata_copy(callbacks_ptr->udata)) == NULL)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't copy the suppplied udata")
    } /* end if */

    /* Set values */
    if(H5P_set(fapl, H5F_ACS_FILE_IMAGE_INFO_NAME, &info) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set file image info")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_file_image_callbacks() */


/*-------------------------------------------------------------------------
 * Function: H5Pget_file_image_callbacks
 *
 * Purpose:     Sets the callbacks for file images. Some file drivers allow
 *              the use of user-defined callbacks for allocating, freeing and
 *              copying the drivers internal buffer, potentially allowing a 
 *              clever user to do optimizations such as avoiding large mallocs
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Jacob Gruber
 *              Thurday, August 11, 2011
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_file_image_callbacks(hid_t fapl_id, H5FD_file_image_callbacks_t *callbacks_ptr)
{
    H5P_genplist_t *fapl;               /* Property list pointer */
    H5FD_file_image_info_t info;        /* File image info */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*x", fapl_id, callbacks_ptr);

    /* Get the plist structure */
    if(NULL == (fapl = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get old info */
    if(H5P_get(fapl, H5F_ACS_FILE_IMAGE_INFO_NAME, &info) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get file image info")

    /* verify file image field consistancy */
    HDassert(((info.buffer != NULL) && (info.size > 0)) || 
             ((info.buffer == NULL) && (info.size == 0)));

    /* verify that callbacks_ptr is not NULL */
    if(NULL == callbacks_ptr)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL callbacks_ptr")

    /* Transfer values to parameters */
    *callbacks_ptr = info.callbacks;

    /* Copy udata if it exists */
    if(info.callbacks.udata != NULL) {
        HDassert(info.callbacks.udata_copy);
        if((callbacks_ptr->udata = info.callbacks.udata_copy(info.callbacks.udata)) == 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't copy udata")
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_file_image_callbacks() */


/*-------------------------------------------------------------------------
 * Function: H5P_file_image_info_del
 *
 * Purpose:     Delete callback for the file image info property, called
 *              when the property is deleted from the plist. The buffer
 *              and udata may need to be freed, possibly using their 
 *              respective callbacks so the default free won't work.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Jacob Gruber
 *              Thurday, August 11, 2011
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P_file_image_info_del(hid_t UNUSED prop_id, const char UNUSED *name, size_t UNUSED size, void *value)
{
    H5FD_file_image_info_t info;        /* Image info struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(value) {
        info = *(H5FD_file_image_info_t *)value;

        /* verify file image field consistancy */
        HDassert(((info.buffer != NULL) && (info.size > 0)) || 
                 ((info.buffer == NULL) && (info.size == 0)));

        if(info.buffer && info.size > 0) {
            /* Free buffer */
            if(info.callbacks.image_free) {
                if(info.callbacks.image_free(info.buffer, H5FD_FILE_IMAGE_OP_PROPERTY_LIST_CLOSE, info.callbacks.udata) < 0)
		    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, "image_free callback failed")
            } /* end if */
            else
                HDfree(info.buffer);
        } /* end if */

        /* Free udata if it exists */
        if(info.callbacks.udata) {
            if(NULL == info.callbacks.udata_free)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "udata_free not defined")

            if(info.callbacks.udata_free(info.callbacks.udata) < 0)
	        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, "udata_free callback failed")
        } /* end if */
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_file_image_info_del() */


/*-------------------------------------------------------------------------
 * Function: H5P_file_image_info_copy
 *
 * Purpose:     Copy callback for the file image info property. The buffer
 *              and udata may need to be copied, possibly using their 
 *              respective callbacks so the default copy won't work.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Jacob Gruber
 *              Thurday, August 11, 2011
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P_file_image_info_copy(const char UNUSED *name, size_t UNUSED size, void *value)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(value) {
        H5FD_file_image_info_t *info;   /* Image info struct */

        info = (H5FD_file_image_info_t *)value;

        /* verify file image field consistancy */
        HDassert(((info->buffer != NULL) && (info->size > 0)) || 
                 ((info->buffer == NULL) && (info->size == 0)));

        if(info->buffer && info->size > 0) {
            void *old_buffer;            /* Pointer to old image buffer */

            /* Store the old buffer */
            old_buffer = info->buffer;

            /* Allocate new buffer */
            if(info->callbacks.image_malloc) {
                if(NULL == (info->buffer = info->callbacks.image_malloc(info->size,
                        H5FD_FILE_IMAGE_OP_PROPERTY_LIST_COPY, info->callbacks.udata)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "image malloc callback failed")
            } /* end if */
            else {
                if(NULL == (info->buffer = H5MM_malloc(info->size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to allocate memory block")
            } /* end else */
            
            /* Copy data to new buffer */
            if(info->callbacks.image_memcpy) {
                if(info->buffer != info->callbacks.image_memcpy(info->buffer, old_buffer, 
			info->size, H5FD_FILE_IMAGE_OP_PROPERTY_LIST_COPY, 
			info->callbacks.udata))
		    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTCOPY, FAIL, "image_memcpy callback failed")
            } /* end if */
	    else
                HDmemcpy(info->buffer, old_buffer, info->size);
        } /* end if */

        /* Copy udata if it exists */
        if(info->callbacks.udata) {
            void *old_udata = info->callbacks.udata;

            if(NULL == info->callbacks.udata_copy)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "udata_copy not defined")

            info->callbacks.udata = info->callbacks.udata_copy(old_udata);
        } /* end if */

    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_file_image_info_copy() */


/*-------------------------------------------------------------------------
 * Function: H5P_file_image_info_close
 *
 * Purpose:     Close callback for the file image info property. The buffer
 *              and udata may need to be freed, possibly using their 
 *              respective callbacks so the standard free won't work.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Jacob Gruber
 *              Thurday, August 11, 2011
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P_file_image_info_close(const char UNUSED *name, size_t UNUSED size, void *value)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if(value) {
        H5FD_file_image_info_t *info;        /* Image info struct */

        info = (H5FD_file_image_info_t *)value;
             
        if(info->buffer != NULL && info->size > 0) { 
            /* Free buffer */
            if(info->callbacks.image_free) {
                if(info->callbacks.image_free(info->buffer, H5FD_FILE_IMAGE_OP_PROPERTY_LIST_CLOSE,
                        info->callbacks.udata) < 0)
		    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, "image_free callback failed")
            } /* end if */
            else
                H5MM_xfree(info->buffer);
        } /* end if */

        /* Free udata if it exists */
        if(info->callbacks.udata) {
            if(NULL == info->callbacks.udata_free)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "udata_free not defined")
            if(info->callbacks.udata_free(info->callbacks.udata) < 0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, "udata_free callback failed")
        } /* end if */
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_file_image_info_close() */


/*-------------------------------------------------------------------------
 * Function: H5P__facc_cache_config_cmp
 *
 * Purpose: Compare two cache configurations.
 *
 * Return: positive if VALUE1 is greater than VALUE2, negative if VALUE2 is
 *		greater than VALUE1 and zero if VALUE1 and VALUE2 are equal.
 *
 * Programmer:     Mohamad Chaarawi
 *                 September 24, 2012
 *
 *-------------------------------------------------------------------------
 */
static int
H5P__facc_cache_config_cmp(const void *_config1, const void *_config2, size_t UNUSED size)
{
    const H5AC_cache_config_t *config1 = (const H5AC_cache_config_t *)_config1; /* Create local aliases for values */
    const H5AC_cache_config_t *config2 = (const H5AC_cache_config_t *)_config2; /* Create local aliases for values */
    int ret_value = 0;               /* Return value */

    FUNC_ENTER_STATIC_NOERR

    /* Check for a property being set */
    if(config1 == NULL && config2 != NULL) HGOTO_DONE(-1);
    if(config1 != NULL && config2 == NULL) HGOTO_DONE(1);

    if(config1->version < config2->version) HGOTO_DONE(-1);
    if(config1->version > config2->version) HGOTO_DONE(1);

    if(config1->rpt_fcn_enabled < config2->rpt_fcn_enabled) HGOTO_DONE(-1);
    if(config1->rpt_fcn_enabled > config2->rpt_fcn_enabled) HGOTO_DONE(1);

    if(config1->open_trace_file < config2->open_trace_file) HGOTO_DONE(-1);
    if(config1->open_trace_file > config2->open_trace_file) HGOTO_DONE(1);

    if(config1->close_trace_file < config2->close_trace_file) HGOTO_DONE(-1);
    if(config1->close_trace_file > config2->close_trace_file) HGOTO_DONE(1);

    if((ret_value = HDstrncmp(config1->trace_file_name, config2->trace_file_name, 
                              (size_t)(H5AC__MAX_TRACE_FILE_NAME_LEN + 1))) != 0)
        HGOTO_DONE(ret_value);

    if(config1->evictions_enabled < config2->evictions_enabled) HGOTO_DONE(-1);
    if(config1->evictions_enabled > config2->evictions_enabled) HGOTO_DONE(1);

    if(config1->set_initial_size < config2->set_initial_size) HGOTO_DONE(-1);
    if(config1->set_initial_size > config2->set_initial_size) HGOTO_DONE(1);

    if(config1->initial_size < config2->initial_size) HGOTO_DONE(-1);
    if(config1->initial_size > config2->initial_size) HGOTO_DONE(1);

    if(config1->min_clean_fraction < config2->min_clean_fraction) HGOTO_DONE(-1);
    if(config1->min_clean_fraction > config2->min_clean_fraction) HGOTO_DONE(1);

    if(config1->max_size < config2->max_size) HGOTO_DONE(-1);
    if(config1->max_size > config2->max_size) HGOTO_DONE(1);

    if(config1->min_size < config2->min_size) HGOTO_DONE(-1);
    if(config1->min_size > config2->min_size) HGOTO_DONE(1);

    if(config1->epoch_length < config2->epoch_length) HGOTO_DONE(-1);
    if(config1->epoch_length > config2->epoch_length) HGOTO_DONE(1);

    if(config1->incr_mode < config2->incr_mode) HGOTO_DONE(-1);
    if(config1->incr_mode > config2->incr_mode) HGOTO_DONE(1);

    if(config1->lower_hr_threshold < config2->lower_hr_threshold) HGOTO_DONE(-1);
    if(config1->lower_hr_threshold > config2->lower_hr_threshold) HGOTO_DONE(1);

    if(config1->increment < config2->increment) HGOTO_DONE(-1);
    if(config1->increment > config2->increment) HGOTO_DONE(1);

    if(config1->apply_max_increment < config2->apply_max_increment) HGOTO_DONE(-1);
    if(config1->apply_max_increment > config2->apply_max_increment) HGOTO_DONE(1);

    if(config1->max_increment < config2->max_increment) HGOTO_DONE(-1);
    if(config1->max_increment > config2->max_increment) HGOTO_DONE(1);

    if(config1->flash_incr_mode < config2->flash_incr_mode) HGOTO_DONE(-1);
    if(config1->flash_incr_mode > config2->flash_incr_mode) HGOTO_DONE(1);

    if(config1->flash_multiple < config2->flash_multiple) HGOTO_DONE(-1);
    if(config1->flash_multiple > config2->flash_multiple) HGOTO_DONE(1);

    if(config1->flash_threshold < config2->flash_threshold) HGOTO_DONE(-1);
    if(config1->flash_threshold > config2->flash_threshold) HGOTO_DONE(1);

    if(config1->decr_mode < config2->decr_mode) HGOTO_DONE(-1);
    if(config1->decr_mode > config2->decr_mode) HGOTO_DONE(1);

    if(config1->upper_hr_threshold < config2->upper_hr_threshold) HGOTO_DONE(-1);
    if(config1->upper_hr_threshold > config2->upper_hr_threshold) HGOTO_DONE(1);

    if(config1->decrement < config2->decrement) HGOTO_DONE(-1);
    if(config1->decrement > config2->decrement) HGOTO_DONE(1);

    if(config1->apply_max_decrement < config2->apply_max_decrement) HGOTO_DONE(-1);
    if(config1->apply_max_decrement > config2->apply_max_decrement) HGOTO_DONE(1);

    if(config1->max_decrement < config2->max_decrement) HGOTO_DONE(-1);
    if(config1->max_decrement > config2->max_decrement) HGOTO_DONE(1);

    if(config1->epochs_before_eviction < config2->epochs_before_eviction) HGOTO_DONE(-1);
    if(config1->epochs_before_eviction > config2->epochs_before_eviction) HGOTO_DONE(1);

    if(config1->apply_empty_reserve < config2->apply_empty_reserve) HGOTO_DONE(-1);
    if(config1->apply_empty_reserve > config2->apply_empty_reserve) HGOTO_DONE(1);

    if(config1->empty_reserve < config2->empty_reserve) HGOTO_DONE(-1);
    if(config1->empty_reserve > config2->empty_reserve) HGOTO_DONE(1);

    if(config1->dirty_bytes_threshold < config2->dirty_bytes_threshold) HGOTO_DONE(-1);
    if(config1->dirty_bytes_threshold > config2->dirty_bytes_threshold) HGOTO_DONE(1);

    if(config1->metadata_write_strategy < config2->metadata_write_strategy) HGOTO_DONE(-1);
    if(config1->metadata_write_strategy > config2->metadata_write_strategy) HGOTO_DONE(1);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P__facc_cache_config_cmp() */


/*-------------------------------------------------------------------------
 * Function:       H5P__facc_cache_config_enc
 *
 * Purpose:        Callback routine which is called whenever the default
 *                 cache config property in the file creation property list is
 *                 encoded.
 *
 * Return:	   Success:	Non-negative
 *		   Failure:	Negative
 *
 * Programmer:     Mohamad Chaarawi
 *                 August 09, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__facc_cache_config_enc(const void *value, void **_pp, size_t *size)
{
    const H5AC_cache_config_t *config = (const H5AC_cache_config_t *)value; /* Create local aliases for values */
    uint8_t **pp = (uint8_t **)_pp;
    unsigned enc_size;      /* Size of encoded property */
    uint64_t enc_value;         /* Property to encode */

    FUNC_ENTER_STATIC_NOERR

    /* Sanity check */
    HDassert(value);
    HDcompile_assert(sizeof(size_t) <= sizeof(uint64_t));

    if(NULL != *pp) {
        /* Encode type sizes (as a safety check) */
        *(*pp)++ = (uint8_t)sizeof(unsigned);
        *(*pp)++ = (uint8_t)sizeof(double);

        /* int */
        INT32ENCODE(*pp, (int32_t)config->version);

        H5_ENCODE_UNSIGNED(*pp, config->rpt_fcn_enabled);

        H5_ENCODE_UNSIGNED(*pp, config->open_trace_file);

        H5_ENCODE_UNSIGNED(*pp, config->close_trace_file);

        HDmemcpy(*pp, (const uint8_t *)(config->trace_file_name), (size_t)(H5AC__MAX_TRACE_FILE_NAME_LEN + 1));
        *pp += H5AC__MAX_TRACE_FILE_NAME_LEN + 1;

        H5_ENCODE_UNSIGNED(*pp, config->evictions_enabled);

        H5_ENCODE_UNSIGNED(*pp, config->set_initial_size);

        enc_value = (uint64_t)config->initial_size;
        enc_size = H5V_limit_enc_size(enc_value);
        HDassert(enc_size < 256);
        *(*pp)++ = (uint8_t)enc_size;
        UINT64ENCODE_VAR(*pp, enc_value, enc_size);

        H5_ENCODE_DOUBLE(*pp, config->min_clean_fraction);

        enc_value = (uint64_t)config->max_size;
        enc_size = H5V_limit_enc_size(enc_value);
        HDassert(enc_size < 256);
        *(*pp)++ = (uint8_t)enc_size;
        UINT64ENCODE_VAR(*pp, enc_value, enc_size);

        enc_value = (uint64_t)config->min_size;
        enc_size = H5V_limit_enc_size(enc_value);
        HDassert(enc_size < 256);
        *(*pp)++ = (uint8_t)enc_size;
        UINT64ENCODE_VAR(*pp, enc_value, enc_size);

        /* long int */
        INT64ENCODE(*pp, (int64_t)config->epoch_length);

        /* enum */
        *(*pp)++ = (uint8_t)config->incr_mode;

        H5_ENCODE_DOUBLE(*pp, config->lower_hr_threshold);

        H5_ENCODE_DOUBLE(*pp, config->increment);

        H5_ENCODE_UNSIGNED(*pp, config->apply_max_increment);

        enc_value = (uint64_t)config->max_increment;
        enc_size = H5V_limit_enc_size(enc_value);
        HDassert(enc_size < 256);
        *(*pp)++ = (uint8_t)enc_size;
        UINT64ENCODE_VAR(*pp, enc_value, enc_size);

        /* enum */
        *(*pp)++ = (uint8_t)config->flash_incr_mode;

        H5_ENCODE_DOUBLE(*pp, config->flash_multiple);

        H5_ENCODE_DOUBLE(*pp, config->flash_threshold);

        /* enum */
        *(*pp)++ = (uint8_t)config->decr_mode;

        H5_ENCODE_DOUBLE(*pp, config->upper_hr_threshold);

        H5_ENCODE_DOUBLE(*pp, config->decrement);

        H5_ENCODE_UNSIGNED(*pp, config->apply_max_decrement);

        enc_value = (uint64_t)config->max_decrement;
        enc_size = H5V_limit_enc_size(enc_value);
        HDassert(enc_size < 256);
        *(*pp)++ = (uint8_t)enc_size;
        UINT64ENCODE_VAR(*pp, enc_value, enc_size);

        /* int */
        INT32ENCODE(*pp, (int32_t)config->epochs_before_eviction);

        H5_ENCODE_UNSIGNED(*pp, config->apply_empty_reserve);

        H5_ENCODE_DOUBLE(*pp, config->empty_reserve);

        /* int */
        INT32ENCODE(*pp, (int32_t)config->dirty_bytes_threshold);

        /* int */
        INT32ENCODE(*pp, (int32_t)config->metadata_write_strategy);
    } /* end if */

    /* Compute encoded size of variably-encoded values */
    enc_value = (uint64_t)config->initial_size;
    *size += 1 + H5V_limit_enc_size(enc_value);
    enc_value = (uint64_t)config->max_size;
    *size += 1 + H5V_limit_enc_size(enc_value);
    enc_value = (uint64_t)config->min_size;
    *size += 1 + H5V_limit_enc_size(enc_value);
    enc_value = (uint64_t)config->max_increment;
    *size += 1 + H5V_limit_enc_size(enc_value);
    enc_value = (uint64_t)config->max_decrement;
    *size += 1 + H5V_limit_enc_size(enc_value);

    /* Compute encoded size of fixed-size values */
    *size += (5 + (sizeof(unsigned) * 8) + (sizeof(double) * 8) +
            (sizeof(int32_t) * 4) + sizeof(int64_t) +
            H5AC__MAX_TRACE_FILE_NAME_LEN + 1);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5P__facc_cache_config_enc() */


/*-------------------------------------------------------------------------
 * Function:       H5P__facc_cache_config_dec
 *
 * Purpose:        Callback routine which is called whenever the default
 *                 cache config property in the file creation property list is
 *                 decoded.
 *
 * Return:	   Success:	Non-negative
 *		   Failure:	Negative
 *
 * Programmer:     Mohamad Chaarawi
 *                 August 09, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__facc_cache_config_dec(const void **_pp, void *_value)
{
    H5AC_cache_config_t *config = (H5AC_cache_config_t *)_value;
    const uint8_t **pp = (const uint8_t **)_pp;
    unsigned enc_size;
    uint64_t enc_value;
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(pp);
    HDassert(*pp);
    HDassert(config);
    HDcompile_assert(sizeof(size_t) <= sizeof(uint64_t));

    /* Set property to default value */
    HDmemcpy(config, &H5F_def_mdc_initCacheCfg_g, sizeof(H5AC_cache_config_t));

    /* Decode type sizes */
    enc_size = *(*pp)++;
    if(enc_size != sizeof(unsigned))
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "unsigned value can't be decoded")
    enc_size = *(*pp)++;
    if(enc_size != sizeof(double))
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "double value can't be decoded")

    /* int */
    INT32DECODE(*pp, config->version);

    H5_DECODE_UNSIGNED(*pp, config->rpt_fcn_enabled);

    H5_DECODE_UNSIGNED(*pp, config->open_trace_file);

    H5_DECODE_UNSIGNED(*pp, config->close_trace_file);

    HDstrcpy(config->trace_file_name, (const char *)(*pp));
    *pp += H5AC__MAX_TRACE_FILE_NAME_LEN + 1;

    H5_DECODE_UNSIGNED(*pp, config->evictions_enabled);

    H5_DECODE_UNSIGNED(*pp, config->set_initial_size);

    enc_size = *(*pp)++;
    HDassert(enc_size < 256);
    UINT64DECODE_VAR(*pp, enc_value, enc_size);
    config->initial_size = (size_t)enc_value;

    H5_DECODE_DOUBLE(*pp, config->min_clean_fraction);

    enc_size = *(*pp)++;
    HDassert(enc_size < 256);
    UINT64DECODE_VAR(*pp, enc_value, enc_size);
    config->max_size = (size_t)enc_value;

    enc_size = *(*pp)++;
    HDassert(enc_size < 256);
    UINT64DECODE_VAR(*pp, enc_value, enc_size);
    config->min_size = (size_t)enc_value;

    /* long int */
    {
        int64_t temp;
        INT64DECODE(*pp, temp);
        config->epoch_length = (long int)temp;
    }
    /* enum */
    config->incr_mode = (enum H5C_cache_incr_mode)*(*pp)++;

    H5_DECODE_DOUBLE(*pp, config->lower_hr_threshold);

    H5_DECODE_DOUBLE(*pp, config->increment);

    H5_DECODE_UNSIGNED(*pp, config->apply_max_increment);

    enc_size = *(*pp)++;
    HDassert(enc_size < 256);
    UINT64DECODE_VAR(*pp, enc_value, enc_size);
    config->max_increment = (size_t)enc_value;

    /* enum */
    config->flash_incr_mode = (enum H5C_cache_flash_incr_mode)*(*pp)++;

    H5_DECODE_DOUBLE(*pp, config->flash_multiple);

    H5_DECODE_DOUBLE(*pp, config->flash_threshold);

    /* enum */
    config->decr_mode = (enum H5C_cache_decr_mode)*(*pp)++;

    H5_DECODE_DOUBLE(*pp, config->upper_hr_threshold);

    H5_DECODE_DOUBLE(*pp, config->decrement);

    H5_DECODE_UNSIGNED(*pp, config->apply_max_decrement);

    enc_size = *(*pp)++;
    HDassert(enc_size < 256);
    UINT64DECODE_VAR(*pp, enc_value, enc_size);
    config->max_decrement = (size_t)enc_value;

    /* int */
    INT32DECODE(*pp, config->epochs_before_eviction);

    H5_DECODE_UNSIGNED(*pp, config->apply_empty_reserve);

    H5_DECODE_DOUBLE(*pp, config->empty_reserve);

    /* int */
    INT32DECODE(*pp, config->dirty_bytes_threshold);

    /* int */
    INT32DECODE(*pp, config->metadata_write_strategy);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P__facc_cache_config_dec() */


/*-------------------------------------------------------------------------
 * Function:       H5P__facc_fclose_degree_enc
 *
 * Purpose:        Callback routine which is called whenever the file close
 *                 degree property in the file access property list
 *                 is encoded.
 *
 * Return:	   Success:	Non-negative
 *		   Failure:	Negative
 *
 * Programmer:     Quincey Koziol
 *                 Wednesday, August 15, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__facc_fclose_degree_enc(const void *value, void **_pp, size_t *size)
{
    const H5F_close_degree_t *fclose_degree = (const H5F_close_degree_t *)value; /* Create local alias for values */
    uint8_t **pp = (uint8_t **)_pp;

    FUNC_ENTER_STATIC_NOERR

    /* Sanity check */
    HDassert(fclose_degree);
    HDassert(size);

    if(NULL != *pp)
        /* Encode file close degree */
        *(*pp)++ = (uint8_t)*fclose_degree;

    /* Size of file close degree */
    (*size)++;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5P__facc_fclose_degree_enc() */


/*-------------------------------------------------------------------------
 * Function:       H5P__facc_fclose_degree_dec
 *
 * Purpose:        Callback routine which is called whenever the file close
 *                 degree property in the file access property list
 *                 is decoded.
 *
 * Return:	   Success:	Non-negative
 *		   Failure:	Negative
 *
 * Programmer:     Quincey Koziol
 *                 Wednesday, August 15, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__facc_fclose_degree_dec(const void **_pp, void *_value)
{
    H5F_close_degree_t *fclose_degree = (H5F_close_degree_t *)_value;            /* File close degree */
    const uint8_t **pp = (const uint8_t **)_pp;

    FUNC_ENTER_STATIC_NOERR

    /* Sanity checks */
    HDassert(pp);
    HDassert(*pp);
    HDassert(fclose_degree);

    /* Decode file close degree */
    *fclose_degree = (H5F_close_degree_t)*(*pp)++;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5P__facc_fclose_degree_dec() */


/*-------------------------------------------------------------------------
 * Function:       H5P__facc_multi_type_enc
 *
 * Purpose:        Callback routine which is called whenever the multi VFD
 *                 memory type property in the file access property list
 *                 is encoded.
 *
 * Return:	   Success:	Non-negative
 *		   Failure:	Negative
 *
 * Programmer:     Quincey Koziol
 *                 Wednesday, August 15, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__facc_multi_type_enc(const void *value, void **_pp, size_t *size)
{
    const H5FD_mem_t *type = (const H5FD_mem_t *)value; /* Create local alias for values */
    uint8_t **pp = (uint8_t **)_pp;

    FUNC_ENTER_STATIC_NOERR

    /* Sanity check */
    HDassert(type);
    HDassert(size);

    if(NULL != *pp)
        /* Encode file close degree */
        *(*pp)++ = (uint8_t)*type;

    /* Size of multi VFD memory type */
    (*size)++;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5P__facc_multi_type_enc() */


/*-------------------------------------------------------------------------
 * Function:       H5P__facc_multi_type_dec
 *
 * Purpose:        Callback routine which is called whenever the multi VFD
 *                 memory type property in the file access property list
 *                 is decoded.
 *
 * Return:	   Success:	Non-negative
 *		   Failure:	Negative
 *
 * Programmer:     Quincey Koziol
 *                 Wednesday, August 15, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__facc_multi_type_dec(const void **_pp, void *_value)
{
    H5FD_mem_t *type = (H5FD_mem_t *)_value;            /* File close degree */
    const uint8_t **pp = (const uint8_t **)_pp;

    FUNC_ENTER_STATIC_NOERR

    /* Sanity checks */
    HDassert(pp);
    HDassert(*pp);
    HDassert(type);

    /* Decode multi VFD memory type */
    *type = (H5FD_mem_t)*(*pp)++;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5P__facc_multi_type_dec() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_metadata_read_attempts
 *
 * Purpose:	Sets the # of read attempts in the file access property list
 *		when reading metadata with checksum.
 *		The # of read attempts set via this routine will only apply
 *		when opening a file with SWMR access. 
 *		The # of read attempts set via this routine does not have 
 *		any effect when opening a file with non-SWMR access; for this
 *		case, the # of read attempts will be always be 1.
 *	
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Vailin Choi; Sept 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_metadata_read_attempts(hid_t plist_id, unsigned attempts)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED;   /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iIu", plist_id, attempts);

    /* Cannot set the # of attempts to 0 */
    if(attempts == 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "number of metadatata read attempts must be greater than 0");

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set values */
    if(H5P_set(plist, H5F_ACS_METADATA_READ_ATTEMPTS_NAME, &attempts) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set # of metadata read attempts")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Pset_metadata_read_attempts() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_metadata_read_attempts
 *
 * Purpose:	Returns the # of metadata read attempts set in the file access property list. 
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Vailin Choi; Sept 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_metadata_read_attempts(hid_t plist_id, unsigned *attempts/*out*/)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ix", plist_id, attempts);

    /* Get values */
    if(attempts) {
        H5P_genplist_t *plist;              /* Property list pointer */

        /* Get the plist structure */
        if(NULL == (plist = H5P_object_verify(plist_id, H5P_FILE_ACCESS)))
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

	/* Get the # of read attempts set */
        if(H5P_get(plist, H5F_ACS_METADATA_READ_ATTEMPTS_NAME, attempts) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get the number of metadata read attempts")

	/* If not set, return the default value */
	if(*attempts == H5F_ACS_METADATA_READ_ATTEMPTS_DEF)	/* 0 */
	    *attempts = H5F_METADATA_READ_ATTEMPTS;
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_metadata_read_attempts() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_obj_flush_cb
 *
 * Purpose:	Sets the callback function to invoke and the user data when an
 *		object flush occurs in the file.
 *	
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Vailin Choi; Dec 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_object_flush_cb(hid_t plist_id, H5F_flush_cb_t func, void *udata)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    H5F_object_flush_t flush_info;
    herr_t ret_value = SUCCEED;   /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "ix*x", plist_id, func, udata);

    /* Check if the callback function is NULL and the user data is non-NULL.
     * This is almost certainly an error as the user data will not be used. */
    if(!func && udata)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "callback is NULL while user data is not")

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Update property list */
    flush_info.func = func;
    flush_info.udata = udata;

    /* Set values */
    if(H5P_set(plist, H5F_ACS_OBJECT_FLUSH_CB_NAME, &flush_info) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set object flush callback")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Pset_obj_flush_cb() */

/*-------------------------------------------------------------------------
 * Function:	H5Pget_obj_flush_cb
 *
 * Purpose:	Retrieves the callback function and user data set in the
 *		property list for an object flush.
 *	
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Vailin Choi; Dec 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_object_flush_cb(hid_t plist_id, H5F_flush_cb_t *func, void **udata)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    H5F_object_flush_t flush_info;
    herr_t ret_value = SUCCEED;   /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "i*x**x", plist_id, func, udata);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Retrieve the callback function and user data */
    if(H5P_get(plist, H5F_ACS_OBJECT_FLUSH_CB_NAME, &flush_info) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get object flush callback")

    /* Assign return value */
    if(func)
	*func = flush_info.func;
    if(udata)
	*udata = flush_info.udata;

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Pget_obj_flush_cb() */
