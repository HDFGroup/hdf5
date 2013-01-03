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

/* Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.org>
 *
 * Purpose:	Metadata server code
 */

/****************/
/* Module Setup */
/****************/


#define H5A_PACKAGE		/*suppress error about including H5Apkg	  */
#define H5D_PACKAGE		/*suppress error about including H5Dpkg	  */
#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */
#define H5G_PACKAGE		/*suppress error about including H5Gpkg   */
#define H5P_PACKAGE		/*suppress error about including H5Ppkg	  */
#define H5T_PACKAGE		/*suppress error about including H5Tpkg	  */

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Apkg.h"             /* Attribute pkg                        */
#include "H5Aprivate.h"		/* Attributes				*/
#include "H5Dpkg.h"             /* Dataset pkg                          */
#include "H5Dprivate.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"		/* File access				*/
#include "H5Fpkg.h"             /* File pkg                             */
#include "H5FDmpi.h"            /* MPI-based file drivers		*/
#include "H5FDmulti.h"          /* MULTI-based file drivers		*/
#include "H5FDmds.h"            /* MDS file driver      		*/
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5Gprivate.h"		/* Groups		  		*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MDprivate.h"        /* MDS operations			*/
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5Ppkg.h"		/* Property lists		  	*/
#include "H5Sprivate.h" 	/* Dataspaces                      	*/
#include "H5Tpkg.h"		/* Datatypes				*/
#include "H5Tprivate.h"		/* Datatypes				*/
#include "H5VLprivate.h"	/* VOL plugins				*/
#include "H5VLnative.h"         /* Native VOL plugin			*/

#ifdef H5_HAVE_PARALLEL

/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/


/********************/
/* Local Prototypes */
/********************/
static herr_t H5MD__file_create_func(uint8_t *p, int source);
static herr_t H5MD__file_open_func(uint8_t *p, int source);
static herr_t H5MD__file_flush_func(uint8_t *p, int source);
static herr_t H5MD__file_misc_func(uint8_t *p, int source);
static herr_t H5MD__file_optional_func(uint8_t *p, int source);
static herr_t H5MD__file_close_func(uint8_t *p, int source);
static herr_t H5MD__attr_create_func(uint8_t *p, int source);
static herr_t H5MD__attr_open_func(uint8_t *p, int source);
static herr_t H5MD__attr_read_func(uint8_t *p, int source);
static herr_t H5MD__attr_write_func(uint8_t *p, int source);
static herr_t H5MD__attr_remove_func(uint8_t *p, int source);
static herr_t H5MD__attr_get_func(uint8_t *p, int source);
static herr_t H5MD__attr_close_func(uint8_t *p, int source);
static herr_t H5MD__chunk_insert(uint8_t *p, int source);
static herr_t H5MD__chunk_get_addr(uint8_t *p, int source);
static herr_t H5MD__chunk_iterate(uint8_t *p, int source);
static herr_t H5MD__dataset_create_func(uint8_t *p, int source);
static herr_t H5MD__dataset_open_func(uint8_t *p, int source);
static herr_t H5MD__dataset_set_extent_func(uint8_t *p, int source);
static herr_t H5MD__dataset_refresh_func(uint8_t *p, int source);
static herr_t H5MD__dataset_close_func(uint8_t *p, int source);
static herr_t H5MD__datatype_commit_func(uint8_t *p, int source);
static herr_t H5MD__datatype_open_func(uint8_t *p, int source);
static herr_t H5MD__datatype_close_func(uint8_t *p, int source);
static herr_t H5MD__group_create_func(uint8_t *p, int source);
static herr_t H5MD__group_open_func(uint8_t *p, int source);
static herr_t H5MD__group_get_func(uint8_t *p, int source);
static herr_t H5MD__group_close_func(uint8_t *p, int source);
static herr_t H5MD__link_create_func(uint8_t *p, int source);
static herr_t H5MD__link_move_func(uint8_t *p, int source);
static herr_t H5MD__link_iterate_func(uint8_t *p, int source);
static herr_t H5MD__link_get_func(uint8_t *p, int source);
static herr_t H5MD__link_remove_func(uint8_t *p, int source);
static herr_t H5MD__object_open_func(uint8_t *p, int source);
static herr_t H5MD__object_copy_func(uint8_t *p, int source);
static herr_t H5MD__object_visit_func(uint8_t *p, int source);
static herr_t H5MD__object_misc_func(uint8_t *p, int source);
static herr_t H5MD__object_get_func(uint8_t *p, int source);
static herr_t H5MD__allocate_func(uint8_t *p, int source);
static herr_t H5MD__set_eoa_func(uint8_t *p, int source);
static herr_t H5MD__get_eoa_func(uint8_t *p, int source);

typedef herr_t (*H5VL_mds_op)(uint8_t *p, int source);

static herr_t H5MD__temp_attr_get(void *obj, H5VL_attr_get_t get_type, hid_t req, ...);
static herr_t H5MD__temp_group_get(void *obj, H5VL_group_get_t get_type, hid_t req, ...);
static herr_t H5MD__temp_link_get(void *obj, H5VL_loc_params_t loc_params, H5VL_link_get_t get_type, hid_t req, ...);
static herr_t H5MD__temp_file_misc(void *obj, H5VL_file_misc_t misc_type, hid_t req, ...);
static herr_t H5MD__temp_file_optional(void *obj, H5VL_file_optional_t optional_type, hid_t req, ...);
static herr_t H5MD__temp_object_misc(void *obj, H5VL_loc_params_t loc_params, H5VL_object_misc_t misc_type, hid_t req, ...);
static herr_t H5MD__temp_object_get(void *obj, H5VL_loc_params_t loc_params, H5VL_object_get_t get_type, hid_t req, ...);
static herr_t H5MD_multi_query(const H5FD_t *_f, unsigned long *flags /* out */);

static H5VL_mds_op mds_ops[H5VL_NUM_OPS] = {
    H5MD__file_create_func,
    H5MD__file_open_func,
    H5MD__file_flush_func,
    H5MD__file_misc_func,
    H5MD__file_optional_func,
    H5MD__file_close_func,
    H5MD__attr_create_func,
    H5MD__attr_open_func,
    H5MD__attr_read_func,
    H5MD__attr_write_func,
    H5MD__attr_remove_func,
    H5MD__attr_get_func,
    H5MD__attr_close_func,
    H5MD__chunk_insert,
    H5MD__chunk_get_addr,
    H5MD__chunk_iterate,
    H5MD__dataset_create_func,
    H5MD__dataset_open_func,
    H5MD__dataset_set_extent_func,
    H5MD__dataset_refresh_func,
    H5MD__dataset_close_func,
    H5MD__datatype_commit_func,
    H5MD__datatype_open_func,
    H5MD__datatype_close_func,
    H5MD__group_create_func,
    H5MD__group_open_func,
    H5MD__group_get_func,
    H5MD__group_close_func,
    H5MD__link_create_func,
    H5MD__link_move_func,
    H5MD__link_iterate_func,
    H5MD__link_get_func,
    H5MD__link_remove_func,
    H5MD__object_open_func,
    H5MD__object_copy_func,
    H5MD__object_visit_func,
    H5MD__object_misc_func,
    H5MD__object_get_func,
    H5MD__allocate_func,
    H5MD__get_eoa_func,
    H5MD__set_eoa_func
};

/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/


/*-------------------------------------------------------------------------
 * Function:	H5MD_server_terminate_cb
 *
 * Purpose:	Callback routine that is registered with MPI_COMM_SELF that sends 
 *              a request to the MDS server to indicate that this process called
 *              MPI_Finalize and is quitting.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              November, 2012
 *
 *-------------------------------------------------------------------------
 */
int 
H5MD_server_terminate_cb(MPI_Comm UNUSED comm, int UNUSED comm_keyval, void UNUSED *attribute_val, 
                         void UNUSED *extra_state)
{
    H5VL_op_type_t req = H5VL_DONE;
    int ret_value = MPI_SUCCESS;

    FUNC_ENTER_NOAPI_NOINIT

    MPI_Pcontrol(0);
    /* send a message to the MDS process indicating that we called MPI_Finalize() */
    if(MPI_SUCCESS != (ret_value = MPI_Send(&req, sizeof(int), MPI_BYTE, MDS_RANK, H5MD_LISTEN_TAG,
                                            MPI_COMM_WORLD)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    MPI_Pcontrol(1);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5MD_start
 *
 * Purpose:	This is the API routine that the MDS process calls to start 
 *              looping and accept requests from clients.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              August, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MD_start(void)
{
    int counter = 0;
    int num_procs;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* call the group interface intialization, because it hasn't been called yet */
    if(H5G__init() < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to initialize group interface");
    /* call the attribute interface intialization, because it hasn't been called yet */
    if(H5A_init() < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to initialize attribute interface");

    /* turn off commsplitter to talk to the other processes */
    MPI_Pcontrol(0);

    /* get the number of processes in world */
    MPI_Comm_size(MPI_COMM_WORLD, &num_procs);

    /* loop accepting requests from clients */
    while(1) {
        MPI_Status status;
        int incoming_msg_size;
        void *recv_buf = NULL;
        uint8_t *p;     /* Current pointer into buffer */
        H5VL_op_type_t op_type;

        /* probe for a message from a client */
        if(MPI_SUCCESS != MPI_Probe(MPI_ANY_SOURCE, H5MD_LISTEN_TAG, MPI_COMM_WORLD, &status))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to probe for a message");
        /* get the incoming message size from the probe result */
        if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to get incoming message size");

        /* allocate the receive buffer */
        if(NULL == (recv_buf = H5MM_malloc((size_t)incoming_msg_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

        /* receive the actual message */
        if(MPI_SUCCESS != MPI_Recv (recv_buf, incoming_msg_size, MPI_BYTE, status.MPI_SOURCE, 
                                    H5MD_LISTEN_TAG, MPI_COMM_WORLD, &status))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to receivemessage");

        p = (uint8_t *)recv_buf;
        op_type = (H5VL_op_type_t)*p++;

        /* if the operation is actually a finished code from the client, then increment the 
           clients_done counter. Then check if all clients are done, exit if so */
        if(H5VL_DONE == op_type) {
            counter ++;
            if(counter == num_procs-1) {
                H5MM_xfree(recv_buf);
                H5_term_library();
                MPI_Finalize();
                break;
            }
        }
        else if((*mds_ops[op_type])(p, status.MPI_SOURCE) < 0) {
            printf("failed mds op: %d\n", op_type);
        }
        H5MM_xfree(recv_buf);
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VLmds_start() */

/*-------------------------------------------------------------------------
 * Function:	H5MD__file_create_func
 *------------------------------------------------------------------------- */
static herr_t
H5MD__file_create_func(uint8_t *p, int source)
{
    char *mds_filename = NULL; /* name of the metadata file (generated from name) */
    unsigned flags; /* access flags */
    hid_t fcpl_id = FAIL, fapl_id = FAIL; /* plist IDs */
    hid_t temp_fapl; /* stub fapl for raw data file that maintains EOA */
    hid_t split_fapl = FAIL; /* split fapl for container */
    H5F_t *new_file = NULL; /* struct for MDS file */
    hid_t file_id = FAIL; /* ID of MDS file */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode the file creation parameters */
    if(H5VL__decode_file_create_params(p, &mds_filename, &flags, &fcpl_id, &fapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode file create params");

    /* Create a split fapl with the decoded fapl used for the metadata file access and
       a stub fapl (temp_fapl) used for raw data file to manage the EOA */
    split_fapl = H5Pcreate(H5P_FILE_ACCESS);
    temp_fapl = H5Pcreate(H5P_FILE_ACCESS);
    if(H5P_set_fapl_mds(temp_fapl) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "failed to set MDS plist");

    /* set up the split multi VFD info */
    {
        H5FD_mem_t	memb_map[H5FD_MEM_NTYPES];
        hid_t		memb_fapl[H5FD_MEM_NTYPES];
        const char	*memb_name[H5FD_MEM_NTYPES];
        haddr_t		memb_addr[H5FD_MEM_NTYPES];
        H5FD_mem_t      mt;

        /* Initialize */
        for (mt=H5FD_MEM_DEFAULT; mt<H5FD_MEM_NTYPES; mt=(H5FD_mem_t)(mt+1)) {
            /* Treat global heap as raw data, not metadata */
            memb_map[mt] = ((mt == H5FD_MEM_DRAW || mt == H5FD_MEM_GHEAP) ? H5FD_MEM_DRAW : H5FD_MEM_SUPER);
            memb_fapl[mt] = -1;
            memb_name[mt] = NULL;
            memb_addr[mt] = HADDR_UNDEF;
        }

        /* The file access properties */
        memb_fapl[H5FD_MEM_SUPER] = fapl_id;
        memb_fapl[H5FD_MEM_DRAW] = temp_fapl;

        /* file names */
        memb_name[H5FD_MEM_SUPER] = mds_filename;
        memb_name[H5FD_MEM_DRAW] = "who cares";

        /* The sizes */
        memb_addr[H5FD_MEM_SUPER] = 0;
        memb_addr[H5FD_MEM_DRAW] = HADDR_MAX/2;
        H5Pset_fapl_multi(split_fapl, memb_map, memb_fapl, memb_name, memb_addr, TRUE);
    }

    /* reset the feature flags for the split file driver, because the default ones will not work with
       the MDS plugin */
    {
        H5FD_class_t	*driver;                /* VFD for file */
        hid_t               driver_id = -1;         /* VFD ID */
        H5P_genplist_t      *plist;                 /* Property list pointer */

        /* Get file access property list */
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(split_fapl)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list");
        /* Get the VFD to open the file with */
        if(H5P_get(plist, H5F_ACS_FILE_DRV_ID_NAME, &driver_id) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get driver ID");
        /* Get driver info */
        if(NULL == (driver = (H5FD_class_t *)H5I_object(driver_id)))
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "invalid driver ID in file access property list");
        driver->query = &H5MD_multi_query;
    }

    /* call the native plugin file create callback*/
    if(NULL == (new_file = (H5F_t *)H5VL_native_file_create(mds_filename, flags, fcpl_id, split_fapl, -1)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to create file");

    /* register atom for file */
    if((file_id = H5VL_native_register(H5I_FILE, new_file, FALSE)) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to create file");

done:
    if(SUCCEED == ret_value) {
        /* Send the meta data file ID to the client */
        if(MPI_SUCCESS != MPI_Send(&file_id, sizeof(hid_t), MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }
    else {
        /* send a failed message to the client */
        if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }
    if(H5I_dec_ref(split_fapl) < 0)
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close");
    if(H5I_dec_ref(temp_fapl) < 0)
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close");
    H5MM_xfree(mds_filename);

    FUNC_LEAVE_NOAPI(ret_value)
}/* H5MD__file_create_func */

/*-------------------------------------------------------------------------
 * Function:	H5MD__file_open_func
 *------------------------------------------------------------------------- */
static herr_t
H5MD__file_open_func(uint8_t *p, int source)
{
    char *mds_filename = NULL; /* name of the metadata file (generated from name) */
    unsigned flags; /* access flags */
    hid_t fapl_id = FAIL; /* plist IDs */
    hid_t temp_fapl; /* stub fapl for raw data file that maintains EOA */
    hid_t split_fapl = FAIL; /* split fapl for container */
    H5F_t *new_file = NULL; /* struct for MDS file */
    hid_t file_id; /* ID of MDS file */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode the file open parameters */
    if(H5VL__decode_file_open_params(p, &mds_filename, &flags, &fapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode file open params");

    /* Create a split fapl with the decoded fapl used for the metadata file access and
       a stub fapl (temp_fapl) used for raw data file to manage the EOA */
    split_fapl = H5Pcreate(H5P_FILE_ACCESS);
    temp_fapl = H5Pcreate(H5P_FILE_ACCESS);
    if(H5P_set_fapl_mds(temp_fapl) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "failed to set MDS plist");

    /* set up the split multi VFD info */
    {
        H5FD_mem_t	memb_map[H5FD_MEM_NTYPES];
        hid_t		memb_fapl[H5FD_MEM_NTYPES];
        const char	*memb_name[H5FD_MEM_NTYPES];
        haddr_t		memb_addr[H5FD_MEM_NTYPES];
        H5FD_mem_t      mt;

        /* Initialize */
        for (mt=H5FD_MEM_DEFAULT; mt<H5FD_MEM_NTYPES; mt=(H5FD_mem_t)(mt+1)) {
            /* Treat global heap as raw data, not metadata */
            memb_map[mt] = ((mt == H5FD_MEM_DRAW || mt == H5FD_MEM_GHEAP) ? H5FD_MEM_DRAW : H5FD_MEM_SUPER);
            //memb_map[mt] = ((mt == H5FD_MEM_DRAW) ? H5FD_MEM_DRAW : H5FD_MEM_SUPER);
            memb_fapl[mt] = -1;
            memb_name[mt] = NULL;
            memb_addr[mt] = HADDR_UNDEF;
        }

        /* The file access properties */
        memb_fapl[H5FD_MEM_SUPER] = fapl_id;
        memb_fapl[H5FD_MEM_DRAW] = temp_fapl;

        /* file names */
        memb_name[H5FD_MEM_SUPER] = mds_filename;
        memb_name[H5FD_MEM_DRAW] = "who cares";

        /* The sizes */
        memb_addr[H5FD_MEM_SUPER] = 0;
        memb_addr[H5FD_MEM_DRAW] = HADDR_MAX/2;
        H5Pset_fapl_multi(split_fapl, memb_map, memb_fapl, memb_name, memb_addr, TRUE);
    }

    /* reset the feature flags for the split file driver, because the default ones will not work with
       the MDS plugin */
    {
        H5FD_class_t	*driver;                /* VFD for file */
        hid_t               driver_id = -1;         /* VFD ID */
        H5P_genplist_t      *plist;                 /* Property list pointer */

        /* Get file access property list */
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(split_fapl)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list");
        /* Get the VFD to open the file with */
        if(H5P_get(plist, H5F_ACS_FILE_DRV_ID_NAME, &driver_id) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get driver ID");
        /* Get driver info */
        if(NULL == (driver = (H5FD_class_t *)H5I_object(driver_id)))
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "invalid driver ID in file access property list");
        driver->query = &H5MD_multi_query;
    }

    /* call the native plugin file open callback*/
    if(NULL == (new_file = (H5F_t *)H5VL_native_file_open(mds_filename, flags, split_fapl, -1)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to create file");

    /* register atom for file */
    if((file_id = H5VL_native_register(H5I_FILE, new_file, FALSE)) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to create file");

done:
    if(SUCCEED == ret_value) {
        /* Send the meta data file ID to the client */
        if(MPI_SUCCESS != MPI_Send(&file_id, sizeof(hid_t), MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }
    else {
        /* send a failed message to the client */
        if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }
    if(H5I_dec_ref(split_fapl) < 0)
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close");
    if(H5I_dec_ref(temp_fapl) < 0)
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close");
    H5MM_xfree(mds_filename);

    FUNC_LEAVE_NOAPI(ret_value)
}/* H5MD__file_open_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__file_flush_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__file_flush_func(uint8_t *p, int source)
{
    hid_t obj_id; /* metadata object ID */
    H5F_scope_t scope;
    H5VL_loc_params_t loc_params;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5VL__decode_file_flush_params(p, &obj_id, &loc_params, &scope) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode file flush params");

    /* call the native plugin file create callback*/
    if(H5VL_native_file_flush(H5I_object(obj_id), loc_params, scope, -1) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to create file");

done:
    /* send status to client */
    if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                               H5MD_RETURN_TAG, MPI_COMM_WORLD))
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

    if(H5VL__close_loc_params(loc_params) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MD__file_flush_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__file_misc_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__file_misc_func(uint8_t *p, int source)
{
    H5VL_file_misc_t misc_type = -1;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    misc_type = (H5VL_file_misc_t)*p++;

    switch(misc_type) {
        case H5VL_FILE_MOUNT:
            {
                hid_t obj_id, child_id;
                char *name = NULL;
                H5I_type_t type;
                hid_t plist_id;

                /* decode params */
                if(H5VL__decode_file_misc_params(p, misc_type, &obj_id, &type, &name, &child_id, 
                                                 &plist_id) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode attr get params");

                /* get value through the native plugin */
                ret_value = H5MD__temp_file_misc(H5I_object(obj_id), misc_type, H5_REQUEST_NULL, type, name,
                                           H5I_object(child_id), plist_id);

                if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                if(plist_id != H5P_FILE_MOUNT_DEFAULT && H5I_dec_ref(plist_id) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close plist");
                H5MM_xfree(name);
                break;
            }
        case H5VL_FILE_UNMOUNT:
            {
                hid_t obj_id;
                H5I_type_t type;
                char *name = NULL;

                /* decode params */
                if(H5VL__decode_file_misc_params(p, misc_type, &obj_id, &type, &name) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode attr get params");

                /* get value through the native plugin */
                ret_value = H5MD__temp_file_misc(H5I_object(obj_id), misc_type, H5_REQUEST_NULL, 
                                                 type, name);

                if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                H5MM_xfree(name);
                break;
            }
        case H5VL_FILE_IS_ACCESSIBLE:
            {
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't perform this type of operation on file");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MD__file_misc_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__file_optional_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__file_optional_func(uint8_t *p, int source)
{
    H5VL_file_optional_t optional_type = -1;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    optional_type = (H5VL_file_optional_t)*p++;

    switch(optional_type) {
        case H5VL_FILE_GET_FREE_SPACE:
            {
                hid_t obj_id;
                hssize_t ret = -1;

                INT32DECODE(p, obj_id);

                /* get value through the native plugin */
                ret_value = H5MD__temp_file_optional(H5I_object(obj_id), optional_type, 
                                                     H5_REQUEST_NULL, &ret);

                if(MPI_SUCCESS != MPI_Send(&ret, sizeof(int64_t), MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                if(ret < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to check free space for file");
                break;
            }
        case H5VL_FILE_GET_FREE_SECTIONS:
            {
                hid_t obj_id;
                H5F_mem_t type;
                size_t nsects;
                uint8_t flag;
                H5F_sect_info_t *sect_info = NULL;
                ssize_t ret;
                void *send_buf = NULL;
                size_t buf_size = 0;
                uint8_t *p1 = NULL;

                INT32DECODE(p, obj_id);
                type = (H5F_mem_t)*p++;
                UINT64DECODE_VARLEN(p, nsects);
                flag = *p++;

                if(1==flag)
                    /* allocate section info array */
                    if(NULL == (sect_info = (H5F_sect_info_t *)H5MM_malloc(nsects * sizeof(H5F_sect_info_t))))
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* get value through the native plugin */
                ret_value = H5MD__temp_file_optional(H5I_object(obj_id), optional_type, H5_REQUEST_NULL,
                                                     sect_info, &ret, type, nsects);
                if(ret < 0 || ret_value < 0){
                    if(MPI_SUCCESS != MPI_Send(&ret, sizeof(int64_t), MPI_BYTE, source, 
                                               H5MD_RETURN_TAG, MPI_COMM_WORLD))
                        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to get free sections for file");
                }

                buf_size = sizeof(int64_t);
                if(sect_info != NULL) {
                    unsigned u;
                    for(u=0 ; u<nsects ; u++) {
                        buf_size += 1 + H5V_limit_enc_size((uint64_t)sect_info[u].addr) + 
                            1 + H5V_limit_enc_size((uint64_t)sect_info[u].size);
                    }
                }

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                p1 = (uint8_t *)send_buf;
                INT64ENCODE(p1, ret);
                if(sect_info != NULL) {
                    unsigned u;
                    for(u=0 ; u<nsects ; u++) {
                        UINT64ENCODE_VARLEN(p1, sect_info[u].addr);
                        UINT64ENCODE_VARLEN(p1, sect_info[u].size);
                    }
                }

                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
                H5MM_free(send_buf);
                H5MM_xfree(sect_info);
                break;
            }
        case H5VL_FILE_GET_INFO:
            {
                hid_t obj_id;
                H5I_type_t type;
                H5F_info2_t finfo;
                void *send_buf = NULL;
                size_t buf_size = 0;
                uint8_t *p1 = NULL;

                INT32DECODE(p, obj_id);
                type = (H5I_type_t)*p++;

                /* get value through the native plugin */
                if((ret_value = H5MD__temp_file_optional(H5I_object(obj_id), optional_type, 
                                                         H5_REQUEST_NULL, type, &finfo)) < 0) {
                    if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(int), MPI_BYTE, source, 
                                               H5MD_RETURN_TAG, MPI_COMM_WORLD))
                        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to get file info");
                }

                buf_size += sizeof(unsigned) * 3 + sizeof(int);
                buf_size += 1 + H5V_limit_enc_size((uint64_t)finfo.super.super_size) +
                    1 + H5V_limit_enc_size((uint64_t)finfo.super.super_ext_size) +
                    1 + H5V_limit_enc_size((uint64_t)finfo.free.meta_size) +
                    1 + H5V_limit_enc_size((uint64_t)finfo.free.tot_space) +
                    1 + H5V_limit_enc_size((uint64_t)finfo.sohm.hdr_size) +
                    1 + H5V_limit_enc_size((uint64_t)finfo.sohm.msgs_info.index_size) +
                    1 + H5V_limit_enc_size((uint64_t)finfo.sohm.msgs_info.heap_size);

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                p1 = (uint8_t *)send_buf;
                INT32ENCODE(p1, ret_value);

                H5_ENCODE_UNSIGNED(p1, finfo.super.version);
                UINT64ENCODE_VARLEN(p1, finfo.super.super_size);
                UINT64ENCODE_VARLEN(p1, finfo.super.super_ext_size);
                H5_ENCODE_UNSIGNED(p1, finfo.free.version);
                UINT64ENCODE_VARLEN(p1, finfo.free.meta_size);
                UINT64ENCODE_VARLEN(p1, finfo.free.tot_space);
                H5_ENCODE_UNSIGNED(p1, finfo.sohm.version);
                UINT64ENCODE_VARLEN(p1, finfo.sohm.hdr_size);
                UINT64ENCODE_VARLEN(p1, finfo.sohm.msgs_info.index_size);
                UINT64ENCODE_VARLEN(p1, finfo.sohm.msgs_info.heap_size);

                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
                H5MM_free(send_buf);
                break;
            }
        /* H5Fget_mdc_config */
        case H5VL_FILE_GET_MDC_CONF:
            {
                H5AC_cache_config_t config_ptr;
                hid_t file_id;
                void *send_buf = NULL;
                size_t buf_size = 0;
                uint8_t *p1 = NULL;

                INT32DECODE(p, config_ptr.version);
                INT32DECODE(p, file_id);

                /* get value through the native plugin */
                if((ret_value = H5MD__temp_file_optional(H5I_object(file_id), optional_type, 
                                                         H5_REQUEST_NULL, &config_ptr)) < 0) {
                    if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(int), MPI_BYTE, source, 
                                               H5MD_RETURN_TAG, MPI_COMM_WORLD))
                        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to get cache configuration");
                }

                if(H5P__facc_cache_config_enc(&config_ptr, (void **)(&p1), &buf_size) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode cache config");

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size + sizeof(int))))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                p1 = (uint8_t *)send_buf;

                INT32ENCODE(p1, ret_value);

                if(H5P__facc_cache_config_enc(&config_ptr, (void **)(&p1), &buf_size) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode cache config");

                buf_size += sizeof(int);

                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
                H5MM_free(send_buf);                
                break;
            }
        /* H5Fget_mdc_hit_rate */
        case H5VL_FILE_GET_MDC_HR:
            {
                double ret = -1;
                hid_t obj_id;

                INT32DECODE(p, obj_id);

                /* get value through the native plugin */
                ret_value = H5MD__temp_file_optional(H5I_object(obj_id), optional_type, 
                                                     H5_REQUEST_NULL, &ret);

                if(MPI_SUCCESS != MPI_Send(&ret, sizeof(double), MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                if(ret < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to get hit rate");
                break;
            }
        /* H5Fget_mdc_size */
        case H5VL_FILE_GET_MDC_SIZE:
            {
                size_t max_size_ptr;
                size_t min_clean_size_ptr;
                size_t cur_size_ptr;
                int    cur_num_entries_ptr;
                hid_t file_id;
                void *send_buf = NULL;
                size_t buf_size = 0;
                uint8_t *p1 = NULL;

                INT32DECODE(p, file_id);

                /* get value through the native plugin */
                if((ret_value = H5MD__temp_file_optional(H5I_object(file_id), optional_type, 
                                         H5_REQUEST_NULL, &max_size_ptr, &min_clean_size_ptr, 
                                         &cur_size_ptr, &cur_num_entries_ptr)) < 0) {
                    if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(int), MPI_BYTE, source, 
                                               H5MD_RETURN_TAG, MPI_COMM_WORLD))
                        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to get mdc size");
                }

                buf_size = 2*sizeof(int32_t) +
                    1 + H5V_limit_enc_size((uint64_t)max_size_ptr) +
                    1 + H5V_limit_enc_size((uint64_t)min_clean_size_ptr) +
                    1 + H5V_limit_enc_size((uint64_t)cur_size_ptr);

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                p1 = (uint8_t *)send_buf;

                INT32ENCODE(p1, ret_value);

                UINT64ENCODE_VARLEN(p1, max_size_ptr);
                UINT64ENCODE_VARLEN(p1, min_clean_size_ptr);
                UINT64ENCODE_VARLEN(p1, cur_size_ptr);
                INT32ENCODE(p1, cur_num_entries_ptr);

                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
                H5MM_free(send_buf);                

                break;
            }
        /* H5Fclear_elink_file_cache */
        case H5VL_FILE_CLEAR_ELINK_CACHE:
            {
                hid_t obj_id;

                INT32DECODE(p, obj_id);

                /* get value through the native plugin */
                ret_value = H5MD__temp_file_optional(H5I_object(obj_id), optional_type, 
                                                     H5_REQUEST_NULL);

                if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                if(ret_value < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to clear elink cache")
                break;
            }
        /* H5Freopen */
        case H5VL_FILE_REOPEN:
            {
                hid_t obj_id, file_id = FAIL;
                H5F_t *new_file = NULL;

                INT32DECODE(p, obj_id);

                /* get value through the native plugin */
                if(H5MD__temp_file_optional(H5I_object(obj_id), optional_type, H5_REQUEST_NULL,
                                            &new_file) < 0) {
                    MPI_Send(&file_id, sizeof(hid_t), MPI_BYTE, source, H5MD_RETURN_TAG, MPI_COMM_WORLD);
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to reopen file");
                }

                /* register atom for file */
                if((file_id = H5VL_native_register(H5I_FILE, new_file, FALSE)) < 0) {
                    MPI_Send(&file_id, sizeof(hid_t), MPI_BYTE, source, H5MD_RETURN_TAG, MPI_COMM_WORLD);
                    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to create file");
                }

                /* Send the meta data file ID to the client */
                if(MPI_SUCCESS != MPI_Send(&file_id, sizeof(hid_t), MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
                break;
            }
        /* H5Freset_mdc_hit_rate_stats */
        case H5VL_FILE_RESET_MDC_HIT_RATE:
            {
                hid_t obj_id;

                INT32DECODE(p, obj_id);

                /* get value through the native plugin */
                ret_value = H5MD__temp_file_optional(H5I_object(obj_id), optional_type, H5_REQUEST_NULL);

                if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                if(ret_value < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to reset mdc hit rate")

                break;
            }
        case H5VL_FILE_SET_MDC_CONFIG:
            {
                H5AC_cache_config_t config_ptr;
                hid_t obj_id;

                INT32DECODE(p, obj_id);

                if((ret_value = H5P__facc_cache_config_dec((const void **)(&p), &config_ptr)) < 0) {
                    MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, H5MD_RETURN_TAG, MPI_COMM_WORLD);
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to decode cache config");
                }

                /* get value through the native plugin */
                ret_value = H5MD__temp_file_optional(H5I_object(obj_id), optional_type, H5_REQUEST_NULL,
                                                     &config_ptr);

                if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                if(ret_value < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to set MDC config")

                break;
            }
        case H5VL_FILE_GET_VFD_HANDLE:
        case H5VL_FILE_GET_SIZE:
        case H5VL_FILE_GET_FILE_IMAGE:
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't perform this type of operation on file");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MD__file_optional_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__file_close_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__file_close_func(uint8_t *p, int source)
{
    hid_t file_id; /* metadata file ID */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5VL__decode_file_close_params(p, &file_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode file close params");

    /* Check/fix arguments. */
    if(H5I_FILE != H5I_get_type(file_id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file ID");

    /* Decrement reference count on atom.  When it reaches zero the file will be closed. */
    if(H5I_dec_ref(file_id) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTCLOSEFILE, FAIL, "decrementing file ID failed");

#if 0
    /* call the native plugin file create callback*/
    if(H5VL_native_file_close(H5I_object(file_id), -1) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to create file");
#endif
done:
    /* send status to client */
    if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                               H5MD_RETURN_TAG, MPI_COMM_WORLD))
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MD__file_close_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__attr_create_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__attr_create_func(uint8_t *p, int source)
{
    hid_t obj_id; /* id of location for attr */
    hid_t attr_id = FAIL; /* attr id */
    H5A_t *attr = NULL; /* New attr's info */
    H5VL_loc_params_t loc_params; /* location parameters for obj_id */
    char *name = NULL; /* name of attr (if named) */
    hid_t acpl_id = FAIL, aapl_id = FAIL;
    hid_t type_id; /* datatype for attr */
    hid_t space_id; /* dataspace for attr */
    H5P_genplist_t *plist;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5VL__decode_attr_create_params(p, &obj_id, &loc_params, &name, &acpl_id, &aapl_id,
                                       &type_id, &space_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode attr create params");

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(acpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* set creation properties */
    if(H5P_set(plist, H5VL_ATTR_TYPE_ID, &type_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for datatype id");
    if(H5P_set(plist, H5VL_ATTR_SPACE_ID, &space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for space id");

    if(NULL == (attr = (H5A_t *)H5VL_native_attr_create(H5I_object(obj_id), loc_params, name,
                                                        acpl_id, aapl_id, -1)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create attribute");

    if((attr_id = H5VL_native_register(H5I_ATTR, attr, FALSE)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENFILE, FAIL, "unable to create attr");

done:
    /* Send the meta attribute ID to the client */
    if(MPI_SUCCESS != MPI_Send(&attr_id, sizeof(hid_t), MPI_BYTE, source, 
                               H5MD_RETURN_TAG, MPI_COMM_WORLD))
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

    H5MM_xfree(name);
    if(acpl_id && acpl_id != H5P_ATTRIBUTE_CREATE_DEFAULT && H5I_dec_ref(acpl_id) < 0)
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close plist");
    if(H5I_dec_ref(type_id) < 0)
        HDONE_ERROR(H5E_DATATYPE, H5E_CANTFREE, FAIL, "can't close type");
    if(H5I_dec_ref(space_id) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTFREE, FAIL, "can't close dataspace");
    if(H5VL__close_loc_params(loc_params) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");

    FUNC_LEAVE_NOAPI(ret_value)
}/* H5MD__attr_create_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__attr_open_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__attr_open_func(uint8_t *p, int source)
{
    hid_t obj_id; /* id of location for attr */
    hid_t attr_id = FAIL; /* attr id */
    H5A_t *attr = NULL; /* New attr's info */
    H5VL_loc_params_t loc_params; /* location parameters for obj_id */
    char *name = NULL; /* name of attr (if named) */
    size_t acpl_size = 0, type_size = 0, space_size = 0;
    H5P_genplist_t *acpl;
    size_t buf_size = 0;
    hid_t aapl_id = FAIL, acpl_id = FAIL; /* plist IDs */
    void *send_buf = NULL; /* buffer to hold the attr id and layout to be sent to client */
    uint8_t *p1 = NULL; /* temporary pointer into send_buf for encoding */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5VL__decode_attr_open_params(p, &obj_id, &loc_params, &name, &aapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode attr open params");

    if(NULL == (attr = (H5A_t *)H5VL_native_attr_open(H5I_object(obj_id), loc_params, name, 
                                                      aapl_id, -1)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create attribute");

    if((attr_id = H5VL_native_register(H5I_ATTR, attr, FALSE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register attribute atom");

    /* START Encode metadata of the attr and send them to the client along with the ID */
    if((acpl_id = H5A_get_create_plist(attr)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get creation property list for attr");

    /* determine the size of the acpl if it is not default */
    if(H5P_ATTRIBUTE_CREATE_DEFAULT != acpl_id) {
        if(NULL == (acpl = (H5P_genplist_t *)H5I_object_verify(acpl_id, H5I_GENPROP_LST)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if((ret_value = H5P__encode(acpl, FALSE, NULL, &acpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }

    if(NULL != attr->shared->dt) {
        /* get Type size to encode */
        if((ret_value = H5T_encode(attr->shared->dt, NULL, &type_size)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "unable to encode datatype");
    }

    if(NULL != attr->shared->ds) {
        /* get Dataspace size to encode */
        if((ret_value = H5S_encode(attr->shared->ds, NULL, &space_size)) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "unable to encode dataspace");
    }

    buf_size = sizeof(hid_t) + /* attr ID */
        1 + H5V_limit_enc_size((uint64_t)acpl_size) + acpl_size +
        1 + H5V_limit_enc_size((uint64_t)type_size) + type_size + 
        1 + H5V_limit_enc_size((uint64_t)space_size) + space_size;

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    p1 = (uint8_t *)send_buf;

    /* encode the object id */
    INT32ENCODE(p1, attr_id);

    /* encode the plist size */
    UINT64ENCODE_VARLEN(p1, acpl_size);
    /* encode property lists if they are not default*/
    if(acpl_size) {
        if((ret_value = H5P__encode(acpl, FALSE, p1, &acpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
        p1 += acpl_size;
    }

    /* encode the datatype size */
    UINT64ENCODE_VARLEN(p1, type_size);
    if(type_size) {
        /* encode datatype */
        if((ret_value = H5T_encode(attr->shared->dt, p1, &type_size)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "unable to encode datatype");
        p1 += type_size;
    }

    /* encode the dataspace size */
    UINT64ENCODE_VARLEN(p1, space_size);
    if(space_size) {
        /* encode datatspace */
        if((ret_value = H5S_encode(attr->shared->ds, p1, &space_size)) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "unable to encode datatspace");
        p1 += space_size;
    }

done:
    if(SUCCEED == ret_value) {
        /* Send the attr id & metadata to the client */
        if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }
    else {
        /* Send failed to the client */
        if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }

    H5MM_xfree(send_buf);
    H5MM_xfree(name);
    if(H5VL__close_loc_params(loc_params) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MD__attr_open_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__attr_read_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__attr_read_func(uint8_t *p, int source)
{
    hid_t attr_id = FAIL; /* attr id */
    hid_t type_id;
    H5A_t *attr = NULL; /* New attr's info */
    size_t buf_size = 0;
    void *buf = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5VL__decode_attr_read_params(p, &attr_id, &type_id, &buf_size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode attr read params");

    if(NULL == (attr = (H5A_t *)H5I_object_verify(attr_id, H5I_ATTR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid attribute identifier");

    if(NULL == (buf = H5MM_malloc(buf_size + sizeof(int))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    if(H5VL_native_attr_read(attr, type_id, buf, -1) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_READERROR, FAIL, "unable to read attribute");

done:
    /* Send status to the client */
    if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                               H5MD_RETURN_TAG, MPI_COMM_WORLD))
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

    if(SUCCEED == ret_value) {
        /* Send the attr value to the client */
        if(MPI_SUCCESS != MPI_Send(buf, (int)buf_size, MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }

    H5MM_xfree(buf);
    if(H5I_dec_ref(type_id) < 0)
        HDONE_ERROR(H5E_DATATYPE, H5E_CANTFREE, FAIL, "can't close type");

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MD__attr_read_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__attr_write_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__attr_write_func(uint8_t *p, int source)
{
    hid_t attr_id = FAIL; /* attr id */
    hid_t type_id;
    H5A_t *attr = NULL; /* New attr's info */
    size_t buf_size = 0;
    void *buf = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5VL__decode_attr_write_params(p, &attr_id, &type_id, &buf, &buf_size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode attr write params");

    if(NULL == (attr = (H5A_t *)H5I_object_verify(attr_id, H5I_ATTR)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid attribute identifier");

    if(H5VL_native_attr_write(attr, type_id, (const void *)buf, -1) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_WRITEERROR, FAIL, "unable to write attribute");

done:
    if(H5I_dec_ref(type_id) < 0)
        HDONE_ERROR(H5E_DATATYPE, H5E_CANTFREE, FAIL, "can't close type");
    /* Send failed to the client */
    if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                               H5MD_RETURN_TAG, MPI_COMM_WORLD))
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MD__attr_write_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__attr_remove_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__attr_remove_func(uint8_t *p, int source)
{
    hid_t obj_id; /* id of location for attr */
    H5VL_loc_params_t loc_params; /* location parameters for obj_id */
    char *name = NULL; /* name of attr (if named) */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5VL__decode_attr_remove_params(p, &obj_id, &loc_params, &name) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode attr remove params");

    if(H5VL_native_attr_remove(H5I_object(obj_id), loc_params, name, -1) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to remove attribute");

done:
    /* send status to client */
    if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                               H5MD_RETURN_TAG, MPI_COMM_WORLD))
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

    if(H5VL__close_loc_params(loc_params) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MD__attr_remove_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__attr_get_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__attr_get_func(uint8_t *p, int source)
{
    H5VL_attr_get_t get_type = -1;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    get_type = (H5VL_attr_get_t)*p++;

    switch(get_type) {
        case H5VL_ATTR_EXISTS:
            {
                hid_t obj_id;
                H5VL_loc_params_t loc_params;
                char *attr_name = NULL;
                htri_t ret = -1;

                /* decode params */
                if(H5VL__decode_attr_get_params(p, get_type, &obj_id, &loc_params, &attr_name) < 0) {
                    MPI_Send(&ret, sizeof(int32_t), MPI_BYTE, source, H5MD_RETURN_TAG, MPI_COMM_WORLD);
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode attr get params");
                }
                /* get value through the native plugin */
                if(H5MD__temp_attr_get(H5I_object(obj_id), get_type, H5_REQUEST_NULL, loc_params,
                                       attr_name, &ret) < 0) {
                    MPI_Send(&ret, sizeof(int32_t), MPI_BYTE, source, H5MD_RETURN_TAG, MPI_COMM_WORLD);
                    HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "unable to determine if attribute exists");
                }

                /* send query value to client */
                if(MPI_SUCCESS != MPI_Send(&ret, sizeof(htri_t), MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                if(H5VL__close_loc_params(loc_params) < 0)
                    HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");
                H5MM_xfree(attr_name);
                break;
            }
        case H5VL_ATTR_GET_NAME:
            {
                hid_t obj_id;
                H5VL_loc_params_t loc_params;
                char *name = NULL;
                size_t size;
                ssize_t ret = -1;
                void *send_buf = NULL;
                size_t buf_size = 0; /* send_buf size */
                uint8_t *p1 = NULL; /* temporary pointer into send_buf for encoding */

                /* decode params */
                if(H5VL__decode_attr_get_params(p, get_type, &obj_id, &loc_params, &size) < 0) {
                    MPI_Send(&ret, sizeof(int64_t), MPI_BYTE, source, H5MD_RETURN_TAG, MPI_COMM_WORLD);
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode attr get params");
                }

                if(size) {
                    /* allocate the buffer for the attribute name if the size > 0 */
                    if(NULL == (name = (char *)H5MM_malloc(size)))
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
                }

                /* get value through the native plugin */
                if(H5MD__temp_attr_get(H5I_object(obj_id), get_type, H5_REQUEST_NULL, loc_params,
                                       size, name, &ret) < 0) {
                    MPI_Send(&ret, sizeof(int64_t), MPI_BYTE, source, H5MD_RETURN_TAG, MPI_COMM_WORLD);
                    HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "unable to determine if attribute exists");
                }
                buf_size = sizeof(int64_t) + size;

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                p1 = (uint8_t *)send_buf;

                /* encode the actual size of the attribute name, which may be different than the 
                   size of the name buffer being sent */
                INT64ENCODE(p1, ret);

                /* encode length of the buffer and the buffer containing part of or all the attr name*/
                if(size && name)
                    HDstrcpy((char *)p1, name);
                p1 += size;

                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                H5MM_xfree(send_buf);
                H5MM_xfree(name);
                if(H5VL__close_loc_params(loc_params) < 0)
                    HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");
                break;
            }
        case H5VL_ATTR_GET_INFO:
            {
                hid_t obj_id;
                H5VL_loc_params_t loc_params;
                void *send_buf = NULL;
                H5A_info_t ainfo;
                char *attr_name = NULL;
                size_t buf_size = 0; /* send_buf size */
                uint8_t *p1 = NULL; /* temporary pointer into send_buf for encoding */

                /* decode params */
                if((ret_value = H5VL__decode_attr_get_params(p, get_type, &obj_id, &loc_params, &attr_name)) < 0) {
                    MPI_Send(&ret_value, sizeof(int32_t), MPI_BYTE, source, H5MD_RETURN_TAG, MPI_COMM_WORLD);
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode attr get params");
                }
                if(H5VL_OBJECT_BY_SELF == loc_params.type || H5VL_OBJECT_BY_IDX == loc_params.type) {
                    /* get value through the native plugin */
                    if((ret_value = H5MD__temp_attr_get(H5I_object(obj_id), get_type, 
                                                        H5_REQUEST_NULL, loc_params, &ainfo)) < 0) {
                        MPI_Send(&ret_value, sizeof(int32_t), MPI_BYTE, source, H5MD_RETURN_TAG, MPI_COMM_WORLD);
                        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "unable to dget attribute info");
                    }
                }
                else if(H5VL_OBJECT_BY_NAME == loc_params.type) {
                    /* get value through the native plugin */
                    if((ret_value = H5MD__temp_attr_get(H5I_object(obj_id), get_type, H5_REQUEST_NULL, 
                                                        loc_params, &ainfo, attr_name)) < 0) {
                        MPI_Send(&ret_value, sizeof(int32_t), MPI_BYTE, source, H5MD_RETURN_TAG, MPI_COMM_WORLD);
                        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "unable to dget attribute info");
                    }
                    H5MM_xfree(attr_name);
                }

                buf_size = sizeof(unsigned) + sizeof(uint32_t) + 1 +
                    1 + H5V_limit_enc_size((uint64_t)ainfo.data_size);

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                p1 = (uint8_t *)send_buf;

                H5_ENCODE_UNSIGNED(p1, ainfo.corder_valid);
                UINT32ENCODE(p1, ainfo.corder);
                *p1++ = (uint8_t)ainfo.cset;
                UINT64ENCODE_VARLEN(p1, ainfo.data_size);

                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
                H5MM_xfree(send_buf);
                if(H5VL__close_loc_params(loc_params) < 0)
                    HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");
                break;
            }
        case H5VL_ATTR_GET_SPACE:
        case H5VL_ATTR_GET_TYPE:
        case H5VL_ATTR_GET_ACPL:
        case H5VL_ATTR_GET_STORAGE_SIZE:
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from attr");
    }

done:
    if(SUCCEED != ret_value) {
        /* send status to client */
        if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MD__attr_get_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__attr_close_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__attr_close_func(uint8_t *p, int source)
{
    hid_t attr_id = FAIL; /* attr id */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode the object id */
    if(H5VL__decode_attr_close_params(p, &attr_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode attr close params");

    /* Check/fix arguments. */
    if(H5I_ATTR != H5I_get_type(attr_id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a attr ID");

    if(H5I_dec_ref(attr_id) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTDEC, FAIL, "can't decrement count on attr ID");

done:
    /* send status to client */
    if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                               H5MD_RETURN_TAG, MPI_COMM_WORLD))
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MD__attr_close_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__dataset_create_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__dataset_create_func(uint8_t *p, int source)
{
    hid_t obj_id; /* id of location for dataset */
    hid_t dset_id = FAIL; /* dset id */
    H5D_t *dset = NULL; /* New dataset's info */
    H5VL_loc_params_t loc_params; /* location parameters for obj_id */
    char *name = NULL; /* name of dataset (if named) */
    hid_t dcpl_id = FAIL, dapl_id = FAIL, lcpl_id = FAIL; /* plist IDs */
    hid_t type_id; /* datatype for dataset */
    hid_t space_id; /* dataspace for dataset */
    H5P_genplist_t  *plist;
    haddr_t eoa;
    void *send_buf = NULL; /* buffer to hold the dataset id and layout to be sent to client */
    size_t buf_size = 0; /* send_buf size */
    uint8_t *p1 = NULL; /* temporary pointer into send_buf for encoding */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5VL__decode_dataset_create_params(p, &obj_id, &loc_params, &name, &dcpl_id, &dapl_id,
                                          &type_id, &space_id, &lcpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode dataset create params");

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* set creation properties */
    if(H5P_set(plist, H5VL_DSET_TYPE_ID, &type_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for datatype id");
    if(H5P_set(plist, H5VL_DSET_SPACE_ID, &space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for space id");
    if(H5P_set(plist, H5VL_DSET_LCPL_ID, &lcpl_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for lcpl id");

    /* Create the dataset through the native VOL */
    if(NULL == (dset = (H5D_t *)H5VL_native_dataset_create(H5I_object(obj_id), loc_params, name, 
                                                           dcpl_id, dapl_id, H5_REQUEST_NULL)))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create dataset")

    if((dset_id = H5VL_native_register(H5I_DATASET, dset, FALSE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register dataset atom");

    /* initialize current version of dataset to 0 */
    dset->shared->current_version = 0;

    /* determine the buffer size needed to store the encoded layout of the dataset */ 
    if(FAIL == (ret_value = H5D__encode_layout(dset->shared->layout, NULL, &buf_size)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode dataset layout");

    if(HADDR_UNDEF == (eoa = H5F_get_eoa(dset->oloc.file, H5FD_MEM_DRAW)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get file EOA");

    /* for the dataset ID */
    buf_size += sizeof(int) + 1 + H5V_limit_enc_size((uint64_t)eoa);

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    p1 = (uint8_t *)send_buf;

    /* encode the object id */
    INT32ENCODE(p1, dset_id);

    UINT64ENCODE_VARLEN(p1, eoa);

    /* encode layout of the dataset */ 
    if(FAIL == (ret_value = H5D__encode_layout(dset->shared->layout, p1, &buf_size)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode dataset layout");

    buf_size += sizeof(int) + 1 + H5V_limit_enc_size((uint64_t)eoa);

done:
    if(SUCCEED == ret_value) {
        /* Send the dataset id & metadata to the client */
        if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }
    else {
        /* Send fail to the client */
        if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }

    H5MM_xfree(send_buf);
    H5MM_xfree(name);
    if(type_id && H5I_dec_ref(type_id) < 0)
        HDONE_ERROR(H5E_DATATYPE, H5E_CANTFREE, FAIL, "can't close type");
    if(space_id && H5I_dec_ref(space_id) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTFREE, FAIL, "can't close space");
    if(lcpl_id && lcpl_id != H5P_LINK_CREATE_DEFAULT && H5I_dec_ref(lcpl_id) < 0)
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close plist");
    if(dcpl_id && dcpl_id != H5P_DATASET_CREATE_DEFAULT && H5I_dec_ref(dcpl_id) < 0)
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close plist");
    if(dapl_id && dapl_id != H5P_DATASET_ACCESS_DEFAULT && H5I_dec_ref(dapl_id) < 0)
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close plist");
    if(H5VL__close_loc_params(loc_params) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");

    FUNC_LEAVE_NOAPI(ret_value)
}/* H5MD__dataset_create_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__dataset_open_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__dataset_open_func(uint8_t *p, int source)
{
    hid_t obj_id; /* id of location for dataset */
    hid_t dset_id = FAIL; /* dset id */
    H5D_t *dset = NULL; /* New dataset's info */
    H5VL_loc_params_t loc_params; /* location parameters for obj_id */
    char *name = NULL; /* name of dataset (if named) */
    size_t dcpl_size = 0, type_size = 0, space_size = 0, layout_size = 0;
    H5P_genplist_t *dcpl;
    size_t buf_size = 0;
    hid_t dapl_id = FAIL; /* plist IDs */
    void *send_buf = NULL; /* buffer to hold the dataset id and layout to be sent to client */
    uint8_t *p1 = NULL; /* temporary pointer into send_buf for encoding */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5VL__decode_dataset_open_params(p, &obj_id, &loc_params, &name, &dapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode dataset open params");

    /* Open the dataset through the native VOL */
    if(NULL == (dset = (H5D_t *)H5VL_native_dataset_open(H5I_object(obj_id), loc_params, name, 
                                                         dapl_id, H5_REQUEST_NULL)))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to open dataset")

    if((dset_id = H5VL_native_register(H5I_DATASET, dset, FALSE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register dataset atom")

    /* initialize current version of dataset to 0 */
    dset->shared->current_version = 0;

    /* START Encode metadata of the dataset and send them to the client along with the ID */
    /* determine the size of the dcpl if it is not default */
    if(H5P_DATASET_CREATE_DEFAULT != dset->shared->dcpl_id) {
        if(NULL == (dcpl = (H5P_genplist_t *)H5I_object_verify(dset->shared->dcpl_id, 
                                                               H5I_GENPROP_LST)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if((ret_value = H5P__encode(dcpl, FALSE, NULL, &dcpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }

    if(NULL != dset->shared->type) {
        /* get Type size to encode */
        if((ret_value = H5T_encode(dset->shared->type, NULL, &type_size)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "unable to encode datatype");
    }

    if(NULL != dset->shared->space) {
        /* get Dataspace size to encode */
        if((ret_value = H5S_encode(dset->shared->space, NULL, &space_size)) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "unable to encode dataspace");
    }

    /* determine the buffer size needed to store the encoded layout of the dataset */ 
    if(FAIL == H5D__encode_layout(dset->shared->layout, NULL, &layout_size))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode dataset layout");

    /* for the dataset ID */
    buf_size = sizeof(hid_t) + /* dataset ID */
        1 + H5V_limit_enc_size((uint64_t)dcpl_size) + dcpl_size +
        1 + H5V_limit_enc_size((uint64_t)type_size) + type_size + 
        1 + H5V_limit_enc_size((uint64_t)space_size) + space_size + 
        1 + H5V_limit_enc_size((uint64_t)layout_size) + layout_size;

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    p1 = (uint8_t *)send_buf;

    /* encode the object id */
    INT32ENCODE(p1, dset_id);

    /* encode the plist size */
    UINT64ENCODE_VARLEN(p1, dcpl_size);
    /* encode property lists if they are not default*/
    if(H5P_DATASET_CREATE_DEFAULT != dset->shared->dcpl_id) {
        if((ret_value = H5P__encode(dcpl, FALSE, p1, &dcpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
        p1 += dcpl_size;
    }

    /* encode the datatype size */
    UINT64ENCODE_VARLEN(p1, type_size);
    if(type_size) {
        /* encode datatype */
        if((ret_value = H5T_encode(dset->shared->type, p1, &type_size)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "unable to encode datatype");
        p1 += type_size;
    }

    /* encode the dataspace size */
    UINT64ENCODE_VARLEN(p1, space_size);
    if(space_size) {
        /* encode datatspace */
        if((ret_value = H5S_encode(dset->shared->space, p1, &space_size)) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "unable to encode datatspace");
        p1 += space_size;
    }

    /* encode the layout size */
    UINT64ENCODE_VARLEN(p1, layout_size);
    if(layout_size) {
        /* encode layout of the dataset */ 
        if(FAIL == H5D__encode_layout(dset->shared->layout, p1, &layout_size))
            HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode dataset layout");
        p1 += layout_size;
    }

done:
    if(SUCCEED == ret_value) {
        /* Send the dataset id & metadata to the client */
        if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }
    else {
        /* Send failed to the client */
        if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }

    H5MM_xfree(send_buf);
    H5MM_xfree(name);
    if(dapl_id && dapl_id != H5P_DATASET_ACCESS_DEFAULT && H5I_dec_ref(dapl_id) < 0)
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close plist");
    if(H5VL__close_loc_params(loc_params) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MD__dataset_open_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__dataset_set_extent_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__dataset_set_extent_func(uint8_t *p, int source)
{
    hid_t dset_id = FAIL; /* dset id */
    H5D_t *dset = NULL;
    hsize_t *size = NULL;
    int rank;
    void *send_buf = NULL; /* buffer to hold the dataset id and layout to be sent to client */
    uint8_t *p1 = NULL; /* temporary pointer into send_buf for encoding */
    size_t dcpl_size = 0, space_size = 0, layout_size = 0;
    H5P_genplist_t *dcpl;
    size_t buf_size = 0;
    haddr_t eoa;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5VL__decode_dataset_set_extent_params(p, &dset_id, &rank, &size) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode dataset set_extent params");

    if(NULL == (dset = (H5D_t *)H5I_object_verify(dset_id, H5I_DATASET)))
        HGOTO_ERROR(H5E_ARGS, H5E_CANTINIT, FAIL, "not a dataset");

    /* Set_Extent the dataset through the native VOL */
    if(H5VL_native_dataset_set_extent(dset, size, H5_REQUEST_NULL) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to set_extent dataset");

    /* increment current version of dataset */
    dset->shared->current_version ++;

    if(HADDR_UNDEF == (eoa = H5F_get_eoa(dset->oloc.file, H5FD_MEM_DRAW)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get file EOA");

    /* encode metadata of the dataset and send them to the client */
    /* determine the size of the dcpl if it is not default */
    if(H5P_DATASET_CREATE_DEFAULT != dset->shared->dcpl_id) {
        if(NULL == (dcpl = (H5P_genplist_t *)H5I_object_verify(dset->shared->dcpl_id, 
                                                               H5I_GENPROP_LST)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if((ret_value = H5P__encode(dcpl, FALSE, NULL, &dcpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }

    if(NULL != dset->shared->space) {
        /* get Dataspace size to encode */
        if((ret_value = H5S_encode(dset->shared->space, NULL, &space_size)) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "unable to encode dataspace");
    }

    /* determine the buffer size needed to store the encoded layout of the dataset */ 
    if(FAIL == H5D__encode_layout(dset->shared->layout, NULL, &layout_size))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode dataset layout");

    /* for the dataset ID */
    buf_size = sizeof(int32_t) + 
        1 + H5V_limit_enc_size((uint64_t)eoa) +
        1 + H5V_limit_enc_size((uint64_t)dcpl_size) + dcpl_size +
        1 + H5V_limit_enc_size((uint64_t)space_size) + space_size + 
        1 + H5V_limit_enc_size((uint64_t)layout_size) + layout_size;

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    p1 = (uint8_t *)send_buf;

    INT32ENCODE(p1, ret_value);
    UINT64ENCODE_VARLEN(p1, eoa);

    /* encode the plist size */
    UINT64ENCODE_VARLEN(p1, dcpl_size);
    /* encode property lists if they are not default*/
    if(H5P_DATASET_CREATE_DEFAULT != dset->shared->dcpl_id) {
        if((ret_value = H5P__encode(dcpl, FALSE, p1, &dcpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
        p1 += dcpl_size;
    }

    /* encode the dataspace size */
    UINT64ENCODE_VARLEN(p1, space_size);
    if(space_size) {
        /* encode datatspace */
        if((ret_value = H5S_encode(dset->shared->space, p1, &space_size)) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "unable to encode datatspace");
        p1 += space_size;
    }

    /* encode the layout size */
    UINT64ENCODE_VARLEN(p1, layout_size);
    if(layout_size) {
        /* encode layout of the dataset */ 
        if(FAIL == H5D__encode_layout(dset->shared->layout, p1, &layout_size))
            HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode dataset layout");
        p1 += layout_size;
    }

done:
    if(SUCCEED == ret_value) {
        /* Send the dataset id & metadata to the client */
        if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }
    else {
        /* Send failed to the client */
        if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }
    H5MM_xfree(send_buf);
    H5MM_xfree(size);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MD__dataset_set_extent_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__dataset_refresh_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__dataset_refresh_func(uint8_t *p, int UNUSED source)
{
    hid_t dset_id = FAIL; /* dset id */
    H5D_t *dset = NULL; /* New dataset's info */
    int client_version = -1;
    int need_to_refresh = -1;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode the dset id */
    INT32DECODE(p, dset_id);

    /* decode the version number */
    INT32DECODE(p, client_version);

    if(NULL == (dset = (H5D_t *)H5I_object_verify(dset_id, H5I_DATASET)))
        HGOTO_ERROR(H5E_ARGS, H5E_CANTINIT, FAIL, "not a dataset");

    if(dset->shared->current_version == client_version) {
        need_to_refresh = 0;
        if(MPI_SUCCESS != MPI_Send(&need_to_refresh, sizeof(int), MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }
    else {
        size_t dcpl_size = 0, space_size = 0, layout_size = 0;
        H5P_genplist_t *dcpl;
        size_t buf_size = 0;
        void *send_buf = NULL; /* buffer to hold the dataset id and layout to be sent to client */
        uint8_t *p1 = NULL; /* temporary pointer into send_buf for encoding */

        need_to_refresh = 1;

        /* START Encode metadata of the dataset and send them to the client */
        /* determine the size of the dcpl if it is not default */
        if(H5P_DATASET_CREATE_DEFAULT != dset->shared->dcpl_id) {
            if(NULL == (dcpl = (H5P_genplist_t *)H5I_object_verify(dset->shared->dcpl_id, 
                                                                   H5I_GENPROP_LST)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
            if((ret_value = H5P__encode(dcpl, FALSE, NULL, &dcpl_size)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
        }

        if(NULL != dset->shared->space) {
            /* get Dataspace size to encode */
            if((ret_value = H5S_encode(dset->shared->space, NULL, &space_size)) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "unable to encode dataspace");
        }

        /* determine the buffer size needed to store the encoded layout of the dataset */ 
        if(FAIL == H5D__encode_layout(dset->shared->layout, NULL, &layout_size))
            HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode dataset layout");

        buf_size = 2 * sizeof(int) +
            1 + H5V_limit_enc_size((uint64_t)dcpl_size) + dcpl_size +
            1 + H5V_limit_enc_size((uint64_t)space_size) + space_size + 
            1 + H5V_limit_enc_size((uint64_t)layout_size) + layout_size;

        /* allocate the buffer for encoding the parameters */
        if(NULL == (send_buf = H5MM_malloc(buf_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

        p1 = (uint8_t *)send_buf;

        /* encode a flag to refresh and the current version number*/
        INT32ENCODE(p1, need_to_refresh);
        INT32ENCODE(p1, dset->shared->current_version);

        /* encode the plist size */
        UINT64ENCODE_VARLEN(p1, dcpl_size);
        /* encode property lists if they are not default*/
        if(H5P_DATASET_CREATE_DEFAULT != dset->shared->dcpl_id) {
            if((ret_value = H5P__encode(dcpl, FALSE, p1, &dcpl_size)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
            p1 += dcpl_size;
        }

        /* encode the dataspace size */
        UINT64ENCODE_VARLEN(p1, space_size);
        if(space_size) {
            /* encode datatspace */
            if((ret_value = H5S_encode(dset->shared->space, p1, &space_size)) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "unable to encode datatspace");
            p1 += space_size;
        }

        /* encode the layout size */
        UINT64ENCODE_VARLEN(p1, layout_size);
        if(layout_size) {
            /* encode layout of the dataset */ 
            if(FAIL == H5D__encode_layout(dset->shared->layout, p1, &layout_size))
                HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode dataset layout");
            p1 += layout_size;
        }

        /* Send the dataset id & metadata to the client */
        if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

        H5MM_xfree(send_buf);
    }

done:
    if(SUCCEED != ret_value) {
        /* Send failed to the client */
        if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MD__dataset_refresh_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__dataset_close_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__dataset_close_func(uint8_t *p, int source)
{
    hid_t dset_id = FAIL; /* dset id */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode the object id */
    if(H5VL__decode_dataset_close_params(p, &dset_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode dataset close params");

    /* Check/fix arguments. */
    if(H5I_DATASET != H5I_get_type(dset_id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset ID");

    if(H5I_dec_app_ref_always_close(dset_id) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTDEC, FAIL, "can't decrement count on dataset ID");

done:
    /* send status to client */
    if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                               H5MD_RETURN_TAG, MPI_COMM_WORLD))
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MD__dataset_close_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__datatype_commit_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__datatype_commit_func(uint8_t *p, int source)
{
    hid_t obj_id; /* id of location for dataset */
    H5T_t *type = NULL; /* New dataset's info */
    H5VL_loc_params_t loc_params; /* location parameters for obj_id */
    H5G_loc_t loc; /* Object location to insert dataset into */
    char *name = NULL; /* name of dataset (if named) */
    hid_t tcpl_id = FAIL, tapl_id = FAIL, lcpl_id = FAIL; /* plist IDs */
    hid_t type_id; /* datatype for dataset */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5VL__decode_datatype_commit_params(p, &obj_id, &loc_params, &name, &type_id, &lcpl_id, 
                                           &tcpl_id, &tapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode dataset create params");

    if(H5G_loc(obj_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object");
    if(NULL == (type = (H5T_t *)H5I_object_verify(type_id, H5I_DATATYPE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");

    if(NULL != name) { /* H5Tcommit */
        /* Commit the type */
        if(H5T__commit_named(&loc, name, type, lcpl_id, tcpl_id, tapl_id, H5AC_dxpl_id) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to commit datatype");
    }
    else { /* H5Tcommit_anon */
        /* Commit the type */
        if(H5T__commit(loc.oloc->file, type, tcpl_id, H5AC_dxpl_id) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to commit datatype");

        /* Release the datatype's object header */
        {
            H5O_loc_t *oloc;         /* Object location for datatype */

            /* Get the new committed datatype's object location */
            if(NULL == (oloc = H5T_oloc(type)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "unable to get object location of committed datatype");

            /* Decrement refcount on committed datatype's object header in memory */
            if(H5O_dec_rc_by_loc(oloc, H5AC_dxpl_id) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDEC, FAIL, "unable to decrement refcount on newly created object");
        } /* end if */
    }

done:
    if(SUCCEED == ret_value) {
        /* Send the meta data type ID to the client */
        if(MPI_SUCCESS != MPI_Send(&type_id, sizeof(hid_t), MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }
    else {
        /* send a failed message to the client */
        if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }

    H5MM_xfree(name);
    if(lcpl_id && lcpl_id != H5P_LINK_CREATE_DEFAULT && H5I_dec_ref(lcpl_id) < 0)
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close plist");
    if(tcpl_id && tcpl_id != H5P_DATATYPE_CREATE_DEFAULT && H5I_dec_ref(tcpl_id) < 0)
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close plist");
    if(tapl_id && tapl_id != H5P_DATATYPE_ACCESS_DEFAULT && H5I_dec_ref(tapl_id) < 0)
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close plist");
    if(H5VL__close_loc_params(loc_params) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");

    FUNC_LEAVE_NOAPI(ret_value)
}/* H5MD__datatype_commit_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__datatype_open_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__datatype_open_func(uint8_t *p, int source)
{
    hid_t obj_id; /* id of location for dataset */
    H5T_t *type = NULL; /* New datatype */
    hid_t type_id; /* datatype id */
    hid_t tapl_id = FAIL;
    H5VL_loc_params_t loc_params; /* location parameters for obj_id */
    char *name = NULL; /* name of dataset (if named) */
    size_t type_size = 0, buf_size = 0;
    void *send_buf = NULL; /* buffer to hold the dataset id and layout to be sent to client */
    uint8_t *p1 = NULL; /* temporary pointer into send_buf for encoding */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5VL__decode_datatype_open_params(p, &obj_id, &loc_params, &name, &tapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode dataset create params");

    /* Open the datatype through the native VOL */
    if(NULL == (type = (H5T_t *)H5VL_native_datatype_open(H5I_object(obj_id), loc_params, name, 
                                                          tapl_id, H5_REQUEST_NULL)))
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to open datatype")

    if((type_id = H5I_register(H5I_DATATYPE, type, FALSE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register datatype atom")

    /* get Type size to encode */
    if((ret_value = H5T_encode(type, NULL, &type_size)) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "unable to encode datatype");

    buf_size = type_size + sizeof(hid_t);

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    p1 = (uint8_t *)send_buf;

    /* encode the object id */
    INT32ENCODE(p1, type_id);

    /* encode datatype */
    if((ret_value = H5T_encode(type, p1, &type_size)) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "unable to encode datatype");
    p1 += type_size;

done:
    if(SUCCEED == ret_value) {
        /* Send the datatype id & metadata to the client */
        if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }
    else {
        /* Send the failed to the client */
        if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }

    H5MM_xfree(send_buf);
    H5MM_xfree(name);
    if(tapl_id && tapl_id != H5P_DATATYPE_ACCESS_DEFAULT && H5I_dec_ref(tapl_id) < 0)
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close plist");
    if(H5VL__close_loc_params(loc_params) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");

    FUNC_LEAVE_NOAPI(ret_value)
}/* H5MD__datatype_open_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__datatype_close_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__datatype_close_func(uint8_t *p, int source)
{
    hid_t type_id = FAIL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode the object id */
    if(H5VL__decode_datatype_close_params(p, &type_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode datatype close params");

    if(H5I_dec_ref(type_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDEC, FAIL, "can't close datatype");

done:
    /* send status to client */
    if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                               H5MD_RETURN_TAG, MPI_COMM_WORLD))
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MD__datatype_close_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__group_create_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__group_create_func(uint8_t *p, int source)
{
    hid_t obj_id; /* id of location for group */
    hid_t grp_id = FAIL; /* group id */
    H5G_t *grp = NULL; /* New group's info */
    H5VL_loc_params_t loc_params; /* location parameters for obj_id */
    char *name = NULL; /* name of group (if named) */
    H5P_genplist_t  *plist;            /* Property list pointer */
    hid_t gcpl_id = FAIL, gapl_id = FAIL, lcpl_id = FAIL; /* plist IDs */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5VL__decode_group_create_params(p, &obj_id, &loc_params, &name, &gcpl_id, &gapl_id, 
                                        &lcpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode group create params");

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(gcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    if(H5P_set(plist, H5VL_GRP_LCPL_ID, &lcpl_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for lcpl id")

    /* Create the group through the native VOL */
    if(NULL == (grp = (H5G_t *)H5VL_native_group_create(H5I_object(obj_id), loc_params, name, 
                                                        gcpl_id, gapl_id, H5_REQUEST_NULL)))
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create group")

    /* Get an atom for the group */
    if((grp_id = H5VL_native_register(H5I_GROUP, grp, FALSE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize group handle");

done:
    if(SUCCEED == ret_value) {
        /* Send the meta datagroup ID to the client */
        if(MPI_SUCCESS != MPI_Send(&grp_id, sizeof(hid_t), MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }
    else {
        /* send a failed message to the client */
        if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }

    if(name)
        H5MM_xfree(name);
    if(lcpl_id && lcpl_id != H5P_LINK_CREATE_DEFAULT && H5I_dec_ref(lcpl_id) < 0)
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close plist");
    if(gcpl_id && gcpl_id != H5P_GROUP_CREATE_DEFAULT && H5I_dec_ref(gcpl_id) < 0)
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close plist");
    if(gapl_id && gapl_id != H5P_GROUP_ACCESS_DEFAULT && H5I_dec_ref(gapl_id) < 0)
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close plist");
    if(H5VL__close_loc_params(loc_params) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");

    FUNC_LEAVE_NOAPI(ret_value)
}/* H5MD__group_create_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__group_open_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__group_open_func(uint8_t *p, int source)
{
    hid_t obj_id; /* id of location for group */
    hid_t grp_id = FAIL; /* group id */
    H5VL_loc_params_t loc_params; /* location parameters for obj_id */
    H5G_t *grp = NULL;
    char *name = NULL; /* name of group (if named) */
    hid_t gapl_id = FAIL; /* plist IDs */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5VL__decode_group_open_params(p, &obj_id, &loc_params, &name, &gapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode group open params");

    /* Open the group through the native VOL */
    if(NULL == (grp = (H5G_t *)H5VL_native_group_open(H5I_object(obj_id), loc_params, name, 
                                                      gapl_id, H5_REQUEST_NULL)))
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to open group")

    if((grp_id = H5VL_native_register(H5I_GROUP, grp, FALSE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize group handle");

done:
    if(SUCCEED == ret_value) {
        /* Send the meta datagroup ID to the client */
        if(MPI_SUCCESS != MPI_Send(&grp_id, sizeof(hid_t), MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }
    else {
        /* send a failed message to the client */
        if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }

    if(name)
        H5MM_xfree(name);
    if(gapl_id && gapl_id != H5P_GROUP_ACCESS_DEFAULT && H5I_dec_ref(gapl_id) < 0)
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close plist");
    if(H5VL__close_loc_params(loc_params) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");

    FUNC_LEAVE_NOAPI(ret_value)
}/* H5MD__group_open_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__group_get_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__group_get_func(uint8_t *p, int source)
{
    H5VL_group_get_t get_type = -1;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    get_type = (H5VL_group_get_t)*p++;

    switch(get_type) {
        /* H5Gget_info */
        case H5VL_GROUP_GET_GCPL:
            {
                hid_t obj_id;
                hid_t gcpl_id;
                void *send_buf = NULL;
                size_t buf_size = 0, gcpl_size = 0;
                uint8_t *p1;     /* Current pointer into buffer */
                H5P_genplist_t *gcpl = NULL;

                /* decode params */
                if(H5VL__decode_group_get_params(p, get_type, &obj_id) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode group get params");

                /* get value through the native plugin */
                if(H5MD__temp_group_get(H5I_object(obj_id), get_type, H5_REQUEST_NULL, &gcpl_id) < 0)
                    HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "unable to determine if attribute exists");

                /* get size for property lists to encode */
                if(H5P_GROUP_CREATE_DEFAULT != gcpl_id) {
                    if(NULL == (gcpl = (H5P_genplist_t *)H5I_object_verify(gcpl_id, H5I_GENPROP_LST)))
                        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
                    if((ret_value = H5P__encode(gcpl, FALSE, NULL, &gcpl_size)) < 0)
                        HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
                }

                buf_size = 1 + H5V_limit_enc_size((uint64_t)gcpl_size) + gcpl_size;
                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
                p1 = (uint8_t *)send_buf;

                /* encode the plist size */
                UINT64ENCODE_VARLEN(p1, gcpl_size);
                /* encode property lists if they are not default*/
                if(gcpl_size) {
                    if((ret_value = H5P__encode(gcpl, FALSE, p1, &gcpl_size)) < 0)
                        HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
                    p1 += gcpl_size;
                }

                /* send query value to client */
                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                H5MM_xfree(send_buf);
                if(gcpl_id && gcpl_id != H5P_GROUP_CREATE_DEFAULT && H5I_dec_ref(gcpl_id) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close plist");
                break;
            }
        /* H5Gget_info */
        case H5VL_GROUP_GET_INFO:
            {
                hid_t obj_id;
                H5VL_loc_params_t loc_params;
                void *send_buf = NULL;
                H5G_info_t ginfo;
                size_t buf_size = 0; /* send_buf size */
                uint8_t *p1 = NULL; /* temporary pointer into send_buf for encoding */

                /* decode params */
                if(H5VL__decode_group_get_params(p, get_type, &obj_id, &loc_params) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode group get params");

                /* get value through the native plugin */
                if(H5MD__temp_group_get(H5I_object(obj_id), get_type, H5_REQUEST_NULL, loc_params,
                                        &ginfo) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "unable to dget groupibute info");

                buf_size = 1 + sizeof(unsigned) + sizeof(int64_t) +
                    1 + H5V_limit_enc_size((uint64_t)ginfo.nlinks);

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                p1 = (uint8_t *)send_buf;

                *p1++ = (uint8_t)ginfo.storage_type;
                UINT64ENCODE_VARLEN(p1, ginfo.nlinks);
                INT64ENCODE(p1, ginfo.max_corder);
                H5_ENCODE_UNSIGNED(p1, ginfo.mounted);

                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
                H5MM_xfree(send_buf);
                if(H5VL__close_loc_params(loc_params) < 0)
                    HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from group");
    }

done:
    if(SUCCEED != ret_value) {
        /* send status to client */
        if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MD__group_get_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__group_close_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__group_close_func(uint8_t *p, int source)
{
    hid_t grp_id = FAIL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode the object id */
    if(H5VL__decode_group_close_params(p, &grp_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode group close params");

    /* Check/fix arguments. */
    if(H5I_GROUP != H5I_get_type(grp_id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a group ID");

    /* Decrement reference count on atom.  When it reaches zero the group will be closed. */
    if(H5I_dec_ref(grp_id) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTDEC, FAIL, "decrementing grp ID failed");

done:
    /* send status to client */
    if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                               H5MD_RETURN_TAG, MPI_COMM_WORLD))
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MD__group_close_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__link_create_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__link_create_func(uint8_t *p, int source)
{
    hid_t obj_id; /* id of location for link */
    void *obj;
    H5VL_loc_params_t loc_params; /* location parameters for obj_id */
    hid_t lcpl_id = FAIL, lapl_id = FAIL;
    H5VL_link_create_type_t create_type;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5VL__decode_link_create_params(p, &create_type, &obj_id, &loc_params, &lcpl_id, &lapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode link create params");

    if(H5L_SAME_LOC == obj_id)
        obj = NULL;
    else
        obj = H5I_object(obj_id);

    /* Create the link through the native VOL */
    if(H5VL_native_link_create(create_type, obj, loc_params, lcpl_id, lapl_id, H5_REQUEST_NULL) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create link");

done:
    if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                               H5MD_RETURN_TAG, MPI_COMM_WORLD))
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

    switch(create_type) {
        case H5VL_LINK_CREATE_HARD:
            {
                H5P_genplist_t *plist;
                H5VL_loc_params_t cur_loc_params;

                /* Get the plist structure */
                if(NULL == (plist = (H5P_genplist_t *)H5I_object(lcpl_id)))
                    HDONE_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

                if(H5P_get(plist, H5VL_LINK_TARGET_LOC_PARAMS, &cur_loc_params) < 0)
                    HDONE_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value");
                if(H5VL__close_loc_params(cur_loc_params) < 0)
                    HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");
                break;
            }
        case H5VL_LINK_CREATE_SOFT:
            {
                H5P_genplist_t *plist;
                char *name;

                /* Get the plist structure */
                if(NULL == (plist = (H5P_genplist_t *)H5I_object(lcpl_id)))
                    HDONE_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

                if(H5P_get(plist, H5VL_LINK_TARGET_NAME, &name) < 0)
                    HDONE_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for target name");

                H5MM_xfree(name);
                break;
            }
        case H5VL_LINK_CREATE_UD:
            {
                H5P_genplist_t *plist;
                void *udata;

                /* Get the plist structure */
                if(NULL == (plist = (H5P_genplist_t *)H5I_object(lcpl_id)))
                    HDONE_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

                if(H5P_get(plist, H5VL_LINK_UDATA, &udata) < 0)
                    HDONE_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value from plist");
                H5MM_xfree(udata);
                break;
            }
        default:
            HDONE_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "invalid link creation call");
    }

    if(lcpl_id && lcpl_id != H5P_LINK_CREATE_DEFAULT && H5I_dec_ref(lcpl_id) < 0)
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close plist");
    if(lapl_id && lapl_id != H5P_LINK_ACCESS_DEFAULT && H5I_dec_ref(lapl_id) < 0)
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close plist");
    if(H5VL__close_loc_params(loc_params) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");

    FUNC_LEAVE_NOAPI(ret_value)
}/* H5MD__link_create_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__link_move_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__link_move_func(uint8_t *p, int source)
{
    hid_t src_id;
    hid_t dst_id;
    H5VL_loc_params_t loc_params1;
    H5VL_loc_params_t loc_params2;
    hbool_t copy_flag;
    hid_t lcpl_id = FAIL, lapl_id = FAIL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5VL__decode_link_move_params(p, &src_id, &loc_params1, &dst_id, &loc_params2, 
                                     &copy_flag, &lcpl_id, &lapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode link move params");

    if(H5VL_native_link_move(H5I_object(src_id), loc_params1, H5I_object(dst_id), loc_params2,
                             copy_flag, lcpl_id, lapl_id, H5_REQUEST_NULL) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to move link");

done:
    if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                               H5MD_RETURN_TAG, MPI_COMM_WORLD))
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

    if(lcpl_id && lcpl_id != H5P_LINK_CREATE_DEFAULT && H5I_dec_ref(lcpl_id) < 0)
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close plist");
    if(lapl_id && lapl_id != H5P_LINK_ACCESS_DEFAULT && H5I_dec_ref(lapl_id) < 0)
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close plist");
    if(H5VL__close_loc_params(loc_params1) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");
    if(H5VL__close_loc_params(loc_params2) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");

    FUNC_LEAVE_NOAPI(ret_value)
}/* H5MD__link_move_func */

static herr_t
H5L__iterate_cb(hid_t group_id, const char *link_name, const H5L_info_t *linfo, void *op_data)
{
    int source = *((int *)op_data);
    void *send_buf = NULL;
    size_t buf_size = 0; /* send_buf size */
    uint8_t *p = NULL; /* temporary pointer into send_buf for encoding */
    size_t len = 0;
    herr_t ret_value = H5_ITER_CONT;

    FUNC_ENTER_NOAPI_NOINIT

    /* get name size to encode */
    if(NULL != link_name)
        len = HDstrlen(link_name) + 1;

    /* calculate size of buffer needed */
    buf_size = 2*sizeof(int32_t) + 1 + H5V_limit_enc_size((uint64_t)len) + len +
        2 + sizeof(unsigned) + sizeof(int64_t);
    if(linfo->type == H5L_TYPE_HARD) {
        buf_size += 1 + H5V_limit_enc_size((uint64_t)linfo->u.address);
    }
    else if (linfo->type == H5L_TYPE_SOFT || linfo->type == H5L_TYPE_EXTERNAL ||
             (linfo->type >= H5L_TYPE_UD_MIN && linfo->type <= H5L_TYPE_MAX)) {
        buf_size += 1 + H5V_limit_enc_size((uint64_t)linfo->u.val_size);
    }
    else
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "invalid link type");

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    p = (uint8_t *)send_buf;

    /* encode a flag that this message is for the client to call the user callback on
       and not for the client to finish iteration */
    INT32ENCODE(p, H5MD_CONT);

    /* encode the group id */
    INT32ENCODE(p, group_id);

    /* encode length of the link name and the actual link name */
    UINT64ENCODE_VARLEN(p, len);
    if(NULL != link_name && len != 0)
        HDstrcpy((char *)p, link_name);
    p += len;

    /* encode Link info struct */
    *p++ = (uint8_t)linfo->type;
    H5_ENCODE_UNSIGNED(p, linfo->corder_valid);
    INT64ENCODE(p, linfo->corder);
    *p++ = (uint8_t)linfo->cset;
    if(linfo->type == H5L_TYPE_HARD) {
        UINT64ENCODE_VARLEN(p, linfo->u.address);
    }
    else if (linfo->type == H5L_TYPE_SOFT || linfo->type == H5L_TYPE_EXTERNAL ||
             (linfo->type >= H5L_TYPE_UD_MIN && linfo->type <= H5L_TYPE_MAX)) {
        UINT64ENCODE_VARLEN(p, linfo->u.val_size);
    }

    /* send the callback data back to the client and recieve the return value */
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, source, H5MD_RETURN_TAG,
                                   &ret_value, sizeof(int), MPI_BYTE, source, H5MD_LISTEN_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to communicate with MDS server");

    H5MM_free(send_buf);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
* Function:	H5MD__link_iterate_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__link_iterate_func(uint8_t *p, int source)
{
    hid_t obj_id;
    H5VL_loc_params_t loc_params;
    hbool_t recursive;
    H5_index_t idx_type;
    H5_iter_order_t order;
    hsize_t *idx = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5VL__decode_link_iterate_params(p, &obj_id, &loc_params, &recursive, &idx_type, 
                                        &order, &idx) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode link iterate params");

    if(H5VL_native_link_iterate(H5I_object(obj_id), loc_params, recursive, idx_type, order, idx,
                                H5L__iterate_cb, &source, H5_REQUEST_NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_BADITER, FAIL, "link iteration failed");

done:
    if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(int32_t), MPI_BYTE, source, 
                               H5MD_RETURN_TAG, MPI_COMM_WORLD))
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

    if(NULL != idx) {
        if(MPI_SUCCESS != MPI_Send(idx, sizeof(hsize_t), MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }
    H5MM_xfree(idx);
    if(H5VL__close_loc_params(loc_params) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");

    FUNC_LEAVE_NOAPI(ret_value)
}/* H5MD__link_iterate_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__link_get_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__link_get_func(uint8_t *p, int source)
{
    H5VL_link_get_t get_type = -1;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    get_type = (H5VL_link_get_t)*p++;

    switch(get_type) {
        case H5VL_LINK_EXISTS:
            {
                hid_t obj_id;
                H5VL_loc_params_t loc_params;
                htri_t ret = -1;

                /* decode params */
                if(H5VL__decode_link_get_params(p, get_type, &obj_id, &loc_params) < 0) {
                    MPI_Send(&ret, sizeof(int32_t), MPI_BYTE, source, H5MD_RETURN_TAG, MPI_COMM_WORLD);
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode link get params");
                }

                /* get value through the native plugin */
                if(H5MD__temp_link_get(H5I_object(obj_id), loc_params, get_type, H5_REQUEST_NULL,
                                       &ret) < 0) {
                    ret = -1;
                    MPI_Send(&ret, sizeof(int32_t), MPI_BYTE, source, H5MD_RETURN_TAG, MPI_COMM_WORLD);
                    HGOTO_ERROR(H5E_LINK, H5E_CANTGET, FAIL, "unable to determine if link exists");
                }

                /* send query value to client */
                if(MPI_SUCCESS != MPI_Send(&ret, sizeof(htri_t), MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                if(H5VL__close_loc_params(loc_params) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");
                break;
            }
        case H5VL_LINK_GET_INFO:
            {
                hid_t obj_id;
                H5VL_loc_params_t loc_params;
                void *send_buf = NULL;
                H5L_info_t linfo;
                size_t buf_size = 0; /* send_buf size */
                uint8_t *p1 = NULL; /* temporary pointer into send_buf for encoding */

                /* decode params */
                if((ret_value = H5VL__decode_link_get_params(p, get_type, &obj_id, &loc_params)) < 0) {
                    MPI_Send(&ret_value, sizeof(int32_t), MPI_BYTE, source, H5MD_RETURN_TAG, MPI_COMM_WORLD);
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode link get params");
                }

                /* get info through the native plugin */
                if((ret_value = H5MD__temp_link_get(H5I_object(obj_id), loc_params, get_type, 
                                                    H5_REQUEST_NULL, &linfo)) < 0) {
                    MPI_Send(&ret_value, sizeof(int32_t), MPI_BYTE, source, H5MD_RETURN_TAG, MPI_COMM_WORLD);
                    HGOTO_ERROR(H5E_LINK, H5E_CANTGET, FAIL, "unable to dget link info");
                }

                buf_size = 1 + sizeof(unsigned) + sizeof(int64_t) + 1;

                if(linfo.type == H5L_TYPE_HARD) {
                    buf_size += 1 + H5V_limit_enc_size((uint64_t)linfo.u.address);
                }
                else if (linfo.type == H5L_TYPE_SOFT || linfo.type == H5L_TYPE_EXTERNAL ||
                         (linfo.type >= H5L_TYPE_UD_MIN && linfo.type <= H5L_TYPE_MAX)) {
                    buf_size += 1 + H5V_limit_enc_size((uint64_t)linfo.u.val_size);
                }
                else
                    HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "invalid link type");

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                p1 = (uint8_t *)send_buf;

                *p1++ = (uint8_t)linfo.type;
                H5_ENCODE_UNSIGNED(p1, linfo.corder_valid);
                INT64ENCODE(p1, linfo.corder);
                *p1++ = (uint8_t)linfo.cset;

                if(linfo.type == H5L_TYPE_HARD) {
                    UINT64ENCODE_VARLEN(p1, linfo.u.address);
                }
                else if (linfo.type == H5L_TYPE_SOFT || linfo.type == H5L_TYPE_EXTERNAL ||
                         (linfo.type >= H5L_TYPE_UD_MIN && linfo.type <= H5L_TYPE_MAX)) {
                    UINT64ENCODE_VARLEN(p1, linfo.u.val_size);
                }

                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                H5MM_xfree(send_buf);
                if(H5VL__close_loc_params(loc_params) < 0)
                    HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");
                break;
            }
        case H5VL_LINK_GET_NAME:
            {
                hid_t obj_id;
                H5VL_loc_params_t loc_params;
                char *name = NULL;
                size_t size;
                ssize_t ret = -1;
                void *send_buf = NULL;
                size_t buf_size = 0; /* send_buf size */
                uint8_t *p1 = NULL; /* temporary pointer into send_buf for encoding */

                /* decode params */
                if(H5VL__decode_link_get_params(p, get_type, &obj_id, &loc_params, &size) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode link get params");

                if(size) {
                    /* allocate the buffer for the link name if the size > 0 */
                    if(NULL == (name = (char *)H5MM_malloc(size)))
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
                }

                /* get value through the native plugin */
                if(H5MD__temp_link_get(H5I_object(obj_id), loc_params, get_type, H5_REQUEST_NULL,
                                       name, size, &ret) < 0) {
                    MPI_Send(&ret, sizeof(int64_t), MPI_BYTE, source, H5MD_RETURN_TAG, MPI_COMM_WORLD);
                    HGOTO_ERROR(H5E_LINK, H5E_CANTGET, FAIL, "unable to determine link name");
                }

                buf_size = sizeof(int64_t) + size;

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                p1 = (uint8_t *)send_buf;

                /* encode the actual size of the linkibute name, which may be different than the 
                   size of the name buffer being sent */
                INT64ENCODE(p1, ret);

                /* encode length of the buffer and the buffer containing part of or all the link name*/
                if(size && name)
                    HDstrcpy((char *)p1, name);
                p1 += size;

                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                H5MM_xfree(send_buf);
                H5MM_xfree(name);
                if(H5VL__close_loc_params(loc_params) < 0)
                    HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");
                break;
            }
        case H5VL_LINK_GET_VAL:
            {
                hid_t obj_id;
                H5VL_loc_params_t loc_params;
                void *val = NULL;
                size_t size;

                /* decode params */
                if(H5VL__decode_link_get_params(p, get_type, &obj_id, &loc_params, &size) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode link get params");

                if(size) {
                    /* allocate the buffer for the link val if the size > 0 */
                    if(NULL == (val = H5MM_malloc(size)))
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
                }

                /* get value through the native plugin */
                if(H5MD__temp_link_get(H5I_object(obj_id), loc_params, get_type, H5_REQUEST_NULL,
                                       val, size) < 0)
                    HGOTO_ERROR(H5E_LINK, H5E_CANTGET, FAIL, "unable to determine link val");

                if(MPI_SUCCESS != MPI_Send(val, (int)size, MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                H5MM_xfree(val);
                if(H5VL__close_loc_params(loc_params) < 0)
                    HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from link");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MD__link_get_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__link_remove_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__link_remove_func(uint8_t *p, int source)
{
    hid_t obj_id;
    H5VL_loc_params_t loc_params;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5VL__decode_link_remove_params(p, &obj_id, &loc_params) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode link remove params");

    if(H5VL_native_link_remove(H5I_object(obj_id), loc_params, H5_REQUEST_NULL) < 0)
        HGOTO_ERROR(H5E_LINK, H5E_CANTDELETE, FAIL, "unable to delete link");

done:
    if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                               H5MD_RETURN_TAG, MPI_COMM_WORLD))
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

    if(H5VL__close_loc_params(loc_params) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");

    FUNC_LEAVE_NOAPI(ret_value)
}/* H5MD__link_remove_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__object_open_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__object_open_func(uint8_t *p, int source)
{
    hid_t obj_id; /* id of location for object */
    H5VL_loc_params_t loc_params; /* location parameters for obj_id */
    void *object = NULL; /* The object that is opened by the native plugin */
    H5I_type_t opened_type; /* the object type that was opened */
    hid_t new_id = FAIL; /* MDS ID for the object opened */
    size_t buf_size = 0;
    void *send_buf = NULL; /* buffer to hold the dataset id and layout to be sent to client */
    uint8_t *p1 = NULL; /* temporary pointer into send_buf for encoding */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5VL__decode_object_open_params(p, &obj_id, &loc_params) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode object open params");

    /* Open the object through the native VOL */
    if(NULL == (object = H5VL_native_object_open(H5I_object(obj_id), loc_params, &opened_type,
                                                 H5_REQUEST_NULL)))
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to open object")

    if((new_id = H5VL_native_register(opened_type, object, FALSE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize object handle");

    switch(opened_type) {
        case H5I_DATASET:
            {
                H5D_t *dset = (H5D_t *)object;
                size_t dcpl_size = 0, type_size = 0, space_size = 0, layout_size = 0;
                H5P_genplist_t *dcpl;

                /* START Encode metadata of the dataset and send them to the client along with the ID */
                /* determine the size of the dcpl if it is not default */
                if(H5P_DATASET_CREATE_DEFAULT != dset->shared->dcpl_id) {
                    if(NULL == (dcpl = (H5P_genplist_t *)H5I_object_verify(dset->shared->dcpl_id, 
                                                                           H5I_GENPROP_LST)))
                        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
                    if((ret_value = H5P__encode(dcpl, FALSE, NULL, &dcpl_size)) < 0)
                        HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
                }

                if(NULL != dset->shared->type) {
                    /* get Type size to encode */
                    if((ret_value = H5T_encode(dset->shared->type, NULL, &type_size)) < 0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "unable to encode datatype");
                }

                if(NULL != dset->shared->space) {
                    /* get Dataspace size to encode */
                    if((ret_value = H5S_encode(dset->shared->space, NULL, &space_size)) < 0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "unable to encode dataspace");
                }

                /* determine the buffer size needed to store the encoded layout of the dataset */ 
                if(FAIL == H5D__encode_layout(dset->shared->layout, NULL, &layout_size))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode dataset layout");

                /* for the dataset ID */
                buf_size = 1 + sizeof(hid_t) + /* dataset ID */
                    1 + H5V_limit_enc_size((uint64_t)dcpl_size) + dcpl_size +
                    1 + H5V_limit_enc_size((uint64_t)type_size) + type_size + 
                    1 + H5V_limit_enc_size((uint64_t)space_size) + space_size + 
                    1 + H5V_limit_enc_size((uint64_t)layout_size) + layout_size;

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                p1 = (uint8_t *)send_buf;

                /* encode the object id and type*/
                INT32ENCODE(p1, new_id);
                *p1++ = (uint8_t)opened_type;

                /* encode the plist size */
                UINT64ENCODE_VARLEN(p1, dcpl_size);
                /* encode property lists if they are not default*/
                if(H5P_DATASET_CREATE_DEFAULT != dset->shared->dcpl_id) {
                    if((ret_value = H5P__encode(dcpl, FALSE, p1, &dcpl_size)) < 0)
                        HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
                    p1 += dcpl_size;
                }

                /* encode the datatype size */
                UINT64ENCODE_VARLEN(p1, type_size);
                if(type_size) {
                    /* encode datatype */
                    if((ret_value = H5T_encode(dset->shared->type, p1, &type_size)) < 0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "unable to encode datatype");
                    p1 += type_size;
                }

                /* encode the dataspace size */
                UINT64ENCODE_VARLEN(p1, space_size);
                if(space_size) {
                    /* encode datatspace */
                    if((ret_value = H5S_encode(dset->shared->space, p1, &space_size)) < 0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "unable to encode datatspace");
                    p1 += space_size;
                }

                /* encode the layout size */
                UINT64ENCODE_VARLEN(p1, layout_size);
                if(layout_size) {
                    /* encode layout of the dataset */ 
                    if(FAIL == H5D__encode_layout(dset->shared->layout, p1, &layout_size))
                        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode dataset layout");
                    p1 += layout_size;
                }

                break;
            }
        case H5I_DATATYPE:
            {
                H5T_t *type = (H5T_t *)object; /* New datatype */
                size_t type_size = 0;

                /* get Type size to encode */
                if((ret_value = H5T_encode(type, NULL, &type_size)) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "unable to encode datatype");

                buf_size = 1 + type_size + sizeof(hid_t);

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                p1 = (uint8_t *)send_buf;

                /* encode the object id and type*/
                INT32ENCODE(p1, new_id);
                *p1++ = (uint8_t)opened_type;

                /* encode datatype */
                if((ret_value = H5T_encode(type, p1, &type_size)) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "unable to encode datatype");
                p1 += type_size;

                break;
            }
        case H5I_GROUP:
            {
                buf_size = 1 + sizeof(hid_t);

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                p1 = (uint8_t *)send_buf;

                /* encode the object id and type*/
                INT32ENCODE(p1, new_id);
                *p1++ = (uint8_t)opened_type;

                break;
            }
        case H5I_UNINIT:
        case H5I_BADID:
        case H5I_FILE:
        case H5I_DATASPACE:
        case H5I_ATTR:
        case H5I_REFERENCE:
        case H5I_VFL:
        case H5I_VOL:
        case H5I_GENPROP_CLS:
        case H5I_GENPROP_LST:
        case H5I_ERROR_CLASS:
        case H5I_ERROR_MSG:
        case H5I_ERROR_STACK:
        case H5I_NTYPES:
        default:
            HGOTO_ERROR(H5E_ARGS, H5E_CANTRELEASE, FAIL, "not a valid file object (dataset, group, or datatype)")
        break;
    }
done:
    if(SUCCEED == ret_value) {
        /* Send the dataset id & metadata to the client */
        if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }
    else {
        /* send a failed message to the client */
        if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }

    if(H5VL__close_loc_params(loc_params) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");
    H5MM_xfree(send_buf);
    FUNC_LEAVE_NOAPI(ret_value)
}/* H5MD__object_open_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__object_copy_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__object_copy_func(uint8_t *p, int source)
{
    hid_t src_id;
    hid_t dst_id;
    H5VL_loc_params_t loc_params1;
    H5VL_loc_params_t loc_params2;
    char *src_name = NULL;
    char *dst_name = NULL;
    hid_t lcpl_id = FAIL, ocpypl_id = FAIL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5VL__decode_object_copy_params(p, &src_id, &loc_params1, &src_name,
                                       &dst_id, &loc_params2, &dst_name,
                                       &ocpypl_id, &lcpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode object copy params");

    if(H5VL_native_object_copy(H5I_object(src_id), loc_params1, src_name,
                               H5I_object(dst_id), loc_params2, dst_name,
                               ocpypl_id, lcpl_id, H5_REQUEST_NULL) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to copy object");

done:
    if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                               H5MD_RETURN_TAG, MPI_COMM_WORLD))
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

    H5MM_xfree(src_name);
    H5MM_xfree(dst_name);
    if(lcpl_id && lcpl_id != H5P_LINK_CREATE_DEFAULT && H5I_dec_ref(lcpl_id) < 0)
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close plist");
    if(ocpypl_id && ocpypl_id != H5P_OBJECT_COPY_DEFAULT && H5I_dec_ref(ocpypl_id) < 0)
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close plist");
    if(H5VL__close_loc_params(loc_params1) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");
    if(H5VL__close_loc_params(loc_params2) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");

    FUNC_LEAVE_NOAPI(ret_value)
}/* H5MD__object_copy_func */

static herr_t
H5O__visit_cb(hid_t obj_id, const char *name, const H5O_info_t *info, void *op_data)
{
    int source = *((int *)op_data);
    void *send_buf = NULL;
    size_t buf_size = 0, info_size = 0; /* send_buf size */
    uint8_t *p = NULL; /* temporary pointer into send_buf for encoding */
    size_t len = 0;
    herr_t ret_value = H5_ITER_CONT;

    FUNC_ENTER_NOAPI_NOINIT

    /* get name size to encode */
    if(NULL != name)
        len = HDstrlen(name) + 1;

    /* determine the buffer size needed to store the encoded info struct */ 
    if(FAIL == H5O__encode_info(info, NULL, &info_size))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode object info");

    /* calculate size of buffer needed */
    buf_size = 2*sizeof(int32_t) + 
        1 + H5V_limit_enc_size((uint64_t)len) + len +
        1 + H5V_limit_enc_size((uint64_t)info_size) + info_size;

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    p = (uint8_t *)send_buf;

    /* encode a flag that this message is for the client to call the user callback on
       and not for the client to finish iteration */
    INT32ENCODE(p, H5MD_CONT);

    /* encode the object id */
    INT32ENCODE(p, obj_id);

    /* encode length of the link name and the actual link name */
    UINT64ENCODE_VARLEN(p, len);
    if(NULL != name && len != 0)
        HDstrcpy((char *)p, name);
    p += len;

    /* encode the info size */
    UINT64ENCODE_VARLEN(p, info_size);
    if(info_size) {
        /* encode info of the object */ 
        if(FAIL == H5O__encode_info(info, p, &info_size))
            HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode object info");
        p += info_size;
    }

    /* send the callback data back to the client and recieve the return value */
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, source, H5MD_RETURN_TAG,
                                   &ret_value, sizeof(int), MPI_BYTE, source, H5MD_LISTEN_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to communicate with MDS server");

    H5MM_free(send_buf);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
* Function:	H5MD__object_visit_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__object_visit_func(uint8_t *p, int source)
{
    hid_t obj_id;
    H5VL_loc_params_t loc_params;
    H5_index_t idx_type;
    H5_iter_order_t order;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5VL__decode_object_visit_params(p, &obj_id, &loc_params, &idx_type, &order) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode object visit params");

    if(H5VL_native_object_visit(H5I_object(obj_id), loc_params, idx_type, order, 
                                H5O__visit_cb, &source, H5_REQUEST_NULL) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_BADITER, FAIL, "object visit failed");

done:
    if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(int32_t), MPI_BYTE, source, 
                               H5MD_RETURN_TAG, MPI_COMM_WORLD))
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    if(H5VL__close_loc_params(loc_params) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");

    FUNC_LEAVE_NOAPI(ret_value)
}/* H5MD__object_visit_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__object_misc_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__object_misc_func(uint8_t *p, int source)
{
    H5VL_object_misc_t misc_type = -1;
    hid_t obj_id;
    H5VL_loc_params_t loc_params;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    misc_type = (H5VL_object_misc_t)*p++;

    switch(misc_type) {
        case H5VL_ATTR_RENAME:
            {
                char *old_name;
                char *new_name;

                /* decode params */
                if(H5VL__decode_object_misc_params(p, misc_type, &obj_id, &loc_params,
                                                   &old_name, &new_name) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode object misc params");

                /* perform operation through native plugin */
                ret_value = H5MD__temp_object_misc(H5I_object(obj_id), loc_params, misc_type, 
                                                   H5_REQUEST_NULL, old_name, new_name);

                /* send status to client */
                if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                H5MM_xfree(old_name);
                H5MM_xfree(new_name);

                if(ret_value < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to rename attribute");
                break;
            }
        case H5VL_OBJECT_CHANGE_REF_COUNT:
            {
                int update_ref;

                /* decode params */
                if(H5VL__decode_object_misc_params(p, misc_type, &obj_id, &loc_params,
                                                   &update_ref) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode object misc params");

                /* perform operation through native plugin */
                ret_value = H5MD__temp_object_misc(H5I_object(obj_id), loc_params, misc_type, 
                                                   H5_REQUEST_NULL, update_ref);

                /* send status to client */
                if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                if(ret_value < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "unable to change ref count");
                break;
            }
        case H5VL_OBJECT_SET_COMMENT:
            {
                char *comment;

                /* decode params */
                if(H5VL__decode_object_misc_params(p, misc_type, &obj_id, &loc_params,
                                                   &comment) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode object misc params");

                /* perform operation through native plugin */
                ret_value = H5MD__temp_object_misc(H5I_object(obj_id), loc_params, misc_type, 
                                                   H5_REQUEST_NULL, comment);

                /* send status to client */
                if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                if(ret_value < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTSET, FAIL, "unable to set comment");

                H5MM_xfree(comment);
                break;
            }
        case H5VL_REF_CREATE:
            {
                char *name = NULL;
                H5R_type_t ref_type;
                hid_t space_id = -1;
                void *ref = NULL;
                size_t ref_size = 0;

                /* decode params */
                if(H5VL__decode_object_misc_params(p, misc_type, &obj_id, &loc_params,
                                                   &name, &ref_type, &space_id) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode object misc params");

                if(ref_type == H5R_DATASET_REGION)
                    ref_size = sizeof(hdset_reg_ref_t);
                else if (ref_type == H5R_OBJECT)
                    ref_size = sizeof(hobj_ref_t);

                ref = HDmalloc(ref_size);

                /* perform operation through native plugin */
                if(H5MD__temp_object_misc(H5I_object(obj_id), loc_params, misc_type, H5_REQUEST_NULL,
                                          ref, name, ref_type, space_id) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "unable to create reference");

                /* send reference pointer to client */
                if(MPI_SUCCESS != MPI_Send(ref, (int)ref_size, MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to ref pointer");

                H5MM_xfree(name);
                HDfree(ref);
                if(space_id && H5I_dec_ref(space_id) < 0)
                    HGOTO_ERROR(H5E_DATASPACE, H5E_CANTFREE, FAIL, "can't close dataspace");
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't recognize this operation type");
    }

done:
    if(H5VL__close_loc_params(loc_params) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MD__object_misc_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__object_get_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__object_get_func(uint8_t *p, int source)
{
    H5VL_object_get_t get_type = -1;
    hid_t obj_id;
    H5VL_loc_params_t loc_params;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    get_type = (H5VL_object_get_t)*p++;

    switch(get_type) {
        case H5VL_OBJECT_EXISTS:
            {
                htri_t ret = -1;

                /* decode params */
                if(H5VL__decode_object_get_params(p, get_type, &obj_id, &loc_params) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode object get params");

                /* perform operation through native plugin */
                if(H5MD__temp_object_get(H5I_object(obj_id), loc_params, get_type, H5_REQUEST_NULL, 
                                         &ret) < 0) {
                    MPI_Send(&ret, sizeof(int32_t), MPI_BYTE, source, H5MD_RETURN_TAG, MPI_COMM_WORLD);
                    HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "unable to determine if object exists");
                }

                /* send query value to client */
                if(MPI_SUCCESS != MPI_Send(&ret, sizeof(htri_t), MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                break;
            }
        case H5VL_OBJECT_GET_INFO:
            {
                H5O_info_t obj_info;
                size_t info_size = 0;
                size_t buf_size = 0;
                void *send_buf = NULL; /* buffer to hold the dataset id and layout to be sent to client */
                uint8_t *p1 = NULL; /* temporary pointer into send_buf for encoding */

                /* decode params */
                if(H5VL__decode_object_get_params(p, get_type, &obj_id, &loc_params) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode object get params");

                /* perform operation through native plugin */
                if((ret_value = H5MD__temp_object_get(H5I_object(obj_id), loc_params, get_type, 
                                                      H5_REQUEST_NULL, &obj_info)) < 0) {
                    MPI_Send(&ret_value, sizeof(int32_t), MPI_BYTE, source, H5MD_RETURN_TAG, MPI_COMM_WORLD);
                    HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "unable to get object info");
                }

                /* determine the buffer size needed to store the encoded info struct */ 
                if(FAIL == H5O__encode_info(&obj_info, NULL, &info_size))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode object info");

                buf_size += 1 + H5V_limit_enc_size((uint64_t)info_size) + info_size;

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                p1 = (uint8_t *)send_buf;

                /* encode the info size */
                UINT64ENCODE_VARLEN(p1, info_size);
                if(info_size) {
                    /* encode info of the object */ 
                    if(FAIL == H5O__encode_info(&obj_info, p1, &info_size))
                        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "failed to encode object info");
                    p1 += info_size;
                }

                /* Send the dataset id & metadata to the client */
                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                H5MM_xfree(send_buf);
                break;
            }
        case H5VL_OBJECT_GET_COMMENT:
            {
                char *comment;
                size_t size;
                ssize_t ret = -1;
                void *send_buf = NULL;
                size_t buf_size = 0; /* send_buf size */
                uint8_t *p1 = NULL; /* temporary pointer into send_buf for encoding */

                /* decode params */
                if(H5VL__decode_object_get_params(p, get_type, &obj_id, &loc_params, &size) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode object get params");

                if(size) {
                    /* allocate the buffer for the link comment if the size > 0 */
                    if(NULL == (comment = (char *)H5MM_malloc(size)))
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
                }

                /* get value through the native plugin */
                if(H5MD__temp_object_get(H5I_object(obj_id), loc_params, get_type, H5_REQUEST_NULL,
                                         comment, size, &ret) < 0) {
                    ret = -1;
                    MPI_Send(&ret, sizeof(int64_t), MPI_BYTE, source, H5MD_RETURN_TAG, MPI_COMM_WORLD);
                    HGOTO_ERROR(H5E_LINK, H5E_CANTGET, FAIL, "unable to determine link comment");
                }

                buf_size = sizeof(int64_t) + size;

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                p1 = (uint8_t *)send_buf;

                /* encode the actual size of the comment, which may be different than the 
                   size of the comment buffer being sent */
                INT64ENCODE(p1, ret);

                /* encode length of the buffer and the buffer containing part of or all the comment*/
                if(size && comment)
                    HDstrcpy((char *)p1, comment);
                p1 += size;

                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                H5MM_xfree(send_buf);
                H5MM_xfree(comment);
                break;
            }
        case H5VL_REF_GET_REGION:
            {
                hid_t space_id;
                H5R_type_t ref_type;
                void *ref = NULL;
                H5S_t *dspace = NULL;
                void *send_buf = NULL;
                size_t space_size = 0, buf_size = 0;
                uint8_t *p1 = NULL; /* temporary pointer into send_buf for encoding */

                /* decode params */
                if(H5VL__decode_object_get_params(p, get_type, &obj_id, &loc_params, &ref_type,
                                                  &ref) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode object get params");

                /* get value through the native plugin */
                if(H5MD__temp_object_get(H5I_object(obj_id), loc_params, get_type, H5_REQUEST_NULL,
                                         &space_id, ref_type, ref) < 0)
                    HGOTO_ERROR(H5E_LINK, H5E_CANTGET, FAIL, "unable to determine ref region");

                /* get Dataspace size to encode */
                if(space_id >= 0) {
                    if (NULL==(dspace=(H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
                        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace");
                    if(H5S_encode(dspace, NULL, &space_size)<0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "can't encode datatype");
                }

                buf_size += 1 + H5V_limit_enc_size((uint64_t)space_size) + space_size;

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                p1 = (uint8_t *)send_buf;

                /* encode the dataspace size */
                UINT64ENCODE_VARLEN(p1, space_size);
                if (space_size) {
                    /* encode datatspace */
                    if((ret_value = H5S_encode(dspace, p1, &space_size)) < 0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "unable to encode datatspace");
                    p1 += space_size;
                }
                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                H5MM_xfree(send_buf);
                if(H5I_dec_ref(space_id) < 0)
                    HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDEC, FAIL, "failed to close space ID");
                HDfree(ref);
                break;
            }
        case H5VL_REF_GET_TYPE:
            {
                H5R_type_t ref_type;
                void *ref = NULL;
                H5O_type_t obj_type;

                /* decode params */
                if(H5VL__decode_object_get_params(p, get_type, &obj_id, &loc_params, &ref_type,
                                                  &ref) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode object get params");

                /* get value through the native plugin */
                if(H5MD__temp_object_get(H5I_object(obj_id), loc_params, get_type, H5_REQUEST_NULL,
                                         &obj_type, ref_type, ref) < 0)
                    HGOTO_ERROR(H5E_LINK, H5E_CANTGET, FAIL, "unable to determine reference type");

                if(MPI_SUCCESS != MPI_Send(&obj_type, 1, MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                HDfree(ref);
                break;
            }
        case H5VL_REF_GET_NAME:
            {
                H5R_type_t ref_type;
                void *ref = NULL;
                size_t size = 0;
                ssize_t ret = 0;
                char *name = NULL;
                void *send_buf = NULL;
                size_t buf_size = 0;
                uint8_t *p1 = NULL; /* temporary pointer into send_buf for encoding */

                /* decode params */
                if(H5VL__decode_object_get_params(p, get_type, &obj_id, &loc_params, &ref_type,
                                                  &ref, &size) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode object get params");

                if(size) {
                    /* allocate the buffer for the name if the size > 0 */
                    if(NULL == (name = (char *)H5MM_malloc(size)))
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
                }

                /* get value through the native plugin */
                if(H5MD__temp_object_get(H5I_object(obj_id), loc_params, get_type, H5_REQUEST_NULL,
                                         &ret, name, size, ref_type, ref) < 0)
                    HGOTO_ERROR(H5E_LINK, H5E_CANTGET, FAIL, "unable to determine ref region");

                buf_size = sizeof(int64_t) + size;

                /* allocate the buffer for encoding the parameters */
                if(NULL == (send_buf = H5MM_malloc(buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                p1 = (uint8_t *)send_buf;

                /* encode the actual size of the name, which may be different than the 
                   size of the name buffer being sent */
                INT64ENCODE(p1, ret);

                /* encode length of the buffer and the buffer containing part of or all the link name*/
                if(size && name)
                    HDstrcpy((char *)p1, name);
                p1 += size;

                if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, source, 
                                           H5MD_RETURN_TAG, MPI_COMM_WORLD))
                    HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                H5MM_xfree(send_buf);
                H5MM_xfree(name);
                HDfree(ref);
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from object")
    }

done:
    if(H5VL__close_loc_params(loc_params) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "Can't close loc_params");

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MD__object_get_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__allocate_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__allocate_func(uint8_t *p, int source)
{
    hid_t file_id; /* metadata file ID */
    H5F_t *file = NULL; /* metadata file struct */
    H5FD_t *fd = NULL; /* metadata file driver struct */
    hid_t dxpl_id; /* encoded dxpl */
    size_t dxpl_size;
    H5FD_mem_t type; /* Memory VFD type to indicate metadata or raw data */
    hsize_t size; /* requested size to allocate from the EOA */
    hsize_t orig_size; /* Original allocation size */
    haddr_t eoa; /* Address of end-of-allocated space */
    hsize_t extra; /* Extra space to allocate, to align request */
    haddr_t return_addr = HADDR_UNDEF;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* the metadata file id */
    INT32DECODE(p, file_id);

    /* the memory VFD type */
    type = (H5FD_mem_t)*p++;

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, dxpl_size);
    /* decode property lists if they are not default*/
    if(dxpl_size) {
        if((dxpl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += dxpl_size;
    }
    else {
        dxpl_id = H5P_DATASET_XFER_DEFAULT;
    }

    /* size requested to allocate from the VFD */
    UINT64DECODE_VARLEN(p, size);

    /* get the file object */
    if(NULL == (file = (H5F_t *)H5I_object(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier");

    fd = file->shared->lf;
    /* Get current end-of-allocated space address */
    eoa = fd->cls->get_eoa(fd, type);

    /* Compute extra space to allocate, if this is a new block and should be aligned */
    extra = 0;
    orig_size = size;

    if(fd->alignment > 1 && orig_size >= fd->threshold) {
        hsize_t mis_align;              /* Amount EOA is misaligned */

        /* Check for EOA already aligned */
        if((mis_align = (eoa % fd->alignment)) > 0) {
            extra = fd->alignment - mis_align;
        } /* end if */
    } /* end if */

    /* Add in extra allocation amount */
    size += extra;

    /* Check for overflow when extending */
    if(H5F_addr_overflow(eoa, size) || (eoa + size) > fd->maxaddr)
        HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, FAIL, "file allocation request failed");
    /* Set the [possibly aligned] address to return */
    return_addr = eoa + extra;

    /* Extend the end-of-allocated space address */
    eoa += size;
    if(fd->cls->set_eoa(fd, type, eoa) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, FAIL, "file allocation request failed");

    if(H5F_super_dirty(file) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTMARKDIRTY, FAIL, "unable to mark superblock as dirty");

done:
    if(SUCCEED != ret_value)
        return_addr = HADDR_UNDEF;

    /* Send the haddr to the client */
    if(MPI_SUCCESS != MPI_Send(&return_addr, sizeof(uint64_t), MPI_BYTE, source, 
                               H5MD_RETURN_TAG, MPI_COMM_WORLD))
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MD__allocate_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__get_eoa_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__get_eoa_func(uint8_t *p, int source)
{
    hid_t file_id; /* metadata file ID */
    H5F_t *file = NULL; /* metadata file struct */
    H5FD_t *fd = NULL; /* metadata file driver struct */
    H5FD_mem_t type; /* Memory VFD type to indicate metadata or raw data */
    haddr_t eoa; /* Address of end-of-allocated space */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* the metadata file id */
    INT32DECODE(p, file_id);
    /* the memory VFD type */
    type = (H5FD_mem_t)*p++;

    /* get the file object */
    if(NULL == (file = (H5F_t *)H5I_object(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier");

    fd = file->shared->lf;
    /* Get current end-of-allocated space address */
    eoa = fd->cls->get_eoa(fd, type);

done:
    if(SUCCEED != ret_value)
        eoa = HADDR_UNDEF;

    /* Send the haddr to the client */
    if(MPI_SUCCESS != MPI_Send(&eoa, sizeof(uint64_t), MPI_BYTE, source, 
                               H5MD_RETURN_TAG, MPI_COMM_WORLD))
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MD__get_eoa_func */

/*-------------------------------------------------------------------------
* Function:	H5MD__set_eoa_func
*------------------------------------------------------------------------- */
static herr_t
H5MD__set_eoa_func(uint8_t *p, int source)
{
    hid_t file_id; /* metadata file ID */
    H5F_t *file = NULL; /* metadata file struct */
    H5FD_t *fd = NULL; /* metadata file driver struct */
    H5FD_mem_t type; /* Memory VFD type to indicate metadata or raw data */
    haddr_t eoa; /* Address of end-of-allocated space */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* the metadata file id */
    INT32DECODE(p, file_id);
    /* the memory VFD type */
    type = (H5FD_mem_t)*p++;
    /* the EOA to set*/
    UINT64DECODE(p, eoa);

    /* get the file object */
    if(NULL == (file = (H5F_t *)H5I_object(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier");

    fd = file->shared->lf;
    /* Get current end-of-allocated space address */
    ret_value = fd->cls->set_eoa(fd, type, eoa);

done:
    /* Send the confirmation to the client */
    if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(int), MPI_BYTE, source, 
                               H5MD_RETURN_TAG, MPI_COMM_WORLD))
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_MDS_SET_EOA */

/*-------------------------------------------------------------------------
* Function:	H5MD__chunk_insert
*------------------------------------------------------------------------- */
static herr_t
H5MD__chunk_insert(uint8_t *p, int source)
{
    hid_t dset_id; /* metadata file ID */
    H5D_t *dset = NULL;
    H5D_chunk_ud_t udata;
    H5D_chk_idx_info_t idx_info;
    void *send_buf;
    size_t buf_size = 0, dxpl_size = 0;
    uint8_t *p1;
    unsigned u;
    hsize_t *offsets;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    /* the metadata file id */
    INT32DECODE(p, dset_id);

    /* get the dataset object */
    if(NULL == (dset = (H5D_t *)H5I_object_verify(dset_id, H5I_DATASET)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid dataset identifier");

    /* Compose chunked index info struct */
    idx_info.f = dset->oloc.file;
    idx_info.pline = &dset->shared->dcpl_cache.pline;
    idx_info.layout = &dset->shared->layout.u.chunk;
    idx_info.storage = &dset->shared->layout.storage.u.chunk;

    /* Initialize the query information about the chunk we are looking for */
    udata.common.layout = &(dset->shared->layout.u.chunk);
    udata.common.storage = &(dset->shared->layout.storage.u.chunk);
    udata.common.rdcc = &(dset->shared->cache.chunk);

    /* decode udata */
    H5_DECODE_UNSIGNED(p, udata.idx_hint);
    UINT32DECODE(p, udata.nbytes);
    H5_DECODE_UNSIGNED(p, udata.filter_mask);
    UINT64DECODE_VARLEN(p, udata.addr);

    if(NULL == (offsets = (hsize_t *)H5MM_malloc(sizeof(hsize_t) * idx_info.layout->ndims)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    for(u = 0; u < idx_info.layout->ndims; u++)
        UINT64DECODE_VARLEN(p, offsets[u]);

    udata.common.offset = offsets;

    /* decode the index address */
    UINT64DECODE_VARLEN(p, idx_info.storage->idx_addr);

    /* decode the dxpl size */
    UINT64DECODE_VARLEN(p, dxpl_size);
    /* decode property lists if they are not default*/
    if(dxpl_size) {
        if((idx_info.dxpl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += dxpl_size;
    }
    else
        idx_info.dxpl_id = H5P_DATASET_XFER_DEFAULT;

    /* apply cache tag to dxpl */
    if(H5AC_tag(idx_info.dxpl_id, dset->oloc.addr, NULL) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "unable to apply metadata tag");

    /* Create the chunk */
    if((dset->shared->layout.storage.u.chunk.ops->insert)(&idx_info, &udata) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINSERT, FAIL, "unable to insert/resize chunk");

    buf_size = sizeof(unsigned)*2 + sizeof(uint32_t) +
        1 + H5V_limit_enc_size((uint64_t)(udata.addr));

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    p1 = (uint8_t *)send_buf;

    /* encode udata */
    H5_ENCODE_UNSIGNED(p1, udata.idx_hint);
    UINT32ENCODE(p1, udata.nbytes);
    H5_ENCODE_UNSIGNED(p1, udata.filter_mask);
    UINT64ENCODE_VARLEN(p1, udata.addr);

done:
    /* Send the confirmation to the client */
    if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, source, 
                               H5MD_RETURN_TAG, MPI_COMM_WORLD))
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

    H5MM_xfree(offsets);
    if(idx_info.dxpl_id && idx_info.dxpl_id != H5P_DATASET_XFER_DEFAULT && 
       H5I_dec_ref(idx_info.dxpl_id) < 0)
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close plist");

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MD__chunk_insert */

/*-------------------------------------------------------------------------
* Function:	H5MD__chunk_get_addr
*------------------------------------------------------------------------- */
static herr_t
H5MD__chunk_get_addr(uint8_t *p, int source)
{
    hid_t dset_id; /* metadata file ID */
    H5D_t *dset = NULL;
    H5D_chunk_ud_t udata;
    H5D_chk_idx_info_t idx_info;
    void *send_buf;
    size_t buf_size = 0, dxpl_size = 0;
    uint8_t *p1;
    unsigned u;
    hsize_t *offsets;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    /* the metadata file id */
    INT32DECODE(p, dset_id);

    /* get the dataset object */
    if(NULL == (dset = (H5D_t *)H5I_object_verify(dset_id, H5I_DATASET)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid dataset identifier");

    /* Compose chunked index info struct */
    idx_info.f = dset->oloc.file;
    idx_info.pline = &dset->shared->dcpl_cache.pline;
    idx_info.layout = &dset->shared->layout.u.chunk;
    idx_info.storage = &dset->shared->layout.storage.u.chunk;

    /* Initialize the query information about the chunk we are looking for */
    udata.common.layout = &(dset->shared->layout.u.chunk);
    udata.common.storage = &(dset->shared->layout.storage.u.chunk);
    udata.common.rdcc = &(dset->shared->cache.chunk);

    /* decode udata */
    H5_DECODE_UNSIGNED(p, udata.idx_hint);
    UINT32DECODE(p, udata.nbytes);
    H5_DECODE_UNSIGNED(p, udata.filter_mask);
    UINT64DECODE_VARLEN(p, udata.addr);

    if(NULL == (offsets = (hsize_t *)H5MM_malloc(sizeof(hsize_t) * idx_info.layout->ndims)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    for(u = 0; u < idx_info.layout->ndims; u++)
        UINT64DECODE_VARLEN(p, offsets[u]);

    udata.common.offset = offsets;

    /* decode the index address */
    UINT64DECODE_VARLEN(p, idx_info.storage->idx_addr);

    /* decode the dxpl size */
    UINT64DECODE_VARLEN(p, dxpl_size);
    /* decode property lists if they are not default*/
    if(dxpl_size) {
        if((idx_info.dxpl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += dxpl_size;
    }
    else
        idx_info.dxpl_id = H5P_DATASET_XFER_DEFAULT;

    /* apply cache tag to dxpl */
    if(H5AC_tag(idx_info.dxpl_id, dset->oloc.addr, NULL) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "unable to apply metadata tag");

    /* Create the chunk */
    if((dset->shared->layout.storage.u.chunk.ops->get_addr)(&idx_info, &udata) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't query chunk address");

    buf_size = sizeof(unsigned)*2 + sizeof(uint32_t) +
        1 + H5V_limit_enc_size((uint64_t)(udata.addr));

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    p1 = (uint8_t *)send_buf;

    /* encode udata */
    H5_ENCODE_UNSIGNED(p1, udata.idx_hint);
    UINT32ENCODE(p1, udata.nbytes);
    H5_ENCODE_UNSIGNED(p1, udata.filter_mask);
    UINT64ENCODE_VARLEN(p1, udata.addr);

done:
    if(SUCCEED == ret_value) {
        /* Send the confirmation to the client */
        if(MPI_SUCCESS != MPI_Send(send_buf, (int)buf_size, MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }
    else {
        /* send a failed message to the client */
        if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                                   H5MD_RETURN_TAG, MPI_COMM_WORLD))
            HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
    }

    H5MM_xfree(offsets);
    if(idx_info.dxpl_id && idx_info.dxpl_id != H5P_DATASET_XFER_DEFAULT && 
       H5I_dec_ref(idx_info.dxpl_id) < 0)
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close plist");
    H5MM_xfree(send_buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MD__chunk_get_addr */

static int
H5D__chunk_iterate_cb(const H5D_chunk_rec_t *chunk_rec, void *op_data)
{
    int source = *((int *)op_data);
    void *send_buf = NULL;
    size_t buf_size = 0; /* send_buf size */
    uint8_t *p = NULL; /* temporary pointer into send_buf for encoding */
    unsigned u;
    int ret_value = H5_ITER_CONT;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* calculate size of buffer needed */
    buf_size = sizeof(int32_t) + sizeof(uint32_t) + sizeof(unsigned) + 
        1 + H5V_limit_enc_size((uint64_t)chunk_rec->chunk_addr);

    for(u=0 ; u<H5O_LAYOUT_NDIMS; u++)
        buf_size += 1 + H5V_limit_enc_size((uint64_t)chunk_rec->offset[u]);

    /* allocate the buffer for encoding the parameters */
    if(NULL == (send_buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    p = (uint8_t *)send_buf;

    /* encode a flag that this message contains a chunk address and not a FAIL or DONE */
    INT32ENCODE(p, H5MD_CONT);

    UINT32ENCODE(p, chunk_rec->nbytes);
    for(u=0 ; u<H5O_LAYOUT_NDIMS; u++)
        UINT64ENCODE_VARLEN(p, chunk_rec->offset[u]);
    H5_ENCODE_UNSIGNED(p, chunk_rec->filter_mask);    
    UINT64ENCODE_VARLEN(p, chunk_rec->chunk_addr);

    /* send the callback data back to the client and recieve the return value */
    if(MPI_SUCCESS != MPI_Sendrecv(send_buf, (int)buf_size, MPI_BYTE, source, H5MD_RETURN_TAG,
                                   &ret_value, sizeof(int), MPI_BYTE, source, H5MD_LISTEN_TAG,
                                   MPI_COMM_WORLD, MPI_STATUS_IGNORE))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to communicate with client");

    H5MM_xfree(send_buf);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
* Function:	H5MD__chunk_iterate
*------------------------------------------------------------------------- */
static herr_t
H5MD__chunk_iterate(uint8_t *p, int source)
{
    hid_t dset_id; /* metadata file ID */
    H5D_t *dset = NULL;
    H5D_chk_idx_info_t idx_info;
    size_t dxpl_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    /* the metadata file id */
    INT32DECODE(p, dset_id);

    /* get the dataset object */
    if(NULL == (dset = (H5D_t *)H5I_object_verify(dset_id, H5I_DATASET)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid dataset identifier");

    /* Compose chunked index info struct */
    idx_info.f = dset->oloc.file;
    idx_info.pline = &dset->shared->dcpl_cache.pline;
    idx_info.layout = &dset->shared->layout.u.chunk;
    idx_info.storage = &dset->shared->layout.storage.u.chunk;

    /* decode the index address */
    UINT64DECODE_VARLEN(p, idx_info.storage->idx_addr);

    /* decode the dxpl size */
    UINT64DECODE_VARLEN(p, dxpl_size);
    /* decode property lists if they are not default*/
    if(dxpl_size) {
        if((idx_info.dxpl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += dxpl_size;
    }
    else
        idx_info.dxpl_id = H5P_DATASET_XFER_DEFAULT;

    /* apply cache tag to dxpl */
    if(H5AC_tag(idx_info.dxpl_id, dset->oloc.addr, NULL) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "unable to apply metadata tag");

    /* iterate */
    if((dset->shared->layout.storage.u.chunk.ops->iterate)(&idx_info, H5D__chunk_iterate_cb, 
                                                           &source) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "chunk iteration failed");

done:
    /* send a failed message to the client */
    if(MPI_SUCCESS != MPI_Send(&ret_value, sizeof(herr_t), MPI_BYTE, source, 
                               H5MD_RETURN_TAG, MPI_COMM_WORLD))
        HDONE_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

    if(idx_info.dxpl_id && idx_info.dxpl_id != H5P_DATASET_XFER_DEFAULT && 
       H5I_dec_ref(idx_info.dxpl_id) < 0)
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close plist");

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MD__chunk_iterate */

/*
 * Just a temporary routine to create a var_args to pass through the native MDS get routine
 */
static herr_t
H5MD__temp_attr_get(void *obj, H5VL_attr_get_t get_type, hid_t req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    va_start (arguments, req);
    if(H5VL_native_attr_get(obj, get_type, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end (arguments);
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MD__temp_attr_get() */

/*
 * Just a temporary routine to create a var_args to pass through the native MDS get routine
 */
static herr_t
H5MD__temp_group_get(void *obj, H5VL_group_get_t get_type, hid_t req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    va_start (arguments, req);
    if(H5VL_native_group_get(obj, get_type, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end (arguments);
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MD__temp_group_get() */

/*
 * Just a temporary routine to create a var_args to pass through the native MDS get routine
 */
static herr_t
H5MD__temp_link_get(void *obj, H5VL_loc_params_t loc_params, H5VL_link_get_t get_type, hid_t req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    va_start (arguments, req);
    if(H5VL_native_link_get(obj, loc_params, get_type, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end (arguments);
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MD__temp_link_get() */

/*
 * Just a temporary routine to create a var_args to pass through the native MDS file misc routine
 */
static herr_t
H5MD__temp_file_misc(void *obj, H5VL_file_misc_t misc_type, hid_t req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    va_start (arguments, req);
    if(H5VL_native_file_misc(obj, misc_type, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end (arguments);
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MD__temp_file_misc() */

/*
 * Just a temporary routine to create a var_args to pass through the native MDS file optional routine
 */
static herr_t
H5MD__temp_file_optional(void *obj, H5VL_file_optional_t optional_type, hid_t req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    va_start (arguments, req);
    if(H5VL_native_file_optional(obj, optional_type, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end (arguments);
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MD__temp_file_optional() */

/*
 * Just a temporary routine to create a var_args to pass through the native MDS object misc routine
 */
static herr_t
H5MD__temp_object_misc(void *obj, H5VL_loc_params_t loc_params, H5VL_object_misc_t misc_type, 
                       hid_t req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    va_start (arguments, req);
    if(H5VL_native_object_misc(obj, loc_params, misc_type, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end (arguments);
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MD__temp_object_misc() */

/*
 * Just a temporary routine to create a var_args to pass through the native MDS object get routine
 */
static herr_t
H5MD__temp_object_get(void *obj, H5VL_loc_params_t loc_params, H5VL_object_get_t get_type, 
                      hid_t req, ...)
{
    va_list           arguments;             /* argument list passed from the API call */
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    va_start (arguments, req);
    if(H5VL_native_object_get(obj, loc_params, get_type, req, arguments) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "get failed")
    va_end (arguments);
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MD__temp_object_get() */

static herr_t 
H5MD_multi_query(const H5FD_t *_f, unsigned long *flags /* out */)
{
    /* Shut compiler up */
    _f=_f;

    /* Set the VFL feature flags that this driver supports */
    if(flags) {
        *flags = 0;
        *flags |= H5FD_FEAT_ALLOCATE_EARLY;
    } /* end if */

    return(0);
} /* end H5MD_multi_query() */

#endif /* H5_HAVE_PARALLEL */
