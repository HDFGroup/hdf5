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
#define H5L_PACKAGE		/*suppress error about including H5Lpkg   */
#define H5O_PACKAGE		/*suppress error about including H5Opkg	  */
#define H5P_PACKAGE		/*suppress error about including H5Ppkg	  */
#define H5R_PACKAGE		/*suppress error about including H5Rpkg	  */
#define H5T_PACKAGE		/*suppress error about including H5Tpkg	  */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5P_init_mdserver_interface


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
#include "H5FDmds.h"            /* MDS file driver      		*/
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5HGprivate.h"	/* Global Heaps				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Lprivate.h"         /* links                                */
#include "H5Lpkg.h"             /* links headers			*/
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Opkg.h"             /* Object headers			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5Ppkg.h"		/* Property lists		  	*/
#include "H5Rpkg.h"		/* References   			*/
#include "H5SMprivate.h"	/* Shared Object Header Messages	*/
#include "H5Tpkg.h"		/* Datatypes				*/
#include "H5Tprivate.h"		/* Datatypes				*/
#include "H5VLprivate.h"	/* VOL plugins				*/
#include "H5VLmdserver.h"       /* MDS helper routines			*/

/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/


/********************/
/* Local Prototypes */
/********************/


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*--------------------------------------------------------------------------
NAME
   H5P_init_encdec_interface -- Initialize interface-specific information
USAGE
    herr_t H5P_init_encdec_interface()
RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5P_init() currently).

--------------------------------------------------------------------------*/
static herr_t
H5P_init_mdserver_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(H5P_init())
} /* H5P_init_encdec_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5VLmds_start
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
H5VL_mds_start(void)
{
    
    MPI_Status status;
    int incoming_msg_size;
    void *recv_buf = NULL;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* turn off commsplitter to talk to the other processes */
    MPI_Pcontrol(0);

    while(1) {
        printf("MDS Process Waiting\n");
        /* probe for a message from a client */
        if(MPI_SUCCESS != MPI_Probe(MPI_ANY_SOURCE, H5VL_MDS_LISTEN_TAG, MPI_COMM_WORLD, &status))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to probe for a message")
        /* get the incoming message size from the probe result */
        if(MPI_SUCCESS != MPI_Get_count(&status, MPI_BYTE, &incoming_msg_size))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to get incoming message size")

        /* allocate the receive buffer */
        recv_buf = H5MM_malloc (incoming_msg_size);

        /* receive the actual message */
        if(MPI_SUCCESS != MPI_Recv (recv_buf, incoming_msg_size, MPI_BYTE, status.MPI_SOURCE, 
                                    H5VL_MDS_LISTEN_TAG, MPI_COMM_WORLD, &status))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to receivemessage")

        /* decode the buffer and perform the requested operation */        
        if((ret_value = H5VL_mds_perform_op(recv_buf, status.MPI_SOURCE)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to decode buffer and execute operation")

        if (NULL != recv_buf)
            H5MM_free(recv_buf);
    }

done:
    if (NULL != recv_buf)
        H5MM_free(recv_buf);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VLmds_start() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_perform_op
 *
 * Purpose:	MDS process executing an operation requested by clients that is 
 *              encoded in buf
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              August, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_mds_perform_op(const void *buf, int source)
{
    const uint8_t *p = (const uint8_t *)buf;     /* Current pointer into buffer */
    H5VL_mds_op_type_t op_type;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Sanity check */
    if(NULL == p)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "decode buffer is NULL")

    /* Get the operation type */
    op_type = (H5VL_mds_op_type_t)*p++;

    switch(op_type) {
        case H5VL_MDS_FILE_CREATE:
            {
                char *name = NULL; /* name of HDF5 container (decoded) */
                size_t len = 0; /* length of name (decoded) */
                char *mds_filename = NULL; /* name of the metadata file (generated from name) */
                unsigned flags; /* access flags */
                hbool_t fcpl_encoded, fapl_encoded; /* flag to indicate whether plists are encoded (non-default) or not encoded (default) */
                hid_t fcpl_id = FAIL, fapl_id = FAIL; /* plist IDs */
                hid_t temp_fapl; /* fapl used for underlying MDS VFD */
                H5F_t *new_file = NULL; /* struct for MDS file */
                hid_t file_id; /* ID of MDS file */

                /* decode length of name and name */
                UINT64DECODE_VARLEN(p, len);

                name = H5MM_xstrdup((const char *)(p));
                p += len;

                /* generate the MDS file name by adding a .md extension to the file name */
                mds_filename = (char *)H5MM_malloc (sizeof(char) * (len + 3));
                sprintf(mds_filename, "%s.md", name);
                printf("file name  %d  %s  %s\n", len, name, mds_filename);

                /* deocde create flags */
                H5_DECODE_UNSIGNED(p, flags);

                /* decode a flag to indicate whether the property lists are default or not & 
                 * decode property lists if they are not default*/
                fcpl_encoded = (hbool_t)*p++;
                if(fcpl_encoded) {
                    if((fcpl_id = H5P__decode(p)) < 0)
                        HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
                }
                else {
                    fcpl_id = H5P_FILE_CREATE_DEFAULT;
                }
                printf("%p\n",p);
                fapl_encoded = (hbool_t)*p++;
                if(fapl_encoded) {
                    if((fapl_id = H5P__decode(p)) < 0)
                        HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
                }
                else {
                    fapl_id = H5P_FILE_ACCESS_DEFAULT;
                }

                printf("%s %d %d %d\n", name, flags, fcpl_id, fapl_id);
                /* set the underlying MDS VFD */
                temp_fapl = H5Pcreate(H5P_FILE_ACCESS);
                if(H5P_set_fapl_mds(fapl_id, name, temp_fapl) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, NULL, "failed to set MDS plist")

                /* create the metadata file locally */
                if(NULL == (new_file = H5F_open(mds_filename, flags, fcpl_id, fapl_id, H5AC_dxpl_id)))
                    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to create file")

                new_file->id_exists = TRUE;

                if((file_id = H5I_register(H5I_FILE, new_file, FALSE)) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to create file");

                /* Send the meta data file to the client */
                if(MPI_SUCCESS != MPI_Send(&file_id, sizeof(hid_t), MPI_BYTE, source, 
                                           H5VL_MDS_SEND_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                H5MM_xfree(name);
                H5MM_xfree(mds_filename);
                break;
            }
        case H5VL_MDS_DSET_CREATE:
            {
                hid_t obj_id; /* id of location for dataset */
                hid_t dset_id = FAIL; /* dset id */
                H5D_t *dset = NULL; /* New dataset's info */
                H5VL_loc_params_t loc_params; /* location parameters for obj_id */
                H5G_loc_t loc; /* Object location to insert dataset into */
                char *name = NULL; /* name of dataset (if named) */
                size_t len = 0; /* len of dataset name */
                hbool_t dcpl_encoded, dapl_encoded, lcpl_encoded; /* flag to indicate whether plists are encoded (non-default) or not encoded (default) */
                hid_t dcpl_id = FAIL, dapl_id = FAIL, lcpl_id = FAIL; /* plist IDs */
                hid_t type_id; /* datatype for dataset */
                hid_t space_id; /* dataspace for dataset */
                const H5S_t *space; /* Dataspace for dataset */

                /* decode the object id */
                INT32DECODE(p, obj_id);

                /* decode the location parameters */
                if((ret_value = H5VL__decode_loc_params(&p, &loc_params)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");

                /* decode length of the dataset name and the actual dataset name */
                UINT64DECODE_VARLEN(p, len);
                if(len) {
                    name = H5MM_xstrdup((const char *)(p));
                    p += len;
                }
                printf("dataset name  %d  %s\n", len, name);

                /* decode a flag to indicate whether the property lists are default or not & 
                 * decode property lists if they are not default*/
                dcpl_encoded = (hbool_t)*p++;
                if(dcpl_encoded) {
                    if((dcpl_id = H5Pdecode((const void *)p)) < 0)
                        HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
                }
                else
                    dcpl_id = H5P_DEFAULT;

                dapl_encoded = (hbool_t)*p++;
                if(dapl_encoded) {
                    if((dapl_id = H5Pdecode((const void *)p)) < 0)
                        HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
                }
                else
                    dapl_id = H5P_DEFAULT;

                lcpl_encoded = (hbool_t)*p++;
                if(lcpl_encoded) {
                    if((lcpl_id = H5Pdecode((const void *)p)) < 0)
                        HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
                }
                else
                    lcpl_id = H5P_DEFAULT;

                if((type_id = H5Tdecode((const void *)p)) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, FAIL, "unable to decode datatype");

                if((space_id = H5Sdecode((const void *)p)) < 0)
                    HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDECODE, FAIL, "unable to decode dataspace");

                /* Check dataset create parameters */
                if(H5G_loc(obj_id, &loc) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object");
                if(H5I_DATATYPE != H5I_get_type(type_id))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype ID");
                if(NULL == (space = (const H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace ID");

                /* H5Dcreate_anon */
                if(NULL == name) {
                    /* build and open the new dataset */
                    if(NULL == (dset = H5D__create(loc.oloc->file, type_id, space, dcpl_id, dapl_id, 
                                                   H5AC_dxpl_id)))
                        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create dataset");
                }
                /* H5Dcreate2 */
                else {
                    /* Create the new dataset & get its ID */
                    if(NULL == (dset = H5D__create_named(&loc, name, type_id, space, lcpl_id, 
                                                         dcpl_id, dapl_id, H5AC_dxpl_id)))
                        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create dataset");
                }

                if((dset_id = H5I_register(H5I_DATASET, dset, FALSE)) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENFILE, FAIL, "unable to create dataset");

                /* Send the dataset id to the client */
                if(MPI_SUCCESS != MPI_Send(&dset_id, sizeof(hid_t), MPI_BYTE, source, 
                                           H5VL_MDS_SEND_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                H5MM_xfree(name);
                break;
            }
        case H5VL_MDS_ALLOC:
            {
                hid_t file_id; /* metadata file ID */
                H5F_t *file = NULL; /* metadata file struct */
                H5FD_t *fd = NULL; /* metadata file driver struct */
                hid_t dxpl_id; /* encoded dxpl */
                hbool_t dxpl_encoded; /* flag to indicate whether plists are encoded (non-default) or not encoded (default) */
                H5FD_mem_t type; /* Memory VFD type to indicate metadata or raw data */
                hsize_t size; /* requested size to allocate from the EOA */
                hsize_t orig_size; /* Original allocation size */
                haddr_t eoa; /* Address of end-of-allocated space */
                hsize_t extra; /* Extra space to allocate, to align request */
                haddr_t return_addr = HADDR_UNDEF;

                /* the metadata file id */
                INT32DECODE(p, file_id);
                /* the memory VFD type */
                type = (H5FD_mem_t)*p++;

                /* decode a flag to indicate whether the property lists are default or not & 
                 * decode property lists if they are not default*/
                dxpl_encoded = (hbool_t)*p++;
                if(dxpl_encoded) {
                    if((dxpl_id = H5Pdecode((const void *)p)) < 0)
                        HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
                }
                else
                    dxpl_id = H5P_DEFAULT;

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
                if(H5F_addr_overflow(eoa, size) || (eoa + size) > fd->maxaddr) {
                    /* Send undefined haddr to the client */
                    if(MPI_SUCCESS != MPI_Send(&return_addr, sizeof(uint64_t), MPI_UINT64_T, source, 
                                               H5VL_MDS_SEND_TAG, MPI_COMM_WORLD))
                        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
                    HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, FAIL, "file allocation request failed")
                }
                /* Set the [possibly aligned] address to return */
                return_addr = eoa + extra;

                /* Extend the end-of-allocated space address */
                eoa += size;
                if(fd->cls->set_eoa(fd, type, eoa) < 0) {
                    return_addr = HADDR_UNDEF;
                    /* Send undefined haddr to the client */
                    if(MPI_SUCCESS != MPI_Send(&return_addr, sizeof(uint64_t), MPI_UINT64_T, source, 
                                               H5VL_MDS_SEND_TAG, MPI_COMM_WORLD))
                        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
                    HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, FAIL, "file allocation request failed")
                }

                /* Send the haddr to the client */
                if(MPI_SUCCESS != MPI_Send(&return_addr, sizeof(uint64_t), MPI_UINT64_T, source, 
                                           H5VL_MDS_SEND_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
            } /* end H5VL_MDS_ALLOC */
        case H5VL_MDS_GET_EOA:
            {
                hid_t file_id; /* metadata file ID */
                H5F_t *file = NULL; /* metadata file struct */
                H5FD_t *fd = NULL; /* metadata file driver struct */
                H5FD_mem_t type; /* Memory VFD type to indicate metadata or raw data */
                haddr_t eoa; /* Address of end-of-allocated space */

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

                /* Send the haddr to the client */
                if(MPI_SUCCESS != MPI_Send(&eoa, sizeof(uint64_t), MPI_UINT64_T, source, 
                                           H5VL_MDS_SEND_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
            } /* end H5VL_MDS_GET_EOA */
        case H5VL_MDS_SET_EOA:
            {
                hid_t file_id; /* metadata file ID */
                H5F_t *file = NULL; /* metadata file struct */
                H5FD_t *fd = NULL; /* metadata file driver struct */
                H5FD_mem_t type; /* Memory VFD type to indicate metadata or raw data */
                haddr_t eoa; /* Address of end-of-allocated space */
                herr_t ret = SUCCEED;

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
                ret = fd->cls->set_eoa(fd, type, eoa);

                /* Send the haddr to the client */
                if(MPI_SUCCESS != MPI_Send(&ret, sizeof(int), MPI_INT, source, 
                                           H5VL_MDS_SEND_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
            } /* end H5VL_MDS_SET_EOA */
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTDECODE, FAIL, "invalid operation type to decode");
    } /* end switch */    

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_perform_op() */


/*-------------------------------------------------------------------------
 * Function:	H5VL_mds_encode
 *
 * Purpose:	encode a set of parameters into a binary buffer to send to
 *              the MDS process.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              August, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL_mds_encode(H5VL_mds_op_type_t request_type, void *buf, size_t *size, ...)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    va_list arguments;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    va_start (arguments, size);

    switch(request_type) {
        case H5VL_MDS_FILE_CREATE:
            {
                char *name = va_arg (arguments, char *);
                unsigned flags = va_arg (arguments, unsigned);
                hid_t fcpl_id = va_arg (arguments, hid_t);
                hid_t fapl_id = va_arg (arguments, hid_t);
                size_t len = 0, fcpl_size = 0, fapl_size = 0;
                H5P_genplist_t *fcpl, *fapl;

                /* check plists */
                if(NULL == (fcpl = (H5P_genplist_t *)H5I_object_verify(fcpl_id, H5I_GENPROP_LST)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
                if(NULL == (fapl = (H5P_genplist_t *)H5I_object_verify(fapl_id, H5I_GENPROP_LST)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

                /* get property list sizes */
                if(H5P_DEFAULT != fcpl_id)
                    if((ret_value = H5P__encode(fcpl, FALSE, NULL, &fcpl_size)) < 0)
                        HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
                if(H5P_DEFAULT != fapl_id)
                    if((ret_value = H5P__encode(fapl, FALSE, NULL, &fapl_size)) < 0)
                        HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");

                len = HDstrlen(name);
                printf("%s %d\n", name, flags);
                if(NULL != p) {
                    /* encode request type */
                    *p++ = (uint8_t)request_type;

                    /* encode length of name and name */
                    UINT64ENCODE_VARLEN(p, len);
                    HDmemcpy(p, (uint8_t *)name, len);
                    p += len;

                    /* encode create flags */
                    H5_ENCODE_UNSIGNED(p, flags);

                    /* encode a flag to indicate whether the property lists are default or not & 
                     * encode property lists if they are not default*/
                    if(H5P_DEFAULT != fcpl_id) {
                        *p++ = (uint8_t)TRUE;
                        if((ret_value = H5P__encode(fcpl, FALSE, (void *)p, &fcpl_size)) < 0)
                            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
                    }
                    else
                        *p++ = (uint8_t)FALSE;

                    if(H5P_DEFAULT != fapl_id) {
                        *p++ = (uint8_t)TRUE;
                        if((ret_value = H5P__encode(fapl, FALSE, (void *)p, &fapl_size)) < 0)
                            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
                    }
                    else
                        *p++ = (uint8_t)FALSE;
                }
                *size += (1 + H5V_limit_enc_size((uint64_t)len) + len + sizeof(unsigned) + 
                          1 + fapl_size + 1 + fcpl_size);

                break;
            }
        case H5VL_MDS_DSET_CREATE:
            {
                hid_t obj_id = va_arg (arguments, hid_t);
                H5VL_loc_params_t loc_params = va_arg (arguments, H5VL_loc_params_t);
                const char *name = va_arg (arguments, const char *);
                hid_t dcpl_id = va_arg (arguments, hid_t);
                hid_t dapl_id = va_arg (arguments, hid_t);
                hid_t type_id = va_arg (arguments, hid_t);
                hid_t space_id = va_arg (arguments, hid_t);
                hid_t lcpl_id = va_arg (arguments, hid_t);
                size_t len = 0, dcpl_size = 0, dapl_size = 0, lcpl_size = 0;
                H5P_genplist_t *dcpl, *dapl, *lcpl;
                size_t type_size = 0, space_size = 0, loc_size = 0;

                /* check plists */
                if(NULL == (dcpl = (H5P_genplist_t *)H5I_object_verify(dcpl_id, H5I_GENPROP_LST)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
                if(NULL == (dapl = (H5P_genplist_t *)H5I_object_verify(dapl_id, H5I_GENPROP_LST)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
                if(NULL == (lcpl = (H5P_genplist_t *)H5I_object_verify(lcpl_id, H5I_GENPROP_LST)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

                /* get size for property lists to encode */
                if(H5P_DEFAULT != dcpl_id)
                    if((ret_value = H5P__encode(dcpl, FALSE, NULL, &dcpl_size)) < 0)
                        HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
                if(H5P_DEFAULT != dapl_id)
                    if((ret_value = H5P__encode(dapl, FALSE, NULL, &dapl_size)) < 0)
                        HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
                if(H5P_DEFAULT != lcpl_id)
                    if((ret_value = H5P__encode(lcpl, FALSE, NULL, &lcpl_size)) < 0)
                        HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");

                /* get Type size to encode */
                if((ret_value = H5Tencode(type_id, NULL, &type_size)) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "unable to encode datatype");

                /* get Dataspace size to encode */
                if((ret_value = H5Sencode(space_id, NULL, &space_size)) < 0)
                    HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "unable to encode dataspace");

                /* get name size to encode */
                if(NULL != name)
                    len = HDstrlen(name);

                /* get loc params size to encode */
                if((ret_value = H5VL__encode_loc_params(loc_params, NULL, &loc_size)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");

                if(NULL != p) {
                    /* encode request type */
                    *p++ = (uint8_t)request_type;

                    /* encode the object id */
                    INT32ENCODE(p, obj_id);

                    /* encode the location parameters */
                    if((ret_value = H5VL__encode_loc_params(loc_params, &p, &loc_size)) < 0)
                        HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");                    

                    /* encode length of the dataset name and the actual dataset name */
                    UINT64ENCODE_VARLEN(p, len);
                    if(NULL != name)
                        HDmemcpy(p, (uint8_t *)name, len);
                    *p += len;

                    /* encode a flag to indicate whether the property lists are default or not & 
                     * encode property lists if they are not default*/
                    if(H5P_DEFAULT != dcpl_id) {
                        *p++ = (uint8_t)TRUE;
                        if((ret_value = H5P__encode(dcpl, FALSE, (void *)p, &dcpl_size)) < 0)
                            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
                    }
                    else
                        *p++ = (uint8_t)FALSE;

                    if(H5P_DEFAULT != dapl_id) {
                        *p++ = (uint8_t)TRUE;
                        if((ret_value = H5P__encode(dapl, FALSE, (void *)p, &dapl_size)) < 0)
                            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
                    }
                    else
                        *p++ = (uint8_t)FALSE;

                    if(NULL!= name && H5P_DEFAULT != lcpl_id) {
                        *p++ = (uint8_t)TRUE;
                        if((ret_value = H5P__encode(lcpl, FALSE, (void *)p, &lcpl_size)) < 0)
                            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
                    }
                    else
                        *p++ = (uint8_t)FALSE;

                    /* encode datatype */
                    if((ret_value = H5Tencode(type_id, (void *)p, &type_size)) < 0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "unable to encode datatype");

                    /* encode datatspace */
                    if((ret_value = H5Sencode(space_id, (void *)p, &space_size)) < 0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "unable to encode datatspace");

                }
                *size += (1 + sizeof(int32_t) + loc_size + H5V_limit_enc_size((uint64_t)len) + len + 
                          1 + dapl_size + 1 + dcpl_size + 1 + lcpl_size + type_size + space_size);
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "invalid operation type to encode");
    } /* end switch */
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_mds_encode() */
