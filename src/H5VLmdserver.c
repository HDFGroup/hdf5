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
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5HGprivate.h"	/* Global Heaps				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Lprivate.h"         /* links                                */
#include "H5Lpkg.h"             /* links headers			*/
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Opkg.h"             /* Object headers			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5Rpkg.h"		/* References   			*/
#include "H5SMprivate.h"	/* Shared Object Header Messages	*/
#include "H5Tpkg.h"		/* Datatypes				*/
#include "H5Tprivate.h"		/* Datatypes				*/
#include "H5VLprivate.h"	/* VOL plugins				*/
#include "H5VLmds.h"            /* MDS VOL plugin			*/
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

    FUNC_ENTER_API(FAIL)

    /* turn off commsplitter to talk to the other processes */
    MPI_Pcontrol(0);

    while(1) {
        /* probe for a message from a client */
        if(MPI_SUCCESS != MPI_probe(MPI_ANY_SOURCE, H5VL_MDS_LISTEN_TAG, MPI_COMM_WORLD, &status))
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
        if((ret_value = H5VL_mds_perform_op(recv_buf, incoming_msg_size, status.MPI_SOURCE)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to decode buffer and execute operation")

        if (NULL != recv_buf)
            H5MM_free(recv_buf);
    }

done:
    if (NULL != recv_buf)
        H5MM_free(recv_buf);
    FUNC_LEAVE_API(ret_value)
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
H5VL_mds_perform_op(const void *buf, size_t buf_size, int source)
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
                char *name = NULL;
                char *mds_filename = NULL;
                unsigned flags;
                hbool_t fcpl_encoded, fapl_encoded;
                hid_t fcpl_id = FAIL, fapl_id = FAIL;
                H5F_t *new_file = NULL;
                hid_t file_id;
                size_t len = 0;

                /* decode length of name and name */
                UINT64DECODE_VARLEN(p, len);
                name = H5MM_xstrdup((const char *)(p));
                *p += len;
                mds_filename = H5MM_alloc (sizeof(char) * (len + 3));
                sprintf(mds_filename, "%s.md", name);
                printf("file name  %d  %s  %s\n", len, name, mds_filename);

                /* deocde create flags */
                H5_DECODE_UNSIGNED(p, flags);

                /* decode a flag to indicate whether the property lists are default or not & 
                 * decode property lists if they are not default*/
                fcpl_encoded = (hbool_t)*p++;
                if(fcpl_encoded) {
                    if((fcpl_id = H5Pdecode((const void *)p)) < 0)
                        HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
                }
                else
                    fcpl_id = H5P_DEFAULT;

                fapl_encoded = (hbool_t)*p++;
                if(fapl_encoded) {
                    if((fapl_id = H5Pdecode((const void *)p)) < 0)
                        HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
                }
                else
                    fapl_id = H5P_FILE_ACCESS_DEFAULT;

                /* set the underlying MDS VFD */
                temp_fapl = H5Pcreate(H5P_FILE_ACCESS);
                if(H5P_fapl_set_mds(temp_fapl, name, fapl_id) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, NULL, "failed to set MDS plist")

                /* create the metadata file locally */
                if(NULL == (new_file = H5F_open(mds_filename, flags, fcpl_id, temp_fapl, H5AC_dxpl_id)))
                    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to create file")

                new_file->id_exists = TRUE;

                if((file_id = H5I_register(new_file, H5I_FILE)) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to create file");

                /* Send the meta data file to the client */
                if(MPI_SUCCESS != MPI_Send(&file_id, sizeof(hid_t), MPI_BYTE, source, 
                                           H5VL_MDS_SEND_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                H5MM_xfree(name);
                H5MM_xfree(mds_filename);
                break;
            }
        case H5VL_MDS_DATASET_CREATE:
            {
                hid_t obj_id, dset_id = FAIL;
                H5VL_loc_params_t loc_params;
                char *name = NULL;
                hbool_t dcpl_encoded, dapl_encoded, lcpl_encoded;
                hid_t dcpl_id = FAIL, dapl_id = FAIL, lcpl_id = FAIL;
                size_t len = 0;
                H5G_loc_t loc; /* Object location to insert dataset into */
                H5D_t *dset = NULL;        /* New dataset's info */
                const H5S_t *space;              /* Dataspace for dataset */

                /* decode the object id */
                INT32DECODE(p, obj_id);

                /* decode the location parameters */
                if((ret_value = H5VL__decode_loc_params(&p, &loc_params)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");

                /* decode length of the dataset name and the actual dataset name */
                UINT64DECODE_VARLEN(p, len);
                if(len) {
                    name = H5MM_xstrdup((char *)(p));
                    *p += len;
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
                    HGOTO_ERROR(H5E_TYPE, H5E_CANTDECODE, FAIL, "unable to decode datatype");

                if((space_id = H5Sdecode((const void *)p)) < 0)
                    HGOTO_ERROR(H5E_SPACE, H5E_CANTDECODE, FAIL, "unable to decode dataspace");

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

                if((dset_id = H5I_register(dset, H5I_DATASET)) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENFILE, FAIL, "unable to create dataset");

                /* Send the dataset id to the client */
                if(MPI_SUCCESS != MPI_Send(&dset_id, sizeof(hid_t), MPI_BYTE, source, 
                                           H5VL_MDS_SEND_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");

                H5MM_xfree(name);
                break;
            }
        case H5VL_MDS_GET_EOA:
            {
                hid_t file_id, dxpl_id;
                hsize_t size;
                H5FD_mem_t type;
                H5F_t *file = NULL;
                H5FD_t *fd = NULL;
                hsize_t orig_size;   /* Original allocation size */
                haddr_t eoa;         /* Address of end-of-allocated space */
                hsize_t extra;       /* Extra space to allocate, to align request */
                haddr_t return_addr;

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
                        extra = file->alignment - mis_align;
                    } /* end if */
                } /* end if */

                /* Add in extra allocation amount */
                size += extra;

                /* Check for overflow when extending */
                if(H5F_addr_overflow(eoa, size) || (eoa + size) > fd->maxaddr)
                    HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, HADDR_UNDEF, "file allocation request failed")

                /* Set the [possibly aligned] address to return */
                return_addr = eoa + extra;

                /* Extend the end-of-allocated space address */
                eoa += size;
                if(file->cls->set_eoa(file, fd, eoa) < 0)
                    HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, HADDR_UNDEF, "file allocation request failed")

                /* Send the dataset id to the client */
                if(MPI_SUCCESS != MPI_Send(&return_addr, sizeof(uint64_t), MPI_UINT64_T, source, 
                                           H5VL_MDS_SEND_TAG, MPI_COMM_WORLD))
                    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "failed to send message");
            }                
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

    va_start (arguments, req);

    switch(request_type) {
        case H5VL_MDS_FILE_CREATE:
            {
                char *name = va_arg (arguments, char *);
                unsigned flags = va_arg (arguments, unsigned);
                hid_t fcpl_id = va_arg (arguments, hid_t);
                hid_t fapl_id = va_arg (arguments, hid_t);
                size_t len = 0, fcpl_size = 0, fapl_size = 0;

                /* get property list sizes */
                if(H5P_DEFAULT != fcpl_id)
                    if((ret_value = H5Pencode(fcpl_id, FALSE, NULL, &fcpl_size)) < 0)
                        HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
                if(H5P_DEFAULT != fapl_id)
                    if((ret_value = H5Pencode(fapl_id, FALSE, NULL, &fapl_size)) < 0)
                        HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");

                len = HDstrlen(name);

                if(NULL != p) {
                    /* encode request type */
                    *p++ = (uint8_t)request_type;

                    /* encode length of name and name */
                    UINT64ENCODE_VARLEN(p, len);
                    HDmemcpy(p, (uint8_t *)name, len);
                    *p += len;

                    /* encode create flags */
                    H5_ENCODE_UNSIGNED(p, flags);

                    /* encode a flag to indicate whether the property lists are default or not & 
                     * encode property lists if they are not default*/
                    if(H5P_DEFAULT != fcpl_id) {
                        *p++ = (uint8_t)TRUE;
                        if((ret_value = H5Pencode(fcpl_id, FALSE, (void *)p, fcpl_size)) < 0)
                            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
                    }
                    else
                        *p++ = (uint8_t)FALSE;

                    if(H5P_DEFAULT != fapl_id) {
                        *p++ = (uint8_t)TRUE;
                        if((ret_value = H5Pencode(fapl_id, FALSE, (void *)p, fapl_size)) < 0)
                            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
                    }
                    else
                        *p++ = (uint8_t)FALSE;
                }
                *size += (1 + H5V_limit_enc_size((uint64_t)len) + sizeof(unsigned) + 
                          len + 1 + fapl_size + 1 + fcpl_size);

                break;
            }
        case H5VL_MDS_DATASET_CREATE:
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
                size_t type_size = 0, space_size = 0;

                /* get size for property lists to encode */
                if(H5P_DEFAULT != dcpl_id)
                    if((ret_value = H5Pencode(dcpl_id, FALSE, NULL, &dcpl_size)) < 0)
                        HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
                if(H5P_DEFAULT != dapl_id)
                    if((ret_value = H5Pencode(dapl_id, FALSE, NULL, &dapl_size)) < 0)
                        HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
                if(H5P_DEFAULT != lcpl_id)
                    if((ret_value = H5Pencode(lcpl_id, FALSE, NULL, &lcpl_size)) < 0)
                        HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");

                /* get Type size to encode */
                if((ret_value = H5Tencode(type_id, NULL, &type_size)) < 0)
                    HGOTO_ERROR(H5E_TYPE, H5E_CANTENCODE, FAIL, "unable to encode datatype");

                /* get Dataspace size to encode */
                if((ret_value = H5Sencode(space_id, NULL, &space_size)) < 0)
                    HGOTO_ERROR(H5E_SPACE, H5E_CANTENCODE, FAIL, "unable to encode dataspace");

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
                        if((ret_value = H5Pencode(dcpl_id, FALSE, (void *)p, dcpl_size)) < 0)
                            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
                    }
                    else
                        *p++ = (uint8_t)FALSE;

                    if(H5P_DEFAULT != dapl_id) {
                        *p++ = (uint8_t)TRUE;
                        if((ret_value = H5Pencode(dapl_id, FALSE, (void *)p, dapl_size)) < 0)
                            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
                    }
                    else
                        *p++ = (uint8_t)FALSE;

                    if(NULL!= name && H5P_DEFAULT != lcpl_id) {
                        *p++ = (uint8_t)TRUE;
                        if((ret_value = H5Pencode(lcpl_id, FALSE, (void *)p, lcpl_size)) < 0)
                            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
                    }
                    else
                        *p++ = (uint8_t)FALSE;

                    /* encode datatype */
                    if((ret_value = H5Tencode(type_id, (void *)p, &type_size)) < 0)
                        HGOTO_ERROR(H5E_TYPE, H5E_CANTENCODE, FAIL, "unable to encode datatype");

                    /* encode datatspace */
                    if((ret_value = H5Sencode(space_id, (void *)p, &space_size)) < 0)
                        HGOTO_ERROR(H5E_SPACE, H5E_CANTENCODE, FAIL, "unable to encode datatspace");

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
