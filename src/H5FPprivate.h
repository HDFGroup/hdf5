/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
#ifndef H5FPPRIVATE_H__
#define H5FPPRIVATE_H__ 0

#include "H5FPpublic.h"         /* Flexible Parallel HDF5               */
#include "H5Rprivate.h"         /* References                           */

#define H5FP_BYTE_BITS   8

/* Different types of requests */

/*
 * The H5FP_REQ_LOCK_END and H5FP_REQ_RELEASE_END are used to lock and
 * release a collection of objects at the same time. The last object you
 * need to lock is sent with the H5FP_REQ_LOCK_END request type (this can
 * also be a null message - where you're just sending a message that has
 * H5FP_REQ_LOCK_END as the request type with no data associated with it.
 * In that case, the md_len MUST be set to 0). The SAP will then try to
 * lock all of the objects you've requested to lock at once. If it cannot
 * do so, then it will fail and you have to try again at a later time.
 *
 * Releasing locks is done in the exact same way, except that the action
 * will always release locks (i.e., not fail) if there is a vaild lock
 * for that object.
 */
enum sap_request {
    H5FP_REQ_OPEN,              /* Open a file (or eventually an object) */
    H5FP_REQ_LOCK,              /* Lock an object (in a sequence) */
    H5FP_REQ_LOCK_END,          /* Last lock request in lock sequence */
    H5FP_REQ_RELEASE,           /* Unlock an object (in a sequence) */
    H5FP_REQ_RELEASE_END,       /* Last unlock request in unlock sequence */
    H5FP_REQ_CHANGE,            /* Change an object */
    H5FP_REQ_SYNC,              /* Syncronize changes in file */
    H5FP_REQ_CLOSE,             /* Close a file (or eventually an object) */
    H5FP_REQ_STOP               /* Stop SAP */
};

/* Actions to take when performing a change */
enum sap_action {
    H5FP_ACT_CREATE,
    H5FP_ACT_EXTEND,
    H5FP_ACT_DELETE
};

/* Types of objects we can change */
enum sap_obj_type {
    H5FP_OBJ_FILE,
    H5FP_OBJ_GROUP,
    H5FP_OBJ_DATASET,
    H5FP_OBJ_DATATYPE,
    H5FP_OBJ_ATTRIBUTE
};

/* Types of locks we can get */
enum sap_lock_type {
    H5FP_LOCK_READ,
    H5FP_LOCK_WRITE
};

/* The structure sent to the SAP which holds all of the requested action */
struct SAP_request {
    enum sap_request req_type;  /* Request type                             */
    unsigned int req_id;        /* ID for request set by sending process    */
    unsigned int proc_rank;     /* Rank of sending process                  */
    unsigned int sap_file_id;   /* SAP's file ID for the specific file      */
    int md_len;                 /* Length of the metadata sent in next msg  */
    enum sap_obj_type obj_type; /* Type of object                           */
    enum sap_action action;     /* Action to do to object (H5FP_REQ_CHANGE only) */
    enum sap_lock_type rw_lock; /* Indicates read or write lock             */
    unsigned char oid[H5R_OBJ_REF_BUF_SIZE]; /* Buffer to store OID of object referenced */
};

extern MPI_Datatype SAP_request_t;  /* MPI datatype for the SAP_request obj */

/* The status of the SAP */
enum sap_status {
    H5FP_STATUS_OK,

    /* For locking */
    H5FP_STATUS_LOCK_ACQUIRED,
    H5FP_STATUS_LOCK_FAILED,

    /* For releasing locks */
    H5FP_STATUS_LOCK_RELEASED,
    H5FP_STATUS_LOCK_RELEASE_FAILED,
    H5FP_STATUS_BAD_LOCK,       /* Process doesn't own a lock on the OID */

    /* For change requests */
    H5FP_STATUS_FILE_CLOSING,
    H5FP_STATUS_NO_LOCK,

    /* Out of memory error */
    H5FP_STATUS_OOM,

    /* Bad file ID */
    H5FP_STATUS_BAD_FILE_ID,

    /* Reserved for completely disasterous failures which require an abort */
    H5FP_STATUS_CATASTROPHIC
};

/* Reply from the SAP on an SAP_request send */
struct SAP_reply {
    unsigned int req_id;        /* Request ID copied from the SAP_request   */
    unsigned int sap_file_id;   /* File ID assigned to an open file         */
    enum sap_status status;     /* Status of the request                    */
};

extern MPI_Datatype SAP_reply_t;    /* MPI datatype for the SAP_reply obj   */

/* The sync message from the SAP on an SAP_request H5FP_REQ_SYNC send */
struct SAP_sync {
    unsigned int req_id;        /* Request ID copied from the SAP_request   */
    unsigned int sync_id;       /* Sync ID to order the sync messages       */
    unsigned int sap_file_id;   /* SAP's file ID for the specific file      */
    unsigned int last_msg;      /* Indicates this is the last sync msg sent */
    int md_len;                 /* Length of the metadata sent in next msg  */
    enum sap_obj_type obj_type; /* Type of object                           */
    enum sap_action action;     /* Action done on the object                */
    enum sap_status status;     /* Status of the request                    */
};

extern MPI_Datatype SAP_sync_t; /* MPI datatype for the SAP_sync obj    */

/*
 * Special tag numbers for requests, replies, and string passing messages.
 *
 * Certain actions (Open, Change, and Close) require a pathname to the
 * object. This pathname is sent in a separate message and the SAP will
 * search for it after getting the appropriate request.
 */
enum {
    H5FP_TAG_REQUEST,
    H5FP_TAG_REPLY,
    H5FP_TAG_SYNC,
    H5FP_TAG_METADATA,
    H5FP_TAG_FILE_ID
};

/* Handy #define for copying OIDs */
#define H5FP_COPY_OID(dst, src)     HDmemcpy((dst), (src), H5R_OBJ_REF_BUF_SIZE)

/* SAP specific variables */
extern MPI_Comm H5FP_SAP_COMM;  /* Comm we use: Supplied by user            */
extern MPI_Comm H5FP_SAP_BARRIER_COMM; /* Comm if you want to do a barrier  */

extern unsigned H5FP_sap_rank;  /* The rank of the SAP: Supplied by user    */
extern unsigned H5FP_capt_rank; /* The rank which tells SAP of opens        */
extern unsigned H5FP_my_rank;   /* Rank of this process in the COMM         */
extern int H5FP_comm_size;      /* Size of the COMM                         */

#ifdef __cplusplus
extern "C" {
#endif  /* __cplusplus */

/* NOTE: Don't use this function explicitly!! */
extern herr_t H5FP_send_metadata(const char *mdata, int len, int rank);

/* Start the SAP */
extern herr_t H5FP_sap_receive_loop(void);

/* Use these functions to communicate with the SAP */
extern herr_t H5FP_request_open(const char *mdata, int md_len, enum sap_obj_type obj_type,
                                unsigned *file_id, unsigned *req_id);
extern herr_t H5FP_request_lock(unsigned int sap_file_id, unsigned char *mdata,
                                enum sap_lock_type rw_lock, int last, unsigned *req_id,
                                enum sap_status *status);
extern herr_t H5FP_request_release_lock(unsigned int sap_file_id, unsigned char *mdata,
                                        int last, unsigned *req_id, enum sap_status *status);
extern herr_t H5FP_request_change(unsigned int sap_file_id, enum sap_obj_type obj_type,
                                  enum sap_action action, int mdata_len, const char *mdata,
                                  unsigned *req_id);
extern herr_t H5FP_request_sync(unsigned int sap_file_id, hid_t hdf_file_id,
                                unsigned *req_id, enum sap_status *status);
extern herr_t H5FP_request_close(unsigned sap_file_id, unsigned *req_id);

#ifdef __cplusplus
}
#endif  /* __cplusplus */

#endif  /* H5FPPRIVATE_H__ */
