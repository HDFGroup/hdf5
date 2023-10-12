/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Remote writer process for the mirror (socket) VFD.
 *
 * Writer is started with arguments for slaved port.
 * Awaits a connection on the socket.
 * Handles instructions from the master 'Driver' process.
 *
 * Current implementation uses Sec2 as the underlying driver when opening a
 * file. This is reflected in the source (H5FDmirror.c) of the Mirror driver.
 */

#include "mirror_remote.h"

#ifdef H5_HAVE_MIRROR_VFD

#define HEXDUMP_XMITS 1     /* Toggle whether to print xmit bytes-blob */
                            /* in detailed logging  */
#define HEXDUMP_WRITEDATA 0 /* Toggle whether to print bytes to write */
                            /* in detailed logging  */
#define LISTENQ 80          /* max pending Driver requests */

#define MW_SESSION_MAGIC   0x88F36B32u
#define MW_SOCK_COMM_MAGIC 0xDF10A157u
#define MW_OPTS_MAGIC      0x3BA8B462u

/* ---------------------------------------------------------------------------
 * Structure: struct mirror_session
 *
 * Bundle of information used to manage the operation of this remote Writer
 * in a "session" with the Driver process.
 *
 * magic (uint32_t)
 *       Semi-unique "magic" number used to sanity-check a structure for
 *       validity. MUST equal MW_SESSION_MAGIC to be valid.
 *
 * sockfd (int)
 *       File descriptor to the socket.
 *       Used for receiving bytes from and writing bytes to the Driver
 *       across the network.
 *       If not NULL, should be a valid descriptor.
 *
 * token (uint32t)
 *      Number used to help sanity-check received transmission from the Writer.
 *      Each Driver/Writer pairing should have a semi-unique "token" to help
 *      guard against commands from the wrong entity.
 *
 * xmit_count (uint32_t)
 *      Record of transmissions received from the Driver. While the transmission
 *      protocol should be trustworthy, this serves as an additional guard.
 *      Starts a 0 and should be incremented for each one-way transmission.
 *
 * file (H5FD_t *)
 *      Virtual File handle for the hdf5 file.
 *      Set on file open if H5Fopen() is successful. If NULL, it is invalid.
 *
 * log_verbosity (unsigned int)
 *      The verbosity level for logging. Should be set to one of the values
 *      defined at the top of this file.
 *
 * log_stream (FILE *)
 *      File pointer to which logging output is written. Starts (and ends)
 *      with a default stream, such as stdout, but can be overridden at
 *      runtime.
 *
 * reply (H5FD_mirror_xmit_reply_t)
 *      Structure space for persistent reply data.
 *      Should be initialized with basic header info (magic, version, op),
 *      then with session info (token, xmit count), and finally with specific
 *      reply info (update xmit_count, status code, and message) before
 *      transmission.
 *
 * ----------------------------------------------------------------------------
 */
struct mirror_session {
    uint32_t                 magic;
    int                      sockfd;
    uint32_t                 token;
    uint32_t                 xmit_count;
    H5FD_t                  *file;
    loginfo_t               *loginfo;
    H5FD_mirror_xmit_reply_t reply;
};

/* ---------------------------------------------------------------------------
 * Structure: struct sock_comm
 *
 * Structure for placing the data read and pre-processed from Driver in an
 * organized fashion. Useful for pre-processing a received xmit.
 *
 * magic (uint32_t)
 *      Semi-unique number to sanity-check structure pointer and validity.
 *      Must equal MW_SOCK_COMM_MAGIC to be valid.
 *
 * recd_die (int)
 *      "Boolean" flag indicating that an explicit shutdown/kill/die command
 *      was received. Potentially useful for debugging and or "manual"
 *      operation of the program.
 *      0 indicates normal operation, non-0 (1) indicates to die.
 *
 * xmit_recd (H5FD_mirror_xmit_t *)
 *      Structure pointer for the "xmit header" as decoded from the raw
 *      binary stream read from the socket.
 *
 * raw (char *)
 *      Pointer to a raw byte array, for storing data as read from the
 *      socket. Bytes buffer is decoded into xmit_t header and derivative
 *      structures.
 *
 * raw_size (size_t)
 *      Give the size of the `raw` buffer.
 *
 * ---------------------------------------------------------------------------
 */
struct sock_comm {
    uint32_t            magic;
    int                 recd_die;
    H5FD_mirror_xmit_t *xmit_recd;
    char               *raw;
    size_t              raw_size;
};

/* ---------------------------------------------------------------------------
 * Structure: struct mirror_writer_opts
 *
 * Container for default values and options as parsed from the command line.
 * Currently rather vestigal, but may be expanded and/or moved to be set by
 * Server and passed around as an argument.
 *
 * magic (uint32_t)
 *      Semi-unique number to sanity-check structure pointer and validity.
 *      Must equal MW_OPTS_MAGIC to be valid.
 *
 * logpath (char *)
 *      String pointer. Allocated at runtime.
 *      Specifies file location for logging output.
 *      May be NULL -- uses default output (e.g., stdout).
 *
 * ----------------------------------------------------------------------------
 */
struct mirror_writer_opts {
    uint32_t magic;
    char    *logpath;
};

static int do_open(struct mirror_session *session, const H5FD_mirror_xmit_open_t *xmit_open);

/* ---------------------------------------------------------------------------
 * Function:    session_init
 *
 * Purpose:     Populate mirror_session structure with default and
 *              options-drived values.
 *
 * Return:      An allocated mirror_session structure pointer on success,
 *              else NULL.
 * ----------------------------------------------------------------------------
 */
static struct mirror_session *
session_init(struct mirror_writer_opts *opts)
{
    struct mirror_session *session = NULL;

    mirror_log(NULL, V_INFO, "session_init()");

    if (NULL == opts || opts->magic != MW_OPTS_MAGIC) {
        mirror_log(NULL, V_ERR, "invalid opts pointer");
        goto error;
    }

    session = (struct mirror_session *)malloc(sizeof(struct mirror_session));
    if (session == NULL) {
        mirror_log(NULL, V_ERR, "can't allocate session structure");
        goto error;
    }

    session->magic      = MW_SESSION_MAGIC;
    session->sockfd     = -1;
    session->xmit_count = 0;
    session->token      = 0;
    session->file       = NULL;

    session->reply.pub.magic         = H5FD_MIRROR_XMIT_MAGIC;
    session->reply.pub.version       = H5FD_MIRROR_XMIT_CURR_VERSION;
    session->reply.pub.op            = H5FD_MIRROR_OP_REPLY;
    session->reply.pub.session_token = 0;
    memset(session->reply.message, 0, H5FD_MIRROR_STATUS_MESSAGE_MAX);

    /* Options-derived population */

    session->loginfo = mirror_log_init(opts->logpath, "W- ", MIRROR_LOG_DEFAULT_VERBOSITY);

    return session;

error:
    if (session) {
        free(session);
    }
    return NULL;
} /* end session_init() */

/* ---------------------------------------------------------------------------
 * Function:    session_stop
 *
 * Purpose:     Stop and clean up a session.
 *              Only do this as part of program termination or aborting startup.
 *
 * Return:      0 on success, or negative sum of errors encountered.
 * ----------------------------------------------------------------------------
 */
static int
session_stop(struct mirror_session *session)
{
    int ret_value = 0;

    assert(session && (session->magic == MW_SESSION_MAGIC));

    mirror_log(session->loginfo, V_INFO, "session_stop()");

    /* Close HDF5 file if it is still open (probably in error) */
    if (session->file) {
        mirror_log(session->loginfo, V_WARN, "HDF5 file still open at cleanup");
        if (H5FDclose(session->file) < 0) {
            mirror_log(session->loginfo, V_ERR, "H5FDclose() during cleanup!");
            ret_value--;
        }
    }

    /* Socket will be closed by parent side of server fork after exit */

    /* Close custom logging stream */
    if (mirror_log_term(session->loginfo) < 0) {
        mirror_log(NULL, V_ERR, "Problem closing logging stream");
        ret_value--;
    }
    session->loginfo = NULL;

    /* Invalidate and release structure */
    session->magic++;
    free(session);

    return ret_value;
} /* end session_stop() */

/* ---------------------------------------------------------------------------
 * Function:    session_start
 *
 * Purpose:     Initiate session, open files.
 *
 * Return:      Success: A valid mirror_session pointer which must later be
 *                       cleaned up with session_stop().
 *              Failure: NULL, after cleaning up after itself.
 * ---------------------------------------------------------------------------
 */
static struct mirror_session *
session_start(int socketfd, const H5FD_mirror_xmit_open_t *xmit_open)
{
    struct mirror_session    *session = NULL;
    struct mirror_writer_opts opts;

    mirror_log(NULL, V_INFO, "session_start()");

    if (false == H5FD_mirror_xmit_is_open(xmit_open)) {
        mirror_log(NULL, V_ERR, "invalid OPEN xmit");
        return NULL;
    }

    opts.magic   = MW_OPTS_MAGIC;
    opts.logpath = NULL;

    session = session_init(&opts);
    if (NULL == session) {
        mirror_log(NULL, V_ERR, "can't instantiate session");
        goto error;
    }

    session->sockfd = socketfd;

    if (do_open(session, xmit_open) < 0) {
        mirror_log(NULL, V_ERR, "unable to open file");
        goto error;
    }

    return session;

error:
    if (session != NULL) {
        if (session_stop(session) < 0) {
            mirror_log(NULL, V_WARN, "Can't abort session init");
        }
        session = NULL;
    }
    return NULL;
}

/* ---------------------------------------------------------------------------
 * Function:    _xmit_reply
 *
 * Purpose:     Common operations to send a reply xmit through the session.
 *
 * Return:      0 on success, -1 if error.
 * ----------------------------------------------------------------------------
 */
static int
_xmit_reply(struct mirror_session *session)
{
    unsigned char             xmit_buf[H5FD_MIRROR_XMIT_REPLY_SIZE];
    H5FD_mirror_xmit_reply_t *reply = &(session->reply);

    assert(session && (session->magic == MW_SESSION_MAGIC));

    mirror_log(session->loginfo, V_ALL, "_xmit_reply()");

    reply->pub.xmit_count = session->xmit_count++;
    if (H5FD_mirror_xmit_encode_reply(xmit_buf, (const H5FD_mirror_xmit_reply_t *)reply) !=
        H5FD_MIRROR_XMIT_REPLY_SIZE) {
        mirror_log(session->loginfo, V_ERR, "can't encode reply");
        return -1;
    }

    mirror_log(session->loginfo, V_ALL, "reply xmit data\n```");
    mirror_log_bytes(session->loginfo, V_ALL, H5FD_MIRROR_XMIT_REPLY_SIZE, (const unsigned char *)xmit_buf);
    mirror_log(session->loginfo, V_ALL, "```");

    if (HDwrite(session->sockfd, xmit_buf, H5FD_MIRROR_XMIT_REPLY_SIZE) < 0) {
        mirror_log(session->loginfo, V_ERR, "can't write reply to Driver");
        return -1;
    }

    return 0;
} /* end _xmit_reply() */

/* ---------------------------------------------------------------------------
 * Function:    reply_ok
 *
 * Purpose:     Send an OK reply through the session.
 *
 * Return:      0 on success, -1 if error.
 * ---------------------------------------------------------------------------
 */
static int
reply_ok(struct mirror_session *session)
{
    H5FD_mirror_xmit_reply_t *reply = &(session->reply);

    assert(session && (session->magic == MW_SESSION_MAGIC));

    mirror_log(session->loginfo, V_ALL, "reply_ok()");

    reply->status = H5FD_MIRROR_STATUS_OK;
    memset(reply->message, 0, H5FD_MIRROR_STATUS_MESSAGE_MAX);
    return _xmit_reply(session);
} /* end reply_ok() */

/* ---------------------------------------------------------------------------
 * Function:    reply_error
 *
 * Purpose:     Send an ERROR reply with message through the session.
 *              Message may be cut short if it would overflow the available
 *              buffer in the xmit.
 *
 * Return:      0 on success, -1 if error.
 * ---------------------------------------------------------------------------
 */
static int
reply_error(struct mirror_session *session, const char *msg)
{
    H5FD_mirror_xmit_reply_t *reply = &(session->reply);

    assert(session && (session->magic == MW_SESSION_MAGIC));

    mirror_log(session->loginfo, V_ALL, "reply_error(%s)", msg);

    reply->status = H5FD_MIRROR_STATUS_ERROR;
    snprintf(reply->message, H5FD_MIRROR_STATUS_MESSAGE_MAX - 1, "%s", msg);
    return _xmit_reply(session);
} /* end reply_error() */

/* ---------------------------------------------------------------------------
 * Function:    do_close
 *
 * Purpose:     Handle an CLOSE operation.
 *
 * Return:      0 on success, -1 if error.
 * ---------------------------------------------------------------------------
 */
static int
do_close(struct mirror_session *session)
{

    assert(session && (session->magic == MW_SESSION_MAGIC));

    mirror_log(session->loginfo, V_INFO, "do_close()");

    if (NULL == session->file) {
        mirror_log(session->loginfo, V_ERR, "no file to close!");
        reply_error(session, "no file to close");
        return -1;
    }

    if (H5FDclose(session->file) < 0) {
        mirror_log(session->loginfo, V_ERR, "H5FDclose()");
        reply_error(session, "H5FDclose()");
        return -1;
    }
    session->file = NULL;

    if (reply_ok(session) < 0) {
        mirror_log(session->loginfo, V_ERR, "can't reply");
        reply_error(session, "ok reply failed; session contaminated");
        return -1;
    }

    return 0;
} /* end do_close() */

/* ---------------------------------------------------------------------------
 * Function:    do_lock
 *
 * Purpose:     Handle a LOCK operation.
 *
 * Return:      0 on success, -1 if error.
 * ---------------------------------------------------------------------------
 */
static int
do_lock(struct mirror_session *session, const unsigned char *xmit_buf)
{
    size_t                  decode_ret = 0;
    H5FD_mirror_xmit_lock_t xmit_lock;

    assert(session && (session->magic == MW_SESSION_MAGIC) && xmit_buf);

    mirror_log(session->loginfo, V_INFO, "do_lock()");

    decode_ret = H5FD_mirror_xmit_decode_lock(&xmit_lock, xmit_buf);
    if (H5FD_MIRROR_XMIT_LOCK_SIZE != decode_ret) {
        mirror_log(session->loginfo, V_ERR, "can't decode set-eoa xmit");
        reply_error(session, "remote xmit_eoa_t decoding size failure");
        return -1;
    }

    if (!H5FD_mirror_xmit_is_lock(&xmit_lock)) {
        mirror_log(session->loginfo, V_ERR, "not a set-eoa xmit");
        reply_error(session, "remote xmit_eoa_t decode failure");
        return -1;
    }
    mirror_log(session->loginfo, V_INFO, "lock rw: (%d)", xmit_lock.rw);

    if (H5FDlock(session->file, (bool)xmit_lock.rw) < 0) {
        mirror_log(session->loginfo, V_ERR, "H5FDlock()");
        reply_error(session, "remote H5FDlock() failure");
        return -1;
    }

    if (reply_ok(session) < 0) {
        mirror_log(session->loginfo, V_ERR, "can't reply");
        reply_error(session, "ok reply failed; session contaminated");
        return -1;
    }

    return 0;
} /* end do_lock() */

/* ---------------------------------------------------------------------------
 * Function:    do_open
 *
 * Purpose:     Handle an OPEN operation.
 *
 * Return:      0 on success, -1 if error.
 * ---------------------------------------------------------------------------
 */
static int
do_open(struct mirror_session *session, const H5FD_mirror_xmit_open_t *xmit_open)
{
    hid_t    fapl_id  = H5I_INVALID_HID;
    unsigned _flags   = 0;
    haddr_t  _maxaddr = HADDR_UNDEF;

    assert(session && (session->magic == MW_SESSION_MAGIC) && xmit_open &&
           true == H5FD_mirror_xmit_is_open(xmit_open));

    mirror_log(session->loginfo, V_INFO, "do_open()");

    if (0 != xmit_open->pub.xmit_count) {
        mirror_log(session->loginfo, V_ERR, "open with xmit count not zero!");
        reply_error(session, "initial transmission count not zero");
        goto error;
    }
    if (0 != session->token) {
        mirror_log(session->loginfo, V_ERR, "open with token already set!");
        reply_error(session, "initial session token not zero");
        goto error;
    }

    session->xmit_count              = 1;
    session->token                   = xmit_open->pub.session_token;
    session->reply.pub.session_token = session->token;

    _flags   = (unsigned)xmit_open->flags;
    _maxaddr = (haddr_t)xmit_open->maxaddr;

    /* Check whether the native size_t on the remote machine (Driver) is larger
     * than that on the local machine; if so, issue a warning.
     * The blob is always an 8-byte bitfield -- check its contents.
     */
    if (xmit_open->size_t_blob > (uint64_t)((size_t)(-1))) {
        mirror_log(session->loginfo, V_WARN, "Driver size_t is larger than our own");
    }

    mirror_log(session->loginfo, V_INFO, "to open file %s (flags %d) (maxaddr %d)", xmit_open->filename,
               _flags, _maxaddr);

    /* Explicitly use Sec2 as the underlying driver for now.
     */
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    if (fapl_id < 0) {
        mirror_log(session->loginfo, V_ERR, "can't create FAPL");
        reply_error(session, "H5Pcreate() failure");
        goto error;
    }
    if (H5Pset_fapl_sec2(fapl_id) < 0) {
        mirror_log(session->loginfo, V_ERR, "can't set FAPL as Sec2");
        reply_error(session, "H5Pset_fapl_sec2() failure");
        goto error;
    }

    session->file = H5FDopen(xmit_open->filename, _flags, fapl_id, _maxaddr);
    if (NULL == session->file) {
        mirror_log(session->loginfo, V_ERR, "H5FDopen()");
        reply_error(session, "remote H5FDopen() failure");
        goto error;
    }

    /* FAPL is set and in use; clean up */
    if (H5Pclose(fapl_id) < 0) {
        mirror_log(session->loginfo, V_ERR, "can't set FAPL as Sec2");
        reply_error(session, "H5Pset_fapl_sec2() failure");
        goto error;
    }

    if (reply_ok(session) < 0) {
        mirror_log(session->loginfo, V_ERR, "can't reply");
        reply_error(session, "ok reply failed; session contaminated");
        return -1;
    }

    return 0;

error:
    if (fapl_id > 0) {
        H5E_BEGIN_TRY
        {
            (void)H5Pclose(fapl_id);
        }
        H5E_END_TRY
    }
    return -1;
} /* end do_open() */

/* ---------------------------------------------------------------------------
 * Function:    do_set_eoa
 *
 * Purpose:     Handle a SET_EOA operation.
 *
 * Return:      0 on success, -1 if error.
 * ---------------------------------------------------------------------------
 */
static int
do_set_eoa(struct mirror_session *session, const unsigned char *xmit_buf)
{
    size_t                 decode_ret = 0;
    H5FD_mirror_xmit_eoa_t xmit_seoa;

    assert(session && (session->magic == MW_SESSION_MAGIC) && xmit_buf);

    mirror_log(session->loginfo, V_INFO, "do_set_eoa()");

    decode_ret = H5FD_mirror_xmit_decode_set_eoa(&xmit_seoa, xmit_buf);
    if (H5FD_MIRROR_XMIT_EOA_SIZE != decode_ret) {
        mirror_log(session->loginfo, V_ERR, "can't decode set-eoa xmit");
        reply_error(session, "remote xmit_eoa_t decoding size failure");
        return -1;
    }

    if (!H5FD_mirror_xmit_is_set_eoa(&xmit_seoa)) {
        mirror_log(session->loginfo, V_ERR, "not a set-eoa xmit");
        reply_error(session, "remote xmit_eoa_t decode failure");
        return -1;
    }

    mirror_log(session->loginfo, V_INFO, "set EOA addr %d", xmit_seoa.eoa_addr);

    if (H5FDset_eoa(session->file, (H5FD_mem_t)xmit_seoa.type, (haddr_t)xmit_seoa.eoa_addr) < 0) {
        mirror_log(session->loginfo, V_ERR, "H5FDset_eoa()");
        reply_error(session, "remote H5FDset_eoa() failure");
        return -1;
    }

    if (reply_ok(session) < 0) {
        mirror_log(session->loginfo, V_ERR, "can't reply");
        reply_error(session, "ok reply failed; session contaminated");
        return -1;
    }

    return 0;
} /* end do_set_eoa() */

/* ---------------------------------------------------------------------------
 * Function:    do_truncate
 *
 * Purpose:     Handle a TRUNCATE operation.
 *
 * Return:      0 on success, -1 if error.
 * ---------------------------------------------------------------------------
 */
static int
do_truncate(struct mirror_session *session)
{

    assert(session && (session->magic == MW_SESSION_MAGIC));

    mirror_log(session->loginfo, V_INFO, "do_truncate()");

    /* default DXPL ID (0), 0 for "false" closing -- both probably unused */
    if (H5FDtruncate(session->file, 0, 0) < 0) {
        mirror_log(session->loginfo, V_ERR, "H5FDtruncate()");
        reply_error(session, "remote H5FDtruncate() failure");
        return -1;
    }

    if (reply_ok(session) < 0) {
        mirror_log(session->loginfo, V_ERR, "can't reply");
        reply_error(session, "ok reply failed; session contaminated");
        return -1;
    }

    return 0;
} /* end do_truncate() */

/* ---------------------------------------------------------------------------
 * Function:    do_unlock
 *
 * Purpose:     Handle an UNLOCK operation.
 *
 * Return:      0 on success, -1 if error.
 * ---------------------------------------------------------------------------
 */
static int
do_unlock(struct mirror_session *session)
{
    assert(session && (session->magic == MW_SESSION_MAGIC));

    mirror_log(session->loginfo, V_INFO, "do_unlock()");

    if (H5FDunlock(session->file) < 0) {
        mirror_log(session->loginfo, V_ERR, "H5FDunlock()");
        reply_error(session, "remote H5FDunlock() failure");
        return -1;
    }

    if (reply_ok(session) < 0) {
        mirror_log(session->loginfo, V_ERR, "can't reply");
        reply_error(session, "ok reply failed; session contaminated");
        return -1;
    }

    return 0;
} /* end do_unlock() */

/* ---------------------------------------------------------------------------
 * Function:    do_write
 *
 * Purpose:     Handle a WRITE operation.
 *              Receives command, replies; receives & writes data, replies.
 *
 *              It is known that this results in suboptimal performance,
 *              but handling both small and very, very large write buffers
 *              with a single "over the wire" exchange
 *              poses design challenges not worth tackling as of March 2020.
 *
 * Return:      0 on success, -1 if error.
 * ---------------------------------------------------------------------------
 */
static int
do_write(struct mirror_session *session, const unsigned char *xmit_buf)
{
    size_t                   decode_ret        = 0;
    haddr_t                  addr              = 0;
    haddr_t                  sum_bytes_written = 0;
    H5FD_mem_t               type              = 0;
    char                    *buf               = NULL;
    ssize_t                  nbytes_in_packet  = 0;
    H5FD_mirror_xmit_write_t xmit_write;

    assert(session && (session->magic == MW_SESSION_MAGIC) && xmit_buf);

    mirror_log(session->loginfo, V_INFO, "do_write()");

    if (NULL == session->file) {
        mirror_log(session->loginfo, V_ERR, "no open file!");
        reply_error(session, "no file open on remote");
        return -1;
    }

    decode_ret = H5FD_mirror_xmit_decode_write(&xmit_write, xmit_buf);
    if (H5FD_MIRROR_XMIT_WRITE_SIZE != decode_ret) {
        mirror_log(session->loginfo, V_ERR, "can't decode write xmit");
        reply_error(session, "remote xmit_write_t decoding size failure");
        return -1;
    }

    if (!H5FD_mirror_xmit_is_write(&xmit_write)) {
        mirror_log(session->loginfo, V_ERR, "not a write xmit");
        reply_error(session, "remote xmit_write_t decode failure");
        return -1;
    }

    addr = (haddr_t)xmit_write.offset;
    type = (H5FD_mem_t)xmit_write.type;

    /* Allocate the buffer once -- reuse between loops.
     */
    buf = (char *)malloc(sizeof(char) * H5FD_MIRROR_DATA_BUFFER_MAX);
    if (NULL == buf) {
        mirror_log(session->loginfo, V_ERR, "can't allocate databuffer");
        reply_error(session, "can't allocate buffer for receiving data");
        return -1;
    }

    /* got write signal; ready for data */
    if (reply_ok(session) < 0) {
        mirror_log(session->loginfo, V_ERR, "can't reply");
        reply_error(session, "ok reply failed; session contaminated");
        return -1;
    }

    mirror_log(session->loginfo, V_INFO, "to write %zu bytes at %zu", xmit_write.size, addr);

    /* The given write may be:
     * 1. larger than the allowed single buffer size
     * 2. larger than the native size_t of this system
     *
     * Handle all cases by looping, ingesting as much of the stream as possible
     * and writing that part to the file.
     */
    sum_bytes_written = 0;
    do {
        if ((nbytes_in_packet = HDread(session->sockfd, buf, H5FD_MIRROR_DATA_BUFFER_MAX)) < 0) {
            mirror_log(session->loginfo, V_ERR, "can't read into databuffer");
            reply_error(session, "can't read data buffer");
            return -1;
        }

        mirror_log(session->loginfo, V_INFO, "received %zd bytes", nbytes_in_packet);
        if (HEXDUMP_WRITEDATA) {
            mirror_log(session->loginfo, V_ALL, "DATA:\n```");
            mirror_log_bytes(session->loginfo, V_ALL, (size_t)nbytes_in_packet, (const unsigned char *)buf);
            mirror_log(session->loginfo, V_ALL, "```");
        }

        mirror_log(session->loginfo, V_INFO, "writing %zd bytes at %zu", nbytes_in_packet,
                   (addr + sum_bytes_written));

        if (H5FDwrite(session->file, type, H5P_DEFAULT, (addr + sum_bytes_written), (size_t)nbytes_in_packet,
                      buf) < 0) {
            mirror_log(session->loginfo, V_ERR, "H5FDwrite()");
            reply_error(session, "remote H5FDwrite() failure");
            return -1;
        }

        sum_bytes_written += (haddr_t)nbytes_in_packet;

    } while (sum_bytes_written < xmit_write.size); /* end while ingesting */

    free(buf);

    /* signal that we're done here and a-ok */
    if (reply_ok(session) < 0) {
        mirror_log(session->loginfo, V_ERR, "can't reply");
        reply_error(session, "ok reply failed; session contaminated");
        return -1;
    }

    return 0;
} /* end do_write() */

/* ---------------------------------------------------------------------------
 * Function:    receive_communique
 *
 * Purpose:     Accept bytes from the socket, check for emergency shutdown, and
 *              sanity-check received bytes.
 *              The raw bytes read are stored in the sock_comm structure at
 *              comm->raw.
 *              The raw bytes are decoded and a xmit_t (header) struct pointer
 *              in comm is populated at comm->xmit_recd.
 *
 * Return:      0 on success, -1 if error.
 * ---------------------------------------------------------------------------
 */
static int
receive_communique(struct mirror_session *session, struct sock_comm *comm)
{
    ssize_t             read_ret = 0;
    size_t              decode_ret;
    H5FD_mirror_xmit_t *X = comm->xmit_recd;

    assert((session != NULL) && (session->magic == MW_SESSION_MAGIC) && (comm != NULL) &&
           (comm->magic == MW_SOCK_COMM_MAGIC) && (comm->xmit_recd != NULL) && (comm->raw != NULL) &&
           (comm->raw_size >= H5FD_MIRROR_XMIT_BUFFER_MAX));

    mirror_log(session->loginfo, V_INFO, "receive_communique()");

    memset(comm->raw, 0, comm->raw_size);
    comm->recd_die = 0;

    mirror_log(session->loginfo, V_INFO, "ready to receive"); /* TODO */

    if ((read_ret = HDread(session->sockfd, comm->raw, H5FD_MIRROR_XMIT_BUFFER_MAX)) < 0) {
        mirror_log(session->loginfo, V_ERR, "read:%zd", read_ret);
        goto error;
    }

    mirror_log(session->loginfo, V_INFO, "received %zd bytes", read_ret);
    if (HEXDUMP_XMITS) {
        mirror_log(session->loginfo, V_ALL, "```", read_ret);
        mirror_log_bytes(session->loginfo, V_ALL, (size_t)read_ret, (const unsigned char *)comm->raw);
        mirror_log(session->loginfo, V_ALL, "```");
    } /* end if hexdump transmissions received */

    /* old-fashioned manual kill (for debugging) */
    if (!strncmp("GOODBYE", comm->raw, 7)) {
        mirror_log(session->loginfo, V_INFO, "received GOODBYE");
        comm->recd_die = 1;
        goto done;
    }

    decode_ret = H5FD_mirror_xmit_decode_header(X, (const unsigned char *)comm->raw);
    if (H5FD_MIRROR_XMIT_HEADER_SIZE != decode_ret) {
        mirror_log(session->loginfo, V_ERR, "header decode size mismatch: expected (%z), got (%z)",
                   H5FD_MIRROR_XMIT_HEADER_SIZE, decode_ret);
        /* Try to tell Driver that it should stop */
        reply_error(session, "xmit size mismatch");
        goto error;
    }

    if (!H5FD_mirror_xmit_is_xmit(X)) {
        mirror_log(session->loginfo, V_ERR, "bad magic: 0x%X", X->magic);
        /* Try to tell Driver that it should stop */
        reply_error(session, "bad magic");
        goto error;
    }

    if (session->xmit_count != X->xmit_count) {
        mirror_log(session->loginfo, V_ERR, "xmit_count mismatch exp:%d recd:%d", session->xmit_count,
                   X->xmit_count);
        /* Try to tell Driver that it should stop */
        reply_error(session, "xmit_count mismatch");
        goto error;
    }

    if ((session->token > 0) && (session->token != X->session_token)) {
        mirror_log(session->loginfo, V_ERR, "wrong session");
        /* Try to tell Driver that it should stop */
        reply_error(session, "wrong session");
        goto error;
    }

    session->xmit_count++;

done:
    return 0;

error:
    return -1;
} /* end receive_communique() */

/* ---------------------------------------------------------------------------
 * Function:    process_instructions
 *
 * Purpose:     Receive and handle all instructions from Driver.
 *
 * Return:      0 on success, -1 if error.
 * ---------------------------------------------------------------------------
 */
static int
process_instructions(struct mirror_session *session)
{
    struct sock_comm   comm;
    char              *xmit_buf = NULL; /* raw bytes */
    size_t             buf_size;
    H5FD_mirror_xmit_t xmit_recd; /* for decoded xmit header */

    assert(session && (session->magic == MW_SESSION_MAGIC));

    mirror_log(session->loginfo, V_INFO, "process_instructions()");

    buf_size = H5FD_MIRROR_XMIT_BUFFER_MAX * sizeof(char);

    if (NULL == (xmit_buf = malloc(buf_size))) {
        mirror_log(session->loginfo, V_ERR, "out of memory");
        goto error;
    }

    comm.magic     = MW_SOCK_COMM_MAGIC;
    comm.recd_die  = 0; /* Flag for program to terminate */
    comm.xmit_recd = &xmit_recd;
    comm.raw       = xmit_buf;
    comm.raw_size  = buf_size;

    while (1) { /* sill-listening infinite loop */

        /* Use convenience structure for raw/decoded info in/out */
        if (receive_communique(session, &comm) < 0) {
            mirror_log(session->loginfo, V_ERR, "problem reading socket");
            goto error;
        }

        if (comm.recd_die) {
            goto done;
        }

        switch (xmit_recd.op) {
            case H5FD_MIRROR_OP_CLOSE:
                if (do_close(session) < 0) {
                    goto error;
                }
                goto done;
            case H5FD_MIRROR_OP_LOCK:
                if (do_lock(session, (const unsigned char *)xmit_buf) < 0) {
                    goto error;
                }
                break;
            case H5FD_MIRROR_OP_OPEN:
                mirror_log(session->loginfo, V_ERR, "OPEN xmit during session");
                reply_error(session, "illegal OPEN xmit during session");
                goto error;
            case H5FD_MIRROR_OP_SET_EOA:
                if (do_set_eoa(session, (const unsigned char *)xmit_buf) < 0) {
                    goto error;
                }
                break;
            case H5FD_MIRROR_OP_TRUNCATE:
                if (do_truncate(session) < 0) {
                    goto error;
                }
                break;
            case H5FD_MIRROR_OP_UNLOCK:
                if (do_unlock(session) < 0) {
                    goto error;
                }
                break;
            case H5FD_MIRROR_OP_WRITE:
                if (do_write(session, (const unsigned char *)xmit_buf) < 0) {
                    goto error;
                }
                break;
            default:
                mirror_log(session->loginfo, V_ERR, "unrecognized transmission");
                reply_error(session, "unrecognized transmission");
                goto error;
        } /* end switch (xmit_recd.op) */

    } /* end while still listening */

done:
    comm.magic      = 0; /* invalidate structure, on principle */
    xmit_recd.magic = 0; /* invalidate structure, on principle */
    free(xmit_buf);
    return 0;

error:
    free(xmit_buf);
    return -1;
} /* end process_instructions() */

/* ---------------------------------------------------------------------------
 * Function:    run_writer
 *
 * Purpose:     Initiate Writer operations.
 *
 *              Receives as parameters a socket which has accepted the
 *              connection to the Driver and the OPEN xmit (which must be
 *              decoded into the structure and verified prior to being passed
 *              to this function).
 *
 *              Is not responsible for closing or cleaning up any of the
 *              received parameters.
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 * ---------------------------------------------------------------------------
 */
herr_t
run_writer(int socketfd, H5FD_mirror_xmit_open_t *xmit_open)
{
    struct mirror_session *session   = NULL;
    int                    ret_value = SUCCEED;

    session = session_start(socketfd, xmit_open);
    if (NULL == session) {
        mirror_log(NULL, V_ERR, "Can't start session -- aborting");
        ret_value = FAIL;
    }
    else {
        if (process_instructions(session) < 0) {
            mirror_log(session->loginfo, V_ERR, "problem processing instructions");
            ret_value = FAIL;
        }
        if (session_stop(session) < 0) {
            mirror_log(NULL, V_ERR, "Can't stop session -- going down hard");
            ret_value = FAIL;
        }
    }

    return ret_value;
} /* end run_writer */

#endif /* H5_HAVE_MIRROR_VFD */
