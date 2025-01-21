/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*****************************************************************************
 * Read-Only S3 Virtual File Driver (VFD)
 *
 * Source for S3 Communications module
 *
 * ***NOT A FILE DRIVER***
 *
 * Provide functions and structures required for interfacing with Amazon
 * Simple Storage Service (S3).
 *
 * Provide S3 object access as if it were a local file.
 *
 * Connect to remote host, send and receive HTTP requests and responses
 * as part of the AWS REST API, authenticating requests as appropriate.
 *
 *****************************************************************************/

/****************/
/* Module Setup */
/****************/

/***********/
/* Headers */
/***********/

/* There's no H5FDs3comms_test.c file, so the test functions are located here */
#define H5FD_S3COMMS_TESTING

#include "H5private.h"   /* generic functions */
#include "H5Eprivate.h"  /* error handling    */
#include "H5MMprivate.h" /* memory management */
#include "H5FDs3comms.h" /* S3 Communications */
#include "H5FDros3.h"    /* ros3 file driver  */

#ifdef H5_HAVE_ROS3_VFD
#include <openssl/buffer.h>

/****************/
/* Local Macros */
/****************/

/* manipulate verbosity of CURL output
 *
 * 0 -> no explicit curl output
 * 1 -> print: (1) failure info to stderr on error, (2) basic HTTP range GET info
 * 2 -> in addition to above, print information for all performs; sets all
 *      curl handles with CURLOPT_VERBOSE
 */
#define S3COMMS_CURL_VERBOSITY 0

/* size to allocate for "bytes=<first_byte>[-<last_byte>]" HTTP Range value */
#define S3COMMS_MAX_RANGE_STRING_SIZE 128

/* Size of buffers that hold hex representations of SHA256 digests */
#define S3COMMS_SHA256_HEXSTR_LENGTH (SHA256_DIGEST_LENGTH * 2 + 1)

/******************/
/* Local Typedefs */
/******************/

/********************/
/* Local Structures */
/********************/

/* Structure passed to curl write callback
 *
 * Pointer to data region and record of bytes written (offset)
 */
struct s3r_datastruct {
    char  *data;
    size_t size;
};

/********************/
/* Local Prototypes */
/********************/

static size_t H5FD__s3comms_curl_write_callback(char *ptr, size_t size, size_t nmemb, void *userdata);

static herr_t H5FD__s3comms_s3r_configure_aws(s3r_t *handle, const H5FD_ros3_fapl_t *fa,
                                              const char *fapl_token);

static herr_t H5FD__s3comms_s3r_getsize(s3r_t *handle);

static herr_t H5FD__s3comms_bytes_to_hex(char *dest, size_t dest_len, const unsigned char *msg,
                                         size_t msg_len);

static herr_t H5FD__s3comms_load_aws_creds_from_file(FILE *file, const char *profile_name, char *key_id,
                                                     char *access_key, char *aws_region);

static herr_t H5FD__s3comms_make_iso_8661_string(time_t time, char iso8601[ISO8601_SIZE]);

static parsed_url_t *H5FD__s3comms_parse_url(const char *url);

static herr_t H5FD__s3comms_free_purl(parsed_url_t *purl);

/*********************/
/* Package Variables */
/*********************/

/*****************************/
/* Library Private Variables */
/*****************************/

/*******************/
/* Local Variables */
/*******************/

/*************/
/* Functions */
/*************/

/*----------------------------------------------------------------------------
 * Function:    H5FD__s3comms_curl_write_callback
 *
 * Purpose:     Function called by CURL to write received data
 *
 * Return:      Number of bytes processed
 *----------------------------------------------------------------------------
 */
static size_t
H5FD__s3comms_curl_write_callback(char *ptr, size_t size, size_t nmemb, void *userdata)
{
    struct s3r_datastruct *sds    = (struct s3r_datastruct *)userdata;
    size_t                 nbytes = size * nmemb;

    /* Write bytes and size to userdata/sds struct */
    if (size > 0) {
        H5MM_memcpy(&(sds->data[sds->size]), ptr, nbytes);
        sds->size += nbytes;
    }

    return nbytes;
} /* end H5FD__s3comms_curl_write_callback() */

/*----------------------------------------------------------------------------
 * Function: H5FD__s3comms_hrb_node_set
 *
 * Purpose:
 *
 *     Create, insert, modify, and remove elements in a field node list.
 *
 *     `name` cannot be NULL; will return FAIL and list will be unaltered.
 *
 *     Entries are accessed via the lowercase representation of their name:
 *     "Host", "host", and "hOSt" would all access the same node,
 *     but name's case is relevant in HTTP request output.
 *
 *     List pointer `L` must always point to either of :
 *     - header node with lowest alphabetical order (by lowername)
 *     - NULL, if list is empty
 *
 *    Types of operations:
 *
 *    - CREATE
 *        - If `L` is NULL and `name` and `value` are not NULL,
 *          a new node is created at `L`, starting a list.
 *    - MODIFY
 *        - If a node is found with a matching lowercase name and `value`
 *          is not NULL, the existing name, value, and cat values are released
 *          and replaced with the new data.
 *        - No modifications are made to the list pointers.
 *    - REMOVE
 *        - If `value` is NULL, will attempt to remove node with matching
 *          lowercase name.
 *        - If no match found, returns FAIL and list is not modified.
 *        - When removing a node, all its resources is released.
 *        - If removing the last node in the list, list pointer is set to NULL.
 *    - INSERT
 *        - If no nodes exists with matching lowercase name and `value`
 *          is not NULL, a new node is created, inserted into list
 *          alphabetically by lowercase name.
 *
 * Return:
 *
 *     - SUCCESS: `SUCCEED`
 *         - List was successfully modified
 *     - FAILURE: `FAIL`
 *         - Unable to perform operation
 *             - Forbidden (attempting to remove absent node, e.g.)
 *             - Internal error
 *
 *----------------------------------------------------------------------------
 */
herr_t
H5FD__s3comms_hrb_node_set(hrb_node_t **L, const char *name, const char *value)
{
    size_t      i          = 0;
    char       *valuecpy   = NULL;
    char       *namecpy    = NULL;
    size_t      namelen    = 0;
    char       *lowername  = NULL;
    char       *nvcat      = NULL;
    hrb_node_t *node_ptr   = NULL;
    hrb_node_t *new_node   = NULL;
    bool        is_looking = true;
    herr_t      ret_value  = SUCCEED;

    FUNC_ENTER_PACKAGE

    if (name == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to operate on NULL name");
    namelen = strlen(name);

    /***********************
     * PREPARE ALL STRINGS *
     **********************/

    /* Copy and lowercase name */
    if (NULL == (lowername = strdup(name)))
        HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, FAIL, "cannot make space for lowercase name copy");
    for (i = 0; i < namelen; i++)
        lowername[i] = (char)tolower((int)lowername[i]);

    /* If value supplied, copy name, value, and concatenated "name: value".
     * If NULL, we will be removing a node or doing nothing, so no need for
     * copies.
     */
    if (value != NULL) {
        int    ret      = 0;
        size_t valuelen = strlen(value);
        size_t catlen   = namelen + valuelen + 2; /* +2 from ": " */
        size_t catwrite = catlen + 3;             /* 3 not 1 to quiet compiler warning */

        if (NULL == (namecpy = strdup(name)))
            HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, FAIL, "cannot copy name");
        if (NULL == (valuecpy = strdup(value)))
            HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, FAIL, "cannot copy value");

        if (NULL == (nvcat = (char *)H5MM_malloc(sizeof(char) * catwrite)))
            HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, FAIL, "cannot make space for concatenated string");
        ret = snprintf(nvcat, catwrite, "%s: %s", name, value);
        if (ret < 0 || (size_t)ret > catlen)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "cannot concatenate `%s: %s", name, value);

        /* Create new_node, should we need it */
        if (NULL == (new_node = (hrb_node_t *)H5MM_calloc(sizeof(hrb_node_t))))
            HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, FAIL, "cannot make space for new_node");
    }

    /***************
     * ACT ON LIST *
     ***************/

    if (*L == NULL) {
        if (value == NULL)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "trying to remove node from empty list");
        else {
            /*******************
             * CREATE NEW LIST *
             *******************/

            new_node->cat       = nvcat;
            new_node->name      = namecpy;
            new_node->lowername = lowername;
            new_node->value     = valuecpy;

            *L = new_node;
            goto done; /* bypass further seeking */
        }
    }

    node_ptr = (*L);

    /* Check whether to modify/remove first node in list */
    if (strcmp(lowername, node_ptr->lowername) == 0) {

        is_looking = false;

        if (value == NULL) {
            /***************
             * REMOVE HEAD *
             ***************/

            *L = node_ptr->next;

            H5MM_xfree(node_ptr->cat);
            H5MM_xfree(node_ptr->lowername);
            H5MM_xfree(node_ptr->name);
            H5MM_xfree(node_ptr->value);
            H5MM_xfree(node_ptr);

            H5MM_xfree(lowername);
            lowername = NULL;
        }
        else {
            /***************
             * MODIFY HEAD *
             ***************/

            H5MM_xfree(node_ptr->cat);
            H5MM_xfree(node_ptr->name);
            H5MM_xfree(node_ptr->value);

            node_ptr->name  = namecpy;
            node_ptr->value = valuecpy;
            node_ptr->cat   = nvcat;

            H5MM_xfree(lowername);
            lowername = NULL;
            H5MM_xfree(new_node);
            new_node = NULL;
        }
    }
    else if (strcmp(lowername, node_ptr->lowername) < 0) {

        is_looking = false;

        if (value == NULL)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "trying to remove a node 'before' head");
        else {
            /*******************
             * INSERT NEW HEAD *
             *******************/

            new_node->name      = namecpy;
            new_node->value     = valuecpy;
            new_node->lowername = lowername;
            new_node->cat       = nvcat;
            new_node->next      = node_ptr;
            *L                  = new_node;
        }
    }

    /***************
     * SEARCH LIST *
     ***************/

    while (is_looking) {
        if (node_ptr->next == NULL) {

            is_looking = false;

            if (value == NULL)
                HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "trying to remove absent node");
            else {
                /*******************
                 * APPEND NEW NODE *
                 *******************/

                assert(strcmp(lowername, node_ptr->lowername) > 0);
                new_node->name      = namecpy;
                new_node->value     = valuecpy;
                new_node->lowername = lowername;
                new_node->cat       = nvcat;
                node_ptr->next      = new_node;
            }
        }
        else if (strcmp(lowername, node_ptr->next->lowername) < 0) {

            is_looking = false;

            if (value == NULL)
                HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "trying to remove absent node");
            else {
                /*******************
                 * INSERT NEW NODE *
                 *******************/

                assert(strcmp(lowername, node_ptr->lowername) > 0);
                new_node->name      = namecpy;
                new_node->value     = valuecpy;
                new_node->lowername = lowername;
                new_node->cat       = nvcat;
                new_node->next      = node_ptr->next;
                node_ptr->next      = new_node;
            }
        }
        else if (strcmp(lowername, node_ptr->next->lowername) == 0) {

            is_looking = false;

            if (value == NULL) {
                /*****************
                 * REMOVE A NODE *
                 *****************/

                hrb_node_t *tmp = node_ptr->next;
                node_ptr->next  = tmp->next;

                H5MM_xfree(tmp->cat);
                H5MM_xfree(tmp->lowername);
                H5MM_xfree(tmp->name);
                H5MM_xfree(tmp->value);

                H5MM_xfree(tmp);

                H5MM_xfree(lowername);
                lowername = NULL;
            }
            else {
                /*****************
                 * MODIFY A NODE *
                 *****************/

                node_ptr = node_ptr->next;
                H5MM_xfree(node_ptr->name);
                H5MM_xfree(node_ptr->value);
                H5MM_xfree(node_ptr->cat);

                H5MM_xfree(new_node);
                H5MM_xfree(lowername);
                new_node  = NULL;
                lowername = NULL;

                node_ptr->name  = namecpy;
                node_ptr->value = valuecpy;
                node_ptr->cat   = nvcat;
            }
        }
        else {
            /****************
             * KEEP LOOKING *
             ****************/

            node_ptr = node_ptr->next;
        }
    } /* end while is_looking */

done:
    if (ret_value < 0) {
        H5MM_xfree(nvcat);
        H5MM_xfree(namecpy);
        H5MM_xfree(lowername);
        H5MM_xfree(valuecpy);
        H5MM_xfree(new_node);
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__s3comms_hrb_node_set() */

/*----------------------------------------------------------------------------
 * Function:    H5FD__s3comms_hrb_destroy
 *
 * Purpose:     Destroy and free resources associated with an HTTP buffer
 *
 *              buf can be NULL
 *
 *              NOTE: The hrb_node_t list is not destroyed by this function
 *
 * Return:      SUCCEED (can't fail)
 *----------------------------------------------------------------------------
 */
herr_t
H5FD__s3comms_hrb_destroy(hrb_t *buf)
{
    FUNC_ENTER_PACKAGE_NOERR

    if (buf != NULL) {
        H5MM_xfree(buf->verb);
        H5MM_xfree(buf->version);
        H5MM_xfree(buf->resource);
        H5MM_xfree(buf);
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD__s3comms_hrb_destroy() */

/*----------------------------------------------------------------------------
 * Function:    H5FD__s3comms_hrb_init_request
 *
 * Purpose:     Create a new HTTP Request Buffer
 *
 *              All non-NULL arguments should be NUL-terminated strings.
 *
 *              If `verb` is NULL, defaults to "GET".
 *              If `http_version` is NULL, defaults to "HTTP/1.1".
 *
 *              `resource` cannot be NULL
 *
 * Return:      SUCCESS:    pointer to new hrb_t
 *              FAILURE:    NULL
 *----------------------------------------------------------------------------
 */
hrb_t *
H5FD__s3comms_hrb_init_request(const char *_verb, const char *_resource, const char *_http_version)
{
    hrb_t *request   = NULL;
    char  *res       = NULL;
    char  *verb      = NULL;
    char  *vrsn      = NULL;
    hrb_t *ret_value = NULL;

    FUNC_ENTER_PACKAGE

    if (_resource == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "resource string cannot be NULL");

    /* Populate valid NULLs with defaults */
    if (_verb == NULL)
        _verb = "GET";
    if (_http_version == NULL)
        _http_version = "HTTP/1.1";

    /* Allocate space for HTTP request buffer */
    if (NULL == (request = (hrb_t *)H5MM_calloc(sizeof(hrb_t))))
        HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, NULL, "no space for request structure");

    /* Ensure the resource string starts with '/' */
    if (_resource[0] == '/') {
        if (NULL == (res = strdup(_resource)))
            HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, NULL, "cannot copy resource string");
    }
    else {
        size_t reslen = strlen(_resource);

        if (NULL == (res = (char *)H5MM_malloc(sizeof(char) * (reslen + 2))))
            HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, NULL, "no space for resource string");
        *res = '/';
        H5MM_memcpy((&res[1]), _resource, (reslen + 1));
    }

    if (NULL == (verb = strdup(_verb)))
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "cannot copy verb string");

    if (NULL == (vrsn = strdup(_http_version)))
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "cannot copy http-version string");

    request->resource = res;
    request->verb     = verb;
    request->version  = vrsn;

    ret_value = request;

done:
    if (ret_value == NULL) {
        H5MM_xfree(request);
        H5MM_xfree(vrsn);
        H5MM_xfree(verb);
        H5MM_xfree(res);
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__s3comms_hrb_init_request() */

/****************************************************************************
 * S3R FUNCTIONS
 ****************************************************************************/

/*----------------------------------------------------------------------------
 * Function:    H5FD__s3comms_s3r_close
 *
 * Purpose:     Close communications through given S3 Request Handle (s3r_t)
 *              and clean up associated resources
 *
 * Return:      SUCCEED/FAIL
 *----------------------------------------------------------------------------
 */
herr_t
H5FD__s3comms_s3r_close(s3r_t *handle)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    if (handle == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "handle cannot be NULL");

    curl_easy_cleanup(handle->curlhandle);

    H5MM_xfree(handle->secret_id);
    H5MM_xfree(handle->aws_region);
    H5MM_xfree(handle->signing_key);
    H5MM_xfree(handle->token);
    H5MM_xfree(handle->http_verb);

    if (H5FD__s3comms_free_purl(handle->purl) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "unable to release parsed url structure");

    H5MM_xfree(handle);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD__s3comms_s3r_close */

/*----------------------------------------------------------------------------
 * Function:    H5FD__s3comms_s3r_get_filesize
 *
 * Purpose:     Retrieve the filesize of an open request handle
 *
 * Return:      SUCCEED/FAIL
 *----------------------------------------------------------------------------
 */
size_t
H5FD__s3comms_s3r_get_filesize(s3r_t *handle)
{
    size_t ret_value = 0;

    FUNC_ENTER_PACKAGE_NOERR

    if (handle != NULL)
        ret_value = handle->filesize;

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD__s3comms_s3r_get_filesize */

/*----------------------------------------------------------------------------
 * Function:    H5FD__s3comms_s3r_getsize
 *
 * Purpose:     Get the number of bytes of handle's target resource
 *
 * Return:      SUCCEED/FAIL
 *----------------------------------------------------------------------------
 */
static herr_t
H5FD__s3comms_s3r_getsize(s3r_t *handle)
{
    uintmax_t             content_length  = 0;
    CURL                 *curlh           = NULL;
    char                 *end             = NULL;
    char                 *header_response = NULL;
    struct s3r_datastruct sds             = {NULL, 0};
    char                 *start           = NULL;
    herr_t                ret_value       = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(handle);
    assert(handle->curlhandle);
    assert(handle->http_verb);

    /********************
     * PREPARE FOR HEAD *
     ********************/

    /* Set handle and curlhandle to perform an HTTP HEAD request on file */

    curlh = handle->curlhandle;
    if (CURLE_OK != curl_easy_setopt(curlh, CURLOPT_NOBODY, 1))
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "error while setting CURL option (CURLOPT_NOBODY)");

    if (CURLE_OK != curl_easy_setopt(curlh, CURLOPT_HEADERDATA, &sds))
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "error while setting CURL option (CURLOPT_HEADERDATA)");

    strcpy(handle->http_verb, "HEAD");

    if (NULL == (header_response = (char *)H5MM_malloc(sizeof(char) * CURL_MAX_HTTP_HEADER)))
        HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL, "unable to allocate space for curl header response");
    sds.data = header_response;

    /*******************
     * PERFORM REQUEST *
     *******************/

    /* These parameters fetch the entire file, but, with a NULL destination and
     * NOBODY and HEADERDATA supplied above, only http metadata will be sent by
     * the server and recorded by s3comms
     */
    if (H5FD__s3comms_s3r_read(handle, 0, 0, NULL) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "problem in reading during getsize");

    if (sds.size > CURL_MAX_HTTP_HEADER)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "HTTP metadata buffer overrun");
    else if (sds.size == 0)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "No HTTP metadata");

    /******************
     * PARSE RESPONSE *
     ******************/

    /* Parse received headers to extract "Content-Length" from response
     * headers, storing file size at handle->filesize.
     */

    if (NULL == (start = HDstrcasestr(header_response, "\r\nContent-Length: ")))
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "could not find \"Content-Length\" in response");

    /* move "start" to beginning of value in line; find end of line */
    start = start + strlen("\r\nContent-Length: ");
    end   = strstr(start, "\r\n");
    if (end == NULL)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "could not find end of content length line");

    /* place NUL terminator at end of numbers */
    *end = '\0';

    content_length = strtoumax((const char *)start, NULL, 0);
    if (UINTMAX_MAX > SIZE_MAX && content_length > SIZE_MAX)
        HGOTO_ERROR(H5E_VFL, H5E_OVERFLOW, FAIL, "content_length overflows size_t");

    /* errno set by strtoumax */
    if (content_length == 0 || errno == ERANGE)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL,
                    "could not convert found \"Content-Length\" response (\"%s\")", start);

    handle->filesize = (size_t)content_length;

#if S3COMMS_CURL_VERBOSITY > 0
    fprintf(stdout, " -- size: %ju\n", content_length);
    fflush(stdout);
#endif

    /**********************
     * UNDO HEAD SETTINGS *
     **********************/

    if (CURLE_OK != curl_easy_setopt(curlh, CURLOPT_NOBODY, 0))
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "error while setting CURL option (CURLOPT_NOBODY)");

    /* Unset HTTP HEAD settings from curl handle, returning to initial state */
    if (CURLE_OK != curl_easy_setopt(curlh, CURLOPT_HEADERDATA, NULL))
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "error while setting CURL option (CURLOPT_HEADERDATA)");

    strcpy(handle->http_verb, "GET");
done:
    H5MM_xfree(header_response);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD__s3comms_s3r_getsize */

/*----------------------------------------------------------------------------
 * Function:    H5FD__s3comms_s3r_open
 *
 * Purpose:     Logically open a file hosted on S3
 *
 *              fa can be NULL (implies no authentication)
 *              fapl_token can be NULL
 *
 * Return:      SUCCESS:    Pointer to new request handle.
 *              FAILURE:    NULL
 *----------------------------------------------------------------------------
 */
s3r_t *
H5FD__s3comms_s3r_open(const char *url, const H5FD_ros3_fapl_t *fa, const char *fapl_token)
{
    CURL  *curlh     = NULL;
    s3r_t *handle    = NULL;
    s3r_t *ret_value = NULL;

    FUNC_ENTER_PACKAGE

    if (url == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "url cannot be NULL");
    if (url[0] == '\0')
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "url cannot be an empty string");

    /* Create handle and set fields */
    if (NULL == (handle = (s3r_t *)H5MM_calloc(sizeof(s3r_t))))
        HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, NULL, "could not allocate space for handle");

    if (NULL == (handle->http_verb = (char *)H5MM_calloc(sizeof(char) * 16)))
        HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, NULL, "unable to allocate space for S3 request HTTP verb");

    /* Parse URL */
    if (NULL == (handle->purl = H5FD__s3comms_parse_url(url)))
        HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, NULL, "could not allocate and create parsed URL");

    /*************************************
     * RECORD AUTHENTICATION INFORMATION *
     *************************************/

    if (fa && fa->authenticate)
        if (H5FD__s3comms_s3r_configure_aws(handle, fa, fapl_token) < 0)
            HGOTO_ERROR(H5E_ARGS, H5E_CANTINIT, NULL, "failure to configure AWS");

    /**************************
     * INITIALIZE CURL HANDLE *
     **************************/

    if (NULL == (curlh = curl_easy_init()))
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "problem creating curl easy handle!");

    if (CURLE_OK != curl_easy_setopt(curlh, CURLOPT_HTTPGET, 1L))
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "error while setting CURL option (CURLOPT_HTTPGET)");
    if (CURLE_OK != curl_easy_setopt(curlh, CURLOPT_HTTP_VERSION, CURL_HTTP_VERSION_1_1))
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "error while setting CURL option (CURLOPT_HTTP_VERSION)");
    if (CURLE_OK != curl_easy_setopt(curlh, CURLOPT_FAILONERROR, 1L))
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "error while setting CURL option (CURLOPT_FAILONERROR)");
    if (CURLE_OK != curl_easy_setopt(curlh, CURLOPT_WRITEFUNCTION, H5FD__s3comms_curl_write_callback))
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "error while setting CURL option (CURLOPT_WRITEFUNCTION)");
    if (CURLE_OK != curl_easy_setopt(curlh, CURLOPT_URL, url))
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "error while setting CURL option (CURLOPT_URL)");

#if S3COMMS_CURL_VERBOSITY > 1
    /* CURL will print (to stdout) information for each operation */
    curl_easy_setopt(curlh, CURLOPT_VERBOSE, 1L);
#endif

    handle->curlhandle = curlh;

    /***************
     *  FINISH UP  *
     ***************/

    /* Get the S3 object's size. This is the only time we touch the S3 object
     * (and thus ensure it exists) during the VFD's open callback.
     */
    if (H5FD__s3comms_s3r_getsize(handle) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "problem in H5FD__s3comms_s3r_getsize");

    ret_value = handle;

done:
    if (ret_value == NULL) {
        curl_easy_cleanup(curlh);

        if (handle != NULL) {
            if (H5FD__s3comms_free_purl(handle->purl) < 0)
                HDONE_ERROR(H5E_VFL, H5E_CANTFREE, NULL, "unable to free parsed url structure");

            H5MM_xfree(handle->aws_region);
            H5MM_xfree(handle->secret_id);
            H5MM_xfree(handle->signing_key);
            H5MM_xfree(handle->token);
            H5MM_xfree(handle->http_verb);
            H5MM_xfree(handle);
        }
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD__s3comms_s3r_open */

/*----------------------------------------------------------------------------
 * Function:    H5FD__s3comms_s3r_configure_aws
 *
 * Purpose:     Add AWS configuration and authentication info to an s3r_t
 *              handle
 *
 * Return:      SUCCEED/FAIL
 *----------------------------------------------------------------------------
 */
static herr_t
H5FD__s3comms_s3r_configure_aws(s3r_t *handle, const H5FD_ros3_fapl_t *fa, const char *fapl_token)
{
    uint8_t signing_key[SHA256_DIGEST_LENGTH];
    char    iso8601[ISO8601_SIZE]; /* ISO-8601 time string */

    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    /* These all need to exist to authenticate */
    if (fa->aws_region[0] == '\0')
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "region cannot be NULL");
    if (fa->secret_id[0] == '\0')
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "secret id cannot be NULL");
    if (fa->secret_key[0] == '\0')
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "signing key cannot be NULL");

    /* Copy strings into the s3r_t handle */
    if (NULL == (handle->aws_region = strdup(fa->aws_region)))
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "could not copy AWS region");
    if (NULL == (handle->secret_id = strdup(fa->secret_id)))
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "could not copy secret_id");

    /* SIGNING KEY */

    /* Get the current time in ISO-8601 format */
    if (H5FD__s3comms_make_iso_8661_string(time(NULL), iso8601) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "could not construct ISO-8601 string");

    /* Compute signing key (part of AWS/S3 REST API). Can be reused by
     * user/key for 7 days after creation.
     */
    if (H5FD__s3comms_make_aws_signing_key(signing_key, (const char *)fa->secret_key,
                                           (const char *)fa->aws_region, (const char *)iso8601) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "problem while computing signing key");

    /* Copy signing key (not a string) */
    if (NULL == (handle->signing_key = (uint8_t *)H5MM_malloc(sizeof(uint8_t) * SHA256_DIGEST_LENGTH)))
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "could not allocate space for handle key");
    H5MM_memcpy(handle->signing_key, signing_key, SHA256_DIGEST_LENGTH);

    /* TOKEN */

    if (fapl_token) {
        if (NULL == (handle->token = strdup(fapl_token)))
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "could not copy token");
    }
    else {
        if (NULL == (handle->token = strdup("")))
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "could not copy empty token");
    }

done:
    /* Cleanup is handled when the s3r_t handle is cleaned up */
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__s3comms_s3r_configure_aws() */

/*----------------------------------------------------------------------------
 * Function:    H5FD__s3comms_s3r_read
 *
 * Purpose:     Read file pointed to by request handle, writing specified
 *              offset .. (offset + len) bytes to buffer dest
 *
 *     If `len` is 0, reads entirety of file starting at `offset`.
 *     If `offset` and `len` are both 0, reads entire file.
 *
 *     If `offset` or `offset+len` is greater than the file size, read is
 *     aborted and returns `FAIL`.
 *
 *     Uses configured "curl easy handle" to perform request.
 *
 *     In event of error, buffer should remain unaltered.
 *
 *     If handle is set to authorize a request, creates a new (temporary)
 *     HTTP Request object (hrb_t) for generating requisite headers,
 *     which is then translated to a `curl slist` and set in the curl handle
 *     for the request.
 *
 *     `dest` _may_ be NULL, but no body data will be recorded.
 *
 *     - In general practice, NULL should never be passed in as `dest`.
 *     - NULL `dest` passed in by internal function `s3r_getsize()`, in
 *       conjunction with CURLOPT_NOBODY to preempt transmission of file data
 *       from server.
 *
 * Return:      SUCCEED/FAIL
 *----------------------------------------------------------------------------
 */
herr_t
H5FD__s3comms_s3r_read(s3r_t *handle, haddr_t offset, size_t len, void *dest)
{
    CURL                  *curlh          = NULL;
    CURLcode               p_status       = CURLE_OK;
    struct curl_slist     *curlheaders    = NULL;
    hrb_node_t            *headers        = NULL;
    hrb_node_t            *node           = NULL;
    char                  *rangebytesstr  = NULL;
    hrb_t                 *request        = NULL;
    char                  *authorization  = NULL;
    char                  *buffer1        = NULL;
    char                  *signed_headers = NULL;
    struct s3r_datastruct *sds            = NULL;
    int                    ret            = 0;
    herr_t                 ret_value      = SUCCEED;

    FUNC_ENTER_PACKAGE

    /**************************************
     * ABSOLUTELY NECESSARY SANITY-CHECKS *
     **************************************/

    if (handle == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "handle cannot be NULL");
    if (handle->curlhandle == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "handle has bad (NULL) curlhandle");
    if (handle->purl == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "handle has bad (NULL) url");
    if (offset > handle->filesize || (len + offset) > handle->filesize)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to read past EOF");

    curlh = handle->curlhandle;

    /*********************
     * PREPARE WRITEDATA *
     *********************/

    if (dest != NULL) {
        if (NULL == (sds = (struct s3r_datastruct *)H5MM_malloc(sizeof(struct s3r_datastruct))))
            HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL, "could not malloc destination datastructure");

        sds->data = (char *)dest;
        sds->size = 0;
        if (CURLE_OK != curl_easy_setopt(curlh, CURLOPT_WRITEDATA, sds))
            HGOTO_ERROR(H5E_VFL, H5E_UNINITIALIZED, FAIL,
                        "error while setting CURL option (CURLOPT_WRITEDATA)");
    }

    /*********************
     * FORMAT HTTP RANGE *
     *********************/

    if (len > 0) {
        if (NULL == (rangebytesstr = (char *)H5MM_malloc(sizeof(char) * (S3COMMS_MAX_RANGE_STRING_SIZE + 1))))
            HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL, "could not malloc range format string");
        ret = snprintf(rangebytesstr, (S3COMMS_MAX_RANGE_STRING_SIZE), "bytes=%" PRIuHADDR "-%" PRIuHADDR,
                       offset, offset + len - 1);
        if (ret <= 0 || ret >= S3COMMS_MAX_RANGE_STRING_SIZE)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "unable to format HTTP Range value");
    }
    else if (offset > 0) {
        if (NULL == (rangebytesstr = (char *)H5MM_malloc(sizeof(char) * (S3COMMS_MAX_RANGE_STRING_SIZE + 1))))
            HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL, "could not malloc range format string.");
        ret = snprintf(rangebytesstr, (S3COMMS_MAX_RANGE_STRING_SIZE), "bytes=%" PRIuHADDR "-", offset);
        if (ret <= 0 || ret >= S3COMMS_MAX_RANGE_STRING_SIZE)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "unable to format HTTP Range value");
    }

#if S3COMMS_CURL_VERBOSITY > 0
    fprintf(stdout, "%s: Bytes %" PRIuHADDR " - %" PRIuHADDR ", Request Size: %zu\n", handle->http_verb,
            offset, offset + len - 1, len);
    fflush(stdout);
#endif

    /*******************
     * COMPILE REQUEST *
     *******************/

    if (handle->signing_key == NULL) {
        /* Do not authenticate.  */
        if (rangebytesstr != NULL) {
            /* Pass in range directly */
            char *bytesrange_ptr = NULL; /* pointer past "bytes=" portion */

            bytesrange_ptr = strchr(rangebytesstr, '=');
            assert(bytesrange_ptr != NULL);
            bytesrange_ptr++; /* move to first char past '=' */
            assert(*bytesrange_ptr != '\0');

            if (CURLE_OK != curl_easy_setopt(curlh, CURLOPT_RANGE, bytesrange_ptr))
                HGOTO_ERROR(H5E_VFL, H5E_UNINITIALIZED, FAIL,
                            "error while setting CURL option (CURLOPT_RANGE)");
        }
    }
    else {
        unsigned char md[SHA256_DIGEST_LENGTH];
        unsigned int  md_len = SHA256_DIGEST_LENGTH;

        /*   4608 := approximate max length...
         *     67 <len("AWS4-HMAC-SHA256 Credential=///s3/aws4_request,"
         *             "SignedHeaders=,Signature=")>
         * +    8 <yyyyMMDD>
         * +   64 <hex(sha256())>
         * +  128 <max? len(secret_id)>
         * +   20 <max? len(region)>
         * +  128 <max? len(signed_headers)>
         * + 4096 <max? len(session_token)>
         */
        char buffer2[256 + 1]; /* -> String To Sign -> Credential */
        char iso8601[ISO8601_SIZE];

        /* Authenticate request */
        authorization = (char *)H5MM_malloc(512 + H5FD_ROS3_MAX_SECRET_TOK_LEN + 1);
        if (authorization == NULL)
            HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, FAIL, "cannot make space for authorization variable");

        /* -> Canonical Request -> Signature */
        buffer1 = (char *)H5MM_malloc(512 + H5FD_ROS3_MAX_SECRET_TOK_LEN + 1);
        if (buffer1 == NULL)
            HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, FAIL, "cannot make space for buffer1 variable");
        signed_headers = (char *)H5MM_malloc(48 + H5FD_ROS3_MAX_SECRET_KEY_LEN + 1);
        if (signed_headers == NULL)
            HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, FAIL, "cannot make space for signed_headers variable");

        /* Should be large enough for nominal listing:
         * "host;range;x-amz-content-sha256;x-amz-date;x-amz-security-token"
         * + '\0', with "range;" and/or "x-amz-security-token" possibly absent
         */

        /* Zero start of strings */
        authorization[0]  = '\0';
        buffer1[0]        = '\0';
        buffer2[0]        = '\0';
        iso8601[0]        = '\0';
        signed_headers[0] = '\0';

        /**** VERIFY INFORMATION EXISTS ****/

        if (handle->aws_region == NULL)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "handle must have non-NULL region");
        if (handle->secret_id == NULL)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "handle must have non-NULL secret_id");
        if (handle->signing_key == NULL)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "handle must have non-NULL signing_key");
        if (handle->token == NULL)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "handle must have non-NULL token");
        if (handle->http_verb == NULL)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "handle must have non-NULL http_verb");
        if (handle->purl->host == NULL)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "handle must have non-NULL host");
        if (handle->purl->path == NULL)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "handle must have non-NULL resource");

        /**** CREATE HTTP REQUEST STRUCTURE (hrb_t) ****/

        request = H5FD__s3comms_hrb_init_request((const char *)handle->http_verb,
                                                 (const char *)handle->purl->path, "HTTP/1.1");
        if (request == NULL)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "could not allocate hrb_t request");

        /* Get a time string for the current time in ISO-8601 format */
        if (H5FD__s3comms_make_iso_8661_string(time(NULL), iso8601) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "could not format ISO-8601 time");

        if (H5FD__s3comms_hrb_node_set(&headers, "x-amz-date", (const char *)iso8601) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "unable to set x-amz-date header");
        if (headers == NULL)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "problem building headers list");

        if (H5FD__s3comms_hrb_node_set(&headers, "x-amz-content-sha256", (const char *)EMPTY_SHA256) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "unable to set x-amz-content-sha256 header");
        if (headers == NULL)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "problem building headers list");

        if (strlen((const char *)handle->token) > 0) {
            if (H5FD__s3comms_hrb_node_set(&headers, "x-amz-security-token", (const char *)handle->token) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "unable to set x-amz-security-token header");
            if (headers == NULL)
                HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "problem building headers list");
        }

        if (rangebytesstr != NULL) {
            if (H5FD__s3comms_hrb_node_set(&headers, "Range", rangebytesstr) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "unable to set range header");
            if (headers == NULL)
                HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "problem building headers list");
        }

        if (H5FD__s3comms_hrb_node_set(&headers, "Host", handle->purl->host) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "unable to set host header");
        if (headers == NULL)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "problem building headers list");

        request->first_header = headers;

        /**** COMPUTE AUTHORIZATION ****/

        /* buffer1 -> canonical request */
        if (H5FD__s3comms_make_aws_canonical_request(buffer1, 512 + H5FD_ROS3_MAX_SECRET_TOK_LEN,
                                                     signed_headers, 48 + H5FD_ROS3_MAX_SECRET_TOK_LEN,
                                                     request) < 0) {
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "bad canonical request");
        }
        /* buffer2->string-to-sign */
        if (H5FD__s3comms_make_aws_stringtosign(buffer2, buffer1, iso8601, handle->aws_region) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "bad string-to-sign");

        /* buffer1 -> signature */
        HMAC(EVP_sha256(), handle->signing_key, SHA256_DIGEST_LENGTH, (const unsigned char *)buffer2,
             strlen(buffer2), md, &md_len);
        if (H5FD__s3comms_bytes_to_hex(buffer1, 512 + H5FD_ROS3_MAX_SECRET_TOK_LEN + 1,
                                       (const unsigned char *)md, (size_t)md_len) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "could not convert to hex string.");

        /* Trim to yyyyMMDD */
        iso8601[8] = 0;

        ret = S3COMMS_FORMAT_CREDENTIAL(buffer2, handle->secret_id, iso8601, handle->aws_region, "s3");
        if (ret == 0 || ret >= S3COMMS_MAX_CREDENTIAL_SIZE)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "unable to format aws4 credential string");

        ret = snprintf(authorization, 512 + H5FD_ROS3_MAX_SECRET_TOK_LEN,
                       "AWS4-HMAC-SHA256 Credential=%s,SignedHeaders=%s,Signature=%s", buffer2,
                       signed_headers, buffer1);
        if (ret <= 0 || ret >= 512 + H5FD_ROS3_MAX_SECRET_TOK_LEN)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "unable to format aws4 authorization string");

        /* Append authorization header to http request buffer */
        if (H5FD__s3comms_hrb_node_set(&headers, "Authorization", (const char *)authorization) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "unable to set Authorization header");
        if (headers == NULL)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "problem building headers list");

        /* Update hrb's "first header" pointer */
        request->first_header = headers;

        /**** SET CURLHANDLE HTTP HEADERS FROM GENERATED DATA ****/

        node = request->first_header;
        while (node != NULL) {
            curlheaders = curl_slist_append(curlheaders, (const char *)node->cat);
            if (curlheaders == NULL)
                HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "could not append header to curl slist");
            node = node->next;
        }

        /* Sanity-check */
        if (curlheaders == NULL)
            /* above loop was probably never run */
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "curlheaders was never populated");

        /* Finally, set http headers in curl handle */
        if (curl_easy_setopt(curlh, CURLOPT_HTTPHEADER, curlheaders) != CURLE_OK)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "error while setting CURL option (CURLOPT_HTTPHEADER)");
    } /* end if should authenticate (info provided) */

    /*******************
     * PERFORM REQUEST *
     *******************/

#if S3COMMS_CURL_VERBOSITY > 0
    /* In event of error, print detailed information to stderr
     * This is not the default behavior.
     */
    {
        long int httpcode = 0;
        char     curlerrbuf[CURL_ERROR_SIZE];
        curlerrbuf[0] = '\0';

        if (CURLE_OK != curl_easy_setopt(curlh, CURLOPT_ERRORBUFFER, curlerrbuf))
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "problem setting error buffer");

        p_status = curl_easy_perform(curlh);

        if (p_status != CURLE_OK) {
            if (CURLE_OK != curl_easy_getinfo(curlh, CURLINFO_RESPONSE_CODE, &httpcode))
                HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "problem getting response code");
            fprintf(stdout, "CURL ERROR CODE: %d\nHTTP CODE: %ld\n", p_status, httpcode);
            fprintf(stdout, "%s\n", curl_easy_strerror(p_status));

            HGOTO_ERROR(H5E_VFL, H5E_CANTOPENFILE, FAIL, "problem while performing request");
        }
        if (CURLE_OK != curl_easy_setopt(curlh, CURLOPT_ERRORBUFFER, NULL))
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "problem unsetting error buffer");
    }
#else
    p_status = curl_easy_perform(curlh);

    if (p_status != CURLE_OK)
        HGOTO_ERROR(H5E_VFL, H5E_CANTOPENFILE, FAIL, "curl cannot perform request");
#endif

done:
    H5MM_xfree(authorization);
    H5MM_xfree(buffer1);
    H5MM_xfree(signed_headers);
    H5MM_xfree(rangebytesstr);
    H5MM_xfree(sds);

    if (curlheaders != NULL)
        curl_slist_free_all(curlheaders);
    if (request != NULL) {
        while (headers != NULL)
            if (H5FD__s3comms_hrb_node_set(&headers, headers->name, NULL) < 0)
                HDONE_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "cannot release header node");
        assert(NULL == headers);
        if (H5FD__s3comms_hrb_destroy(request) < 0)
            HDONE_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "cannot release header request structure");
    }

    if (curlh != NULL) {
        /* Clear any Range */
        if (CURLE_OK != curl_easy_setopt(curlh, CURLOPT_RANGE, NULL))
            HDONE_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "cannot unset CURLOPT_RANGE");

        /* Clear headers */
        if (CURLE_OK != curl_easy_setopt(curlh, CURLOPT_HTTPHEADER, NULL))
            HDONE_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "cannot unset CURLOPT_HTTPHEADER");
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD__s3comms_s3r_read */

/****************************************************************************
 * MISCELLANEOUS FUNCTIONS
 ****************************************************************************/

/*----------------------------------------------------------------------------
 * Function:    H5FD__s3comms_make_iso_8661_string
 *
 * Purpose:     Create an ISO-8601 string from a time_t
 *
 * Return:      SUCCEED/FAIL
 *----------------------------------------------------------------------------
 */
static herr_t
H5FD__s3comms_make_iso_8661_string(time_t time, char iso8601[ISO8601_SIZE])
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(iso8601);

    if (strftime(iso8601, ISO8601_SIZE, "%Y%m%dT%H%M%SZ", gmtime(&time)) != (ISO8601_SIZE - 1))
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "could not construct ISO-8601 string");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__s3comms_make_iso_8661_string() */

/*----------------------------------------------------------------------------
 * Function:    H5FD__s3comms_make_aws_canonical_request
 *
 * Purpose:     Compose AWS "Canonical Request" (and signed headers string)
 *              as defined in the REST API documentation.
 *
 *              NOTE: Destination string arguments must be provided with
 *                    adequate space
 *
 *              Canonical Request format:
 *
 *              <HTTP VERB>"\n"
 *              <resource path>"\n"
 *              <query string>"\n"
 *              <header1>"\n" (`lowercase(name)`":"`trim(value)`)
 *              <header2>"\n"
 *              ... (headers sorted by name)
 *              <header_n>"\n"
 *              "\n"
 *              <signed headers>"\n" (`lowercase(header 1 name)`";"`header 2 name`;...)
 *              <hex-string of sha256sum of body> ("e3b0c4429...", e.g.)
 *
 * Return:      SUCCEED/FAIL
 *----------------------------------------------------------------------------
 */
herr_t
H5FD__s3comms_make_aws_canonical_request(char *canonical_request_dest, int _cr_size,
                                         char *signed_headers_dest, int _sh_size, hrb_t *http_request)
{
    hrb_node_t  *node         = NULL;
    const char  *query_params = ""; /* unused at present */
    int          ret          = 0;
    size_t       cr_size      = (size_t)_cr_size;
    size_t       sh_size      = (size_t)_sh_size;
    size_t       cr_len       = 0; /* working length of canonical request str */
    size_t       sh_len       = 0; /* working length of signed headers str */
    char        *tmpstr       = NULL;
    const size_t TMP_STR_SIZE = sizeof(char) * H5FD_ROS3_MAX_SECRET_TOK_LEN;
    herr_t       ret_value    = SUCCEED;

    /* "query params" refers to the optional element in the URL, e.g.
     *     http://bucket.aws.com/myfile.txt?max-keys=2&prefix=J
     *                                      ^-----------------^
     *
     * Not handled/implemented as of 2017-10-xx.
     * Element introduced as empty placeholder and reminder.
     * Further research to be done if this is ever relevant for the
     * VFD use-cases.
     */

    FUNC_ENTER_PACKAGE

    if (http_request == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "hrb object cannot be NULL");
    if (canonical_request_dest == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "canonical request destination cannot be NULL");
    if (signed_headers_dest == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "signed headers destination cannot be NULL");

    /* HTTP verb, resource path, and query string lines */
    cr_len = (strlen(http_request->verb) + strlen(http_request->resource) + strlen(query_params) +
              (size_t)3); /* three newline chars */
    if (cr_len >= cr_size)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "not enough space in canonical request");

    ret = snprintf(canonical_request_dest, (cr_size - 1), "%s\n%s\n%s\n", http_request->verb,
                   http_request->resource, query_params);
    if (ret < 0 || (size_t)ret >= cr_size)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "unable to compose canonical request first line");

    if (NULL == (tmpstr = (char *)H5MM_calloc(TMP_STR_SIZE)))
        HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, FAIL, "unable to allocate space for temp string");

    /* Write in canonical headers, building signed headers concurrently */
    node = http_request->first_header; /* Assumed sorted */
    while (node != NULL) {

        ret = snprintf(tmpstr, TMP_STR_SIZE, "%s:%s\n", node->lowername, node->value);
        if (ret < 0 || ret >= (int)TMP_STR_SIZE)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "unable to concatenate HTTP header %s:%s",
                        node->lowername, node->value);
        cr_len += strlen(tmpstr);
        if (cr_len + 1 > cr_size)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "not enough space in canonical request");
        strcat(canonical_request_dest, tmpstr);

        ret = snprintf(tmpstr, TMP_STR_SIZE, "%s;", node->lowername);
        if (ret < 0 || ret >= (int)TMP_STR_SIZE)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "unable to append semicolon to lowername %s",
                        node->lowername);
        sh_len += strlen(tmpstr);
        if (sh_len + 1 > sh_size)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "not enough space in signed headers");
        strcat(signed_headers_dest, tmpstr);

        node = node->next;
    }

    /* Remove trailing ';' from signed headers sequence */
    if (*signed_headers_dest != '\0')
        signed_headers_dest[strlen(signed_headers_dest) - 1] = '\0';

    /* Append signed headers and payload hash
     * (no HTTP body is handled, per the nature of requests/range-gets)
     */
    strcat(canonical_request_dest, "\n");
    strcat(canonical_request_dest, signed_headers_dest);
    strcat(canonical_request_dest, "\n");
    strcat(canonical_request_dest, EMPTY_SHA256);

done:
    free(tmpstr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__s3comms_make_aws_canonical_request() */

/*----------------------------------------------------------------------------
 * Function:    H5FD__s3comms_bytes_to_hex
 *
 * Purpose:     Create a NUL-terminated hex string from a byte array
 *
 * Return:      SUCCEED/FAIL
 *----------------------------------------------------------------------------
 */
static herr_t
H5FD__s3comms_bytes_to_hex(char *dest, size_t dest_len, const unsigned char *msg, size_t msg_len)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(dest);
    assert(msg);

    memset(dest, 0, dest_len);

    if (0 == (OPENSSL_buf2hexstr_ex(dest, dest_len, NULL, msg, msg_len, '\0')))
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "could not create hex string");

    /* AWS demands lower-case and buf2hexstr returns an upper-case hex string */
    for (size_t i = 0; i < dest_len; i++)
        dest[i] = (char)tolower(dest[i]);

    dest[dest_len - 1] = '\0';

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__s3comms_bytes_to_hex() */

/*----------------------------------------------------------------------------
 * Function:    H5FD__s3comms_parse_url
 *
 * Purpose:     Release resources from a parsed_url_t pointer
 *
 * Return:      Success:    A pointer to a parsed_url_t
 *              Failure:    NULL
 *----------------------------------------------------------------------------
 */
static parsed_url_t *
H5FD__s3comms_parse_url(const char *url)
{
    CURLUcode     rc;
    CURLU        *curlurl   = NULL;
    parsed_url_t *purl      = NULL;
    parsed_url_t *ret_value = NULL;

    FUNC_ENTER_PACKAGE

    assert(url);

    /* Get a curl URL handle */
    if (NULL == (curlurl = curl_url()))
        HGOTO_ERROR(H5E_VFL, H5E_CANTCREATE, NULL, "unable to get curl url");

    /* Separate the URL into parts using libcurl */
    if (CURLUE_OK != curl_url_set(curlurl, CURLUPART_URL, url, 0))
        HGOTO_ERROR(H5E_VFL, H5E_CANTCREATE, NULL, "unable to parse url");

    /* Allocate memory for the parsed URL to return */
    if (NULL == (purl = (parsed_url_t *)H5MM_calloc(sizeof(parsed_url_t))))
        HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, NULL, "can't allocate space for parsed_url_t");

    /* Extract the URL components using libcurl */

    /* scheme */
    rc = curl_url_get(curlurl, CURLUPART_SCHEME, &(purl->scheme), 0);
    if (CURLUE_OK != rc)
        HGOTO_ERROR(H5E_VFL, H5E_CANTCREATE, NULL, "unable to get url scheme");
    /* host */
    rc = curl_url_get(curlurl, CURLUPART_HOST, &(purl->host), 0);
    if (CURLUE_OK != rc)
        HGOTO_ERROR(H5E_VFL, H5E_CANTCREATE, NULL, "unable to get url host");
    /* port - okay to not exist */
    rc = curl_url_get(curlurl, CURLUPART_PORT, &(purl->port), 0);
    if (CURLUE_OK != rc && CURLUE_NO_PORT != rc)
        HGOTO_ERROR(H5E_VFL, H5E_CANTCREATE, NULL, "unable to get url port");
    /* path */
    rc = curl_url_get(curlurl, CURLUPART_PATH, &(purl->path), 0);
    if (CURLUE_OK != rc)
        HGOTO_ERROR(H5E_VFL, H5E_CANTCREATE, NULL, "unable to get url path");
    /* query - okay to not exist */
    rc = curl_url_get(curlurl, CURLUPART_QUERY, &(purl->query), 0);
    if (CURLUE_OK != rc && CURLUE_NO_QUERY != rc)
        HGOTO_ERROR(H5E_VFL, H5E_CANTCREATE, NULL, "unable to get url query");

    ret_value = purl;

done:
    curl_url_cleanup(curlurl);

    if (ret_value == NULL) {
        if (H5FD__s3comms_free_purl(purl) < 0)
            HDONE_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "unable to free parsed url structure");
        H5MM_xfree(purl);
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__s3comms_parse_url() */

/*----------------------------------------------------------------------------
 * Function:    H5FD__s3comms_free_purl
 *
 * Purpose:     Release resources from a parsed_url_t pointer
 *
 * Return:      SUCCEED (Can't fail - passing NULL is okay)
 *----------------------------------------------------------------------------
 */
static herr_t
H5FD__s3comms_free_purl(parsed_url_t *purl)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE_NOERR

    if (NULL == purl)
        HGOTO_DONE(SUCCEED);

    curl_free(purl->scheme);
    curl_free(purl->host);
    curl_free(purl->port);
    curl_free(purl->path);
    curl_free(purl->query);

    H5MM_xfree(purl);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__s3comms_free_purl() */

/*-----------------------------------------------------------------------------
 * Function:    H5FD__s3comms_load_aws_creds_from_file
 *
 * Purpose:     Extract AWS configuration information from a target file
 *
 *     Given a file and a profile name, e.g. "ros3_vfd_test", attempt to locate
 *     that region in the file. If not found, returns in error and output
 *     pointers are not modified.
 *
 *     Following AWS documentation, looks for any of:
 *     + aws_access_key_id
 *     + aws_secret_access_key
 *     + region
 *
 *     Upon successful parsing of a setting line, will store the result in the
 *     corresponding output pointer. If the output pointer is NULL, will skip
 *     any matching setting line while parsing -- useful to prevent overwrite
 *     when reading from multiple files.
 *
 * Return:      SUCCEED/FAIL
 *-----------------------------------------------------------------------------
 */
static herr_t
H5FD__s3comms_load_aws_creds_from_file(FILE *file, const char *profile_name, char *key_id, char *access_key,
                                       char *aws_region)
{
    char        profile_line[32];
    char        buffer[128];
    const char *setting_names[] = {
        "region",
        "aws_access_key_id",
        "aws_secret_access_key",
    };
    char *const setting_pointers[] = {
        aws_region,
        key_id,
        access_key,
    };
    unsigned setting_count = 3;
    herr_t   ret_value     = SUCCEED;
    unsigned setting_i     = 0;
    int      found_setting = 0;
    char    *line_buffer   = &(buffer[0]);
    size_t   end           = 0;

    FUNC_ENTER_PACKAGE

    /* Format target line for start of profile */
    if (32 < snprintf(profile_line, 32, "[%s]", profile_name))
        HGOTO_ERROR(H5E_VFL, H5E_CANTCOPY, FAIL, "unable to format profile label");

    /* Look for start of profile */
    do {
        memset(buffer, 0, 128);

        line_buffer = fgets(line_buffer, 128, file);
        if (line_buffer == NULL)
            goto done;
    } while (strncmp(line_buffer, profile_line, strlen(profile_line)));

    /* Extract credentials from lines */
    do {
        memset(buffer, 0, 128);
        found_setting = 0;

        line_buffer = fgets(line_buffer, 128, file);
        if (line_buffer == NULL)
            goto done; /* end of file */

        /* Loop over names to see if line looks like assignment */
        for (setting_i = 0; setting_i < setting_count; setting_i++) {
            size_t      setting_name_len = 0;
            const char *setting_name     = NULL;
            char        line_prefix[128];

            setting_name     = setting_names[setting_i];
            setting_name_len = strlen(setting_name);
            if (snprintf(line_prefix, 128, "%s=", setting_name) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_CANTCOPY, FAIL, "unable to format line prefix");

            /* Found a matching name? */
            if (!strncmp(line_buffer, line_prefix, setting_name_len + 1)) {
                found_setting = 1;

                /* Skip NULL destination buffer */
                if (setting_pointers[setting_i] == NULL)
                    break;

                /* Advance to end of name in string */
                do {
                    line_buffer++;
                } while (*line_buffer != 0 && *line_buffer != '=');

                if (*line_buffer == 0 || *(line_buffer + 1) == 0)
                    HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "incomplete assignment in file");
                line_buffer++; /* Was pointing at '='; advance */

                /* Copy line buffer into out pointer */
                strncpy(setting_pointers[setting_i], (const char *)line_buffer, strlen(line_buffer));

                /* "Trim" tailing whitespace by replacing with NUL terminator*/
                end = strlen(line_buffer) - 1;
                while (end > 0 && isspace((int)setting_pointers[setting_i][end])) {
                    setting_pointers[setting_i][end] = '\0';
                    end--;
                }

                break; /* have read setting; don't compare with others */
            }          /* end if possible name match */
        }              /* end for each setting name */
    } while (found_setting);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__s3comms_load_aws_creds_from_file() */

/*----------------------------------------------------------------------------
 * Function:    H5FD__s3comms_load_aws_profile
 *
 * Purpose:     Read AWS profile elements from ~/.aws/config and credentials
 *
 * Return:      SUCCEED/FAIL
 *----------------------------------------------------------------------------
 */
herr_t
H5FD__s3comms_load_aws_profile(const char *profile_name, char *key_id_out, char *secret_access_key_out,
                               char *aws_region_out)
{
    herr_t ret_value = SUCCEED;
    FILE  *credfile  = NULL;
    char   awspath[117];
    char   filepath[128];
    int    ret = 0;

    FUNC_ENTER_PACKAGE

#ifdef H5_HAVE_WIN32_API
    ret = snprintf(awspath, 117, "%s/.aws/", getenv("USERPROFILE"));
#else
    ret = snprintf(awspath, 117, "%s/.aws/", getenv("HOME"));
#endif
    if (ret < 0 || (size_t)ret >= 117)
        HGOTO_ERROR(H5E_VFL, H5E_CANTCOPY, FAIL, "unable to format home-aws path");
    ret = snprintf(filepath, 128, "%s%s", awspath, "credentials");
    if (ret < 0 || (size_t)ret >= 128)
        HGOTO_ERROR(H5E_VFL, H5E_CANTCOPY, FAIL, "unable to format credentials path");

    /* Looks for both `~/.aws/config` and `~/.aws/credentials`, the standard
     * files for AWS tools. If a file exists (can be opened), looks for the
     * given profile name and reads the settings into the relevant buffer.
     *
     * Any setting duplicated in both files will be set to that from
     * credentials
     */

    credfile = fopen(filepath, "r");
    if (credfile != NULL) {
        if (H5FD__s3comms_load_aws_creds_from_file(credfile, profile_name, key_id_out, secret_access_key_out,
                                                   aws_region_out) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "unable to load from aws credentials");
        if (fclose(credfile) == EOF)
            HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "unable to close credentials file");
        credfile = NULL;
    }

    ret = snprintf(filepath, 128, "%s%s", awspath, "config");
    if (ret < 0 || (size_t)ret >= 128)
        HGOTO_ERROR(H5E_VFL, H5E_CANTCOPY, FAIL, "unable to format config path");

    credfile = fopen(filepath, "r");
    if (credfile != NULL) {
        if (H5FD__s3comms_load_aws_creds_from_file(
                credfile, profile_name, (*key_id_out == 0) ? key_id_out : NULL,
                (*secret_access_key_out == 0) ? secret_access_key_out : NULL,
                (*aws_region_out == 0) ? aws_region_out : NULL) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "unable to load from aws config");
        if (fclose(credfile) == EOF)
            HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "unable to close config file");
        credfile = NULL;
    }

    /* Fail if not all three settings were loaded */
    if (*key_id_out == 0 || *secret_access_key_out == 0 || *aws_region_out == 0)
        ret_value = FAIL;

done:
    if (credfile != NULL)
        if (fclose(credfile) == EOF)
            HDONE_ERROR(H5E_VFL, H5E_VFL, FAIL, "problem error-closing aws configuration file");

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__s3comms_load_aws_profile() */

/*----------------------------------------------------------------------------
 * Function:    H5FD__s3comms_make_aws_signing_key
 *
 * Purpose:     Create AWS4 "Signing Key" from secret key, AWS region, and
 *              timestamp
 *
 *              `secret` is `access key id` for targeted service/bucket/resource
 *
 *              `region` should be one of AWS service region names, e.g. "us-east-1"
 *
 *              `iso8601` is an ISO-8601 time string with no punctuation
 *              (e.g.: "20170713T145903Z", so... YYYYmmdd'T'HHMMSSZ).
 *              This can be constructed using H5FD__s3comms_make_iso_8661_string().
 *
 *              Hard-coded "service" algorithm requirement to "s3"
 *
 *              Writes to `md` the raw byte data, length of `SHA256_DIGEST_LENGTH`.
 *              Programmer must ensure that `md` is appropriately allocated.
 *
 * Return:      SUCCEED/FAIL
 *----------------------------------------------------------------------------
 */
herr_t
H5FD__s3comms_make_aws_signing_key(unsigned char *md, const char *secret, const char *region,
                                   const char *iso8601)
{
    char         *AWS4_secret     = NULL;
    size_t        AWS4_secret_len = 0;
    unsigned char datekey[SHA256_DIGEST_LENGTH];
    unsigned char dateregionkey[SHA256_DIGEST_LENGTH];
    unsigned char dateregionservicekey[SHA256_DIGEST_LENGTH];
    int           ret       = 0; /* return value of snprintf */
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    if (md == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Destination `md` cannot be NULL");
    if (secret == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "`secret` cannot be NULL");
    if (region == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "`region` cannot be NULL");

    AWS4_secret_len = 4 + strlen(secret) + 1;
    AWS4_secret     = (char *)H5MM_malloc(AWS4_secret_len);
    if (AWS4_secret == NULL)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "Could not allocate space");

    /* Prepend "AWS4" to start of the secret key */
    ret = snprintf(AWS4_secret, AWS4_secret_len, "%s%s", "AWS4", secret);
    if ((size_t)ret != (AWS4_secret_len - 1))
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "problem writing AWS4+secret `%s`", secret);

    /* Sequentially runs HMAC_SHA256 on strings in specified order,
     * generating reusable checksum (according to documentation, valid for
     * 7 days from time given)
     *
     * hash_func, key, len(key), msg, len(msg), digest_dest, digest_len_dest
     * we know digest length, so ignore via NULL
     */
    HMAC(EVP_sha256(), (const unsigned char *)AWS4_secret, (int)strlen(AWS4_secret),
         (const unsigned char *)iso8601, 8, /* 8 --> length of 8 --> "yyyyMMDD"  */
         datekey, NULL);

    HMAC(EVP_sha256(), (const unsigned char *)datekey, SHA256_DIGEST_LENGTH, (const unsigned char *)region,
         strlen(region), dateregionkey, NULL);

    HMAC(EVP_sha256(), (const unsigned char *)dateregionkey, SHA256_DIGEST_LENGTH,
         (const unsigned char *)"s3", 2, dateregionservicekey, NULL);

    HMAC(EVP_sha256(), (const unsigned char *)dateregionservicekey, SHA256_DIGEST_LENGTH,
         (const unsigned char *)"aws4_request", 12, md, NULL);

done:
    H5MM_xfree(AWS4_secret);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__s3comms_make_aws_signing_key() */

/*----------------------------------------------------------------------------
 *
 * Function: H5FD__s3comms_make_aws_stringtosign()
 *
 * Purpose:
 *
 *     Get AWS "String to Sign" from Canonical Request, timestamp,
 *     and AWS "region".
 *
 *     Common between single request and "chunked upload",
 *     conforms to:
 *         "AWS4-HMAC-SHA256\n" +
 *         <ISO8601 date format> + "\n" +  // yyyyMMDD'T'hhmmss'Z'
 *         <yyyyMMDD> + "/" + <AWS Region> + "/s3/aws4-request\n" +
 *         hex(SHA256(<CANONICAL-REQUEST>))
 *
 *     Inputs `creq` (canonical request string), `now` (ISO8601 format),
 *     and `region` (s3 region designator string) must all be
 *     NUL-terminated strings.
 *
 *     Result is written to `dest` with NUL-terminator.
 *     It is left to programmer to ensure `dest` has adequate space.
 *
 * Return:
 *
 *     - SUCCESS: `SUCCEED`
 *         - "string to sign" written to `dest` and NUL-terminated
 *     - FAILURE: `FAIL`
 *         - if any of the inputs are NULL
 *         - if an error is encountered while computing checksum
 *
 *----------------------------------------------------------------------------
 */
herr_t
H5FD__s3comms_make_aws_stringtosign(char *dest, const char *req, const char *now, const char *region)
{
    unsigned char checksum[S3COMMS_SHA256_HEXSTR_LENGTH];
    size_t        d = 0;
    char          day[9];
    char          hexsum[S3COMMS_SHA256_HEXSTR_LENGTH];
    size_t        i   = 0;
    int           ret = 0; /* snprintf return value */
    char          tmp[128];
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    if (dest == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "destination buffer cannot be NULL");
    if (req == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "canonical request cannot be NULL");
    if (now == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Timestring cannot be NULL");
    if (region == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Region cannot be NULL");

    for (i = 0; i < 128; i++)
        tmp[i] = '\0';
    for (i = 0; i < S3COMMS_SHA256_HEXSTR_LENGTH; i++) {
        checksum[i] = '\0';
        hexsum[i]   = '\0';
    }
    strncpy(day, now, 8);
    day[8] = '\0';
    ret    = snprintf(tmp, 127, "%s/%s/s3/aws4_request", day, region);
    if (ret <= 0 || ret >= 127)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "problem adding day and region to string");

    H5MM_memcpy((dest + d), "AWS4-HMAC-SHA256\n", 17);
    d = 17;

    H5MM_memcpy((dest + d), now, strlen(now));
    d += strlen(now);
    dest[d++] = '\n';

    H5MM_memcpy((dest + d), tmp, strlen(tmp));
    d += strlen(tmp);
    dest[d++] = '\n';

    SHA256((const unsigned char *)req, strlen(req), checksum);

    if (H5FD__s3comms_bytes_to_hex(hexsum, S3COMMS_SHA256_HEXSTR_LENGTH, (const unsigned char *)checksum,
                                   SHA256_DIGEST_LENGTH) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "could not create hex string");

    for (i = 0; i < S3COMMS_SHA256_HEXSTR_LENGTH - 1; i++)
        dest[d++] = hexsum[i];

    dest[d] = '\0';

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__s3comms_make_aws_stringtosign() */

#endif /* H5_HAVE_ROS3_VFD */
