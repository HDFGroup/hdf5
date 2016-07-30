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

/***********************************************************
*
* Test program:  file_image
*
* Test setting file images
*
*************************************************************/

#include "h5test.h"
#include "H5Fprivate.h" /* required to test property removals */
#define VERIFY(condition, string) do { if (!(condition)) FAIL_PUTS_ERROR(string) } while(0)

/* Values for callback bit field */
#define MALLOC      0x01
#define MEMCPY      0x02
#define REALLOC     0x04
#define FREE        0x08
#define UDATA_COPY  0x10
#define UDATA_FREE  0x20

#define RANK 2
#define DIM0 1024
#define DIM1 32
#define DSET_NAME "test_dset"

#define FAMILY_SIZE (2 * 1024)

#define USERBLOCK_SIZE 512

const char *FILENAME[] = {
    "file_image_core_test",
    NULL
};

/* need a second file name array, as the first file name array contains
 * files we don't want to delete on cleanup.
 */
const char *FILENAME2[] = {
    "sec2_get_file_image_test",
    "stdio_get_file_image_test",
    "core_get_file_image_test",
    "family_get_file_image_test",
    "multi_get_file_image_test",
    "split_get_file_image_test",
    "get_file_image_error_rejection_test",
    NULL
};

typedef struct {
    unsigned char used_callbacks;       /* Bitfield for tracking callbacks */
    H5FD_file_image_op_t malloc_src;    /* Source of file image callbacks */
    H5FD_file_image_op_t memcpy_src;
    H5FD_file_image_op_t realloc_src;
    H5FD_file_image_op_t free_src;
} udata_t;


/******************************************************************************
 * Function:    test_properties
 *
 * Purpose:     Tests that the file image properties (buffer pointer and length)
 *              are set properly. Image callbacks are not set in this test.
 *
 * Returns:     Success: 0
 *              Failure: 1
 *
 * Programmer:  Jacob Gruber
 *              Monday, August 22, 2011
 *
 ******************************************************************************
 */
static int
test_properties(void)
{
    hid_t   fapl_1 = -1;
    hid_t   fapl_2 = -1;
    char    *buffer = 0;
    int     count = 10; 
    void    *temp = 0;
    char    *temp2 = 0;
    int     i;   
    size_t  size;
    size_t  temp_size;
    int     retval = 1;

    TESTING("File image property list functions");
    
    /* Initialize file image buffer
     *
     * Note: this image will not contain a valid HDF5 file, as it complicates testing
     * property list functions. In the file driver tests further down, this will
     * not be the case.
     */
    size = (size_t)count * sizeof(char);
    buffer = (char *)HDmalloc(size);
    for(i = 0; i < count - 1; i++)
        buffer[i] = (char)(65 + i);
    buffer[count - 1] = '\0';

    /* Create fapl */
    if((fapl_1 = H5Pcreate(H5P_FILE_ACCESS)) < 0) FAIL_STACK_ERROR

    /* Get file image stuff */
    if(H5Pget_file_image(fapl_1, (void **)&temp, &temp_size) < 0) FAIL_STACK_ERROR

    /* Check default values */
    VERIFY(temp == NULL, "Default pointer is wrong");
    VERIFY(temp_size == 0, "Default size is wrong");

    /* Set file image stuff */
    if(H5Pset_file_image(fapl_1, (void *)buffer, size) < 0) FAIL_STACK_ERROR
    
    /* Get the same */
    if(H5Pget_file_image(fapl_1, (void **)&temp, &temp_size) < 0) FAIL_STACK_ERROR

    /* Check that sizes are the same, and that the buffers are identical but separate */
    VERIFY(temp != NULL, "temp is null!");
    VERIFY(temp_size == size, "Sizes of buffers don't match");
    VERIFY(temp != buffer, "Retrieved buffer is the same as original");
    VERIFY(0 == HDmemcmp(temp, buffer, size), "Buffers contain different data");

    /* Copy the fapl */
    if((fapl_2 = H5Pcopy(fapl_1)) < 0) FAIL_STACK_ERROR

    /* Get values from the new fapl */
    if(H5Pget_file_image(fapl_2, (void **)&temp2, &temp_size) < 0) FAIL_STACK_ERROR
    
    /* Check that sizes are the same, and that the buffers are identical but separate */
    VERIFY(temp_size == size,"Sizes of buffers don't match"); 
    VERIFY(temp2 != NULL,"Recieved buffer not set");
    VERIFY(temp2 != buffer, "Retrieved buffer is the same as original");
    VERIFY(temp2 != temp, "Retrieved buffer is the same as previously retrieved buffer");
    VERIFY(0 == HDmemcmp(temp2, buffer, size),"Buffers contain different data");

    retval = 0;

error:

    /* Close everything */
    if(H5Pclose(fapl_1) < 0) retval = 1;
    if(H5Pclose(fapl_2) < 0) retval = 1;
    HDfree(buffer);
    H5free_memory(temp);
    H5free_memory(temp2);

    if(retval == 0)
        PASSED();

    return retval;
} /* end test_properties() */


/******************************************************************************
 * Function:    malloc_cb
 *
 * Purpose:     This function allows calls to the malloc callback to be tracked.
 *
 * Returns:     The result of a standard malloc
 *
 * Programmer:  Jacob Gruber
 *              Monday, August 22, 2011
 *
 ******************************************************************************
 */
static void *
malloc_cb(size_t size, H5FD_file_image_op_t op, void *udata)
{
    udata_t *u = (udata_t *)udata;

    u->used_callbacks |= MALLOC;
    u->malloc_src = op;
    return HDmalloc(size);
}


/******************************************************************************
 * Function:    memcpy_cb
 *
 * Purpose:     This function allows calls to the memcpy callback to be tracked.
 *
 * Returns:     The result of a standard memcpy
 *
 * Programmer:  Jacob Gruber
 *              Monday, August 22, 2011
 *
 ******************************************************************************
 */
static void *
memcpy_cb(void *dest, const void *src, size_t size, H5FD_file_image_op_t op, void *udata)
{
    udata_t *u = (udata_t *)udata;

    u->used_callbacks |= MEMCPY;
    u->memcpy_src = op;
    return HDmemcpy(dest, src, size);
}


/******************************************************************************
 * Function:    realloc_cb
 *
 * Purpose:     This function allows calls to the realloc callback to be tracked.
 *
 * Returns:     The result of a standard realloc
 *
 * Programmer:  Jacob Gruber
 *              Monday, August 22, 2011
 *
 ******************************************************************************
 */
static void *
realloc_cb(void *ptr, size_t size, H5FD_file_image_op_t op, void *udata)
{
    udata_t *u = (udata_t *)udata;

    u->used_callbacks |= REALLOC;
    u->realloc_src = op;
    return HDrealloc(ptr,size);
}


/******************************************************************************
 * Function:    free_cb
 *
 * Purpose:     This function allows calls to the free callback to be tracked.
 *
 * Programmer:  Jacob Gruber
 *              Monday, August 22, 2011
 *
 ******************************************************************************
 */
static herr_t
free_cb(void *ptr, H5FD_file_image_op_t op, void *udata)
{
    udata_t *u = (udata_t *)udata;

    u->used_callbacks |= FREE;
    u->free_src = op;
    HDfree(ptr);
    return(SUCCEED);
}


/******************************************************************************
 * Function:    udata_copy_cb
 *
 * Purpose:     This function allows calls to the udata_copy callback to be tracked.
 *              No copying actualy takes place; it is easier to deal with only one
 *              instance of the udata.
 *
 * Returns:     A pointer to the same udata that was passed in.
 *
 * Programmer:  Jacob Gruber
 *              Monday, August 22, 2011
 *
 ******************************************************************************
 */
static void *
udata_copy_cb(void *udata)
{
    udata_t *u = (udata_t *)udata;

    u->used_callbacks |= UDATA_COPY;
    return udata;
}


/******************************************************************************
 * Function:    udata_free_cb
 *
 * Purpose:     This function allows calls to the udata_free callback to be tracked.
 *
 *              Note: this callback doesn't actually do anything. Since the
 *              udata_copy callback doesn't copy, only one instance of the udata
 *              is kept alive and such it must be freed explicitly at the end of the tests.
 *
 * Programmer:  Jacob Gruber
 *              Monday, August 22, 2011
 *
 ******************************************************************************
 */
static herr_t
udata_free_cb(void *udata)
{
    udata_t *u = (udata_t *)udata;

    u->used_callbacks |= UDATA_FREE;
    return(SUCCEED);
}


/******************************************************************************
 * Function:    reset_udata
 *
 * Purpose:     Resets the udata to default values. This facilitates storing only
 *              the results of a single operation in the udata.
 *
 * Programmer:  Jacob Gruber
 *              Monday, August 22, 2011
 *
 ******************************************************************************
 */
static void
reset_udata(udata_t *u)
{
    u->used_callbacks = 0;
    u->malloc_src = u->memcpy_src = u->realloc_src = u->free_src = H5FD_FILE_IMAGE_OP_NO_OP;
}


/******************************************************************************
 * Function:    test_callbacks
 *
 * Purpose:     Tests that callbacks are called properly in property list functions.
 *
 * Programmer:  Jacob Gruber
 *              Monday, August 22, 2011
 *
 ******************************************************************************
 */
static int
test_callbacks(void)
{
    H5FD_file_image_callbacks_t real_callbacks = {&malloc_cb, &memcpy_cb, &realloc_cb, 
    &free_cb, &udata_copy_cb, &udata_free_cb, NULL};
    H5FD_file_image_callbacks_t null_callbacks = {NULL, NULL, NULL, NULL, NULL, NULL, NULL};
    H5FD_file_image_callbacks_t callbacks;
    hid_t fapl_1;
    hid_t fapl_2;
    udata_t *udata;
    char *file_image;
    char *temp_file_image;
    int   count = 10;
    int   i;
    size_t size;
    size_t temp_size;

    TESTING("Callback use in property list operations");

    /* Allocate and initialize udata */
    udata = (udata_t *)HDmalloc(sizeof(udata_t));
    reset_udata(udata);

    /* copy the address of the user data into read_callbacks */
    real_callbacks.udata = (void *)udata;

    /* Allocate and initialize file image buffer */
    size = (size_t)count * sizeof(char);
    file_image = (char *)HDmalloc(size);
    for(i = 0; i < count - 1; i++)
        file_image[i] = (char)(65 + i);
    file_image[count - 1] = '\0';

    /* Create fapl */
    if((fapl_1 = H5Pcreate(H5P_FILE_ACCESS)) < 0) FAIL_STACK_ERROR

    /* Get file image stuff */
    callbacks = real_callbacks;
    if(H5Pget_file_image_callbacks(fapl_1, &callbacks) < 0) FAIL_STACK_ERROR

    /* Check default values */
    VERIFY(callbacks.image_malloc == NULL, "Default malloc callback is wrong");
    VERIFY(callbacks.image_memcpy == NULL, "Default memcpy callback is wrong");
    VERIFY(callbacks.image_realloc == NULL, "Default realloc callback is wrong");
    VERIFY(callbacks.image_free == NULL, "Default free callback is wrong");
    VERIFY(callbacks.udata_copy == NULL, "Default udata copy callback is wrong");
    VERIFY(callbacks.udata_free == NULL, "Default udata free callback is wrong");
    VERIFY(callbacks.udata == NULL, "Default udata is wrong");


    /* Set file image callbacks */
    callbacks = real_callbacks;
    if(H5Pset_file_image_callbacks(fapl_1, &callbacks) < 0) FAIL_STACK_ERROR

    /* Get file image callbacks */
    callbacks = null_callbacks;
    if(H5Pget_file_image_callbacks(fapl_1, &callbacks) < 0) FAIL_STACK_ERROR
    
    /* Verify values */
    VERIFY(callbacks.image_malloc == &malloc_cb, "malloc callback was not set or retrieved properly");   
    VERIFY(callbacks.image_memcpy == &memcpy_cb, "memcpy callback was not set or retrieved properly");
    VERIFY(callbacks.image_realloc == &realloc_cb, "realloc callback was not set or retrieved properly");
    VERIFY(callbacks.image_free == &free_cb, "free callback was not set or retrieved properly");
    VERIFY(callbacks.udata_copy == &udata_copy_cb, "udata copy callback was not set or retrieved properly");
    VERIFY(callbacks.udata_free == &udata_free_cb, "udata free callback was not set or retrieved properly");
    VERIFY(callbacks.udata == udata, "udata was not set or retrieved properly");

    
    /*
     * Check callbacks in internal function without a previously set file image
     */

    /* Copy fapl */
    reset_udata(udata);
    if((fapl_2 = H5Pcopy(fapl_1)) < 0) FAIL_STACK_ERROR
    
    /* Verify that the property's copy callback used the correct image callbacks */
    VERIFY(udata->used_callbacks == (UDATA_COPY), "Copying a fapl with no image used incorrect callbacks");

    /* Close fapl */
    reset_udata(udata);
    if(H5Pclose(fapl_2) < 0) FAIL_STACK_ERROR

    /* Verify that the udata free callback was used */
    VERIFY(udata->used_callbacks == (UDATA_FREE), "Closing a fapl with no image used incorrect callbacks");

    /* Copy again */
    if((fapl_2 = H5Pcopy(fapl_1)) < 0) FAIL_STACK_ERROR
    
    /* Remove property from fapl */
    reset_udata(udata);
    if(H5Premove(fapl_2, H5F_ACS_FILE_IMAGE_INFO_NAME) < 0) FAIL_STACK_ERROR

    /* Verify that the property's delete callback was called using the correct image callbacks */
    VERIFY(udata->used_callbacks == (UDATA_FREE), "Removing a property from a fapl with no image used incorrect callbacks");
    
    /* Close it again */
    if(H5Pclose(fapl_2) < 0) FAIL_STACK_ERROR

    /* Get file image */
    reset_udata(udata);
    if(H5Pget_file_image(fapl_1, (void **)&temp_file_image, &temp_size) < 0) FAIL_STACK_ERROR

    /* Verify that the correct callbacks were used */
    VERIFY(udata->used_callbacks == 0, "attempting to retrieve the image from a fapl without an image has an unexpected callback");

    /* Set file image */
    reset_udata(udata);
    if(H5Pset_file_image(fapl_1, (void *)file_image, size) < 0) FAIL_STACK_ERROR

    VERIFY(udata->used_callbacks == (MALLOC | MEMCPY), "Setting a file image (first time) used incorrect callbacks");
    
    /*
     * Check callbacks in internal functions with a previously set file image
     */
    
    /* Copy fapl */
    reset_udata(udata);
    if((fapl_2 = H5Pcopy(fapl_1)) < 0) FAIL_STACK_ERROR
    
    /* Verify that the property's copy callback used the correct image callbacks */
    VERIFY(udata->used_callbacks == (MALLOC | MEMCPY | UDATA_COPY), "Copying a fapl with an image used incorrect callbacks");
    VERIFY(udata->malloc_src == H5FD_FILE_IMAGE_OP_PROPERTY_LIST_COPY, "malloc callback has wrong source");
    VERIFY(udata->memcpy_src == H5FD_FILE_IMAGE_OP_PROPERTY_LIST_COPY, "memcpy callback has wrong source");

    /* Close fapl */
    reset_udata(udata);
    if(H5Pclose(fapl_2) < 0) FAIL_STACK_ERROR

    /* Verify that the udata free callback was used */
    VERIFY(udata->used_callbacks == (FREE | UDATA_FREE), "Closing a fapl with an image used incorrect callbacks");
    VERIFY(udata->free_src == H5FD_FILE_IMAGE_OP_PROPERTY_LIST_CLOSE, "free callback has wrong source");

    /* Copy again */
    if((fapl_2 = H5Pcopy(fapl_1)) < 0) FAIL_STACK_ERROR
    
    /* Remove property from fapl */
    reset_udata(udata);
    if(H5Premove(fapl_2, H5F_ACS_FILE_IMAGE_INFO_NAME) < 0) FAIL_STACK_ERROR

    /* Verify that the property's delete callback was called using the correct image callbacks */
    VERIFY(udata->used_callbacks == (FREE | UDATA_FREE), "Removing a property from a fapl with an image used incorrect callbacks");
    VERIFY(udata->free_src == H5FD_FILE_IMAGE_OP_PROPERTY_LIST_CLOSE, "free callback has wrong source");
    
    /* Close it again */
    if(H5Pclose(fapl_2) < 0) FAIL_STACK_ERROR

    /* Get file image */ 
    reset_udata(udata);
    if(H5Pget_file_image(fapl_1, (void **)&temp_file_image, &temp_size) < 0) FAIL_STACK_ERROR

    /* Verify that the correct callbacks were used */
    VERIFY(udata->used_callbacks == (MALLOC | MEMCPY), "attempting to retrieve the image from a fapl with an image has an unexpected callback");
    VERIFY(udata->malloc_src == H5FD_FILE_IMAGE_OP_PROPERTY_LIST_GET, "malloc callback has wrong source");
    VERIFY(udata->memcpy_src == H5FD_FILE_IMAGE_OP_PROPERTY_LIST_GET, "memcpy callback has wrong source");

    /* Set file image */
    reset_udata(udata);
    if(H5Pset_file_image(fapl_1, (void *)file_image, size) < 0) FAIL_STACK_ERROR

    VERIFY(udata->used_callbacks == (FREE | MALLOC | MEMCPY), "Setting a file image (second time) used incorrect callbacks");
    VERIFY(udata->malloc_src == H5FD_FILE_IMAGE_OP_PROPERTY_LIST_SET, "malloc callback has wrong source");
    VERIFY(udata->memcpy_src == H5FD_FILE_IMAGE_OP_PROPERTY_LIST_SET, "memcpy callback has wrong source");
    VERIFY(udata->free_src == H5FD_FILE_IMAGE_OP_PROPERTY_LIST_SET, "freec callback has wrong source");

    /* Close stuff */
    if(H5Pclose(fapl_1) < 0) FAIL_STACK_ERROR
    HDfree(file_image);
    HDfree(temp_file_image);
    HDfree(udata);

    PASSED();
    return 0;

error:
    return 1;
} /* test_callbacks() */


/******************************************************************************
 * Function:    test_core
 *
 * Purpose:     Tests that callbacks are called properly in the core VFD and
 *              that the initial file image works properly.
 *
 * Programmer:  Jacob Gruber
 *              Monday, August 22, 2011
 *
 ******************************************************************************
 */
static int
test_core(void)
{
    hid_t   fapl;
    hid_t   file;
    hid_t   dset;
    hid_t   space;
    udata_t *udata;
    unsigned char *file_image;
    char    filename[1024];
    char    copied_filename[1024];
    const char *tmp = NULL;
    size_t  size;
    hsize_t dims[2];
    int     fd;
    h5_stat_t  sb;
    herr_t ret;
    H5FD_file_image_callbacks_t callbacks = {&malloc_cb, &memcpy_cb, &realloc_cb, 
    &free_cb, &udata_copy_cb, &udata_free_cb, NULL};

    TESTING("Initial file image and callbacks in Core VFD");
    
    /* Create fapl */
    fapl = h5_fileaccess();
    VERIFY(fapl >= 0, "fapl creation failed");

    /* Set up the core VFD */
    ret = H5Pset_fapl_core(fapl, (size_t)0, 0);
    VERIFY(ret >= 0, "setting core driver in fapl failed");

    tmp = h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));
    VERIFY(tmp != NULL, "h5_fixname failed");

    /* Append ".copy" to the filename from the source directory */
    VERIFY(HDstrlen(filename) < (1023 - 5), "file name too long.");
    HDstrncpy(copied_filename, filename, (size_t)1023);
    copied_filename[1023] = '\0';
    HDstrcat(copied_filename, ".copy");

    /* Make a copy of the data file from svn. */
    ret = h5_make_local_copy(filename, copied_filename);
    VERIFY(ret >= 0, "h5_make_local_copy");

    /* Allocate and initialize udata */
    udata = (udata_t *)HDmalloc(sizeof(udata_t));
    VERIFY(udata != NULL, "udata malloc failed");

    /* copy the address of the udata into the callbacks structure */
    callbacks.udata = (void *)udata;

    /* Set file image callbacks */
    ret = H5Pset_file_image_callbacks(fapl, &callbacks);
    VERIFY(ret >= 0, "set image callbacks failed");

    /* Test open (no file image) */
    reset_udata(udata);
    file = H5Fopen(copied_filename, H5F_ACC_RDONLY, fapl);
    VERIFY(file >= 0, "H5Fopen failed");
    VERIFY(udata->used_callbacks == MALLOC, "opening a core file used the wrong callbacks");
    VERIFY(udata->malloc_src == H5FD_FILE_IMAGE_OP_FILE_OPEN, "Malloc callback came from wrong sourc in core open");

    /* Close file */
    reset_udata(udata);
    ret = H5Fclose(file);
    VERIFY(ret >= 0, "H5Fclose failed");
    VERIFY(udata->used_callbacks == FREE, "Closing a core file used the wrong callbacks");
    VERIFY(udata->free_src == H5FD_FILE_IMAGE_OP_FILE_CLOSE, "Free callback came from wrong sourc in core close");

    /* Reopen file */
    file = H5Fopen(copied_filename, H5F_ACC_RDWR, fapl);
    VERIFY(file >= 0, "H5Fopen failed");

    /* Set up a new dset */
    dims[0] = DIM0;
    dims[1] = DIM1;
    space = H5Screate_simple(RANK, dims, dims);
    VERIFY(space >= 0, "H5Screate failed");
    
    /* Create new dset, invoking H5FD_core_write */
    reset_udata(udata);
    dset = H5Dcreate2(file, DSET_NAME, H5T_NATIVE_INT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VERIFY(dset >=0, "H5Dcreate failed");
    
    /* Flush the write and check the realloc callback */
    ret = H5Fflush(file, H5F_SCOPE_LOCAL);
    VERIFY(ret >= 0, "H5Fflush failed");
    VERIFY(udata->used_callbacks == (REALLOC), "core write used the wrong callbacks");
    VERIFY(udata->realloc_src == H5FD_FILE_IMAGE_OP_FILE_RESIZE, "Realloc callback came from wrong source in core write");
    
    /* Close dset and space */
    ret = H5Dclose(dset);
    VERIFY(ret >= 0, "H5Dclose failed");
    ret = H5Sclose(space);
    VERIFY(ret >= 0, "H5Sclose failed");
    
    /* Test file close */
    reset_udata(udata);
    ret = H5Fclose(file);
    VERIFY(ret >= 0, "H5Fclose failed");
    VERIFY(udata->used_callbacks == (FREE), "Closing a core file used the wrong callbacks");
    VERIFY(udata->free_src == H5FD_FILE_IMAGE_OP_FILE_CLOSE, "Free callback came from wrong sourc in core close");

    /* Create file image buffer */
    fd = HDopen(copied_filename, O_RDONLY, 0666);
    VERIFY(fd > 0, "open failed");
    ret = HDfstat(fd, &sb);
    VERIFY(ret == 0, "fstat failed");
    size = (size_t)sb.st_size;
    file_image = (unsigned char *)HDmalloc(size);
    if(HDread(fd, file_image, size) < 0)
        FAIL_PUTS_ERROR("unable to read from file descriptor");
    ret = HDclose(fd);
    VERIFY(ret == 0, "close failed");

    /* Set file image in plist */
    if(H5Pset_file_image(fapl, file_image, size) < 0) FAIL_STACK_ERROR

    /* Test open with file image */
    if((file = H5Fopen("dne.h5", H5F_ACC_RDONLY, fapl)) < 0) FAIL_STACK_ERROR
    if(H5Fclose(file) < 0) FAIL_STACK_ERROR

    /* Release resources */
    h5_clean_files(FILENAME, fapl); 
    HDfree(udata);
    HDfree(file_image);
    HDremove(copied_filename);
    
    PASSED();

    return 0;

error:
    return 1;
} /* end test_core() */


/******************************************************************************
 * Function:    test_get_file_image
 *
 * Purpose:     Test the H5Fget_file_image() call.
 *
 * Programmer:  John Mainzer
 *              Tuesday, November 15, 2011
 *
 * Modifications:
 *      Vailin Choi; July 2013
 *      Add the creation of user block to the file as indicated by the parameter "user".
 *
 ******************************************************************************
 */
static int
test_get_file_image(const char * test_banner,
                    const int file_name_num,
		    hid_t fapl,
                    hbool_t user)
{
    char file_name[1024] = "\0";
    void * insertion_ptr = NULL;
    void * image_ptr = NULL;
    void * file_image_ptr = NULL;
    hbool_t is_family_file = FALSE;
    hbool_t identical;
    int data[100];
    int i;
    int fd = -1;
    int result;
    hid_t driver = -1;
    hid_t file_id = -1;
    hid_t dset_id = -1;
    hid_t space_id = -1;
    hid_t core_fapl_id = -1;
    hid_t core_file_id = -1;
    herr_t err;
    hsize_t dims[2];
    ssize_t bytes_read;
    ssize_t image_size;
    ssize_t file_size;
    h5_stat_t stat_buf;
    hid_t fcpl = -1;
    herr_t ret;

    TESTING(test_banner);

    /* set flag if we are dealing with a family file */
    driver = H5Pget_driver(fapl);
    VERIFY(driver >= 0, "H5Pget_driver(fapl) failed");

    if(driver == H5FD_FAMILY)
        is_family_file = TRUE;
    
    /* setup the file name */
    h5_fixname(FILENAME2[file_name_num], fapl, file_name, sizeof(file_name));
    VERIFY(HDstrlen(file_name)>0, "h5_fixname failed");

    fcpl = H5Pcreate(H5P_FILE_CREATE);
    VERIFY(fcpl >= 0, "H5Pcreate");
    if(user) {
        ret = H5Pset_userblock(fcpl, (hsize_t)USERBLOCK_SIZE);
        VERIFY(ret >=0, "H5Pset_userblock");
    }

    /* create the file */
    file_id = H5Fcreate(file_name, 0, fcpl, fapl);
    VERIFY(file_id >= 0, "H5Fcreate() failed.");

    /* Set up data space for new new data set */
    dims[0] = 10;
    dims[1] = 10;
    space_id = H5Screate_simple(2, dims, dims);
    VERIFY(space_id >= 0, "H5Screate() failed");

    /* Create a dataset */
    dset_id = H5Dcreate2(file_id, "dset 0", H5T_NATIVE_INT, space_id, 
                         H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VERIFY(dset_id >=0, "H5Dcreate() failed");

    /* write some data to the data set */
    for (i = 0; i < 100; i++)
        data[i] = i;
    err = H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, (void *)data);
    VERIFY(err >= 0, "H5Dwrite() failed.");
    
    /* Flush the file */
    err = H5Fflush(file_id, H5F_SCOPE_GLOBAL);
    VERIFY(err >= 0, "H5Fflush failed");

    /* get the size of the file */
    image_size = H5Fget_file_image(file_id, NULL, (size_t)0);
    VERIFY(image_size > 0, "H5Fget_file_image(1) failed.");

    /* allocate a buffer of the appropriate size */
    image_ptr = HDmalloc((size_t)image_size);
    VERIFY(image_ptr != NULL, "HDmalloc(1) failed.");

    /* load the image of the file into the buffer */
    bytes_read = H5Fget_file_image(file_id, image_ptr, (size_t)image_size);
    VERIFY(bytes_read == image_size, "H5Fget_file_image(2) failed.");

    /* Close dset and space */
    err = H5Dclose(dset_id); 
    VERIFY(err >= 0, "H5Dclose failed");
    err = H5Sclose(space_id);
    VERIFY(err >= 0, "H5Sclose failed");

    /* close the test file */
    err = H5Fclose(file_id);
    VERIFY(err == SUCCEED, "H5Fclose(file_id) failed.");

    if(is_family_file) {
        char member_file_name[1024];
        ssize_t bytes_to_read;
        ssize_t member_size;
        ssize_t size_remaining;

	/*
         * Modifications need to be made to accommodate userblock when
         * H5Fget_file_image() works for family driver
         */
        i = 0;
        file_size = 0;

        do {
            HDsnprintf(member_file_name, (size_t)1024, file_name, i);

            /* get the size of the member file */
            result = HDstat(member_file_name, &stat_buf);
            VERIFY(result == 0, "HDstat() failed.");

            member_size = (ssize_t)stat_buf.st_size;

            i++;
            file_size += member_size;
        } while(member_size > 0);

        /* Since we use the eoa to calculate the image size, the file size
         * may be larger.  This is OK, as long as (in this specialized instance)
         * the remainder of the file is all '\0's.
         */
        VERIFY(file_size >= image_size, "file size != image size.");

        /* allocate a buffer for the test file image */
        file_image_ptr = HDmalloc((size_t)file_size);
        VERIFY(file_image_ptr != NULL, "HDmalloc(2f) failed.");

        size_remaining = image_size;
        insertion_ptr = file_image_ptr;
        i = 0;

        while(size_remaining > 0) {
            /* construct the member file name */
            HDsnprintf(member_file_name, 1024, file_name, i);

            /* open the test file using standard I/O calls */
            fd = HDopen(member_file_name, O_RDONLY, 0666);
            VERIFY(fd >= 0, "HDopen() failed.");

            if(size_remaining >= FAMILY_SIZE ){
            bytes_to_read = FAMILY_SIZE;
                size_remaining -= FAMILY_SIZE;
            } else {
            bytes_to_read = size_remaining;
                size_remaining = 0;
            }

            /* read the member file from disk into the buffer */
            bytes_read = HDread(fd, insertion_ptr, (size_t)bytes_to_read);
            VERIFY(bytes_read == bytes_to_read, "HDread() failed.");

            insertion_ptr = (void *)(((char *)insertion_ptr) + bytes_to_read);

            i++;

            /* close the test file */
            result = HDclose(fd);
            VERIFY(result == 0, "HDclose() failed.");
        }
    } else {
        /* get the size of the test file */
        result = HDstat(file_name, &stat_buf);
        VERIFY(result == 0, "HDstat() failed.");

        /* Since we use the eoa to calculate the image size, the file size
         * may be larger.  This is OK, as long as (in this specialized instance)
         * the remainder of the file is all '\0's.
         */
        file_size = (ssize_t)stat_buf.st_size;
	if(user) {
            VERIFY(file_size > USERBLOCK_SIZE, "file size !> userblock size.");
            file_size -= USERBLOCK_SIZE;
        }

    /* with latest mods to truncate call in core file drive, 
         * file size should match image size 
         */
        VERIFY(file_size == image_size, "file size != image size.");

        /* allocate a buffer for the test file image */
        file_image_ptr = HDmalloc((size_t)file_size);
        VERIFY(file_image_ptr != NULL, "HDmalloc(2) failed.");

        /* open the test file using standard I/O calls */
        fd = HDopen(file_name, O_RDONLY, 0666);
        VERIFY(fd >= 0, "HDopen() failed.");

	if(user) {
            HDoff_t off;

            /* Position at userblock */
            off = HDlseek(fd, (off_t)USERBLOCK_SIZE, SEEK_SET);
            VERIFY(off >= 0, "HDlseek() failed.");
        }

        /* read the test file from disk into the buffer */
        bytes_read = HDread(fd, file_image_ptr, (size_t)file_size);
        VERIFY(bytes_read == file_size, "HDread() failed.");

        /* close the test file */
        result = HDclose(fd);
        VERIFY(result == 0, "HDclose() failed.");
    }

    /* verify that the file and the image contain the same data */
    identical = TRUE;
    i = 0;
    while((i < (int)image_size) && identical) {
        if(((char *)image_ptr)[i] != ((char *)file_image_ptr)[i])
            identical = FALSE;
        i++;
    }
    VERIFY(identical, "file and image differ.");


    /* finally, verify that we can use the core file driver to open the image */

    /* create fapl for core file driver */
    core_fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    VERIFY(core_fapl_id >=0, "H5Pcreate() failed");

    /* setup core_fapl_id to use the core file driver */
    err = H5Pset_fapl_core(core_fapl_id, (size_t)(64 * 1024), FALSE);
    VERIFY(err == SUCCEED, "H5Pset_fapl_core() failed.");

    /* Set file image in core fapl */
    err = H5Pset_file_image(core_fapl_id, image_ptr, (size_t)image_size);
    VERIFY(err == SUCCEED, "H5Pset_file_image() failed.");

    /* open the file image with the core file driver */
    core_file_id = H5Fopen("nonesuch", H5F_ACC_RDWR, core_fapl_id);
    VERIFY(core_file_id >= 0, "H5Fopen() of file image failed.");

    /* close the file image with the core file driver */
    err = H5Fclose(core_file_id);
    VERIFY(err == SUCCEED, "H5Fclose(core_file_id) failed.");

    /* dicard core fapl */
    err = H5Pclose(core_fapl_id);
    VERIFY(err == SUCCEED, "H5Pclose(core_fapl_id) failed.");

    /* tidy up */
    h5_clean_files(FILENAME2, fapl);

    /* discard the image buffer if it exists */
    if(image_ptr != NULL) 
        HDfree(image_ptr);

    /* discard the image buffer if it exists */
    if(file_image_ptr != NULL) 
        HDfree(file_image_ptr);
    
    PASSED();

    return 0;

error:
    return 1;
} /* end test_get_file_image() */


/******************************************************************************
 * Function:    test_get_file_image_error_rejection
 *
 * Purpose:     Verify that H5Fget_file_image() rejects invalid input.
 *
 * Programmer:  John Mainzer
 *              Tuesday, November 22, 2011
 *
 ******************************************************************************
 */

#define TYPE_SLICE ((haddr_t)0x10000LL)

static int
test_get_file_image_error_rejection(void)
{
    const char  *memb_name[H5FD_MEM_NTYPES];
    char file_name[1024] = "\0";
    void * image_ptr = NULL;
    int data[100];
    int i;
    hid_t fapl_id = -1;
    hid_t file_id = -1;
    hid_t dset_id = -1;
    hid_t space_id = -1;
    herr_t err;
    hsize_t dims[2];
    ssize_t bytes_read;
    ssize_t image_size;
    hid_t memb_fapl[H5FD_MEM_NTYPES];
    haddr_t memb_addr[H5FD_MEM_NTYPES];
    H5FD_mem_t mt;
    H5FD_mem_t memb_map[H5FD_MEM_NTYPES];


    TESTING("H5Fget_file_image() error rejection");

    /************************ Sub-Test #1 ********************************/
    /* set up a test file, and try to get its image with a buffer that is 
     * too small.  Call to H5Fget_file_image() should fail.
     *
     * Since we have already done the necessary setup, verify that 
     * H5Fget_file_image() will fail with:
     *
     *        bad file id, or
     *
     *        good id, but not a file id
     */


    /* setup fapl -- driver type doesn't matter much, so make it stdio */
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    VERIFY(fapl_id >= 0, "H5Pcreate(1) failed");

    err = H5Pset_fapl_stdio(fapl_id);
    VERIFY(err >= 0, "H5Pset_fapl_stdio() failed");

    /* setup the file name */
    h5_fixname(FILENAME2[6], fapl_id, file_name, sizeof(file_name));
    VERIFY(HDstrlen(file_name)>0, "h5_fixname failed");

    /* create the file */
    file_id = H5Fcreate(file_name, 0, H5P_DEFAULT, fapl_id);
    VERIFY(file_id >= 0, "H5Fcreate() failed.");

    /* Set up data space for new new data set */
    dims[0] = 10;
    dims[1] = 10;
    space_id = H5Screate_simple(2, dims, dims);
    VERIFY(space_id >= 0, "H5Screate() failed");

    /* Create a dataset */
    dset_id = H5Dcreate2(file_id, "dset 0", H5T_NATIVE_INT, space_id, 
                         H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VERIFY(dset_id >=0, "H5Dcreate() failed");

    /* write some data to the data set */
    for (i = 0; i < 100; i++)
        data[i] = i;
    err = H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, 
                   H5P_DEFAULT, (void *)data);
    VERIFY(err >= 0, "H5Dwrite() failed.");
    
    /* Flush the file */
    err = H5Fflush(file_id, H5F_SCOPE_GLOBAL);
    VERIFY(err >= 0, "H5Fflush failed");

    /* get the size of the file */
    image_size = H5Fget_file_image(file_id, NULL, (size_t)0);
    VERIFY(image_size > 0, "H5Fget_file_image(1 -- test 1) failed.");

    /* allocate a buffer of the appropriate size */
    image_ptr = HDmalloc((size_t)image_size);
    VERIFY(image_ptr != NULL, "HDmalloc(1) failed.");

    /* load the image of the file into the buffer */
    H5E_BEGIN_TRY {
        bytes_read = H5Fget_file_image(file_id, image_ptr, (size_t)(image_size - 1));
    } H5E_END_TRY;
    VERIFY(bytes_read < 0, "H5Fget_file_image(2 -- test 1) succeeded.");

    /* Call H5Fget_file_image() with good buffer and buffer size,
     * but non-existant file_id.  Should fail.
     */
    H5E_BEGIN_TRY {
        bytes_read = H5Fget_file_image((hid_t)0, image_ptr, (size_t)(image_size));
    } H5E_END_TRY;
    VERIFY(bytes_read < 0, "H5Fget_file_image(3 -- test 1) succeeded.");

    /* Call H5Fget_file_image() with good buffer and buffer size,
     * but a file_id of the wrong type.  Should fail.
     */
    H5E_BEGIN_TRY {
        bytes_read = H5Fget_file_image(dset_id, image_ptr, (size_t)(image_size));
    } H5E_END_TRY;
    VERIFY(bytes_read < 0, "H5Fget_file_image(4 -- test 1) succeeded.");

    /* Close dset and space */
    err = H5Dclose(dset_id); 
    VERIFY(err >= 0, "H5Dclose failed");
    err = H5Sclose(space_id);
    VERIFY(err >= 0, "H5Sclose failed");

    /* close the test file */
    err = H5Fclose(file_id);
    VERIFY(err == SUCCEED, "H5Fclose(file_id) failed.");

    /* tidy up */
    h5_clean_files(FILENAME2, fapl_id);

    /* discard the image buffer if it exists */
    if(image_ptr != NULL) 
        HDfree(image_ptr);

    /************************** Test #2 **********************************/
    /* set up a multi file driver test file, and try to get its image 
     * with H5Fget_file_image().  Attempt should fail.
     */

    /* setup parameters for multi file driver */
    for(mt = (H5FD_mem_t)0; mt < H5FD_MEM_NTYPES; mt = (H5FD_mem_t)(mt + 1)) {
        memb_addr[mt] = HADDR_UNDEF;
        memb_fapl[mt] = H5P_DEFAULT;
        memb_map[mt]  = H5FD_MEM_DRAW;
        memb_name[mt] = NULL;
    }

    memb_map[H5FD_MEM_SUPER]  = H5FD_MEM_SUPER;
    memb_fapl[H5FD_MEM_SUPER] = H5P_DEFAULT;
    memb_name[H5FD_MEM_SUPER] = "%s-s.h5";
    memb_addr[H5FD_MEM_SUPER] = 0;

    memb_map[H5FD_MEM_BTREE]  = H5FD_MEM_BTREE;
    memb_fapl[H5FD_MEM_BTREE] = H5P_DEFAULT;
    memb_name[H5FD_MEM_BTREE] = "%s-b.h5";
    memb_addr[H5FD_MEM_BTREE] = memb_addr[H5FD_MEM_SUPER] + TYPE_SLICE;

    memb_map[H5FD_MEM_DRAW]   = H5FD_MEM_DRAW;
    memb_fapl[H5FD_MEM_DRAW]  = H5P_DEFAULT;
    memb_name[H5FD_MEM_DRAW]  = "%s-r.h5";
    memb_addr[H5FD_MEM_DRAW]  =  memb_addr[H5FD_MEM_BTREE] + TYPE_SLICE;

    memb_map[H5FD_MEM_GHEAP]  = H5FD_MEM_GHEAP;
    memb_fapl[H5FD_MEM_GHEAP] = H5P_DEFAULT;
    memb_name[H5FD_MEM_GHEAP] = "%s-g.h5";
    memb_addr[H5FD_MEM_GHEAP] = memb_addr[H5FD_MEM_DRAW] + TYPE_SLICE;

    memb_map[H5FD_MEM_LHEAP]  = H5FD_MEM_LHEAP;
    memb_fapl[H5FD_MEM_LHEAP] = H5P_DEFAULT;
    memb_name[H5FD_MEM_LHEAP] = "%s-l.h5";
    memb_addr[H5FD_MEM_LHEAP] = memb_addr[H5FD_MEM_GHEAP] + TYPE_SLICE;

    memb_map[H5FD_MEM_OHDR]   = H5FD_MEM_OHDR;
    memb_fapl[H5FD_MEM_OHDR]  = H5P_DEFAULT;
    memb_name[H5FD_MEM_OHDR]  = "%s-o.h5";
    memb_addr[H5FD_MEM_OHDR]  = memb_addr[H5FD_MEM_LHEAP] + TYPE_SLICE;

    /* setup fapl */
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    VERIFY(fapl_id >= 0, "H5Pcreate(2) failed");

    /* setup the fapl for the multi file driver */
    err = H5Pset_fapl_multi(fapl_id, memb_map, memb_fapl, memb_name, 
                            memb_addr, FALSE);
    VERIFY(err >= 0, "H5Pset_fapl_multi failed");

    /* setup the file name */
    h5_fixname(FILENAME2[4], fapl_id, file_name, sizeof(file_name));
    VERIFY(HDstrlen(file_name)>0, "h5_fixname failed");

    /* create the file */
    file_id = H5Fcreate(file_name, 0, H5P_DEFAULT, fapl_id);
    VERIFY(file_id >= 0, "H5Fcreate() failed.");

    /* Set up data space for new new data set */
    dims[0] = 10;
    dims[1] = 10;
    space_id = H5Screate_simple(2, dims, dims);
    VERIFY(space_id >= 0, "H5Screate() failed");

    /* Create a dataset */
    dset_id = H5Dcreate2(file_id, "dset 0", H5T_NATIVE_INT, space_id, 
                         H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VERIFY(dset_id >=0, "H5Dcreate() failed");

    /* write some data to the data set */
    for (i = 0; i < 100; i++)
        data[i] = i;
    err = H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, 
                   H5P_DEFAULT, (void *)data);
    VERIFY(err >= 0, "H5Dwrite() failed.");
    
    /* Flush the file */
    err = H5Fflush(file_id, H5F_SCOPE_GLOBAL);
    VERIFY(err >= 0, "H5Fflush failed");

    /* attempt to get the size of the file -- should fail */
    H5E_BEGIN_TRY {
        image_size = H5Fget_file_image(file_id, NULL, (size_t)0);
    } H5E_END_TRY;
    VERIFY(image_size == -1, "H5Fget_file_image(5) succeeded.");

    /* Close dset and space */
    err = H5Dclose(dset_id); 
    VERIFY(err >= 0, "H5Dclose failed");
    err = H5Sclose(space_id);
    VERIFY(err >= 0, "H5Sclose failed");

    /* close the test file */
    err = H5Fclose(file_id);
    VERIFY(err == SUCCEED, "H5Fclose(2) failed.");

    /* tidy up */
    h5_clean_files(FILENAME2, fapl_id);

    /************************** Test #3 **********************************/
    /* set up a split file driver test file, and try to get its image 
     * with H5Fget_file_image().  Attempt should fail.
     */

    /* create fapl */ 
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    VERIFY(fapl_id >= 0, "H5Pcreate(3) failed");

    /* setup the fapl for the split file driver */
    err = H5Pset_fapl_split(fapl_id, "-m.h5", H5P_DEFAULT, "-r.h5", H5P_DEFAULT);
    VERIFY(err >= 0, "H5Pset_fapl_split failed");

    /* setup the file name */
    h5_fixname(FILENAME2[5], fapl_id, file_name, sizeof(file_name));
    VERIFY(HDstrlen(file_name)>0, "h5_fixname failed");

    /* create the file */
    file_id = H5Fcreate(file_name, 0, H5P_DEFAULT, fapl_id);
    VERIFY(file_id >= 0, "H5Fcreate() failed.");

    /* Set up data space for new new data set */
    dims[0] = 10;
    dims[1] = 10;
    space_id = H5Screate_simple(2, dims, dims);
    VERIFY(space_id >= 0, "H5Screate() failed");

    /* Create a dataset */
    dset_id = H5Dcreate2(file_id, "dset 0", H5T_NATIVE_INT, space_id, 
                         H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VERIFY(dset_id >=0, "H5Dcreate() failed");

    /* write some data to the data set */
    for (i = 0; i < 100; i++)
        data[i] = i;
    err = H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, 
                   H5P_DEFAULT, (void *)data);
    VERIFY(err >= 0, "H5Dwrite() failed.");
    
    /* Flush the file */
    err = H5Fflush(file_id, H5F_SCOPE_GLOBAL);
    VERIFY(err >= 0, "H5Fflush failed");

    /* attempt to get the size of the file -- should fail */
    H5E_BEGIN_TRY {
        image_size = H5Fget_file_image(file_id, NULL, (size_t)0);
    } H5E_END_TRY;
    VERIFY(image_size == -1, "H5Fget_file_image(6) succeeded.");

    /* Close dset and space */
    err = H5Dclose(dset_id); 
    VERIFY(err >= 0, "H5Dclose failed");
    err = H5Sclose(space_id);
    VERIFY(err >= 0, "H5Sclose failed");

    /* close the test file */
    err = H5Fclose(file_id);
    VERIFY(err == SUCCEED, "H5Fclose(2) failed.");

    /* tidy up */
    h5_clean_files(FILENAME2, fapl_id);

    /************************** Test #4 **********************************/
    /* set up a family file driver test file, and try to get its image 
     * with H5Fget_file_image().  Attempt should fail.
     */

    /* create fapl */ 
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    VERIFY(fapl_id >= 0, "H5Pcreate(3) failed");

    err = H5Pset_fapl_family(fapl_id, (hsize_t)FAMILY_SIZE, H5P_DEFAULT);
    VERIFY(err >= 0, "H5Pset_fapl_family failed");

    h5_fixname(FILENAME2[3], fapl_id, file_name, sizeof(file_name));
    VERIFY(HDstrlen(file_name)>0, "h5_fixname failed");

    /* create the file */
    file_id = H5Fcreate(file_name, 0, H5P_DEFAULT, fapl_id);
    VERIFY(file_id >= 0, "H5Fcreate() failed.");

    /* Set up data space for new new data set */
    dims[0] = 10;
    dims[1] = 10;
    space_id = H5Screate_simple(2, dims, dims);
    VERIFY(space_id >= 0, "H5Screate() failed");

    /* Create a dataset */
    dset_id = H5Dcreate2(file_id, "dset 0", H5T_NATIVE_INT, space_id, 
                         H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VERIFY(dset_id >=0, "H5Dcreate() failed");

    /* write some data to the data set */
    for (i = 0; i < 100; i++)
        data[i] = i;
    err = H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, 
                   H5P_DEFAULT, (void *)data);
    VERIFY(err >= 0, "H5Dwrite() failed.");
    
    /* Flush the file */
    err = H5Fflush(file_id, H5F_SCOPE_GLOBAL);
    VERIFY(err >= 0, "H5Fflush failed");

    /* attempt to get the size of the file -- should fail */
    H5E_BEGIN_TRY {
        image_size = H5Fget_file_image(file_id, NULL, (size_t)0);
    } H5E_END_TRY;
    VERIFY(image_size == -1, "H5Fget_file_image(7) succeeded.");

    /* Close dset and space */
    err = H5Dclose(dset_id); 
    VERIFY(err >= 0, "H5Dclose failed");
    err = H5Sclose(space_id);
    VERIFY(err >= 0, "H5Sclose failed");

    /* close the test file */
    err = H5Fclose(file_id);
    VERIFY(err == SUCCEED, "H5Fclose(2) failed.");

    /* tidy up */
    h5_clean_files(FILENAME2, fapl_id);
    
    PASSED();

    return 0;

error:
    return 1;
} /* test_get_file_image_error_rejection() */

/*
 * Modifications:
 *     Add testing for file image with or without user block in the file.
 */
int
main(void)
{
    int errors = 0;
    hid_t fapl;
    unsigned user;

    h5_reset();

    HDprintf("Testing File Image Functionality.\n");

    errors += test_properties();
    errors += test_callbacks();
    errors += test_core();

    /* Perform tests with/without user block */
    for(user = FALSE; user <= TRUE; user++) {

	/* test H5Fget_file_image() with sec2 driver */
	fapl = H5Pcreate(H5P_FILE_ACCESS);
	if(H5Pset_fapl_sec2(fapl) < 0)
	    errors++;
	else
	    errors += test_get_file_image("H5Fget_file_image() with sec2 driver",
                                      0, fapl, user);

	/* test H5Fget_file_image() with stdio driver */
	fapl = H5Pcreate(H5P_FILE_ACCESS);
	if(H5Pset_fapl_stdio(fapl) < 0)
	    errors++;
	else
	    errors += test_get_file_image("H5Fget_file_image() with stdio driver",
                                      1, fapl, user);

	/* test H5Fget_file_image() with core driver */
	fapl = H5Pcreate(H5P_FILE_ACCESS);
	if(H5Pset_fapl_core(fapl, (size_t)(64 *1024), TRUE) < 0)
	    errors++;
	else
	    errors += test_get_file_image("H5Fget_file_image() with core driver",
                                      2, fapl, user);

     } /* end for */

#if 0
    /* at present, H5Fget_file_image() rejects files opened with the 
     * family file driver, due to the addition of a driver info message
     * in the super block.  This message prevents the image being opened
     * with any driver other than the family file driver, which sort of 
     * defeats the purpose of the get file image operation.
     *
     * While this issues is quite fixable, we don't have time or resources
     * for this right now.  Once we do, the following code should be 
     * suitable for testing the fix.
     */
    /* test H5Fget_file_image() with family file driver */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    if(H5Pset_fapl_family(fapl, (hsize_t)FAMILY_SIZE, H5P_DEFAULT) < 0)
        errors++;
    else
        errors += test_get_file_image("H5Fget_file_image() with family driver",
                                      3, fapl);
#endif

    errors += test_get_file_image_error_rejection();

    /* Restore the default error handler (set in h5_reset()) */
    h5_restore_err();

    if(errors) { 
        HDprintf("***** %d File Image TEST%s FAILED! *****\n", 
            errors, errors > 1 ? "S" : ""); 
        return 1; 
    }

    HDprintf("All File Image tests passed.\n");
    return 0;
}

