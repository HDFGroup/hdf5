/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5. The full HDF5 copyright notice, including      *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*
 * Programmer:	Lucas C. Villa Real
 *              25 March 2022
 *
 * Purpose:	Tests H5Zget_pipeline_file_id() function
 */
#include "h5test.h"

const char *FILENAME[] = {"filter_file_id", NULL};

#define DSET_NAME         "test_dataset"
#define FILENAME_BUF_SIZE 1024
#define DSET_DIM1         100
#define DSET_DIM2         200

#define H5Z_FILTER_DUMMY 312

/*
 * Define a global file id that we can use to compare against the
 * output of H5Zget_pipeline_file_id().
 */
static hid_t file_id_g = H5I_INVALID_HID;


/*
 * Callbacks for test VFD
 */
typedef struct VFD_handle_t {
    H5FD_t    driver_handle; /* public field, must be first */
    char      name[256];
    off_t     file_size;
    haddr_t   eoa;
    int       fd;
} VFD_handle_t;

static H5FD_t *
H5FD__ctl_test_vfd_open(const char *name, unsigned H5_ATTR_UNUSED flags,
                        hid_t H5_ATTR_UNUSED fapl_id, haddr_t H5_ATTR_UNUSED maxaddr)
{
    int           fd;
    struct stat   statbuf;
    VFD_handle_t *ret_val = NULL;

    if ((fd = open(name, O_RDONLY)) < 0) {
        HDprintf("Failed to open %s: %s\n", name, strerror(errno));
        return NULL;
    }

    if (fstat(fd, &statbuf) < 0) {
        HDprintf("Failed to stat %s: %s\n", name, strerror(errno));
        close(fd);
        return NULL;
    }

    ret_val = HDcalloc(1, sizeof(VFD_handle_t));
    ret_val->file_size = statbuf.st_size;
    ret_val->fd        = fd;
    ret_val->eoa       = 0;
    snprintf(ret_val->name, sizeof(ret_val->name)-1, "%s", name);

    return (H5FD_t *) ret_val;
}

static herr_t
H5FD__ctl_test_vfd_close(H5FD_t *_file)
{
    const VFD_handle_t *file_driver = (const VFD_handle_t *) _file;
    close(file_driver->fd);
    HDfree(_file);
    return SUCCEED;
}

static haddr_t
H5FD__ctl_test_vfd_get_eoa(const H5FD_t H5_ATTR_UNUSED *file, H5FD_mem_t H5_ATTR_UNUSED type)
{
    const VFD_handle_t *file_driver = (const VFD_handle_t *) file;
    return (haddr_t) file_driver->eoa;
}

static herr_t
H5FD__ctl_test_vfd_set_eoa(H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type, haddr_t addr)
{
    VFD_handle_t *file_driver = (VFD_handle_t *) _file;
    file_driver->eoa = addr;
    return SUCCEED;
}

static haddr_t
H5FD__ctl_test_vfd_get_eof(const H5FD_t H5_ATTR_UNUSED *file, H5FD_mem_t H5_ATTR_UNUSED type)
{
    const VFD_handle_t *file_driver = (const VFD_handle_t *) file;
    return (haddr_t) file_driver->file_size;
}

static herr_t
H5FD__ctl_test_vfd_read(H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type,
                        hid_t H5_ATTR_UNUSED fapl_id, haddr_t addr, size_t size,
                        void *buf)
{
    const VFD_handle_t *file_driver = (const VFD_handle_t *) _file;
    pread(file_driver->fd, buf, size, (off_t) addr);
    return SUCCEED;
}

static herr_t
H5FD__ctl_test_vfd_write(H5FD_t H5_ATTR_UNUSED *_file, H5FD_mem_t H5_ATTR_UNUSED type,
                         hid_t H5_ATTR_UNUSED fapl_id, haddr_t H5_ATTR_UNUSED addr,
                         size_t H5_ATTR_UNUSED size, const void H5_ATTR_UNUSED *buf)
{
    return FAIL;
}

static herr_t
H5FD__ctl_test_vfd_ctl(H5FD_t H5_ATTR_UNUSED *_file, uint64_t op_code, uint64_t flags,
                       const void *input, void **output)
{
    const H5FD_ctl_alloc_args_t *alloc_args;
    const H5FD_ctl_free_args_t  *free_args;
    herr_t                       ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL);

    switch (op_code) {
        case H5FD_CTL__MEM_ALLOC:
            alloc_args = (const H5FD_ctl_alloc_args_t *) input;
            if (alloc_args->flags & H5FD_MEM_CLEAR)
                *output = calloc(sizeof(char), alloc_args->size);
            else
                *output = malloc(alloc_args->size);
            break;
        case H5FD_CTL__MEM_REALLOC:
            alloc_args = (const H5FD_ctl_alloc_args_t *) input;
            *output = alloc_args->args; /* quiet const warnings */
            *output = realloc(*output, alloc_args->size);
            break;
        case H5FD_CTL__MEM_FREE:
            free_args = (const H5FD_ctl_free_args_t *) input;
            free(free_args->buf);
            break;

        /* Unknown op code */
        default:
            if (flags & H5FD_CTL__FAIL_IF_UNKNOWN_FLAG)
                HGOTO_ERROR(H5E_VFL, H5E_UNSUPPORTED, FAIL, "unknown opcode")
            break;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

static const H5FD_class_t H5FD_ctl_test_vfd_g = {
    (H5FD_class_value_t)201,    /* value                 */
    "ctl_test_vfd",             /* name                  */
    HADDR_MAX,                  /* maxaddr               */
    H5F_CLOSE_SEMI,             /* fc_degree             */
    NULL,                       /* terminate             */
    NULL,                       /* sb_size               */
    NULL,                       /* sb_encode             */
    NULL,                       /* sb_decode             */
    0,                          /* fapl_size             */
    NULL,                       /* fapl_get              */
    NULL,                       /* fapl_copy             */
    NULL,                       /* fapl_free             */
    0,                          /* dxpl_size             */
    NULL,                       /* dxpl_copy             */
    NULL,                       /* dxpl_free             */
    H5FD__ctl_test_vfd_open,    /* open                  */
    H5FD__ctl_test_vfd_close,   /* close                 */
    NULL,                       /* cmp                   */
    NULL,                       /* query                 */
    NULL,                       /* get_type_map          */
    NULL,                       /* alloc                 */
    NULL,                       /* free                  */
    H5FD__ctl_test_vfd_get_eoa, /* get_eoa               */
    H5FD__ctl_test_vfd_set_eoa, /* set_eoa               */
    H5FD__ctl_test_vfd_get_eof, /* get_eof               */
    NULL,                       /* get_handle            */
    H5FD__ctl_test_vfd_read,    /* read                  */
    H5FD__ctl_test_vfd_write,   /* write                 */
    NULL,                       /* flush                 */
    NULL,                       /* truncate              */
    NULL,                       /* lock                  */
    NULL,                       /* unlock                */
    NULL,                       /* del                   */
    H5FD__ctl_test_vfd_ctl,     /* ctl                   */
    H5FD_FLMAP_DICHOTOMY        /* fl_map                */
};


/*
 * Core test function
 */
static hbool_t
check_pipeline_file_id(void)
{
    int     i;
    void   *data = NULL;
    size_t  alloc_size = 1024 * 1024 * 10;

    /* Test H5Zget_pipeline_file_id */
    hid_t pipeline_file_id = H5Zget_pipeline_file_id();
    if (pipeline_file_id < 0) {
        HDputs("failed to get file handle from pipeline");
        return FALSE;
    } else if (pipeline_file_id != file_id_g) {
        HDprintf("unexpected file handle obtained. expected %ld, got %ld\n",
            file_id_g, pipeline_file_id);
        return FALSE;
    }

    /* Memory management functions loop. Here we test memory allocation
     * with the clear flag set (e.g., calloc,memset) and unset (such as
     * a plain malloc call).
     */
    for (i=0; i<2; ++i) {
        hbool_t clear_mem = i == 0 ? FALSE : TRUE;

        /* Test H5allocate_memory2 */
        data = H5allocate_memory2(pipeline_file_id, alloc_size, clear_mem);
        if (NULL == data) {
            HDprintf("failed to allocate memory, clear flag=%d\n", clear_mem);
            return FALSE;
        }

        /* Test H5resize_memory2 */
        data = H5resize_memory2(pipeline_file_id, data, alloc_size * 2);
        if (NULL == data) {
            HDprintf("failed to resize memory\n");
            return FALSE;
        }

        /* Test H5free_memory2 */
        if (FAIL == H5free_memory2(pipeline_file_id, data)) {
            HDprintf("failed to free memory\n");
            return FALSE;
        }
    }

    return TRUE;
}

/*-------------------------------------------------------------------------
 * Function:    set_local_cb
 *
 * Purpose:     A dummy set_local method that doesn't do anything (except
 *              for testing of API calls).
 *
 * Return:      Success: 1
 *              Failure: -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
set_local_cb(hid_t H5_ATTR_UNUSED dcpl_id,
             hid_t H5_ATTR_UNUSED type_id,
             hid_t H5_ATTR_UNUSED space_id)
{
    TESTING_2("I/O filter 'set_local' callback");

    if (FALSE == check_pipeline_file_id())
        return -1;

    PASSED();
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    can_apply_cb
 *
 * Purpose:     A dummy can_apply method that doesn't do anything (except
 *              for testing of API calls).
 *
 * Return:      Success: 1
 *              Failure: -1
 *
 *-------------------------------------------------------------------------
 */
static htri_t
can_apply_cb(hid_t H5_ATTR_UNUSED dcpl_id,
             hid_t H5_ATTR_UNUSED type_id,
             hid_t H5_ATTR_UNUSED space_id)
{
    TESTING_2("I/O filter 'can_apply' callback");

    if (FALSE == check_pipeline_file_id())
        return -1;

    PASSED();
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    filter_cb
 *
 * Purpose:     A dummy compression method that doesn't do anything (except
 *              for testing of API calls).
 *
 * Return:      Data chunk size on success, 0 on failure.
 *
 *-------------------------------------------------------------------------
 */
static size_t
filter_cb(unsigned int H5_ATTR_UNUSED flags, size_t H5_ATTR_UNUSED cd_nelmts,
           const unsigned int H5_ATTR_UNUSED *cd_values, size_t nbytes, size_t H5_ATTR_UNUSED *buf_size,
           void H5_ATTR_UNUSED **buf)
{
    TESTING_2("I/O filter 'filter' callback");

    if (FALSE == check_pipeline_file_id())
        return 0;

    PASSED();
    return nbytes;
}

/* Filter definition */
const H5Z_class2_t H5Z_DUMMY[1] = {{
    H5Z_CLASS_T_VERS, /* H5Z_class_t version              */
    H5Z_FILTER_DUMMY, /* Filter ID number                 */
    1, 1,             /* Encoding and decoding enabled    */
    "dummy",          /* Filter name for debugging        */
    can_apply_cb,     /* The "can apply" callback         */
    set_local_cb,     /* The "set local" callback         */
    filter_cb,        /* The actual filter function       */
}};

/*-------------------------------------------------------------------------
 * Function:    test_filter_write
 *
 * Purpose:     Create an HDF5 file compressed with the H5Z_DUMMY filter.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_filter_write(const char *filename, hid_t fapl_id)
{
    hid_t         dcpl_id  = H5I_INVALID_HID;
    hid_t         dset_id  = H5I_INVALID_HID;
    hid_t         space_id = H5I_INVALID_HID;
    int           i;
    const hsize_t chunk_dims[2] = {DSET_DIM1, DSET_DIM2};
    hsize_t       dims[2];
    int *         buf_data = NULL;

    HDprintf("Testing retrieval of pipeline file id and memory management VFD ctls\n");

    /* Set up data array */
    if (NULL == (buf_data = (int *)HDcalloc(DSET_DIM1 * DSET_DIM2, sizeof(int))))
        TEST_ERROR;
    for (i = 0; i < DSET_DIM1 * DSET_DIM2; i++)
        buf_data[i] = i;

    /* Create file */
    if ((file_id_g = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        goto error;

    /* Register DUMMY filter */
    if (H5Zregister(H5Z_DUMMY) < 0)
        goto error;
    if (H5Zfilter_avail(H5Z_FILTER_DUMMY) != TRUE)
        goto error;

    /* Use DUMMY filter for creating datasets */
    if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if (H5Pset_chunk(dcpl_id, 2, chunk_dims) < 0)
        goto error;
    if (H5Pset_filter(dcpl_id, H5Z_FILTER_DUMMY, 0, (size_t)0, NULL) < 0)
        goto error;

    /* Create the dataspace */
    dims[0] = DSET_DIM1;
    dims[1] = DSET_DIM2;
    if ((space_id = H5Screate_simple(2, dims, NULL)) < 0)
        goto error;

    /* Create a dataset */
    if ((dset_id = H5Dcreate2(file_id_g, DSET_NAME, H5T_NATIVE_INT, space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0)
        goto error;

    /* Write the data to the dataset */
    if (H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_data) < 0)
        goto error;

    /* Clean up objects used for this test */
    if (H5Dclose(dset_id) < 0)
        goto error;
    if (H5Sclose(space_id) < 0)
        goto error;
    if (H5Pclose(dcpl_id) < 0)
        goto error;
    if (H5Fclose(file_id_g) < 0)
        goto error;

    /* Unregister the filter */
    if (H5Zunregister(H5Z_FILTER_DUMMY) < 0)
        goto error;

    HDfree(buf_data);

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(dset_id);
        H5Sclose(space_id);
        H5Pclose(dcpl_id);
        H5Fclose(file_id_g);
    }
    H5E_END_TRY;

    HDfree(buf_data);

    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    test_filter_read
 *
 * Purpose:     Read an HDF5 file compressed with the H5Z_DUMMY filter.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_filter_read(const char *filename, hid_t fapl_id)
{
    hid_t         dset_id  = H5I_INVALID_HID;
    int *         buf_data = NULL;

    HDprintf("Testing retrieval of pipeline file id and memory management VFD ctls, custom VFD\n");

    /* Register DUMMY filter */
    if (H5Zregister(H5Z_DUMMY) < 0)
        goto error;
    if (H5Zfilter_avail(H5Z_FILTER_DUMMY) != TRUE)
        goto error;

    /* Set up data array */
    if (NULL == (buf_data = (int *)HDcalloc(DSET_DIM1 * DSET_DIM2, sizeof(int))))
        TEST_ERROR;

    /* Create file */
    if ((file_id_g = H5Fopen(filename, H5F_ACC_RDONLY, fapl_id)) < 0)
        goto error;

    /* Open the dataset */
    if ((dset_id = H5Dopen2(file_id_g, DSET_NAME, H5P_DEFAULT)) < 0)
        goto error;

    /* Read the data from the dataset */
    if (H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_data) < 0)
        goto error;

    /* Clean up objects used for this test */
    if (H5Dclose(dset_id) < 0)
        goto error;
    if (H5Fclose(file_id_g) < 0)
        goto error;

    /* Unregister the filter */
    if (H5Zunregister(H5Z_FILTER_DUMMY) < 0)
        goto error;

    HDfree(buf_data);

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(file_id_g);
        H5Dclose(dset_id);
    }
    H5E_END_TRY;

    HDfree(buf_data);

    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests retrieval of pipeline's file handle
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    char    filename[FILENAME_BUF_SIZE];
    hid_t   fapl_id        = H5I_INVALID_HID;
    hid_t   driver_id      = H5I_INVALID_HID;
    int     nerrors        = 0;

    /* Testing setup */
    h5_reset();
    fapl_id = h5_fileaccess();
    h5_fixname(FILENAME[0], fapl_id, filename, sizeof(filename));

    /* Launch test with the default VFD */
    nerrors += (test_filter_write(filename, fapl_id) < 0 ? 1 : 0);

    /* Launch test with our dummy VFD */
    if ((driver_id = H5FDregister(&H5FD_ctl_test_vfd_g)) < 0)
        PUTS_ERROR("failed to register VFD for testing");
    if (H5Pset_driver(fapl_id, driver_id, NULL) < 0)
        PUTS_ERROR("failed to set testing VFD on fapl");
    nerrors += (test_filter_read(filename, fapl_id) < 0 ? 1 : 0);

    h5_cleanup(FILENAME, fapl_id);

    if (nerrors)
        goto error;
    HDprintf("All filter pipeline file handle tests passed.\n");

    HDexit(EXIT_SUCCESS);

error:
    nerrors = MAX(1, nerrors);
    HDprintf("***** %d FILTER PIPELINE FILE HANDLE TEST%s FAILED! *****\n", nerrors, 1 == nerrors ? "" : "S");

    HDexit(EXIT_FAILURE);
} /* end main() */
