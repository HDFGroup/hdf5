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

/*-------------------------------------------------------------------------
 *
 * Created:     H5FDint.c
 *
 * Purpose:     Internal routine for VFD operations
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#include "H5FDmodule.h" /* This source code file is part of the H5FD module */

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions                        */
#include "H5CXprivate.h" /* API Contexts                             */
#include "H5Eprivate.h"  /* Error handling                           */
#include "H5Fprivate.h"  /* File access                              */
#include "H5FDpkg.h"     /* File Drivers                             */
#include "H5FLprivate.h" /* Free Lists                               */
#include "H5Iprivate.h"  /* IDs                                      */
#include "H5PLprivate.h" /* Plugins                                  */

/****************/
/* Local Macros */
/****************/

/* Length of sequence lists requested from dataspace selections */
#define H5FD_SEQ_LIST_LEN 128

/* Length of stack allocated arrays for building vector I/O operations.
 * Corresponds to the number of contiguous blocks in a selection I/O operation.
 * If more space is needed dynamic allocation will be used instead. */
#define H5FD_LOCAL_VECTOR_LEN 8

/* Length of stack allocated arrays for dataspace IDs/structs for selection I/O
 * operations. Corresponds to the number of file selection/memory selection
 * pairs (along with addresses, etc.) in a selection I/O operation. If more
 * space is needed dynamic allocation will be used instead */
#define H5FD_LOCAL_SEL_ARR_LEN 8

/******************/
/* Local Typedefs */
/******************/

/*************************************************************************
 *
 * H5FD_vsrt_tmp_t
 *
 * Structure used to store vector I/O request addresses and the associated
 * indexes in the addrs[] array for the purpose of determine the sorted
 * order.
 *
 * This is done by allocating an array of H5FD_vsrt_tmp_t of length
 * count, loading it with the contents of the addrs[] array and the
 * associated indices, and then sorting it.
 *
 * This sorted array of H5FD_vsrt_tmp_t is then used to populate sorted
 * versions of the types[], addrs[], sizes[] and bufs[] vectors.
 *
 * addr:        haddr_t containing the value of addrs[i],
 *
 * index:       integer containing the value of i used to obtain the
 *              value of the addr field from the addrs[] vector.
 *
 *************************************************************************/

typedef struct H5FD_vsrt_tmp_t {
    haddr_t addr;
    size_t  index;
} H5FD_vsrt_tmp_t;

/* Information needed for iterating over the registered VFD hid_t IDs.
 * The name or value of the new VFD that is being registered is stored
 * in the name (or value) field and the found_id field is initialized to
 * H5I_INVALID_HID (-1).  If we find a VFD with the same name / value,
 * we set the found_id field to the existing ID for return to the function.
 */
typedef struct H5FD_get_driver_ud_t {
    /* IN */
    H5PL_vfd_key_t key;

    /* OUT */
    hid_t found_id; /* The driver ID, if we found a match */
} H5FD_get_driver_ud_t;

/********************/
/* Package Typedefs */
/********************/

/********************/
/* Local Prototypes */
/********************/
static int    H5FD__get_driver_cb(void *obj, hid_t id, void *_op_data);
static herr_t H5FD__read_selection_translate(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, uint32_t count,
                                             H5S_t **mem_spaces, H5S_t **file_spaces, haddr_t offsets[],
                                             size_t element_sizes[], void *bufs[] /* out */);
static herr_t H5FD__write_selection_translate(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, uint32_t count,
                                              H5S_t **mem_spaces, H5S_t **file_spaces, haddr_t offsets[],
                                              size_t element_sizes[], const void *bufs[]);

/*********************/
/* Package Variables */
/*********************/

/*****************************/
/* Library Private Variables */
/*****************************/

/*******************/
/* Local Variables */
/*******************/

/* Declare extern free list to manage the H5S_sel_iter_t struct */
H5FL_EXTERN(H5S_sel_iter_t);

/*-------------------------------------------------------------------------
 * Function:    H5FD_locate_signature
 *
 * Purpose:     Finds the HDF5 superblock signature in a file.  The
 *              signature can appear at address 0, or any power of two
 *              beginning with 512.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_locate_signature(H5FD_t *file, haddr_t *sig_addr)
{
    haddr_t  addr = HADDR_UNDEF;
    haddr_t  eoa  = HADDR_UNDEF;
    haddr_t  eof  = HADDR_UNDEF;
    uint8_t  buf[H5F_SIGNATURE_LEN];
    unsigned n;
    unsigned maxpow;
    herr_t   ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Sanity checks */
    HDassert(file);
    HDassert(sig_addr);

    /* Find the least N such that 2^N is larger than the file size */
    eof  = H5FD_get_eof(file, H5FD_MEM_SUPER);
    eoa  = H5FD_get_eoa(file, H5FD_MEM_SUPER);
    addr = MAX(eof, eoa);
    if (HADDR_UNDEF == addr)
        HGOTO_ERROR(H5E_IO, H5E_CANTINIT, FAIL, "unable to obtain EOF/EOA value")
    for (maxpow = 0; addr; maxpow++)
        addr >>= 1;
    maxpow = MAX(maxpow, 9);

    /* Search for the file signature at format address zero followed by
     * powers of two larger than 9.
     */
    for (n = 8; n < maxpow; n++) {
        addr = (8 == n) ? 0 : (haddr_t)1 << n;
        if (H5FD_set_eoa(file, H5FD_MEM_SUPER, addr + H5F_SIGNATURE_LEN) < 0)
            HGOTO_ERROR(H5E_IO, H5E_CANTINIT, FAIL, "unable to set EOA value for file signature")
        if (H5FD_read(file, H5FD_MEM_SUPER, addr, (size_t)H5F_SIGNATURE_LEN, buf) < 0)
            HGOTO_ERROR(H5E_IO, H5E_CANTINIT, FAIL, "unable to read file signature")
        if (!HDmemcmp(buf, H5F_SIGNATURE, (size_t)H5F_SIGNATURE_LEN))
            break;
    }

    /* If the signature was not found then reset the EOA value and return
     * HADDR_UNDEF.
     */
    if (n >= maxpow) {
        if (H5FD_set_eoa(file, H5FD_MEM_SUPER, eoa) < 0)
            HGOTO_ERROR(H5E_IO, H5E_CANTINIT, FAIL, "unable to reset EOA value")
        *sig_addr = HADDR_UNDEF;
    }
    else
        /* Set return value */
        *sig_addr = addr;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_locate_signature() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_read
 *
 * Purpose:     Private version of H5FDread()
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_read(H5FD_t *file, H5FD_mem_t type, haddr_t addr, size_t size, void *buf /*out*/)
{
    hid_t  dxpl_id   = H5I_INVALID_HID; /* DXPL for operation */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(file);
    HDassert(file->cls);
    HDassert(buf);

    /* Get proper DXPL for I/O */
    dxpl_id = H5CX_get_dxpl();

#ifndef H5_HAVE_PARALLEL
    /* The no-op case
     *
     * Do not return early for Parallel mode since the I/O could be a
     * collective transfer.
     */
    if (0 == size)
        HGOTO_DONE(SUCCEED)
#endif /* H5_HAVE_PARALLEL */

    /* If the file is open for SWMR read access, allow access to data past
     * the end of the allocated space (the 'eoa').  This is done because the
     * eoa stored in the file's superblock might be out of sync with the
     * objects being written within the file by the application performing
     * SWMR write operations.
     */
    if (!(file->access_flags & H5F_ACC_SWMR_READ)) {
        haddr_t eoa;

        if (HADDR_UNDEF == (eoa = (file->cls->get_eoa)(file, type)))
            HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "driver get_eoa request failed")

        if ((addr + file->base_addr + size) > eoa)
            HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "addr overflow, addr = %llu, size = %llu, eoa = %llu",
                        (unsigned long long)(addr + file->base_addr), (unsigned long long)size,
                        (unsigned long long)eoa)
    }

    /* Dispatch to driver */
    if ((file->cls->read)(file, type, dxpl_id, addr + file->base_addr, size, buf) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "driver read request failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_read() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_write
 *
 * Purpose:     Private version of H5FDwrite()
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_write(H5FD_t *file, H5FD_mem_t type, haddr_t addr, size_t size, const void *buf)
{
    hid_t   dxpl_id;                 /* DXPL for operation */
    haddr_t eoa       = HADDR_UNDEF; /* EOA for file */
    herr_t  ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(file);
    HDassert(file->cls);
    HDassert(buf);

    /* Get proper DXPL for I/O */
    dxpl_id = H5CX_get_dxpl();

#ifndef H5_HAVE_PARALLEL
    /* The no-op case
     *
     * Do not return early for Parallel mode since the I/O could be a
     * collective transfer.
     */
    if (0 == size)
        HGOTO_DONE(SUCCEED)
#endif /* H5_HAVE_PARALLEL */

    if (HADDR_UNDEF == (eoa = (file->cls->get_eoa)(file, type)))
        HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "driver get_eoa request failed")
    if ((addr + file->base_addr + size) > eoa)
        HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "addr overflow, addr = %llu, size=%llu, eoa=%llu",
                    (unsigned long long)(addr + file->base_addr), (unsigned long long)size,
                    (unsigned long long)eoa)

    /* Dispatch to driver */
    if ((file->cls->write)(file, type, dxpl_id, addr + file->base_addr, size, buf) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "driver write request failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_write() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_read_vector
 *
 * Purpose:     Private version of H5FDread_vector()
 *
 *              Perform count reads from the specified file at the offsets
 *              provided in the addrs array, with the lengths and memory
 *              types provided in the sizes and types arrays.  Data read
 *              is returned in the buffers provided in the bufs array.
 *
 *              If i > 0 and sizes[i] == 0, presume sizes[n] = sizes[i-1]
 *              for all n >= i and < count.
 *
 *              Similarly, if i > 0 and types[i] == H5FD_MEM_NOLIST,
 *              presume types[n] = types[i-1] for all n >= i and < count.
 *
 *              If the underlying VFD supports vector reads, pass the
 *              call through directly.
 *
 *              If it doesn't, convert the vector read into a sequence
 *              of individual reads.
 *
 *              Note that it is not in general possible to convert a
 *              vector read into a selection read, because each element
 *              in the vector read may have a different memory type.
 *              In contrast, selection reads are of a single type.
 *
 * Return:      Success:    SUCCEED
 *                          All reads have completed successfully, and
 *                          the results havce been into the supplied
 *                          buffers.
 *
 *              Failure:    FAIL
 *                          The contents of supplied buffers are undefined.
 *
 * Programmer:  JRM -- 6/10/20
 *
 * Changes:     None
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_read_vector(H5FD_t *file, uint32_t count, H5FD_mem_t types[], haddr_t addrs[], size_t sizes[],
                 void *bufs[] /* out */)
{
    hbool_t    addrs_cooked = FALSE;
    hbool_t    extend_sizes = FALSE;
    hbool_t    extend_types = FALSE;
    uint32_t   i;
    size_t     size;
    H5FD_mem_t type;
    hid_t      dxpl_id   = H5I_INVALID_HID; /* DXPL for operation */
    herr_t     ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(file);
    HDassert(file->cls);
    HDassert((types) || (count == 0));
    HDassert((addrs) || (count == 0));
    HDassert((sizes) || (count == 0));
    HDassert((bufs) || (count == 0));

    /* verify that the first elements of the sizes and types arrays are
     * valid.
     */
    HDassert((count == 0) || (sizes[0] != 0));
    HDassert((count == 0) || (types[0] != H5FD_MEM_NOLIST));

    /* Get proper DXPL for I/O */
    dxpl_id = H5CX_get_dxpl();

#ifndef H5_HAVE_PARALLEL
    /* The no-op case
     *
     * Do not return early for Parallel mode since the I/O could be a
     * collective transfer.
     */
    if (0 == count) {
        HGOTO_DONE(SUCCEED)
    }
#endif /* H5_HAVE_PARALLEL */

    if (file->base_addr > 0) {

        /* apply the base_addr offset to the addrs array.  Must undo before
         * we return.
         */
        for (i = 0; i < count; i++) {

            addrs[i] += file->base_addr;
        }
        addrs_cooked = TRUE;
    }

    /* If the file is open for SWMR read access, allow access to data past
     * the end of the allocated space (the 'eoa').  This is done because the
     * eoa stored in the file's superblock might be out of sync with the
     * objects being written within the file by the application performing
     * SWMR write operations.
     */
    if ((!(file->access_flags & H5F_ACC_SWMR_READ)) && (count > 0)) {
        haddr_t eoa;

        extend_sizes = FALSE;
        extend_types = FALSE;

        for (i = 0; i < count; i++) {

            if (!extend_sizes) {

                if (sizes[i] == 0) {

                    extend_sizes = TRUE;
                    size         = sizes[i - 1];
                }
                else {

                    size = sizes[i];
                }
            }

            if (!extend_types) {

                if (types[i] == H5FD_MEM_NOLIST) {

                    extend_types = TRUE;
                    type         = types[i - 1];
                }
                else {

                    type = types[i];
                }
            }

            if (HADDR_UNDEF == (eoa = (file->cls->get_eoa)(file, type)))
                HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "driver get_eoa request failed")

            if ((addrs[i] + size) > eoa)

                HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL,
                            "addr overflow, addrs[%d] = %llu, sizes[%d] = %llu, eoa = %llu", (int)i,
                            (unsigned long long)(addrs[i]), (int)i, (unsigned long long)size,
                            (unsigned long long)eoa)
        }
    }

    /* if the underlying VFD supports vector read, make the call */
    if (file->cls->read_vector) {

        if ((file->cls->read_vector)(file, dxpl_id, count, types, addrs, sizes, bufs) < 0)

            HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "driver read vector request failed")
    }
    else {

        /* otherwise, implement the vector read as a sequence of regular
         * read calls.
         */
        extend_sizes = FALSE;
        extend_types = FALSE;

        for (i = 0; i < count; i++) {

            /* we have already verified that sizes[0] != 0 and
             * types[0] != H5FD_MEM_NOLIST
             */

            if (!extend_sizes) {

                if (sizes[i] == 0) {

                    extend_sizes = TRUE;
                    size         = sizes[i - 1];
                }
                else {

                    size = sizes[i];
                }
            }

            if (!extend_types) {

                if (types[i] == H5FD_MEM_NOLIST) {

                    extend_types = TRUE;
                    type         = types[i - 1];
                }
                else {

                    type = types[i];
                }
            }

            if ((file->cls->read)(file, type, dxpl_id, addrs[i], size, bufs[i]) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "driver read request failed")
        }
    }

done:
    /* undo the base addr offset to the addrs array if necessary */
    if (addrs_cooked) {

        HDassert(file->base_addr > 0);

        for (i = 0; i < count; i++) {

            addrs[i] -= file->base_addr;
        }
    }
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_read_vector() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_write_vector
 *
 * Purpose:     Private version of H5FDwrite_vector()
 *
 *              Perform count writes to the specified file at the offsets
 *              provided in the addrs array, with the lengths and memory
 *              types provided in the sizes and types arrays.  Data written
 *              is taken from the buffers provided in the bufs array.
 *
 *              If i > 0 and sizes[i] == 0, presume sizes[n] = sizes[i-1]
 *              for all n >= i and < count.
 *
 *              Similarly, if i > 0 and types[i] == H5FD_MEM_NOLIST,
 *              presume types[n] = types[i-1] for all n >= i and < count.
 *
 *              If the underlying VFD supports vector writes, pass the
 *              call through directly.
 *
 *              If it doesn't, convert the vector write into a sequence
 *              of individual writes.
 *
 *              Note that it is not in general possible to convert a
 *              vector write into a selection write, because each element
 *              in the vector write may have a different memory type.
 *              In contrast, selection writes are of a single type.
 *
 * Return:      Success:    SUCCEED
 *                          All writes have completed successfully.
 *
 *              Failure:    FAIL
 *                          One or more writes failed.
 *
 * Programmer:  JRM -- 6/10/20
 *
 * Changes:     None
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_write_vector(H5FD_t *file, uint32_t count, H5FD_mem_t types[], haddr_t addrs[], size_t sizes[],
                  const void *bufs[])
{
    hbool_t    addrs_cooked = FALSE;
    hbool_t    extend_sizes = FALSE;
    hbool_t    extend_types = FALSE;
    uint32_t   i;
    size_t     size;
    H5FD_mem_t type;
    hid_t      dxpl_id;                 /* DXPL for operation */
    haddr_t    eoa       = HADDR_UNDEF; /* EOA for file */
    herr_t     ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(file);
    HDassert(file->cls);
    HDassert((types) || (count == 0));
    HDassert((addrs) || (count == 0));
    HDassert((sizes) || (count == 0));
    HDassert((bufs) || (count == 0));

    /* verify that the first elements of the sizes and types arrays are
     * valid.
     */
    HDassert((count == 0) || (sizes[0] != 0));
    HDassert((count == 0) || (types[0] != H5FD_MEM_NOLIST));

    /* Get proper DXPL for I/O */
    dxpl_id = H5CX_get_dxpl();

#ifndef H5_HAVE_PARALLEL
    /* The no-op case
     *
     * Do not return early for Parallel mode since the I/O could be a
     * collective transfer.
     */
    if (0 == count)
        HGOTO_DONE(SUCCEED)
#endif /* H5_HAVE_PARALLEL */

    if (file->base_addr > 0) {

        /* apply the base_addr offset to the addrs array.  Must undo before
         * we return.
         */
        for (i = 0; i < count; i++) {

            addrs[i] += file->base_addr;
        }
        addrs_cooked = TRUE;
    }

    extend_sizes = FALSE;
    extend_types = FALSE;

    for (i = 0; i < count; i++) {

        if (!extend_sizes) {

            if (sizes[i] == 0) {

                extend_sizes = TRUE;
                size         = sizes[i - 1];
            }
            else {

                size = sizes[i];
            }
        }

        if (!extend_types) {

            if (types[i] == H5FD_MEM_NOLIST) {

                extend_types = TRUE;
                type         = types[i - 1];
            }
            else {

                type = types[i];
            }
        }

        if (HADDR_UNDEF == (eoa = (file->cls->get_eoa)(file, type)))

            HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "driver get_eoa request failed")

        if ((addrs[i] + size) > eoa)

            HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "addr overflow, addrs[%d] = %llu, sizes[%d] = %llu, \
                        eoa = %llu",
                        (int)i, (unsigned long long)(addrs[i]), (int)i, (unsigned long long)size,
                        (unsigned long long)eoa)
    }

    /* if the underlying VFD supports vector write, make the call */
    if (file->cls->write_vector) {

        if ((file->cls->write_vector)(file, dxpl_id, count, types, addrs, sizes, bufs) < 0)

            HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "driver write vector request failed")
    }
    else {
        /* otherwise, implement the vector write as a sequence of regular
         * write calls.
         */
        extend_sizes = FALSE;
        extend_types = FALSE;

        for (i = 0; i < count; i++) {

            /* we have already verified that sizes[0] != 0 and
             * types[0] != H5FD_MEM_NOLIST
             */

            if (!extend_sizes) {

                if (sizes[i] == 0) {

                    extend_sizes = TRUE;
                    size         = sizes[i - 1];
                }
                else {

                    size = sizes[i];
                }
            }

            if (!extend_types) {

                if (types[i] == H5FD_MEM_NOLIST) {

                    extend_types = TRUE;
                    type         = types[i - 1];
                }
                else {

                    type = types[i];
                }
            }

            if ((file->cls->write)(file, type, dxpl_id, addrs[i], size, bufs[i]) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "driver write request failed")
        }
    }

done:
    /* undo the base addr offset to the addrs array if necessary */
    if (addrs_cooked) {

        HDassert(file->base_addr > 0);

        for (i = 0; i < count; i++) {

            addrs[i] -= file->base_addr;
        }
    }
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_write_vector() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__read_selection_translate
 *
 * Purpose:     Translates a selection read call to a vector read call if
 *              vector reads are supported, or a series of scalar read
 *              calls otherwise.
 *
 * Return:      Success:    SUCCEED
 *                          All reads have completed successfully, and
 *                          the results havce been into the supplied
 *                          buffers.
 *
 *              Failure:    FAIL
 *                          The contents of supplied buffers are undefined.
 *
 * Programmer:  NAF -- 5/13/21
 *
 * Changes:     None
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__read_selection_translate(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, uint32_t count,
                               H5S_t **mem_spaces, H5S_t **file_spaces, haddr_t offsets[],
                               size_t element_sizes[], void *bufs[] /* out */)
{
    hbool_t         extend_sizes = FALSE;
    hbool_t         extend_bufs  = FALSE;
    uint32_t        i;
    size_t          element_size;
    void           *buf;
    hbool_t         use_vector = FALSE;
    haddr_t         addrs_local[H5FD_LOCAL_VECTOR_LEN];
    haddr_t        *addrs = addrs_local;
    size_t          sizes_local[H5FD_LOCAL_VECTOR_LEN];
    size_t         *sizes = sizes_local;
    void           *vec_bufs_local[H5FD_LOCAL_VECTOR_LEN];
    void          **vec_bufs = vec_bufs_local;
    hsize_t         file_off[H5FD_SEQ_LIST_LEN];
    size_t          file_len[H5FD_SEQ_LIST_LEN];
    hsize_t         mem_off[H5FD_SEQ_LIST_LEN];
    size_t          mem_len[H5FD_SEQ_LIST_LEN];
    size_t          file_seq_i;
    size_t          mem_seq_i;
    size_t          file_nseq;
    size_t          mem_nseq;
    size_t          io_len;
    size_t          nelmts;
    hssize_t        hss_nelmts;
    size_t          seq_nelem;
    H5S_sel_iter_t *file_iter      = NULL;
    H5S_sel_iter_t *mem_iter       = NULL;
    hbool_t         file_iter_init = FALSE;
    hbool_t         mem_iter_init  = FALSE;
    H5FD_mem_t      types[2]       = {type, H5FD_MEM_NOLIST};
    size_t          vec_arr_nalloc = H5FD_LOCAL_VECTOR_LEN;
    size_t          vec_arr_nused  = 0;
    herr_t          ret_value      = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(file);
    HDassert(file->cls);
    HDassert((mem_spaces) || (count == 0));
    HDassert((file_spaces) || (count == 0));
    HDassert((offsets) || (count == 0));
    HDassert((element_sizes) || (count == 0));
    HDassert((bufs) || (count == 0));

    /* Check if we're using vector I/O */
    use_vector = file->cls->read_vector != NULL;

    if (count > 0) {
        /* Verify that the first elements of the element_sizes and bufs arrays are
         * valid. */
        HDassert(element_sizes[0] != 0);
        HDassert(bufs[0] != NULL);

        /* Allocate sequence lists for memory and file spaces */
        if (NULL == (file_iter = H5FL_MALLOC(H5S_sel_iter_t)))
            HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL, "couldn't allocate file selection iterator")
        if (NULL == (mem_iter = H5FL_MALLOC(H5S_sel_iter_t)))
            HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL, "couldn't allocate memory selection iterator")
    }

    /* Loop over dataspaces */
    for (i = 0; i < count; i++) {

        /* we have already verified that element_sizes[0] != 0 and bufs[0]
         * != NULL */

        if (!extend_sizes) {

            if (element_sizes[i] == 0) {

                extend_sizes = TRUE;
                element_size = element_sizes[i - 1];
            }
            else {

                element_size = element_sizes[i];
            }
        }

        if (!extend_bufs) {

            if (bufs[i] == NULL) {

                extend_bufs = TRUE;
                buf         = bufs[i - 1];
            }
            else {

                buf = bufs[i];
            }
        }

        /* Initialize sequence lists for memory and file spaces */
        if (H5S_select_iter_init(file_iter, file_spaces[i], element_size, 0) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "can't initialize sequence list for file space")
        file_iter_init = TRUE;
        if (H5S_select_iter_init(mem_iter, mem_spaces[i], element_size, 0) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "can't initialize sequence list for memory space")
        mem_iter_init = TRUE;

        /* Get the number of elements in selection */
        if ((hss_nelmts = (hssize_t)H5S_GET_SELECT_NPOINTS(file_spaces[i])) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTCOUNT, FAIL, "can't get number of elements selected")
        H5_CHECKED_ASSIGN(nelmts, size_t, hss_nelmts, hssize_t);

#ifndef NDEBUG
        /* Verify mem space has the same number of elements */
        {
            if ((hss_nelmts = (hssize_t)H5S_GET_SELECT_NPOINTS(mem_spaces[i])) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_CANTCOUNT, FAIL, "can't get number of elements selected")
            HDassert((hssize_t)nelmts == hss_nelmts);
        }
#endif /* NDEBUG */

        /* Initialize values so sequence lists are retrieved on the first
         * iteration */
        file_seq_i = H5FD_SEQ_LIST_LEN;
        mem_seq_i  = H5FD_SEQ_LIST_LEN;
        file_nseq  = 0;
        mem_nseq   = 0;

        /* Loop until all elements are processed */
        while (file_seq_i < file_nseq || nelmts > 0) {
            /* Fill/refill file sequence list if necessary */
            if (file_seq_i == H5FD_SEQ_LIST_LEN) {
                if (H5S_SELECT_ITER_GET_SEQ_LIST(file_iter, H5FD_SEQ_LIST_LEN, SIZE_MAX, &file_nseq,
                                                 &seq_nelem, file_off, file_len) < 0)
                    HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "sequence length generation failed")
                HDassert(file_nseq > 0);

                nelmts -= seq_nelem;
                file_seq_i = 0;
            }
            HDassert(file_seq_i < file_nseq);

            /* Fill/refill memory sequence list if necessary */
            if (mem_seq_i == H5FD_SEQ_LIST_LEN) {
                if (H5S_SELECT_ITER_GET_SEQ_LIST(mem_iter, H5FD_SEQ_LIST_LEN, SIZE_MAX, &mem_nseq, &seq_nelem,
                                                 mem_off, mem_len) < 0)
                    HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "sequence length generation failed")
                HDassert(mem_nseq > 0);

                mem_seq_i = 0;
            }
            HDassert(mem_seq_i < mem_nseq);

            /* Calculate length of this IO */
            io_len = MIN(file_len[file_seq_i], mem_len[mem_seq_i]);

            /* Check if we're using vector I/O */
            if (use_vector) {
                /* Check if we need to extend the arrays */
                if (vec_arr_nused == vec_arr_nalloc) {
                    /* Check if we're using the static arrays */
                    if (addrs == addrs_local) {
                        HDassert(sizes == sizes_local);
                        HDassert(vec_bufs == vec_bufs_local);

                        /* Allocate dynamic arrays */
                        if (NULL == (addrs = H5MM_malloc(sizeof(addrs_local) * 2)))
                            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                        "memory allocation failed for address list")
                        if (NULL == (sizes = H5MM_malloc(sizeof(sizes_local) * 2)))
                            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                        "memory allocation failed for size list")
                        if (NULL == (vec_bufs = H5MM_malloc(sizeof(vec_bufs_local) * 2)))
                            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                        "memory allocation failed for buffer list")

                        /* Copy the existing data */
                        (void)H5MM_memcpy(addrs, addrs_local, sizeof(addrs_local));
                        (void)H5MM_memcpy(sizes, sizes_local, sizeof(sizes_local));
                        (void)H5MM_memcpy(vec_bufs, vec_bufs_local, sizeof(vec_bufs_local));
                    }
                    else {
                        void *tmp_ptr;

                        /* Reallocate arrays */
                        if (NULL == (tmp_ptr = H5MM_realloc(addrs, vec_arr_nalloc * sizeof(*addrs) * 2)))
                            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                        "memory reallocation failed for address list")
                        addrs = tmp_ptr;
                        if (NULL == (tmp_ptr = H5MM_realloc(sizes, vec_arr_nalloc * sizeof(*sizes) * 2)))
                            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                        "memory reallocation failed for size list")
                        sizes = tmp_ptr;
                        if (NULL ==
                            (tmp_ptr = H5MM_realloc(vec_bufs, vec_arr_nalloc * sizeof(*vec_bufs) * 2)))
                            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                        "memory reallocation failed for buffer list")
                        vec_bufs = tmp_ptr;
                    }

                    /* Record that we've doubled the array sizes */
                    vec_arr_nalloc *= 2;
                }

                /* Add this segment to vector read list */
                addrs[vec_arr_nused]    = offsets[i] + file_off[file_seq_i];
                sizes[vec_arr_nused]    = io_len;
                vec_bufs[vec_arr_nused] = (void *)((uint8_t *)buf + mem_off[mem_seq_i]);
                vec_arr_nused++;
            }
            else
                /* Issue scalar read call */
                if ((file->cls->read)(file, type, dxpl_id, offsets[i] + file_off[file_seq_i], io_len,
                                      (void *)((uint8_t *)buf + mem_off[mem_seq_i])) < 0)
                    HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "driver read request failed")

            /* Update file sequence */
            if (io_len == file_len[file_seq_i])
                file_seq_i++;
            else {
                file_off[file_seq_i] += io_len;
                file_len[file_seq_i] -= io_len;
            }

            /* Update memory sequence */
            if (io_len == mem_len[mem_seq_i])
                mem_seq_i++;
            else {
                mem_off[mem_seq_i] += io_len;
                mem_len[mem_seq_i] -= io_len;
            }
        }

        /* Make sure both memory and file sequences terminated at the same time */
        if (mem_seq_i < mem_nseq)
            HGOTO_ERROR(H5E_INTERNAL, H5E_BADVALUE, FAIL, "file selection terminated before memory selection")

        /* Terminate iterators */
        if (H5S_SELECT_ITER_RELEASE(file_iter) < 0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "can't release file selection iterator")
        file_iter_init = FALSE;
        if (H5S_SELECT_ITER_RELEASE(mem_iter) < 0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "can't release memory selection iterator")
        mem_iter_init = FALSE;
    }

    /* Issue vector read call if appropriate */
    if (use_vector) {
        H5_CHECK_OVERFLOW(vec_arr_nused, size_t, uint32_t)
        if ((file->cls->read_vector)(file, dxpl_id, (uint32_t)vec_arr_nused, types, addrs, sizes, vec_bufs) <
            0)
            HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "driver read vector request failed")
    }

done:
    /* Terminate and free iterators */
    if (file_iter) {
        if (file_iter_init && H5S_SELECT_ITER_RELEASE(file_iter) < 0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "can't release file selection iterator")
        file_iter = H5FL_FREE(H5S_sel_iter_t, file_iter);
    }
    if (mem_iter) {
        if (mem_iter_init && H5S_SELECT_ITER_RELEASE(mem_iter) < 0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "can't release memory selection iterator")
        mem_iter = H5FL_FREE(H5S_sel_iter_t, mem_iter);
    }

    /* Cleanup vector arrays */
    if (use_vector) {
        if (addrs != addrs_local)
            addrs = H5MM_xfree(addrs);
        if (sizes != sizes_local)
            sizes = H5MM_xfree(sizes);
        if (vec_bufs != vec_bufs_local)
            vec_bufs = H5MM_xfree(vec_bufs);
    }

    /* Make sure we cleaned up */
    HDassert(!addrs || addrs == addrs_local);
    HDassert(!sizes || sizes == sizes_local);
    HDassert(!vec_bufs || vec_bufs == vec_bufs_local);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__read_selection_translate() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_read_selection
 *
 * Purpose:     Private version of H5FDread_selection()
 *
 *              Perform count reads from the specified file at the
 *              locations selected in the dataspaces in the file_spaces
 *              array, with each of those dataspaces starting at the file
 *              address specified by the corresponding element of the
 *              offsets array, and with the size of each element in the
 *              dataspace specified by the corresponding element of the
 *              element_sizes array.  The memory type provided by type is
 *              the same for all selections.  Data read is returned in
 *              the locations selected in the dataspaces in the
 *              mem_spaces array, within the buffers provided in the
 *              corresponding elements of the bufs array.
 *
 *              If i > 0 and element_sizes[i] == 0, presume
 *              element_sizes[n] = element_sizes[i-1] for all n >= i and
 *              < count.
 *
 *              If the underlying VFD supports selection reads, pass the
 *              call through directly.
 *
 *              If it doesn't, convert the vector read into a sequence
 *              of individual reads.
 *
 * Return:      Success:    SUCCEED
 *                          All reads have completed successfully, and
 *                          the results havce been into the supplied
 *                          buffers.
 *
 *              Failure:    FAIL
 *                          The contents of supplied buffers are undefined.
 *
 * Programmer:  NAF -- 3/29/21
 *
 * Changes:     None
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_read_selection(H5FD_t *file, H5FD_mem_t type, uint32_t count, H5S_t **mem_spaces, H5S_t **file_spaces,
                    haddr_t offsets[], size_t element_sizes[], void *bufs[] /* out */)
{
    hbool_t  offsets_cooked = FALSE;
    hid_t    mem_space_ids_local[H5FD_LOCAL_SEL_ARR_LEN];
    hid_t   *mem_space_ids = mem_space_ids_local;
    hid_t    file_space_ids_local[H5FD_LOCAL_SEL_ARR_LEN];
    hid_t   *file_space_ids = file_space_ids_local;
    uint32_t num_spaces     = 0;
    hid_t    dxpl_id        = H5I_INVALID_HID; /* DXPL for operation */
    uint32_t i;
    herr_t   ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(file);
    HDassert(file->cls);
    HDassert((mem_spaces) || (count == 0));
    HDassert((file_spaces) || (count == 0));
    HDassert((offsets) || (count == 0));
    HDassert((element_sizes) || (count == 0));
    HDassert((bufs) || (count == 0));

    /* Verify that the first elements of the element_sizes and bufs arrays are
     * valid. */
    HDassert((count == 0) || (element_sizes[0] != 0));
    HDassert((count == 0) || (bufs[0] != NULL));

    /* Get proper DXPL for I/O */
    dxpl_id = H5CX_get_dxpl();

#ifndef H5_HAVE_PARALLEL
    /* The no-op case
     *
     * Do not return early for Parallel mode since the I/O could be a
     * collective transfer.
     */
    if (0 == count) {
        HGOTO_DONE(SUCCEED)
    }
#endif /* H5_HAVE_PARALLEL */

    if (file->base_addr > 0) {

        /* apply the base_addr offset to the offsets array.  Must undo before
         * we return.
         */
        for (i = 0; i < count; i++) {

            offsets[i] += file->base_addr;
        }
        offsets_cooked = TRUE;
    }

    /* If the file is open for SWMR read access, allow access to data past
     * the end of the allocated space (the 'eoa').  This is done because the
     * eoa stored in the file's superblock might be out of sync with the
     * objects being written within the file by the application performing
     * SWMR write operations.
     */
    /* For now at least, only check that the offset is not past the eoa, since
     * looking into the highest offset in the selection (different from the
     * bounds) is potentially expensive.
     */
    if (!(file->access_flags & H5F_ACC_SWMR_READ)) {
        haddr_t eoa;

        if (HADDR_UNDEF == (eoa = (file->cls->get_eoa)(file, type)))
            HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "driver get_eoa request failed")

        for (i = 0; i < count; i++) {

            if ((offsets[i]) > eoa)

                HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "addr overflow, offsets[%d] = %llu, eoa = %llu",
                            (int)i, (unsigned long long)(offsets[i]), (unsigned long long)eoa)
        }
    }

    /* if the underlying VFD supports selection read, make the call */
    if (file->cls->read_selection) {
        /* Allocate array of space IDs if necessary, otherwise use local
         * buffers */
        if (count > sizeof(mem_space_ids_local) / sizeof(mem_space_ids_local[0])) {
            if (NULL == (mem_space_ids = H5MM_malloc(count * sizeof(hid_t))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for dataspace list")
            if (NULL == (file_space_ids = H5MM_malloc(count * sizeof(hid_t))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for dataspace list")
        }

        /* Create IDs for all dataspaces */
        for (; num_spaces < count; num_spaces++) {
            if ((mem_space_ids[num_spaces] = H5I_register(H5I_DATASPACE, mem_spaces[num_spaces], TRUE)) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_CANTREGISTER, FAIL, "unable to register dataspace ID")

            if ((file_space_ids[num_spaces] = H5I_register(H5I_DATASPACE, file_spaces[num_spaces], TRUE)) <
                0) {
                if (NULL == H5I_remove(mem_space_ids[num_spaces]))
                    HDONE_ERROR(H5E_VFL, H5E_CANTREMOVE, FAIL, "problem removing id")
                HGOTO_ERROR(H5E_VFL, H5E_CANTREGISTER, FAIL, "unable to register dataspace ID")
            }
        }

        if ((file->cls->read_selection)(file, type, dxpl_id, count, mem_space_ids, file_space_ids, offsets,
                                        element_sizes, bufs) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "driver read selection request failed")
    }
    else
        /* Otherwise, implement the selection read as a sequence of regular
         * or vector read calls.
         */
        if (H5FD__read_selection_translate(file, type, dxpl_id, count, mem_spaces, file_spaces, offsets,
                                           element_sizes, bufs) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "translation to vector or scalar read failed")

done:
    /* undo the base addr offset to the offsets array if necessary */
    if (offsets_cooked) {

        HDassert(file->base_addr > 0);

        for (i = 0; i < count; i++) {

            offsets[i] -= file->base_addr;
        }
    }

    /* Cleanup dataspace arrays.  Use H5I_remove() so we only close the IDs and
     * not the underlying dataspaces, which were not created by this function.
     */
    for (i = 0; i < num_spaces; i++) {
        if (NULL == H5I_remove(mem_space_ids[i]))
            HDONE_ERROR(H5E_VFL, H5E_CANTREMOVE, FAIL, "problem removing id")
        if (NULL == H5I_remove(file_space_ids[i]))
            HDONE_ERROR(H5E_VFL, H5E_CANTREMOVE, FAIL, "problem removing id")
    }
    if (mem_space_ids != mem_space_ids_local)
        mem_space_ids = H5MM_xfree(mem_space_ids);
    if (file_space_ids != file_space_ids_local)
        file_space_ids = H5MM_xfree(file_space_ids);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_read_selection() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_read_selection_id
 *
 * Purpose:     Like H5FD_read_selection(), but takes hid_t arrays instead
 *              of H5S_t * arrays for the dataspaces.
 *
 * Return:      Success:    SUCCEED
 *                          All reads have completed successfully, and
 *                          the results havce been into the supplied
 *                          buffers.
 *
 *              Failure:    FAIL
 *                          The contents of supplied buffers are undefined.
 *
 * Programmer:  NAF -- 5/19/21
 *
 * Changes:     None
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_read_selection_id(H5FD_t *file, H5FD_mem_t type, uint32_t count, hid_t mem_space_ids[],
                       hid_t file_space_ids[], haddr_t offsets[], size_t element_sizes[],
                       void *bufs[] /* out */)
{
    hbool_t  offsets_cooked = FALSE;
    H5S_t   *mem_spaces_local[H5FD_LOCAL_SEL_ARR_LEN];
    H5S_t  **mem_spaces = mem_spaces_local;
    H5S_t   *file_spaces_local[H5FD_LOCAL_SEL_ARR_LEN];
    H5S_t  **file_spaces = file_spaces_local;
    hid_t    dxpl_id     = H5I_INVALID_HID; /* DXPL for operation */
    uint32_t i;
    herr_t   ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(file);
    HDassert(file->cls);
    HDassert((mem_space_ids) || (count == 0));
    HDassert((file_space_ids) || (count == 0));
    HDassert((offsets) || (count == 0));
    HDassert((element_sizes) || (count == 0));
    HDassert((bufs) || (count == 0));

    /* Verify that the first elements of the element_sizes and bufs arrays are
     * valid. */
    HDassert((count == 0) || (element_sizes[0] != 0));
    HDassert((count == 0) || (bufs[0] != NULL));

    /* Get proper DXPL for I/O */
    dxpl_id = H5CX_get_dxpl();

#ifndef H5_HAVE_PARALLEL
    /* The no-op case
     *
     * Do not return early for Parallel mode since the I/O could be a
     * collective transfer.
     */
    if (0 == count) {
        HGOTO_DONE(SUCCEED)
    }
#endif /* H5_HAVE_PARALLEL */

    if (file->base_addr > 0) {

        /* apply the base_addr offset to the offsets array.  Must undo before
         * we return.
         */
        for (i = 0; i < count; i++) {

            offsets[i] += file->base_addr;
        }
        offsets_cooked = TRUE;
    }

    /* If the file is open for SWMR read access, allow access to data past
     * the end of the allocated space (the 'eoa').  This is done because the
     * eoa stored in the file's superblock might be out of sync with the
     * objects being written within the file by the application performing
     * SWMR write operations.
     */
    /* For now at least, only check that the offset is not past the eoa, since
     * looking into the highest offset in the selection (different from the
     * bounds) is potentially expensive.
     */
    if (!(file->access_flags & H5F_ACC_SWMR_READ)) {
        haddr_t eoa;

        if (HADDR_UNDEF == (eoa = (file->cls->get_eoa)(file, type)))
            HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "driver get_eoa request failed")

        for (i = 0; i < count; i++) {

            if ((offsets[i]) > eoa)

                HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "addr overflow, offsets[%d] = %llu, eoa = %llu",
                            (int)i, (unsigned long long)(offsets[i]), (unsigned long long)eoa)
        }
    }

    /* if the underlying VFD supports selection read, make the call */
    if (file->cls->read_selection) {
        if ((file->cls->read_selection)(file, type, dxpl_id, count, mem_space_ids, file_space_ids, offsets,
                                        element_sizes, bufs) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "driver read selection request failed")
    }
    else {
        /* Otherwise, implement the selection read as a sequence of regular
         * or vector read calls.
         */

        /* Allocate arrays of space objects if necessary, otherwise use local
         * buffers */
        if (count > sizeof(mem_spaces_local) / sizeof(mem_spaces_local[0])) {
            if (NULL == (mem_spaces = H5MM_malloc(count * sizeof(H5S_t *))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for dataspace list")
            if (NULL == (file_spaces = H5MM_malloc(count * sizeof(H5S_t *))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for dataspace list")
        }

        /* Get object pointers for all dataspaces */
        for (i = 0; i < count; i++) {
            if (NULL == (mem_spaces[i] = (H5S_t *)H5I_object_verify(mem_space_ids[i], H5I_DATASPACE)))
                HGOTO_ERROR(H5E_VFL, H5E_BADTYPE, H5I_INVALID_HID, "can't retrieve memory dataspace from ID")
            if (NULL == (file_spaces[i] = (H5S_t *)H5I_object_verify(file_space_ids[i], H5I_DATASPACE)))
                HGOTO_ERROR(H5E_VFL, H5E_BADTYPE, H5I_INVALID_HID, "can't retrieve file dataspace from ID")
        }

        /* Translate to vector or scalar I/O */
        if (H5FD__read_selection_translate(file, type, dxpl_id, count, mem_spaces, file_spaces, offsets,
                                           element_sizes, bufs) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "translation to vector or scalar read failed")
    }

done:
    /* undo the base addr offset to the offsets array if necessary */
    if (offsets_cooked) {

        HDassert(file->base_addr > 0);

        for (i = 0; i < count; i++) {

            offsets[i] -= file->base_addr;
        }
    }

    /* Cleanup dataspace arrays */
    if (mem_spaces != mem_spaces_local)
        mem_spaces = H5MM_xfree(mem_spaces);
    if (file_spaces != file_spaces_local)
        file_spaces = H5MM_xfree(file_spaces);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_read_selection_id() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__write_selection_translate
 *
 * Purpose:     Translates a selection write call to a vector write call
 *              if vector writes are supported, or a series of scalar
 *              write calls otherwise.
 *
 * Return:      Success:    SUCCEED
 *                          All writes have completed successfully.
 *
 *              Failure:    FAIL
 *                          One or more writes failed.
 *
 * Programmer:  NAF -- 5/13/21
 *
 * Changes:     None
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__write_selection_translate(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, uint32_t count,
                                H5S_t **mem_spaces, H5S_t **file_spaces, haddr_t offsets[],
                                size_t element_sizes[], const void *bufs[])
{
    hbool_t         extend_sizes = FALSE;
    hbool_t         extend_bufs  = FALSE;
    uint32_t        i;
    size_t          element_size;
    const void     *buf;
    hbool_t         use_vector = FALSE;
    haddr_t         addrs_local[H5FD_LOCAL_VECTOR_LEN];
    haddr_t        *addrs = addrs_local;
    size_t          sizes_local[H5FD_LOCAL_VECTOR_LEN];
    size_t         *sizes = sizes_local;
    const void     *vec_bufs_local[H5FD_LOCAL_VECTOR_LEN];
    const void    **vec_bufs = vec_bufs_local;
    hsize_t         file_off[H5FD_SEQ_LIST_LEN];
    size_t          file_len[H5FD_SEQ_LIST_LEN];
    hsize_t         mem_off[H5FD_SEQ_LIST_LEN];
    size_t          mem_len[H5FD_SEQ_LIST_LEN];
    size_t          file_seq_i;
    size_t          mem_seq_i;
    size_t          file_nseq;
    size_t          mem_nseq;
    size_t          io_len;
    size_t          nelmts;
    hssize_t        hss_nelmts;
    size_t          seq_nelem;
    H5S_sel_iter_t *file_iter      = NULL;
    H5S_sel_iter_t *mem_iter       = NULL;
    hbool_t         file_iter_init = FALSE;
    hbool_t         mem_iter_init  = FALSE;
    H5FD_mem_t      types[2]       = {type, H5FD_MEM_NOLIST};
    size_t          vec_arr_nalloc = H5FD_LOCAL_VECTOR_LEN;
    size_t          vec_arr_nused  = 0;
    herr_t          ret_value      = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(file);
    HDassert(file->cls);
    HDassert((mem_spaces) || (count == 0));
    HDassert((file_spaces) || (count == 0));
    HDassert((offsets) || (count == 0));
    HDassert((element_sizes) || (count == 0));
    HDassert((bufs) || (count == 0));

    /* Check if we're using vector I/O */
    use_vector = file->cls->write_vector != NULL;

    if (count > 0) {
        /* Verify that the first elements of the element_sizes and bufs arrays are
         * valid. */
        HDassert(element_sizes[0] != 0);
        HDassert(bufs[0] != NULL);

        /* Allocate sequence lists for memory and file spaces */
        if (NULL == (file_iter = H5FL_MALLOC(H5S_sel_iter_t)))
            HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL, "couldn't allocate file selection iterator")
        if (NULL == (mem_iter = H5FL_MALLOC(H5S_sel_iter_t)))
            HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL, "couldn't allocate memory selection iterator")
    }

    /* Loop over dataspaces */
    for (i = 0; i < count; i++) {

        /* we have already verified that element_sizes[0] != 0 and bufs[0]
         * != NULL */

        if (!extend_sizes) {

            if (element_sizes[i] == 0) {

                extend_sizes = TRUE;
                element_size = element_sizes[i - 1];
            }
            else {

                element_size = element_sizes[i];
            }
        }

        if (!extend_bufs) {

            if (bufs[i] == NULL) {

                extend_bufs = TRUE;
                buf         = bufs[i - 1];
            }
            else {

                buf = bufs[i];
            }
        }

        /* Initialize sequence lists for memory and file spaces */
        if (H5S_select_iter_init(file_iter, file_spaces[i], element_size, 0) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "can't initialize sequence list for file space")
        file_iter_init = TRUE;
        if (H5S_select_iter_init(mem_iter, mem_spaces[i], element_size, 0) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "can't initialize sequence list for memory space")
        mem_iter_init = TRUE;

        /* Get the number of elements in selection */
        if ((hss_nelmts = (hssize_t)H5S_GET_SELECT_NPOINTS(file_spaces[i])) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTCOUNT, FAIL, "can't get number of elements selected")
        H5_CHECKED_ASSIGN(nelmts, size_t, hss_nelmts, hssize_t);

#ifndef NDEBUG
        /* Verify mem space has the same number of elements */
        {
            if ((hss_nelmts = (hssize_t)H5S_GET_SELECT_NPOINTS(mem_spaces[i])) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_CANTCOUNT, FAIL, "can't get number of elements selected")
            HDassert((hssize_t)nelmts == hss_nelmts);
        }
#endif /* NDEBUG */

        /* Initialize values so sequence lists are retrieved on the first
         * iteration */
        file_seq_i = H5FD_SEQ_LIST_LEN;
        mem_seq_i  = H5FD_SEQ_LIST_LEN;
        file_nseq  = 0;
        mem_nseq   = 0;

        /* Loop until all elements are processed */
        while (file_seq_i < file_nseq || nelmts > 0) {
            /* Fill/refill file sequence list if necessary */
            if (file_seq_i == H5FD_SEQ_LIST_LEN) {
                if (H5S_SELECT_ITER_GET_SEQ_LIST(file_iter, H5FD_SEQ_LIST_LEN, SIZE_MAX, &file_nseq,
                                                 &seq_nelem, file_off, file_len) < 0)
                    HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "sequence length generation failed")
                HDassert(file_nseq > 0);

                nelmts -= seq_nelem;
                file_seq_i = 0;
            }
            HDassert(file_seq_i < file_nseq);

            /* Fill/refill memory sequence list if necessary */
            if (mem_seq_i == H5FD_SEQ_LIST_LEN) {
                if (H5S_SELECT_ITER_GET_SEQ_LIST(mem_iter, H5FD_SEQ_LIST_LEN, SIZE_MAX, &mem_nseq, &seq_nelem,
                                                 mem_off, mem_len) < 0)
                    HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "sequence length generation failed")
                HDassert(mem_nseq > 0);

                mem_seq_i = 0;
            }
            HDassert(mem_seq_i < mem_nseq);

            /* Calculate length of this IO */
            io_len = MIN(file_len[file_seq_i], mem_len[mem_seq_i]);

            /* Check if we're using vector I/O */
            if (use_vector) {
                /* Check if we need to extend the arrays */
                if (vec_arr_nused == vec_arr_nalloc) {
                    /* Check if we're using the static arrays */
                    if (addrs == addrs_local) {
                        HDassert(sizes == sizes_local);
                        HDassert(vec_bufs == vec_bufs_local);

                        /* Allocate dynamic arrays */
                        if (NULL == (addrs = H5MM_malloc(sizeof(addrs_local) * 2)))
                            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                        "memory allocation failed for address list")
                        if (NULL == (sizes = H5MM_malloc(sizeof(sizes_local) * 2)))
                            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                        "memory allocation failed for size list")
                        if (NULL == (vec_bufs = H5MM_malloc(sizeof(vec_bufs_local) * 2)))
                            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                        "memory allocation failed for buffer list")

                        /* Copy the existing data */
                        (void)H5MM_memcpy(addrs, addrs_local, sizeof(addrs_local));
                        (void)H5MM_memcpy(sizes, sizes_local, sizeof(sizes_local));
                        (void)H5MM_memcpy(vec_bufs, vec_bufs_local, sizeof(vec_bufs_local));
                    }
                    else {
                        void *tmp_ptr;

                        /* Reallocate arrays */
                        if (NULL == (tmp_ptr = H5MM_realloc(addrs, vec_arr_nalloc * sizeof(*addrs) * 2)))
                            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                        "memory reallocation failed for address list")
                        addrs = tmp_ptr;
                        if (NULL == (tmp_ptr = H5MM_realloc(sizes, vec_arr_nalloc * sizeof(*sizes) * 2)))
                            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                        "memory reallocation failed for size list")
                        sizes = tmp_ptr;
                        if (NULL ==
                            (tmp_ptr = H5MM_realloc(vec_bufs, vec_arr_nalloc * sizeof(*vec_bufs) * 2)))
                            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                        "memory reallocation failed for buffer list")
                        vec_bufs = tmp_ptr;
                    }

                    /* Record that we've doubled the array sizes */
                    vec_arr_nalloc *= 2;
                }

                /* Add this segment to vector write list */
                addrs[vec_arr_nused]    = offsets[i] + file_off[file_seq_i];
                sizes[vec_arr_nused]    = io_len;
                vec_bufs[vec_arr_nused] = (const void *)((const uint8_t *)buf + mem_off[mem_seq_i]);
                vec_arr_nused++;
            }
            else
                /* Issue scalar write call */
                if ((file->cls->write)(file, type, dxpl_id, offsets[i] + file_off[file_seq_i], io_len,
                                       (const void *)((const uint8_t *)buf + mem_off[mem_seq_i])) < 0)
                    HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "driver write request failed")

            /* Update file sequence */
            if (io_len == file_len[file_seq_i])
                file_seq_i++;
            else {
                file_off[file_seq_i] += io_len;
                file_len[file_seq_i] -= io_len;
            }

            /* Update memory sequence */
            if (io_len == mem_len[mem_seq_i])
                mem_seq_i++;
            else {
                mem_off[mem_seq_i] += io_len;
                mem_len[mem_seq_i] -= io_len;
            }
        }

        /* Make sure both memory and file sequences terminated at the same time */
        if (mem_seq_i < mem_nseq)
            HGOTO_ERROR(H5E_INTERNAL, H5E_BADVALUE, FAIL, "file selection terminated before memory selection")

        /* Terminate iterators */
        if (H5S_SELECT_ITER_RELEASE(file_iter) < 0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "can't release file selection iterator")
        file_iter_init = FALSE;
        if (H5S_SELECT_ITER_RELEASE(mem_iter) < 0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "can't release memory selection iterator")
        mem_iter_init = FALSE;
    }

    /* Issue vector write call if appropriate */
    if (use_vector) {
        H5_CHECK_OVERFLOW(vec_arr_nused, size_t, uint32_t)
        if ((file->cls->write_vector)(file, dxpl_id, (uint32_t)vec_arr_nused, types, addrs, sizes, vec_bufs) <
            0)
            HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "driver write vector request failed")
    }

done:
    /* Terminate and free iterators */
    if (file_iter) {
        if (file_iter_init && H5S_SELECT_ITER_RELEASE(file_iter) < 0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "can't release file selection iterator")
        file_iter = H5FL_FREE(H5S_sel_iter_t, file_iter);
    }
    if (mem_iter) {
        if (mem_iter_init && H5S_SELECT_ITER_RELEASE(mem_iter) < 0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "can't release memory selection iterator")
        mem_iter = H5FL_FREE(H5S_sel_iter_t, mem_iter);
    }

    /* Cleanup vector arrays */
    if (use_vector) {
        if (addrs != addrs_local)
            addrs = H5MM_xfree(addrs);
        if (sizes != sizes_local)
            sizes = H5MM_xfree(sizes);
        if (vec_bufs != vec_bufs_local)
            vec_bufs = H5MM_xfree(vec_bufs);
    }

    /* Make sure we cleaned up */
    HDassert(!addrs || addrs == addrs_local);
    HDassert(!sizes || sizes == sizes_local);
    HDassert(!vec_bufs || vec_bufs == vec_bufs_local);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__write_selection_translate() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_write_selection
 *
 * Purpose:     Private version of H5FDwrite_selection()
 *
 *              Perform count writes to the specified file at the
 *              locations selected in the dataspaces in the file_spaces
 *              array, with each of those dataspaces starting at the file
 *              address specified by the corresponding element of the
 *              offsets array, and with the size of each element in the
 *              dataspace specified by the corresponding element of the
 *              element_sizes array.  The memory type provided by type is
 *              the same for all selections.  Data write is from
 *              the locations selected in the dataspaces in the
 *              mem_spaces array, within the buffers provided in the
 *              corresponding elements of the bufs array.
 *
 *              If i > 0 and element_sizes[i] == 0, presume
 *              element_sizes[n] = element_sizes[i-1] for all n >= i and
 *              < count.
 *
 *              If the underlying VFD supports selection writes, pass the
 *              call through directly.
 *
 *              If it doesn't, convert the vector write into a sequence
 *              of individual writes.
 *
 * Return:      Success:    SUCCEED
 *                          All writes have completed successfully.
 *
 *              Failure:    FAIL
 *                          One or more writes failed.
 *
 * Programmer:  NAF -- 3/29/21
 *
 * Changes:     None
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_write_selection(H5FD_t *file, H5FD_mem_t type, uint32_t count, H5S_t **mem_spaces, H5S_t **file_spaces,
                     haddr_t offsets[], size_t element_sizes[], const void *bufs[])
{
    hbool_t  offsets_cooked = FALSE;
    hid_t    mem_space_ids_local[H5FD_LOCAL_SEL_ARR_LEN];
    hid_t   *mem_space_ids = mem_space_ids_local;
    hid_t    file_space_ids_local[H5FD_LOCAL_SEL_ARR_LEN];
    hid_t   *file_space_ids = file_space_ids_local;
    uint32_t num_spaces     = 0;
    hid_t    dxpl_id        = H5I_INVALID_HID; /* DXPL for operation */
    uint32_t i;
    herr_t   ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(file);
    HDassert(file->cls);
    HDassert((mem_spaces) || (count == 0));
    HDassert((file_spaces) || (count == 0));
    HDassert((offsets) || (count == 0));
    HDassert((element_sizes) || (count == 0));
    HDassert((bufs) || (count == 0));

    /* Verify that the first elements of the element_sizes and bufs arrays are
     * valid. */
    HDassert((count == 0) || (element_sizes[0] != 0));
    HDassert((count == 0) || (bufs[0] != NULL));

    /* Get proper DXPL for I/O */
    dxpl_id = H5CX_get_dxpl();

#ifndef H5_HAVE_PARALLEL
    /* The no-op case
     *
     * Do not return early for Parallel mode since the I/O could be a
     * collective transfer.
     */
    if (0 == count) {
        HGOTO_DONE(SUCCEED)
    }
#endif /* H5_HAVE_PARALLEL */

    if (file->base_addr > 0) {

        /* apply the base_addr offset to the offsets array.  Must undo before
         * we return.
         */
        for (i = 0; i < count; i++) {

            offsets[i] += file->base_addr;
        }
        offsets_cooked = TRUE;
    }

    /* For now at least, only check that the offset is not past the eoa, since
     * looking into the highest offset in the selection (different from the
     * bounds) is potentially expensive.
     */
    {
        haddr_t eoa;

        if (HADDR_UNDEF == (eoa = (file->cls->get_eoa)(file, type)))
            HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "driver get_eoa request failed")

        for (i = 0; i < count; i++) {

            if ((offsets[i]) > eoa)

                HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "addr overflow, offsets[%d] = %llu, eoa = %llu",
                            (int)i, (unsigned long long)(offsets[i]), (unsigned long long)eoa)
        }
    }

    /* if the underlying VFD supports selection write, make the call */
    if (file->cls->write_selection) {
        /* Allocate array of space IDs if necessary, otherwise use local
         * buffers */
        if (count > sizeof(mem_space_ids_local) / sizeof(mem_space_ids_local[0])) {
            if (NULL == (mem_space_ids = H5MM_malloc(count * sizeof(hid_t))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for dataspace list")
            if (NULL == (file_space_ids = H5MM_malloc(count * sizeof(hid_t))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for dataspace list")
        }

        /* Create IDs for all dataspaces */
        for (; num_spaces < count; num_spaces++) {
            if ((mem_space_ids[num_spaces] = H5I_register(H5I_DATASPACE, mem_spaces[num_spaces], TRUE)) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_CANTREGISTER, FAIL, "unable to register dataspace ID")

            if ((file_space_ids[num_spaces] = H5I_register(H5I_DATASPACE, file_spaces[num_spaces], TRUE)) <
                0) {
                if (NULL == H5I_remove(mem_space_ids[num_spaces]))
                    HDONE_ERROR(H5E_VFL, H5E_CANTREMOVE, FAIL, "problem removing id")
                HGOTO_ERROR(H5E_VFL, H5E_CANTREGISTER, FAIL, "unable to register dataspace ID")
            }
        }

        if ((file->cls->write_selection)(file, type, dxpl_id, count, mem_space_ids, file_space_ids, offsets,
                                         element_sizes, bufs) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "driver write selection request failed")
    }
    else
        /* Otherwise, implement the selection write as a sequence of regular
         * or vector write calls.
         */
        if (H5FD__write_selection_translate(file, type, dxpl_id, count, mem_spaces, file_spaces, offsets,
                                            element_sizes, bufs) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "translation to vector or scalar write failed")

done:
    /* undo the base addr offset to the offsets array if necessary */
    if (offsets_cooked) {

        HDassert(file->base_addr > 0);

        for (i = 0; i < count; i++) {

            offsets[i] -= file->base_addr;
        }
    }

    /* Cleanup dataspace arrays.  Use H5I_remove() so we only close the IDs and
     * not the underlying dataspaces, which were not created by this function.
     */
    for (i = 0; i < num_spaces; i++) {
        if (NULL == H5I_remove(mem_space_ids[i]))
            HDONE_ERROR(H5E_VFL, H5E_CANTREMOVE, FAIL, "problem removing id")
        if (NULL == H5I_remove(file_space_ids[i]))
            HDONE_ERROR(H5E_VFL, H5E_CANTREMOVE, FAIL, "problem removing id")
    }
    if (mem_space_ids != mem_space_ids_local)
        mem_space_ids = H5MM_xfree(mem_space_ids);
    if (file_space_ids != file_space_ids_local)
        file_space_ids = H5MM_xfree(file_space_ids);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_write_selection() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_write_selection_id
 *
 * Purpose:     Like H5FD_write_selection(), but takes hid_t arrays
 *              instead of H5S_t * arrays for the dataspaces.
 *
 * Return:      Success:    SUCCEED
 *                          All writes have completed successfully.
 *
 *              Failure:    FAIL
 *                          One or more writes failed.
 *
 * Programmer:  NAF -- 5/19/21
 *
 * Changes:     None
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_write_selection_id(H5FD_t *file, H5FD_mem_t type, uint32_t count, hid_t mem_space_ids[],
                        hid_t file_space_ids[], haddr_t offsets[], size_t element_sizes[], const void *bufs[])
{
    hbool_t  offsets_cooked = FALSE;
    H5S_t   *mem_spaces_local[H5FD_LOCAL_SEL_ARR_LEN];
    H5S_t  **mem_spaces = mem_spaces_local;
    H5S_t   *file_spaces_local[H5FD_LOCAL_SEL_ARR_LEN];
    H5S_t  **file_spaces = file_spaces_local;
    hid_t    dxpl_id     = H5I_INVALID_HID; /* DXPL for operation */
    uint32_t i;
    herr_t   ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(file);
    HDassert(file->cls);
    HDassert((mem_space_ids) || (count == 0));
    HDassert((file_space_ids) || (count == 0));
    HDassert((offsets) || (count == 0));
    HDassert((element_sizes) || (count == 0));
    HDassert((bufs) || (count == 0));

    /* Verify that the first elements of the element_sizes and bufs arrays are
     * valid. */
    HDassert((count == 0) || (element_sizes[0] != 0));
    HDassert((count == 0) || (bufs[0] != NULL));

    /* Get proper DXPL for I/O */
    dxpl_id = H5CX_get_dxpl();

#ifndef H5_HAVE_PARALLEL
    /* The no-op case
     *
     * Do not return early for Parallel mode since the I/O could be a
     * collective transfer.
     */
    if (0 == count) {
        HGOTO_DONE(SUCCEED)
    }
#endif /* H5_HAVE_PARALLEL */

    if (file->base_addr > 0) {

        /* apply the base_addr offset to the offsets array.  Must undo before
         * we return.
         */
        for (i = 0; i < count; i++) {

            offsets[i] += file->base_addr;
        }
        offsets_cooked = TRUE;
    }

    /* For now at least, only check that the offset is not past the eoa, since
     * looking into the highest offset in the selection (different from the
     * bounds) is potentially expensive.
     */
    {
        haddr_t eoa;

        if (HADDR_UNDEF == (eoa = (file->cls->get_eoa)(file, type)))
            HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "driver get_eoa request failed")

        for (i = 0; i < count; i++) {

            if ((offsets[i]) > eoa)

                HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "addr overflow, offsets[%d] = %llu, eoa = %llu",
                            (int)i, (unsigned long long)(offsets[i]), (unsigned long long)eoa)
        }
    }

    /* if the underlying VFD supports selection write, make the call */
    if (file->cls->write_selection) {
        if ((file->cls->write_selection)(file, type, dxpl_id, count, mem_space_ids, file_space_ids, offsets,
                                         element_sizes, bufs) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "driver write selection request failed")
    }
    else {
        /* Otherwise, implement the selection write as a sequence of regular
         * or vector write calls.
         */

        /* Allocate arrays of space objects if necessary, otherwise use local
         * buffers */
        if (count > sizeof(mem_spaces_local) / sizeof(mem_spaces_local[0])) {
            if (NULL == (mem_spaces = H5MM_malloc(count * sizeof(H5S_t *))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for dataspace list")
            if (NULL == (file_spaces = H5MM_malloc(count * sizeof(H5S_t *))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for dataspace list")
        }

        /* Get object pointers for all dataspaces */
        for (i = 0; i < count; i++) {
            if (NULL == (mem_spaces[i] = (H5S_t *)H5I_object_verify(mem_space_ids[i], H5I_DATASPACE)))
                HGOTO_ERROR(H5E_VFL, H5E_BADTYPE, H5I_INVALID_HID, "can't retrieve memory dataspace from ID")
            if (NULL == (file_spaces[i] = (H5S_t *)H5I_object_verify(file_space_ids[i], H5I_DATASPACE)))
                HGOTO_ERROR(H5E_VFL, H5E_BADTYPE, H5I_INVALID_HID, "can't retrieve file dataspace from ID")
        }

        /* Translate to vector or scalar I/O */
        if (H5FD__write_selection_translate(file, type, dxpl_id, count, mem_spaces, file_spaces, offsets,
                                            element_sizes, bufs) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "translation to vector or scalar write failed")
    }

done:
    /* undo the base addr offset to the offsets array if necessary */
    if (offsets_cooked) {

        HDassert(file->base_addr > 0);

        for (i = 0; i < count; i++) {

            offsets[i] -= file->base_addr;
        }
    }

    /* Cleanup dataspace arrays */
    if (mem_spaces != mem_spaces_local)
        mem_spaces = H5MM_xfree(mem_spaces);
    if (file_spaces != file_spaces_local)
        file_spaces = H5MM_xfree(file_spaces);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_write_selection_id() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_set_eoa
 *
 * Purpose:     Private version of H5FDset_eoa()
 *
 *              This function expects the EOA is a RELATIVE address, i.e.
 *              relative to the base address.  This is NOT the same as the
 *              EOA stored in the superblock, which is an absolute
 *              address.  Object addresses are relative.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_set_eoa(H5FD_t *file, H5FD_mem_t type, haddr_t addr)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(file && file->cls);
    HDassert(H5F_addr_defined(addr) && addr <= file->maxaddr);

    /* Dispatch to driver, convert to absolute address */
    if ((file->cls->set_eoa)(file, type, addr + file->base_addr) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "driver set_eoa request failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_set_eoa() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_get_eoa
 *
 * Purpose:     Private version of H5FDget_eoa()
 *
 *              This function returns the EOA as a RELATIVE address, i.e.
 *              relative to the base address.  This is NOT the same as the
 *              EOA stored in the superblock, which is an absolute
 *              address.  Object addresses are relative.
 *
 * Return:      Success:    First byte after allocated memory
 *
 *              Failure:    HADDR_UNDEF
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5FD_get_eoa(const H5FD_t *file, H5FD_mem_t type)
{
    haddr_t ret_value = HADDR_UNDEF; /* Return value */

    FUNC_ENTER_NOAPI(HADDR_UNDEF)

    HDassert(file && file->cls);

    /* Dispatch to driver */
    if (HADDR_UNDEF == (ret_value = (file->cls->get_eoa)(file, type)))
        HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, HADDR_UNDEF, "driver get_eoa request failed")

    /* Adjust for base address in file (convert to relative address) */
    ret_value -= file->base_addr;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_get_eoa() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_get_eof
 *
 * Purpose:     Private version of H5FDget_eof()
 *
 *              This function returns the EOF as a RELATIVE address, i.e.
 *              relative to the base address.  This will be different
 *              from  the end of the physical file if there is a user
 *              block.
 *
 * Return:      Success:    The EOF address.
 *
 *              Failure:    HADDR_UNDEF
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5FD_get_eof(const H5FD_t *file, H5FD_mem_t type)
{
    haddr_t ret_value = HADDR_UNDEF; /* Return value */

    FUNC_ENTER_NOAPI(HADDR_UNDEF)

    HDassert(file && file->cls);

    /* Dispatch to driver */
    if (file->cls->get_eof) {
        if (HADDR_UNDEF == (ret_value = (file->cls->get_eof)(file, type)))
            HGOTO_ERROR(H5E_VFL, H5E_CANTGET, HADDR_UNDEF, "driver get_eof request failed")
    }
    else
        ret_value = file->maxaddr;

    /* Adjust for base address in file (convert to relative address)  */
    ret_value -= file->base_addr;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_get_eof() */

/*-------------------------------------------------------------------------
 * Function:     H5FD_driver_query
 *
 * Purpose:      Similar to H5FD_query(), but intended for cases when we don't
 *               have a file available (e.g. before one is opened). Since we
 *               can't use the file to get the driver, the driver is passed in
 *               as a parameter.
 *
 * Return:       SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_driver_query(const H5FD_class_t *driver, unsigned long *flags /*out*/)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(driver);
    HDassert(flags);

    /* Check for the driver to query and then query it */
    if (driver->query)
        ret_value = (driver->query)(NULL, flags);
    else
        *flags = 0;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_driver_query() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_sort_vector_io_req
 *
 * Purpose:     Determine whether the supplied vector I/O request is
 *              sorted.
 *
 *              if is is, set *vector_was_sorted to TRUE, set:
 *
 *                 *s_types_ptr = types
 *                 *s_addrs_ptr = addrs
 *                 *s_sizes_ptr = sizes
 *                 *s_bufs_ptr = bufs
 *
 *              and return.
 *
 *              If it is not sorted, duplicate the type, addrs, sizes,
 *              and bufs vectors, storing the base addresses of the new
 *              vectors in *s_types_ptr, *s_addrs_ptr, *s_sizes_ptr, and
 *              *s_bufs_ptr respectively.  Determine the sorted order
 *              of the vector I/O request, and load it into the new
 *              vectors in sorted order.
 *
 *              Note that in this case, it is the callers responsibility
 *              to free the sorted vectors.
 *
 *                                            JRM -- 3/15/21
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */

static int
H5FD__vsrt_tmp_cmp(const void *element_1, const void *element_2)
{
    haddr_t addr_1    = ((const H5FD_vsrt_tmp_t *)element_1)->addr;
    haddr_t addr_2    = ((const H5FD_vsrt_tmp_t *)element_2)->addr;
    int     ret_value = 0; /* Return value */

    FUNC_ENTER_PACKAGE_NOERR

    /* Sanity checks */
    HDassert(H5F_addr_defined(addr_1));
    HDassert(H5F_addr_defined(addr_2));

    /* Compare the addresses */
    if (H5F_addr_gt(addr_1, addr_2))
        ret_value = 1;
    else if (H5F_addr_lt(addr_1, addr_2))
        ret_value = -1;

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD__vsrt_tmp_cmp() */

herr_t
H5FD_sort_vector_io_req(hbool_t *vector_was_sorted, uint32_t _count, H5FD_mem_t types[], haddr_t addrs[],
                        size_t sizes[], H5_flexible_const_ptr_t bufs[], H5FD_mem_t **s_types_ptr,
                        haddr_t **s_addrs_ptr, size_t **s_sizes_ptr, H5_flexible_const_ptr_t **s_bufs_ptr)
{
    herr_t                  ret_value = SUCCEED; /* Return value */
    size_t                  count     = (size_t)_count;
    size_t                  i;
    struct H5FD_vsrt_tmp_t *srt_tmp = NULL;

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */

    HDassert(vector_was_sorted);

    HDassert((types) || (count == 0));
    HDassert((addrs) || (count == 0));
    HDassert((sizes) || (count == 0));
    HDassert((bufs) || (count == 0));

    /* verify that the first elements of the sizes and types arrays are
     * valid.
     */
    HDassert((count == 0) || (sizes[0] != 0));
    HDassert((count == 0) || (types[0] != H5FD_MEM_NOLIST));

    HDassert((count == 0) || ((s_types_ptr) && (NULL == *s_types_ptr)));
    HDassert((count == 0) || ((s_addrs_ptr) && (NULL == *s_addrs_ptr)));
    HDassert((count == 0) || ((s_sizes_ptr) && (NULL == *s_sizes_ptr)));
    HDassert((count == 0) || ((s_bufs_ptr) && (NULL == *s_bufs_ptr)));

    /* scan the addrs array to see if it is sorted */
    for (i = 1; i < count; i++) {
        HDassert(H5F_addr_defined(addrs[i - 1]));

        if (H5F_addr_gt(addrs[i - 1], addrs[i]))
            break;
        else if (H5F_addr_eq(addrs[i - 1], addrs[i]))
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "duplicate addr in vector")
    }

    /* if we traversed the entire array without breaking out, then
     * the array was already sorted */
    if (i >= count)
        *vector_was_sorted = TRUE;
    else
        *vector_was_sorted = FALSE;

    if (*vector_was_sorted) {

        *s_types_ptr = types;
        *s_addrs_ptr = addrs;
        *s_sizes_ptr = sizes;
        *s_bufs_ptr  = bufs;
    }
    else {

        /* must sort the addrs array in increasing addr order, while
         * maintaining the association between each addr, and the
         * sizes[], types[], and bufs[] values at the same index.
         *
         * Do this by allocating an array of struct H5FD_vsrt_tmp_t, where
         * each instance of H5FD_vsrt_tmp_t has two fields, addr and index.
         * Load the array with the contents of the addrs array and
         * the index of the associated entry. Sort the array, allocate
         * the s_types_ptr, s_addrs_ptr, s_sizes_ptr, and s_bufs_ptr
         * arrays and populate them using the mapping provided by
         * the sorted array of H5FD_vsrt_tmp_t.
         */
        size_t j;
        size_t fixed_size_index = count;
        size_t fixed_type_index = count;
        size_t srt_tmp_size;

        srt_tmp_size = (count * sizeof(struct H5FD_vsrt_tmp_t));

        if (NULL == (srt_tmp = (H5FD_vsrt_tmp_t *)HDmalloc(srt_tmp_size)))

            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't alloc srt_tmp")

        for (i = 0; i < count; i++) {
            srt_tmp[i].addr  = addrs[i];
            srt_tmp[i].index = i;
        }

        /* sort the srt_tmp array */
        HDqsort(srt_tmp, count, sizeof(struct H5FD_vsrt_tmp_t), H5FD__vsrt_tmp_cmp);

        /* verify no duplicate entries */
        i = 1;

        for (i = 1; i < count; i++) {
            HDassert(H5F_addr_lt(srt_tmp[i - 1].addr, srt_tmp[i].addr));

            if (H5F_addr_eq(addrs[i - 1], addrs[i]))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "duplicate addr in vector")
        }

        if ((NULL == (*s_types_ptr = (H5FD_mem_t *)HDmalloc(count * sizeof(H5FD_mem_t)))) ||
            (NULL == (*s_addrs_ptr = (haddr_t *)HDmalloc(count * sizeof(haddr_t)))) ||
            (NULL == (*s_sizes_ptr = (size_t *)HDmalloc(count * sizeof(size_t)))) ||
            (NULL ==
             (*s_bufs_ptr = (H5_flexible_const_ptr_t *)HDmalloc(count * sizeof(H5_flexible_const_ptr_t))))) {

            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't alloc sorted vector(s)")
        }

        HDassert(sizes[0] != 0);
        HDassert(types[0] != H5FD_MEM_NOLIST);

        /* Scan the sizes and types vectors to determine if the fixed size / type
         * optimization is in use, and if so, to determine the index of the last
         * valid value on each vector.  We have already verified that the first
         * elements of these arrays are valid so we can start at the second
         * element (if it exists).
         */
        for (i = 1; i < count && ((fixed_size_index == count) || (fixed_type_index == count)); i++) {
            if ((fixed_size_index == count) && (sizes[i] == 0))
                fixed_size_index = i - 1;
            if ((fixed_type_index == count) && (types[i] == H5FD_MEM_NOLIST))
                fixed_type_index = i - 1;
        }

        HDassert(fixed_size_index <= count);
        HDassert(fixed_type_index <= count);

        /* Populate the sorted vectors.  Note that the index stored in srt_tmp
         * refers to the index in the unsorted array, while the position of
         * srt_tmp within the sorted array is the index in the sorted arrays */
        for (i = 0; i < count; i++) {

            j = srt_tmp[i].index;

            (*s_types_ptr)[i] = types[MIN(j, fixed_type_index)];
            (*s_addrs_ptr)[i] = addrs[j];
            (*s_sizes_ptr)[i] = sizes[MIN(j, fixed_size_index)];
            (*s_bufs_ptr)[i]  = bufs[j];
        }
    }

done:
    if (srt_tmp) {

        HDfree(srt_tmp);
        srt_tmp = NULL;
    }

    /* On failure, free the sorted vectors if they were allocated.
     * Note that we only allocate these vectors if the original array
     * was not sorted -- thus we check both for failure, and for
     * the flag indicating that the original vector was not sorted
     * in increasing address order.
     */
    if ((ret_value != SUCCEED) && (!(*vector_was_sorted))) {

        /* free space allocated for sorted vectors */
        if (*s_types_ptr) {

            HDfree(*s_types_ptr);
            *s_types_ptr = NULL;
        }

        if (*s_addrs_ptr) {

            HDfree(*s_addrs_ptr);
            *s_addrs_ptr = NULL;
        }

        if (*s_sizes_ptr) {

            HDfree(*s_sizes_ptr);
            *s_sizes_ptr = NULL;
        }

        if (*s_bufs_ptr) {

            HDfree(*s_bufs_ptr);
            *s_bufs_ptr = NULL;
        }
    }

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_sort_vector_io_req() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_delete
 *
 * Purpose:     Private version of H5FDdelete()
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_delete(const char *filename, hid_t fapl_id)
{
    H5FD_class_t      *driver;              /* VFD for file */
    H5FD_driver_prop_t driver_prop;         /* Property for driver ID & info */
    H5P_genplist_t    *plist;               /* Property list pointer */
    herr_t             ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */

    HDassert(filename);

    /* Get file access property list */
    if (NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    /* Get the VFD to open the file with */
    if (H5P_peek(plist, H5F_ACS_FILE_DRV_NAME, &driver_prop) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get driver ID & info")

    /* Get driver info */
    if (NULL == (driver = (H5FD_class_t *)H5I_object(driver_prop.driver_id)))
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "invalid driver ID in file access property list")
    if (NULL == driver->del)
        HGOTO_ERROR(H5E_VFL, H5E_UNSUPPORTED, FAIL, "file driver has no 'del' method")

    /* Dispatch to file driver */
    if ((driver->del)(filename, fapl_id))
        HGOTO_ERROR(H5E_VFL, H5E_CANTDELETEFILE, FAIL, "delete failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_delete() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_check_plugin_load
 *
 * Purpose:     Check if a VFD plugin matches the search criteria, and can
 *              be loaded.
 *
 * Note:        Matching the driver's name / value, but the driver having
 *              an incompatible version is not an error, but means that the
 *              driver isn't a "match".  Setting the SUCCEED value to FALSE
 *              and not failing for that case allows the plugin framework
 *              to keep looking for other DLLs that match and have a
 *              compatible version.
 *
 * Return:      SUCCEED / FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_check_plugin_load(const H5FD_class_t *cls, const H5PL_key_t *key, hbool_t *success)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOERR

    /* Sanity checks */
    HDassert(cls);
    HDassert(key);
    HDassert(success);

    /* Which kind of key are we looking for? */
    if (key->vfd.kind == H5FD_GET_DRIVER_BY_NAME) {
        /* Check if plugin name matches VFD class name */
        if (cls->name && !HDstrcmp(cls->name, key->vfd.u.name))
            *success = TRUE;
    }
    else {
        /* Sanity check */
        HDassert(key->vfd.kind == H5FD_GET_DRIVER_BY_VALUE);

        /* Check if plugin value matches VFD class value */
        if (cls->value == key->vfd.u.value)
            *success = TRUE;
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_check_plugin_load() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__get_driver_cb
 *
 * Purpose:     Callback routine to search through registered VFDs
 *
 * Return:      Success:    H5_ITER_STOP if the class and op_data name
 *                          members match. H5_ITER_CONT otherwise.
 *              Failure:    Can't fail
 *
 *-------------------------------------------------------------------------
 */
static int
H5FD__get_driver_cb(void *obj, hid_t id, void *_op_data)
{
    H5FD_get_driver_ud_t *op_data   = (H5FD_get_driver_ud_t *)_op_data; /* User data for callback */
    H5FD_class_t         *cls       = (H5FD_class_t *)obj;
    int                   ret_value = H5_ITER_CONT; /* Callback return value */

    FUNC_ENTER_PACKAGE_NOERR

    if (H5FD_GET_DRIVER_BY_NAME == op_data->key.kind) {
        if (0 == HDstrcmp(cls->name, op_data->key.u.name)) {
            op_data->found_id = id;
            ret_value         = H5_ITER_STOP;
        } /* end if */
    }     /* end if */
    else {
        HDassert(H5FD_GET_DRIVER_BY_VALUE == op_data->key.kind);
        if (cls->value == op_data->key.u.value) {
            op_data->found_id = id;
            ret_value         = H5_ITER_STOP;
        } /* end if */
    }     /* end else */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__get_driver_cb() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_register_driver_by_name
 *
 * Purpose:     Registers a new VFD as a member of the virtual file driver
 *              class.
 *
 * Return:      Success:    A VFD ID which is good until the library is
 *                          closed.
 *
 *              Failure:    H5I_INVALID_HID
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5FD_register_driver_by_name(const char *name, hbool_t app_ref)
{
    htri_t driver_is_registered = FALSE;
    hid_t  driver_id            = H5I_INVALID_HID;
    hid_t  ret_value            = H5I_INVALID_HID; /* Return value */

    FUNC_ENTER_NOAPI(H5I_INVALID_HID)

    /* Check if driver is already registered */
    if ((driver_is_registered = H5FD_is_driver_registered_by_name(name, &driver_id)) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_BADITER, H5I_INVALID_HID, "can't check if driver is already registered")

    /* If driver is already registered, increment ref count on ID and return ID */
    if (driver_is_registered) {
        HDassert(driver_id >= 0);

        if (H5I_inc_ref(driver_id, app_ref) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTINC, H5I_INVALID_HID, "unable to increment ref count on VFD")
    } /* end if */
    else {
        H5PL_key_t          key;
        const H5FD_class_t *cls;

        /* Try loading the driver */
        key.vfd.kind   = H5FD_GET_DRIVER_BY_NAME;
        key.vfd.u.name = name;
        if (NULL == (cls = (const H5FD_class_t *)H5PL_load(H5PL_TYPE_VFD, &key)))
            HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, H5I_INVALID_HID, "unable to load VFD")

        /* Register the driver we loaded */
        if ((driver_id = H5FD_register(cls, sizeof(*cls), app_ref)) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTREGISTER, H5I_INVALID_HID, "unable to register VFD ID")
    } /* end else */

    ret_value = driver_id;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_register_driver_by_name() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_register_driver_by_value
 *
 * Purpose:     Registers a new VFD as a member of the virtual file driver
 *              class.
 *
 * Return:      Success:    A VFD ID which is good until the library is
 *                          closed.
 *
 *              Failure:    H5I_INVALID_HID
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5FD_register_driver_by_value(H5FD_class_value_t value, hbool_t app_ref)
{
    htri_t driver_is_registered = FALSE;
    hid_t  driver_id            = H5I_INVALID_HID;
    hid_t  ret_value            = H5I_INVALID_HID; /* Return value */

    FUNC_ENTER_NOAPI(H5I_INVALID_HID)

    /* Check if driver is already registered */
    if ((driver_is_registered = H5FD_is_driver_registered_by_value(value, &driver_id)) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_BADITER, H5I_INVALID_HID, "can't check if driver is already registered")

    /* If driver is already registered, increment ref count on ID and return ID */
    if (driver_is_registered) {
        HDassert(driver_id >= 0);

        if (H5I_inc_ref(driver_id, app_ref) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTINC, H5I_INVALID_HID, "unable to increment ref count on VFD")
    } /* end if */
    else {
        H5PL_key_t          key;
        const H5FD_class_t *cls;

        /* Try loading the driver */
        key.vfd.kind    = H5FD_GET_DRIVER_BY_VALUE;
        key.vfd.u.value = value;
        if (NULL == (cls = (const H5FD_class_t *)H5PL_load(H5PL_TYPE_VFD, &key)))
            HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, H5I_INVALID_HID, "unable to load VFD")

        /* Register the driver we loaded */
        if ((driver_id = H5FD_register(cls, sizeof(*cls), app_ref)) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTREGISTER, H5I_INVALID_HID, "unable to register VFD ID")
    } /* end else */

    ret_value = driver_id;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_register_driver_by_value() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_is_driver_registered_by_name
 *
 * Purpose:     Checks if a driver with a particular name is registered.
 *              If `registered_id` is non-NULL and a driver with the
 *              specified name has been registered, the driver's ID will be
 *              returned in `registered_id`.
 *
 * Return:      >0 if a VFD with that name has been registered
 *              0 if a VFD with that name has NOT been registered
 *              <0 on errors
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5FD_is_driver_registered_by_name(const char *driver_name, hid_t *registered_id)
{
    H5FD_get_driver_ud_t op_data;           /* Callback info for driver search */
    htri_t               ret_value = FALSE; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Set up op data for iteration */
    op_data.key.kind   = H5FD_GET_DRIVER_BY_NAME;
    op_data.key.u.name = driver_name;
    op_data.found_id   = H5I_INVALID_HID;

    /* Find driver with name */
    if (H5I_iterate(H5I_VFL, H5FD__get_driver_cb, &op_data, FALSE) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_BADITER, FAIL, "can't iterate over VFDs")

    /* Found a driver with that name */
    if (op_data.found_id != H5I_INVALID_HID) {
        if (registered_id)
            *registered_id = op_data.found_id;
        ret_value = TRUE;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_is_driver_registered_by_name() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_is_driver_registered_by_value
 *
 * Purpose:     Checks if a driver with a particular value (ID) is
 *              registered. If `registered_id` is non-NULL and a driver
 *              with the specified value has been registered, the driver's
 *              ID will be returned in `registered_id`.
 *
 * Return:      >0 if a VFD with that value has been registered
 *              0 if a VFD with that value has NOT been registered
 *              <0 on errors
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5FD_is_driver_registered_by_value(H5FD_class_value_t driver_value, hid_t *registered_id)
{
    H5FD_get_driver_ud_t op_data;           /* Callback info for driver search */
    htri_t               ret_value = FALSE; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Set up op data for iteration */
    op_data.key.kind    = H5FD_GET_DRIVER_BY_VALUE;
    op_data.key.u.value = driver_value;
    op_data.found_id    = H5I_INVALID_HID;

    /* Find driver with value */
    if (H5I_iterate(H5I_VFL, H5FD__get_driver_cb, &op_data, FALSE) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_BADITER, FAIL, "can't iterate over VFDs")

    /* Found a driver with that value */
    if (op_data.found_id != H5I_INVALID_HID) {
        if (registered_id)
            *registered_id = op_data.found_id;
        ret_value = TRUE;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_is_driver_registered_by_value() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_get_driver_id_by_name
 *
 * Purpose:     Retrieves the ID for a registered VFL driver.
 *
 * Return:      Positive if the VFL driver has been registered
 *              Negative on error (if the driver is not a valid driver or
 *                  is not registered)
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5FD_get_driver_id_by_name(const char *name, hbool_t is_api)
{
    H5FD_get_driver_ud_t op_data;
    hid_t                ret_value = H5I_INVALID_HID; /* Return value */

    FUNC_ENTER_NOAPI(H5I_INVALID_HID)

    /* Set up op data for iteration */
    op_data.key.kind   = H5FD_GET_DRIVER_BY_NAME;
    op_data.key.u.name = name;
    op_data.found_id   = H5I_INVALID_HID;

    /* Find driver with specified name */
    if (H5I_iterate(H5I_VFL, H5FD__get_driver_cb, &op_data, FALSE) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_BADITER, H5I_INVALID_HID, "can't iterate over VFL drivers")

    /* Found a driver with that name */
    if (op_data.found_id != H5I_INVALID_HID) {
        ret_value = op_data.found_id;
        if (H5I_inc_ref(ret_value, is_api) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTINC, H5I_INVALID_HID, "unable to increment ref count on VFL driver")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_get_driver_id_by_name() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_get_driver_id_by_value
 *
 * Purpose:     Retrieves the ID for a registered VFL driver.
 *
 * Return:      Positive if the VFL driver has been registered
 *              Negative on error (if the driver is not a valid driver or
 *                  is not registered)
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5FD_get_driver_id_by_value(H5FD_class_value_t value, hbool_t is_api)
{
    H5FD_get_driver_ud_t op_data;
    hid_t                ret_value = H5I_INVALID_HID; /* Return value */

    FUNC_ENTER_NOAPI(H5I_INVALID_HID)

    /* Set up op data for iteration */
    op_data.key.kind    = H5FD_GET_DRIVER_BY_VALUE;
    op_data.key.u.value = value;
    op_data.found_id    = H5I_INVALID_HID;

    /* Find driver with specified value */
    if (H5I_iterate(H5I_VFL, H5FD__get_driver_cb, &op_data, FALSE) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_BADITER, H5I_INVALID_HID, "can't iterate over VFL drivers")

    /* Found a driver with that value */
    if (op_data.found_id != H5I_INVALID_HID) {
        ret_value = op_data.found_id;
        if (H5I_inc_ref(ret_value, is_api) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTINC, H5I_INVALID_HID, "unable to increment ref count on VFL driver")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_get_driver_id_by_value() */
