/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
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
#include "H5Iprivate.h"  /* IDs                                      */

/****************/
/* Local Macros */
/****************/

/* Length of sequence lists requested from dataspace selections */
#define H5FD_SEQ_LIST_LEN 128

/******************/
/* Local Typedefs */
/******************/

/********************/
/* Package Typedefs */
/********************/

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
    if (!(file->access_flags & H5F_ACC_SWMR_READ)) {
        haddr_t eoa;

        for (i = 0; i < count; i++) {

            if (HADDR_UNDEF == (eoa = (file->cls->get_eoa)(file, types[i])))
                HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "driver get_eoa request failed")

            if ((addrs[i] + sizes[i]) > eoa)

                HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL,
                            "addr overflow, addrs[%d] = %llu, sizes[%d] = %llu, eoa = %llu", (int)i,
                            (unsigned long long)(addrs[i]), (int)i, (unsigned long long)sizes[i],
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
 *              in the vector read may have a different memory type.
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
                  const void *bufs[] /* out */)
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

    for (i = 0; i < count; i++) {

        if (HADDR_UNDEF == (eoa = (file->cls->get_eoa)(file, types[i])))

            HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "driver get_eoa request failed")

        if ((addrs[i] + sizes[i]) > eoa)

            HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "addr overflow, addrs[%d] = %llu, sizes[%d] = %llu, \
                        eoa = %llu",
                        (int)i, (unsigned long long)(addrs[i]), (int)i, (unsigned long long)sizes[i],
                        (unsigned long long)eoa)
    }

    /* if the underlying VFD supports vector write, make the call */
    if (file->cls->write_vector) {

        if ((file->cls->write_vector)(file, dxpl_id, count, types, addrs, sizes, bufs) < 0)

            HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "driver write vector request failed")
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
H5FD_read_selection(H5FD_t *file, uint32_t count, H5FD_mem_t type, H5S_t *mem_spaces[], H5S_t *file_spaces[],
                    haddr_t offsets[], size_t element_sizes[], void *bufs[] /* out */)
{
    hbool_t  offsets_cooked = FALSE;
    hbool_t  extend_sizes   = FALSE;
    uint32_t i;
    size_t   element_size;
    hid_t    dxpl_id   = H5I_INVALID_HID; /* DXPL for operation */
    herr_t   ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(file);
    HDassert(file->cls);
    HDassert((mem_spaces) || (count == 0));
    HDassert((file_spaces) || (count == 0));
    HDassert((offsets) || (count == 0));
    HDassert((element_sizes) || (count == 0));
    HDassert((bufs) || (count == 0));

    /* Verify that the first elements of the element_sizes array is valid. */
    HDassert((count == 0) || (element_sizes[0] != 0));

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
    /*if (file->cls->read_selection) {

        if ((file->cls->read_selection)(file, count, type, mem_spaces, file_spaces, offsets, element_sizes,
    bufs) < 0)

            HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "driver read selection request failed")
    }
    else*/
    {
        hsize_t        file_off[H5FD_SEQ_LIST_LEN];
        size_t         file_len[H5FD_SEQ_LIST_LEN];
        hsize_t        mem_off[H5FD_SEQ_LIST_LEN];
        size_t         mem_len[H5FD_SEQ_LIST_LEN];
        size_t         file_seq_i;
        size_t         mem_seq_i;
        size_t         file_nseq;
        size_t         mem_nseq;
        size_t         io_len;
        size_t         dummy_nelem;
        H5S_sel_iter_t file_iter;
        H5S_sel_iter_t mem_iter;

        /* otherwise, implement the selection read as a sequence of regular
         * read calls.
         */
        extend_sizes = FALSE;

        for (i = 0; i < count; i++) {

            /* we have already verified that element_sizes[0] != 0 */

            if (!extend_sizes) {

                if (element_sizes[i] == 0) {

                    extend_sizes = TRUE;
                    element_size = element_sizes[i - 1];
                }
                else {

                    element_size = element_sizes[i];
                }
            }

            /* Initialize sequence lists for memory and file spaces */
            if (H5S_select_iter_init(&file_iter, file_spaces[i], element_size, 0) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "can't initialize sequence list for file space")
            if (H5S_select_iter_init(&mem_iter, mem_spaces[i], element_size, 0) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "can't initialize sequence list for memory space")

            /* Fill sequence lists */
            if (H5S_SELECT_ITER_GET_SEQ_LIST(&file_iter, H5FD_SEQ_LIST_LEN, SIZE_MAX, &file_nseq,
                                             &dummy_nelem, file_off, file_len) < 0)
                HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "sequence length generation failed")
            if (H5S_SELECT_ITER_GET_SEQ_LIST(&mem_iter, H5FD_SEQ_LIST_LEN, SIZE_MAX, &mem_nseq, &dummy_nelem,
                                             mem_off, mem_len) < 0)
                HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "sequence length generation failed")
            if (file_nseq && !mem_nseq)
                HGOTO_ERROR(H5E_INTERNAL, H5E_BADVALUE, FAIL,
                            "memory selection is empty but file selection is not")
            file_seq_i = 0;
            mem_seq_i  = 0;

            while (file_seq_i < file_nseq) {
                /* Calculate length of this IO */
                io_len = MIN(file_len[file_seq_i], mem_len[mem_seq_i]);

                /* Issue scalar read call */
                if ((file->cls->read)(file, type, dxpl_id, file_off[file_seq_i], io_len,
                                      (void *)(((uint8_t *)bufs[i]) + mem_off[mem_seq_i])) < 0)
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

                /* Refill file sequence list if necessary */
                if (file_seq_i == H5FD_SEQ_LIST_LEN) {
                    if (H5S_SELECT_ITER_GET_SEQ_LIST(&file_iter, H5FD_SEQ_LIST_LEN, SIZE_MAX, &file_nseq,
                                                     &dummy_nelem, file_off, file_len) < 0)
                        HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "sequence length generation failed")

                    file_seq_i = 0;
                }
                HDassert(file_seq_i <= file_nseq);

                /* Refill memory sequence list if necessary */
                if (mem_seq_i == H5FD_SEQ_LIST_LEN) {
                    if (H5S_SELECT_ITER_GET_SEQ_LIST(&mem_iter, H5FD_SEQ_LIST_LEN, SIZE_MAX, &mem_nseq,
                                                     &dummy_nelem, mem_off, mem_len) < 0)
                        HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "sequence length generation failed")

                    if (!mem_nseq && file_seq_i < file_nseq)
                        HGOTO_ERROR(H5E_INTERNAL, H5E_BADVALUE, FAIL,
                                    "memory selection terminated before file selection")

                    mem_seq_i = 0;
                }
                HDassert(mem_seq_i <= mem_nseq);
            }

            if (mem_seq_i < mem_nseq)
                HGOTO_ERROR(H5E_INTERNAL, H5E_BADVALUE, FAIL,
                            "file selection terminated before memory selection")

            /* Terminate iterators */
            if (H5S_SELECT_ITER_RELEASE(&file_iter) < 0)
                HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "can't release file selection iterator")
            if (H5S_SELECT_ITER_RELEASE(&mem_iter) < 0)
                HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "can't release memory selection iterator")
        }
    }

done:
    /* undo the base addr offset to the offsets array if necessary */
    if (offsets_cooked) {

        HDassert(file->base_addr > 0);

        for (i = 0; i < count; i++) {

            offsets[i] -= file->base_addr;
        }
    }
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_read_selection() */

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
 *              If the underlying VFD supports selection reads, pass the
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
H5FD_write_selection(H5FD_t *file, uint32_t count, H5FD_mem_t type, H5S_t *mem_spaces[], H5S_t *file_spaces[],
                     haddr_t offsets[], size_t element_sizes[], const void *bufs[] /* out */)
{
    hbool_t  offsets_cooked = FALSE;
    hbool_t  extend_sizes   = FALSE;
    uint32_t i;
    size_t   element_size;
    hid_t    dxpl_id   = H5I_INVALID_HID; /* DXPL for operation */
    herr_t   ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(file);
    HDassert(file->cls);
    HDassert((mem_spaces) || (count == 0));
    HDassert((file_spaces) || (count == 0));
    HDassert((offsets) || (count == 0));
    HDassert((element_sizes) || (count == 0));
    HDassert((bufs) || (count == 0));

    /* Verify that the first elements of the element_sizes array is valid. */
    HDassert((count == 0) || (element_sizes[0] != 0));

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
    /*if (file->cls->write_selection) {

        if ((file->cls->write_selection)(file, count, type, mem_spaces, file_spaces, offsets, element_sizes,
    bufs) < 0)

            HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "driver write selection request failed")
    }
    else*/
    {
        hsize_t        file_off[H5FD_SEQ_LIST_LEN];
        size_t         file_len[H5FD_SEQ_LIST_LEN];
        hsize_t        mem_off[H5FD_SEQ_LIST_LEN];
        size_t         mem_len[H5FD_SEQ_LIST_LEN];
        size_t         file_seq_i;
        size_t         mem_seq_i;
        size_t         file_nseq;
        size_t         mem_nseq;
        size_t         io_len;
        size_t         dummy_nelem;
        H5S_sel_iter_t file_iter;
        H5S_sel_iter_t mem_iter;

        /* otherwise, implement the selection write as a sequence of regular
         * write calls.
         */
        extend_sizes = FALSE;

        for (i = 0; i < count; i++) {

            /* we have already verified that element_sizes[0] != 0 and */

            if (!extend_sizes) {

                if (element_sizes[i] == 0) {

                    extend_sizes = TRUE;
                    element_size = element_sizes[i - 1];
                }
                else {

                    element_size = element_sizes[i];
                }
            }

            /* Initialize sequence lists for memory and file spaces */
            if (H5S_select_iter_init(&file_iter, file_spaces[i], element_size, 0) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "can't initialize sequence list for file space")
            if (H5S_select_iter_init(&mem_iter, mem_spaces[i], element_size, 0) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "can't initialize sequence list for memory space")

            /* Fill sequence lists */
            if (H5S_SELECT_ITER_GET_SEQ_LIST(&file_iter, H5FD_SEQ_LIST_LEN, SIZE_MAX, &file_nseq,
                                             &dummy_nelem, file_off, file_len) < 0)
                HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "sequence length generation failed")
            if (H5S_SELECT_ITER_GET_SEQ_LIST(&mem_iter, H5FD_SEQ_LIST_LEN, SIZE_MAX, &mem_nseq, &dummy_nelem,
                                             mem_off, mem_len) < 0)
                HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "sequence length generation failed")
            if (file_nseq && !mem_nseq)
                HGOTO_ERROR(H5E_INTERNAL, H5E_BADVALUE, FAIL,
                            "memory selection is empty but file selection is not")
            file_seq_i = 0;
            mem_seq_i  = 0;

            while (file_seq_i < file_nseq) {
                /* Calculate length of this IO */
                io_len = MIN(file_len[file_seq_i], mem_len[mem_seq_i]);

                /* Issue scalar write call */
                if ((file->cls->write)(file, type, dxpl_id, file_off[file_seq_i], io_len,
                                       (const void *)(((const uint8_t *)bufs[i]) + mem_off[mem_seq_i])) < 0)
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

                /* Refill file sequence list if necessary */
                if (file_seq_i == H5FD_SEQ_LIST_LEN) {
                    if (H5S_SELECT_ITER_GET_SEQ_LIST(&file_iter, H5FD_SEQ_LIST_LEN, SIZE_MAX, &file_nseq,
                                                     &dummy_nelem, file_off, file_len) < 0)
                        HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "sequence length generation failed")

                    file_seq_i = 0;
                }
                HDassert(file_seq_i <= file_nseq);

                /* Refill memory sequence list if necessary */
                if (mem_seq_i == H5FD_SEQ_LIST_LEN) {
                    if (H5S_SELECT_ITER_GET_SEQ_LIST(&mem_iter, H5FD_SEQ_LIST_LEN, SIZE_MAX, &mem_nseq,
                                                     &dummy_nelem, mem_off, mem_len) < 0)
                        HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "sequence length generation failed")

                    if (!mem_nseq && file_seq_i < file_nseq)
                        HGOTO_ERROR(H5E_INTERNAL, H5E_BADVALUE, FAIL,
                                    "memory selection terminated before file selection")

                    mem_seq_i = 0;
                }
                HDassert(mem_seq_i <= mem_nseq);
            }

            if (mem_seq_i < mem_nseq)
                HGOTO_ERROR(H5E_INTERNAL, H5E_BADVALUE, FAIL,
                            "file selection terminated before memory selection")

            /* Terminate iterators */
            if (H5S_SELECT_ITER_RELEASE(&file_iter) < 0)
                HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "can't release file selection iterator")
            if (H5S_SELECT_ITER_RELEASE(&mem_iter) < 0)
                HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "can't release memory selection iterator")
        }
    }

done:
    /* undo the base addr offset to the offsets array if necessary */
    if (offsets_cooked) {

        HDassert(file->base_addr > 0);

        for (i = 0; i < count; i++) {

            offsets[i] -= file->base_addr;
        }
    }
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_write_selection() */

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
