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
 * Created:		H5Ftest.c
 *			Jan  3 2007
 *			Quincey Koziol
 *
 * Purpose:		File testing routines.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#include "H5Fmodule.h" /* This source code file is part of the H5F module  */
#define H5F_TESTING    /* Suppress warning about H5F testing funcs         */
#define H5G_FRIEND     /* Suppress error about including H5Gpkg.h          */
#define H5G_TESTING    /* Suppress warning about H5G testing funcs         */
#define H5SM_FRIEND    /* Suppress error about including H5SMpkg.h         */
#define H5SM_TESTING   /* Suppress warning about H5SM testing funcs        */

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions                        */
#include "H5FDprivate.h" /* File Drivers                             */
#include "H5CXprivate.h" /* API Contexts                             */
#include "H5Eprivate.h"  /* Error handling                           */
#include "H5Fpkg.h"      /* File access                              */
#include "H5Gpkg.h"      /* Groups                                   */
#include "H5Iprivate.h"  /* IDs                                      */
#include "H5SMpkg.h"     /* Shared object header messages            */
#include "H5MMprivate.h" /* Memory management                        */
#include "H5VLprivate.h" /* Virtual Object Layer                     */

/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/

/********************/
/* Package Typedefs */
/********************/

/********************/
/* Local Prototypes */
/********************/
static herr_t H5F__vfd_swmr_decode_md_hdr(int md_fd, H5FD_vfd_swmr_md_header *md_hdr);
static herr_t H5F__vfd_swmr_decode_md_idx(int md_fd, H5FD_vfd_swmr_md_header *md_hdr,
                                          H5FD_vfd_swmr_md_index *md_idx);
static herr_t H5F__vfd_swmr_verify_md_hdr_and_idx(H5F_t *f, H5FD_vfd_swmr_md_header *md_hdr,
                                                  H5FD_vfd_swmr_md_index *md_idx, unsigned num_entries,
                                                  H5FD_vfd_swmr_idx_entry_t *index);

/*********************/
/* Package Variables */
/*********************/

/*****************************/
/* Library Private Variables */
/*****************************/
/* Declare external the free list for H5FD_vfd_swmr_idx_entry_t */
H5FL_SEQ_EXTERN(H5FD_vfd_swmr_idx_entry_t);

/*******************/
/* Local Variables */
/*******************/

/*-------------------------------------------------------------------------
 * Function:    H5F__get_sohm_mesg_count_test
 *
 * Purpose:     Retrieve the number of shared messages of a given type in a file
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Jan  3, 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F__get_sohm_mesg_count_test(hid_t file_id, unsigned type_id, size_t *mesg_count)
{
    H5F_t * file;                     /* File info */
    hbool_t api_ctx_pushed = FALSE;   /* Whether API context pushed */
    herr_t  ret_value      = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check arguments */
    if (NULL == (file = (H5F_t *)H5VL_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")

    /* Push API context */
    if (H5CX_push() < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "can't set API context")
    api_ctx_pushed = TRUE;

    /* Retrieve count for message type */
    if (H5SM__get_mesg_count_test(file, type_id, mesg_count) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't retrieve shared message count")

done:
    if (api_ctx_pushed && H5CX_pop() < 0)
        HDONE_ERROR(H5E_FILE, H5E_CANTRESET, FAIL, "can't reset API context")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__get_sohm_mesg_count_test() */

/*-------------------------------------------------------------------------
 * Function:    H5F__check_cached_stab_test
 *
 * Purpose:     Check that a file's superblock contains a cached symbol
 *              table entry, that the entry matches that in the root
 *              group's object header, and check that the addresses are
 *              valid.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:	Neil Fortner
 *	        Mar  31, 2009
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F__check_cached_stab_test(hid_t file_id)
{
    H5F_t * file;                     /* File info */
    hbool_t api_ctx_pushed = FALSE;   /* Whether API context pushed */
    herr_t  ret_value      = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check arguments */
    if (NULL == (file = (H5F_t *)H5VL_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")

    /* Push API context */
    if (H5CX_push() < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "can't set API context")
    api_ctx_pushed = TRUE;

    /* Verify the cached stab info */
    if (H5G__verify_cached_stab_test(H5G_oloc(file->shared->root_grp), file->shared->sblock->root_ent) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to verify cached symbol table info")

done:
    if (api_ctx_pushed && H5CX_pop() < 0)
        HDONE_ERROR(H5E_FILE, H5E_CANTRESET, FAIL, "can't reset API context")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__check_cached_stab_test() */

/*-------------------------------------------------------------------------
 * Function:    H5F__get_maxaddr_test
 *
 * Purpose:     Retrieve the maximum address for a file
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *	        Jun 10, 2009
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F__get_maxaddr_test(hid_t file_id, haddr_t *maxaddr)
{
    H5F_t *file;                /* File info */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check arguments */
    if (NULL == (file = (H5F_t *)H5VL_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")

    /* Retrieve maxaddr for file */
    *maxaddr = file->shared->maxaddr;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__get_maxaddr_test() */

/*-------------------------------------------------------------------------
 * Function:    H5F__get_sbe_addr_test
 *
 * Purpose:     Retrieve the address of a superblock extension's object header
 *              for a file
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *	        Jul 10, 2016
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F__get_sbe_addr_test(hid_t file_id, haddr_t *sbe_addr)
{
    H5F_t *file;                /* File info */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check arguments */
    if (NULL == (file = (H5F_t *)H5VL_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")

    /* Retrieve maxaddr for file */
    *sbe_addr = file->shared->sblock->ext_addr;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__get_sbe_addr_test() */

/*-------------------------------------------------------------------------
 * Function:    H5F__same_file_test
 *
 * Purpose:     Check if two file IDs refer to the same underlying file.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *	        Oct 13, 2018
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5F__same_file_test(hid_t file_id1, hid_t file_id2)
{
    H5F_t *file1, *file2;    /* File info */
    htri_t ret_value = FAIL; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check arguments */
    if (NULL == (file1 = (H5F_t *)H5VL_object_verify(file_id1, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")
    if (NULL == (file2 = (H5F_t *)H5VL_object_verify(file_id2, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")

    /* If they are using the same underlying "shared" file struct, they are the same file */
    ret_value = (file1->shared == file2->shared);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__same_file_test() */

/*-------------------------------------------------------------------------
 * Function:    H5F__reparse_file_lock_variable_test
 *
 * Purpose:     Re-parse the file locking environment variable.
 *
 *              Since getenv(3) is fairly expensive, we only parse it once,
 *              when the library opens. This test function is used to
 *              re-parse the environment variable after we've changed it
 *              with setnev(3).
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:	Dana Robinson
 *              Summer 2020
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F__reparse_file_lock_variable_test(void)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    /* Check the file locking environment variable */
    if (H5F__parse_file_lock_env_var(&use_locks_env_g) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to parse file locking environment variable")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__reparse_file_lock_variable_test() */

/*
 * VFD SWMR tests
 */

/*-------------------------------------------------------------------------
 * Function:    H5F__vfd_swmr_writer_create_open_flush_test
 *
 * Purpose:     Verify info in the header and index when:
 *              (1) creating an HDF5 file
 *              (2) opening an existing HDF5 file
 *              (3) flushing an HDF5 file
 *
 *              Open the metadata file
 *              Verify the file size is as expected (md_pages_reserved)
 *              For file create:
 *                  --No header magic is found
 *              For file open or file flush:
 *                  --Read and decode the header and index in the metadata file
 *                  --Verify info in the header and index read from
 *                    the metadata file is as expected (empty index)
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F__vfd_swmr_writer_create_open_flush_test(hid_t file_id, hbool_t file_create)
{
    H5F_t *                 f;                   /* File pointer */
    h5_stat_t               stat_buf;            /* Buffer for stat info */
    H5FD_vfd_swmr_md_header md_hdr;              /* Header for the metadata file */
    H5FD_vfd_swmr_md_index  md_idx;              /* Indedx for the metadata file */
    int                     md_fd     = -1;      /* The metadata file descriptor */
    herr_t                  ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check arguments */
    if (NULL == (f = (H5F_t *)H5VL_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")

    /* Open the metadata file */
    if ((md_fd = HDopen(f->shared->vfd_swmr_config.md_file_path, O_RDONLY)) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "error opening metadata file")

    /* Verify the minimum size for the metadata file */
    if (HDstat(f->shared->vfd_swmr_config.md_file_path, &stat_buf) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_BADFILE, FAIL, "unable to stat the metadata file")
    if (stat_buf.st_size <
        (HDoff_t)((hsize_t)f->shared->vfd_swmr_config.md_pages_reserved * f->shared->fs_page_size))
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "incorrect metadata file size")

    if (file_create) { /* Creating file */
        uint32_t hdr_magic;

        /* Seek to the beginning of the file */
        if (HDlseek(md_fd, (HDoff_t)H5FD_MD_HEADER_OFF, SEEK_SET) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_SEEKERROR, FAIL, "error seeking metadata file")

        /* Try to read the magic for header */
        if (HDread(md_fd, &hdr_magic, H5_SIZEOF_MAGIC) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_READERROR, FAIL, "error reading metadata file")

        /* Verify that there is no header magic in the metadata file */
        if (HDmemcmp(&hdr_magic, H5FD_MD_HEADER_MAGIC, (size_t)H5_SIZEOF_MAGIC) == 0)
            HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "error finding header magic in the metadata file")
    }
    else { /* Opening or flushing the file */

        HDmemset(&md_hdr, 0, sizeof(H5FD_vfd_swmr_md_header));
        HDmemset(&md_idx, 0, sizeof(H5FD_vfd_swmr_md_index));

        /* Decode the header */
        if (H5F__vfd_swmr_decode_md_hdr(md_fd, &md_hdr) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTDECODE, FAIL, "error decoding header in the metadata file")

        /* Decode the index */
        if (H5F__vfd_swmr_decode_md_idx(md_fd, &md_hdr, &md_idx) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTDECODE, FAIL, "error decoding index in the metadata file")

        /* Verify info in header and index read from the metadata file */
        if (H5F__vfd_swmr_verify_md_hdr_and_idx(f, &md_hdr, &md_idx, 0, NULL) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL,
                        "incorrect info found in header and index of the metadata file")
    }

done:
    /* Free the index entries */
    if (!file_create && md_idx.entries) {
        HDassert(md_idx.num_entries);
        H5MM_free(md_idx.entries);
    }
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__vfd_swmr_writer_create_open_flush_test() */

/*-------------------------------------------------------------------------
 * Function:    H5F__vfd_swmr_decode_md_hdr
 *
 * Purpose:     Decode header and verify header magic
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__vfd_swmr_decode_md_hdr(int md_fd, H5FD_vfd_swmr_md_header *md_hdr)
{
    uint64_t index_length;
    uint8_t  image[H5FD_MD_HEADER_SIZE]; /* Buffer for the header image */
    uint8_t *p         = NULL;           /* Points to the image */
    herr_t   ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_PACKAGE

    p = image;

    /* Seek to the beginning of the file */
    if (HDlseek(md_fd, (HDoff_t)H5FD_MD_HEADER_OFF, SEEK_SET) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_SEEKERROR, FAIL, "error seeking metadata file")

    /* Read the header */
    if (HDread(md_fd, image, H5FD_MD_HEADER_SIZE) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_READERROR, FAIL, "error reading metadata file")

    /* Verify magic for header */
    if (HDmemcmp(p, H5FD_MD_HEADER_MAGIC, (size_t)H5_SIZEOF_MAGIC) != 0)
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "does not find header magic in the metadata file")

    p += H5_SIZEOF_MAGIC;

    /* Deserialize fs_page_size, tick_num, index_offset, index_length */
    UINT32DECODE(p, md_hdr->fs_page_size);
    UINT64DECODE(p, md_hdr->tick_num);
    UINT64DECODE(p, md_hdr->index_offset);
    if ((index_length = uint64_decode(&p)) > SIZE_MAX) {
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "index is too long")
    }
    md_hdr->index_length = (size_t)index_length;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5F__vfd_swmr_decode_md_hdr() */

/*-------------------------------------------------------------------------
 * Function:    H5F__vfd_swmr_decode_md_idx
 *
 * Purpose:     Decode index and verify index magic
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__vfd_swmr_decode_md_idx(int md_fd, H5FD_vfd_swmr_md_header *md_hdr, H5FD_vfd_swmr_md_index *md_idx)
{
    uint8_t *image = NULL;        /* Points to the buffer for the index image */
    uint8_t *p     = NULL;        /* Points to the image */
    unsigned i;                   /* Local index variable */
    herr_t   ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Allocate buffer for the index image */
    if (NULL == (image = H5MM_malloc(md_hdr->index_length)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, FAIL, "memory allocation failed for index on disk buffer")

    p = image;

    /* Seek to the position of the index */
    if (HDlseek(md_fd, (HDoff_t)md_hdr->index_offset, SEEK_SET) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_SEEKERROR, FAIL, "unable to seek in metadata file")

    /* Read the index */
    if (HDread(md_fd, image, md_hdr->index_length) < (int64_t)md_hdr->index_length)
        HGOTO_ERROR(H5E_FILE, H5E_READERROR, FAIL, "error in reading the header in metadata file")

    /* Verify magic for index */
    if (HDmemcmp(p, H5FD_MD_INDEX_MAGIC, H5_SIZEOF_MAGIC) != 0)
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "no header magic in the metadata file")

    p += H5_SIZEOF_MAGIC;

    /* Deserialize tick_num and num_entries */
    UINT64DECODE(p, md_idx->tick_num);
    UINT32DECODE(p, md_idx->num_entries);

    /* Deserialize index entries */
    if (md_idx->num_entries) {
        md_idx->entries = H5MM_calloc(md_idx->num_entries * sizeof(md_idx->entries[0]));
        /* Allocate memory for the index entries */
        if (NULL == md_idx->entries)
            HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, FAIL, "memory allocation failed for index entries")

        /* Decode index entries */
        for (i = 0; i < md_idx->num_entries; i++) {
            UINT32DECODE(p, md_idx->entries[i].hdf5_page_offset);
            UINT32DECODE(p, md_idx->entries[i].md_file_page_offset);
            UINT32DECODE(p, md_idx->entries[i].length);
            UINT32DECODE(p, md_idx->entries[i].chksum);
        } /* end for */

    } /* end if */

done:
    /* Free the buffer */
    if (image)
        H5MM_free(image);
    if (ret_value < 0) {
        /* Free the index entries */
        if (md_idx->entries) {
            HDassert(md_idx->num_entries);
            H5MM_free(md_idx->entries);
        }
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5F__vfd_swmr_decode_md_idx() */

/*-------------------------------------------------------------------------
 * Function: H5F__vfd_swmr_verify_md_hdr_idx_test
 *
 * Purpose: Verify the header and index in the metadata file:
 *          --fs_page_size in md header is the same as that stored in "f"
 *          --index_length in md header is as indicated by num_entries
 *          --index_offset in md header is right after the header
 *          --number of entries in md index is num_entries
 *          --entries in md index is as indicated by num_entries and index
 *
 * Return:  SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__vfd_swmr_verify_md_hdr_and_idx(H5F_t *f, H5FD_vfd_swmr_md_header *md_hdr, H5FD_vfd_swmr_md_index *md_idx,
                                    unsigned num_entries, H5FD_vfd_swmr_idx_entry_t *index)
{
    unsigned i;                   /* Local index variable */
    herr_t   ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Verify fs_page_size read from header in the metadata file is fs_page_size in f */
    if (md_hdr->fs_page_size != f->shared->fs_page_size)
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "incorrect fs_page_size read from metadata file")

    /* Verify index_length read from header in the metadata file is the size of num_entries index */
    if (md_hdr->index_length != H5FD_MD_INDEX_SIZE(num_entries))
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "incorrect index_length read from metadata file")

    /* Verify index_offset read from header in the metadata file is the size of md header */
    if (md_hdr->index_offset != f->shared->fs_page_size)
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "incorrect index_offset read from metadata file")

    /* Verify num_entries read from index in the metadata file is num_entries */
    if (md_idx->num_entries != num_entries)
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "incorrect num_entries read from metadata file")

    /* Verify empty/non-empty index entries */
    if (num_entries == 0) {
        /* Verify the index is empty */
        if (md_idx->entries != NULL)
            HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "incorrect entries in index")
    }
    else {
        /* Verify entries */
        for (i = 0; i < num_entries; i++) {
            if (md_idx->entries[i].length != index[i].length)
                HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "incorrect length read from metadata file")

            if (md_idx->entries[i].hdf5_page_offset != index[i].hdf5_page_offset)
                HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL,
                            "incorrect hdf5_page_offset read from metadata file")

            if (md_idx->entries[i].md_file_page_offset != index[i].md_file_page_offset)
                HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL,
                            "incorrect md_file_page_offset read from metadata file")

            if (md_idx->entries[i].chksum != index[i].chksum)
                HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "incorrect chksum read from metadata file")
        }
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5F__vfd_swmr_verify_md_hdr_and_idx() */

static unsigned
count_shadow_defrees(shadow_defree_queue_t *shadow_defrees)
{
    shadow_defree_t *shadow_defree;
    unsigned         count = 0;

    TAILQ_FOREACH(shadow_defree, shadow_defrees, link)
    count++;

    return count;
}

/*-------------------------------------------------------------------------
 * Function: H5F__vfd_swmr_writer_md_test
 *
 * Purpose: Update the metadata file with the input index and verify
 *          the following:
 *          --info read from the metadata file is as indicated by
 *          the input: num_entries, index
 *          --# of entries on the delayed list is as indicated by
 *          the input: nshadow_defrees
 *
 * Return:  SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F__vfd_swmr_writer_md_test(hid_t file_id, unsigned num_entries, H5FD_vfd_swmr_idx_entry_t *index,
                             unsigned nshadow_defrees)
{
    H5F_t *                 f;                   /* File pointer */
    int                     md_fd = -1;          /* The metadata file descriptor */
    H5FD_vfd_swmr_md_header md_hdr;              /* Header for the metadata file */
    H5FD_vfd_swmr_md_index  md_idx;              /* Index for the metadata file */
    herr_t                  ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    HDmemset(&md_hdr, 0, sizeof(H5FD_vfd_swmr_md_header));
    HDmemset(&md_idx, 0, sizeof(H5FD_vfd_swmr_md_index));

    /* Check arguments */
    if (NULL == (f = (H5F_t *)H5VL_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")

    /* Update the metadata file with the input index */
    if (H5F_update_vfd_swmr_metadata_file(f, num_entries, index) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, FAIL, "error updating the md file with the index")

    /* Verify the number of entries in the delayed list is as expected */
    if (count_shadow_defrees(&f->shared->shadow_defrees) == nshadow_defrees)
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "incorrect # of entries in the delayed list")

    /* Open the metadata file */
    if ((md_fd = HDopen(f->shared->vfd_swmr_config.md_file_path, O_RDONLY)) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "error opening metadata file")

    /* Decode the header in the metadata file */
    if (H5F__vfd_swmr_decode_md_hdr(md_fd, &md_hdr) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTDECODE, FAIL, "error decoding header in the metadata file")

    /* Decode the index in the metadata file */
    if (H5F__vfd_swmr_decode_md_idx(md_fd, &md_hdr, &md_idx) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "error decoding index in the metadata file")

    /* Verify info read from the metadata file is the same as the input index */
    if (H5F__vfd_swmr_verify_md_hdr_and_idx(f, &md_hdr, &md_idx, num_entries, index) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL,
                    "incorrect info found in header and index of the metadata file")

done:
    /* Free the index entries */
    if (md_idx.entries) {
        HDassert(md_idx.num_entries);
        H5MM_free(md_idx.entries);
    }
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5F__vfd_swmr_writer_md_test() */
