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
 * Created:             H5Fvfd_swmr.c
 *                      Oct 10 2019
 *
 * Purpose:             Functions for VFD SWMR.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#include "H5Fmodule.h" /* This source code file is part of the H5F module */
#define H5FD_FRIEND    /*suppress error about including H5FDpkg   */

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions                        */
#include "H5Aprivate.h"  /* Attributes                               */
#include "H5ACprivate.h" /* Metadata cache                           */
#include "H5CXprivate.h" /* API Contexts                             */
#include "H5Dprivate.h"  /* Datasets                                 */
#include "H5Eprivate.h"  /* Error handling                           */
#include "H5Fpkg.h"      /* File access                              */
#include "H5FDpkg.h"     /* File drivers                             */
#include "H5Gprivate.h"  /* Groups                                   */
#include "H5Iprivate.h"  /* IDs                                      */
#include "H5Lprivate.h"  /* Links                                    */
#include "H5MFprivate.h" /* File memory management                   */
#include "H5MVprivate.h" /* File memory management for VFD SWMR      */
#include "H5MMprivate.h" /* Memory management                        */
#include "H5Pprivate.h"  /* Property lists                           */
#include "H5SMprivate.h" /* Shared Object Header Messages            */
#include "H5Tprivate.h"  /* Datatypes                                */

/****************/
/* Local Macros */
/****************/

#define nanosecs_per_second    1000000000 /* nanoseconds per second */
#define nanosecs_per_tenth_sec 100000000  /* nanoseconds per 0.1 second */

/* Declare an array of string to identify the VFD SMWR Log tags.
 * Note this array is used to generate the entry tag by the log reporting macro
 * H5F_POST_VFD_SWMR_LOG_ENTRY.
 *
 * The following is the first version. Developers can add/modify the tags as necessary.
 *
 * If the entry code is 0, H5Fvfd_swmr_log_tags[0] is used to report the entry tag.
 * H5F_POST_VFD_SWMR_LOG_ENTRY(f, EOT_PROCESSING_TIME, log_msg) will put the log_msg attached to
 * the entry tag "EOT_PROCESSING_TIME".
 * The entry code number is listed in the comment for convenience.
 * Currently for the production mode, only the "EOT_PROCESSING_TIME" is present.
 */

/* clang-format off */
static const char *H5Fvfd_swmr_log_tags[] = {
                                             "EOT_PROCESSING_TIME",         /* 0 */
                                             "FILE_OPEN",                   /* 1 */                        
                                             "FILE_CLOSE",                  /* 2 */
                                             "EOT_TRIGGER_TIME",            /* 3 */
                                             "EOT_META_FILE_INDEX"          /* 4 */
                                            };
/* clang-format on */

/* This string defines the format of the VFD SWMR log file.
 * The current maximum length of entry tag string is set to 26.
 * One can enlarge or reduce this number as necessary.
 * For example, to enlarge the maximum length of entry tag string to 30,
 * Just change 26 to 30 in the following line, like
 * const char *log_fmt_str="%-30s: %.3lf s: %s\n";
 */
const char *log_fmt_str = "%-26s: %.3lf s: %s\n";

/* The length of the EOT processing time log message, subject to change */
const unsigned int eot_pt_log_mesg_length = 48;

/* The length of error message in the log */
const unsigned int log_err_mesg_length = 14;

/********************/
/* Local Prototypes */
/********************/

static herr_t H5F__vfd_swmr_update_end_of_tick_and_tick_num(H5F_shared_t *, hbool_t);
static herr_t H5F__vfd_swmr_construct_write_md_hdr(H5F_shared_t *, uint32_t, uint8_t *);
static herr_t H5F__vfd_swmr_construct_write_md_idx(H5F_shared_t *, uint32_t,
                                                   struct H5FD_vfd_swmr_idx_entry_t[], uint8_t *);
static herr_t H5F__idx_entry_cmp(const void *_entry1, const void *_entry2);
static herr_t H5F__vfd_swmr_create_index(H5F_shared_t *);
static herr_t H5F__vfd_swmr_writer__wait_a_tick(H5F_t *);

static herr_t H5F__vfd_swmr_construct_ud_hdr(H5F_vfd_swmr_updater_t *updater);
static herr_t H5F__vfd_swmr_construct_ud_cl(H5F_vfd_swmr_updater_t *updater);
static herr_t H5F__generate_updater_file(H5F_t *f, uint32_t num_entries, uint16_t flags,
                                         uint8_t *md_file_hdr_image_ptr, size_t md_file_hdr_image_len,
                                         uint8_t *md_file_index_image_ptr, uint64_t md_file_index_offset,
                                         size_t md_file_index_image_len);

/*********************/
/* Package Variables */
/*********************/

/*
 * Globals for VFD SWMR
 */

unsigned int vfd_swmr_api_entries_g = 0; /* Times the library was entered
                                          * and re-entered minus the times
                                          * it was exited.  We only perform
                                          * the end-of-tick processing
                                          * on the 0->1 and 1->0
                                          * transitions.
                                          */
/*
 *  The head of the end of tick queue (EOT queue) for files opened in either
 *  VFD SWMR write or VFD SWMR read mode
 */
eot_queue_t eot_queue_g = TAILQ_HEAD_INITIALIZER(eot_queue_g);

/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage the shadow_defree_t struct */
H5FL_DEFINE(shadow_defree_t);

/* Declare a free list to manage the eot_queue_entry_t struct */
H5FL_DEFINE(eot_queue_entry_t);

/*-------------------------------------------------------------------------
 *
 * Function:    H5F_vfd_swmr_init
 *
 * Purpose:     Initialize globals and the corresponding fields in
 *              file pointer.
 *
 *              For both VFD SWMR writer and reader:
 *
 *                  --set end_of_tick to the current time + tick length
 *
 *              For VFD SWMR writer:
 *
 *                  --set f->shared->tick_num to 1
 *                  --create the metadata file
 *                  --when opening an existing HDF5 file, write header and
 *                    empty index in the metadata file
 *
 *              For VFD SWMR reader:
 *
 *                  --set f->shared->tick_num to the current tick read from the
 *                    metadata file
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  Vailin Choi -- 11/??/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_vfd_swmr_init(H5F_t *f, hbool_t file_create)
{
    hsize_t       md_size;  /* Size of the metadata file */
    haddr_t       hdr_addr; /* Address returned from H5MV_alloc() */
    H5F_shared_t *shared = f->shared;
    uint8_t       md_idx_image[H5FD_MD_INDEX_SIZE(0)]; /* Buffer for metadata file index */
    uint8_t       md_hdr_image[H5FD_MD_HEADER_SIZE];   /* Buffer for metadata file header */
    herr_t        ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(H5F_SHARED_VFD_SWMR_CONFIG(shared));

    shared->vfd_swmr = TRUE;

    if (H5F_SHARED_INTENT(shared) & H5F_ACC_RDWR) {

        HDassert(shared->vfd_swmr_config.writer);
        HDassert(shared->vfd_swmr_config.maintain_metadata_file ||
                 shared->vfd_swmr_config.generate_updater_files);

        SIMPLEQ_INIT(&shared->lower_defrees);
        shared->vfd_swmr_writer = TRUE;
        shared->tick_num        = 0;

        /* Create the metadata file */
        if (((shared->vfd_swmr_md_fd = HDopen(shared->vfd_swmr_config.md_file_path,
                                              O_CREAT | O_RDWR | O_TRUNC, H5_POSIX_CREATE_MODE_RW))) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to create the metadata file")

        md_size = (hsize_t)shared->vfd_swmr_config.md_pages_reserved * shared->fs_page_size;

        if ((hdr_addr = H5MV_alloc(f, md_size)) == HADDR_UNDEF) {
            HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, "error allocating shadow-file header");
        }
        HDassert(H5F_addr_eq(hdr_addr, H5FD_MD_HEADER_OFF));

        shared->writer_index_offset = H5FD_MD_HEADER_SIZE;
        shared->vfd_swmr_md_eoa     = (haddr_t)md_size;

        /* When opening an existing HDF5 file, create header and empty
         * index in the metadata file
         */
        if (!file_create) {

            if (H5F__vfd_swmr_construct_write_md_idx(shared, 0, NULL, md_idx_image) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "fail to create index in md");

            if (H5F__vfd_swmr_construct_write_md_hdr(shared, 0, md_hdr_image) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "fail to create header in md");
        }

        /* For VFD SWMR testing: invoke callback if set to generate metadata file checksum */
        if (shared->generate_md_ck_cb) {
            if (shared->generate_md_ck_cb(shared->vfd_swmr_config.md_file_path, shared->updater_seq_num) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_SYSTEM, FAIL, "error from generate_md_ck_cb()")
        }

        /* Generate updater files if configuration indicates so */
        if (shared->vfd_swmr_config.generate_updater_files) {
            shared->updater_seq_num = 0;
            if (H5F__generate_updater_file(f, 0, file_create ? CREATE_METADATA_FILE_ONLY_FLAG : 0,
                                           md_hdr_image, H5FD_MD_HEADER_SIZE, md_idx_image,
                                           shared->writer_index_offset, H5FD_MD_INDEX_SIZE(0)) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_SYSTEM, FAIL, "can't generate updater file")
        }

        shared->tick_num = 1;

        if (H5PB_vfd_swmr__set_tick(shared) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_SYSTEM, FAIL, "Can't update page buffer current tick")
    }
    else { /* VFD SWMR reader  */

        HDassert(!shared->vfd_swmr_config.writer);

        shared->vfd_swmr_writer = FALSE;
        shared->max_jump_ticks  = 0;

        HDassert(shared->mdf_idx == NULL);

        /* allocate an index to save the initial index */
        if (H5F__vfd_swmr_create_index(shared) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, FAIL, "unable to allocate metadata file index");

        /* Set tick_num to the current tick read from the metadata file */
        shared->mdf_idx_entries_used = shared->mdf_idx_len;
        if (H5FD_vfd_swmr_get_tick_and_idx(shared->lf, FALSE, &shared->tick_num,
                                           &(shared->mdf_idx_entries_used), shared->mdf_idx) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTLOAD, FAIL, "unable to load/decode metadata file");

        vfd_swmr_reader_did_increase_tick_to(shared->tick_num);

#if 0  /* JRM */
        HDfprintf(stderr, 
                 "##### initialized index: tick/used/len = %lld/%d/%d #####\n",
                 shared->tick_num, shared->mdf_idx_entries_used,
                 shared->mdf_idx_len);
#endif /* JRM */
    }

    /* Update end_of_tick */
    if (H5F__vfd_swmr_update_end_of_tick_and_tick_num(shared, FALSE) < 0) {
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "unable to update end of tick");
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5F_vfd_swmr_init() */

/*-------------------------------------------------------------------------
 *
 * Function:    H5F_vfd_swmr_close_or_flush
 *
 * Purpose:     Used by the VFD SWMR writer when the HDF5 file is closed
 *              or flushed:
 *
 *              1) For file close:
 *                  --write header and an empty index to the metadata file
 *                  --increment tick_num
 *                  --close the metadata file
 *                  --unlink the metadata file
 *                  --close the free-space manager for the metadata file
 *
 *              2) For file flush:
 *                  --write header and an empty index to the metadata file
 *                  --increment tick_num
 *                  --start a new tick (??check with JM for sure)
 *                    ??update end_of_tick
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  Vailin Choi -- 11/??/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_vfd_swmr_close_or_flush(H5F_t *f, hbool_t closing)
{
    H5F_shared_t *   shared = f->shared;
    shadow_defree_t *curr;
    uint8_t          md_idx_image[H5FD_MD_INDEX_SIZE(0)]; /* Buffer for metadata file index */
    uint8_t          md_hdr_image[H5FD_MD_HEADER_SIZE];   /* Buffer for metadata file header */
    herr_t           ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(shared->vfd_swmr_writer);
    HDassert(shared->vfd_swmr_md_fd >= 0);

    /* Write empty index to the md file */
    if (H5F__vfd_swmr_construct_write_md_idx(shared, 0, NULL, md_idx_image) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "fail to create index in md");

    /* Write header to the md file */
    if (H5F__vfd_swmr_construct_write_md_hdr(shared, 0, md_hdr_image) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "fail to create header in md");

    if (closing) { /* For file close */

        /* Close the md file */
        if (HDclose(shared->vfd_swmr_md_fd) < 0)
            HSYS_GOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "unable to close the metadata file");
        shared->vfd_swmr_md_fd = -1;

        /* For VFD SWMR testing: invoke callback if set to generate metadata file checksum */
        if (shared->generate_md_ck_cb) {
            if (shared->generate_md_ck_cb(shared->vfd_swmr_config.md_file_path, shared->updater_seq_num) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_SYSTEM, FAIL, "error from generate_md_ck_cb()")
        }

        /* Unlink the md file */
        if (HDunlink(shared->vfd_swmr_config.md_file_path) < 0)
            HSYS_GOTO_ERROR(H5E_FILE, H5E_CANTREMOVE, FAIL, "unable to unlink the metadata file");

        /* Close the free-space manager for the metadata file */
        if (H5MV_close(f) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL,
                        "unable to close the free-space manager for the metadata file");

        /* Free the delayed list */
        while ((curr = TAILQ_FIRST(&shared->shadow_defrees)) != NULL) {
            TAILQ_REMOVE(&shared->shadow_defrees, curr, link);
            H5FL_FREE(shadow_defree_t, curr);
        }

        HDassert(TAILQ_EMPTY(&shared->shadow_defrees));

        if (shared->vfd_swmr_config.generate_updater_files) {
            if (H5F__generate_updater_file(f, 0, FINAL_UPDATE_FLAG, md_hdr_image, H5FD_MD_HEADER_SIZE,
                                           md_idx_image, shared->writer_index_offset,
                                           H5FD_MD_INDEX_SIZE(0)) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_SYSTEM, FAIL, "can't generate updater file")
        }
    }
    else { /* For file flush */
        /* Update end_of_tick */
        if (H5F__vfd_swmr_update_end_of_tick_and_tick_num(shared, TRUE) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "unable to update end of tick");
    }
#if 1 /*Kent  Save the end of close info. to the log file, subject to comment out. */
    if (closing)
        H5F_POST_VFD_SWMR_LOG_ENTRY(f, FILE_CLOSE, "VFD SWMR File close ends");
#endif
done:

    /* Kent: Stop the timer and close the VFD SWMR log file if it is turned on.
     * Please REVIEW to ensure this is the right place to
     * close the log file.
     */
    if (shared->vfd_swmr_log_on && closing) {
        H5_timer_stop(&(shared->vfd_swmr_log_start_time));
        HDfclose(shared->vfd_swmr_log_file_ptr);
    }
    /* Kent */
    FUNC_LEAVE_NOAPI(ret_value)
}

static int
shadow_range_defer_free(H5F_shared_t *shared, uint64_t offset, uint32_t length)
{
    shadow_defree_t *shadow_defree;

    if (NULL == (shadow_defree = H5FL_CALLOC(shadow_defree_t)))
        return -1;

    shadow_defree->offset   = offset;
    shadow_defree->length   = length;
    shadow_defree->tick_num = shared->tick_num;

    TAILQ_INSERT_HEAD(&shared->shadow_defrees, shadow_defree, link);
    return 0;
}

int
shadow_image_defer_free(H5F_shared_t *shared, const H5FD_vfd_swmr_idx_entry_t *entry)
{
    return shadow_range_defer_free(shared, entry->md_file_page_offset * shared->fs_page_size, entry->length);
}

/*-------------------------------------------------------------------------
 *
 * Function: H5F_update_vfd_swmr_metadata_file()
 *
 * Purpose:  Update the metadata file with the input index
 *
 *           --Sort index
 *
 *           --For each non-null entry_ptr in the index entries:
 *               --Insert previous image of the entry onto the delayed list
 *               --Allocate space for the entry in the metadata file
 *               --Compute checksum
 *               --Update index entry
 *               --Write the entry to the metadata file
 *               --Set entry_ptr to NULL
 *
 *           --Construct on disk image of the index and write index to the
 *             metadata file
 *
 *           --Construct on disk image of the header and write header to
 *             the metadata file
 *
 *           --Release time out entries from the delayed list to the
 *             free-space manager
 *
 * Return:   SUCCEED/FAIL
 *
 * Programmer: Vailin Choi  11/??/18
 *
 * Changes:  None.
 *
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_update_vfd_swmr_metadata_file(H5F_t *f, uint32_t num_entries, H5FD_vfd_swmr_idx_entry_t *index)
{
    H5F_shared_t *   shared = f->shared;
    shadow_defree_t *prev;
    shadow_defree_t *shadow_defree;
    haddr_t          md_addr; /* Address in the metadata file */
    uint32_t         i;       /* Local index variable */
    uint8_t *        md_idx_image = NULL;
    uint8_t          md_hdr_image[H5FD_MD_HEADER_SIZE]; /* Buffer for metadata file header */
    herr_t           ret_value = SUCCEED;               /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sort index entries by increasing offset in the HDF5 file */
    if (num_entries > 0) {
        HDqsort(index, num_entries, sizeof(*index), H5F__idx_entry_cmp);
        /* Assert that no HDF5 page offsets are duplicated. */
        for (i = 1; i < num_entries; i++)
            HDassert(index[i - 1].hdf5_page_offset < index[i].hdf5_page_offset);
    }

    /* For each non-null entry_ptr in the index:
     *
     *  --Insert previous image of the entry (if exists) to the
     *    beginning of the delayed list
     *
     *  --Allocate space for the entry in the metadata file
     *
     *  --Compute checksum, update the index entry, write entry to
     *    the metadata file
     *
     *  --Set entry_ptr to NULL when not generating updater files
     */
    for (i = 0; i < num_entries; i++) {

        if (index[i].entry_ptr == NULL)
            continue;

        HDassert(index[i].tick_of_last_change == f->shared->tick_num);

        /* Prepend previous image of the entry to the delayed list */
        if (index[i].md_file_page_offset) {
            if (shadow_image_defer_free(shared, &index[i]) == -1) {
                HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, FAIL, "unable to allocate the delayed entry")
            }
        }

        /* Allocate space for the entry in the metadata file */
        if ((md_addr = H5MV_alloc(f, index[i].length)) == HADDR_UNDEF)
            HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, "error in allocating space from the metadata file")

        HDassert(md_addr % shared->fs_page_size == 0);

        /* Compute checksum and update the index entry */
        index[i].md_file_page_offset = md_addr / shared->fs_page_size;
        index[i].checksum            = H5_checksum_metadata(index[i].entry_ptr, index[i].length, 0);

#if 0  /* JRM */
        HDfprintf(stderr, 
   "writing index[%d] fo/mdfo/l/checksum/fc/lc = %lld/%lld/%ld/%lx/%lx/%lx\n",
                i,
                  index[i].hdf5_page_offset,
                  index[i].md_file_page_offset,
                  index[i].length,
                  index[i].checksum,
                  (((char*)(index[i].entry_ptr))[0]),
                  (((char*)(index[i].entry_ptr))[4095]));

        HDassert(md_addr == index[i].md_file_page_offset * 
                            shared->fs_page_size);
        HDassert(shared->fs_page_size == 4096);
#endif /* JRM */

        if (shared->vfd_swmr_config.maintain_metadata_file) {

            /* Seek and write the entry to the metadata file */
            if (HDlseek(shared->vfd_swmr_md_fd, (HDoff_t)md_addr, SEEK_SET) < 0)

                HGOTO_ERROR(H5E_FILE, H5E_SEEKERROR, FAIL, "unable to seek in the metadata file")

            if (HDwrite(shared->vfd_swmr_md_fd, index[i].entry_ptr, index[i].length) !=
                (ssize_t)index[i].length)

                HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL,
                            "error in writing the page/multi-page entry to metadata file")
        }

        if (!shared->vfd_swmr_config.generate_updater_files)
            index[i].entry_ptr = NULL;
    }

    if ((md_idx_image = HDmalloc(H5FD_MD_INDEX_SIZE(num_entries))) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for md index")

    /* Construct and write index to the metadata file */
    if (H5F__vfd_swmr_construct_write_md_idx(shared, num_entries, index, md_idx_image) < 0)

        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "fail to construct & write index to md")

    /* Construct and write header to the md file */
    if (H5F__vfd_swmr_construct_write_md_hdr(shared, num_entries, md_hdr_image) < 0)

        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "fail to construct & write header to md")

    /*
     * Release time out entries from the delayed list by scanning the
     * list from the bottom up:
     *
     *      --release to the metadata file free space manager all index
     *        entries that have resided on the list for more than
     *        max_lag ticks
     *
     *      --remove the associated entries from the list
     */

    /* if (shared->tick_num <= shared->vfd_swmr_config.max_lag),
       it is too early for any reclamations to be due.
     */
    if (shared->tick_num > shared->vfd_swmr_config.max_lag) {

        TAILQ_FOREACH_REVERSE_SAFE(shadow_defree, &shared->shadow_defrees, shadow_defree_queue, link, prev)
        {

            if (shadow_defree->tick_num + shared->vfd_swmr_config.max_lag > shared->tick_num) {
                break; // No more entries are due for reclamation.
            }

            if (H5MV_free(f, shadow_defree->offset, shadow_defree->length) < 0) {
                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush clean entry");
            }

            TAILQ_REMOVE(&shared->shadow_defrees, shadow_defree, link);

            H5FL_FREE(shadow_defree_t, shadow_defree);
        }
    }

    /* For VFD SWMR testing: invoke callback if set to generate metadata file checksum */
    if (shared->generate_md_ck_cb) {
        if (shared->generate_md_ck_cb(shared->vfd_swmr_config.md_file_path, shared->updater_seq_num) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_SYSTEM, FAIL, "error from generate_md_ck_cb()")
    }

    /* Generate updater files with num_entries */
    if (shared->vfd_swmr_config.generate_updater_files)
        if (H5F__generate_updater_file(f, num_entries, 0, md_hdr_image, H5FD_MD_HEADER_SIZE, md_idx_image,
                                       shared->writer_index_offset, H5FD_MD_INDEX_SIZE(num_entries)) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_SYSTEM, FAIL, "can't generate updater file")

done:

    if (md_idx_image)
        HDfree(md_idx_image);

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5F_update_vfd_swmr_metadata_file() */

/*-------------------------------------------------------------------------
 *
 * Function: H5F_vfd_swmr_writer__delay_write
 *
 * Purpose:  Given the base address of a page of metadata, or of a multi-
 *           page metadata entry, determine whether the write must be
 *           delayed.
 *
 *           At the conceptual level, the VFD SWMR writer must delay the
 *           write of any metadata page or multi-page metadata that
 *           overwrites an existing metadata page or multi-page metadata
 *           entry until it has appeared in the metadata file index for
 *           at least max_lag ticks.  Since the VFD SWMR reader goes
 *           to the HDF5 file for any piece of metadata not listed in
 *           the metadata file index, failure to delay such writes can
 *           result in message from the future bugs.
 *
 *           The easy case is pages or multi-page metadata entries
 *           have just been allocated.  Obviously, these can be written
 *           immediately.  This case is tracked and tested by the page
 *           buffer proper.
 *
 *           This routine looks up the supplied page in the metadata file
 *           index.
 *
 *           If the entry doesn't exist, the function sets
 *           *untilp to the current tick plus max_lag.
 *
 *           If the entry exists, the function sets *untilp
 *           equal to the entries delayed flush field if it is greater than
 *           or equal to the current tick, or zero otherwise.
 *
 * Return:   SUCCEED/FAIL
 *
 * Programmer: John Mainzer 11/4/18
 *
 * Changes:  None.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_vfd_swmr_writer__delay_write(H5F_shared_t *shared, uint64_t page, uint64_t *untilp)
{
    uint64_t                   until;
    H5FD_vfd_swmr_idx_entry_t *ie_ptr;
    H5FD_vfd_swmr_idx_entry_t *idx;
    herr_t                     ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(shared);
    HDassert(shared->vfd_swmr);
    HDassert(shared->vfd_swmr_writer);

    idx = shared->mdf_idx;

    HDassert(idx != NULL || shared->tick_num <= 1);

    /* do a binary search on the metadata file index to see if
     * it already contains an entry for `page`.
     */

    if (idx == NULL) {
        ie_ptr = NULL;
    }
    else {
        ie_ptr = vfd_swmr_pageno_to_mdf_idx_entry(idx, shared->mdf_idx_entries_used, page, FALSE);
    }

    if (ie_ptr == NULL)
        until = shared->tick_num + shared->vfd_swmr_config.max_lag;
    else if (ie_ptr->delayed_flush >= shared->tick_num)
        until = ie_ptr->delayed_flush;
    else
        until = 0;

    if (until != 0 &&
        (until < shared->tick_num || shared->tick_num + shared->vfd_swmr_config.max_lag < until))
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "VFD SWMR write delay out of range")

    *untilp = until;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5F_vfd_swmr_writer__delay_write() */

/*-------------------------------------------------------------------------
 *
 * Function: H5F_vfd_swmr_writer__prep_for_flush_or_close
 *
 * Purpose:  In the context of the VFD SWMR writer, two issues must be
 *           addressed before the page buffer can be flushed -- as is
 *           necessary on both HDF5 file flush or close:
 *
 *           1) We must force an end of tick so as to clean the tick list
 *              in the page buffer.
 *
 *           2) If the page buffer delayed write list is not empty, we
 *              must repeatedly wait a tick and then run the writer end
 *              of tick function until the delayed write list drains.
 *
 *           This function manages these details.
 *
 * Return:   SUCCEED/FAIL
 *
 * Programmer: John Mainzer 11/27/18
 *
 * Changes:  None.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_vfd_swmr_writer__prep_for_flush_or_close(H5F_t *f)
{
    herr_t        ret_value = SUCCEED; /* Return value */
    H5F_shared_t *shared    = f->shared;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(shared->vfd_swmr);
    HDassert(shared->vfd_swmr_writer);
    HDassert(shared->page_buf);

    /* since we are about to flush the page buffer, force and end of
     * tick so as to avoid attempts to flush entries on the page buffer
     * tick list that were modified during the current tick.
     */
    if (H5F_vfd_swmr_writer_end_of_tick(f, TRUE) < 0)

        HGOTO_ERROR(H5E_FILE, H5E_SYSTEM, FAIL, "H5F_vfd_swmr_writer_end_of_tick() failed.")

    while (shared->page_buf->dwl_len > 0) {

        if (H5F__vfd_swmr_writer__wait_a_tick(f) < 0)

            HGOTO_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "wait a tick failed.")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5F_vfd_swmr_writer__prep_for_flush_or_close() */

static int
clean_shadow_index(H5F_t *f, uint32_t nentries, H5FD_vfd_swmr_idx_entry_t *idx, uint32_t *ndeletedp)
{
    H5F_shared_t *             shared = f->shared;
    uint32_t                   i, j, ndeleted, max_lag = shared->vfd_swmr_config.max_lag;
    uint64_t                   tick_num = shared->tick_num;
    H5FD_vfd_swmr_idx_entry_t *ie;

    for (i = j = ndeleted = 0; i < nentries; i++) {
        ie = &idx[i];

        if (ie->clean && ie->tick_of_last_flush + max_lag < tick_num) {

            HDassert(!ie->garbage);
            HDassert(ie->entry_ptr == NULL);

            if (ie->md_file_page_offset != 0) {
                if (shadow_image_defer_free(shared, ie) == -1)
                    return -1;
                ie->md_file_page_offset = 0;
            }
            ndeleted++;
            continue;
        }
        if (j != i)
            idx[j] = *ie;
        j++;
    }
    *ndeletedp = ndeleted;
    return 0;
}

/*-------------------------------------------------------------------------
 *
 * Function: H5F_vfd_swmr_writer_end_of_tick
 *
 * Purpose:  Main routine for managing the end of tick for the VFD
 *           SWMR writer.
 *
 *           This function performs all end of tick operations for the
 *           writer -- specifically:
 *
 *            1) If requested, flush all raw data to the HDF5 file.
 *
 *               (Not for first cut.)
 *
 *            2) Flush the metadata cache to the page buffer.
 *
 *               Note that we must run a tick after the destruction
 *               of the metadata cache, since this operation will usually
 *               dirty the first page in the HDF5 file.  However, the
 *               metadata cache will no longer exist at this point.
 *
 *               Thus, we must check for the existance of the metadata
 *               cache, and only attempt to flush it if it exists.
 *
 *            3) If this is the first tick (i.e. tick == 1), create the
 *               in memory version of the metadata file index.
 *
 *            4) Scan the page buffer tick list, and use it to update
 *               the metadata file index, adding or modifying entries as
 *               appropriate.
 *
 *            5) Scan the metadata file index for entries that can be
 *               removed -- specifically entries that have been written
 *               to the HDF5 file more than max_lag ticks ago, and haven't
 *               been modified since.
 *
 *               (This is an optimization -- address it later)
 *
 *            6) Update the metadata file.  Must do this before we
 *               release the tick list, as otherwise the page buffer
 *               entry images may not be available.
 *
 *            7) Release the page buffer tick list.
 *
 *            8) Release any delayed writes whose delay has expired.
 *
 *            9) Increment the tick, and update the end of tick.
 *
 *           In passing, generate log entries as appropriate.
 *
 * Return:   SUCCEED/FAIL
 *
 * Programmer: John Mainzer 11/4/18
 *
 * Changes:  None.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_vfd_swmr_writer_end_of_tick(H5F_t *f, hbool_t wait_for_reader)
{
    H5F_shared_t *shared                    = f->shared;
    uint32_t      idx_entries_added         = 0;
    uint32_t      idx_entries_modified      = 0;
    uint32_t      idx_entries_removed       = 0;
    uint32_t      idx_ent_not_in_tl         = 0;
    uint32_t      idx_ent_not_in_tl_flushed = 0;
    herr_t        ret_value                 = SUCCEED; /* Return value */
    hbool_t       incr_tick                 = FALSE;

    /* Kent: define the local variables to calculate the EOT time
             and write them to the log file. */
    H5_timevals_t current_time;
    double        start_elapsed_time, end_elapsed_time;
    unsigned int  temp_time;
    char *        log_msg;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(shared);
    HDassert(shared->page_buf);
    HDassert(shared->vfd_swmr_writer);

    /* Kent */
    /* Obtain the starting time for the logging info: the processing time of this function. */
    if (shared->vfd_swmr_log_on == true) {
        if (H5_timer_get_times(shared->vfd_swmr_log_start_time, &current_time) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get time from H5_timer_get_times")
        start_elapsed_time = current_time.elapsed;
    }
    /* Kent */

    if (!vfd_swmr_writer_may_increase_tick_to(shared->tick_num + 1, wait_for_reader))
        goto update_eot;

    incr_tick = TRUE;

    /* 1) If requested, flush all raw data to the HDF5 file.
     *
     */
    if (shared->vfd_swmr_config.flush_raw_data) {

        /* Test to see if b-tree corruption seen in VFD SWMR tests
         * is caused by client hiding data from the metadata cache.  Do
         * this by calling H5D_flush_all(), which flushes any cached
         * dataset storage.  Eventually, we will do this regardless
         * when the above flush_raw_data flag is set.
         */

        if (H5D_flush_all(f) < 0)

            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush dataset cache")

        if (H5MF_free_aggrs(f) < 0)

            HGOTO_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "can't release file space")
    }

    /* 2) If it exists, flush the metadata cache to the page buffer. */
    if (shared->cache) {

        if (H5AC_prep_for_file_flush(f) < 0)

            HDONE_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "prep for MDC flush failed")

        if (H5AC_flush(f) < 0)

            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't flush metadata cache to the page buffer")

        if (H5AC_secure_from_file_flush(f) < 0)

            HDONE_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "secure from MDC flush failed")
    }

    if (H5FD_truncate(shared->lf, FALSE) < 0)

        HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, "low level truncate failed")

    /* 3) If this is the first tick (i.e. tick == 1), create the
     *    in memory version of the metadata file index.
     */
    if ((shared->tick_num == 1) && (H5F__vfd_swmr_create_index(shared) < 0))

        HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, FAIL, "unable to allocate metadata file index")

    /* 4) Scan the page buffer tick list, and use it to update
     *    the metadata file index, adding or modifying entries as
     *    appropriate.
     */
    if (H5PB_vfd_swmr__update_index(f, &idx_entries_added, &idx_entries_modified, &idx_ent_not_in_tl,
                                    &idx_ent_not_in_tl_flushed) < 0)

        HGOTO_ERROR(H5E_FILE, H5E_SYSTEM, FAIL, "can't update MD file index")

    /* 5) Scan the metadata file index for entries that can be
     *    removed -- specifically entries that have been written
     *    to the HDF5 file more than max_lag ticks ago, and haven't
     *    been modified since.
     */
    if (clean_shadow_index(f, shared->mdf_idx_entries_used + idx_entries_added, shared->mdf_idx,
                           &idx_entries_removed) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_SYSTEM, FAIL, "can't clean shadow file index")

    /* 6) Update the metadata file.  Must do this before we
     *    release the tick list, as otherwise the page buffer
     *    entry images may not be available.
     *
     *    Note that this operation will restore the index to
     *    sorted order.
     */
    if (H5F_update_vfd_swmr_metadata_file(
            f, shared->mdf_idx_entries_used + idx_entries_added - idx_entries_removed, shared->mdf_idx) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_SYSTEM, FAIL, "can't update MD file")

    /* at this point the metadata file index should be sorted -- update
     * shared->mdf_idx_entries_used.
     */
    shared->mdf_idx_entries_used += idx_entries_added;
    shared->mdf_idx_entries_used -= idx_entries_removed;

    HDassert(shared->mdf_idx_entries_used <= shared->mdf_idx_len);

#if 0  /* JRM */
    H5F__vfd_swmr_writer__dump_index(f);
#endif /* JRM */

    /* 7) Release the page buffer tick list. */
    if (H5PB_vfd_swmr__release_tick_list(shared) < 0)

        HGOTO_ERROR(H5E_FILE, H5E_SYSTEM, FAIL, "can't release tick list")

    /* 8) Release any delayed writes whose delay has expired */
    if (H5PB_vfd_swmr__release_delayed_writes(shared) < 0)

        HGOTO_ERROR(H5E_FILE, H5E_SYSTEM, FAIL, "can't release delayed writes")

update_eot:

    /* 9) Increment the tick, and update the end of tick. */

    /* Update end_of_tick */
    if (H5F__vfd_swmr_update_end_of_tick_and_tick_num(shared, incr_tick) < 0)

        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "unable to update end of tick")

    /* Remove the entry from the EOT queue */
    if (H5F_vfd_swmr_remove_entry_eot(f) < 0)
        HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "unable to remove entry from EOT queue")

    /* Re-insert the entry that corresponds to f onto the EOT queue */
    if (H5F_vfd_swmr_insert_entry_eot(f) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "unable to insert entry into the EOT queue")

done:
    /* Kent: Calcuate the processing time and write the time info to the log file */
    if (shared->vfd_swmr_log_on == true) {
        if (H5_timer_get_times(shared->vfd_swmr_log_start_time, &current_time) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get time from H5_timer_get_times")
        end_elapsed_time = current_time.elapsed;
        if (NULL != (log_msg = HDmalloc(eot_pt_log_mesg_length * sizeof(char)))) {
            temp_time = (unsigned int)((end_elapsed_time - start_elapsed_time) * 1000);
            HDsprintf(log_msg, "Writer time is %u milliseconds", temp_time);
            H5F_POST_VFD_SWMR_LOG_ENTRY(f, EOT_PROCESSING_TIME, log_msg);
            HDfree(log_msg);
        }
    }
    /* Kent */
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 *
 * Function: H5F_vfd_swmr_writer__dump_index
 *
 * Purpose:  Dump a summary of the metadata file index.
 *
 * Return:   SUCCEED/FAIL
 *
 * Programmer: John Mainzer 12/14/19
 *
 * Changes:  None.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_vfd_swmr_writer__dump_index(H5F_shared_t *shared)
{
    unsigned int               i;
    uint32_t                   mdf_idx_len;
    uint32_t                   mdf_idx_entries_used;
    H5FD_vfd_swmr_idx_entry_t *index     = NULL;
    herr_t                     ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(shared);
    HDassert(shared->vfd_swmr);
    HDassert(shared->mdf_idx);

    index                = shared->mdf_idx;
    mdf_idx_len          = shared->mdf_idx_len;
    mdf_idx_entries_used = shared->mdf_idx_entries_used;

    HDfprintf(stderr, "\n\nDumping Index:\n\n");
    HDfprintf(stderr, "index len / entries used = %" PRIu32 " / %" PRIu32 "\n\n", mdf_idx_len,
              mdf_idx_entries_used);

    for (i = 0; i < mdf_idx_entries_used; i++) {

        HDfprintf(stderr, "%u: %" PRIu64 " %" PRIu64 " %" PRIu32 "\n", i, index[i].hdf5_page_offset,
                  index[i].md_file_page_offset, index[i].length);
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5F_vfd_swmr_writer__dump_index() */

/*-------------------------------------------------------------------------
 * Function: H5F_vfd_swmr_reader_end_of_tick
 *
 * Purpose:  Main routine for VFD SWMR reader end of tick operations.
 *           The following operations must be performed:
 *
 *           1) Direct the VFD SWMR reader VFD to load the current header
 *              from the metadata file, and report the current tick.
 *
 *              If the tick reported has not increased since the last
 *              call, do nothing and exit.
 *
 *           2) If the tick has increased, obtain a copy of the new
 *              index from the VFD SWMR reader VFD, and compare it with
 *              the old index to identify all pages that have been updated
 *              in the previous tick.
 *
 *              If any such pages or multi-page metadata entries are found:
 *
 *                 a) direct the page buffer to evict any such superceeded
 *                    pages, and
 *
 *                 b) direct the metadata cache to either evict or refresh
 *                    any entries residing in the superceeded pages.
 *
 *              Note that this operation MUST be performed in this order,
 *              as the metadata cache will refer to the page buffer
 *              when refreshing entries.
 *
 *           9) Increment the tick, and update the end of tick.
 *
 * Return:   SUCCEED/FAIL
 *
 * Programmer: John Mainzer 12/29/18
 *
 * Changes:  None.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_vfd_swmr_reader_end_of_tick(H5F_t *f, hbool_t entering_api)
{
    uint64_t                   tmp_tick_num = 0;
    H5FD_vfd_swmr_idx_entry_t *tmp_mdf_idx;
    uint32_t                   entries_added   = 0;
    uint32_t                   entries_removed = 0;
    uint32_t                   entries_moved   = 0;
    uint32_t                   tmp_mdf_idx_len;
    uint32_t                   tmp_mdf_idx_entries_used;
    uint32_t                   mdf_idx_entries_used;
    H5F_shared_t *             shared = f->shared;
    struct {
        uint64_t pgno;
        uint32_t length;
    } *change          = NULL;
    herr_t   ret_value = SUCCEED;
    uint32_t i, j, nchanges;
    H5FD_t * file = shared->lf;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(shared->page_buf);
    HDassert(shared->vfd_swmr);
    HDassert(!shared->vfd_swmr_writer);
    HDassert(file);

    /* 1) Direct the VFD SWMR reader VFD to load the current header
     *    from the metadata file, and report the current tick.
     *
     *    If the tick reported has not increased since the last
     *    call, do nothing and exit.
     */
    if (H5FD_vfd_swmr_get_tick_and_idx(file, TRUE, &tmp_tick_num, NULL, NULL) < 0)

        HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "error in retrieving tick_num from driver")

    /* This is ok if we're entering the API, but it should
     * not happen if we're exiting the API.
     */
    /* JRM  review this */
    /* The following line is added for more meaningful error message when
     * the long running API on the reader side exceeds the max_lag of ticks.
     *        KY 2021-09-02
     *               */
    if (!entering_api && tmp_tick_num >= shared->tick_num + shared->vfd_swmr_config.max_lag) {
        HGOTO_ERROR(H5E_FILE, H5E_SYSTEM, FAIL,
                    "Reader's API time exceeds max_lag of ticks, may increase the value of max_lag.");
    }
#if 0 /* Kent */
    /* The original code */
    HDassert(entering_api || tmp_tick_num < shared->tick_num + shared->vfd_swmr_config.max_lag);
#endif

    if (!entering_api) {
        H5FD_vfd_swmr_record_elapsed_ticks(shared->lf, tmp_tick_num - shared->tick_num);
    }

    if (tmp_tick_num != shared->tick_num) {
        const H5FD_vfd_swmr_idx_entry_t *new_mdf_idx;
        const H5FD_vfd_swmr_idx_entry_t *old_mdf_idx;
        uint32_t                         new_mdf_idx_entries_used;
        uint32_t                         old_mdf_idx_entries_used;

        /* swap the old and new metadata file indexes */

        tmp_mdf_idx              = shared->old_mdf_idx;
        tmp_mdf_idx_len          = shared->old_mdf_idx_len;
        tmp_mdf_idx_entries_used = shared->old_mdf_idx_entries_used;

        shared->old_mdf_idx              = shared->mdf_idx;
        shared->old_mdf_idx_len          = shared->mdf_idx_len;
        shared->old_mdf_idx_entries_used = shared->mdf_idx_entries_used;

        shared->mdf_idx              = tmp_mdf_idx;
        shared->mdf_idx_len          = tmp_mdf_idx_len;
        shared->mdf_idx_entries_used = tmp_mdf_idx_entries_used;

        /* if shared->mdf_idx is NULL, allocate an index */
        if (shared->mdf_idx == NULL && H5F__vfd_swmr_create_index(shared) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, FAIL, "unable to allocate metadata file index");

        mdf_idx_entries_used = shared->mdf_idx_len;

#if 0  /* JRM */
        HDfprintf(stderr, "--- reader EOT mdf_idx_entries_used = %d ---\n",
                  mdf_idx_entries_used);
#endif /* JRM */

        if (H5FD_vfd_swmr_get_tick_and_idx(file, FALSE, NULL, &mdf_idx_entries_used, shared->mdf_idx) < 0)
            HGOTO_ERROR(H5E_ARGS, H5E_CANTGET, FAIL, "error in retrieving tick_num from driver");

        HDassert(mdf_idx_entries_used <= shared->mdf_idx_len);

        shared->mdf_idx_entries_used = mdf_idx_entries_used;

#if 0  /* JRM */
        HDfprintf(stderr,
            "--- reader EOT index used / len = %" PRIu32 "/%" PRIu32 " ---\n",
            shared->mdf_idx_entries_used, shared->mdf_idx_len);
#endif /* JRM */

        new_mdf_idx              = shared->mdf_idx;
        old_mdf_idx              = shared->old_mdf_idx;
        new_mdf_idx_entries_used = shared->mdf_idx_entries_used;
        old_mdf_idx_entries_used = shared->old_mdf_idx_entries_used;

        change = malloc(sizeof(change[0]) * (old_mdf_idx_entries_used + new_mdf_idx_entries_used));

        if (change == NULL) {
            HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, FAIL, "unable to allocate removed pages list");
        }

        /* If an old metadata file index exists, compare it with the
         * new index and evict any modified, new, or deleted pages
         * and any associated metadata cache entries.
         *
         * Note that we must evict in two passes---page buffer first,
         * and then metadata cache.  This is necessary as the metadata
         * cache may attempt to refresh entries rather than evict them,
         * in which case it may access an entry in the page buffer.
         */

        for (i = j = nchanges = 0; i < old_mdf_idx_entries_used && j < new_mdf_idx_entries_used;) {
            const H5FD_vfd_swmr_idx_entry_t *oent = &old_mdf_idx[i], *nent = &new_mdf_idx[j];

            /* Verify that the old and new indices are sorted as expected. */
            HDassert(i == 0 || oent[-1].hdf5_page_offset < oent[0].hdf5_page_offset);

            HDassert(j == 0 || nent[-1].hdf5_page_offset < nent[0].hdf5_page_offset);

            if (oent->hdf5_page_offset == nent->hdf5_page_offset) {

                if (oent->md_file_page_offset != nent->md_file_page_offset) {

                    /* It's ok if the length changes, I think, but I need
                     * to think about how to perform MDC invalidation in the
                     * case where the new entry is *longer*, because the
                     * extension could overlap with a second entry.
                     */

                    /* JRM  review this */
                    /*  Kent: need to comment out the line to make reader iterate
                     *        a large number of groups
                     * */
#if 0 /*Kent*/
                    HDassert(oent->length == nent->length);
#endif

                    /* the page has been altered -- evict it and
                     * any contained metadata cache entries.
                     */
                    change[nchanges].pgno   = oent->hdf5_page_offset;
                    change[nchanges].length = oent->length;
                    nchanges++;
                    entries_moved++;
                }
                i++;
                j++;
            }
            else if (oent->hdf5_page_offset < nent->hdf5_page_offset) {
                /* the page has been removed from the new version
                 * of the index.  Evict it and any contained metadata
                 * cache entries.
                 *
                 * If we are careful about removing entries from the
                 * the index so as to ensure that they haven't changed
                 * for several ticks, we can probably omit this.  However,
                 * lets not worry about this for the first cut.
                 */
                change[nchanges].pgno   = oent->hdf5_page_offset;
                change[nchanges].length = oent->length;
                nchanges++;
                entries_removed++;
                i++;
            }
            else { /* oent->hdf5_page_offset >
                    * nent->hdf5_page_offset
                    */

                /* The page has been added to the index. */
                change[nchanges].pgno   = nent->hdf5_page_offset;
                change[nchanges].length = nent->length;
                nchanges++;
                entries_added++;
                j++;
            }
        }

        for (; j < new_mdf_idx_entries_used; j++) {
            const H5FD_vfd_swmr_idx_entry_t *nent = &new_mdf_idx[j];
            change[nchanges].pgno                 = nent->hdf5_page_offset;
            change[nchanges].length               = nent->length;
            nchanges++;
            entries_added++;
        }

        /* cleanup any left overs in the old index */
        for (; i < old_mdf_idx_entries_used; i++) {
            const H5FD_vfd_swmr_idx_entry_t *oent = &old_mdf_idx[i];

            /* the page has been removed from the new version of the
             * index.  Evict it from the page buffer and also evict any
             * contained metadata cache entries
             */
            change[nchanges].pgno   = oent->hdf5_page_offset;
            change[nchanges].length = oent->length;
            nchanges++;
            entries_removed++;
        }
        for (i = 0; i < nchanges; i++) {
            haddr_t page_addr = (haddr_t)(change[i].pgno * shared->page_buf->page_size);
            if (H5PB_remove_entry(shared, page_addr) < 0) {
                HGOTO_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "remove page buffer entry failed");
            }
        }
        for (i = 0; i < nchanges; i++) {
            if (H5C_evict_or_refresh_all_entries_in_page(f, change[i].pgno, change[i].length, tmp_tick_num) <
                0) {
                HGOTO_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "evict or refresh stale MDC entries failed");
            }
        }

#if 0  /* JRM */
        HDfprintf(stderr, "--- reader EOT pre new tick index "
            "used/len = %" PRIu32 "/ %" PRIu32 " ---\n",
            shared->mdf_idx_entries_used, shared->mdf_idx_len);
#endif /* JRM */
        shared->max_jump_ticks = MAX(shared->max_jump_ticks, (tmp_tick_num - shared->tick_num));

        /* At this point, we should have evicted or refreshed all stale
         * page buffer and metadata cache entries.
         *
         * Start the next tick.
         */
        shared->tick_num = tmp_tick_num;

        vfd_swmr_reader_did_increase_tick_to(tmp_tick_num);

        /* Update end_of_tick */
        if (H5F__vfd_swmr_update_end_of_tick_and_tick_num(shared, FALSE) < 0) {
            HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "unable to update end of tick");
        }
    }

    /* Remove the entry from the EOT queue */
    if (H5F_vfd_swmr_remove_entry_eot(f) < 0) {
        HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "unable to remove entry from EOT queue")
    }

    /* Re-insert the entry that corresponds to f onto the EOT queue */
    if (H5F_vfd_swmr_insert_entry_eot(f) < 0) {
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "unable to insert entry into the EOT queue")
    }

done:

    if (change != NULL)
        free(change);

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5F_vfd_swmr_reader_end_of_tick() */

static void
insert_eot_entry(eot_queue_entry_t *entry_ptr)
{
    eot_queue_entry_t *prec_ptr; /* The predecessor entry on the EOT end of tick queue */

    /* Find the insertion point for the entry on the EOT queue */
    TAILQ_FOREACH_REVERSE(prec_ptr, &eot_queue_g, eot_queue, link)
    {
        if (HDtimespeccmp(&prec_ptr->end_of_tick, &entry_ptr->end_of_tick, <=))
            break;
    }

    /* Insert the entry onto the EOT queue */
    if (prec_ptr != NULL)
        TAILQ_INSERT_AFTER(&eot_queue_g, prec_ptr, entry_ptr, link);
    else
        TAILQ_INSERT_HEAD(&eot_queue_g, entry_ptr, link);
}

/* Update an entry on the EOT queue and move it to its proper place.
 */
void
H5F_vfd_swmr_update_entry_eot(eot_queue_entry_t *entry)
{
    H5F_t *       f      = entry->vfd_swmr_file;
    H5F_shared_t *shared = f->shared;

    /* Free the entry on the EOT queue that corresponds to f */

    TAILQ_REMOVE(&eot_queue_g, entry, link);

    HDassert(entry->vfd_swmr_writer == shared->vfd_swmr_writer);
    entry->tick_num    = shared->tick_num;
    entry->end_of_tick = shared->end_of_tick;

    insert_eot_entry(entry);
}

/*-------------------------------------------------------------------------
 *
 * Function:    H5F__vfd_swmr_remove_entry_eot
 *
 * Purpose:     Remove an entry from the EOT queue
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  Vailin Choi -- 11/18/2019
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_vfd_swmr_remove_entry_eot(H5F_t *f)
{
    eot_queue_entry_t *curr;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Free the entry on the EOT queue that corresponds to f */

    TAILQ_FOREACH(curr, &eot_queue_g, link)
    {
        if (curr->vfd_swmr_file == f)
            break;
    }

    if (curr != NULL) {
        TAILQ_REMOVE(&eot_queue_g, curr, link);
        curr = H5FL_FREE(eot_queue_entry_t, curr);
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5F_vfd_swmr_remove_entry_eot() */

/*-------------------------------------------------------------------------
 *
 * Function:    H5F_vfd_swmr_insert_entry_eot
 *
 * Purpose:     Insert an entry onto the EOT queue
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  Vailin Choi -- 11/18/2019
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_vfd_swmr_insert_entry_eot(H5F_t *f)
{
    H5F_shared_t *     shared = f->shared;
    eot_queue_entry_t *entry_ptr;           /* An entry on the EOT end of tick queue */
    herr_t             ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Allocate an entry to be inserted onto the EOT queue */
    if (NULL == (entry_ptr = H5FL_CALLOC(eot_queue_entry_t)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, FAIL, "unable to allocate the end of tick queue entry")

    /* Initialize the entry */
    entry_ptr->vfd_swmr_writer = shared->vfd_swmr_writer;
    entry_ptr->tick_num        = shared->tick_num;
    entry_ptr->end_of_tick     = shared->end_of_tick;
    entry_ptr->vfd_swmr_file   = f;

    insert_eot_entry(entry_ptr);

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* H5F_vfd_swmr_insert_entry_eot() */

/*-------------------------------------------------------------------------
 *
 * Function:    H5F_dump_eot_queue()
 *
 * Purpose:     Dump the contents of the EOT queue
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  Vailin Choi -- 11/18/2019
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_dump_eot_queue(void)
{
    int                i;
    eot_queue_entry_t *curr;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    for (curr = TAILQ_FIRST(&eot_queue_g), i = 0; curr != NULL; curr = TAILQ_NEXT(curr, link), i++) {
        HDfprintf(stderr, "%d: %s tick_num %" PRIu64 ", end_of_tick %jd.%09ld, vfd_swmr_file %p\n", i,
                  curr->vfd_swmr_writer ? "writer" : "not writer", curr->tick_num, curr->end_of_tick.tv_sec,
                  curr->end_of_tick.tv_nsec, (void *)curr->vfd_swmr_file);
    }

    if (i == 0)
        HDfprintf(stderr, "EOT head is null\n");

    FUNC_LEAVE_NOAPI(SUCCEED)

} /* H5F_dump_eot_queue() */

/*
 * Beginning of static functions
 */

/*-------------------------------------------------------------------------
 *
 * Function:    H5F__vfd_swmr_update_end_of_tick_and_tick_num
 *
 * Purpose:     Update end_of_tick (shared->end_of_tick)
 *              Update tick_num (shared->tick_num)
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  Vailin Choi -- 11/??/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__vfd_swmr_update_end_of_tick_and_tick_num(H5F_shared_t *shared, hbool_t incr_tick_num)
{
    struct timespec curr;                /* Current time in struct timespec */
    struct timespec new_end_of_tick;     /* new end_of_tick in struct timespec */
    int64_t         curr_nsecs;          /* current time in nanoseconds */
    int64_t         tlen_nsecs;          /* tick_len in nanoseconds */
    int64_t         new_end_nsecs;       /* new end_of_tick in nanoseconds */
    herr_t          ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC

    /* Get current time in struct timespec */
#ifdef H5_HAVE_WIN32_API
    if (timespec_get(&curr, TIME_UTC) != TIME_UTC)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get time via timespec_get");
#else
    if (HDclock_gettime(CLOCK_MONOTONIC, &curr) < 0) {
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get time via clock_gettime");
    }
#endif

    /* Convert curr to nsecs */
    curr_nsecs = curr.tv_sec * nanosecs_per_second + curr.tv_nsec;

    /* Convert tick_len to nanosecs */
    tlen_nsecs = shared->vfd_swmr_config.tick_len * nanosecs_per_tenth_sec;

    /*
     *  Update shared->tick_num
     */
    if (incr_tick_num) {

        shared->tick_num++;

        if (H5PB_vfd_swmr__set_tick(shared) < 0)

            HGOTO_ERROR(H5E_FILE, H5E_SYSTEM, FAIL, "Can't update page buffer current tick")
    }

    /*
     * Update shared->end_of_tick
     */
    /* Calculate new end_of_tick */

    /* TODO: The modulo operation is very expensive on most machines --
     *       re-work this code so as to avoid it.
     *
     *                                    JRM -- 11/12/18
     */

    new_end_nsecs           = curr_nsecs + tlen_nsecs;
    new_end_of_tick.tv_nsec = (long)(new_end_nsecs % nanosecs_per_second);
    new_end_of_tick.tv_sec  = new_end_nsecs / nanosecs_per_second;

    shared->end_of_tick = new_end_of_tick;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5F__vfd_swmr_update_end_of_tick_and_tick_num() */

/*-------------------------------------------------------------------------
 *
 * Function:    H5F__vfd_swmr_construct_write_md_hdr
 *
 * Purpose:     Encode and write header to the metadata file.
 *
 *              This is used by the VFD SWMR writer:
 *
 *                  --when opening an existing HDF5 file
 *                  --when closing the HDF5 file
 *                  --after flushing an HDF5 file
 *                  --when updating the metadata file
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  Vailin Choi -- 11/??/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__vfd_swmr_construct_write_md_hdr(H5F_shared_t *shared, uint32_t num_entries, uint8_t *image)
{
    uint8_t *p = NULL;        /* Pointer to buffer */
    uint32_t metadata_chksum; /* Computed metadata checksum value */
    /* Size of header and index */
    const size_t hdr_size = H5FD_MD_HEADER_SIZE;
    ssize_t      nwritten;
    herr_t       ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC

    /*
     * Encode metadata file header
     */
    p = image;

    /* Encode magic for header */
    HDmemcpy(p, H5FD_MD_HEADER_MAGIC, (size_t)H5_SIZEOF_MAGIC);
    p += H5_SIZEOF_MAGIC;

    /* Encode page size, tick number, index offset, index length */
    UINT32ENCODE(p, shared->fs_page_size);
    UINT64ENCODE(p, shared->tick_num);
    UINT64ENCODE(p, shared->writer_index_offset);
    UINT64ENCODE(p, H5FD_MD_INDEX_SIZE(num_entries));

    /* Calculate checksum for header */
    metadata_chksum = H5_checksum_metadata(image, (size_t)(p - image), 0);

    /* Encode checksum for header */
    UINT32ENCODE(p, metadata_chksum);

    /* Sanity checks on header */
    HDassert(p - image == (ptrdiff_t)hdr_size);

    if (shared->vfd_swmr_config.maintain_metadata_file) {
        /* Set to beginning of the file */
        if (HDlseek(shared->vfd_swmr_md_fd, H5FD_MD_HEADER_OFF, SEEK_SET) < 0)

            HGOTO_ERROR(H5E_VFL, H5E_SEEKERROR, FAIL, "unable to seek in metadata file")

        nwritten = HDwrite(shared->vfd_swmr_md_fd, image, hdr_size);
        /* Write header to the metadata file */
        if (nwritten != (ssize_t)hdr_size) {
            HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, "error in writing header to metadata file")
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5F__vfd_swmr_construct_write_md_hdr() */

/*-------------------------------------------------------------------------

 * Function:    H5F__vfd_swmr_construct_write_md_idx
 *
 * Purpose:     Encode and write index to the metadata file.
 *
 *              This is used by the VFD SWMR writer:
 *
 *                  --when opening an existing HDF5 file
 *                  --when closing the HDF5 file
 *                  --after flushing an HDF5 file
 *                  --when updating the metadata file
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  Vailin Choi -- 11/??/18
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__vfd_swmr_construct_write_md_idx(H5F_shared_t *shared, uint32_t num_entries,
                                     struct H5FD_vfd_swmr_idx_entry_t index[], uint8_t *image)
{
    uint8_t *p = NULL;        /* Pointer to buffer */
    uint32_t metadata_chksum; /* Computed metadata checksum value */
    /* Size of index */
    const size_t idx_size = H5FD_MD_INDEX_SIZE(num_entries);
    ssize_t      nwritten;
    unsigned     i;                   /* Local index variable */
    herr_t       ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC

    HDassert(num_entries == 0 || index != NULL);

    /*
     * Encode metadata file index
     */
    p = image;

    /* Encode magic for index */
    HDmemcpy(p, H5FD_MD_INDEX_MAGIC, H5_SIZEOF_MAGIC);
    p += H5_SIZEOF_MAGIC;

    /* Encode tick number */
    UINT64ENCODE(p, shared->tick_num);

    /* Encode number of entries in index */
    UINT32ENCODE(p, num_entries);

    /* Encode the index entries */
    for (i = 0; i < num_entries; i++) {
        UINT32ENCODE(p, index[i].hdf5_page_offset);
        UINT32ENCODE(p, index[i].md_file_page_offset);
        UINT32ENCODE(p, index[i].length);
        UINT32ENCODE(p, index[i].checksum);
    }

    /* Calculate checksum for index */
    metadata_chksum = H5_checksum_metadata(image, (size_t)(p - image), 0);

    /* Encode checksum for index */
    UINT32ENCODE(p, metadata_chksum);

    /* Sanity checks on index */
    HDassert(p - image == (ptrdiff_t)idx_size);

    /* Verify the md file descriptor exists */
    HDassert(shared->vfd_swmr_md_fd >= 0);

    if (shared->vfd_swmr_config.maintain_metadata_file) {

        if (HDlseek(shared->vfd_swmr_md_fd, (HDoff_t)shared->writer_index_offset, SEEK_SET) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_SEEKERROR, FAIL, "unable to seek in metadata file")

        nwritten = HDwrite(shared->vfd_swmr_md_fd, image, idx_size);
        /* Write index to the metadata file */
        if (nwritten != (ssize_t)idx_size) {
            HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, "error in writing index to metadata file")
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5F__vfd_swmr_construct_write_idx() */

/*-------------------------------------------------------------------------
 * Function: H5F__idx_entry_cmp()
 *
 * Purpose:  Callback used by HDqsort to sort entries in the index
 *
 * Return:   0 if the entries are the same
 *           -1 if entry1's offset is less than that of entry2
 *           1 if entry1's offset is greater than that of entry2
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__idx_entry_cmp(const void *_entry1, const void *_entry2)
{
    const H5FD_vfd_swmr_idx_entry_t *entry1 = _entry1;
    const H5FD_vfd_swmr_idx_entry_t *entry2 = _entry2;

    int ret_value = 0; /* Return value */

    FUNC_ENTER_STATIC_NOERR

    /* Sanity checks */
    HDassert(entry1);
    HDassert(entry2);

    if (entry1->hdf5_page_offset < entry2->hdf5_page_offset)
        ret_value = -1;
    else if (entry1->hdf5_page_offset > entry2->hdf5_page_offset)
        ret_value = 1;

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5F__idx_entry_cmp() */

/*-------------------------------------------------------------------------
 *
 * Function: H5F__vfd_swmr_create_index
 *
 * Purpose:  Allocate and initialize the index for the VFD SWMR metadata
 *           file.
 *
 * Return:   SUCCEED/FAIL
 *
 * Programmer: John Mainzer 11/5/18
 *
 * Changes:  None.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__vfd_swmr_create_index(H5F_shared_t *shared)
{
    size_t                     bytes_available;
    size_t                     entries_in_index;
    H5FD_vfd_swmr_idx_entry_t *index;
    herr_t                     ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    HDassert(shared->vfd_swmr);
    HDassert(shared->mdf_idx == NULL);
    HDassert(shared->mdf_idx_len == 0);
    HDassert(shared->mdf_idx_entries_used == 0);

    bytes_available = (size_t)shared->fs_page_size * (size_t)(shared->vfd_swmr_config.md_pages_reserved - 1);

    HDassert(bytes_available > 0);

    entries_in_index = (bytes_available - H5FD_MD_INDEX_SIZE(0)) / H5FD_MD_INDEX_ENTRY_SIZE;

    HDassert(entries_in_index > 0);

    index = H5MM_calloc(entries_in_index * sizeof(index[0]));

    if (index == NULL) {
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for md index")
    }

    HDassert(entries_in_index <= UINT32_MAX);

    shared->mdf_idx              = index;
    shared->mdf_idx_len          = (uint32_t)entries_in_index;
    shared->mdf_idx_entries_used = 0;
done:
    FUNC_LEAVE_NOAPI(ret_value)
}

H5FD_vfd_swmr_idx_entry_t *
vfd_swmr_enlarge_shadow_index(H5F_t *f)
{
    H5F_shared_t *             shared    = f->shared;
    H5FD_vfd_swmr_idx_entry_t *ret_value = NULL;
    haddr_t                    idx_addr;
    hsize_t                    idx_size;
    H5FD_vfd_swmr_idx_entry_t *new_mdf_idx = NULL, *old_mdf_idx;
    uint32_t                   new_mdf_idx_len, old_mdf_idx_len;

    FUNC_ENTER_NOAPI(NULL)

    old_mdf_idx     = shared->mdf_idx;
    old_mdf_idx_len = shared->mdf_idx_len;

    /* New length is double previous or UINT32_MAX, whichever is smaller. */
    if (UINT32_MAX - old_mdf_idx_len >= old_mdf_idx_len)
        new_mdf_idx_len = old_mdf_idx_len * 2;
    else
        new_mdf_idx_len = UINT32_MAX;

    idx_size = H5FD_MD_INDEX_SIZE(new_mdf_idx_len);

    idx_addr = H5MV_alloc(f, idx_size);

    if (idx_addr == HADDR_UNDEF) {
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "shadow-file allocation failed for index")
    }

    new_mdf_idx = H5MM_calloc(new_mdf_idx_len * sizeof(new_mdf_idx[0]));

    if (new_mdf_idx == NULL) {
        (void)H5MV_free(f, idx_addr, idx_size);
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for md index")
    }

    /* Copy the old index in its entirety to the new, instead of copying
     * just the _entries_used, because the caller may have been in the
     * process of adding entries, and some callers may not update
     * _entries_used immediately.
     */
    H5MM_memcpy(new_mdf_idx, old_mdf_idx, sizeof(new_mdf_idx[0]) * old_mdf_idx_len);

    shared->writer_index_offset = idx_addr;
    ret_value = shared->mdf_idx = new_mdf_idx;
    shared->mdf_idx_len         = new_mdf_idx_len;

    H5MM_xfree(f->shared->old_mdf_idx);

    shared->old_mdf_idx        = old_mdf_idx;
    f->shared->old_mdf_idx_len = old_mdf_idx_len;

    /* Postpone reclamation of the old index until max_lag ticks from now.
     * It's only necessary to wait until after the new index is in place,
     * so it's possible that some disused shadow storage will build up
     * past what is strictly necessary, but it seems like a reasonable
     * trade-off for simplicity.
     */
    if (shadow_range_defer_free(shared, shared->writer_index_offset, H5FD_MD_INDEX_SIZE(old_mdf_idx_len)) ==
        -1) {
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "could not schedule index reclamation");
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 *
 * Function: H5F__vfd_swmr_writer__wait_a_tick
 *
 * Purpose:  Before a file that has been opened by a VFD SWMR writer,
 *           all pending delayed writes must be allowed drain.
 *
 *           This function facilitates this by sleeping for a tick, and
 *           then running the writer end of tick function.
 *
 *           It should only be called as part the flush or close operations.
 *
 * Return:   SUCCEED/FAIL
 *
 * Programmer: John Mainzer 11/23/18
 *
 * Changes:  None.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__vfd_swmr_writer__wait_a_tick(H5F_t *f)
{
    uint64_t      tick_in_nsec;
    H5F_shared_t *shared;
    herr_t        ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC

    HDassert(f);
    shared = f->shared;
    HDassert(shared->vfd_swmr);
    HDassert(shared->vfd_swmr_writer);

    tick_in_nsec = shared->vfd_swmr_config.tick_len * nanosecs_per_tenth_sec;

    H5_nanosleep(tick_in_nsec);

    if (H5F_vfd_swmr_writer_end_of_tick(f, FALSE) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_SYSTEM, FAIL, "H5F_vfd_swmr_writer_end_of_tick() failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5F__vfd_swmr_writer__wait_a_tick() */

herr_t
H5F_vfd_swmr_process_eot_queue(hbool_t entering_api)
{
    struct timespec    now;
    eot_queue_entry_t *first_head, *head;
    herr_t             ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    first_head = head = TAILQ_FIRST(&eot_queue_g);

    do {
        H5F_t *       f      = head->vfd_swmr_file;
        H5F_shared_t *shared = f->shared;

#ifdef H5_HAVE_WIN32_API
        if (timespec_get(&now, TIME_UTC) != TIME_UTC)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get time via timespec_get");
#else
        if (HDclock_gettime(CLOCK_MONOTONIC, &now) < 0) {
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get time via clock_gettime");
        }
#endif
        if (HDtimespeccmp(&now, &head->end_of_tick, <))
            break;
        /* If the H5F_shared_t is labeled with a later EOT time than
         * the queue entry is, then we have already performed the
         * H5F_shared_t's EOT processing.  That can happen if
         * multiple H5F_t share the H5F_shared_t.  Just update the
         * EOT queue entry and move to the next.
         */
        if (HDtimespeccmp(&head->end_of_tick, &shared->end_of_tick, <)) {
            H5F_vfd_swmr_update_entry_eot(head);
        }
        else if (shared->vfd_swmr_writer) {
            if (H5F_vfd_swmr_writer_end_of_tick(f, FALSE) < 0)
                HGOTO_ERROR(H5E_FUNC, H5E_CANTSET, FAIL, "end of tick error for VFD SWMR writer");
        }
        else if (H5F_vfd_swmr_reader_end_of_tick(f, entering_api) < 0) {
            HGOTO_ERROR(H5E_FUNC, H5E_CANTSET, FAIL, "end of tick error for VFD SWMR reader");
        }
    } while ((head = TAILQ_FIRST(&eot_queue_g)) != NULL && head != first_head);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 *
 * Function:    H5F_post_vfd_swmr_log_entry
 *
 * Purpose:     Write the log information to the log file.
 *
 * Parameters:
 *              H5F_t *f                IN: HDF5 file pointer
 *              int entry_type_code     IN: The entry type code to identify the
 *                                          log entry tag.
 *              char *log_info          IN: The information to be stored in the
 *                                          log file.
 * Return:   None
 *
 *-------------------------------------------------------------------------
 */

void
H5F_post_vfd_swmr_log_entry(H5F_t *f, int entry_type_code, char *log_info)
{
    double        temp_time;
    H5_timevals_t current_time;
    char *        gettime_error;

    /* Obtain the current time.
       If   failed, write an error message to the log file.
       else obtain the elapsed time in seconds since the log file
            was created and write the time to the log file. */
    if (H5_timer_get_times(f->shared->vfd_swmr_log_start_time, &current_time) < 0) {
        if (NULL != (gettime_error = HDmalloc(log_err_mesg_length * sizeof(char)))) {
            HDsprintf(gettime_error, "gettime_error");
            HDfprintf(f->shared->vfd_swmr_log_file_ptr, "%-26s:  %s\n", H5Fvfd_swmr_log_tags[entry_type_code],
                      gettime_error);
            HDfree(gettime_error);
        }
    }
    else {
        temp_time = current_time.elapsed;
        HDfprintf(f->shared->vfd_swmr_log_file_ptr, log_fmt_str, H5Fvfd_swmr_log_tags[entry_type_code],
                  temp_time, log_info);
    }
    return;
}

/*-------------------------------------------------------------------------
 *
 * Function:    H5F__vfd_swmr_construct_ud_hdr
 *
 * Purpose:     Encode updater header in the buffer updater->header_image_ptr
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  Vailin Choi -- 10/2021
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__vfd_swmr_construct_ud_hdr(H5F_vfd_swmr_updater_t *updater)
{
    uint8_t *p     = NULL; /* Pointer to buffer */
    uint8_t *image = (uint8_t *)updater->header_image_ptr;
    uint32_t metadata_chksum;     /* Computed metadata checksum value */
    herr_t   ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC_NOERR

    /*
     * Encode metadata file header
     */
    p = image;

    /* Encode magic for header */
    HDmemcpy(p, H5F_UD_HEADER_MAGIC, (size_t)H5_SIZEOF_MAGIC);
    p += H5_SIZEOF_MAGIC;

    /* Encode version number, flags, page size, sequence number, tick number, change list offset, change list
     * length */
    UINT16ENCODE(p, H5F_UD_VERSION);
    UINT16ENCODE(p, updater->flags);
    UINT32ENCODE(p, updater->page_size);
    UINT64ENCODE(p, updater->sequence_number);
    UINT64ENCODE(p, updater->tick_num);

    UINT64ENCODE(p, updater->change_list_offset);
    UINT64ENCODE(p, updater->change_list_len);

    /* Calculate checksum for header */
    metadata_chksum = H5_checksum_metadata(image, (size_t)(p - image), 0);

    /* Encode checksum for header */
    UINT32ENCODE(p, metadata_chksum);

    /* Sanity checks on header */
    HDassert(p - image == (ptrdiff_t)updater->header_image_len);

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5F__vfd_swmr_construct_ud_hdr() */

/*-------------------------------------------------------------------------
 *
 * Function:    H5F__vfd_swmr_construct_ud_cl
 *
 * Purpose:     Encode updater change list in the buffer
 *              updater->change_list_image_ptr
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  Vailin Choi -- 10/2021
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__vfd_swmr_construct_ud_cl(H5F_vfd_swmr_updater_t *updater)
{
    uint8_t *p     = NULL; /* Pointer to buffer */
    uint8_t *image = (uint8_t *)updater->change_list_image_ptr;
    uint32_t metadata_chksum;     /* Computed metadata checksum value */
    unsigned i;                   /* Local index variable */
    herr_t   ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC_NOERR

    /*
     * Encode ud cl
     */
    p = image;

    /* Encode magic for ud cl */
    HDmemcpy(p, H5F_UD_CL_MAGIC, H5_SIZEOF_MAGIC);
    p += H5_SIZEOF_MAGIC;

    /* Encode tick number */
    UINT64ENCODE(p, updater->tick_num);

    /* Encode Metadata File Header Updater File Page Offset*/
    UINT32ENCODE(p, updater->md_file_header_ud_file_page_offset);

    /* Encode Metadata File Header Length */
    UINT32ENCODE(p, updater->md_file_header_len);

    /* Calculate checksum on the image of the metadata file header */
    updater->md_file_header_image_chksum =
        H5_checksum_metadata(updater->md_file_header_image_ptr, (size_t)updater->md_file_header_len, 0);

    /* Encode Metadata File Header Checksum */
    UINT32ENCODE(p, updater->md_file_header_image_chksum);

    /* Encode Metadata File Index Updater File Page Offset*/
    UINT32ENCODE(p, updater->md_file_index_ud_file_page_offset);

    /* Encode Metadata File Index Metadata File Offset */
    UINT64ENCODE(p, updater->md_file_index_md_file_offset);

    /* Encode Metadata File Index Length */
    UINT32ENCODE(p, updater->md_file_index_len);

    /* Calculate checksum on the image of the metadata file index */
    updater->md_file_index_image_chksum =
        H5_checksum_metadata(updater->md_file_index_image_ptr, (size_t)updater->md_file_index_len, 0);

    /* Encode Metadata File Index Checksum */
    UINT32ENCODE(p, updater->md_file_index_image_chksum);

    UINT32ENCODE(p, updater->num_change_list_entries);

    /* Encode the ud cl entries */
    for (i = 0; i < updater->num_change_list_entries; i++) {
        UINT32ENCODE(p, updater->change_list[i].entry_image_ud_file_page_offset);
        UINT32ENCODE(p, updater->change_list[i].entry_image_md_file_page_offset);
        UINT32ENCODE(p, updater->change_list[i].entry_image_h5_file_page_offset);
        UINT32ENCODE(p, updater->change_list[i].entry_image_len);
        UINT32ENCODE(p, updater->change_list[i].entry_image_checksum);
    }

    /* Calculate checksum for ud cl */
    metadata_chksum = H5_checksum_metadata(image, (size_t)(p - image), 0);

    /* Encode checksum for index */
    UINT32ENCODE(p, metadata_chksum);

    /* Sanity checks on index */
    HDassert(p - image == (ptrdiff_t)updater->change_list_len);

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5F__vfd_swmr_construct_ud_cl() */

/*-------------------------------------------------------------------------
 *
 * Function: H5F_generate_updater_file()
 *
 * Purpose:  Generate updater file:
 *              --assemble and initialize data in the updater struct
 *              --determine num_change_list entries
 *              --allocate buffers
 *              --construct on disk image (serialize) of the updater header and change list
 *              --create updater file using a temporay file name:
 *                  --<shared->vfd_swmr_config.updater_file_path>.ud_tmp
 *              --allocate space and write the following to the updater file
 *                  --updater file header
 *                  --updater file change list
 *                  --metadata entries
 *                  --metadata file index
 *                  --metadata file header
 *              --close the updater file
 *              --rename the updater file with the correct name:
 *                  <shared->vfd_swmr_config.updater_file_path>.<shared->updater_seq_num>
 *
 *              --increment shared->updater_seq_num
 *              --free buffers
 *
 * Return:   SUCCEED/FAIL
 *
 * Programmer: Vailin Choi  8/24/2021
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__generate_updater_file(H5F_t *f, uint32_t num_entries, uint16_t flags, uint8_t *md_file_hdr_image_ptr,
                           size_t md_file_hdr_image_len, uint8_t *md_file_index_image_ptr,
                           uint64_t md_file_index_offset, size_t md_file_index_image_len)
{
    H5F_shared_t *         shared = f->shared; /* shared file pointer */
    H5F_vfd_swmr_updater_t updater;            /* Updater struct */
    uint32_t               next_page_offset;
    H5FD_t *               ud_file = NULL; /* Low-level file struct            */
    char                   namebuf[H5F__MAX_VFD_SWMR_FILE_NAME_LEN];
    char                   newname[H5F__MAX_VFD_SWMR_FILE_NAME_LEN];
    unsigned               i, j;
    hsize_t                alloc_size;
    herr_t                 ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Updater file header fields */
    updater.version               = H5F_UD_VERSION;
    updater.flags                 = flags;
    updater.page_size             = (uint32_t)shared->fs_page_size;
    updater.sequence_number       = shared->updater_seq_num;
    updater.tick_num              = shared->tick_num;
    updater.header_image_ptr      = NULL;
    updater.header_image_len      = H5F_UD_HEADER_SIZE;
    updater.change_list_image_ptr = NULL;
    updater.change_list_offset    = 0;
    updater.change_list_len       = 0;

    /* Updater file change list fields */

    /* md_file_header related fields */
    updater.md_file_header_ud_file_page_offset = 0;
    updater.md_file_header_image_ptr           = md_file_hdr_image_ptr; /* parameter */
    updater.md_file_header_len                 = md_file_hdr_image_len; /* parameter */

    /* md_file_index related fields */
    updater.md_file_index_ud_file_page_offset = 0;
    updater.md_file_index_image_ptr           = md_file_index_image_ptr; /* parameter */
    updater.md_file_index_md_file_offset      = md_file_index_offset;    /* parameter */
    updater.md_file_index_len                 = md_file_index_image_len; /* parameter */

    updater.num_change_list_entries = 0;
    updater.change_list             = NULL;

    /* Scan index to determine updater.num_change_list_entries */
    for (i = 0; i < num_entries; i++) {
        if (shared->mdf_idx[i].entry_ptr != NULL &&
            shared->mdf_idx[i].tick_of_last_change == shared->tick_num)
            updater.num_change_list_entries += 1;
    }

    if (flags == CREATE_METADATA_FILE_ONLY_FLAG)
        HDassert(updater.sequence_number == 0);
    /* For file creation, just generate a header with this flag set */
    else {
        /* Update 2 updater file header fields: change_list_len, change_list_offset */
        updater.change_list_len    = H5F_UD_CL_SIZE(updater.num_change_list_entries);
        updater.change_list_offset = updater.header_image_len;
    }

    /* Create the updater file with a temporary file name */
    HDsprintf(namebuf, "%s.ud_tmp", shared->vfd_swmr_config.updater_file_path);

    if ((ud_file = H5FD_open(namebuf, H5F_ACC_TRUNC | H5F_ACC_RDWR | H5F_ACC_CREAT, H5P_FILE_ACCESS_DEFAULT,
                             HADDR_UNDEF)) == NULL)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "fail to open updater file");

    if ((updater.header_image_ptr = HDmalloc(updater.header_image_len)) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for ud header")

    /* Serialize updater file hdr in updater.header_image_ptr */
    if (H5F__vfd_swmr_construct_ud_hdr(&updater) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "fail to create updater file header ");

    /* Allocate space in updater file for updater file header */
    if (H5FD__alloc_real(ud_file, H5FD_MEM_DEFAULT, updater.header_image_len, NULL, NULL) == HADDR_UNDEF)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to allocate file memory")

    /* Write updater file header */
    if (H5FD_write(ud_file, H5FD_MEM_DEFAULT, H5F_UD_HEADER_OFF, updater.header_image_len,
                   updater.header_image_ptr) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, "ud file write failed")

    if (flags != CREATE_METADATA_FILE_ONLY_FLAG) {

        next_page_offset =
            ((uint32_t)(updater.header_image_len + updater.change_list_len) / updater.page_size) + 1;

        if (updater.num_change_list_entries) {

            /* Allocate space for change list entries */
            if ((updater.change_list = HDmalloc(sizeof(H5F_vfd_swmr_updater_cl_entry_t) *
                                                updater.num_change_list_entries)) == NULL)
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for ud cl")

            /* Initialize change list entries */
            i = 0;
            for (j = 0; j < num_entries; j++) {

                if (shared->mdf_idx[j].entry_ptr != NULL &&
                    shared->mdf_idx[j].tick_of_last_change == shared->tick_num) {

                    updater.change_list[i].entry_image_ptr                 = shared->mdf_idx[j].entry_ptr;
                    updater.change_list[i].entry_image_ud_file_page_offset = 0;
                    updater.change_list[i].entry_image_md_file_page_offset =
                        (uint32_t)shared->mdf_idx[j].md_file_page_offset;
                    updater.change_list[i].entry_image_h5_file_page_offset =
                        (uint32_t)shared->mdf_idx[j].hdf5_page_offset;
                    updater.change_list[i].entry_image_len      = shared->mdf_idx[j].length;
                    updater.change_list[i].entry_image_checksum = shared->mdf_idx[j].checksum;

                    shared->mdf_idx[j].entry_ptr = NULL;
                    i++;
                }
            }

            /* Set up page aligned space for all metadata pages */
            for (i = 0; i < updater.num_change_list_entries; i++) {
                updater.change_list[i].entry_image_ud_file_page_offset = next_page_offset;
                next_page_offset +=
                    (((uint32_t)updater.change_list[i].entry_image_len / updater.page_size) + 1);
            }
        }

        /* Set up page aligned space for the metadata file index */
        updater.md_file_index_ud_file_page_offset = next_page_offset;

        /* Set up page aligned space for the metadata file header */
        next_page_offset += (((uint32_t)updater.md_file_index_len / updater.page_size) + 1);
        updater.md_file_header_ud_file_page_offset = next_page_offset;

        if ((updater.change_list_image_ptr = HDmalloc(updater.change_list_len)) == NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for ud cl ")

        /* Serialize updater file change list in updater.change_list_image_ptr */
        if (H5F__vfd_swmr_construct_ud_cl(&updater) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "fail to create updater file cl");

        /* Allocate space in updater file for updater file change list */
        if (H5FD__alloc_real(ud_file, H5FD_MEM_DEFAULT, updater.header_image_len + updater.change_list_len,
                             NULL, NULL) == HADDR_UNDEF)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to allocate file memory")

        /* Write updater file change list */
        if (H5FD_write(ud_file, H5FD_MEM_DEFAULT, updater.header_image_len, updater.change_list_len,
                       updater.change_list_image_ptr) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, "ud file write failed")

        /* Allocate and write metadata pages */
        for (i = 0; i < updater.num_change_list_entries; i++) {
            alloc_size = updater.change_list[i].entry_image_ud_file_page_offset * updater.page_size +
                         updater.change_list[i].entry_image_len;

            if (H5FD__alloc_real(ud_file, H5FD_MEM_DEFAULT, alloc_size, NULL, NULL) == HADDR_UNDEF)
                HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to allocate file memory")

            if (H5FD_write(ud_file, H5FD_MEM_DEFAULT,
                           updater.change_list[i].entry_image_ud_file_page_offset * updater.page_size,
                           updater.change_list[i].entry_image_len,
                           updater.change_list[i].entry_image_ptr) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, "ud file write failed")
        }

        /* Allocate and write metadata file index */
        alloc_size =
            updater.md_file_index_ud_file_page_offset * updater.page_size + updater.md_file_index_len;
        if (H5FD__alloc_real(ud_file, H5FD_MEM_DEFAULT, alloc_size, NULL, NULL) == HADDR_UNDEF)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to allocate file memory")

        if (H5FD_write(ud_file, H5FD_MEM_DEFAULT,
                       updater.md_file_index_ud_file_page_offset * updater.page_size,
                       updater.md_file_index_len, updater.md_file_index_image_ptr) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, "ud file write failed")

        /* Allocate and write metadata file header */
        alloc_size =
            updater.md_file_header_ud_file_page_offset * updater.page_size + updater.md_file_header_len;
        if (H5FD__alloc_real(ud_file, H5FD_MEM_DEFAULT, alloc_size, NULL, NULL) == HADDR_UNDEF)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to allocate file memory")

        if (H5FD_write(ud_file, H5FD_MEM_DEFAULT,
                       updater.md_file_header_ud_file_page_offset * updater.page_size,
                       updater.md_file_header_len, updater.md_file_header_image_ptr) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, "ud file write failed")
    }

    /* Close the updater file and rename the file */
    if (H5FD_close(ud_file) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "unable to close updater file")
    HDsprintf(newname, "%s.%lu", shared->vfd_swmr_config.updater_file_path, shared->updater_seq_num);
    HDrename(namebuf, newname);

    ++shared->updater_seq_num;

done:
    if (updater.header_image_ptr)
        HDfree(updater.header_image_ptr);
    if (updater.change_list_image_ptr)
        HDfree(updater.change_list_image_ptr);
    if (updater.change_list)
        HDfree(updater.change_list);

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5F__generate_updater_file() */
