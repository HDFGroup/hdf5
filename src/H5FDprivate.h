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

/*
 * Programmer:  Robb Matzke
 *              Monday, July 26, 1999
 */
#ifndef H5FDprivate_H
#define H5FDprivate_H

/* Include package's public headers */
#include "H5FDpublic.h"
#include "H5FDdevelop.h"

/* Private headers needed by this file */
#include "H5Pprivate.h" /* Property lists            */

/*
 * The MPI drivers are needed because there are
 * places where we check for things that aren't handled by these drivers.
 */
#include "H5FDmpi.h" /* MPI-based file drivers        */

/**************************/
/* Library Private Macros */
/**************************/

/* Length of filename buffer */
#define H5FD_MAX_FILENAME_LEN 1024

/*
 * VFD SWMR
 */
/* Metadata file header */
#define H5FD_MD_HEADER_OFF   0      /* Header offset in the metadata file */
#define H5FD_MD_HEADER_MAGIC "VHDR" /* Header magic */
#define H5FD_SIZEOF_CHKSUM   4      /* Size of checksum */

/* Size of the header in the metadata file */
#define H5FD_MD_HEADER_SIZE                                                                                  \
    (H5_SIZEOF_MAGIC      /* Signature */                                                                    \
     + 4                  /* Page size */                                                                    \
     + 8                  /* Tick number */                                                                  \
     + 8                  /* Index offset */                                                                 \
     + 8                  /* Index length number */                                                          \
     + H5FD_SIZEOF_CHKSUM /* Metadata header checksum */                                                     \
    )

/* Size of an index entry in the metadata file */
#define H5FD_MD_INDEX_ENTRY_SIZE                                                                             \
    (4                    /* HDF5 file page offset */                                                        \
     + 4                  /* Metadata file page offset */                                                    \
     + 4                  /* Length */                                                                       \
     + H5FD_SIZEOF_CHKSUM /* Index entry checksum */                                                         \
    )

/* Metadata file index magic */
#define H5FD_MD_INDEX_MAGIC "VIDX" /* Index magic */

/* Size of the metadata file index */
#define H5FD_MD_INDEX_SIZE(N)         /* N is number of entries in index */                                  \
    (H5_SIZEOF_MAGIC                  /* Signature */                                                        \
     + 8                              /* Tick num */                                                         \
     + 4                              /* Number of entries */                                                \
     + (N * H5FD_MD_INDEX_ENTRY_SIZE) /* Index entries */                                                    \
     + H5FD_SIZEOF_CHKSUM             /* Metadata index checksum */                                          \
    )

/* Retries for metadata file */
#define H5FD_VFD_SWMR_MD_FILE_RETRY_MAX 50 /* Maximum retries when opening the MD file */
#define H5FD_VFD_SWMR_MD_LOAD_RETRY_MAX                                                                      \
    120 /* Maximum retries when trying to load the MD file header and index */
#define H5FD_VFD_SWMR_MD_INDEX_RETRY_MAX 5 /* Maximum retries when deserializing the MD file index */

/*  Internal representation of metadata file index entry */

/*----------------------------------------------------------------------------
 *
 *  struct H5FD_vfd_swmr_idx_entry_t
 *
 * Indicies into the VFD SWMR metadata file are maintained in arrays of
 * instances of H5FD_vfd_swmr_index_t.
 *
 * The fields of H5FD_vfd_swmr_idx_entry_t are discussed below.
 *
 * hdf5_page_offset: Unsigned 64-bit value containing the base address of the
 *              metadata page, or multi page metadata entry in the HDF5
 *              file IN PAGES.
 *
 *              To obtain byte offset, multiply this value by the page size.
 *
 * md_file_page_offset: Unsigned 64-bit value containing the base address of
 *              the metadata page, or multi page metadata entry in the metadata
 *              file IN PAGES.
 *
 *              To obtain byte offset, multiply this value by the page size.
 *
 * length:      The length of the metadata page or multi- page metadata entry
 *              in BYTES.
 *
 * chksum:      Checksum for the metadata page or multi-page metadata entry.
 *              For the VFD SWMR writer, this value is undefined until the
 *              referenced entry has been written to the metadata file.
 *
 * entry_ptr:   Used by the VFD SWMR writer only.
 *
 *              For the VFD SWMR reader, this field should always be NULL.
 *              If the referenced metadata page or multi-page metadata
 *              entry was modified in the current tick, this field points to
 *              a buffer in the page buffer containing its value.
 *              This field is used by the metadata file creation/update code
 *              to access the metadata pages or multi-page metadata entries
 *              so that their current values can be copied into the metadata
 *              file.  After this copy, this field should be set to NULL.
 *
 * tick_of_last_change: Number of the last tick in which this index entry
 *              was changed.
 *
 *              Used by the VFD SWMR writer only.
 *
 *              For the VFD SWMR reader, this field will always be set to 0.
 *
 * clean:       Used by the VFD SWMR writer only.
 *
 *              Set to TRUE whenever the referenced metadata page or
 *              multi-page metadata entry is written to the HDF5 file.
 *              Set to FALSE whenever it is marked dirty in the page buffer.
 *
 * tick_of_last_flush: Number of the tick in which this entry was last
 *              written to the lower file or zero if it has never been flushed.
 *
 *              Used by the VFD SWMR writer only.
 *
 *              For the VFD SWMR reader, this field should always be 0.
 *
 * delayed_flush: If the flush of the referenced metadata page or multi-page
 *              metadata entry must be delayed, the earliest tick in which
 *              it may be flushed, or zero if there is no such constraint.
 *
 *              Used by the VFD SWMR writer only.
 *
 * moved_to_lower_file: Set to TRUE iff the entry referenced is in the
 *              lower file and is therefore about to be removed from the
 *              metadata file
 *
 * garbage: `true` if the entry is marked for garbage collection and is
 *              thus invalid.
 *
 *              For n the number of entries, deleting an entry is O(n).
 *              H5PB_dest() deletes all entries.  Instead of deleting
 *              entries one-by-one at O(n^2) cost, H5PB_dest() marks
 *              each disused entry for garbage collection and sweeps all
 *              entries up before it is done.
 *
 *----------------------------------------------------------------------------
 */
typedef struct H5FD_vfd_swmr_idx_entry_t {
    uint64_t hdf5_page_offset;
    uint64_t md_file_page_offset;
    uint32_t length;
    uint32_t chksum;
    void *   entry_ptr;
    uint64_t tick_of_last_change;
    hbool_t  clean;
    uint64_t tick_of_last_flush;
    uint64_t delayed_flush;
    bool     moved_to_lower_file;
    bool     garbage;
} H5FD_vfd_swmr_idx_entry_t;

/*
 *  tick_num:       Sequence number of the current tick.
 *                  Initialized to zero on file creation/open, and incremented
 *                  by the VFD SWMR writer at the end of each tick.
 *  num_entries:    The number of entires in the index.
 *  entries:        The array of index entries
 */
typedef struct H5FD_vfd_swmr_md_index {
    uint64_t                   tick_num;
    uint32_t                   num_entries;
    H5FD_vfd_swmr_idx_entry_t *entries;
} H5FD_vfd_swmr_md_index;

/*
 *  fs_page_size:   Size of pages in both the HDF5 file and the metadata file IN BYTES
 *  tick_num:       Sequence number of the current tick.
 *                  Initialized to zero on file creation/open, and incremented by the
 *                  VFD SWMR writer at the end of each tick.
 *  index_offset:   The offset of the current metadata file index in the metadata file
 *                  IN BYTES.
 *  index_length:   The length of the current metadata file index IN BYTES.
 */
typedef struct H5FD_vfd_swmr_md_header {
    uint32_t fs_page_size;
    uint64_t tick_num;
    uint64_t index_offset;
    size_t   index_length;
} H5FD_vfd_swmr_md_header;

/* Lookup the shadow-index entry corresponding to page number `target_page`
 * in the HDF5 file and return it.  If there is no match, return NULL.
 *
 * The lookup is performed by binary search on the `nentries` shadow index
 * entries at `idx`.  The entries must be sorted by their offset in the
 * HDF5 file.  Each entry must have a unique HDF5 file offset.
 *
 * If `reuse_garbage` is true, then entries marked for garbage collection
 * are eligible search results.  Return NULL if a matching entry is
 * found, but the entry is marked for garbage collection and `reuse_garbage`
 * is false.
 */
static inline H5FD_vfd_swmr_idx_entry_t *
vfd_swmr_pageno_to_mdf_idx_entry(H5FD_vfd_swmr_idx_entry_t *idx, uint32_t nentries, uint64_t target_page,
                                 bool reuse_garbage)
{
    uint32_t top;
    uint32_t bottom;
    uint32_t probe;

    if (nentries < 1)
        return NULL;

    bottom = 0;
    top    = nentries;

    do {
        probe = (top + bottom) / 2;

        if (idx[probe].hdf5_page_offset < target_page)
            bottom = probe + 1;
        else if (idx[probe].hdf5_page_offset > target_page)
            top = probe;
        else /* found it */
            return (reuse_garbage || !idx[probe].garbage) ? &idx[probe] : NULL;
    } while (bottom < top);
    /* Previous interval was [top - 1, top] or [bottom, bottom + 1].
     * The new interval is [top, top] or [bottom, bottom], respectively.
     * We probed idx[bottom] in the last step, and idx[top] (if it is
     * not out of bounds) in an earlier round.  So there is nothing
     * to be found at (top + bottom) / 2.
     */
    return NULL;
}

#ifdef H5_HAVE_PARALLEL
/* ======== Temporary data transfer properties ======== */
/* Definitions for memory MPI type property */
#define H5FD_MPI_XFER_MEM_MPI_TYPE_NAME "H5FD_mpi_mem_mpi_type"
/* Definitions for file MPI type property */
#define H5FD_MPI_XFER_FILE_MPI_TYPE_NAME "H5FD_mpi_file_mpi_type"

/* Sub-class the H5FD_class_t to add more specific functions for MPI-based VFDs */
typedef struct H5FD_class_mpi_t {
    H5FD_class_t super;                       /* Superclass information & methods */
    int (*get_rank)(const H5FD_t *file);      /* Get the MPI rank of a process */
    int (*get_size)(const H5FD_t *file);      /* Get the MPI size of a communicator */
    MPI_Comm (*get_comm)(const H5FD_t *file); /* Get the communicator for a file */
} H5FD_class_mpi_t;
#endif

/****************************/
/* Library Private Typedefs */
/****************************/

/* File operations */
typedef enum {
    OP_UNKNOWN = 0, /* Unknown last file operation */
    OP_READ    = 1, /* Last file I/O operation was a read */
    OP_WRITE   = 2  /* Last file I/O operation was a write */
} H5FD_file_op_t;

/* Define structure to hold initial file image and other relevant information */
typedef struct {
    void *                      buffer;
    size_t                      size;
    H5FD_file_image_callbacks_t callbacks;
} H5FD_file_image_info_t;

/* Define default file image info */
#define H5FD_DEFAULT_FILE_IMAGE_INFO                                                                         \
    {                                                                                                        \
        NULL,         /* file image buffer */                                                                \
            0,        /* buffer size */                                                                      \
        {             /* Callbacks */                                                                        \
            NULL,     /* image_malloc */                                                                     \
                NULL, /* image_memcpy */                                                                     \
                NULL, /* image_realloc */                                                                    \
                NULL, /* image_free */                                                                       \
                NULL, /* udata_copy */                                                                       \
                NULL, /* udata_free */                                                                       \
                NULL, /* udata */                                                                            \
        }                                                                                                    \
    }

/* Define structure to hold driver ID & info for FAPLs */
typedef struct {
    hid_t       driver_id;   /* Driver's ID */
    const void *driver_info; /* Driver info, for open callbacks */
} H5FD_driver_prop_t;

/*****************************/
/* Library Private Variables */
/*****************************/

/******************************/
/* Library Private Prototypes */
/******************************/

/* Forward declarations for prototype arguments */
struct H5F_t;

H5_DLL int    H5FD_term_interface(void);
H5_DLL herr_t H5FD_locate_signature(H5FD_t *file, haddr_t *sig_addr);
H5_DLL H5FD_class_t *H5FD_get_class(hid_t id);
H5_DLL hsize_t       H5FD_sb_size(H5FD_t *file);
H5_DLL herr_t        H5FD_sb_encode(H5FD_t *file, char *name /*out*/, uint8_t *buf);
H5_DLL herr_t        H5FD_sb_load(H5FD_t *file, const char *name, const uint8_t *buf);
H5_DLL void *        H5FD_fapl_get(H5FD_t *file);
H5_DLL herr_t        H5FD_free_driver_info(hid_t driver_id, const void *driver_info);
H5_DLL hid_t         H5FD_register(const void *cls, size_t size, hbool_t app_ref);
H5_DLL H5FD_t *H5FD_open(const char *name, unsigned flags, hid_t fapl_id, haddr_t maxaddr);
H5FD_t *       H5FD_deduplicate(H5FD_t *, hid_t);
H5_DLL herr_t  H5FD_close(H5FD_t *file);
H5_DLL int     H5FD_cmp(const H5FD_t *f1, const H5FD_t *f2);
H5_DLL herr_t  H5FD_driver_query(const H5FD_class_t *driver, unsigned long *flags /*out*/);
H5_DLL haddr_t H5FD_alloc(H5FD_t *file, H5FD_mem_t type, struct H5F_t *f, hsize_t size, haddr_t *frag_addr,
                          hsize_t *frag_size);
H5_DLL herr_t  H5FD_free(H5FD_t *file, H5FD_mem_t type, struct H5F_t *f, haddr_t addr, hsize_t size);
H5_DLL htri_t  H5FD_try_extend(H5FD_t *file, H5FD_mem_t type, struct H5F_t *f, haddr_t blk_end,
                               hsize_t extra_requested);
H5_DLL haddr_t H5FD_get_eoa(const H5FD_t *file, H5FD_mem_t type);
H5_DLL herr_t  H5FD_set_eoa(H5FD_t *file, H5FD_mem_t type, haddr_t addr);
H5_DLL haddr_t H5FD_get_eof(const H5FD_t *file, H5FD_mem_t type);
H5_DLL haddr_t H5FD_get_maxaddr(const H5FD_t *file);
H5_DLL herr_t  H5FD_get_feature_flags(const H5FD_t *file, unsigned long *feature_flags);
H5_DLL herr_t  H5FD_set_feature_flags(H5FD_t *file, unsigned long feature_flags);
H5_DLL herr_t  H5FD_get_fs_type_map(const H5FD_t *file, H5FD_mem_t *type_map);
H5_DLL herr_t  H5FD_read(H5FD_t *file, H5FD_mem_t type, haddr_t addr, size_t size, void *buf /*out*/);
H5_DLL herr_t  H5FD_write(H5FD_t *file, H5FD_mem_t type, haddr_t addr, size_t size, const void *buf);
H5_DLL herr_t  H5FD_flush(H5FD_t *file, hbool_t closing);
H5_DLL herr_t  H5FD_truncate(H5FD_t *file, hbool_t closing);
H5_DLL herr_t  H5FD_lock(H5FD_t *file, hbool_t rw);
H5_DLL herr_t  H5FD_unlock(H5FD_t *file);
H5_DLL herr_t  H5FD_delete(const char *name, hid_t fapl_id);
H5_DLL herr_t  H5FD_get_fileno(const H5FD_t *file, unsigned long *filenum);
H5_DLL herr_t  H5FD_get_vfd_handle(H5FD_t *file, hid_t fapl, void **file_handle);
H5_DLL herr_t  H5FD_set_base_addr(H5FD_t *file, haddr_t base_addr);
H5_DLL haddr_t H5FD_get_base_addr(const H5FD_t *file);
H5_DLL herr_t  H5FD_set_paged_aggr(H5FD_t *file, hbool_t paged);
H5_DLL herr_t  H5FD_get_driver_name(const H5FD_t *file, char **driver_name);

/* Function prototypes for VFD SWMR */
H5_DLL int    shadow_image_defer_free(struct H5F_shared_t *, const H5FD_vfd_swmr_idx_entry_t *);
H5_DLL herr_t H5FD_vfd_swmr_get_tick_and_idx(H5FD_t *_file, hbool_t read_index, uint64_t *tick_ptr,
                                             uint32_t *num_entries_ptr, H5FD_vfd_swmr_idx_entry_t index[]);
H5_DLL H5FD_vfd_swmr_idx_entry_t *vfd_swmr_enlarge_shadow_index(struct H5F_t *);
H5_DLL void                       H5FD_vfd_swmr_dump_status(H5FD_t *, uint64_t);
H5_DLL void                       H5FD_vfd_swmr_set_pb_configured(H5FD_t *_file);
H5_DLL void                       H5FD_vfd_swmr_record_elapsed_ticks(H5FD_t *, uint64_t);
H5_DLL H5FD_t *                   H5FD_vfd_swmr_dedup(H5FD_t *_self, H5FD_t *_other, hid_t fapl_id);


/* Function prototypes for MPI based VFDs*/
#ifdef H5_HAVE_PARALLEL
/* General routines */
H5_DLL haddr_t H5FD_mpi_MPIOff_to_haddr(MPI_Offset mpi_off);
H5_DLL herr_t  H5FD_mpi_haddr_to_MPIOff(haddr_t addr, MPI_Offset *mpi_off /*out*/);
#ifdef NOT_YET
H5_DLL herr_t H5FD_mpio_wait_for_left_neighbor(H5FD_t *file);
H5_DLL herr_t H5FD_mpio_signal_right_neighbor(H5FD_t *file);
#endif /* NOT_YET */
H5_DLL herr_t H5FD_set_mpio_atomicity(H5FD_t *file, hbool_t flag);
H5_DLL herr_t H5FD_get_mpio_atomicity(H5FD_t *file, hbool_t *flag);

/* Driver specific methods */
H5_DLL int      H5FD_mpi_get_rank(const H5FD_t *file);
H5_DLL int      H5FD_mpi_get_size(const H5FD_t *file);
H5_DLL MPI_Comm H5FD_mpi_get_comm(const H5FD_t *_file);
#endif /* H5_HAVE_PARALLEL */

#endif /* H5FDprivate_H */
