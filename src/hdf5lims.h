/****************************************************************************
 * NCSA HDF                                                                 *
 * Software Development Group                                               *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/

/* $Id$ */

/*
 * This file contains all global hard coded limits for the library, interface
 * particular limits are defined in the interface header file.
 */

#ifndef HDF5LIMS_H
#define HDF5LIMS_H

/* Size of an OID in bytes */
#define HDF5_OID_SIZE   8

/* Version #'s of library code */
#define HDF5_MAJOR_VERSION      0   /* For major interface changes */
#define HDF5_MINOR_VERSION      0   /* For minor interface changes */
#define HDF5_RELEASE_VERSION    0   /* For interface tweaks & bug-fixes */
#define HDF5_PATCH_VERSION      0   /* For small groups of bug fixes */

/* Version #'s of the major components of the file format */
#define HDF5_BOOTBLOCK_VERSION  0   /* Version of the boot block format */
#define HDF5_SMALLOBJECT_VERSION 0  /* Version of the Small-Object Heap */
#define HDF5_FREESPACE_VERSION  0   /* Version of the Free-Space Info */
#define HDF5_OBJECTDIR_VERSION  0   /* Version of the Object Directory format */
#define HDF5_SHAREDHEADER_VERSION 0 /* Version of the Shared-Header Info */

/* Define the HDF5 file signature */
#define HDF5_FILE_SIGNATURE     "\211HDF\r\n\032\n"
#define HDF5_FILE_SIGNATURE_LEN 8

/* Maximum length of function name to push onto error stack */
#define MAX_FUNC_NAME_LEN   32

/* Default sizes of the hash-tables for various atom groups */
#define HDF5_ERRSTACK_HASHSIZE  64
#define HDF5_FILEID_HASHSIZE    64
#define HDF5_TEMPID_HASHSIZE    64
#define HDF5_DATATYPEID_HASHSIZE    64
#define HDF5_DATASPACEID_HASHSIZE   64

/* Default file-creation template values */
#define HDF5_USERBLOCK_DEFAULT  0       /* Default to 0-byte sized user blocks */
#define HDF5_OFFSETSIZE_DEFAULT 4       /* Default to 4-byte offsets */
#define HDF5_LENGTHSIZE_DEFAULT 4       /* Default to 4-byte lengths */
#define HDF5_BTREEPAGE_DEFAULT  1024    /* Default to 1024-byte B-tree pages */

#endif /* HDF5LIMS_H */

