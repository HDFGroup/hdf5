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

/*
 * This file contains public declarations for the H5F module.
 */
#ifndef _H5Fpublic_H
#define _H5Fpublic_H

/* Public header files needed by this file */
#include <H5public.h>
#include <H5Apublic.h>

/*
 * These are the bits that can be passed to the `flags' argument of
 * H5Fcreate() and H5Fopen(). Use the bit-wise OR operator (|) to combine
 * them as needed.
 */
#define H5F_ACC_RDONLY	0x0000	/*absence of write implies read only	*/
#define H5F_ACC_RDWR	0x0001	/*open file for reading and writing	*/
#define H5F_ACC_TRUNC	0x0002	/*overwrite existing files during create*/
#define H5F_ACC_EXCL	0x0004	/*create fails if file already exists	*/
#define H5F_ACC_DEBUG	0x0008  /*print debug info			*/


#ifdef LATER
/*
 * These are here temporarily for backward compatibility with version
 * 5.1.0.0a and should eventually be removed since they violate the naming
 * scheme.
 */
#define H5ACC_DEFAULT	H5F_ACC_RDONLY
#define H5ACC_WRITE	H5F_ACC_RDWR
#define H5ACC_OVERWRITE	H5F_ACC_TRUNC
#endif

/*
 * Low-level file drivers.  These values are returned by H5Pget_file_driver()
 * and are set by the various H5Pset_...() functions that set file driver
 * properties.
 */
typedef enum H5F_driver_t {
    H5F_LOW_ERROR	= -1,	/*error return value			*/
    H5F_LOW_STDIO	= 0,	/*use functions declared in stdio.h	*/
    H5F_LOW_SEC2	= 1, 	/*use functions declared in unistd.h	*/
    H5F_LOW_MPI		= 2,	/*use indep or collective MPI-IO	*/
    H5F_LOW_CORE	= 3,	/*use malloc() and free()		*/
    H5F_LOW_SPLIT	= 4,	/*separate meta data from raw data	*/
    H5F_LOW_FAMILY	= 5, 	/*split addr space over many files	*/
} H5F_driver_t;


/* Parallel styles passed to H5Pset_mpi() */
#ifdef HAVE_PARALLEL
#  define H5ACC_INDEPENDENT 0x0010	/*MPI independent access	*/
#  define H5ACC_COLLECTIVE  0x0011	/*MPI collective access		*/
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Functions in H5F.c */
hbool_t H5Fis_hdf5 (const char *filename);
hid_t H5Fcreate (const char *filename, uintn flags, hid_t create_template,
                 hid_t access_template);
hid_t H5Fopen (const char *filename, uintn flags, hid_t access_template);
herr_t H5Fclose (hid_t fid);
hid_t H5Fget_create_template (hid_t fid);
hid_t H5Fget_access_template (hid_t file_id);

#ifdef __cplusplus
}
#endif
#endif
