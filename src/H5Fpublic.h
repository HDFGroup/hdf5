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
#include <H5Ipublic.h>

/*
 * These are the bits that can be passed to the `flags' argument of
 * H5Fcreate() and H5Fopen(). Use the bit-wise OR operator (|) to combine
 * them as needed.  As a side effect, they call H5vers_check() to make sure
 * that the application is compiled with a version of the hdf5 header files
 * which are compatible with the library to which the application is linked.
 * We're assuming that these constants are used rather early in the hdf5
 * session.
 */
#define H5F_ACC_RDONLY	(H5check(),0x0000u)	/*absence of rdwr => rd-only */
#define H5F_ACC_RDWR	(H5check(),0x0001u)	/*open for read and write    */
#define H5F_ACC_TRUNC	(H5check(),0x0002u)	/*overwrite existing files   */
#define H5F_ACC_EXCL	(H5check(),0x0004u)	/*fail if file already exists*/
#define H5F_ACC_DEBUG	(H5check(),0x0008u)	/*print debug info	     */

#ifdef HAVE_PARALLEL
/* Use this constant string as the MPI_Info key to set H5Fmpio debug flags.
 * To turn on H5Fmpio debug flags,
 * set the MPI_Info value with this key to have the value of a string
 * consisting of the characters that turn on the desired flags. */
#define H5F_MPIO_DEBUG_KEY "H5F_mpio_debug_key"
#endif

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
    H5F_LOW_MPIO	= 2,	/*use indep or collective MPI-IO	*/
    H5F_LOW_CORE	= 3,	/*use malloc() and free()		*/
    H5F_LOW_SPLIT	= 4,	/*separate meta data from raw data	*/
    H5F_LOW_FAMILY	= 5 	/*split addr space over many files	*/
} H5F_driver_t;

/* Unlimited file size for H5Pset_external() */
#define H5F_UNLIMITED	((hsize_t)(-1L))

#ifdef __cplusplus
extern "C" {
#endif

/* Functions in H5F.c */
hbool_t H5Fis_hdf5 (const char *filename);
hid_t H5Fcreate (const char *filename, unsigned flags, hid_t create_plist,
                 hid_t access_plist);
hid_t H5Fopen (const char *filename, unsigned flags, hid_t access_plist);
herr_t H5Fclose (hid_t file_id);
hid_t H5Fget_create_template (hid_t file_id);
hid_t H5Fget_access_template (hid_t file_id);

#ifdef __cplusplus
}
#endif
#endif
