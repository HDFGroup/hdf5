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
 * This file contains public declarations for the H5E module.
 */
#ifndef _H5Epublic_H
#define _H5Epublic_H

#include <stdio.h>              /*FILE arg of H5Eprint()                     */

/* Public headers needed by this file */
#include <H5public.h>
#include <H5Apublic.h>

/*
 * Declare an enumerated type which holds all the valid major HDF error codes.
 */
typedef enum H5E_major_t {
    H5E_NONE_MAJOR       = 0,   /*special zero, no error                     */
    H5E_ARGS,                   /*invalid arguments to routine               */
    H5E_RESOURCE,               /*resource unavailable                       */
    H5E_INTERNAL,               /* Internal error (too specific to document
                                 * in detail)
                                 */
    H5E_FILE,                   /*file Accessability                         */
    H5E_IO,                     /*Low-level I/O                              */
    H5E_FUNC,                   /*function Entry/Exit                        */
    H5E_ATOM,                   /*object Atom                                */
    H5E_CACHE,                  /*object Cache                               */
    H5E_BTREE,                  /*B-Tree Node                                */
    H5E_SYM,                    /*symbol Table                               */
    H5E_HEAP,                   /*Heap                                       */
    H5E_OHDR,                   /*object Header                              */
    H5E_DATATYPE,               /*Datatype                                   */
    H5E_DATASPACE,              /*Dataspace                                  */
    H5E_DATASET,                /*Dataset                                    */
    H5E_STORAGE,                /*data storage                               */
    H5E_TEMPLATE                /*Templates                                  */
} H5E_major_t;

/* Declare an enumerated type which holds all the valid minor HDF error codes */
typedef enum H5E_minor_t {
    H5E_NONE_MINOR       = 0,   /*special zero, no error                     */

    /* Argument errors */
    H5E_UNINITIALIZED,          /*information is unitialized                 */
    H5E_UNSUPPORTED,            /*feature is unsupported                     */
    H5E_BADTYPE,                /*incorrect type found                       */
    H5E_BADRANGE,               /*argument out of range                      */
    H5E_BADVALUE,               /*bad value for argument                     */

    /* Resource errors */
    H5E_NOSPACE,                /*no space available for allocation          */

    /* File accessability errors */
    H5E_FILEEXISTS,             /*file already exists                        */
    H5E_FILEOPEN,               /*file already open                          */
    H5E_CANTCREATE,             /*Can't create file                          */
    H5E_CANTOPENFILE,           /*Can't open file                            */
    H5E_NOTHDF5,                /*not an HDF5 format file                    */
    H5E_BADFILE,                /*bad file ID accessed                       */
    H5E_TRUNCATED,              /*file has been truncated                    */

    /* Generic low-level file I/O errors */
    H5E_SEEKERROR,              /*seek failed                                */
    H5E_READERROR,              /*read failed                                */
    H5E_WRITEERROR,             /*write failed                               */
    H5E_CLOSEERROR,             /*close failed                               */

    /* Function entry/exit interface errors */
    H5E_CANTINIT,               /*Can't initialize interface                 */
    H5E_ALREADYINIT,            /*object already initialized                 */

    /* Object atom related errors */
    H5E_BADATOM,                /*Can't find atom information                */
    H5E_CANTREGISTER,           /*Can't register new atom                    */

    /* Cache related errors */
    H5E_CANTFLUSH,              /*Can't flush object from cache              */
    H5E_CANTLOAD,               /*Can't load object into cache               */
    H5E_PROTECT,                /*protected object error                     */
    H5E_NOTCACHED,              /*object not currently cached                */

    /* B-tree related errors */
    H5E_NOTFOUND,               /*object not found                           */
    H5E_EXISTS,                 /*object already exists                      */
    H5E_CANTENCODE,             /*Can't encode value                         */
    H5E_CANTDECODE,             /*Can't decode value                         */
    H5E_CANTSPLIT,              /*Can't split node                           */
    H5E_CANTINSERT,             /*Can't insert object                        */
    H5E_CANTLIST,               /*Can't list node                            */

    /* Object header related errors */
    H5E_LINKCOUNT,              /*bad object header link count               */
    H5E_VERSION,                /*wrong version number                       */
    H5E_ALIGNMENT,              /*alignment error                            */
    H5E_BADMESG,                /*unrecognized message                       */

    /* Group related errors */
    H5E_CANTOPENOBJ,            /*Can't open object                          */
    H5E_COMPLEN,                /*name component is too long                 */
    H5E_CWG,                    /*problem with current working group         */
    H5E_LINK                    /*link count failure                         */
} H5E_minor_t;

#ifdef __cplusplus
extern "C" {
#endif

hid_t H5Ecreate (uintn initial_stack_nelmts);
herr_t H5Eclose (hid_t estack_id);
herr_t H5Epush (hid_t estack_id, H5E_major_t maj_num, H5E_minor_t min_num,
                const char *function_name, const char *file_name, intn line,
                const char *desc);
herr_t H5Eclear (hid_t estack_id);
herr_t H5Eprint (hid_t estack_id, FILE * file);

#ifdef __cplusplus
}
#endif
#endif
