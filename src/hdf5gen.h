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
 * This file contains general macros used throughout HDF5 library & interfaces
 */

#ifndef HDF5GEN_H
#define HDF5GEN_H

/* return code - since some unix/c routines use 0 and -1 as their return
   code, and some assumption had been made in the code about that, it is
   important to keep these constants the same values.  For explicitly
   boolean functions, use TRUE and FALSE */

#define SUCCEED 0
#define FAIL (-1)

/* boolean values,  reminder: NEVER compare with numeric values */

#ifndef FALSE
#   define FALSE 0
#endif
#ifndef TRUE
#   define TRUE (!FALSE)
#endif

/* Function entry & exit macros */
#define FUNC_ENTER(pablo_mask,pablo_func_id,interface_init_func,err) \
    { PABLO_TRACE_ON(pablo_mask,pablo_func_id); \
        if(library_initialize==FALSE) if(H5_init_library()==FAIL) HGOTO_ERROR(H5E_FUNC,H5E_CANTINIT,err); \
        if(thread_initialize==FALSE) if(H5_init_thread()==FAIL) HGOTO_ERROR(H5E_FUNC,H5E_CANTINIT,err); \
        if(interface_initialize==FALSE) if(interface_init_func()==FAIL) HGOTO_ERROR(H5E_FUNC,H5E_CANTINIT,err); }
#define FUNC_LEAVE(pablo_mask,pablo_func_id,return_value) \
    { PABLO_TRACE_OFF(pablo_mask,pablo_func_id); return(return_value); }

#endif /* HDF5GEN_H */

