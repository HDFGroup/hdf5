/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * This file contains public information about the H5AS analysis shipping 
 * module. It implements the functions specified in the "Data Analysis 
 * Extensions" section of the Fast Forward HDF design document.
 *
 * The functionality should be provided as part of the server-side VOL process, 
 * i.e. this file should
 */
#ifndef _H5ASpublic_H
#define _H5ASpublic_H

/* Public headers needed by this file */
#include "H5public.h"
#include "H5Ipublic.h"

/**************************/
/* Library Private Macros */
/**************************/

/***************************/
/* Library Public Typedefs */
/***************************/

/****************************/
/* Library Public Variables */
/****************************/

/*****************************/
/* Library Public Prototypes */
/*****************************/
#ifdef __cplusplus
extern "C" {
#endif

/* Function prototypes */
H5_DLL herr_t H5ASexecute(const char *file_name, const char *dataset_name,
        hid_t query_id, const char *split_script, const char *combine_script,
        hid_t estack_id);

#ifdef __cplusplus
}
#endif
#endif /* _H5ASpublic_H */
