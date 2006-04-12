/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef _H5LTprivate_H
#define _H5LTprivate_H

#include "H5private.h"
/* public hdf5 prototypes			*/
#include "H5Ipublic.h"		
#include "H5Tpublic.h"		
#include "H5Spublic.h"	
#include "H5Dpublic.h"
#include "H5Ppublic.h"		
#include "H5Gpublic.h"		
#include "H5Apublic.h"		
#include "H5Epublic.h"		
/* public LT prototypes			*/
#include "H5LTpublic.h"		

#define TESTING(WHAT)	{printf("%-70s", "Testing " WHAT); fflush(stdout);}
#define TESTING2(WHAT)  {printf("%-70s", "Testing     " WHAT); fflush(stdout);}
#define TESTING3(WHAT)  {printf("%-70s", "" WHAT); fflush(stdout);}
#define PASSED()	{puts(" PASSED");fflush(stdout);}
#define H5_FAILED()	{puts("*FAILED*");fflush(stdout);}
#define SKIPPED()	{puts(" -SKIP-");fflush(stdout);}
#define EXAMPLE(WHAT)	{printf("%-70s", "Example " WHAT); fflush(stdout);}

/*-------------------------------------------------------------------------
 * Private functions
 *-------------------------------------------------------------------------
 */

H5_HLDLL herr_t  H5LT_get_attribute_mem( hid_t obj_id,
                           const char *attr_name,
                           hid_t mem_type_id,
                           void *data );

H5_HLDLL herr_t  H5LT_get_attribute_disk( hid_t obj_id,
                           const char *attr_name,
                           void *data );

H5_HLDLL herr_t  H5LT_find_attribute( hid_t loc_id, const char *name );

H5_HLDLL herr_t  H5LT_set_attribute_numerical( hid_t loc_id,
                                     const char *obj_name,
                                     const char *attr_name,
                                     size_t size,
                                     hid_t type_id,
                                     const void *data );

H5_HLDLL herr_t  H5LT_set_attribute_string( hid_t dset_id,
                                 char *name,
                                 char *buf );

H5_HLDLL herr_t  H5LT_dtype_to_text(hid_t dtype, char **dt_str, H5LT_lang_t lang, 
                                    size_t *slen, hbool_t no_user_buf);


#endif
