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

#ifndef _H5DSprivate_H
#define _H5DSprivate_H

/* public hdf5 prototypes			*/
#include "H5Ipublic.h"		
#include "H5Tpublic.h"		
#include "H5Spublic.h"	
#include "H5Dpublic.h"
#include "H5Ppublic.h"		
#include "H5Gpublic.h"		
#include "H5Apublic.h"		
#include "H5Epublic.h"	
#include "H5Rpublic.h"		
	
/* public LT prototypes			*/
#include "H5LTpublic.h"		
#include "H5DSpublic.h"


#define DIMENSION_SCALE_CLASS "DIMENSION_SCALE"
#define DIMENSION_LIST        "DIMENSION_LIST"
#define REFERENCE_LIST        "REFERENCE_LIST"
#define DIMENSION_LABELS      "DIMENSION_LABELS"



/* attribute type of a DS dataset */
typedef struct ds_list_t {
 hobj_ref_t ref;     /* object reference  */
 int        dim_idx; /* dimension index of the dataset */
} ds_list_t;


/*-------------------------------------------------------------------------
 * private functions
 *-------------------------------------------------------------------------
 */


H5_HLDLL herr_t  H5DS_is_reserved( hid_t did);



#endif
