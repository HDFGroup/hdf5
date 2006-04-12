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

#ifndef _H5PTpublic_H
#define _H5PTpublic_H


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

#ifdef __cplusplus
extern "C" {
#endif

/*-------------------------------------------------------------------------
 *
 * Create/Open/Close functions
 *
 *-------------------------------------------------------------------------
 */

H5_HLDLL hid_t H5PTcreate_fl ( hid_t loc_id,
                      const char *dset_name,
                      hid_t dtype_id,
                      hsize_t chunk_size );

H5_HLDLL hid_t H5PTcreate_vl ( hid_t loc_id,
                      const char *dset_name,
                      hsize_t chunk_size );

H5_HLDLL hid_t H5PTopen( hid_t loc_id,
                char *dset_name );

H5_HLDLL herr_t  H5PTclose( hid_t table_id );


/*-------------------------------------------------------------------------
 *
 * Write functions
 *
 *-------------------------------------------------------------------------
 */

H5_HLDLL herr_t  H5PTappend( hid_t table_id,
                   hsize_t nrecords,
                   const void * data );

/*-------------------------------------------------------------------------
 *
 * Read functions
 *
 *-------------------------------------------------------------------------
 */


H5_HLDLL herr_t  H5PTget_next( hid_t table_id,
                     hsize_t nrecords,
                     void * data );

H5_HLDLL herr_t  H5PTread_packets( hid_t table_id,
                         hsize_t start,
                         hsize_t nrecords,
                         void *data );

/*-------------------------------------------------------------------------
 *
 * Inquiry functions
 *
 *-------------------------------------------------------------------------
 */


H5_HLDLL herr_t  H5PTget_num_packets( hid_t table_id,
                            hsize_t *nrecords );

H5_HLDLL herr_t  H5PTis_valid( hid_t table_id );

H5_HLDLL herr_t  H5PTis_varlen( hid_t table_id );

/*-------------------------------------------------------------------------
 *
 * Packet Table "current index" functions
 *
 *-------------------------------------------------------------------------
 */

H5_HLDLL herr_t  H5PTcreate_index( hid_t table_id );

H5_HLDLL herr_t  H5PTset_index( hid_t table_id,
                             hsize_t pt_index );

/*-------------------------------------------------------------------------
 *
 * Memory Management functions
 *
 *-------------------------------------------------------------------------
 */

H5_HLDLL herr_t  H5PTfree_vlen_readbuff( hid_t table_id,
                               hsize_t bufflen,
                               void * buff );

#ifdef __cplusplus
}
#endif

#endif
