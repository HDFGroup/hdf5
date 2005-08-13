/****************************************************************************
 * NCSA HDF                                                                 *
 * Scientific Data Technologies                                             *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/


#ifndef _H5PT_H
#define _H5PT_H

#include <hdf5.h>

#if 0
#define H5_PT_DEBUG
#endif

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
