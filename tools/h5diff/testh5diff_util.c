/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
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

#include "testh5diff.h"


/*-------------------------------------------------------------------------
 * Function: write_attr
 *
 * Purpose: utility function to write an attribute in LOC_ID
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 12, 2003
 *
 *-------------------------------------------------------------------------
 */


int write_attr(hid_t loc_id,
               int rank,
               hsize_t *dims,
               const char *attr_name,
               hid_t type_id,
               void *buf)
{
 hid_t   attr_id;
 hid_t   space_id;
 herr_t  status;

 /* Create a buf space  */
 space_id = H5Screate_simple(rank,dims,NULL);

 /* Create the attribute */
 attr_id = H5Acreate(loc_id,attr_name,type_id,space_id,H5P_DEFAULT);

 /* Write the buf */
 if ( buf )
  status = H5Awrite(attr_id,type_id,buf);

 /* Close */
 status = H5Aclose(attr_id);
 status = H5Sclose(space_id);
 return status;
}

/*-------------------------------------------------------------------------
 * Function: write_dset
 *
 * Purpose: utility function to create and write a dataset in LOC_ID
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 12, 2003
 *
 *-------------------------------------------------------------------------
 */


int write_dset( hid_t loc_id,
                int rank,
                hsize_t *dims,
                const char *dset_name,
                hid_t type_id,
                void *buf )
{
 hid_t   dset_id;
 hid_t   space_id;
 herr_t  status;

 /* Create a buf space  */
 space_id = H5Screate_simple(rank,dims,NULL);

 /* Create a dataset */
 dset_id = H5Dcreate(loc_id,dset_name,type_id,space_id,H5P_DEFAULT);

 /* Write the buf */
 if ( buf )
  status = H5Dwrite(dset_id,type_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf);

 /* Close */
 status = H5Dclose(dset_id);
 status = H5Sclose(space_id);

 return status;

}

