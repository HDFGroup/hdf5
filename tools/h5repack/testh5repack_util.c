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

#include "hdf5.h"
#include "h5repack.h"
#include "testh5repack.h"

/*-------------------------------------------------------------------------
 * Function: make_dset
 *
 * Purpose: utility function to create and write a dataset in LOC_ID
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 12, 2003
 *
 *-------------------------------------------------------------------------
 */
int make_dset(hid_t loc_id,
              const char *name,
              hid_t sid,
              hid_t dcpl,
              void *buf)
{
 hid_t   dsid;

 /* create the dataset */
 if((dsid = H5Dcreate (loc_id,name,H5T_NATIVE_INT,sid,dcpl))<0)
  return -1;

 /* write */
 if(H5Dwrite(dsid,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf)<0)
  goto out;

 /* close */
 if(H5Dclose(dsid)<0)
  return -1;

 return 0;
 out:
 H5E_BEGIN_TRY {
  H5Dclose(dsid);
 } H5E_END_TRY;
 return -1;
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

 /* Create a buf space  */
 if ((space_id = H5Screate_simple(rank,dims,NULL))<0)
  return -1;

 /* Create a dataset */
 if ((dset_id = H5Dcreate(loc_id,dset_name,type_id,space_id,H5P_DEFAULT))<0)
  return -1;

 /* Write the buf */
 if ( buf )
  if (H5Dwrite(dset_id,type_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf)<0)
   return -1;

 /* Close */
 if (H5Dclose(dset_id)<0)
  return -1;
 if (H5Sclose(space_id)<0)
  return -1;

 return 0;

}


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

int make_attr(hid_t loc_id,
               int rank,
               hsize_t *dims,
               const char *attr_name,
               hid_t type_id,
               void *buf)
{
 hid_t   attr_id;
 hid_t   space_id;

 /* create a space  */
 if ((space_id = H5Screate_simple(rank,dims,NULL))<0)
  return -1;

 /* create the attribute */
 if ((attr_id = H5Acreate(loc_id,attr_name,type_id,space_id,H5P_DEFAULT))<0)
  goto out;

 /* write the buffer */
 if ( buf )
 {
  if (H5Awrite(attr_id,type_id,buf)<0)
   goto out;
 }

 /* close */
 H5Aclose(attr_id);
 H5Sclose(space_id);
 return 0;

out:
 H5E_BEGIN_TRY {
  H5Aclose(attr_id);
  H5Sclose(space_id);
 } H5E_END_TRY;
 return -1;
}

