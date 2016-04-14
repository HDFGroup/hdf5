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

/*
 * Programmer:  James Laird <jlaird@hdfgroup.org>
 *              Tuesday, June 6, 2006
 *
 *  This program creates HDF5 files with user-defined links.  These files
 *  should be created on a little-endian and a big-endian machine.
 *  They will be named according to the platform and should
 *  be placed in the hdf5/test directory so that the links test can use them.
 *
 */

#include "hdf5.h"
#include <string.h>

#define NAME_LE_1 "le_extlink1.h5"
#define NAME_LE_2 "le_extlink2.h5"
#define NAME_BE_1 "be_extlink1.h5"
#define NAME_BE_2 "be_extlink2.h5"
#define NAME_BUF_SIZE 25

int
main (void)
{
  hid_t fid1=-1;
  hid_t fid2=-1;
  hid_t gid=-1;
  char filename1[NAME_BUF_SIZE];
  char filename2[NAME_BUF_SIZE];

  /* Name the files differently depending on the endianness of this platform */

  switch(H5Tget_order(H5T_NATIVE_INT))
  {
    case H5T_ORDER_LE:
      strcpy(filename1, NAME_LE_1);
      strcpy(filename2, NAME_LE_2);
      break;
    case H5T_ORDER_BE:
      strcpy(filename1, NAME_BE_1);
      strcpy(filename2, NAME_BE_2);
      break;
    default:
      goto error;
  }

  /* Create the two files */
  if((fid1 = H5Fcreate(filename1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;
  if((fid2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;

  /* Create two groups in the second file */
  if((gid = H5Gcreate2(fid2, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;
  if((H5Gclose(gid)) < 0) goto error;
  if((gid = H5Gcreate2(fid2, "group/subgroup", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;
  if((H5Gclose(gid)) < 0) goto error;

  /* Create an external link in the first file pointing to the group in the second file */
  if(H5Lcreate_external(filename2, "group", fid1, "ext_link", H5P_DEFAULT, H5P_DEFAULT) < 0) goto error;

  if((H5Fclose(fid1)) < 0) goto error;
  if((H5Fclose(fid2)) < 0) goto error;

  return 0;

error:
  H5E_BEGIN_TRY {
    H5Fclose(fid1);
    H5Fclose(fid2);
    H5Gclose(gid);
  } H5E_END_TRY
  return 1;
}
