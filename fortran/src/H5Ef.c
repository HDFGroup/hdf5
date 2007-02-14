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

/* This files contains C stubs for H5E Fortran APIs */

#include "H5f90.h"


/*----------------------------------------------------------------------------
 * Name:        h5eclear_c
 * Purpose:     Call H5Eclear to clear the error stack for the current thread
 * Inputs:
 * Outputs:
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, March 29, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/

int_f
nh5eclear_c( )
{
  int ret_val = -1;
  herr_t status;

  /*
   * Call H5Eclear function.
   */
  status = H5Eclear();
  if(status < 0) return ret_val;
  ret_val = 0;
  return ret_val;
}

/*----------------------------------------------------------------------------
 * Name:        h5eprint_c1
 * Purpose:     Call H5Eprint to print the error stack in a default manner.
 * Inputs:      name - file name
 *              namelen - length of name
 * Outputs:
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, March 29, 2000
 * Modifications: Bug fix: Added call to close the file with the error messages
 *                EP 11/26/01
 *---------------------------------------------------------------------------*/
int_f
nh5eprint_c1(_fcd name, int_f* namelen)
{
  int ret_val = -1;
  herr_t status;
  FILE * file;
  char* c_name;
  int c_namelen;
  c_namelen = *namelen;
  c_name = (char*)HD5f2cstring(name, c_namelen);
  if(c_name == NULL) return ret_val;
  file = fopen(c_name, "a");
       if(!file) goto DONE;
  /*
   * Call H5Eprint function.
   */
  status = H5Eprint(file);
  if (status >=0 ) ret_val = 0;
  fclose(file);

DONE:
  HDfree(c_name);
  return ret_val;
}


/*----------------------------------------------------------------------------
 * Name:        h5eprint_c2
 * Purpose:     Call H5Eprint to print the error stack to stderr
 *              in a default manner.
 * Inputs:
 * Outputs:
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, March 29, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5eprint_c2()
{
  int ret_val = -1;
  herr_t status;

  /*
   * Call H5Eprint function.
   */
  status = H5Eprint(NULL);
  if(status >= 0) ret_val = 0;
  return ret_val;
}

/*----------------------------------------------------------------------------
 * Name:        h5eget_major_c
 * Purpose:     Call H5Eget_major to get a character string
 *              describing an error specified by a major error number.
 * Inputs:      error_no - Major error number
 * Outputs:     name - character string describing the error
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, March 29, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5eget_major_c(int_f* error_no, _fcd name)
{
  int ret_val = -1;
  const char* c_name;
  H5E_major_t c_error_no;
  c_error_no = (H5E_major_t)*error_no;

  /*
   * Call H5Eget_major function.
   */
  c_name = H5Eget_major(c_error_no);
  HD5packFstring((char*)c_name, _fcdtocp(name), strlen(c_name));

  if(!strcmp(c_name, "Invalid major error number")) return ret_val;
  ret_val = 0;
  return ret_val;
}

/*----------------------------------------------------------------------------
 * Name:        h5eget_minor_c
 * Purpose:     Call H5Eget_minor to get a character string
 *              describing an error specified by a minor error number.
 * Inputs:      error_no - Major error number
 * Outputs:     name - character string describing the error
 * Returns:     0 on success, -1 on failure
 * Programmer:  Xiangyang Su
 *              Wednesday, March 29, 2000
 * Modifications:
 *---------------------------------------------------------------------------*/
int_f
nh5eget_minor_c(int_f* error_no, _fcd name)
{
  int ret_val = -1;
  const char* c_name;
  H5E_minor_t c_error_no;
  c_error_no = (H5E_minor_t)*error_no;

  /*
   * Call H5Eget_minor function.
   */
  c_name = H5Eget_minor(c_error_no);
  HD5packFstring((char*)c_name, _fcdtocp(name), strlen(c_name));

  if(!strcmp(c_name, "Invalid minor error number")) return ret_val;
  ret_val = 0;
  return ret_val;
}

/*----------------------------------------------------------------------------
 * Name:        h5eset_auto_c
 * Purpose:     Call H5Eset_auto to turn automatic error printing on or off.
 * Inputs:      printflag - flag to turn automatic error printing on or off.
 * Outputs:
 * Returns:     0 on success, -1 on failure
 * Programmer:  Elena Pourmal
 *              Friday, November 17, 2000
 * Modifications:  major bug fix. Function never disabled printing.
 *---------------------------------------------------------------------------*/
int_f
nh5eset_auto_c(int_f* printflag)
{
  int ret_val = -1;
  herr_t status;

  if (*printflag == 1)
    status = H5Eset_auto((H5E_auto_t)H5Eprint, stderr);
  if (*printflag == 0)
    status = H5Eset_auto(NULL,NULL);
  if (status >= 0) ret_val = 0;
  return ret_val;
}
