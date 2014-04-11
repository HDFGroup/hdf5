/****h* H5Ef/H5Ef
 * PURPOSE
 *   This file contains C stubs for H5E Fortran APIs
 *
 * COPYRIGHT
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 ******
*/
#include "H5f90.h"
#include "H5Eprivate.h"

/****if* H5Ef/h5eclear_c
 * NAME
 *  h5eclear_c
 * PURPOSE
 *  Call H5Eclear to clear the error stack for the current thread
 * INPUTS
 *
 * OUTPUTS
 *
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Xiangyang Su
 *  Wednesday, March 29, 2000
 * HISTORY
 *
 * SOURCE
*/
int_f
nh5eclear_c(hid_t_f *estack_id )
/******/
{
  int_f ret_value = 0;

  /*
   * Call H5Eclear function.
   */
  if(H5Eclear2((hid_t)*estack_id) < 0)
      HGOTO_DONE(FAIL)

done:
    return ret_value;
}

/****if* H5Ef/h5eprint_c1
 * NAME
 *  h5eprint_c1
 * PURPOSE
 *  Call H5Eprint to print the error stack in a default manner.
 * INPUTS
 *  name    - file name
 *  namelen - length of name
 * OUTPUTS
 *
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Xiangyang Su
 *  Wednesday, March 29, 2000
 * HISTORY
 *  Bug fix: Added call to close the file with the error messages
 *  EP 11/26/01
 * SOURCE
*/
int_f
nh5eprint_c1(_fcd name, int_f* namelen)
/******/
{
    FILE *file = NULL;
    char *c_name = NULL;
    int_f ret_value = 0;

    if(NULL == (c_name = (char*)HD5f2cstring(name, (size_t)*namelen)))
        HGOTO_DONE(FAIL)
    if(NULL == (file = HDfopen(c_name, "a")))
        HGOTO_DONE(FAIL)

    /*
     * Call H5Eprint2 function.
     */
    if(H5Eprint2(H5E_DEFAULT, file) < 0)
        HGOTO_DONE(FAIL)

done:
    if(file)
        HDfclose(file);
    if(c_name)
        HDfree(c_name);

    return ret_value;
}

/****if* H5Ef/h5eprint_c2
 * NAME
 *  h5eprint_c2
 * PURPOSE
 *  Call H5Eprint to print the error stack to stderr
 *  in a default manner.
 * INPUTS
 *
 * OUTPUTS
 *
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Xiangyang Su
 *  Wednesday, March 29, 2000
 *
 * SOURCE
*/
int_f
nh5eprint_c2(void)
/******/
{
    int_f ret_value = 0;

    /*
     * Call H5Eprint2 function.
     */
    if(H5Eprint2(H5E_DEFAULT, NULL) < 0)
        HGOTO_DONE(FAIL)

done:
    return ret_value;
}

/****if* H5Ef/h5eget_major_c
 * NAME
 *  h5eget_major_c
 * PURPOSE
 *  Get a character string describing an error specified by a
 *  major error number.
 * INPUTS
 *  error_no - Major error number
 * OUTPUTS
 *  name - character string describing the error
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Xiangyang Su
 *  Wednesday, March 29, 2000
 * HISTORY
 *
 * SOURCE
*/
int_f
nh5eget_major_c(int_f* error_no, _fcd name, size_t_f* namelen)
/******/
{
    char *c_name = NULL;
    size_t c_namelen = (size_t)*namelen;
    int_f ret_value = 0;

    if(c_namelen > 0) 
        c_name = (char *)HDmalloc(c_namelen + 1);

    if(!c_name)
        HGOTO_DONE(FAIL)

    /*
     * Call H5Eget_msg function.
     */
    H5Eget_msg((hid_t)*error_no, NULL, c_name, c_namelen);
    HD5packFstring((char*)c_name, _fcdtocp(name), c_namelen);
    if(!HDstrcmp(c_name, "Invalid major error number"))
        HGOTO_DONE(FAIL)

done:
    if(c_name)
        HDfree(c_name);

    return ret_value;
}

/****if* H5Ef/h5eget_minor_c
 * NAME
 *  h5eget_minor_c
 * PURPOSE
 *  Get a character string describing an error specified by a
 *  minor error number.
 * INPUTS
 *  error_no - Major error number
 * OUTPUTS
 *  name - character string describing the error
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Xiangyang Su
 *  Wednesday, March 29, 2000
 * HISTORY
 *
 * SOURCE
*/
int_f
nh5eget_minor_c(int_f* error_no, _fcd name, size_t_f* namelen)
/******/
{
    char *c_name = NULL;
    size_t c_namelen = (size_t)*namelen;
    int_f ret_value = 0;

    if(c_namelen > 0) 
        c_name = (char *)HDmalloc(c_namelen + 1);

    if(!c_name)
        HGOTO_DONE(FAIL)

    /*
     * Call H5Eget_msg function.
     */
    H5Eget_msg((hid_t)*error_no, NULL, c_name, c_namelen);
    HD5packFstring((char *)c_name, _fcdtocp(name), c_namelen);
    if(!HDstrcmp(c_name, "Invalid minor error number"))
        HGOTO_DONE(FAIL)

done:
    if(c_name)
        HDfree(c_name);

    return ret_value;
}

/****if* H5Ef/h5eset_auto_c
 * NAME
 *  h5eset_auto_c
 * PURPOSE
 *  Call H5Eset_auto to turn automatic error printing on or off.
 * INPUTS
 *  printflag - flag to turn automatic error printing on or off.
 * OUTPUTS
 *
 * RETURNS
 *  0 on success, -1 on failure
 * AUTHOR
 *  Elena Pourmal
 *  Friday, November 17, 2000
 * HISTORY
 *  Major bug fix: Function never disabled printing.
 * SOURCE
*/
int_f
nh5eset_auto_c(int_f* printflag)
/******/
{
    herr_t status = -1;
    int_f ret_value = 0;

    if(*printflag == 1)
        status = H5Eset_auto2(H5E_DEFAULT, (H5E_auto2_t)H5Eprint2, stderr);
    else if(*printflag == 0)
        status = H5Eset_auto2(H5E_DEFAULT, NULL, NULL);
    if(status < 0)
        HGOTO_DONE(FAIL)

done:
    return ret_value;
}


/****if* H5Ef/h5eset_auto2_c
 * NAME
 *   h5eset_auto2_c
 * PURPOSE
 *   Calls H5Eset_auto2
 * INPUTS
 *   estack_id    - Error stack identifier.
 *   func 	 - Function to be called upon an error condition.
 *   client_data - Data passed to the error function.
 *   
 * RETURNS
 *   0 on success, -1 on failure
 * AUTHOR
 *   M. Scot Breitenfeld
 *   July 22, 2009
 * SOURCE
*/

int_f
nh5eset_auto2_c(int_f *printflag, hid_t_f *estack_id, H5E_auto2_t func, void *client_data)
/******/
{
  int ret_val = -1;
  herr_t status = -1;

  if (*printflag == 1 && *estack_id == -1)
    status = H5Eset_auto2(H5E_DEFAULT, (H5E_auto2_t)H5Eprint2, stderr);
  else if (*printflag == 1)
    status = H5Eset_auto2((hid_t)*estack_id, func, client_data);
  else if (*printflag == 0)
    status = H5Eset_auto2(H5E_DEFAULT, NULL, NULL);
  if (status >= 0) ret_val = 0;

  return ret_val;
}
