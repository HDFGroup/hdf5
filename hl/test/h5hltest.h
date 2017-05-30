/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Friday, April 28, 2006
 *
 * Purpose:     Test support stuff.
 */

#ifndef _H5HLTEST_H
#define _H5HLTEST_H

/* Get the HDF5 test header */
#include "h5test.h"

/* Include the High-Level private header */
#include "H5HLprivate2.h"

/* Macros used in HL tests */
#define TESTING2(WHAT)  {printf("%-70s", "Testing     " WHAT); fflush(stdout);}
#define TESTING3(WHAT)  {printf("%-70s", "" WHAT); fflush(stdout);}

/* Implements verbose 'assert' with 'goto error' exit  */
#define VERIFY(condition, string) do { if (!(condition)) FAIL_PUTS_ERROR(string) } while(0)

int test_packet_table_with_varlen(void);

#endif /* _H5HLTEST_H */

