/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Test header for Packet Table C++ wrapper API */
/* These tests are not as thorough as the tests for the C API because
 * the C++ API is simply a wrapper.  We don't need to test the
 * actual Packet Table functionality, just that the C APIs are
 * invoked correctly.
 */

#ifndef PTABLETEST
#define PTABLETEST

#include "h5hltest.h"
#include "H5PacketTable.h"
#include "H5Cpp.h"

static hid_t fileID;

/* Test some basic functionality; adding and getting records */
int BasicTest(void);

/* Test a compound fixed-length datatype */
int TestCompoundDatatype(void);

/* Test the GetNext functions and their indexes */
int TestGetNext(void);

/* Make sure that setting compression through the C++ API works. */
int TestCompress(void);

/* Ensure that the functions return the correct errors in
 * response to invalid indexes */
int TestErrors(void);

/* Test getting multiple records at once using GetPacket */
int TestGetPacket(void);

/* Create two packet tables at once using compound datatypes.
   Test for unusual interactions between multiple packet tables. */
int SystemTest(void);

/* Create a packet table with compound type, which has a string type.  Verify
   that data was written and read correctly. */
int TestHDFFV_9758(void);

#endif /* PTABLETEST */
