/* Test header for Packet Table C++ wrapper API */
/* These tests are not as thorough as the tests for the C API because
 * the C++ API is simply a wrapper.  We don't need to test the
 * actual Packet Table functionality, just that the C APIs are
 * invoked correctly.
 */

#ifndef PTABLETEST
#define PTABLETEST

#include "H5PacketTable.h"
#include "h5test.h"

static hid_t fileID;

/* Test some basic functionality; adding and getting records */
int BasicTest(void);

/* Test a compound fixed-length datatype */
int TestCompoundDatatype(void);

/* Test the GetNext functions and their indexes */
int TestGetNext(void);

/* Ensure that the functions return the correct errors in
 * response to invalid indexes */
int TestErrors(void);

/* Test getting multiple records at once using GetPacket */
int TestGetPacket(void);

/* Create two packet tables at once using compound datatypes.
   Test for unusual interactions between multiple packet tables. */
int SystemTest(void);

/* Test the variable length dataset functionality */
int VariableLengthTest(void);

#endif /* PTABLETEST */
