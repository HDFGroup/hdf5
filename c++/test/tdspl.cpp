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

/*****************************************************************************
   FILE
   tdspl.cpp - HDF5 C++ testing the dataset memory and transfer property
		list functionality

 ***************************************************************************/
#ifdef OLD_HEADER_FILENAME
#include <iostream.h>
#else
#include <iostream>
#endif
using std::cerr;
using std::endl;

#include <string>
#include "H5Cpp.h"      // C++ API header file
using namespace H5;

#include "h5test.h"
#include "h5cpputil.h"  // C++ utilility header file

const H5std_string FILENAME("tdatatransform.h5");

static void test_transfplist()
{
    const char* c_to_f = "(9/5.0)*x + 32";
    const char* simple = "(4/2) * ( (2 + 4)/(5 - 2.5))"; /* this equals 4.8 */
    /* inverses the utrans transform in init_test to get back original array */
    const char* utrans_inv = "(x/3)*4 - 100";

    SUBTEST("DSetMemXferPropList::set/getDataTransform()");
    try {
	// Create various data set prop lists and set data transform expression.
	DSetMemXferPropList dxpl_c_to_f(c_to_f);

	DSetMemXferPropList dxpl_simple;
	dxpl_simple.setDataTransform(simple);

	DSetMemXferPropList dxpl_utrans_inv;
	dxpl_utrans_inv.setDataTransform(utrans_inv);

	//
	// Make a copy of one of those prop lists then read the data transform
	// expression and verify that it's the same as the original.
	//

	// Copy the prop list.
	DSetMemXferPropList dxpl_c_to_f_copy;
	dxpl_c_to_f_copy.copy(dxpl_c_to_f);

	// Find out the length of the transform expression, allocate the buffer
	// for it, then read and verify the expression from the copied plist
	ssize_t tran_len = dxpl_c_to_f_copy.getDataTransform(NULL);
	char *c_to_f_read = (char *)HDmalloc(tran_len+1);
	HDmemset(c_to_f_read, 0, tran_len+1);
	dxpl_c_to_f_copy.getDataTransform(c_to_f_read, tran_len+1);
	verify_val((const char*)c_to_f_read, (const char*)c_to_f,
		"DSetMemXferPropList::getDataTransform", __LINE__, __FILE__);
	HDfree(c_to_f_read);

	//
	// Read the expression of each of the prop lists and verify the read
	// expression
	//

	// Get and verify the expression with:
	// ssize_t getDataTransform(char* exp, const size_t buf_size [default=0])
	tran_len =  dxpl_c_to_f.getDataTransform(NULL);
	c_to_f_read = (char *)HDmalloc(tran_len+1);
	HDmemset(c_to_f_read, 0, tran_len+1);
	dxpl_c_to_f.getDataTransform(c_to_f_read, tran_len+1);
	verify_val((const char*)c_to_f_read, (const char*)c_to_f,
		"DSetMemXferPropList::getDataTransform", __LINE__, __FILE__);
	HDfree(c_to_f_read);

	// Get and verify the expression with:
	// H5std_string DSetMemXferPropList::getDataTransform()
	H5std_string simple_read = dxpl_simple.getDataTransform();
	verify_val((const char*)simple_read.c_str(), (const char*)simple,
		"DSetMemXferPropList::getDataTransform", __LINE__, __FILE__);

	// Get and verify the expression with:
	// ssize_t getDataTransform(char* exp, const size_t buf_size)
	tran_len = dxpl_utrans_inv.getDataTransform(NULL, 0);
	char *utrans_inv_read = (char *)HDmalloc(tran_len+1);
	HDmemset(utrans_inv_read, 0, tran_len+1);
	dxpl_utrans_inv.getDataTransform(utrans_inv_read, tran_len+1);
	verify_val((const char*)utrans_inv_read, (const char*)utrans_inv,
		"DSetMemXferPropList::getDataTransform", __LINE__, __FILE__);
	HDfree(utrans_inv_read);

	PASSED();
    }
    catch (Exception& E)
    {
	issue_fail_msg("test_transfplist", __LINE__, __FILE__, E.getCDetailMsg());
    }
}


/****************************************************************
**
**  test_dsproplist(): Main dataset property list testing routine.
**
****************************************************************/
extern "C"
void test_dsproplist()
{
    // Output message about test being performed
    MESSAGE(5, ("Testing Generic Dataset Property Lists\n"));

    test_transfplist(); // test set/getDataTransform()

}   // test_dsproplist()

extern "C"
void cleanup_dsproplist()
{
    HDremove(FILENAME.c_str());
}
