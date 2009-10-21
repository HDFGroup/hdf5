/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
   tlinks.cpp - HDF5 C++ testing functionalities associated with the
        C link interface

 ***************************************************************************/

#ifdef OLD_HEADER_FILENAME
#include <iostream.h>
#else
#include <iostream>
#endif
#include <string>

#ifndef H5_NO_NAMESPACE
#ifndef H5_NO_STD
    using std::cerr;
    using std::endl;
#endif  // H5_NO_STD
#endif

#include "testhdf5.h"   // C test header file
#include "H5Cpp.h"      // C++ API header file

#ifndef H5_NO_NAMESPACE
    using namespace H5;
#endif

#include "h5cpputil.h"  // C++ test utilility header file

static const char *FILENAME[] = {
    "link0",
    "link1",
    "link2",
    NULL
};


/*-------------------------------------------------------------------------
 * Function:	test_basic_links
 *
 * Purpose:	Test building a file with assorted links.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Binh-Minh Ribler
 *		October 16, 2009
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void test_basic_links(hid_t fapl_id)
{
    hsize_t	        size[1] = {1};
    char		filename[1024];
    char* tconv_buf = new char [1000];

    // Output message about test being performed
    SUBTEST("Testing Basic Links");

    // Use the file access template id to create a file access prop. list.
    FileAccPropList fapl(fapl_id);

    try
    {
	h5_fixname(FILENAME[0], fapl_id, filename, sizeof filename);
	H5File file(filename, H5F_ACC_TRUNC, FileCreatPropList::DEFAULT, fapl);

	// Create simple dataspace
	DataSpace scalar (1, size, size);

	// Create a group then close it by letting the object go out of scope
	{
	    Group group(file.createGroup("grp1", 0));
	}

	// Create a dataset then close it by letting the object go out of scope
	{
	    DataSet dset1(file.createDataSet("dset1", PredType::NATIVE_INT, scalar));
	}

	hid_t file_id = file.getId();

	// Create a hard link
	file.link(H5G_LINK_HARD, "dset1", "grp1/hard");

	// Create a symbolic link
	file.link(H5G_LINK_SOFT, "/dset1", "grp1/soft");

	// Create a symbolic link to something that doesn't exist
	file.link(H5G_LINK_SOFT, "foobar", "grp1/dangle");

	// Create a recursive symbolic link
	file.link(H5G_LINK_SOFT, "/grp1/recursive", "/grp1/recursive");

	// Verify link values before closing the file
	H5std_string softlink_val = file.getLinkval("grp1/soft");
	verify_val(softlink_val, "/dset1", "H5File::getLinkval grp1/soft", __LINE__, __FILE__);

	H5std_string dngllink_val = file.getLinkval("grp1/dangle");
	verify_val(dngllink_val, "foobar", "H5File::getLinkval grp1/dangle", __LINE__, __FILE__);

	H5std_string reclink_val = file.getLinkval("grp1/recursive");
	verify_val(reclink_val, "/grp1/recursive", "H5File::getLinkval grp1/recursive", __LINE__, __FILE__);

    } // end of try block
    catch (Exception E)
    {
	issue_fail_msg("test_basic_links()", __LINE__, __FILE__, E.getCDetailMsg());
    }

    // Open the file and check on the links in it
    try
    {
	// Open the file above
	H5File file(filename, H5F_ACC_RDWR, FileCreatPropList::DEFAULT, fapl);

	// Verify link values
	H5std_string softlink_val = file.getLinkval("grp1/soft");
	verify_val(softlink_val, "/dset1", "H5File::getLinkval grp1/soft", __LINE__, __FILE__);

	H5std_string reclink_val = file.getLinkval("grp1/recursive");
	verify_val(reclink_val, "/grp1/recursive", "H5File::getLinkval grp1/recursive", __LINE__, __FILE__);

	PASSED();
    } // end of try block
    catch (Exception E)
    {
	issue_fail_msg("test_basic_links()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}


/*-------------------------------------------------------------------------
 * Function:	test_links
 *
 * Purpose:	Test links
 *
 * Return:	None
 *
 * Programmer:	Binh-Minh Ribler
 *              October 16, 2009
 *
 *-------------------------------------------------------------------------
 */
#ifdef __cplusplus
extern "C"
#endif
void test_links()
{
    hid_t	fapl_id, fapl2_id;    /* File access property lists */

    // Output message about test being performed
    MESSAGE(5, ("Testing Link functions\n"));

    fapl_id = h5_fileaccess();

    test_basic_links(fapl_id);
}

/*-------------------------------------------------------------------------
 * Function:	cleanup_links
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Binh-Minh Ribler
 *		October 16, 2009
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#ifdef __cplusplus
extern "C"
#endif
void cleanup_links()
{
    HDremove(FILENAME[0]);
}

