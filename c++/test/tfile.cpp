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

/*****************************************************************************
   FILE
   tfile.cpp - HDF5 C++ testing the file I/O features

   EXTERNAL ROUTINES/VARIABLES:
     These routines are in the test directory of the C library:
        h5_reset() -- in h5test.c, resets the library by closing it
        h5_fileaccess() -- in h5test.c, returns a file access template
        h5_cleanup() -- in h5test.c, cleanup temporary test files

 ***************************************************************************/

#include <string>

#ifdef OLD_HEADER_FILENAME
#include <iostream.h>
#else
#include <iostream>
#endif

#include "H5Cpp.h"
#include "testhdf5.h"
#include "h5cpputil.h"

#ifndef H5_NO_NAMESPACE
using namespace H5;
#endif

#define F1_USERBLOCK_SIZE       (hsize_t)0
#define F1_OFFSET_SIZE          sizeof(haddr_t)
#define F1_LENGTH_SIZE          sizeof(hsize_t)
#define F1_SYM_LEAF_K           4
#define F1_SYM_INTERN_K         16
const string    FILE1("tfile1.h5");

#define F2_USERBLOCK_SIZE       (hsize_t)512
#define F2_OFFSET_SIZE          8
#define F2_LENGTH_SIZE          8
#define F2_SYM_LEAF_K           8
#define F2_SYM_INTERN_K         32
const string    FILE2("tfile2.h5");

#define F3_USERBLOCK_SIZE       (hsize_t)0
#define F3_OFFSET_SIZE          F2_OFFSET_SIZE
#define F3_LENGTH_SIZE          F2_LENGTH_SIZE
#define F3_SYM_LEAF_K           F2_SYM_LEAF_K
#define F3_SYM_INTERN_K         F2_SYM_INTERN_K
const string    FILE3("tfile3.h5");

#define KB                      1024
//#define FILE4                   "tfile4.h5"
const string    FILE4("tfile4.h5");

const char *FILENAME[] = {
	    "tfile1.h5",
	    "tfile2.h5",
	    "tfile3.h5",
	    "tfile4.h5",
	        NULL
};


/*-------------------------------------------------------------------------
 * Function:    test_file_create
 *
 * Purpose:     Test file and template creations
 *
 * Return:      None
 *
 * Programmer:  Binh-Minh Ribler (use C version)
 *              January, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void 
test_file_create(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing File Creation I/O\n"));

    /* Test create with various sequences of H5F_ACC_EXCL and */
    /* H5F_ACC_TRUNC flags */

    /* Create with H5F_ACC_EXCL */
    /* First ensure the file does not exist */
    remove(FILE1.c_str());

    try {
	H5File* file1 = new H5File (FILE1, H5F_ACC_EXCL);

	/*
	* try to create the same file with H5F_ACC_TRUNC. This should fail
	* because file1 is the same file and is currently open.
	*/
	try {
	    H5File file2 (FILE1, H5F_ACC_TRUNC);  // should throw E

	    // Should FAIL but didn't - BMR (Note 1): a macro, with a diff 
	    // name, that skips the comparison b/w the 1st & 2nd args would 
	    // be more appropriate, but VERIFY can be used for now - Mar 13, 01
	    // also, more text about what is testing would be better.
	    VERIFY(file2.getId(), FAIL, "H5File constructor"); 
	}
	catch( FileIException E ) {} // do nothing, FAIL expected

	// Close file file1 

	delete file1;

	/*
	* Try again with H5F_ACC_EXCL. This should fail because the file already
	* exists from the previous steps.
	*/
	try { 
	    file1 = new H5File( FILE1, H5F_ACC_EXCL );  // should throw E
	    VERIFY(file1->getId(), FAIL, "H5File constructor"); 
	}
	catch( FileIException E ) {} // do nothing, FAIL expected

    	// Test create with H5F_ACC_TRUNC. This will truncate the existing file.
	file1 = new H5File (FILE1, H5F_ACC_TRUNC);

    	/*
     	* Try to truncate first file again. This should fail because file1 is the
     	* same file and is currently open.
     	*/
    	try {
	    H5File file2 (FILE1, H5F_ACC_TRUNC);   // should throw E
	    VERIFY(file2.getId(), FAIL, "H5File constructor"); 
	}
	catch( FileIException E ) {} // do nothing, FAIL expected

    	/*
     	* Try with H5F_ACC_EXCL. This should fail too because the file already
     	* exists.
     	*/
    	try {
	    H5File file3 (FILE1, H5F_ACC_EXCL);  // should throw E
	    VERIFY(file3.getId(), FAIL, "H5File constructor"); 
    	}
	catch( FileIException E ) {} // do nothing, FAIL expected

    	/* Get the file-creation template */
	FileCreatPropList tmpl1 = file1->getCreatePlist();

	hsize_t ublock = tmpl1.getUserblock();
	VERIFY(ublock, F1_USERBLOCK_SIZE, "FileCreatPropList::getUserblock"); 

    	size_t  parm1, parm2;		/*file-creation parameters	*/
	tmpl1.getSizes( parm1, parm2);
    	VERIFY(parm1, F1_OFFSET_SIZE, "FileCreatPropList::getSizes");
    	VERIFY(parm2, F1_LENGTH_SIZE, "FileCreatPropList::getSizes");

    	int  iparm1;		/*file-creation parameters	*/
#ifdef H5_WANT_H5_V1_4_COMPAT
    	int  iparm2;	/*file-creation parameters	*/
#else /* H5_WANT_H5_V1_4_COMPAT */
    	unsigned  iparm2;	/*file-creation parameters	*/
#endif /* H5_WANT_H5_V1_4_COMPAT */
    	tmpl1.getSymk( iparm1, iparm2);
    	VERIFY(iparm1, F1_SYM_INTERN_K, "FileCreatPropList::getSymk");
    	VERIFY(iparm2, F1_SYM_LEAF_K, "FileCreatPropList::getSymk");

	// tmpl1 is automatically closed; if error occurs, it'll be
	// caught in the catch block

	/* Close first file */
	delete file1;
    }
    catch( PropListIException E ) {
	CHECK(FAIL, FAIL, E.getCFuncName());
    }
    catch( FileIException E ) {
	CHECK(FAIL, FAIL, E.getCFuncName());
    }

    try
    {
    	/* Create a new file with a non-standard file-creation template */
	FileCreatPropList* tmpl1 = new FileCreatPropList;

    	/* Set the new file-creation parameters */
	tmpl1->setUserblock (F2_USERBLOCK_SIZE);
	tmpl1->setSizes( F2_OFFSET_SIZE, F2_LENGTH_SIZE );
	tmpl1->setSymk( F2_SYM_INTERN_K, F2_SYM_LEAF_K );

    	/*
     	 * Try to create second file, with non-standard file-creation template
     	 * params.
     	*/
	H5File file2( FILE2, H5F_ACC_TRUNC, *tmpl1 );

    	/* Release file-creation template */
	delete tmpl1;

	/* Get the file-creation template */
	tmpl1 = new FileCreatPropList (file2.getCreatePlist());

	/* Get the file-creation parameters */
	hsize_t ublock = tmpl1->getUserblock();
	VERIFY(ublock, F2_USERBLOCK_SIZE, "FileCreatPropList::getUserblock"); 

    	size_t  parm1, parm2;		/*file-creation parameters	*/
	tmpl1->getSizes( parm1, parm2);
    	VERIFY(parm1, F2_OFFSET_SIZE, "FileCreatPropList::getSizes");
    	VERIFY(parm2, F2_LENGTH_SIZE, "FileCreatPropList::getSizes");

    	int  iparm1;		/*file-creation parameters	*/
#ifdef H5_WANT_H5_V1_4_COMPAT
    	int  iparm2;	/*file-creation parameters	*/
#else /* H5_WANT_H5_V1_4_COMPAT */
    	unsigned  iparm2;	/*file-creation parameters	*/
#endif /* H5_WANT_H5_V1_4_COMPAT */
    	tmpl1->getSymk( iparm1, iparm2);
    	VERIFY(iparm1, F2_SYM_INTERN_K, "FileCreatPropList::getSymk");
    	VERIFY(iparm2, F2_SYM_LEAF_K, "FileCreatPropList::getSymk");

	/* Clone the file-creation template */
	FileCreatPropList tmpl2;
	tmpl2.copy (*tmpl1);

	/* Dynamically release file-creation template */
	delete tmpl1;

	/* Set the new file-creation parameter */
	tmpl2.setUserblock( F3_USERBLOCK_SIZE );

	/*
	* Try to create second file, with non-standard file-creation template
	* params
	*/
	H5File file3( FILE3, H5F_ACC_TRUNC, tmpl2 );

	/* Get the file-creation template */
	tmpl1 = new FileCreatPropList (file3.getCreatePlist());

	/* Get the file-creation parameters */
	ublock = tmpl1->getUserblock();
	VERIFY(ublock, F3_USERBLOCK_SIZE, "FileCreatPropList::getUserblock"); 

	tmpl1->getSizes( parm1, parm2);
    	VERIFY(parm1, F3_OFFSET_SIZE, "FileCreatPropList::getSizes");
    	VERIFY(parm2, F3_LENGTH_SIZE, "FileCreatPropList::getSizes");

    	tmpl1->getSymk( iparm1, iparm2);
    	VERIFY(iparm1, F3_SYM_INTERN_K, "FileCreatPropList::getSymk");
    	VERIFY(iparm2, F3_SYM_LEAF_K, "FileCreatPropList::getSymk");

	/* Dynamically release file-creation template */
	delete tmpl1;
    }
    catch( PropListIException E ) {
	CHECK(FAIL, FAIL, E.getCFuncName());
    }
} /* test_file_create() */


/*-------------------------------------------------------------------------
 * Function:    test_file_open
 *
 * Purpose:     Test file accesses
 *
 * Return:      None
 *
 * Programmer:  Binh-Minh Ribler (use C version)
 *              January, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void 
test_file_open(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing File Opening I/O\n"));

    try {

	/* Open first file */
	H5File file1 (FILE2, H5F_ACC_RDWR );

	/* Get the file-creation template */
	FileCreatPropList tmpl1 = file1.getCreatePlist();

	/* Get the file-creation parameters */
	hsize_t ublock = tmpl1.getUserblock();
	VERIFY(ublock, F2_USERBLOCK_SIZE, "FileCreatPropList::getUserblock");

    	size_t  parm1, parm2;		/*file-creation parameters	*/
	tmpl1.getSizes( parm1, parm2);
	VERIFY(parm1, F2_OFFSET_SIZE, "FileCreatPropList::getSizes");
	VERIFY(parm2, F2_LENGTH_SIZE, "FileCreatPropList::getSizes");

	int  iparm1;            /*file-creation parameters      */
#ifdef H5_WANT_H5_V1_4_COMPAT
	int  iparm2;       /*file-creation parameters      */
#else /* H5_WANT_H5_V1_4_COMPAT */
	unsigned  iparm2;       /*file-creation parameters      */
#endif /* H5_WANT_H5_V1_4_COMPAT */
	tmpl1.getSymk( iparm1, iparm2);
	VERIFY(iparm1, F2_SYM_INTERN_K, "FileCreatPropList::getSymk");
	VERIFY(iparm2, F2_SYM_LEAF_K, "FileCreatPropList::getSymk");
    }   // end of try block

    catch( Exception E ) {
        CHECK(FAIL, FAIL, E.getCFuncName());
    }
} /* test_file_open() */


/*-------------------------------------------------------------------------
 * Function:    test_file_size
 *
 * Purpose:     Test file size.  
 *
 * Return:      None
 *
 * Programmer:  Raymond Lu
 *              June, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void 
test_file_size(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing File Size\n"));

    hid_t	fapl_id;
    fapl_id = h5_fileaccess(); // in h5test.c, returns a file access template

    try {
        // Use the file access template id to create a file access prop.
        // list object to pass in H5File::H5File
        FileAccPropList fapl(fapl_id);

    	// Set to sec2 driver.  Do we want to test other file drivers?
        // They're not tested in C++.
        // File drivers seem not implemented.
	//fapl.setSec2();

        // Create a file
	H5File file4( FILE4, H5F_ACC_TRUNC, FileCreatPropList::DEFAULT, fapl);

        // Get file size
        hsize_t file_size = file4.getFileSize();

        // Check if file size is reasonable.  It's supposed to be 2KB now.
        if(file_size<1*KB || file_size>4*KB)
            CHECK(FAIL, FAIL, "H5File::getFileSize");
    }   // end of try block

    catch( Exception E ) {
        CHECK(FAIL, FAIL, E.getCFuncName());
    }

    // use C test utility routine to close property list.
    H5Pclose(fapl_id);
    
} /* test_file_size() */


/*-------------------------------------------------------------------------
 * Function:    test_file_name
 *
 * Purpose:     Test getting file's name.  
 *
 * Return:      None
 *
 * Programmer:  Binh-Minh Ribler
 *              July, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
const int	RANK = 2;
const int	NX = 4;
const int	NY = 5;
const string	GROUPNAME ("group");
const string	DSETNAME ("dataset");
const string	ATTRNAME ("attribute");
const string	DTYPENAME ("compound");

/* Compound datatype */
typedef struct s1_t {
    unsigned int a;
    float        b;
} s1_t;

static void 
test_file_name()
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing File Name\n"));

    string file_name;
    try {
        // Create a file using default properties.
	H5File file4(FILE4, H5F_ACC_TRUNC);

        // Get file name from the file instance.
        file_name = file4.getFileName();
	verify_val(file_name, FILE4, "H5File::getFileName", __LINE__, __FILE__);

	/* Create a group in the root group */
	Group group(file4.createGroup(GROUPNAME, 0));

	/* Get and verify file name */
	file_name = group.getFileName();
	verify_val(file_name, FILE4, "Group::getFileName", __LINE__, __FILE__);

	/* Create the data space  */
	hsize_t dims[RANK] = {NX, NY};
	DataSpace space(RANK, dims);

	/* Create a new dataset */
	DataSet dataset(file4.createDataSet (DSETNAME, PredType::NATIVE_INT, space));

	/* Get and verify file name */
	file_name = dataset.getFileName();
	verify_val(file_name, FILE4, "DataSet::getFileName", __LINE__, __FILE__);

	/* Create an attribute for the dataset */
	Attribute attr(dataset.createAttribute(ATTRNAME, PredType::NATIVE_INT, space));

	/* Get and verify file name */
	file_name = attr.getFileName();
	verify_val(file_name, FILE4, "Attribute::getFileName", __LINE__, __FILE__);

	/* Create a compound datatype */
	CompType comp_type (sizeof(s1_t));

	/* Insert fields */
	comp_type.insertMember("a", HOFFSET(s1_t, a), PredType::NATIVE_INT);
	comp_type.insertMember("b", HOFFSET(s1_t, b), PredType::NATIVE_FLOAT);

	/* Save it on file */
	comp_type.commit(file4, DTYPENAME);

	/* Get and verify file name */
	comp_type.getFileName();
	verify_val(file_name, FILE4, "CompType::getFileName", __LINE__, __FILE__);
    }   // end of try block
    catch (Exception E) {
        CHECK(FAIL, FAIL, E.getCFuncName());
    }

} /* test_file_name() */


/*-------------------------------------------------------------------------
 * Function:    test_file
 *
 * Purpose:     Main program
 *
 * Return:      None
 *
 * Programmer:  Binh-Minh Ribler (use C version)
 *              January 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void 
test_file(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing File I/O operations\n"));

    test_file_create();	/* Test file creation (also creation templates) */
    test_file_open();	/* Test file opening */
    test_file_size();	/* Test file size */
    test_file_name();	/* Test getting file's name */

    /* use C test utility routine to clean up data files */
 //   h5_cleanup(FILENAME, fapl_id); already called by AddTest

} /* test_file() */


/*-------------------------------------------------------------------------
 * Function:	cleanup_file
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:  (use C version)
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_file(void)
{
    remove(FILE1.c_str());
    remove(FILE2.c_str());
    remove(FILE3.c_str());
    remove(FILE4.c_str());
} /* cleanup_file */
