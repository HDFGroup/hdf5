/****************************************************************************
 * NCSA HDF								    *
 * Software Development Group						    *
 * National Center for Supercomputing Applications			    *
 * University of Illinois at Urbana-Champaign				    *
 * 605 E. Springfield, Champaign IL 61820				    *
 *									    *
 * For conditions of distribution and use, see the accompanying		    *
 * hdf/COPYING file.							    *
 *									    *
 ****************************************************************************/

/***********************************************************
*
* Test program:	 tfile
*
* Test the low-level file I/O features.
*
*************************************************************/

#include <testhdf5.h>
#include <H5Cpp.h>

#ifndef H5_NO_NAMESPACE
using namespace H5;
#endif

#include <H5private.h>
#include <H5Bprivate.h>
#include <H5Pprivate.h>

#define F1_USERBLOCK_SIZE  (hsize_t)0
#define F1_OFFSET_SIZE	   sizeof(haddr_t)
#define F1_LENGTH_SIZE	   sizeof(hsize_t)
#define F1_SYM_LEAF_K	   4
#define F1_SYM_INTERN_K	   16
#define FILE1	"tfile1.h5"

#define F2_USERBLOCK_SIZE  (hsize_t)512
#define F2_OFFSET_SIZE	   8
#define F2_LENGTH_SIZE	   8
#define F2_SYM_LEAF_K	   8
#define F2_SYM_INTERN_K	   32
#define FILE2	"tfile2.h5"

#define F3_USERBLOCK_SIZE  (hsize_t)0
#define F3_OFFSET_SIZE	   F2_OFFSET_SIZE
#define F3_LENGTH_SIZE	   F2_LENGTH_SIZE
#define F3_SYM_LEAF_K	   F2_SYM_LEAF_K
#define F3_SYM_INTERN_K	   F2_SYM_INTERN_K
#define FILE3	"tfile3.h5"


/*-------------------------------------------------------------------------
 * Function:    test_file_create
 *
 * Purpose:     Test file and template creations
 *
 * Return:      None
 *
 * Programmer:  Binh-Minh Ribler
 *              January, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void 
test_file_create(void)
{
    hid_t		tmpl1, tmpl2;	/*file creation templates	*/
    int			iparm, iparm2;
    herr_t		ret;		/*generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Low-Level File Creation I/O\n"));

    /* Test create with various sequences of H5F_ACC_EXCL and */
    /* H5F_ACC_TRUNC flags */

    /* Create with H5F_ACC_EXCL */
    /* First ensure the file does not exist */
    //remove(FILE1);

    try {
	H5File* fid1 = new H5File (FILE1, H5F_ACC_EXCL);

	/*
	* try to create the same file with H5F_ACC_TRUNC. This should fail
	* because fid1 is the same file and is currently open.
	*/
	try { H5File fid2 (FILE1, H5F_ACC_TRUNC); }
	catch( FileIException error ) {
	    // cannot use fid2 here (out of scope), but the exception was
	    // thrown only if file id was < 0, so -1 is used to verify - 1/15/01
            VERIFY(-1, FAIL, "H5File constructor");
        }
	// Close file fid1 
	delete fid1;

	/*
	* Try again with H5F_ACC_EXCL. This should fail because the file already
	* exists from the previous steps.
	*/
	try { fid1 = new H5File( FILE1, H5F_ACC_EXCL ); }
	catch( FileIException error ){ VERIFY(-1, FAIL, "H5File constructor"); }

    	// Test create with H5F_ACC_TRUNC. This will truncate the existing file.
	fid1 = new H5File (FILE1, H5F_ACC_TRUNC);

    	/*
     	* Try to truncate first file again. This should fail because fid1 is the
     	* same file and is currently open.
     	*/
    	try { H5File fid2 (FILE1, H5F_ACC_TRUNC); }
    	catch( FileIException error ) { VERIFY(-1, FAIL, "H5File constructor"); }

    	/*
     	* Try with H5F_ACC_EXCL. This should fail too because the file already
     	* exists.
     	*/
    	try { H5File fid3 (FILE1, H5F_ACC_EXCL); }
    	catch( FileIException error ) { VERIFY(-1, FAIL, "H5File constructor"); }

    	/* Get the file-creation template */
	FileCreatPropList tmpl1 = fid1->getCreatePlist();

	hsize_t ublock = tmpl1.getUserblock();
	VERIFY(ublock, F1_USERBLOCK_SIZE, "FileCreatPropList::H5Pget_userblock"); 

    	size_t  parm1, parm2;		/*file-creation parameters	*/
	tmpl1.getSizes( parm1, parm2);
    	VERIFY(parm1, F1_OFFSET_SIZE, "FileCreatPropList::getSizes");
    	VERIFY(parm2, F1_LENGTH_SIZE, "FileCreatPropList::getSizes");

    	int  iparm1, iparm2;		/*file-creation parameters	*/
    	tmpl1.getSymk( iparm1, iparm2);
    	VERIFY(iparm1, F1_SYM_INTERN_K, "FileCreatPropList::getSymk");
    	VERIFY(iparm2, F1_SYM_LEAF_K, "FileCreatPropList::getSymk");

	// tmpl1 is automatically closed; if error occurs, it'll be
	// caught in the catch block

	/* Close first file */
	delete fid1;
    }
    catch( PropListIException error ) {
	CHECK(-1, FAIL, error.getCDetailMesg());
    }
    catch( FileIException error ) {
	CHECK(-1, FAIL, error.getCDetailMesg());
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
	H5File fid2( FILE2, H5F_ACC_TRUNC, *tmpl1 );

    	/* Release file-creation template */
	delete tmpl1;

	/* Get the file-creation template */
	tmpl1 = new FileCreatPropList (fid2.getCreatePlist());

	/* Get the file-creation parameters */
	hsize_t ublock = tmpl1->getUserblock();
	VERIFY(ublock, F2_USERBLOCK_SIZE, "FileCreatPropList::getUserblock"); 

    	size_t  parm1, parm2;		/*file-creation parameters	*/
	tmpl1->getSizes( parm1, parm2);
    	VERIFY(parm1, F2_OFFSET_SIZE, "FileCreatPropList::getSizes");
    	VERIFY(parm2, F2_LENGTH_SIZE, "FileCreatPropList::getSizes");

    	int  iparm1, iparm2;		/*file-creation parameters	*/
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
	H5File fid3( FILE3, H5F_ACC_TRUNC, tmpl2 );

	/* Get the file-creation template */
	tmpl1 = new FileCreatPropList (fid3.getCreatePlist());

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
    catch( PropListIException error ) {
	CHECK(-1, FAIL, error.getCDetailMesg());
    }
    catch( FileIException error ) {
	CHECK(-1, FAIL, error.getCDetailMesg());
    }
}				/* test_file_create() */


/*-------------------------------------------------------------------------
 * Function:    test_file_open
 *
 * Purpose:     Test file accesses
 *
 * Return:      None
 *
 * Programmer:  Binh-Minh Ribler
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
    MESSAGE(5, ("Testing Low-Level File Opening I/O\n"));

    try {

	/* Open first file */
	H5File fid1 (FILE2, H5F_ACC_RDWR );

	/* Get the file-creation template */
	//FileCreatPropList tmpl1;
	FileCreatPropList tmpl1 = fid1.getCreatePlist();

	/* Get the file-creation parameters */
	hsize_t ublock = tmpl1.getUserblock();
	VERIFY(ublock, F2_USERBLOCK_SIZE, "FileCreatPropList::getUserblock");

    	size_t  parm1, parm2;		/*file-creation parameters	*/
	tmpl1.getSizes( parm1, parm2);
	VERIFY(parm1, F2_OFFSET_SIZE, "FileCreatPropList::getSizes");
	VERIFY(parm2, F2_LENGTH_SIZE, "FileCreatPropList::getSizes");

	int  iparm1, iparm2;            /*file-creation parameters      */
	tmpl1.getSymk( iparm1, iparm2);
	VERIFY(iparm1, F2_SYM_INTERN_K, "FileCreatPropList::getSymk");
	VERIFY(iparm2, F2_SYM_LEAF_K, "FileCreatPropList::getSymk");
    }   // end of try block

    catch( FileIException error ) {
        CHECK(FAIL, FAIL, error.getCDetailMesg());
    }
    catch( PropListIException error ) {
        CHECK(FAIL, FAIL, error.getCDetailMesg());
    }
}				/* test_file_open() */


/*-------------------------------------------------------------------------
 * Function:    test_file
 *
 * Purpose:     Main program
 *
 * Return:      None
 *
 * Programmer:  Binh-Minh Ribler
 *              January, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void 
test_file(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Low-Level File I/O\n"));

    test_file_create();	/* Test file creation (also creation templates) */
    test_file_open();	/* Test file opening */
}				/* test_file() */


/*-------------------------------------------------------------------------
 * Function:	cleanup_file
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Albert Cheng
 *              July 2, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_file(void)
{
    remove(FILE1);
    remove(FILE2);
    remove(FILE3);
}

