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
   titerate.cpp - HDF5 C++ testing iterate related functionality

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

/* Number of datasets for group iteration test */
#define NDATASETS 50

/* Number of attributes for attribute iteration test */
//#define NATTR 50

/* Number of groups for second group iteration test */
//#define ITER_NGROUPS 150

/* General maximum length of names used */
#define NAMELEN     80

/* 1-D dataset with fixed dimensions */
//#define SPACE1_RANK     1
//#define SPACE1_DIM1     4

const H5std_string	FILE_ITERATE("titerate.h5");
const H5std_string	GROUP1("Top Group");
const H5std_string	GROUP1_PATH("/Top Group");
const H5std_string	GROUP1_1("Sub-Group 1.1");
const H5std_string	GROUP1_1_PATH("/Top Group/Sub-Group 1.1");
const H5std_string	GROUP1_2("Sub-Group 1.2");
const H5std_string	GROUP1_2_PATH("/Top Group/Sub-Group 1.2");
const H5std_string	DSET_DEFAULT_NAME("default");
const H5std_string	DSET_IN_FILE("Dataset in File");
const H5std_string	DSET_IN_FILE_PATH("/Dataset in File");
const H5std_string	DSET_IN_GRP1("Dataset in Group 1");
const H5std_string	DSET_IN_GRP1_PATH("/Top Group/Dataset in Group 1");
const H5std_string	DSET_IN_GRP1_2("Dataset in Group 1.2");
const H5std_string	DSET_IN_GRP1_2_PATH("/Top Group/Sub-Group 1.2/Dataset in Group 1.2");

typedef enum {
    RET_ZERO,
    RET_TWO,
    RET_CHANGE,
    RET_CHANGE2
} iter_enum;

/* Custom group iteration callback data */
typedef struct {
    char name[NAMELEN];     /* The name of the object */
    H5O_type_t type;        /* The type of the object */
    iter_enum command;      /* The type of return value */
} iter_info;

int iter_strcmp(const void *s1, const void *s2);

/****************************************************************
**
**  iter_strcmp(): String comparison routine for qsort
**
****************************************************************/
int iter_strcmp(const void *s1, const void *s2)
{
    return(HDstrcmp(*(const char * const *)s1,*(const char * const *)s2));
}

/****************************************************************
**
**  liter_cb(): Custom link iteration callback routine.
**
****************************************************************/
static herr_t
liter_cb(hid_t H5_ATTR_UNUSED group, const char *name, const H5L_info_t H5_ATTR_UNUSED *link_info,
    void *op_data)
{
    iter_info *info = (iter_info *)op_data;
    static int count = 0;
    static int count2 = 0;

    HDstrcpy(info->name, name);

    switch(info->command) {
        case RET_ZERO:
            return(0);

        case RET_TWO:
            return(2);

        case RET_CHANGE:
            count++;
            return(count > 10 ? 1 : 0);

        case RET_CHANGE2:
            count2++;
            return(count2 > 10 ? 1 : 0);

        default:
            printf("invalid iteration command");
            return(-1);
    } /* end switch */
} /* end liter_cb() */

/*-------------------------------------------------------------------------
 * Function:	test_iter_group
 *
 * Purpose:	Tests group iteration
 *
 * Return:	Success: 0
 *		Failure: -1
 *
 * Programmer:	Binh-Minh Ribler
 *		Friday, September 9, 2016
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void test_iter_group(FileAccPropList& fapl)
{
    int i;                  /* counting variable */
    hsize_t idx;            /* Index in the group */
    char name[NAMELEN];     /* temporary name buffer */
    char *lnames[NDATASETS + 2];/* Names of the links created */
    iter_info info;         /* Custom iteration information */
    herr_t ret;		    /* Generic return value */

    /* Output message about test being performed */
    SUBTEST("Group Iteration");

    /* Create the test file with the datasets */
    try {
	// Create file
	H5File file(FILE_ITERATE, H5F_ACC_TRUNC, FileCreatPropList::DEFAULT, fapl);

	/* Test iterating over empty group */
	info.command = RET_ZERO;
	idx = 0;
	ret = H5Literate(file.getId(), H5_INDEX_NAME, H5_ITER_INC, &idx, liter_cb, &info);
	verify_val(ret, SUCCEED, "H5Literate", __LINE__, __FILE__);

	DataType datatype(PredType::NATIVE_INT);

	// Create a scalar file space
	DataSpace filespace;

	for (i=0; i< NDATASETS; i++)
	{
        sprintf(name, "Dataset %d", i);

	// Create a dataset in the file
	DataSet dataset = file.createDataSet(name, datatype, filespace);

        /* Keep a copy of the dataset names */
        lnames[i] = HDstrdup(name);
        check_values(lnames[i], "HDstrdup returns NULL", __LINE__, __FILE__);

	} /* end for */

	/* Create a group and named datatype under root group for testing */
	Group grp(file.createGroup(GROUP1, 0));
	lnames[NDATASETS] = HDstrdup("grp");
	check_values(lnames[NDATASETS], "HDstrdup returns NULL", __LINE__, __FILE__);

	datatype.commit(file, "dtype");
	lnames[NDATASETS + 1] = HDstrdup("dtype");
	check_values(lnames[NDATASETS], "HDstrdup returns NULL", __LINE__, __FILE__);

	/* Sort the dataset names */
	HDqsort(lnames, (size_t)(NDATASETS + 2), sizeof(char *), iter_strcmp);


	/* Iterate through the datasets in the root group in various ways */

	// Open data file to read
	file.openFile(FILE_ITERATE, H5F_ACC_RDONLY, fapl);

	// Open the root group
	Group root_group(file.openGroup("/"));

	// Get the number of object in the root group
	hsize_t nobjs = root_group.getNumObjs();
	verify_val(nobjs, (hsize_t)(NDATASETS + 2), "H5Gget_info", __LINE__, __FILE__);

	H5std_string obj_name;
	for (i = 0; i < nobjs; i++)
	{
	    //H5O_info_t oinfo;               /* Object info */

	    obj_name = root_group.getObjnameByIdx(i);
        //ret = (herr_t)H5Lget_name_by_idx(root_group, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)i, dataset_name, (size_t)NAMELEN, H5P_DEFAULT);

	//oinfo = root_group.childObjType((hsize_t)i, H5_INDEX_NAME, H5_ITER_INC,  ".");
        //ret = H5Oget_info_by_idx(root_group, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)i, &oinfo, H5P_DEFAULT);
	} /* end for */

	// Attempted to iterate with invalid index, should fail
	try {
	    obj_name = root_group.getObjnameByIdx(NDATASETS + 3);

	    // Should FAIL but didn't, so throw an invalid action exception
	    throw InvalidActionException("Group::getObjnameByIdx", "Attempt to iterate with invalid index");
	}
	catch (GroupIException& invalid_action) // invalid index
	{} // do nothing, exception expected

	// Attempted to iterate with negative index, should fail
	try {
	    info.command = RET_ZERO;
	    idx = (hsize_t)-1;
	    obj_name = root_group.getObjnameByIdx(idx);

	    // Should FAIL but didn't, so throw an invalid action exception
	    throw InvalidActionException("Group::getObjnameByIdx", "Attempt to iterate with negative index");
	}
	catch (FileIException& invalid_action) // invalid index
	{} // do nothing, exception expected
	catch (GroupIException& invalid_action) // invalid index
	{} // do nothing, exception expected

	/* Test skipping exactly as many entries as in the group */
	try {
	    info.command = RET_ZERO;
	    idx = NDATASETS + 2;
	    obj_name = root_group.getObjnameByIdx(idx);

	    // Should FAIL but didn't, so throw an invalid action exception
	    throw InvalidActionException("Group::getObjnameByIdx", "Attempt to iterate with negative index");
	}
	catch (FileIException& invalid_action) // invalid index
	{} // do nothing, exception expected
	catch (GroupIException& invalid_action) // invalid index
	{} // do nothing, exception expected

	/* Test skipping more entries than are in the group */
	try {
	    info.command = RET_ZERO;
	    idx = NDATASETS + 3;
	    obj_name = root_group.getObjnameByIdx(idx);

	    // Should FAIL but didn't, so throw an invalid action exception
	    throw InvalidActionException("Group::getObjnameByIdx", "Attempt to iterate with negative index");
	}
	catch (FileIException& invalid_action) // invalid index
	{} // do nothing, exception expected
	catch (GroupIException& invalid_action) // invalid index
	{} // do nothing, exception expected

	/* Free the dataset names */
	for(i = 0; i< (NDATASETS + 2); i++)
	    HDfree(lnames[i]);

	// Everything will be closed as they go out of scope

	PASSED();
    }	// try block

    // catch all other exceptions
    catch (Exception& E)
    {
	issue_fail_msg("test_iter_group", __LINE__, __FILE__);
    }

#if 0
    /* Test all objects in group, when callback always returns 0 */
    info.command = RET_ZERO;
    idx = 0;
    if((ret = H5Literate(file, H5_INDEX_NAME, H5_ITER_INC, &idx, liter_cb, &info)) > 0)
        TestErrPrintf("Group iteration function didn't return zero correctly!\n");

    /* Test all objects in group, when callback always returns 1 */
    /* This also tests the "restarting" ability, because the index changes */
    info.command = RET_TWO;
    i = 0;
    idx = 0;
    while((ret = H5Literate(file, H5_INDEX_NAME, H5_ITER_INC, &idx, liter_cb, &info)) > 0) {
        /* Verify return value from iterator gets propagated correctly */
        verify_val(ret, 2, "H5Literate", __LINE__, __FILE__);

        /* Increment the number of times "2" is returned */
        i++;

        /* Verify that the index is the correct value */
        verify_val(idx, (hsize_t)i, "H5Literate", __LINE__, __FILE__);
        if(idx > (NDATASETS + 2))
            TestErrPrintf("Group iteration function walked too far!\n");

        /* Verify that the correct name is retrieved */
        if(HDstrcmp(info.name, lnames[(size_t)(idx - 1)]) != 0)
            TestErrPrintf("Group iteration function didn't return name correctly for link - lnames[%u] = '%s'!\n", (unsigned)(idx - 1), lnames[(size_t)(idx - 1)]);
    } /* end while */
    verify_val(ret, -1, "H5Literate", __LINE__, __FILE__);

    if(i != (NDATASETS + 2))
        TestErrPrintf("%u: Group iteration function didn't perform multiple iterations correctly!\n", __LINE__);

    /* Test all objects in group, when callback changes return value */
    /* This also tests the "restarting" ability, because the index changes */
    info.command = new_format ? RET_CHANGE2 : RET_CHANGE;
    i = 0;
    idx = 0;
    while((ret = H5Literate(file, H5_INDEX_NAME, H5_ITER_INC, &idx, liter_cb, &info)) >= 0) {
        /* Verify return value from iterator gets propagated correctly */
        verify_val(ret, 1, "H5Literate", __LINE__, __FILE__);

        /* Increment the number of times "1" is returned */
        i++;

        /* Verify that the index is the correct value */
        verify_val(idx, (hsize_t)(i + 10), "H5Literate", __LINE__, __FILE__);
        if(idx > (NDATASETS + 2))
            TestErrPrintf("Group iteration function walked too far!\n");

        /* Verify that the correct name is retrieved */
        if(HDstrcmp(info.name, lnames[(size_t)(idx - 1)]) != 0)
            TestErrPrintf("Group iteration function didn't return name correctly for link - lnames[%u] = '%s'!\n", (unsigned)(idx - 1), lnames[(size_t)(idx - 1)]);
    } /* end while */
    verify_val(ret, -1, "H5Literate", __LINE__, __FILE__);

    if(i != 42 || idx != 52)
        TestErrPrintf("%u: Group iteration function didn't perform multiple iterations correctly!\n", __LINE__);

    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

#endif
} /* test_iter_group() */


/****************************************************************
**
**  printelems(): Open an attribute and verify that it has a 
**		  the correct name
**
****************************************************************/
const H5std_string	FILE_NAME("titerate.h5");
const H5std_string	GRP_NAME("/Group_A");
const H5std_string	FDATASET_NAME( "file dset" );
const H5std_string	GDATASET_NAME( "group dset" );
const H5std_string	ATTR_NAME( "Units" );
const H5std_string	FATTR_NAME( "F attr" );
const H5std_string	GATTR_NAME( "G attr" );
const int	DIM1 = 2;
void printelems(const Group& group, const H5std_string& dsname, const H5std_string& atname)
{
    try
    {
	DataSet d1(group.openDataSet(dsname));
	DataSpace s1 = d1.getSpace();
	s1.close();
	d1.close();

	unsigned idx = 0;
	Attribute a1(group.openAttribute(idx));
	H5std_string aname = a1.getName();
        verify_val(aname, atname, "printelems", __LINE__, __FILE__);

	a1.close();
  }
   // catch failure caused by the DataSpace operations
   catch( DataSpaceIException error )
   {
	error.printError();
   }

   // catch failure caused by the Group operations
   catch( GroupIException error )
   {
	error.printError();
   }

   // catch failure caused by the DataSet operations
   catch( DataSetIException error )
   {
	error.printError();
   }
}

/*-------------------------------------------------------------------------
 * Function:	test_HDFFV_9920
 *
 * Purpose:	Tests the fix for HDFFV-9920
 *
 * Programmer:	Binh-Minh Ribler
 *		Friday, September 9, 2016
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void test_HDFFV_9920()
{
   int attr_data[2] = { 100, 200};
   hsize_t dims[1] = { DIM1 };
   
   try
   {
	// Create a new file and a group in it
	H5File file( FILE_NAME, H5F_ACC_TRUNC );

	Group gr1(file.createGroup(GRP_NAME));

	// Create the data space for the attribute.
	DataSpace dspace = DataSpace (1, dims );

	DataSet fds = file.createDataSet(FDATASET_NAME, PredType::STD_I32BE, dspace);
	DataSet gds = gr1.createDataSet(GDATASET_NAME, PredType::STD_I32BE, dspace);

	// Create a file attribute and a group attribute. 
	Attribute fa1 = file.createAttribute(FATTR_NAME, PredType::STD_I32BE, 
	                                          dspace);
	Attribute ga1 = gr1.createAttribute(GATTR_NAME, PredType::STD_I32BE, 
	                                          dspace);
     
	// Write the attribute data. 
	fa1.write( PredType::NATIVE_INT, attr_data);
	ga1.write( PredType::NATIVE_INT, attr_data);

	fa1.close();
	ga1.close();
	fds.close();
	gds.close();

	// Verify the attributes have correct names.
	printelems(file, FDATASET_NAME, FATTR_NAME);
	printelems(gr1, GDATASET_NAME, GATTR_NAME);

   }  // end of try block

   // catch failure caused by the H5File operations
   catch( DataSpaceIException error )
   {
	error.printError();
   }

   // catch failure caused by the H5File operations
   catch( AttributeIException error )
   {
	error.printError();
   }

   // catch failure caused by the H5File operations
   catch( FileIException error )
   {
	error.printError();
   }

   // catch failure caused by the DataSet operations
   catch( DataSetIException error )
   {
	error.printError();
   }
}


/*-------------------------------------------------------------------------
 * Function:	test_iterate
 *
 * Purpose:	Tests iterate functionality
 *
 * Return:	Success: 0
 *		Failure: -1
 *
 * Programmer:	Binh-Minh Ribler
 *		Tuesday, September 6, 2016
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
extern "C"
void test_iterate()
{
    // Output message about test being performed
    MESSAGE(5, ("Testing Iterate Feature\n"));

    // Create access property with latest library version.
    FileAccPropList fapl;
    fapl.setLibverBounds(H5F_LIBVER_LATEST, H5F_LIBVER_LATEST);

    test_iter_group(fapl);	// Test iterating groups
    test_HDFFV_9920();		// Test the fix of HDFFV-9920
    //test_iter_attr(fapl);	// Test iterating attributes

}   // test_iterate

/*-------------------------------------------------------------------------
 * Function:    cleanup_iterate
 *
 * Purpose:     Cleanup temporary test files
 *
 * Return:      none
 *
 * Programmer:  (use C version)
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
extern "C"
void cleanup_iterate()
{
    HDremove(FILE_ITERATE.c_str());
} // cleanup_iterate
