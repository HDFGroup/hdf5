/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*****************************************************************************
   FILE
   tlinks.cpp - HDF5 C++ testing functionalities associated with the
        C link interface (H5L)

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

#define NAME_BUF_SIZE   1024
#define H5L_DIM1 100
#define H5L_DIM2 100

// Object visit structs
typedef struct {
    const char *path;           /* Path to object */
    H5O_type_t type;            /* Type of object */
} obj_visit_t;

// User data for callback function
typedef struct {
    unsigned idx;               /* Index in object visit structure */
    const obj_visit_t *info;    /* Pointer to the object visit structure to use */
} ovisit_ud_t;

static const char *FILENAME[] = {
    "link0",
    "link1.h5",
    "link2.h5",
    "visit",
    NULL
};


/*-------------------------------------------------------------------------
 * Function:    test_basic_links
 *
 * Purpose      Test building a file with assorted links.
 *
 * Return       Success: 0
 *              Failure: -1
 *
 * October 16, 2009
 *-------------------------------------------------------------------------
 */
static void test_basic_links(hid_t fapl_id, hbool_t new_format)
{
    hsize_t size[1] = {1};
    char filename[NAME_BUF_SIZE];

    // Use the file access template id to create a file access prop. list.
    FileAccPropList fapl(fapl_id);

    try
    {
        if(new_format)
            SUBTEST("Link creation (w/new group format)")
        else
            SUBTEST("Link creation")

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

        // Because these are not implemented in the C++ API yet, they are
        // used so CommonFG::getLinkval can be tested.
        // Create a hard link
        if(H5Lcreate_hard(
                file_id, "dset1", H5L_SAME_LOC, "grp1/hard1",
                H5P_DEFAULT, H5P_DEFAULT) < 0)
            throw Exception("test_basic_links", "H5Lcreate_hard failed");

        // Create a symbolic link
        if(H5Lcreate_soft(
                "/dset1", file_id, "grp1/soft", H5P_DEFAULT, H5P_DEFAULT) < 0)
            throw Exception("test_basic_links", "H5Lcreate_soft failed");

        // Create a symbolic link to something that doesn't exist
        if(H5Lcreate_soft(
                "foobar", file_id, "grp1/dangle", H5P_DEFAULT, H5P_DEFAULT) < 0)
            throw Exception("test_basic_links", "H5Lcreate_soft failed");

        // Create a recursive symbolic link
        if(H5Lcreate_soft(
                "/grp1/recursive", file_id, "/grp1/recursive",
                H5P_DEFAULT, H5P_DEFAULT) < 0)
            throw Exception("test_basic_links", "H5Lcreate_soft failed");

        // Verify link values before closing the file

        H5std_string softlink_val = file.getLinkval("grp1/soft");
        verify_val(softlink_val, "/dset1", "H5File::getLinkval grp1/soft", __LINE__, __FILE__);

        H5std_string dngllink_val = file.getLinkval("grp1/dangle");
        verify_val(dngllink_val, "foobar", "H5File::getLinkval grp1/dangle", __LINE__, __FILE__);

        H5std_string reclink_val = file.getLinkval("grp1/recursive");
        verify_val(reclink_val, "/grp1/recursive", "H5File::getLinkval grp1/recursive", __LINE__, __FILE__);

    } // end of try block
    catch (Exception& E)
    {
        issue_fail_msg("test_basic_links()", __LINE__, __FILE__, E.getCDetailMsg());
    }

    // Open the file and check on the links in it
    try
    {
        // Open the file above
        H5File file(filename, H5F_ACC_RDWR, FileCreatPropList::DEFAULT, fapl);

        // Verify link existence
        if(file.nameExists("dset1", LinkAccPropList::DEFAULT) != TRUE)
            throw InvalidActionException("H5File::nameExists", "dset1 doesn't exist");
        if(file.nameExists("grp1/soft", LinkAccPropList::DEFAULT) != TRUE)
            throw InvalidActionException("H5File::nameExists", "grp1/soft doesn't exist");
        // Deprecated
        if(file.exists("dset1", LinkAccPropList::DEFAULT) != TRUE)
            throw InvalidActionException("H5File::exists", "dset1 doesn't exist");
        if(file.exists("grp1/soft", LinkAccPropList::DEFAULT) != TRUE)
            throw InvalidActionException("H5File::exists", "grp1/soft doesn't exist");

        // Verify link values
        H5std_string softlink_val = file.getLinkval("grp1/soft");
        verify_val(softlink_val, "/dset1", "H5File::getLinkval grp1/soft", __LINE__, __FILE__);

        H5std_string reclink_val = file.getLinkval("grp1/recursive");
        verify_val(reclink_val, "/grp1/recursive", "H5File::getLinkval grp1/recursive", __LINE__, __FILE__);

        PASSED();
    } // end of try block
    catch (Exception& E)
    {
        issue_fail_msg("test_basic_links()", __LINE__, __FILE__, E.getCDetailMsg());
    }
} // test_basic_links


/*-------------------------------------------------------------------------
 * Function:    test_lcpl
 *
 * Purpose:     Tests link creation property lists, specifically, the
 *              character encoding property.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 * March, 2018
 *-------------------------------------------------------------------------
 */
const H5std_string GROUP1NAME("First_group");
const H5std_string GROUP2NAME("Second_group");
static void
test_lcpl(hid_t fapl_id, hbool_t new_format)
{
    H5L_info_t linfo;
    char filename[1024];
    hsize_t dims[2];

    if(new_format)
        SUBTEST("Link creation property lists (w/new group format)")
    else
        SUBTEST("Link creation property lists")

    try
    {
        FileAccPropList fapl(fapl_id);

        // Create a new file.
        h5_fixname(FILENAME[0], fapl_id, filename, sizeof filename);
        H5File file(filename, H5F_ACC_TRUNC, FileCreatPropList::DEFAULT, fapl);

        // Create and link a group with the default LCPL.
        Group grp_1(file.createGroup(GROUP1NAME));
        grp_1.close();

        // Check that its character encoding is the default.
        linfo = file.getLinkInfo(GROUP1NAME);
        if(linfo.cset != H5T_CSET_ASCII)
            throw InvalidActionException("H5Lget_info", "Character encoding is not default");

        // Create and commit a datatype with the default LCPL.
        IntType dtype(PredType::NATIVE_INT);
        dtype.commit(file, "/type");
        dtype.close();

        // Check that its character encoding is the default.
        linfo = file.getLinkInfo("/type");
        verify_val(linfo.cset, H5T_CSET_ASCII, "Character encoding is not default", __LINE__, __FILE__);

        // Create a simple dataspace.
        dims[0] = H5L_DIM1;
        dims[1] = H5L_DIM2;
        DataSpace dspace(2 ,dims);

        // Create a dataset using the default LCPL.
        DataSet dset(file.createDataSet("/dataset", PredType::NATIVE_INT, dspace));
        dset.close();

        // Check that its character encoding is the default.
        linfo = file.getLinkInfo("/dataset");
        verify_val(linfo.cset, H5T_CSET_ASCII, "Character encoding is not default", __LINE__, __FILE__);

        // Create a link creation property list with the UTF-8 character encoding.
        LinkCreatPropList lcpl;
        lcpl.setCharEncoding(H5T_CSET_UTF8);

        // Create and link a group with the new LCPL.
        Group grp_2(file.createGroup(GROUP2NAME, lcpl));
        grp_2.close();

        // Check that its character encoding is UTF-8.
        linfo = file.getLinkInfo(GROUP2NAME);
        verify_val(linfo.cset, H5T_CSET_UTF8, "Character encoding is not UTF-8", __LINE__, __FILE__);

        PASSED();
    } // end of try block
    catch (Exception& E)
    {
        issue_fail_msg("test_lcpl()", __LINE__, __FILE__, E.getCDetailMsg());
    }
} // end test_lcpl()


/*-------------------------------------------------------------------------
 * Function:    test_move
 *
 * Purpose:     Tests wrappers of H5Lmove()
 *
 * Return:      Success: 0
 *              Failure: number of errors
 * March, 2018
 *-------------------------------------------------------------------------
 */
static void
test_move(hid_t fapl_id, hbool_t new_format)
{
    char   filename[1024];

    if(new_format)
        SUBTEST("Group::moveLink (w/new group format)")
    else
        SUBTEST("Group::moveLink")

    try
    {
        FileAccPropList fapl(fapl_id);

        // Create two new files
        h5_fixname(FILENAME[0], fapl_id, filename, sizeof filename);
        H5File file_a(filename, H5F_ACC_TRUNC, FileCreatPropList::DEFAULT, fapl);
        h5_fixname(FILENAME[1], fapl_id, filename, sizeof filename);
        H5File file_b(filename, H5F_ACC_TRUNC, FileCreatPropList::DEFAULT, fapl);

        // Create groups in first file
        Group grp_1(file_a.createGroup(GROUP1NAME));
        Group grp_2(file_a.createGroup(GROUP2NAME));
        Group grp_move(grp_1.createGroup("group_move"));

        // Create hard and soft links
        grp_1.link(H5L_TYPE_HARD, "group_move", "hard");
        grp_2.link(H5L_TYPE_SOFT, "/First_group/group_copy", "soft");

        // Move a group across files, should fail
        try {
            grp_1.moveLink("group_move", file_b, "group_new_name");

            // Should throw an exception but didn't
    	    H5_FAILED();
	        cerr << "    Group group_move should not be moved across files" << endl;
        } catch (Exception& E) {
            // expected
        }

        // Move a soft link across files, should succeed
        grp_2.moveLink("soft", file_b, "soft_new_name");
        if(file_b.exists("soft_new_name") != TRUE)
            throw InvalidActionException("H5File::exists", "grp1/soft doesn't exist");

        // Move a group across groups in the same file while renaming it
        grp_1.moveLink("group_move", grp_2, "group_new_name");

        // Open the group just moved to the new location. */
        Group moved_grp = grp_2.openGroup("group_new_name");
        moved_grp.close();

        // Verify that the group is no longer in the original location
        try {
            moved_grp = grp_1.openGroup("group_move");

            // Should throw an exception but didn't
    	    H5_FAILED();
	        cerr << "    Group group_move should not be in original location" << endl;
        } catch (Exception& E) {
            // expected
        }

        // Use H5Lmove to rename a group without moving it
        H5std_string new_name("group_new_name");
        H5std_string newer_name("group_newer_name");
        grp_2.moveLink(new_name, newer_name);

        // Open the group
        moved_grp = grp_2.openGroup("group_newer_name");
        moved_grp.close();

        // Use H5Lmove to move a group without renaming it
        grp_2.moveLink(newer_name, grp_1, newer_name);

        // Open the group
        moved_grp = grp_1.openGroup("group_newer_name");
        moved_grp.close();

        // Move the group while giving long paths
        file_a.moveLink("/First_group/group_newer_name", grp_2, "/Second_group/group_newest_name");

        // Open the group just moved to the new location
        moved_grp = grp_2.openGroup("group_newest_name");
        moved_grp.close();

        // Verify that the groups are not in previous locations
        try {
            moved_grp = grp_1.openGroup("group_newer_name");
            moved_grp.close();

    	    H5_FAILED(); // Should throw an exception but didn't
	        cerr << "    Group group_newer_name should not be in GROUP1NAME" << endl;
        } catch (Exception& E) {
            // expected
        }
        try {
            moved_grp = grp_2.openGroup("group_newer_name");
            moved_grp.close();

    	    H5_FAILED(); // Should throw an exception but didn't
	        cerr << "    Group group_newer_name should not be in GROUP2NAME" << endl;
        } catch (Exception& E) {
            // expected
        }
        try {
            moved_grp = grp_2.openGroup("group_new_name");
            moved_grp.close();

    	    H5_FAILED(); // Should throw an exception but didn't
	        cerr << "    Group group_new_name should not be in GROUP2NAME" << endl;
        } catch (Exception& E) {
            // expected
        }
        try {
            moved_grp = grp_1.openGroup("group_copy");
            moved_grp.close();

    	    H5_FAILED(); // Should throw an exception but didn't
	        cerr << "    Group group_copy should not be in GROUP1NAME" << endl;
        } catch (Exception& E) {
            // expected
        }
        PASSED();
    } // end of try block
    catch (Exception& E)
    {
        issue_fail_msg("test_move()", __LINE__, __FILE__, E.getCDetailMsg());
    }
} // test_move

/*-------------------------------------------------------------------------
 * Function:    test_copy
 *
 * Purpose:     Tests wrappers of H5Lcopy()
 *
 * Return:      Success: 0
 *              Failure: number of errors
 * March, 2018
 *-------------------------------------------------------------------------
 */
static void test_copy(hid_t fapl_id, hbool_t new_format)
{
    char filename[1024];

    if(new_format)
        SUBTEST("Group::copyLink (w/new group format)")
    else
        SUBTEST("Group::copyLink")

    try
    {
        // Create two new files
        h5_fixname(FILENAME[0], fapl_id, filename, sizeof filename);
        H5File file_a(filename, H5F_ACC_TRUNC, FileCreatPropList::DEFAULT, fapl_id);
        h5_fixname(FILENAME[1], fapl_id, filename, sizeof filename);
        H5File file_b(filename, H5F_ACC_TRUNC, FileCreatPropList::DEFAULT, fapl_id);

        // Create groups in first file
        Group grp_1(file_a.createGroup(GROUP1NAME));
        Group grp_2(file_a.createGroup(GROUP2NAME));
        Group grp_move(grp_1.createGroup("group_copy"));

        // Create hard and soft links
        grp_1.link("group_copy", H5L_SAME_LOC, "hard");
        grp_2.link("/First_group/group_copy", "soft");

        // Copy a group across files, should fail
        try {
            grp_1.copyLink("group_copy", file_b, "group_new_name");
        } catch (Exception& E) {
            // expected
        }

        // Copy a soft link across files, should succeed
        grp_2.copyLink("soft", file_b, "soft_new_name");
        if (file_b.exists("soft_new_name") != TRUE)
            throw InvalidActionException("H5File::exists", "soft_new_name doesn't exist");

        // Move a group across groups in the same file while renaming it
        H5std_string copy_name("group_copy");
        H5std_string new_name("group_new_name");
        grp_1.copyLink(copy_name, grp_2, new_name);

        // Open the group just moved to the new location.
        Group moved_grp(grp_2.openGroup("group_new_name"));
        moved_grp.close();

        // Verify that the group is also in the original location
        moved_grp = grp_1.openGroup("group_copy");
        moved_grp.close();

        // Create a group in the same location with a different name
        grp_2.copyLink("group_new_name", "group_newer_name");

        // Open the group
        moved_grp = grp_2.openGroup("group_newer_name");
        moved_grp.close();

        // Verify that the group is also in the original location
        moved_grp = grp_2.openGroup("group_new_name");
        moved_grp.close();

        // Use H5Lcopy to copy to a different location with the same name
        grp_2.copyLink("group_newer_name", grp_1, "group_newer_name");

        // Open the group
        moved_grp = grp_1.openGroup("group_newer_name");
        moved_grp.close();

        // Verify that the group is still in the previous location
        moved_grp = grp_2.openGroup("group_new_name");
        moved_grp.close();

        // Copy the group while giving long paths
        file_a.copyLink("/First_group/group_newer_name", grp_2, "/Second_group/group_newest_name");

        // Open the newest group just moved to the new location
        moved_grp = grp_2.openGroup("group_newest_name");
        moved_grp.close();

        // Verify that the group is still in all previous original locations
        moved_grp = grp_1.openGroup("group_newer_name");
        moved_grp.close();

        moved_grp = grp_2.openGroup("group_newer_name");
        moved_grp.close();

        moved_grp = grp_2.openGroup("group_new_name");
        moved_grp.close();

        moved_grp = grp_1.openGroup("group_copy");
        moved_grp.close();

        // Delete "group_newer_name" from group 2, then try to open it.
        grp_2.unlink("group_newer_name");
        try {
            moved_grp = grp_2.openGroup("group_newer_name");
            moved_grp.close();

    	    H5_FAILED(); // Should throw an exception but didn't
	        cerr << "    Group group_newer_name should not be in GROUP2NAME" << endl;
        } catch (Exception& E) {
            // expected
        }

        // Delete "group_copy" from group 1, then try to open it.
        grp_1.unlink("group_copy");
        try {
            moved_grp = grp_1.openGroup("group_copy");
            moved_grp.close();

    	    H5_FAILED(); // Should throw an exception but didn't
	        cerr << "    Group group_copy should not be in GROUP1NAME" << endl;
        } catch (Exception& E) {
            // expected
        }

        PASSED();
    } // end of try block
    catch (Exception& E)
    {
        issue_fail_msg("test_copy()", __LINE__, __FILE__, E.getCDetailMsg());
    }
} // test_copy


/*-------------------------------------------------------------------------
 * Function:    test_num_links
 *
 * Purpose      Test setting and getting limit of number of links
 *
 * Return       Success: 0
 *              Failure: -1
 *
 * October 16, 2009
 *-------------------------------------------------------------------------
 */
static void test_num_links(hid_t fapl_id, hbool_t new_format)
{
    char filename[NAME_BUF_SIZE];

    if(new_format)
        SUBTEST("Setting number of links (w/new group format)")
    else
        SUBTEST("Setting number of links")

    try
    {
        // Use the file access template id to create a file access prop. list.
        FileAccPropList fapl(fapl_id);

        h5_fixname(FILENAME[0], fapl_id, filename, sizeof filename);
        H5File file(filename, H5F_ACC_RDWR, FileCreatPropList::DEFAULT, fapl);

        LinkAccPropList lapl;
        size_t nlinks = 5;
        lapl.setNumLinks(nlinks);

        // Read it back and verify
        size_t read_nlinks = lapl.getNumLinks();
        verify_val(read_nlinks, nlinks, "LinkAccPropList::setNumLinks", __LINE__, __FILE__);

        PASSED();
    } // end of try block
    catch (Exception& E)
    {
        issue_fail_msg("test_num_links()", __LINE__, __FILE__, E.getCDetailMsg());
    }
} // test_num_links


// Data for visit on the file
static const obj_visit_t file_visit[] = {
    {".", H5O_TYPE_GROUP},
    {"Data", H5O_TYPE_GROUP},
    {"Data/Compressed_Data", H5O_TYPE_DATASET},
    {"Data/Float_Data", H5O_TYPE_DATASET},
};

// Data for visit on the group
static const obj_visit_t group_visit[] = {
    {".", H5O_TYPE_GROUP},
    {"Compressed_Data", H5O_TYPE_DATASET},
    {"Float_Data", H5O_TYPE_DATASET},
};

const H5std_string FILE_NAME("tvisit.h5");
const H5std_string GROUP_NAME("/Data");
const H5std_string DSET1_NAME("/Data/Compressed_Data");
const H5std_string DSET2_NAME("/Data/Float_Data");
const int RANK = 2;
const int DIM1 = 2;

// Operator function
static int visit_obj_cb(H5Object& obj, const H5std_string name, const H5O_info_t *oinfo, void *_op_data)
{
    ovisit_ud_t *op_data = static_cast <ovisit_ud_t *>(_op_data);

    // Check for correct object information
    if(strcmp(op_data->info[op_data->idx].path, name.c_str())) return(H5_ITER_ERROR);
    if(op_data->info[op_data->idx].type != oinfo->type) return(H5_ITER_ERROR);

    // Advance to next location
    op_data->idx++;

    return(H5_ITER_CONT);
}

/*-------------------------------------------------------------------------
 * Function:    test_visit
 *
 * Purpose      Test H5Object::visit
 *
 * Return       None
 *
 * February 8, 2019
 *-------------------------------------------------------------------------
 */
static void test_visit(hid_t fapl_id, hbool_t new_format)
{
    hsize_t  dims[2];
    hsize_t  cdims[2];
    char filename[NAME_BUF_SIZE];

    if(new_format)
        SUBTEST("H5Object::visit (w/new group format)")
    else
        SUBTEST("H5Object::visit")

    try
    {
        // Use the file access template id to create a file access prop. list
        FileAccPropList fapl(fapl_id);

        // Build the hdf5 file name and create the file
        h5_fixname(FILENAME[3], fapl_id, filename, sizeof filename);
	    H5File *file = new H5File(filename, H5F_ACC_TRUNC, FileCreatPropList::DEFAULT, fapl);

        // Create a group
	    Group* group = new Group(file->createGroup(GROUP_NAME));

        // Create a chunked/compressed dataset within this group specified by path
	    dims[0] = 20;
	    dims[1] = 2;
	    cdims[0] = 2;
	    cdims[1] = 2;
	    DataSpace *dataspace = new DataSpace(RANK, dims); // create new dspace
	    DSetCreatPropList ds_creatplist;  // create dataset creation prop list
	    ds_creatplist.setChunk(2, cdims);  // then modify it for compression
	    ds_creatplist.setDeflate(6);

	    DataSet* dataset = new DataSet(file->createDataSet(DSET1_NAME,
                        PredType::NATIVE_INT, *dataspace, ds_creatplist));

	    delete dataset;
	    delete dataspace;

	    // Create another dataset
	    dims[0] = 5;
	    dims[1] = 2;
	    dataspace = new DataSpace(RANK, dims); // create second dspace
	    dataset = new DataSet(file->createDataSet(DSET2_NAME,
			            PredType::NATIVE_FLOAT, *dataspace));

        // Close everything
	    delete dataset;
	    delete dataspace;
	    delete group;
	    delete file;

	    // Reopen the file and group in the file.
	    file = new H5File(filename, H5F_ACC_RDWR);
	    group = new Group(file->openGroup("Data"));

        // Open the group
	    dataset = new DataSet(group->openDataSet(DSET2_NAME));
        delete dataset;

        // Visit objects in the file
        ovisit_ud_t udata;          /* User-data for visiting */
        udata.idx = 0;
        udata.info = file_visit;

        file->visit(H5_INDEX_NAME, H5_ITER_INC, visit_obj_cb, &udata, H5O_INFO_BASIC);

        // Visit objects in the group
        udata.idx = 0;
        udata.info = group_visit;

        group->visit(H5_INDEX_NAME, H5_ITER_INC, visit_obj_cb, &udata, H5O_INFO_BASIC);

	    // Close the group and file.
	    delete group;
	    delete file;

        PASSED();
    } // end of try block
    catch (Exception& E)
    {
  cerr << "in catch" << endl;
        issue_fail_msg("test_visit()", __LINE__, __FILE__, E.getCDetailMsg());
    }
} // test_visit()


/*-------------------------------------------------------------------------
 * Function:    test_links
 *
 * Purpose      Test links
 *
 * Return       None
 *
 * October 16, 2009
 *-------------------------------------------------------------------------
 */
extern "C"
void test_links()
{
    hid_t        fapl_id, fapl2_id;    /* File access property lists */
    unsigned new_format;     /* Whether to use the new format or not */

    if((fapl_id = h5_fileaccess()) < 0)
        throw Exception("test_links", "Unable to get file access property list");

    // Output message about test being performed
    MESSAGE(5, ("Testing Various Links\n"));
    try
    {
        /* Copy the file access property list */
        if((fapl2_id = H5Pcopy(fapl_id)) < 0)
            throw Exception("test_links", "H5Pcopy failed");

        /* Set the "use the latest version of the format" bounds for creating
           objects in the file */
        if(H5Pset_libver_bounds(fapl2_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
            throw Exception("test_links", "H5Pset_libver_bounds failed");

        /* Loop over using new group format */
        for(new_format = FALSE; new_format <= TRUE; new_format++)
        {
            hid_t my_fapl_id;

            /* Check for FAPL to use */
            if(new_format)
                my_fapl_id = fapl2_id;
            else
                my_fapl_id = fapl_id;

            /* General tests... (on both old & new format groups */
            // FileAccPropList may be passed in instead of fapl id
            test_basic_links(my_fapl_id, new_format);
            test_num_links(my_fapl_id, new_format);
            test_move(my_fapl_id, new_format);
            test_copy(my_fapl_id, new_format);
            test_lcpl(my_fapl_id, new_format);
            test_visit(my_fapl_id, new_format);
        } /* end for */

        /* Close 2nd FAPL */
        H5Pclose(fapl2_id);

        h5_clean_files(FILENAME, fapl_id);
    }
    catch (Exception& E)
    {
        issue_fail_msg("test_links()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}


/*-------------------------------------------------------------------------
 * Function:    cleanup_links
 *
 * Purpose      Cleanup temporary test files
 *
 * Return       none
 *-------------------------------------------------------------------------
 */
extern "C"
void cleanup_links()
{
    HDremove(FILENAME[0]);
    HDremove(FILENAME[1]);
}
