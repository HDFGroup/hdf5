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

// A lot of the definition inherited from C test links.c is left here until
// the H5L API is implemented and tests are completed - BMR 10/19/2009
/*
 * This file needs to access private information from the H5G package.
 * This file also needs to access the group testing code.
 */
//#define H5G_FRIEND
//#define H5G_TESTING

//#include "h5test.h"
//#include "H5Gpkg.h"     /* Groups */
//#include "H5Iprivate.h" /* IDs */
//#include "H5Lprivate.h" /* Links */

/* File for external link test.  Created with gen_udlinks.c */
#define LINKED_FILE  "be_extlink2.h5"

#if 0
const char *FILENAME[] = {
    "links0",
    "links1",
    "links2",
    "links3",
    "links4a", /* 4 */
    "links4b", /* 5 */
    "links4c", /* 6 */
    "links4d", /* 7 */
    "links5",  /* 8 */
    "links6",  /* 9 */
    "links7",  /* 10 */
    "links8",  /* 11 */
    "extlinks0",        /* 12: main files */
    "tmp/extlinks0",    /* 13: */
    "extlinks1",        /* 14: target files */
    "tmp/extlinks1",    /* 15: */
    "extlinks2",        /* 16: */
    "tmp/extlinks2",    /* 17: */
    "extlinks3",        /* 18: */
    "tmp/extlinks3",    /* 19: */
    "extlinks4",        /* 20: */
    "tmp/extlinks4",    /* 21: */
    "extlinks5",        /* 22: */
    "tmp/extlinks6",    /* 23: */
    "extlinks7",        /* 24: */
    "tmp/extlinks7",    /* 25: */
    "tmp/extlinks8",    /* 26: */
    "extlinks9",        /* 27: */
    "tmp/extlinks9",    /* 28: */
    "extlinks10",       /* 29: */ /* TESTS for windows */
    "tmp/extlinks10",   /* 30: */
    "tmp/extlinks11",   /* 31: */
    "tmp/extlinks12",   /* 32: */
    "extlinks13",       /* 33: */
    "tmp/extlinks13",   /* 34: */
    "tmp/extlinks14",   /* 35: */
    "tmp/extlinks15",   /* 36: */
    "extlinks16A",      /* 37: */ /* TESTS for H5P_set_elink_fapl */
    "extlinks16B",      /* 38: */
    "extlinks17",       /* 39: */
    "extlinks18A",      /* 40: */
    "extlinks18B",      /* 41: */
    "extlinks19A",      /* 42: */
    "extlinks19B",      /* 43: */
    "extlinks20",       /* 44: */
    NULL
};

#endif // 0

#define TMPDIR          "tmp"

#define FAMILY_SIZE     1024
#define CORE_INCREMENT  1024
#define NUM400          400

/* do not do check_all_closed() for "ext*" files and "tmp/ext*" */
#define EXTSTOP         12

#define LINK_BUF_SIZE   1024
#define NAME_BUF_SIZE   1024
#define MAX_NAME_LEN    ((64*1024)+1024)

/* Link type IDs */
#define UD_HARD_TYPE 201
#define UD_CB_TYPE H5L_TYPE_MAX
#define UD_PLIST_TYPE 128
#define UD_CBFAIL_TYPE UD_PLIST_TYPE
#define UD_ERROR_TYPE 189
#define UD_BAD_TYPE1 H5L_TYPE_HARD
#define UD_BAD_TYPE2 (H5L_TYPE_UD_MIN - 5)
#define UD_BAD_VERS (H5L_LINK_CLASS_T_VERS + 1)

#define DEST_PROP_NAME "destination_group"
#define REREG_TARGET_NAME "rereg_target"

#define UD_CB_LINK_NAME "ud_callback_link"
#define NEW_UD_CB_LINK_NAME "ud_callback_link2"
#define UD_CB_TARGET "ud_target"
#define UD_CB_TARGET_LEN 10

#define LE_FILENAME "le_extlink1.h5"
#define BE_FILENAME "be_extlink1.h5"

#define ELINK_CB_FAM_SIZE (hsize_t) 100

#define H5L_DIM1 100
#define H5L_DIM2 100

/* Creation order macros */
#define CORDER_SOFT_GROUP_NAME  "corder_soft_group"
#define CORDER_NLINKS               18

/* Timestamp macros */
#define TIMESTAMP_GROUP_1       "timestamp1"
#define TIMESTAMP_GROUP_2       "timestamp2"

/* Link iteration struct */
typedef struct {
    H5_iter_order_t order;      /* Direction of iteration */
    unsigned ncalled;           /* # of times callback is entered */
    unsigned nskipped;          /* # of links skipped */
    int stop;                   /* # of iterations to stop after */
    int64_t curr;               /* Current creation order value */
    size_t max_visit;           /* Size of "visited link" flag array */
    hbool_t *visited;           /* Pointer to array of "visited link" flags */
} link_iter_info_t;

#if 0
/* Link visit structs */
typedef struct {
    const char *path;           /* Path to link */
    H5L_type_t type;            /* Type of link */
} link_visit_t;
static const link_visit_t lvisit0[] = {
    {"Dataset_zero", H5L_TYPE_HARD},
    {"Group1", H5L_TYPE_HARD},
    {"Group1/Dataset_one", H5L_TYPE_HARD},
    {"Group1/Group2", H5L_TYPE_HARD},
    {"Group1/Group2/Dataset_two", H5L_TYPE_HARD},
    {"Group1/Group2/Type_two", H5L_TYPE_HARD},
    {"Group1/Group2/hard_zero", H5L_TYPE_HARD},
    {"Group1/Type_one", H5L_TYPE_HARD},
    {"Group1/hard_one", H5L_TYPE_HARD},
    {"Type_zero", H5L_TYPE_HARD},
    {"ext_dangle", H5L_TYPE_EXTERNAL},
    {"ext_one", H5L_TYPE_EXTERNAL},
    {"hard_one", H5L_TYPE_HARD},
    {"hard_two", H5L_TYPE_HARD},
    {"hard_zero", H5L_TYPE_HARD},
    {"soft_dangle", H5L_TYPE_SOFT},
    {"soft_one", H5L_TYPE_SOFT},
    {"soft_two", H5L_TYPE_SOFT}
};
static const link_visit_t lvisit1[] = {
    {"Dataset_one", H5L_TYPE_HARD},
    {"Group2", H5L_TYPE_HARD},
    {"Group2/Dataset_two", H5L_TYPE_HARD},
    {"Group2/Type_two", H5L_TYPE_HARD},
    {"Group2/hard_zero", H5L_TYPE_HARD},
    {"Group2/hard_zero/Dataset_zero", H5L_TYPE_HARD},
    {"Group2/hard_zero/Group1", H5L_TYPE_HARD},
    {"Group2/hard_zero/Type_zero", H5L_TYPE_HARD},
    {"Group2/hard_zero/ext_dangle", H5L_TYPE_EXTERNAL},
    {"Group2/hard_zero/ext_one", H5L_TYPE_EXTERNAL},
    {"Group2/hard_zero/hard_one", H5L_TYPE_HARD},
    {"Group2/hard_zero/hard_two", H5L_TYPE_HARD},
    {"Group2/hard_zero/hard_zero", H5L_TYPE_HARD},
    {"Group2/hard_zero/soft_dangle", H5L_TYPE_SOFT},
    {"Group2/hard_zero/soft_one", H5L_TYPE_SOFT},
    {"Group2/hard_zero/soft_two", H5L_TYPE_SOFT},
    {"Type_one", H5L_TYPE_HARD},
    {"hard_one", H5L_TYPE_HARD}
};
static const link_visit_t lvisit2[] = {
    {"Dataset_two", H5L_TYPE_HARD},
    {"Type_two", H5L_TYPE_HARD},
    {"hard_zero", H5L_TYPE_HARD},
    {"hard_zero/Dataset_zero", H5L_TYPE_HARD},
    {"hard_zero/Group1", H5L_TYPE_HARD},
    {"hard_zero/Group1/Dataset_one", H5L_TYPE_HARD},
    {"hard_zero/Group1/Group2", H5L_TYPE_HARD},
    {"hard_zero/Group1/Type_one", H5L_TYPE_HARD},
    {"hard_zero/Group1/hard_one", H5L_TYPE_HARD},
    {"hard_zero/Type_zero", H5L_TYPE_HARD},
    {"hard_zero/ext_dangle", H5L_TYPE_EXTERNAL},
    {"hard_zero/ext_one", H5L_TYPE_EXTERNAL},
    {"hard_zero/hard_one", H5L_TYPE_HARD},
    {"hard_zero/hard_two", H5L_TYPE_HARD},
    {"hard_zero/hard_zero", H5L_TYPE_HARD},
    {"hard_zero/soft_dangle", H5L_TYPE_SOFT},
    {"hard_zero/soft_one", H5L_TYPE_SOFT},
    {"hard_zero/soft_two", H5L_TYPE_SOFT}
};

typedef struct {
    unsigned idx;               /* Index in link visit structure */
    const link_visit_t *info;   /* Pointer to the link visit structure to use */
} lvisit_ud_t;


/* Object visit structs */
typedef struct {
    const char *path;           /* Path to object */
    H5O_type_t type;            /* Type of object */
} obj_visit_t;
static const obj_visit_t ovisit0_old[] = {
    {".", H5O_TYPE_GROUP},
    {"Dataset_zero", H5O_TYPE_DATASET},
    {"Group1", H5O_TYPE_GROUP},
    {"Group1/Dataset_one", H5O_TYPE_DATASET},
    {"Group1/Group2", H5O_TYPE_GROUP},
    {"Group1/Group2/Dataset_two", H5O_TYPE_DATASET},
    {"Group1/Group2/Type_two", H5O_TYPE_NAMED_DATATYPE},
    {"Group1/Type_one", H5O_TYPE_NAMED_DATATYPE},
    {"Type_zero", H5O_TYPE_NAMED_DATATYPE}
};
static const obj_visit_t ovisit0_new[] = {
    {".", H5O_TYPE_GROUP},
    {"Dataset_zero", H5O_TYPE_DATASET},
    {"Group1", H5O_TYPE_GROUP},
    {"Group1/Dataset_one", H5O_TYPE_DATASET},
    {"Group1/Group2", H5O_TYPE_GROUP},
    {"Group1/Group2/Dataset_two", H5O_TYPE_DATASET},
    {"Group1/Group2/Type_two", H5O_TYPE_NAMED_DATATYPE},
    {"Group1/Type_one", H5O_TYPE_NAMED_DATATYPE},
    {"Type_zero", H5O_TYPE_NAMED_DATATYPE}
};
static const obj_visit_t ovisit1_old[] = {
    {".", H5O_TYPE_GROUP},
    {"Dataset_one", H5O_TYPE_DATASET},
    {"Group2", H5O_TYPE_GROUP},
    {"Group2/Dataset_two", H5O_TYPE_DATASET},
    {"Group2/Type_two", H5O_TYPE_NAMED_DATATYPE},
    {"Group2/hard_zero", H5O_TYPE_GROUP},
    {"Group2/hard_zero/Dataset_zero", H5O_TYPE_DATASET},
    {"Group2/hard_zero/Type_zero", H5O_TYPE_NAMED_DATATYPE},
    {"Type_one", H5O_TYPE_NAMED_DATATYPE}
};
static const obj_visit_t ovisit1_new[] = {
    {".", H5O_TYPE_GROUP},
    {"Dataset_one", H5O_TYPE_DATASET},
    {"Group2", H5O_TYPE_GROUP},
    {"Group2/Dataset_two", H5O_TYPE_DATASET},
    {"Group2/Type_two", H5O_TYPE_NAMED_DATATYPE},
    {"Group2/hard_zero", H5O_TYPE_GROUP},
    {"Group2/hard_zero/Dataset_zero", H5O_TYPE_DATASET},
    {"Group2/hard_zero/Type_zero", H5O_TYPE_NAMED_DATATYPE},
    {"Type_one", H5O_TYPE_NAMED_DATATYPE}
};
static const obj_visit_t ovisit2_old[] = {
    {".", H5O_TYPE_GROUP},
    {"Dataset_two", H5O_TYPE_DATASET},
    {"Type_two", H5O_TYPE_NAMED_DATATYPE},
    {"hard_zero", H5O_TYPE_GROUP},
    {"hard_zero/Dataset_zero", H5O_TYPE_DATASET},
    {"hard_zero/Group1", H5O_TYPE_GROUP},
    {"hard_zero/Group1/Dataset_one", H5O_TYPE_DATASET},
    {"hard_zero/Group1/Type_one", H5O_TYPE_NAMED_DATATYPE},
    {"hard_zero/Type_zero", H5O_TYPE_NAMED_DATATYPE}
};
static const obj_visit_t ovisit2_new[] = {
    {".", H5O_TYPE_GROUP},
    {"Dataset_two", H5O_TYPE_DATASET},
    {"Type_two", H5O_TYPE_NAMED_DATATYPE},
    {"hard_zero", H5O_TYPE_GROUP},
    {"hard_zero/Dataset_zero", H5O_TYPE_DATASET},
    {"hard_zero/Group1", H5O_TYPE_GROUP},
    {"hard_zero/Group1/Dataset_one", H5O_TYPE_DATASET},
    {"hard_zero/Group1/Type_one", H5O_TYPE_NAMED_DATATYPE},
    {"hard_zero/Type_zero", H5O_TYPE_NAMED_DATATYPE}
};

typedef struct {
    unsigned idx;               /* Index in object visit structure */
    const obj_visit_t *info;    /* Pointer to the object visit structure to use */
} ovisit_ud_t;
#endif

static const char *FILENAME[] = {
    "link0",
    "link1.h5",
    "link2.h5",
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
        issue_fail_msg("test_num_links()", __LINE__, __FILE__, E.getCDetailMsg());
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
        issue_fail_msg("test_num_links()", __LINE__, __FILE__, E.getCDetailMsg());
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
        issue_fail_msg("test_num_links()", __LINE__, __FILE__, E.getCDetailMsg());
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
    const char  *envval;

    envval = HDgetenv("HDF5_DRIVER");
    if(envval == NULL)
        envval = "nomatch";

    fapl_id = h5_fileaccess();

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
            test_move(my_fapl_id, new_format);
            test_copy(my_fapl_id, new_format);
            test_lcpl(my_fapl_id, new_format);
        } /* end for */

        /* Close 2nd FAPL */
        H5Pclose(fapl2_id);

        h5_clean_files(FILENAME, fapl_id);

        /* Test that external links can be used after a library reset.  MUST be
        * called last so the reset doesn't interfere with the property lists.  This
        * routine will delete its own file. */
        /* nerrors += external_reset_register() < 0 ? 1 : 0;
 */
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




