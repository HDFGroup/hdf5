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

/* ptableTest.cpp */

#include <iostream>
#include "ptableTest.h"

using namespace H5;
using namespace std;

#define TEST_FILE "packettest.h5"

/* Main test function */
int main(void)
{
    herr_t err;
    herr_t num_errors = 0;

    /* Create new HDF5 file */
    fileID = H5Fcreate(TEST_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if(fileID <0)
    {
        fprintf(stderr, "Couldn't create file.\n");
        num_errors = 1;
    }
    else {

	num_errors += BasicTest();

	num_errors += TestCompoundDatatype();

	num_errors += TestGetPacket();

	num_errors += TestGetNext();

	num_errors += TestCompress();

	num_errors += TestErrors();

	num_errors += SystemTest();

	/* Test data corruption in packed structs */
	num_errors += TestHDFFV_9758();

        /* Terminate access to the file. */
        err = H5Fclose(fileID);
        if( err < 0 )
        {
            fprintf(stderr, "Failed to close file.\n");
            num_errors++;
        }

        /* Delete the file */
        remove(TEST_FILE);
    }

    if (num_errors == 0)
      /* ALL TESTS PASSED */
      return 0;
    else
      /* ERRORS */
      return -1;
}

const char* BASICTEST_PT("/basicTest");
int BasicTest()
{
    herr_t err;
    int myRecord;
    hsize_t count;
    int error;

    TESTING("basic functionality")

    FL_PacketTable wrapper(fileID, H5P_DEFAULT, BASICTEST_PT, H5T_NATIVE_INT, 1);
    if(! wrapper.IsValid())
      goto error;

    /* Ensure initial count is zero */
    count = wrapper.GetPacketCount(error);
    if(count != 0 || error != 0)
      goto error;

    myRecord = 1;

    /* add some records test */
    err = wrapper.AppendPacket(&myRecord);
    if(err < 0)
        goto error;

    myRecord = 2;

    wrapper.AppendPacket(&myRecord);

    /* get number of records test */
    count = wrapper.GetPacketCount();
    if(count != 2)
      goto error;

    /* get records test */
    err = wrapper.GetPacket(0, &myRecord);
    if(err < 0)
      goto error;

    if(myRecord != 1)
      goto error;

    err = wrapper.GetPacket(1, &myRecord);
    if(err < 0)
      goto error;
    if(myRecord != 2)
      goto error;

    PASSED();
    return 0;

error:
    H5_FAILED();
    return 1;
}

const char* CMPDTEST_PT("/compoundTest");
int TestCompoundDatatype()
{
    hid_t dtypeID;
    hsize_t count;
    int error;

    TESTING("compound datatypes")

    /* Create compound datatype */
    typedef struct compoundType
    {
        short a, b, c;
        int e;
    } compoundType;

    dtypeID = H5Tcreate( H5T_COMPOUND, sizeof(compoundType));

    H5Tinsert(dtypeID, "abbey", HOFFSET( compoundType, a ), H5T_NATIVE_SHORT);
    H5Tinsert(dtypeID, "bert", HOFFSET( compoundType, b ), H5T_NATIVE_SHORT);
    H5Tinsert(dtypeID, "charlie", HOFFSET( compoundType, c ), H5T_NATIVE_SHORT);
    H5Tinsert(dtypeID, "ebert", HOFFSET( compoundType, e ), H5T_NATIVE_INT);

    /* Create packet table using default property list. */
    FL_PacketTable wrapper(fileID, H5P_DEFAULT, CMPDTEST_PT, dtypeID, 1);

    if(! wrapper.IsValid())
      goto error;

    compoundType first;
    first.a = 1;
    first.b = first.c = 3;
    first.e = 5;

    /* Write packet */
    wrapper.AppendPacket(&first);

    count = wrapper.GetPacketCount(error);
    if(count != 1)
      goto error;

    first.a = first.b = first.c = 0;
    first.e = 0;

    /* Read packet back */
    wrapper.GetPacket(0, &first);

    if(first.a != 1)
      goto error;
    if(first.e != 5)
      goto error;

    PASSED();

    H5Tclose(dtypeID);
    return 0;

error:

    H5E_BEGIN_TRY {
        H5Tclose(dtypeID);
    } H5E_END_TRY;


    H5_FAILED();
    return 1;
}

const char* GETNEXT_PT("/TestGetNext");
int TestGetNext()
{
    int error;
    int record;
    int records[2];
    int i;

    TESTING("GetNextPacket")

    /* Create a dataset */
    FL_PacketTable wrapper(fileID, H5P_DEFAULT, GETNEXT_PT, H5T_NATIVE_INT, 500);

    if(! wrapper.IsValid())
      goto error;

    /* Append 5 records to the dataset */
    for(record = 1; record < 6; record++)
        wrapper.AppendPacket(&record);

    /* Ensure that we can interate through the records and get the right ones */
    for(i = 1; i < 6; i++)
    {
        wrapper.GetNextPacket(&record);
        if(record != i)
          goto error;
    }

    /* Reset the index and check that it worked */
    wrapper.ResetIndex();
    if(wrapper.GetIndex(error) != 0) goto error;
    if(error < 0) goto error;

    /* Ensure that we can interate through the records and get the right ones */
    for(i = 1; i < 6; i++)
    {
        error = wrapper.GetNextPacket(&record);
        if(record != i || error <0)
          goto error;
    }

    wrapper.SetIndex(1);
    if(wrapper.GetIndex(error) != 1) goto error;
    if(error < 0) goto error;

    /* Ensure we can get multiple records with our index pointer */
    wrapper.GetNextPackets(2, records);
    if(records[0] != 2 || records[1] != 3)
      goto error;

    /* Ensure our pointer was updated correctly */
    wrapper.GetNextPacket(&record);
    if(record != 4)
      goto error;

    PASSED();
    return 0;

error:
    H5_FAILED();
    return 1;
}

const char* COMPRESS_PT("/compressTest");
int TestCompress()
{
    unsigned int flags = 0;
    unsigned int config = 0;
    size_t cd_nelemts = 0;

    TESTING("compression")
#ifdef H5_HAVE_FILTER_DEFLATE
    try {
	/* Prepare property list to set compression, randomly use deflate */
	DSetCreatPropList dscreatplist;
	dscreatplist.setDeflate(6);

        /* Create packet table with compression. */
        FL_PacketTable wrapper(fileID, COMPRESS_PT, H5T_NATIVE_CHAR, 100, dscreatplist.getId());

	/* Close the property list */
	dscreatplist.close();

	/* Verify that the deflate filter is set */

        /* Create an HDF5 C++ file object */
        H5File file;
        file.setId(fileID);

        /* Make sure that the deflate filter is set by opening the packet table
         * as a dataset and getting its creation property list */
        DataSet dset = file.openDataSet(COMPRESS_PT);

        DSetCreatPropList dcpl = dset.getCreatePlist();

	char filter_name[8];
        dcpl.getFilterById(H5Z_FILTER_DEFLATE, flags, cd_nelemts, NULL, 8, filter_name, config);
	if (HDstrncmp(filter_name, "deflate", 7) != 0)
	    H5_FAILED()
    } catch (Exception e) {
      H5_FAILED();
      return 1;
    }
    PASSED();
#else
    SKIPPED();
    puts("    deflate filter not enabled");
#endif /* H5_HAVE_FILTER_DEFLATE */
    return 0;
}

const char* PT_TESTGETPT = "/TestGetPacket";
int TestGetPacket()
{
    int record;
    int theRecs[3];
    int i;
    TESTING("GetPacket")

    /* Create a dataset.  Does not need to specify property list because
       there is no compression. */
    FL_PacketTable wrapper(fileID, PT_TESTGETPT, H5T_NATIVE_INT, 1);

    if(! wrapper.IsValid())
      goto error;

    /* Append 5 records to the dataset */
    for(record = 1; record < 6; record++)
        wrapper.AppendPacket(&record);

    /* Ensure that the records were written properly */
    wrapper.GetPacket(1, &record);
    if(record != 2)
      goto error;

    /* Ensure that we can retrieve multiple records */
    wrapper.GetPackets(1, 3, theRecs);
    for(i = 0; i < 3; i++)
    {
        if(theRecs[i] != i+2)
          goto error;
    }

    PASSED();
    return 0;

error:
    H5_FAILED();
    return 1;
}

const char* PT_TESTERROR = "/TestErrors";

int TestErrors()
{
    TESTING("error conditions")

    /* Create a dataset */
    FL_PacketTable wrapper(fileID, PT_TESTERROR, H5T_NATIVE_INT, 1);

    if(! wrapper.IsValid())
      goto error;

    int record;
    int records[3];
    int error;

    /* Append 4 records to the dataset */
    for(record = 1; record < 5; record++)
        wrapper.AppendPacket(&record);

    /* Try to confuse functions with bad indexes */
    error = wrapper.GetPacket(static_cast<unsigned>(-1), &record);
    if(error >= 0)
      goto error;
    error = wrapper.GetPacket(4, &record);
    if(error >= 0)
      goto error;
    error = wrapper.GetPacket(static_cast<unsigned>(-250), &record);
    if(error >= 0)
      goto error;
    error = wrapper.GetPacket(3000, &record);
    if(error >= 0)
      goto error;
    error = wrapper.GetPacket(1, &record);
    if(error < 0)
      goto error;

    error = wrapper.GetPackets(static_cast<unsigned>(-1), 1, records);
    if(error >= 0)
      goto error;
    error = wrapper.GetPackets(2, 4, records);
    if(error >= 0)
      goto error;
    error = wrapper.GetPackets(static_cast<unsigned>(-60), static_cast<unsigned>(-62), records);
     if(error >= 0)
      goto error;
    error = wrapper.GetPackets(10, 12, records);
    if(error >= 0)
      goto error;
    error = wrapper.GetPackets(0, 2, records);
    if(error < 0)
      goto error;
    error = wrapper.GetPackets(2, 0, records);
    if(error >= 0)
      goto error;
    error = wrapper.GetPackets(1, 1, records);
    if(error < 0)
      goto error;
    error = wrapper.GetPackets(1, 3, records);
    if(error < 0)
      goto error;

    wrapper.ResetIndex();
    error = wrapper.SetIndex(static_cast<unsigned>(-1));
    if(error >= 0)
      goto error;
    if(wrapper.GetIndex(error) != 0) goto error;
    if(error < 0) goto error;
    error = wrapper.GetNextPacket(&record);
    if(error < 0)
      goto error;
    if(record != 1)
      goto error;
    if(wrapper.GetIndex(error) != 1) goto error;
    if(error < 0) goto error;
    error = wrapper.SetIndex(20);
    if(error >= 0)
      goto error;
    error = wrapper.GetNextPacket(&record);
    if(error < 0)
      goto error;
    if(record != 2)
      goto error;
    wrapper.SetIndex(3);
    error = wrapper.GetNextPacket(&record);
    if(error < 0)
      goto error;
    if(record != 4)
      goto error;
    if(wrapper.GetIndex(error) != 4) goto error;
    if(error < 0) goto error;
    error = wrapper.GetNextPacket(&record);
    if(error >= 0)
      goto error;

    wrapper.ResetIndex();
    error = wrapper.GetNextPackets(10, records);
    if(error >= 0)
      goto error;
    error = wrapper.GetNextPackets(0, records);
    if(error < 0)
      goto error;

    PASSED();
    return 0;

error:
    H5_FAILED();
    return 1;
}

const char* PT_SYSTEMTST1 = "/SystemTest1";
const char* PT_SYSTEMTST2 = "/SystemTest2";
int SystemTest()
{
    TESTING("multiple datatypes")

    hid_t dtypeID1, dtypeID2;
    hsize_t count;
    int error;

    /* Creating two inter-related datatypes.  Create two datasets and put
     * one datatype in each. */
    typedef struct compoundType
    {
        short a, b, c;
        int e;
    } compoundType;

    dtypeID1 = H5Tcreate(H5T_COMPOUND, sizeof(compoundType));

    H5Tinsert(dtypeID1, "abbey", HOFFSET( compoundType, a ), H5T_NATIVE_SHORT);
    H5Tinsert(dtypeID1, "bert", HOFFSET( compoundType, b ), H5T_NATIVE_SHORT);
    H5Tinsert(dtypeID1, "charlie", HOFFSET( compoundType, c ), H5T_NATIVE_SHORT);
    H5Tinsert(dtypeID1, "ebert", HOFFSET( compoundType, e ), H5T_NATIVE_INT);

    typedef struct cType2
    {
        char f;
        compoundType g;
    } cType2;

    dtypeID2 = H5Tcreate(H5T_COMPOUND, sizeof(cType2));

    H5Tinsert(dtypeID2, "f", HOFFSET( cType2, f ), H5T_NATIVE_CHAR);
    H5Tinsert(dtypeID2, "g", HOFFSET( cType2, g ), dtypeID1);

    cType2 ct2[10];
    ct2[0].f = 'h';
    ct2[0].g.a = 9;
    ct2[0].g.b = -13;
    ct2[0].g.c = 0;
    ct2[0].g.e = 3000;

    /* Create the packet table datasets.  One used a deprecated constructor */
    FL_PacketTable wrapper1(fileID, PT_SYSTEMTST1, dtypeID1, 1);
    FL_PacketTable wrapper2(fileID, H5P_DEFAULT, PT_SYSTEMTST2, dtypeID2, 1);

    if(! wrapper1.IsValid())
      goto error;
    if(! wrapper2.IsValid())
      goto error;

    /* Write and read packets, ensure that nothing is unusual */
    wrapper2.AppendPacket(ct2);

    count = wrapper1.GetPacketCount();
    if(count != 0)
      goto error;

    compoundType ct1[10];
    ct1[0].a = 31;
    ct1[0].b = 4607;
    ct1[0].c = -1002;
    ct1[0].e = 3;

    ct2[1].f = 'b';
    ct2[1].g = ct1[0];

    wrapper1.AppendPacket(ct1);
    wrapper2.AppendPacket(&ct2[1]);

    wrapper1.ResetIndex();
    wrapper1.GetNextPacket(&ct1[1]);
    wrapper2.GetPacket(1, &ct2[2]);
    if(wrapper1.GetIndex(error) != 1) goto error;
    if(error < 0) goto error;
    if(wrapper2.GetIndex(error) != 0) goto error;
    if(error < 0) goto error;

    if(ct1[1].b != ct2[2].g.b)
      goto error;

    H5Tclose(dtypeID1);
    H5Tclose(dtypeID2);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Tclose(dtypeID1);
        H5Tclose(dtypeID2);
    } H5E_END_TRY;

    H5_FAILED();
    return 1;
}

/*-------------------------------------------------------------------------
 * TestHDFFV_9758(): Test that a packet table with compound datatype which
 *	contains string type can be created and written correctly. (HDFFV-9758)
 *
 * Notes:
 *	Previously, data of the field that follows the string was read back
 *	as garbage when #pragma pack(1) is used.
 * 2016/10/20 -BMR
 *      Updated:
 *		#pragma pack(1) caused failure on Emu because Sparc cannot
 *		access misaligned data.  Changed it to pack() to do the
 *		default alignment.
 * 2016/10/25 -BMR
 *-------------------------------------------------------------------------
 */
#pragma pack()  // default alignment
const char* ABHI_PT("/abhiTest");
const hsize_t NUM_PACKETS = 5;
const int STRING_LENGTH = 19; // including terminating NULL
int TestHDFFV_9758()
{
    hid_t strtype;
    hid_t compound_type;
    herr_t err;
    struct s1_t
    {
        int a;
        float b;
        double c;
        char d[STRING_LENGTH]; // null terminated string
        int e;
    };

    s1_t s1[NUM_PACKETS];
    
    for (hsize_t i = 0; i < NUM_PACKETS; i++)
    {
        s1[i].a = i;
        s1[i].b = 1.f * static_cast<float>(i * i);
        s1[i].c = 1. / (i + 1);
        sprintf(s1[i].d, "string%d", (int)i);
        s1[i].e = 100+i;
    }

    TESTING("data corruption in packed structs (HDFFV-9758)")

    // Build a compound datatype
    compound_type = H5Tcreate(H5T_COMPOUND, sizeof(s1_t));
    if (compound_type < 0)
	goto error;
    
    err = H5Tinsert(compound_type, "a_name", HOFFSET(s1_t, a), H5T_NATIVE_INT);
    if (err < 0)
	goto error;
    err = H5Tinsert(compound_type, "b_name", HOFFSET(s1_t, b), H5T_NATIVE_FLOAT);
    if (err < 0)
	goto error;
    err = H5Tinsert(compound_type, "c_name", HOFFSET(s1_t, c), H5T_NATIVE_DOUBLE);
    if (err < 0)
	goto error;

    strtype = H5Tcopy (H5T_C_S1);
    if (compound_type < 0)
	goto error;
    err = H5Tset_size (strtype, STRING_LENGTH); /* create string */
    if (err < 0)
	goto error;
    err = H5Tinsert(compound_type, "d_name", HOFFSET(s1_t, d), strtype);
    if (err < 0)
	goto error;
    err = H5Tinsert(compound_type, "e_name", HOFFSET(s1_t, e), H5T_NATIVE_INT);
    if (err < 0)
	goto error;

    { // so ptable will go out of scope before PASSED

    // Create a packet table
    FL_PacketTable ptable(fileID, "/examplePacketTable", compound_type, 1);
    if (!ptable.IsValid())
	goto error;

    // Add packets to the table
    for (size_t i = 0; i < NUM_PACKETS; i++)
    {
	/* Appends one packet at the current position */
        err = ptable.AppendPacket(s1 + i);
        if (err < 0) goto error;
    }

    // Check packet count
    const hsize_t count = ptable.GetPacketCount(err);
    if (err < 0)
	goto error;
  
    if (count != NUM_PACKETS)
    {
	std::cerr
        << "Number of packets in packet table should be " << NUM_PACKETS
        << " but is " << count << endl;
    }

    // Read and verify the data
    ptable.ResetIndex();
    for (size_t i = 0; i < NUM_PACKETS; i++)
    {
	s1_t s2;
	memset(&s2, 0, sizeof(s1_t));
	err = ptable.GetNextPacket(&s2);
	if (err < 0)
	    goto error;

	if (s2.a != s1[i].a || s2.e != s1[i].e)
	    goto error;
	else if (HDstrcmp(s2.d, s1[i].d))
	    goto error;
    }
    } // end of ptable block

    PASSED();
    return 0;

error:

    H5E_BEGIN_TRY {
        H5Tclose(strtype);
        H5Tclose(compound_type);
        H5Fclose(fileID);
    } H5E_END_TRY;

    H5_FAILED();
    return 1;
}

