#include "ptableTest.h"

#define TEST_FILE "packettest.h5"

/* Main test function */
int main(void)
{
    herr_t err;

    /* Create new HDF5 file */
    fileID = H5Fcreate(TEST_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if(fileID <0)
        fprintf(stderr, "Couldn't open file.\n");
    else {

            BasicTest();

            TestCompoundDatatype();

            TestGetPacket();

            TestGetNext();

            TestErrors();

            SystemTest();

            VariableLengthTest();

        /* Terminate access to the file. */
        err = H5Fclose(fileID);
        if( err < 0 )
            fprintf(stderr, "Failed to close file.\n");

        /* Delete the file */
        remove(TEST_FILE);
    }
}


void BasicTest()
{
    printf("Basic Test running...\n");

    herr_t err;
    int myRecord;
    int count;
    int error;

    FL_PacketTable wrapper(fileID, "/basicTest", H5T_NATIVE_INT, 1);
    assert(wrapper.IsValid());

    /* Ensure initial count is zero */
    count = wrapper.GetPacketCount(error);
    assert(count == 0);
    assert(error == 0);

    myRecord = 1;

    /* add some records test */
    err = wrapper.AppendPacket(&myRecord);
    if(err == -1)
        printf("AppendHorizontalRecord failed.\n");
    else{

    myRecord = 2;

    wrapper.AppendPacket(&myRecord);

    /* get number of records test */
    count = wrapper.GetPacketCount();    

    assert(count == 2);

    /* get records test */
    err = wrapper.GetPacket(0, &myRecord);
    if(err == -1)
        fprintf(stderr, "Error in GetPacket.\n");

    assert(myRecord == 1);

    err = wrapper.GetPacket(1, &myRecord);
    if(err == -1)
        fprintf(stderr, "Error in GetPacket.\n");

    assert(myRecord == 2);
}
}

void TestCompoundDatatype()
{
printf("TestCompoundDatatype running...\n");

    hid_t dtypeID;
    int error;

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

    /* Create packet table */
    FL_PacketTable wrapper(fileID, "/compoundTest", dtypeID, 1);

    assert(wrapper.IsValid());

    compoundType first;
    first.a = 1;
    first.b = first.c = 3;
    first.e = 5;

    /* Write packet */
    wrapper.AppendPacket(&first);

    int count = wrapper.GetPacketCount(error);
    assert(count == 1);

    first.a = first.b = first.c = 0;
    first.e = 0;

    /* Read packet back */
    wrapper.GetPacket(0, &first);

    assert(first.a == 1);
    assert(first.e == 5);
}

void TestGetNext()
{
printf("TestGetNext running...\n");

    int error;

    /* Create a dataset */
    FL_PacketTable wrapper(fileID, "/TestGetNext", H5T_NATIVE_INT, 1);

    assert(wrapper.IsValid());

    int record;
    int records[2];

    /* Append 5 records to the dataset */
    for(record = 1; record < 6; record++)
        wrapper.AppendPacket(&record);

    /* Ensure that we can interate through the records and get the right ones */
    for(int i = 1; i < 6; i++)
    {
        wrapper.GetNextPacket(&record);
        assert(record == i);
    }

    wrapper.ResetIndex();

    /* Ensure that we can interate through the records and get the right ones */
    for(int i = 1; i < 6; i++)
    {
        error = wrapper.GetNextPacket(&record);
        assert(record == i);
        assert(error == 0);
    }

    wrapper.SetIndex(1);

    /* Ensure we can get multiple records with our index pointer */
    wrapper.GetNextPackets(2, records);
    assert(records[0] == 2);
    assert(records[1] == 3);

    /* Ensure our pointer was updated correctly */
    wrapper.GetNextPacket(&record);
    assert(record == 4);
}

void TestGetPacket()
{
    printf("TestGetPacket running...\n");

    /* Create a dataset */
    FL_PacketTable wrapper(fileID, "/TestGetPacket", H5T_NATIVE_INT, 1);

    assert(wrapper.IsValid());

    int record;

    /* Append 5 records to the dataset */
    for(record = 1; record < 6; record++)
        wrapper.AppendPacket(&record);

    /* Ensure that the records were written properly */
    wrapper.GetPacket(1, &record);
    assert(record == 2);

    /* Ensure that we can retrieve multiple records */
    int theRecs[3] = {0, 0, 0};
    wrapper.GetPackets(1, 3, theRecs);
    for(int i = 0; i < 3; i++)
        assert(theRecs[i] == i+2);
}

void TestErrors()
{
    printf("TestErrors running...\n");

    /* Create a dataset */
    FL_PacketTable wrapper(fileID, "/TestErrors", H5T_NATIVE_INT, 1);

    assert(wrapper.IsValid());

    int record;
    int records[3];
    int error;

    /* Append 4 records to the dataset */
    for(record = 1; record < 5; record++)
        wrapper.AppendPacket(&record);

    /* Try to confuse functions with bad indexes */
    error = wrapper.GetPacket(-1, &record);
    assert(error == -1);
    error = wrapper.GetPacket(4, &record);
    assert(error == -1);
    error = wrapper.GetPacket(-250, &record);
    assert(error == -1);
    error = wrapper.GetPacket(3000, &record);
    assert(error == -1);
    error = wrapper.GetPacket(1, &record);
    assert(error == 0);

    error = wrapper.GetPackets(-1, 1, records);
    assert(error == -1);
    error = wrapper.GetPackets(2, 4, records);
    assert(error == -1);
    error = wrapper.GetPackets(-60, -62, records);
    assert(error == -1);
    error = wrapper.GetPackets(10, 12, records);
    assert(error == -1);
    error = wrapper.GetPackets(0, 2, records);
    assert(error == 0);
    error = wrapper.GetPackets(2, 0, records);
    assert(error == -1);
    error = wrapper.GetPackets(1, 1, records);
    assert(error == 0);
    error = wrapper.GetPackets(1, 3, records);
    assert(error == 0);

    wrapper.ResetIndex();
    error = wrapper.SetIndex(-1);
    assert(error == -1);
    error = wrapper.GetNextPacket(&record);
    assert(error == 0);
    assert(record == 1);
    error = wrapper.SetIndex(20);
    assert(error == -1);
    error = wrapper.GetNextPacket(&record);
    assert(error == 0);
    assert(record == 2);
    wrapper.SetIndex(3);
    error = wrapper.GetNextPacket(&record);
    assert(error == 0);
    assert(record == 4);
    error = wrapper.GetNextPacket(&record);
    assert(error == -1);

    wrapper.ResetIndex();
    error = wrapper.GetNextPackets(10, records);
    assert(error == -1);
    error = wrapper.GetNextPackets(0, records);
    assert(error == 0);
}

void SystemTest()
{
    printf("SystemTest running...\n");

    hid_t dtypeID1, dtypeID2;

    /* Creating two inter-related datatypes.  Create two datasets and put 
     * one datatype in each. */
    typedef struct compoundType
    {
        short a, b, c;
        int e;
    } compoundType;

    dtypeID1 = H5Tcreate( H5T_COMPOUND, sizeof(compoundType));

    H5Tinsert(dtypeID1, "abbey", HOFFSET( compoundType, a ), H5T_NATIVE_SHORT);
    H5Tinsert(dtypeID1, "bert", HOFFSET( compoundType, b ), H5T_NATIVE_SHORT);
    H5Tinsert(dtypeID1, "charlie", HOFFSET( compoundType, c ), H5T_NATIVE_SHORT);
    H5Tinsert(dtypeID1, "ebert", HOFFSET( compoundType, e ), H5T_NATIVE_INT);

    typedef struct cType2
    {
        char f;
        compoundType g;
    } cType2;

    dtypeID2 = H5Tcreate ( H5T_COMPOUND, sizeof(cType2));

    H5Tinsert(dtypeID2, "f", HOFFSET( cType2, f ), H5T_NATIVE_CHAR);
    H5Tinsert(dtypeID2, "g", HOFFSET( cType2, g ), dtypeID1);

    cType2 ct2[10];
    ct2[0].f = 'h';
    ct2[0].g.a = 9;
    ct2[0].g.b = -13;
    ct2[0].g.c = 0;
    ct2[0].g.e = 3000;

    /* Create the packet table datasets */
    FL_PacketTable wrapper1(fileID, "/SystemTest1", dtypeID1, 1);
    FL_PacketTable wrapper2(fileID, "/SystemTest2", dtypeID2, 1);

    assert(wrapper1.IsValid());
    assert(wrapper2.IsValid());

    /* Write and read packets, ensure that nothing is unusual */
    wrapper2.AppendPacket(ct2);

    unsigned int count = wrapper1.GetPacketCount();
    assert(count == 0);

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

    assert(ct1[1].b == ct2[2].g.b);
}

void VariableLengthTest(void)
{
    long test_long;
    short test_short;
    hvl_t read_buf;

    printf("VariableLengthTest running...\n");

    /* Create a variable length table */
    VL_PacketTable* test_VLPT = new VL_PacketTable(fileID, "/VariableLengthTest", 1);

    /* Verify that the creation succeeded */
    assert(test_VLPT->IsValid());

    /* Append some packets */
    test_short = 9;
    test_VLPT->AppendPacket(&test_short, sizeof(short));
    test_long = 16;
    test_VLPT->AppendPacket(&test_long, sizeof(long));

    /* Read them back and make sure they are correct */
    test_VLPT->GetNextPackets(1, &read_buf);

    assert(read_buf.len == sizeof(short));
    assert(*(short *)(read_buf.p) == test_short);

    /* Free the memory used by the read */
    test_VLPT->FreeReadbuff(1, &read_buf);

    /* Read the second record */
    test_VLPT->GetNextPackets(1, &read_buf);

    assert(read_buf.len == sizeof(long));
    assert(*(long *)(read_buf.p) == test_long);

    /* Free the memory used by the read */
    test_VLPT->FreeReadbuff(1, &read_buf);

    /* Close the packet table */
    delete test_VLPT;

    /* Reopen the packet table and verify that it is variable length */
    PacketTable * new_pt = new PacketTable(fileID, "/VariableLengthTest");

    /* Verify that the open succeeded */
    assert(new_pt->IsValid());

    assert(new_pt->IsVariableLength() == 1);

    /* Close the packet table */
    delete new_pt;
}
