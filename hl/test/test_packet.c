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

#include <stdlib.h>
#include <string.h>
#include "h5hltest.h"
#include "H5PTpublic.h"
#include "H5TBpublic.h"

/*-------------------------------------------------------------------------
 * Packet Table API test
 *
 *-------------------------------------------------------------------------
 */

#define NRECORDS 8
#define BIG_TABLE_SIZE  8000
#define NFIELDS 5
#define TEST_FILE_NAME "test_packet_table.h5"
#define TEST_COMPRESS_FILE "test_packet_compress.h5"
#define PT_NAME "Test Packet Table"
#define H5TB_TABLE_NAME "Table1"

/*-------------------------------------------------------------------------
 * structure used for some tests, a particle
 *-------------------------------------------------------------------------
 */
typedef struct particle_t
{
 char   name[16];
 int    lati;
 int    longi;
 float  pressure;
 double temperature;
} particle_t;

/*-------------------------------------------------------------------------
 * a static array of particles for writing and checking reads
 *-------------------------------------------------------------------------
 */
static particle_t testPart[NRECORDS] = {
    {"zero", 0,0, 0.0f, 0.0f},
    {"one",  10,10, 1.0f, 10.0f},
    {"two",  20,20, 2.0f, 20.0f},
    {"three",30,30, 3.0f, 30.0f},
    {"four", 40,40, 4.0f, 40.0f},
    {"five", 50,50, 5.0f, 50.0f},
    {"six",  60,60, 6.0f, 60.0f},
    {"seven",70,70, 7.0f, 70.0f}
  };

/*-------------------------------------------------------------------------
 * function that compares one particle
 * Comparing floating point should be safe here; HDF5 should store the
 * fields verbatim and not lose any bits.  -JML
 *-------------------------------------------------------------------------
 */
static int cmp_par(size_t i, size_t j, particle_t *rbuf, particle_t *wbuf )
{
 if ( ( HDstrcmp( rbuf[i].name, wbuf[j].name ) != 0 ) ||
  rbuf[i].lati != wbuf[j].lati ||
  rbuf[i].longi != wbuf[j].longi ||
  !H5_FLT_ABS_EQUAL(rbuf[i].pressure,wbuf[j].pressure) ||
  !H5_DBL_ABS_EQUAL(rbuf[i].temperature,wbuf[j].temperature) ) {
  return FAIL;
 }
 return SUCCEED;
}

/*-------------------------------------------------------------------------
 * function to create a datatype representing the particle struct
 *-------------------------------------------------------------------------
 */
static hid_t
make_particle_type(void)
{
 hid_t type_id;
 hid_t string_type;
 size_t type_size = sizeof(particle_t);

 /* Create the memory data type. */
 if ((type_id = H5Tcreate (H5T_COMPOUND, type_size )) < 0 )
  return FAIL;

 /* Insert fields. */
 if ((string_type = H5Tcopy(H5T_C_S1)) < 0)
     return FAIL;
 if (H5Tset_size(string_type, (size_t)16) < 0)
     return FAIL;

 if ( H5Tinsert(type_id, "Name", HOFFSET(particle_t, name) , string_type ) < 0 )
     return FAIL;
 if ( H5Tinsert(type_id, "Lat", HOFFSET(particle_t, lati) , H5T_NATIVE_INT ) < 0 )
     return FAIL;
 if ( H5Tinsert(type_id, "Long", HOFFSET(particle_t, longi) , H5T_NATIVE_INT ) < 0 )
     return FAIL;
 if ( H5Tinsert(type_id, "Pressure", HOFFSET(particle_t, pressure) , H5T_NATIVE_FLOAT ) < 0 )
     return FAIL;
 if ( H5Tinsert(type_id, "Temperature", HOFFSET(particle_t, temperature) , H5T_NATIVE_DOUBLE ) < 0 )
     return FAIL;

 return type_id;
}

    /* Create a normal HL table just like the HL examples do */
static int create_hl_table(hid_t fid)
{
  /* Calculate the offsets of the particle struct members in memory */
  size_t part_offset[NFIELDS] = { HOFFSET( particle_t, name ),
                                  HOFFSET( particle_t, lati ),
                                  HOFFSET( particle_t, longi ),
                                  HOFFSET( particle_t, pressure ),
                                  HOFFSET( particle_t, temperature )};

    /* Define field information */
    const char *field_names[NFIELDS]  =
      { "Name","Latitude", "Longitude", "Pressure", "Temperature" };
    hid_t      field_type[NFIELDS];
    hid_t      string_type;
    hsize_t    chunk_size = 10;
    int        *fill_data = NULL;
    int        compress  = 0;
    herr_t     status;

    /* Initialize the field field_type */
    if ((string_type = H5Tcopy(H5T_C_S1)) < 0)
        return FAIL;
    if (H5Tset_size(string_type, (size_t)16) < 0)
        return FAIL;
    field_type[0] = string_type;
    field_type[1] = H5T_NATIVE_INT;
    field_type[2] = H5T_NATIVE_INT;
    field_type[3] = H5T_NATIVE_FLOAT;
    field_type[4] = H5T_NATIVE_DOUBLE;


  /*------------------------------------------------------------------------
  * H5TBmake_table
  *-------------------------------------------------------------------------
  */

  status=H5TBmake_table( "Table Title", fid, H5TB_TABLE_NAME, (hsize_t)NFIELDS,
                        (hsize_t)NRECORDS, sizeof(particle_t),
                        field_names, part_offset, field_type,
                        chunk_size, fill_data, compress, testPart  );

  if (H5Tclose(string_type) < 0)
    return FAIL;

  if(status<0)
    return FAIL;
  else
    return SUCCEED;
}


/*-------------------------------------------------------------------------
 * test_create_close
 *
 * Tests creation and closing of an FL packet table
 *
 *-------------------------------------------------------------------------
 */

static int test_create_close(hid_t fid)
{
    herr_t err;
    hid_t table;
    hid_t part_t;

    TESTING("H5PTcreate_fl and H5PTclose");

    /* Create a datatype for the particle struct */
    part_t = make_particle_type();

    HDassert(part_t != -1);

    /* Create the table */
    table = H5PTcreate_fl(fid, PT_NAME, part_t, (hsize_t)100, -1);
    if (H5Tclose(part_t) < 0)
	goto error;
    if( H5PTis_valid(table) < 0)
	goto error;
    if( H5PTis_varlen(table) != 0)
	goto error;

    /* Close the table */
    err = H5PTclose(table);
    if( err < 0)
        goto error;

    PASSED();
    return SUCCEED;

error:
      H5_FAILED();
      return FAIL;
}

/*-------------------------------------------------------------------------
 * test_open
 *
 * Tests opening and closing a FL packet table
 *
 *-------------------------------------------------------------------------
 */
static int test_open(hid_t fid)
{
    herr_t err;
    hid_t table;

    TESTING("H5PTopen");

    /* Open the table */
    table = H5PTopen(fid, PT_NAME);
    if( H5PTis_valid(table) < 0)
        goto error;
    if( H5PTis_varlen(table) != 0)
        goto error;

    /* Close the table */
    err = H5PTclose(table);
    if( err < 0)
        goto error;

    PASSED();
    return SUCCEED;

error:
      if (table > 0) H5PTclose(table);
      H5_FAILED();
      return FAIL;
}

/*-------------------------------------------------------------------------
 * test_append
 *
 * Tests appending packets to a FL packet table
 *
 *-------------------------------------------------------------------------
 */
static int test_append(hid_t fid)
{
    herr_t err;
    hid_t table;
    hsize_t count = 0;

    TESTING("H5PTappend");

    /* Open the table */
    table = H5PTopen(fid, PT_NAME);
    if( H5PTis_valid(table) < 0)
        goto error;

    /* Count the number of packets in the table  */
    err = H5PTget_num_packets(table, &count);
    if( err < 0)
        goto error;
    /* There should be 0 records in the table */
    if( count != 0 )
        goto error;

    /* Append one particle */
    err = H5PTappend(table, (size_t)1, &(testPart[0]));
    if( err < 0)
        goto error;

    /* Append several particles */
    err = H5PTappend(table, (size_t)6, &(testPart[1]));
    if( err < 0)
        goto error;

    /* Append one more particle */
    err = H5PTappend(table, (size_t)1, &(testPart[7]));
    if( err < 0)
        goto error;

    /* Count the number of packets in the table  */
    err = H5PTget_num_packets(table, &count);
    if( err < 0)
        goto error;
    /* There should be 8 records in the table now */
    if( count != 8 )
        goto error;

    /* Close the table */
    err = H5PTclose(table);
    if( err < 0)
        goto error;

    PASSED();
    return SUCCEED;

error:
        H5_FAILED();
        if( H5PTis_valid(table) > 0)
            H5PTclose(table);
        return FAIL;
}

/*-------------------------------------------------------------------------
 * test_read
 *
 * Tests that the packets appended by test_append can be read back.
 *
 *-------------------------------------------------------------------------
 */
static int test_read(hid_t fid)
{
    herr_t err;
    hid_t table;
    particle_t readBuf[NRECORDS];
    size_t c;

    TESTING("H5PTread_packets");

    /* Open the table */
    table = H5PTopen(fid, PT_NAME);
    if( H5PTis_valid(table) < 0)
        goto error;

    /* Read several particles */
    err = H5PTread_packets(table, (hsize_t)0, 3, &(readBuf[0]));
    if( err < 0)
        goto error;

    /* Read one particle */
    err = H5PTread_packets(table, (hsize_t)3, 1, &(readBuf[3]));
    if( err < 0)
        goto error;

    /* Read several particles */
    err = H5PTread_packets(table, (hsize_t)4, (NRECORDS - 4 ), &(readBuf[4]));
    if( err < 0)
        goto error;

    /* Ensure that particles were read correctly */
    for(c=0; c<NRECORDS; c++)
    {
        if( cmp_par(c%8, c, testPart, readBuf) != 0)
            goto error;
    }

    /* Close the table */
    err = H5PTclose(table);
    if( err < 0)
        goto error;

    PASSED();

    return SUCCEED;

error:
        H5_FAILED();
        if( H5PTis_valid(table) > 0)
            H5PTclose(table);
        return FAIL;
}

/*-------------------------------------------------------------------------
 * test_get_next
 *
 * Tests the packets written by test_append can be read by
 * H5PTget_next().
 *
 *-------------------------------------------------------------------------
 */
static int test_get_next(hid_t fid)
{
    herr_t err;
    hid_t table;
    particle_t readBuf[NRECORDS];
    particle_t readBuf2[NRECORDS];
    size_t c;

    TESTING("H5PTget_next");

    /* Open the table */
    table = H5PTopen(fid, PT_NAME);
    if( H5PTis_valid(table) < 0)
        goto error;

    /* Read several particles consecutively */
    for(c=0; c < NRECORDS; c++)
    {
        err = H5PTget_next(table, (size_t)1, &readBuf[c]);
        if(err < 0)
            goto error;
    }

    /* Ensure that particles were read correctly */
    for(c=0; c<NRECORDS; c++)
    {
        if( cmp_par(c, c, testPart, readBuf) != 0)
            goto error;
    }

    H5PTcreate_index(table);

    /* Read particles two by two */
    for(c=0; c < NRECORDS / 2; c++)
    {
        err = H5PTget_next(table, (size_t)2, &readBuf2[c * 2]);
        if(err < 0)
            goto error;
    }

    /* Ensure that particles were read correctly */
    for(c=0; c<NRECORDS; c++)
    {
        if( cmp_par(c, c, testPart, readBuf2) != 0)
            goto error;
    }

    /* Close the table */
    err = H5PTclose(table);
    if( err < 0)
        goto error;

    PASSED();
    return SUCCEED;

error:
        H5_FAILED();
        if( H5PTis_valid(table) > 0)
            H5PTclose(table);
        return FAIL;
}

/*-------------------------------------------------------------------------
 * test_big_table
 *
 * Ensures that a FL packet table will not break when many (BIG_TABLE_SIZE)
 * packets are used.
 *
 *-------------------------------------------------------------------------
 */
static int    test_big_table(hid_t fid)
{
    herr_t err;
    hid_t table;
    hid_t part_t;
    size_t c;
    particle_t readPart;
    hsize_t count;

    TESTING("large packet table");

    /* Create a datatype for the particle struct */
    part_t = make_particle_type();

    HDassert(part_t != -1);

    /* Create a new table */
    table = H5PTcreate_fl(fid, "Packet Test Dataset2", part_t, (hsize_t)33, -1);
    if (H5Tclose(part_t) < 0)
        goto error;
    if( H5PTis_valid(table) < 0)
        goto error;

        /* Add many particles */
    for(c = 0; c < BIG_TABLE_SIZE ; c+=8)
    {
        /* Append eight particles at once*/
        err = H5PTappend(table, (size_t)8, &(testPart[0]));
        if( err < 0)
            goto error;
    }

    /* Count the number of packets in the table  */
    err = H5PTget_num_packets(table, &count);
    if( err < 0)
        goto error;
    if( count != BIG_TABLE_SIZE )
        goto error;

    /* Read particles to ensure that all of them were written correctly  */
    /* Also, ensure that H5PTcreate_fl set the current packet to */
    /* the first packet in the table                                     */
    for(c = 0; c < BIG_TABLE_SIZE; c++)
    {
        err = H5PTget_next(table, (size_t)1, &readPart);
        if(err < 0)
            goto error;

        /* Ensure that particles were read correctly */
        if( cmp_par(c % 8, 0, testPart, &readPart) != 0)
            goto error;
    }

    /* Close the table */
    err = H5PTclose(table);
    if( err < 0)
        goto error;

    PASSED();
    return SUCCEED;

error:
        H5_FAILED();
        if( H5PTis_valid(table) > 0)
            H5PTclose(table);
        return FAIL;
}

/*-------------------------------------------------------------------------
 * test_opaque
 *
 * Tests that the packet table works with an opaque datatype.
 *
 *-------------------------------------------------------------------------
 */
static int    test_opaque(hid_t fid)
{
    herr_t err;
    hid_t table;
    hid_t part_t;
    size_t c;
    particle_t readBuf[NRECORDS];

    TESTING("opaque data");

    /* Create an opaque datatype for the particle struct */
    if ((part_t = H5Tcreate (H5T_OPAQUE, sizeof(particle_t) )) < 0 )
        return FAIL;

    HDassert(part_t != -1);

    /* Tag the opaque datatype */
    if ( H5Tset_tag(part_t,  "Opaque Particle"  ) < 0)
        return FAIL;

    /* Create a new table */
    table = H5PTcreate_fl(fid, "Packet Test Dataset3", part_t, (hsize_t)100, -1);
    if( H5Tclose(part_t) < 0)
        goto error;
    if( H5PTis_valid(table) < 0)
        goto error;

    /* Append several particles, starting at particle 1 */
    err = H5PTappend(table, (size_t)(NRECORDS - 1), &(testPart[1]));
    if( err < 0)
        goto error;

    /* Read the particles back */
    err = H5PTread_packets(table, (hsize_t)0, 7, &(readBuf[0]));
    if( err < 0)
        goto error;

    /* Ensure that particles were read correctly */
    for(c=0; c<NRECORDS - 1; c++)
    {
        if( cmp_par(c+1, c, testPart, readBuf) != 0)
            goto error;
    }

    /* Close the table */
    err = H5PTclose(table);
    if( err < 0)
        goto error;

    PASSED();
    return SUCCEED;

error:
        H5_FAILED();
        if( H5PTis_valid(table) > 0)
            H5PTclose(table);
        return FAIL;
}

/*-------------------------------------------------------------------------
 * test_compress
 *
 * Ensures that a FL packet table can be compressed.
 * This test creates a file named TEST_COMPRESS_FILE
 *
 *-------------------------------------------------------------------------
 */
static int
test_compress(void)
{
    hid_t fid1 = H5I_INVALID_HID;
    herr_t err;
    hid_t table = H5I_INVALID_HID;
    hid_t part_t = H5I_INVALID_HID;
    hid_t dset_id = H5I_INVALID_HID;
    hid_t plist_id = H5I_INVALID_HID;
    size_t c;
    size_t num_elems = 1;
    unsigned filter_vals[1];
    particle_t readPart[1];
    hsize_t count;

    TESTING("packet table compression");

    /* Create a file. */
    if((fid1 = H5Fcreate(TEST_COMPRESS_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Create a datatype for the particle struct */
    part_t = make_particle_type();

    HDassert(part_t != -1);

    /* Create a new table with compression level 8 */
    table = H5PTcreate_fl(fid1, "Compressed Test Dataset", part_t, (hsize_t)80, 8);
    if( H5PTis_valid(table) < 0) TEST_ERROR;

    /* We can now use this table exactly the same way we use a normal uncompressed
     * packet table, and it should pass the same tests. */
        /* Add many particles */
    for(c = 0; c < BIG_TABLE_SIZE ; c+=8)
    {
        /* Append eight particles at once*/
        err = H5PTappend(table, (size_t)8, &(testPart[0]));
        if( err < 0) TEST_ERROR;
    }

    /* Count the number of packets in the table  */
    err = H5PTget_num_packets(table, &count);
    if( err < 0) TEST_ERROR;
    if( count != BIG_TABLE_SIZE ) TEST_ERROR;

    /* Read particles to ensure that all of them were written correctly  */
    HDmemset(readPart, 0, sizeof(readPart));
    for(c = 0; c < BIG_TABLE_SIZE; c++)
    {
        err = H5PTget_next(table, (size_t)1, readPart);
        if(err < 0) TEST_ERROR;

        /* Ensure that particles were read correctly */
        if( cmp_par(c % 8, 0, testPart, readPart) != 0) TEST_ERROR;
    }

    /* Close the table */
    err = H5PTclose(table);
    if( err < 0) TEST_ERROR;

    /* Open the packet table as a regular dataset and make sure that the
     * compression filter is set.
     */
    dset_id = H5Dopen2(fid1, "Compressed Test Dataset", H5P_DEFAULT);
    if( dset_id < 0) TEST_ERROR;

    plist_id = H5Dget_create_plist(dset_id);
    if( plist_id < 0) TEST_ERROR;

    err = H5Pget_filter_by_id2(plist_id, H5Z_FILTER_DEFLATE, NULL, &num_elems,
                      filter_vals, 0, NULL, NULL);
    if( err < 0) TEST_ERROR;

    /* The compression level should be 8, the value we passed in */
    if(filter_vals[0] != 8) TEST_ERROR;

    /* Clean up */
    err = H5Pclose(plist_id);
    if( err < 0) TEST_ERROR;
    err = H5Dclose(dset_id);
    if( err < 0) TEST_ERROR;

    /* Create a new table without compression. */
    table = H5PTcreate_fl(fid1, "Uncompressed Dataset", part_t, (hsize_t)80, -1);
    if(table < 0) TEST_ERROR;

    /* Close the packet table */
    err = H5PTclose(table);
    if( err < 0) TEST_ERROR;

    /* Open the packet table as a regular dataset and make sure that the
     * compression filter is not set.
     */
    dset_id = H5Dopen2(fid1, "Uncompressed Dataset", H5P_DEFAULT);
    if( dset_id < 0) TEST_ERROR;

    plist_id = H5Dget_create_plist(dset_id);
    if( plist_id < 0) TEST_ERROR;

    H5E_BEGIN_TRY {
    err = H5Pget_filter_by_id2(plist_id, H5Z_FILTER_DEFLATE, NULL, &num_elems,
                      filter_vals, 0, NULL, NULL);
    if( err >= 0) TEST_ERROR;
    } H5E_END_TRY

    /* Clean up */
    err = H5Pclose(plist_id);
    if( err < 0) TEST_ERROR;
    err = H5Dclose(dset_id);
    if( err < 0) TEST_ERROR;

    /* Close the datatype and the file */
    err = H5Tclose(part_t);
    if( err < 0) TEST_ERROR;
    err = H5Fclose(fid1);
    if( err < 0) TEST_ERROR;

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
       H5Pclose(plist_id);
       H5Dclose(dset_id);
       H5Tclose(part_t);
       H5PTclose(table);
       H5Fclose(fid1);
    } H5E_END_TRY
    H5_FAILED();
    return FAIL;
}

/*-------------------------------------------------------------------------
 * test_rw_non-native_dt
 *
 * test reading and writing packet table using datatypes that are not
 * native.
 *
 *-------------------------------------------------------------------------
 */
static int test_rw_nonnative_dt(hid_t fid)
{
 hid_t          ptable;     /* Packet table identifier */

 herr_t         err;        /* Function return status */
 hsize_t        count;      /* Number of records in the table */

 int            x;          /* Loop variable */

 /* Buffers to hold data */
 int writeBuffer[5];
 int readBuffer[5];

 TESTING("reading/writing non-native packet table");

 /* Initialize buffers */
 for(x=0; x<5; x++) {
   writeBuffer[x]=x;
   readBuffer[x] = -1;
 }

 /* Create a fixed-length packet table within the file */
 /* This table's "packets" will be simple integers and it will use no compression */
 if(H5Tget_order(H5T_NATIVE_INT) == H5T_ORDER_LE) {
   ptable = H5PTcreate(fid, "Packet Test Dataset, Non-native", H5T_STD_I32BE, (hsize_t)100, H5P_DEFAULT);
 } else {
   ptable = H5PTcreate(fid, "Packet Test Dataset, Non-native", H5T_STD_I32LE, (hsize_t)100, H5P_DEFAULT);
 }
 if(ptable == H5I_INVALID_HID)
        goto error;

 /* Write one packet to the packet table */
 if( (err = H5PTappend(ptable, (size_t)1, &(writeBuffer[0]))) < 0 )
        goto error;

 /* Write several packets to the packet table */
 if( (err = H5PTappend(ptable, (size_t)4, &(writeBuffer[1]))) < 0)
        goto error;

 if( (err = H5PTclose(ptable)) < 0)
        goto error;

 /* Open the Packet table */
 if( (ptable = H5PTopen(fid, "Packet Test Dataset, Non-native")) < 0)
        goto error;

 /* Get the number of packets in the packet table.  This should be five. */
 if( (err = H5PTget_num_packets(ptable, &count)) < 0)
        goto error;

 if( (int)count != 5 )
        goto error;

 /* Initialize packet table's "current record" */
 if( (err = H5PTcreate_index(ptable)) < 0)
        goto error;

 /* Iterate through packets, read each one back */
 for(x=0; x<5; x++) {
   if( (err = H5PTget_next(ptable, (size_t)1, &(readBuffer[x]))) < 0)
        goto error;
   if( x != readBuffer[x])
        goto error;
 }

 /* Close the packet table */
 if( (err = H5PTclose(ptable)) < 0)
        goto error;
 
 PASSED();
 return SUCCEED;

error:
     H5_FAILED();
     if( H5PTis_valid(ptable) > 0)
       H5PTclose(ptable);
     return FAIL;
}

/*-------------------------------------------------------------------------
 * test_error
 *
 * ensures that the packet table API throws the correct errors used on
 * objects that are not packet tables.
 *
 *-------------------------------------------------------------------------
 */
static int test_error(hid_t fid)
{
  hid_t id = H5I_INVALID_HID;
  int id_open=0;
  particle_t readBuf[1];

  TESTING("error conditions");

  /* Create a HL table */
  if(create_hl_table(fid) < 0)
        goto error;

  /* Try to open things that are not packet tables */
  H5E_BEGIN_TRY
  if(H5PTopen(fid, "Bogus_name") >= 0)
        goto error;
  if(H5PTopen(fid, "group1") >= 0)
        goto error;
  H5E_END_TRY

  /* Try to execute packet table commands on an invalid ID */
  H5E_BEGIN_TRY
  if(H5PTis_valid(id) >= 0)
        goto error;
  if(H5PTis_varlen(id) >= 0)
        goto error;
  if(H5PTclose(id) >= 0)
        goto error;
  if(H5PTappend(id, (size_t)1, testPart) >= 0)
        goto error;
  if(H5PTread_packets(id, (hsize_t)0, 1, readBuf) >= 0)
        goto error;
  if(H5PTcreate_index(id) >= 0)
        goto error;
  if(H5PTset_index(id, (hsize_t)1) >= 0)
        goto error;
  if(H5PTget_index(id, NULL) >= 0)
        goto error;
  H5E_END_TRY

  /* Open a high-level non-packet (H5TB) table and try to */
  /* execute commands on it. */
  if((id=H5Dopen2(fid, H5TB_TABLE_NAME, H5P_DEFAULT)) <0)
        goto error;
  id_open = 1;

  H5E_BEGIN_TRY
  if(H5PTis_valid(id) >= 0)
        goto error;
  if(H5PTis_varlen(id) >= 0)
        goto error;
  if(H5PTclose(id) >= 0)
        goto error;
  if(H5PTappend(id, (size_t)1, testPart) >= 0)
        goto error;
  if(H5PTread_packets(id, (hsize_t)0, 1, readBuf) >= 0)
        goto error;
  if(H5PTcreate_index(id) >= 0)
        goto error;
  if(H5PTset_index(id, (hsize_t)1) >= 0)
        goto error;
  if(H5PTget_index(id, NULL) >= 0)
        goto error;
  H5E_END_TRY

  id_open=0;
  if(H5Dclose(id) <0)
        goto error;

  /* Open and close a packet table.  Try to execute */
  /* commands on the closed ID. */
  if((id=H5PTopen(fid, PT_NAME))<0)
        goto error;
  if(H5PTclose(id) <0)
        goto error;

  H5E_BEGIN_TRY
  if(H5PTis_valid(id) >= 0)
        goto error;
  if(H5PTis_varlen(id) >= 0)
        goto error;
  if(H5PTclose(id) >= 0)
        goto error;
  if(H5PTappend(id, (size_t)1, testPart) >= 0)
        goto error;
  if(H5PTread_packets(id, (hsize_t)0, 1, readBuf) >= 0)
        goto error;
  if(H5PTcreate_index(id) >= 0)
        goto error;
  if(H5PTset_index(id, (hsize_t)1) >= 0)
        goto error;
  if(H5PTget_index(id, NULL) >= 0)
        goto error;
  H5E_END_TRY

  PASSED();
  return SUCCEED;

error:
  H5_FAILED();
  if(id_open)
    H5Dclose(id);
  return FAIL;
}

/*-------------------------------------------------------------------------
 * test_packet_table(): Invokes individual tests to ensure that all
 *      functions work correctly, except for the variable-length related
 *      functions, which are tested in "test_packet_vlen.c".
 *
 *-------------------------------------------------------------------------
 */
static int test_packet_table(hid_t fid)
{

    if( test_create_close(fid) < 0 )
        return FAIL;

    if( test_open(fid) < 0 )
        return FAIL;

    /* test_append must be run before test_count and test_read, as it */
    /* creates the packet table they use. */
    if( test_append(fid) < 0 )
        return FAIL;

    /* These tests will not necessarily cause failures in each other,
           so we don't abort the other tests if one fails. */
    test_read(fid);
    test_get_next(fid);
    test_big_table(fid);
    test_rw_nonnative_dt(fid);
    test_opaque(fid);
    test_compress();
    test_error(fid);

    return SUCCEED;
}

/*
 *
*/
int main(void)
{
    /* identifier for the file that is used in FL PT tests */
    hid_t       fid;
    int         status = 0;

/*-------------------------------------------------------------------------
 * Packet test: test each function of the packet table library
 *-------------------------------------------------------------------------
 */

 /* create a file using default properties */
 fid=H5Fcreate(TEST_FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

 HDputs("Testing packet table");

    /* Test packet table with fixed length */
    if (test_packet_table(fid) < 0)
        status = 1;

    /* Test packet table with variable length, using separate data file */
    if (test_packet_table_with_varlen() < 0)
        status = 1;

    /* Close the file */
    if (H5Fclose(fid) < 0)
	status = 1;

    return status;
}
