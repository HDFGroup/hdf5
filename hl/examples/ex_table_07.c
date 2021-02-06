/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "hdf5.h"
#include "hdf5_hl.h"
#include <stdlib.h>

/*-------------------------------------------------------------------------
 * Table API example
 *
 * H5TBdelete_record
 *
 *-------------------------------------------------------------------------
 */

#define NFIELDS    (hsize_t)5
#define NRECORDS   (hsize_t)8
#define TABLE_NAME "table"

int
main(void)
{
    typedef struct Particle {
        char   name[16];
        int    lati;
        int    longi;
        float  pressure;
        double temperature;
    } Particle;

    /* Calculate the size and the offsets of our struct members in memory */
    size_t dst_size            = sizeof(Particle);
    size_t dst_offset[NFIELDS] = {HOFFSET(Particle, name), HOFFSET(Particle, lati), HOFFSET(Particle, longi),
                                  HOFFSET(Particle, pressure), HOFFSET(Particle, temperature)};

    /* Define an array of Particles */
    Particle p_data[NRECORDS] = {{"zero", 0, 0, 0.0f, 0.0},    {"one", 10, 10, 1.0f, 10.0},
                                 {"two", 20, 20, 2.0f, 20.0},  {"three", 30, 30, 3.0f, 30.0},
                                 {"four", 40, 40, 4.0f, 40.0}, {"five", 50, 50, 5.0f, 50.0},
                                 {"six", 60, 60, 6.0f, 60.0},  {"seven", 70, 70, 7.0f, 70.0}};

    const char *field_names[NFIELDS] = /* Define field information */
        {"Name", "Latitude", "Longitude", "Pressure", "Temperature"};
    hid_t    field_type[NFIELDS];
    hid_t    string_type;
    hid_t    file_id;
    hsize_t  chunk_size   = 10;
    int      compress     = 0;
    Particle fill_data[1] = {{"no data", -1, -1, -99.0f, -99.0}};
    hsize_t  start;    /* Record to start reading */
    hsize_t  nrecords; /* Number of records to insert/delete */
    hsize_t  nfields_out;
    hsize_t  nrecords_out;

    /* Initialize the field field_type */
    string_type = H5Tcopy(H5T_C_S1);
    H5Tset_size(string_type, 16);
    field_type[0] = string_type;
    field_type[1] = H5T_NATIVE_INT;
    field_type[2] = H5T_NATIVE_INT;
    field_type[3] = H5T_NATIVE_FLOAT;
    field_type[4] = H5T_NATIVE_DOUBLE;

    /* Create a new file using default properties. */
    file_id = H5Fcreate("ex_table_07.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Make the table */
    H5TBmake_table("Table Title", file_id, TABLE_NAME, NFIELDS, NRECORDS, dst_size, field_names, dst_offset,
                   field_type, chunk_size, fill_data, compress, p_data);

    /* Delete records  */
    start    = 3;
    nrecords = 3;
    H5TBdelete_record(file_id, TABLE_NAME, start, nrecords);

    /* Get table info  */
    H5TBget_table_info(file_id, TABLE_NAME, &nfields_out, &nrecords_out);

    /* print */
    printf("Table has %d fields and %d records\n", (int)nfields_out, (int)nrecords_out);

    /* close type */
    H5Tclose(string_type);

    /* close the file */
    H5Fclose(file_id);

    return 0;
}
