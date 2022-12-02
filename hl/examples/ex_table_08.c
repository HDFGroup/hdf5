/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "hdf5_hl.h"
#include <stdlib.h>

/*-------------------------------------------------------------------------
 * Table API example
 *
 * H5TBinsert_record
 *
 *-------------------------------------------------------------------------
 */

#define NFIELDS      (hsize_t)5
#define NRECORDS     (hsize_t)8
#define NRECORDS_INS (hsize_t)2
#define TABLE_NAME   "table"

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

    Particle dst_buf[NRECORDS + NRECORDS_INS];

    /* Define an array of Particles */
    Particle p_data[NRECORDS] = {{"zero", 0, 0, 0.0F, 0.0},    {"one", 10, 10, 1.0F, 10.0},
                                 {"two", 20, 20, 2.0F, 20.0},  {"three", 30, 30, 3.0F, 30.0},
                                 {"four", 40, 40, 4.0F, 40.0}, {"five", 50, 50, 5.0F, 50.0},
                                 {"six", 60, 60, 6.0F, 60.0},  {"seven", 70, 70, 7.0F, 70.0}};

    /* Calculate the size and the offsets of our struct members in memory */
    size_t dst_size            = sizeof(Particle);
    size_t dst_offset[NFIELDS] = {HOFFSET(Particle, name), HOFFSET(Particle, lati), HOFFSET(Particle, longi),
                                  HOFFSET(Particle, pressure), HOFFSET(Particle, temperature)};
    size_t dst_sizes[NFIELDS]  = {sizeof(p_data[0].name), sizeof(p_data[0].lati), sizeof(p_data[0].longi),
                                 sizeof(p_data[0].pressure), sizeof(p_data[0].temperature)};

    /* Define an array of Particles to insert */
    Particle p_data_insert[NRECORDS_INS] = {{"new", 30, 30, 3.0F, 30.0}, {"new", 40, 40, 4.0F, 40.0}};

    /* Define field information */
    const char *field_names[NFIELDS] = {"Name", "Latitude", "Longitude", "Pressure", "Temperature"};
    hid_t       field_type[NFIELDS];
    hid_t       string_type;
    hid_t       file_id;
    hsize_t     chunk_size = 10;
    int         compress   = 0;
    int        *fill_data  = NULL;
    hsize_t     start;    /* Record to start reading */
    hsize_t     nrecords; /* Number of records to insert/delete */
    hsize_t     nfields_out;
    hsize_t     nrecords_out;
    int         i;

    /* Initialize the field field_type */
    string_type = H5Tcopy(H5T_C_S1);
    H5Tset_size(string_type, 16);
    field_type[0] = string_type;
    field_type[1] = H5T_NATIVE_INT;
    field_type[2] = H5T_NATIVE_INT;
    field_type[3] = H5T_NATIVE_FLOAT;
    field_type[4] = H5T_NATIVE_DOUBLE;

    /* Create a new file using default properties. */
    file_id = H5Fcreate("ex_table_08.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Make the table */
    H5TBmake_table("Table Title", file_id, TABLE_NAME, NFIELDS, NRECORDS, dst_size, field_names, dst_offset,
                   field_type, chunk_size, fill_data, compress, p_data);

    /* Insert records */
    start    = 3;
    nrecords = NRECORDS_INS;
    H5TBinsert_record(file_id, TABLE_NAME, start, nrecords, dst_size, dst_offset, dst_sizes, p_data_insert);

    /* read the table */
    H5TBread_table(file_id, TABLE_NAME, dst_size, dst_offset, dst_sizes, dst_buf);

    /* get table info  */
    H5TBget_table_info(file_id, TABLE_NAME, &nfields_out, &nrecords_out);

    /* print */
    printf("Table has %d fields and %d records\n", (int)nfields_out, (int)nrecords_out);

    /* print it by rows */
    for (i = 0; i < nrecords_out; i++) {
        printf("%-5s %-5d %-5d %-5f %-5f", dst_buf[i].name, dst_buf[i].lati, dst_buf[i].longi,
               dst_buf[i].pressure, dst_buf[i].temperature);
        printf("\n");
    }

    /* close type */
    H5Tclose(string_type);

    /* close the file */
    H5Fclose(file_id);

    return 0;
}
