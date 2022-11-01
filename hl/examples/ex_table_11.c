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
 * H5TBinsert_field
 *
 *-------------------------------------------------------------------------
 */

#define NFIELDS    (hsize_t)5
#define NRECORDS   (hsize_t)8
#define TABLE_NAME "table"

int
main(void)
{
    typedef struct Particle1 {
        char   name[16];
        int    lati;
        int    longi;
        float  pressure;
        double temperature;
    } Particle1;

    /* Define an array of Particles */
    Particle1 p_data[NRECORDS] = {{"zero", 0, 0, 0.0F, 0.0},    {"one", 10, 10, 1.0F, 10.0},
                                  {"two", 20, 20, 2.0F, 20.0},  {"three", 30, 30, 3.0F, 30.0},
                                  {"four", 40, 40, 4.0F, 40.0}, {"five", 50, 50, 5.0F, 50.0},
                                  {"six", 60, 60, 6.0F, 60.0},  {"seven", 70, 70, 7.0F, 70.0}};

    /* Calculate the size and the offsets of our struct members in memory */
    size_t dst_size1            = sizeof(Particle1);
    size_t dst_offset1[NFIELDS] = {HOFFSET(Particle1, name), HOFFSET(Particle1, lati),
                                   HOFFSET(Particle1, longi), HOFFSET(Particle1, pressure),
                                   HOFFSET(Particle1, temperature)};

    /* Define field information */
    const char *field_names[NFIELDS] = {"Name", "Latitude", "Longitude", "Pressure", "Temperature"};
    hid_t       field_type[NFIELDS];
    hid_t       string_type;
    hid_t       file_id;
    hsize_t     chunk_size       = 10;
    int         compress         = 0;
    Particle1   fill_data[1]     = {{"no data", -1, -1, -99.0F, -99.0}};
    int         fill_data_new[1] = {-100};
    hsize_t     position;
    hsize_t     nfields_out;
    hsize_t     nrecords_out;

    /* Define the inserted field information */
    hid_t field_type_new = H5T_NATIVE_INT;
    int   data[NRECORDS] = {0, 1, 2, 3, 4, 5, 6, 7};

    /* Initialize the field type */
    string_type = H5Tcopy(H5T_C_S1);
    H5Tset_size(string_type, 16);
    field_type[0] = string_type;
    field_type[1] = H5T_NATIVE_INT;
    field_type[2] = H5T_NATIVE_INT;
    field_type[3] = H5T_NATIVE_FLOAT;
    field_type[4] = H5T_NATIVE_DOUBLE;

    /* Create a new file using default properties. */
    file_id = H5Fcreate("ex_table_11.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Make the table */
    H5TBmake_table("Table Title", file_id, TABLE_NAME, NFIELDS, NRECORDS, dst_size1, field_names, dst_offset1,
                   field_type, chunk_size, fill_data, compress, p_data);

    /* Insert the new field at the end of the field list */
    position = NFIELDS;
    H5TBinsert_field(file_id, TABLE_NAME, "New Field", field_type_new, position, fill_data_new, data);

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
