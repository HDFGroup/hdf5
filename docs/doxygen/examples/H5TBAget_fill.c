unsigned char tmp_fill_buf[40];
...

    file_id = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
dataset_id  = H5Dopen(file_id, TABLE_NAME, H5P_DEFAULT);
datatype_id = H5Dget_type(dataset_id);

status = H5TBget_table_info(file_id, TABLE_NAME, &nfields, &nrecords);

hasfill = H5TBAget_fill(file_id, TABLE_NAME, dataset_id, tmp_fill_buf);

for (i = 0; i < nfields; i++) {
    member_type_id     = H5Tget_member_type(datatype_id, (unsigned)i);
    native_mem_type_id = H5Tget_native_type(member_type_id, H5T_DIR_ASCEND);
    member_offset      = H5Tget_member_offset(datatype_id, (unsigned)i);
    printf("member_offset: %i\n", member_offset);
    memb_class = H5Tget_class(member_type_id);
    switch (memb_class) {
        case H5T_INTEGER:
            /* convert unsigned char array to integer */
            break;
        case H5T_FLOAT:
            /* convert unsigned char array to double or float */

            if (H5Tequal(native_mem_type_id, H5T_NATIVE_DOUBLE)) {
            }
            else if (H5Tequal(native_mem_type_id, H5T_NATIVE_FLOAT)) {
                f.i = tmp_fill_buf[member_offset] | (tmp_fill_buf[member_offset + 1] << 8) |
                      (tmp_fill_buf[member_offset + 2] << 16) | (tmp_fill_buf[member_offset + 3] << 24);
                printf("Field %i  Fill Value:  %lf\n", i, f.f);
            }
            break;

        case H5T_STRING:
            /* convert unsigned char array to string */
            strsize = H5Tget_size(member_type_id);

            printf("Field %i  Fill Value: ", i);
            for (j = 0; j < strsize; j++)
                printf("%c", tmp_fill_buf[member_offset + j]);
            printf("\n");
            break;
    }
