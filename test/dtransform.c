#include "h5test.h"

#define ROWS    12
#define COLS    18
#define TOL     2

int compare(int* a, int* b);

int main()
{
    
    hid_t file_id, dxpl_id_f_to_c, dxpl_id_c_to_f, dset_id, datatype, dataspace;
    hsize_t dim[2] = {ROWS, COLS};
    char* f_to_c = "(5/9.0)*(x-32)";
    char* c_to_f = "(9/5.0)*x + 32";
    int windchillF[ROWS][COLS] =
    {   {36, 31, 25, 19, 13, 7, 1, -5, -11, -16, -22, -28, -34, -40, -46, -52, -57, -63 }, 
        {34, 27, 21, 15, 9, 3, -4, -10, -16, -22, -28, -35, -41, -47, -53, -59, -66, -72 } , 
        {32, 25, 19, 13, 6, 0, -7, -13, -19, -26, -32, -39, -45, -51, -58, -64, -71, -77 }, 
        {30, 24, 17, 11, 4, -2, -9, -15, -22, -29, -35, -42, -48, -55, -61, -68, -74, -81 }, 
        {29, 23, 16, 9, 3, -4, -11, -17, -24, -31, -37, -44, -51, -58, -64, -71, -78, -84 }, 
        {28, 22, 15, 8, 1, -5, -12, -19, -26, -33, -39, -46, -53, -60, -67, -73, -80, -87 }, 
        {28, 21, 14, 7, 0, -7, -14, -21, -27, -34, -41, -48, -55, -62, -69, -76, -82, -89 }, 
        {27, 20, 13, 6, -1, -8, -15, -22, -29, -36, -43, -50, -57, -64, -71, -78, -84, -91 }, 
        {26, 19, 12, 5, -2, -9, -16, -23, -30, -37, -44, -51, -58, -65, -72, -79, -86, -93 },
        {26, 19, 12, 4, -3, -10, -17, -24, -31, -38, -45, -52, -60, -67, -74, -81, -88, -95},
        {25, 18, 11, 4, -3, -11, -18, -25, -32, -39, -46, -54, -61, -68, -75, -82, -89, -97}, 
        {25, 17, 10, 3, -4, -11, -19, -26, -33, -40, -48, -55, -62, -69, -76, -84, -91, -98} 
    };
    int windchillCread[ROWS][COLS];
    int windchillFread[ROWS][COLS];
    int windchillCcalc[ROWS][COLS];
    herr_t err;
    int row, col;


    TESTING("data transform");
    
    dxpl_id_f_to_c = H5Pcreate(H5P_DATASET_XFER);
    dxpl_id_c_to_f = H5Pcreate(H5P_DATASET_XFER);

    err= H5Pset_data_transform(dxpl_id_f_to_c, f_to_c);
    err= H5Pset_data_transform(dxpl_id_c_to_f, c_to_f);


    file_id = H5Fcreate("dtransform.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    dataspace = H5Screate_simple(2, dim, NULL); 

    datatype = H5Tcopy(H5T_NATIVE_INT);

    dset_id = H5Dcreate(file_id, "/transformtest", datatype, dataspace, H5P_DEFAULT);

    err = H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl_id_f_to_c, windchillF);
    err = H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, windchillCread);

    for(row = 0; row<ROWS; row++)
    {
        for(col = 0; col<COLS; col++)
            windchillCcalc[row][col] = (5/9.0)*(windchillF[row][col] - 32);
    }
    
    if( (compare((int*)windchillCread, (int*)windchillCcalc)) == 0)
    {
        fprintf(stderr, "ERROR: Conversion failed to match computed data\n");
        TEST_ERROR;
    }
    else
    {
        err = H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl_id_c_to_f, windchillFread);
        if( (compare((int *)windchillF, (int *)windchillFread)) == 0)
        {
            fprintf(stderr, "ERROR: Conversion failed to match computed data\n");
            TEST_ERROR;
        }
    } 
    

    /* Close the objects we opened/created */
   err = H5Sclose(dataspace);
   err = H5Tclose(datatype); 
   err = H5Dclose(dset_id);
   err = H5Fclose(file_id);
   err = H5Pclose(dxpl_id_f_to_c);
   err = H5Pclose(dxpl_id_c_to_f);

   PASSED();
   
   return 0;

error:
   return -1;
}

int compare(int* a, int* b)
{
    int row, col;
    for(row = 0; row<ROWS; row++)
    {
        for(col = 0; col<COLS; col++)
        {
            if( !((a[row*COLS + col] <= (b[row*COLS + col] + TOL)) && (a[row*COLS + col] >= (b[row*COLS + col] - TOL))) )
                return 0;
        }
    }
    return 1;
}
    


