#include "h5test.h"

#define ROWS    12
#define COLS    18

int init_test(void);
int test_char(const hid_t dxpl_id_c_to_f);
int test_schar(const hid_t dxpl_id_c_to_f);
int test_uchar(const hid_t dxpl_id_utrans);
int test_ushort(const hid_t dxpl_id_utrans);
int test_short(const hid_t dxpl_id_c_to_f);
int test_uint(const hid_t dxpl_id_utrans);
int test_int(const hid_t dxpl_id_c_to_f, const hid_t dxpl_id_polynomial);
int test_ulong(const hid_t dxpl_id_utrans);
int test_long(const hid_t dxpl_id_c_to_f);
int test_llong(const hid_t dxpl_id_c_to_f);
int test_ullong(const hid_t dxpl_id_utrans);
int test_float(const hid_t dxpl_id_c_to_f, const hid_t dxpl_id_polynomial);
int test_ldouble(const hid_t dxpl_id_c_to_f);
int test_double(const hid_t dxpl_id_c_to_f);
int test_copy(const hid_t dxpl_id_c_to_f_copy, const hid_t dxpl_id_polynomial_copy);
int test_trivial(const hid_t dxpl_id_simple);

/* These are used everywhere and are init'ed in init_test() */
hid_t dset_id_int;
hid_t dset_id_float;
hid_t dset_id_char;
hid_t dset_id_uchar;
hid_t dset_id_schar;
hid_t dset_id_short;
hid_t dset_id_long;
hid_t dset_id_ulong;
hid_t dset_id_llong;
hid_t dset_id_ullong;
hid_t dset_id_uint;
hid_t dset_id_double;
hid_t dset_id_ldouble;
hid_t dset_id_ushort;

const float windchillFfloat[ROWS][COLS] = 
    {   {36.0, 31.0, 25.0, 19.0, 13.0, 7.0, 1.0, -5.0, -11.0, -16.0, -22.0, -28.0, -34.0, -40.0, -46.0, -52.0, -57.0, -63.0 },
	{34.0, 27.0, 21.0, 15.0, 9.0, 3.0, -4.0, -10.0, -16.0, -22.0, -28.0, -35.0, -41.0, -47.0, -53.0, -59.0, -66.0, -72.0 } ,  
	{32.0, 25.0, 19.0, 13.0, 6.0, 0.0, -7.0, -13.0, -19.0, -26.0, -32.0, -39.0, -45.0, -51.0, -58.0, -64.0, -71.0, -77.0 },  
	{30.0, 24.0, 17.0, 11.0, 4.0, -2.0, -9.0, -15.0, -22.0, -29.0, -35.0, -42.0, -48.0, -55.0, -61.0, -68.0, -74.0, -81.0 },  
	{29.0, 23.0, 16.0, 9.0, 3.0, -4.0, -11.0, -17.0, -24.0, -31.0, -37.0, -44.0, -51.0, -58.0, -64.0, -71.0, -78.0, -84.0 },  
	{28.0, 22.0, 15.0, 8.0, 1.0, -5.0, -12.0, -19.0, -26.0, -33.0, -39.0, -46.0, -53.0, -60.0, -67.0, -73.0, -80.0, -87.0 },  
	{28.0, 21.0, 14.0, 7.0, 0.0, -7.0, -14.0, -21.0, -27.0, -34.0, -41.0, -48.0, -55.0, -62.0, -69.0, -76.0, -82.0, -89.0 },  
	{27.0, 20.0, 13.0, 6.0, -1.0, -8.0, -15.0, -22.0, -29.0, -36.0, -43.0, -50.0, -57.0, -64.0, -71.0, -78.0, -84.0, -91.0 },  
	{26.0, 19.0, 12.0, 5.0, -2.0, -9.0, -16.0, -23.0, -30.0, -37.0, -44.0, -51.0, -58.0, -65.0, -72.0, -79.0, -86.0, -93.0 },
	{26.0, 19.0, 12.0, 4.0, -3.0, -10.0, -17.0, -24.0, -31.0, -38.0, -45.0, -52.0, -60.0, -67.0, -74.0, -81.0, -88.0, -95.0},
	{25.0, 18.0, 11.0, 4.0, -3.0, -11.0, -18.0, -25.0, -32.0, -39.0, -46.0, -54.0, -61.0, -68.0, -75.0, -82.0, -89.0, -97.0},  
	{25.0, 17.0, 10.0, 3.0, -4.0, -11.0, -19.0, -26.0, -33.0, -40.0, -48.0, -55.0, -62.0, -69.0, -76.0, -84.0, -91.0, -98.0} 
    };

const int transformData[ROWS][COLS] = 
    {   {36, 31, 25, 19, 13, 7, 1, 5, 11, 16, 22, 28, 34, 40, 46, 52, 57, 63 },
        {34, 27, 21, 15, 9, 3, 4, 10, 16, 22, 28, 35, 41, 47, 53, 59, 66, 72 } ,
        {32, 25, 19, 13, 6, 0, 7, 13, 19, 26, 32, 39, 45, 51, 58, 64, 71, 77 },
        {30, 24, 17, 11, 4, 2, 9, 15, 22, 29, 35, 42, 48, 55, 61, 68, 74, 81 },
        {29, 23, 16, 9, 3, 4, 11, 17, 24, 31, 37, 44, 51, 58, 64, 71, 78, 84 },
        {28, 22, 15, 8, 1, 5, 12, 19, 26, 33, 39, 46, 53, 60, 67, 73, 80, 87 },
        {28, 21, 14, 7, 0, 7, 14, 21, 27, 34, 41, 48, 55, 62, 69, 76, 82, 89 },
        {27, 20, 13, 6, 1, 8, 15, 22, 29, 36, 43, 50, 57, 64, 71, 78, 84, 91 },
        {26, 19, 12, 5, 2, 9, 16, 23, 30, 37, 44, 51, 58, 65, 72, 79, 86, 93 },
        {26, 19, 12, 4, 3, 10, 17, 24, 31, 38, 45, 52, 60, 67, 74, 81, 88, 95},
        {25, 18, 11, 4, 3, 11, 18, 25, 32, 39, 46, 54, 61, 68, 75, 82, 89, 97},
        {25, 17, 10, 3, 4, 11, 19, 26, 33, 40, 48, 55, 62, 69, 76, 84, 91, 98}
    };

int compare_int(const int* a, const float* b, int tol);
int compare_uint(const unsigned int* a, const int* b, int tol);
int compare_char(const char* a,const  float* b, int tol);
int compare_schar(const signed char* a, const  float* b, int tol);
int compare_uchar(const unsigned char* a, const  int* b, int tol);
int compare_short(const short* a, const  float* b, int tol);
int compare_ushort(const unsigned short* a, const  int* b, int tol);
int compare_ulong(const unsigned long* a, const  int* b, int tol);
int compare_long(const long* a, const  float* b, int tol);
int compare_llong(const long_long* a, const  float* b, int tol);
int compare_ullong(const unsigned long_long* a, const  int* b, int tol);
int compare_ldouble(const long double* a, const  float* b, double tol);
int compare_float(const float* a,const  float* b, double tol);
int compare_double(const double* a, const  float* b, double tol);

int main(void)
{
    hid_t dxpl_id_c_to_f, dxpl_id_c_to_f_copy, dxpl_id_simple, dxpl_id_polynomial, dxpl_id_polynomial_copy, dxpl_id_utrans_inv;

    const char* c_to_f = "(9/5.0)*x + 32";
    const char* simple = "(4/2) * ( (2 + 4)/(5 - 2.5))"; /* this equals 4.8 */ 
    const char* polynomial = "(2+x)* ((x-8)/2)";
    /* inverses the utrans transform in init_test to get back original array */
    const char* utrans_inv = "(x/3)*4 - 100";
    herr_t err;

    if((dxpl_id_c_to_f = H5Pcreate(H5P_DATASET_XFER))<0) TEST_ERROR;
    if((dxpl_id_simple = H5Pcreate(H5P_DATASET_XFER))<0) TEST_ERROR;
    if((dxpl_id_utrans_inv = H5Pcreate(H5P_DATASET_XFER))<0) TEST_ERROR;
    if((dxpl_id_polynomial =  H5Pcreate(H5P_DATASET_XFER))<0) TEST_ERROR;
    if((err = H5Pset_data_transform(dxpl_id_c_to_f, c_to_f))<0) TEST_ERROR;
    if((err = H5Pset_data_transform(dxpl_id_polynomial, polynomial))<0) TEST_ERROR;
    if((err = H5Pset_data_transform(dxpl_id_simple, simple))<0) TEST_ERROR;
    if((err = H5Pset_data_transform(dxpl_id_utrans_inv, utrans_inv))<0) TEST_ERROR;
    if((dxpl_id_polynomial_copy = H5Pcopy(dxpl_id_polynomial)) < 0) TEST_ERROR;
    if((dxpl_id_c_to_f_copy = H5Pcopy(dxpl_id_c_to_f)) < 0) TEST_ERROR;

    
    /* Run all the tests */
    if((err = init_test()) < 0) TEST_ERROR;

    if((err = test_char(dxpl_id_c_to_f)) < 0) TEST_ERROR;
    if((err = test_schar(dxpl_id_c_to_f)) < 0) TEST_ERROR;
    if((err = test_uchar(dxpl_id_utrans_inv)) < 0) TEST_ERROR;
    if((err = test_short(dxpl_id_c_to_f)) < 0) TEST_ERROR;
    if((err = test_ushort(dxpl_id_utrans_inv)) < 0) TEST_ERROR;
    if((err = test_int(dxpl_id_c_to_f, dxpl_id_polynomial)) < 0) TEST_ERROR;
    if((err = test_uint(dxpl_id_utrans_inv)) < 0) TEST_ERROR;
    if((err = test_long(dxpl_id_c_to_f)) < 0) TEST_ERROR;
    if((err = test_ulong(dxpl_id_utrans_inv)) < 0) TEST_ERROR;
    if((err = test_llong(dxpl_id_c_to_f)) < 0) TEST_ERROR;
    if((err = test_ullong(dxpl_id_utrans_inv)) < 0) TEST_ERROR;
    if((err = test_float(dxpl_id_c_to_f, dxpl_id_polynomial)) < 0) TEST_ERROR;
    if((err = test_double(dxpl_id_c_to_f)) < 0) TEST_ERROR;
    if((err = test_ldouble(dxpl_id_c_to_f)) < 0) TEST_ERROR;

    if((err = test_copy(dxpl_id_c_to_f_copy, dxpl_id_polynomial_copy)) < 0) TEST_ERROR;
    if((err = test_trivial(dxpl_id_simple)) < 0) TEST_ERROR;

    /* Close the objects we opened/created */
   if((err = H5Dclose(dset_id_char))<0) TEST_ERROR;
   if((err = H5Dclose(dset_id_uchar))<0) TEST_ERROR;
   if((err = H5Dclose(dset_id_schar))<0) TEST_ERROR;
   if((err = H5Dclose(dset_id_short))<0) TEST_ERROR;
   if((err = H5Dclose(dset_id_ushort))<0) TEST_ERROR;
   if((err = H5Dclose(dset_id_int))<0) TEST_ERROR;
   if((err = H5Dclose(dset_id_uint))<0) TEST_ERROR;
   if((err = H5Dclose(dset_id_long))<0) TEST_ERROR;
   if((err = H5Dclose(dset_id_ulong))<0) TEST_ERROR;
   if((err = H5Dclose(dset_id_llong))<0) TEST_ERROR;
   if((err = H5Dclose(dset_id_ullong))<0) TEST_ERROR;
   if((err = H5Dclose(dset_id_float))<0) TEST_ERROR;
   if((err = H5Dclose(dset_id_double))<0) TEST_ERROR;
   if((err = H5Dclose(dset_id_ldouble))<0) TEST_ERROR;
   if((err = H5Pclose(dxpl_id_c_to_f))<0) TEST_ERROR;
   if((err = H5Pclose(dxpl_id_c_to_f_copy))<0) TEST_ERROR;
   if((err = H5Pclose(dxpl_id_polynomial))<0) TEST_ERROR;
   if((err = H5Pclose(dxpl_id_polynomial_copy))<0) TEST_ERROR;
   if((err = H5Pclose(dxpl_id_simple))<0) TEST_ERROR;
   if((err = H5Pclose(dxpl_id_utrans_inv))<0) TEST_ERROR;


   return 0;
   
error:
   return -1;
}

int init_test(void)
{
    const char* f_to_c = "(5/9.0)*(x-32)";
    /* utrans is a transform for unsigned types: no negative numbers involved and results are < 255 to fit into uchar */
    const char* utrans = "((x+100)/4)*3";

    herr_t err;
    hid_t file_id, dataspace, dxpl_id_f_to_c, dxpl_id_utrans;
    hsize_t dim[2] = {ROWS, COLS};

    if((dxpl_id_f_to_c = H5Pcreate(H5P_DATASET_XFER))<0) TEST_ERROR;
    if((dxpl_id_utrans = H5Pcreate(H5P_DATASET_XFER))<0) TEST_ERROR;
    
    if((err= H5Pset_data_transform(dxpl_id_f_to_c, f_to_c))<0) TEST_ERROR;
    if((err= H5Pset_data_transform(dxpl_id_utrans, utrans))<0) TEST_ERROR;
    
    if((file_id = H5Fcreate("dtransform.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT))<0) TEST_ERROR;

    if((dataspace = H5Screate_simple(2, dim, NULL))<0) TEST_ERROR;

    TESTING("data transform, writing out data file")

    /* Write out the integer dataset to the file, converting it to c */
    if((dset_id_int = H5Dcreate(file_id, "/transformtest_int", H5T_NATIVE_INT, dataspace, H5P_DEFAULT))<0) TEST_ERROR;
    if((err = H5Dwrite(dset_id_int, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, dxpl_id_f_to_c, windchillFfloat))<0) TEST_ERROR;

    /* Write out the character dataset to the file, converting it to c */
    if((dset_id_char = H5Dcreate(file_id, "/transformtest_char", H5T_NATIVE_CHAR, dataspace, H5P_DEFAULT))<0) TEST_ERROR;
    if((err = H5Dwrite(dset_id_char, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, dxpl_id_f_to_c, windchillFfloat))<0) TEST_ERROR;
    
    /* Write out a floating point version to the file, converting it to c */
    if((dset_id_float = H5Dcreate(file_id, "/transformtest_float", H5T_NATIVE_FLOAT, dataspace, H5P_DEFAULT))<0) TEST_ERROR;
    if((err = H5Dwrite(dset_id_float, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, dxpl_id_f_to_c, windchillFfloat))<0) TEST_ERROR;

    if((dset_id_uchar = H5Dcreate(file_id, "/transformtest_uchar", H5T_NATIVE_UCHAR, dataspace, H5P_DEFAULT))<0) TEST_ERROR;
    if((err = H5Dwrite(dset_id_uchar, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl_id_utrans, transformData))<0) TEST_ERROR;

    if((dset_id_schar = H5Dcreate(file_id, "/transformtest_schar", H5T_NATIVE_SCHAR, dataspace, H5P_DEFAULT))<0) TEST_ERROR;
    if((err = H5Dwrite(dset_id_schar, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, dxpl_id_f_to_c, windchillFfloat))<0) TEST_ERROR;

    if((dset_id_uint = H5Dcreate(file_id, "/transformtest_uint", H5T_NATIVE_UINT, dataspace, H5P_DEFAULT))<0) TEST_ERROR;
    if((err = H5Dwrite(dset_id_uint, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl_id_utrans, transformData))<0) TEST_ERROR;

    if((dset_id_ushort = H5Dcreate(file_id, "/transformtest_ushort", H5T_NATIVE_USHORT, dataspace, H5P_DEFAULT))<0) TEST_ERROR;
    if((err = H5Dwrite(dset_id_ushort, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl_id_utrans, transformData))<0) TEST_ERROR;

    if((dset_id_ulong = H5Dcreate(file_id, "/transformtest_ulong", H5T_NATIVE_ULONG, dataspace, H5P_DEFAULT))<0) TEST_ERROR;
    if((err = H5Dwrite(dset_id_ulong, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl_id_utrans, transformData))<0) TEST_ERROR;

    if((dset_id_long = H5Dcreate(file_id, "/transformtest_long", H5T_NATIVE_LONG, dataspace, H5P_DEFAULT))<0) TEST_ERROR;
    if((err = H5Dwrite(dset_id_long, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, dxpl_id_f_to_c, windchillFfloat))<0) TEST_ERROR;
 
    if((dset_id_llong = H5Dcreate(file_id, "/transformtest_llong", H5T_NATIVE_LLONG, dataspace, H5P_DEFAULT))<0) TEST_ERROR;
    if((err = H5Dwrite(dset_id_llong, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, dxpl_id_f_to_c, windchillFfloat))<0) TEST_ERROR;

    if((dset_id_ullong = H5Dcreate(file_id, "/transformtest_ullong", H5T_NATIVE_ULLONG, dataspace, H5P_DEFAULT))<0) TEST_ERROR;
    if((err = H5Dwrite(dset_id_ullong, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl_id_utrans, transformData))<0) TEST_ERROR;

    if((dset_id_short = H5Dcreate(file_id, "/transformtest_short", H5T_NATIVE_SHORT, dataspace, H5P_DEFAULT))<0) TEST_ERROR;
    if((err = H5Dwrite(dset_id_short, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, dxpl_id_f_to_c, windchillFfloat))<0) TEST_ERROR;

    if((dset_id_double = H5Dcreate(file_id, "/transformtest_double", H5T_NATIVE_DOUBLE, dataspace, H5P_DEFAULT))<0) TEST_ERROR;
    if((err = H5Dwrite(dset_id_double, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, dxpl_id_f_to_c, windchillFfloat))<0) TEST_ERROR;

    if((dset_id_ldouble = H5Dcreate(file_id, "/transformtest_ldouble", H5T_NATIVE_LDOUBLE, dataspace, H5P_DEFAULT))<0) TEST_ERROR;
    if((err = H5Dwrite(dset_id_ldouble, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, dxpl_id_f_to_c, windchillFfloat))<0) TEST_ERROR;


    PASSED();



   if((err = H5Fclose(file_id))<0) TEST_ERROR;
   if((err = H5Pclose(dxpl_id_f_to_c))<0) TEST_ERROR;
   if((err = H5Pclose(dxpl_id_utrans))<0) TEST_ERROR;
   if((err = H5Sclose(dataspace))<0) TEST_ERROR;

   return 0;
error:
   return -1;
}
 
int test_int(const hid_t dxpl_id_c_to_f, const hid_t dxpl_id_polynomial)
{
    int windchillFintread[ROWS][COLS];
    float   polyflres[ROWS][COLS];
    int   polyintread[ROWS][COLS];
    int windchillCint[ROWS][COLS];

    herr_t err;

    int row, col;

    for(row = 0; row<ROWS; row++)
    {
	for(col = 0; col<COLS; col++)
	    windchillCint[row][col] = (5/9.0)*(windchillFfloat[row][col] - 32);
    }

    for(row = 0; row<ROWS; row++)
    {
	for(col = 0; col<COLS; col++)
	    polyflres[row][col] = (2+windchillCint[row][col])*((windchillCint[row][col]-8)/2);
    }

    TESTING("data transform, no data type conversion (int->int)")
    /* Read in the integer data with a data transform. */
    if((err = H5Dread(dset_id_int, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl_id_c_to_f, windchillFintread))<0) TEST_ERROR;
    
    if( (compare_int(*windchillFintread, *windchillFfloat, 2)) == 0)
    {
	H5_FAILED();
        fprintf(stderr, "    ERROR: Conversion failed to match computed data\n");
	goto error;
    }
    else
	PASSED();

   TESTING("data transform, with data type conversion (float->int)")
    if((err = H5Dread(dset_id_float, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl_id_c_to_f, windchillFintread))<0) TEST_ERROR;
    if( (compare_int(*windchillFintread, *windchillFfloat, 2)) == 0)
         {   
	     H5_FAILED();
             fprintf(stderr, "    ERROR: Conversion failed to match computed data\n");
	goto error;
    }
    else
       PASSED();

    TESTING("data transform, polynomial transform (float->int)")
    if((err = H5Dread(dset_id_float, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl_id_polynomial, polyintread))<0) TEST_ERROR;
    if( (compare_int(*polyintread, *polyflres, 2)) == 0)
    {
	H5_FAILED();	
	fprintf(stderr, "    ERROR: Conversion failed to match computed data\n");
	goto error;
    }
    else
       PASSED();

    return 0;

error:
    return -1;

}

int test_float(const hid_t dxpl_id_c_to_f, const hid_t dxpl_id_polynomial)
{
    float polyflres[ROWS][COLS];
    float windchillFfloatread[ROWS][COLS];
    float polyflread[ROWS][COLS];
    int windchillCint[ROWS][COLS];
    
    herr_t err;
    int row, col;

    for(row = 0; row<ROWS; row++)
    {
	for(col = 0; col<COLS; col++)
	    windchillCint[row][col] = (5/9.0)*(windchillFfloat[row][col] - 32);
    }
   
    for(row = 0; row<ROWS; row++)
    {
	for(col = 0; col<COLS; col++)
	    polyflres[row][col] = (2.0+windchillCint[row][col])*((windchillCint[row][col]-8.0)/2.0);
    }



   TESTING("data transform, no data type conversion (float->float)")
    if((err = H5Dread(dset_id_float, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, dxpl_id_c_to_f, windchillFfloatread))<0) TEST_ERROR;
    if( (compare_float(*windchillFfloat, *windchillFfloatread, 2.0)) == 0)
    {
	H5_FAILED();	
	fprintf(stderr, "    ERROR: Conversion failed to match computed data\n");
	goto error;
    }
    else
      PASSED();

   TESTING("data transform, with data type conversion (int->float)")
    if((err = H5Dread(dset_id_int, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, dxpl_id_c_to_f, windchillFfloatread))<0) TEST_ERROR;
    if( (compare_float(*windchillFfloat, *windchillFfloatread, 2.0)) == 0)
         {   
	H5_FAILED();	
             fprintf(stderr, "    ERROR: Conversion failed to match computed data\n");
	goto error;
    }
    else
       PASSED();

    TESTING("data transform, polynomial transform (int->float)")
    if((err = H5Dread(dset_id_int, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, dxpl_id_polynomial, polyflread))<0) TEST_ERROR;
    if( (compare_float(*polyflread, *polyflres, 2.0)) == 0)
    {   
	H5_FAILED();	
	fprintf(stderr, "    ERROR: Conversion failed to match computed data\n");
	goto error;
    }
    else
       PASSED();

    return 0;

error:
    return -1;

}

int test_uchar(const hid_t dxpl_id_utrans_inv)
{
    unsigned char transformDatauchar[ROWS][COLS];

    herr_t err;
    
    TESTING("data transform, no data type conversion (uchar->uchar)")

    if((err = H5Dread(dset_id_uchar, H5T_NATIVE_UCHAR, H5S_ALL, H5S_ALL, dxpl_id_utrans_inv, transformDatauchar))<0) TEST_ERROR;
    if( (compare_uchar(*transformDatauchar, *transformData, 4)) == 0)
    {   
	H5_FAILED();
	fprintf(stderr, "    ERROR: Conversion failed to match computed data\n");
	goto error;
    }
    else
       PASSED();

    return 0;

error:
    return -1;
}

int test_schar(const hid_t dxpl_id_c_to_f)
{
    signed char windchillFschar[ROWS][COLS];

    herr_t err;
    
    TESTING("data transform, no data type conversion (schar->schar)")
	SKIPPED();
/*    if((err = H5Dread(dset_id_schar, H5T_NATIVE_SCHAR, H5S_ALL, H5S_ALL, dxpl_id_c_to_f, windchillFschar))<0) TEST_ERROR;
    if( (compare_schar(*windchillFschar, *windchillFfloat, 2)) == 0)
    {   
	H5_FAILED();
	fprintf(stderr, "    ERROR: Conversion failed to match computed data\n");
	goto error;
    }
    else
       PASSED();
*/
    return 0;

error:
    return -1;
}

int test_char(const hid_t dxpl_id_c_to_f)
{
    char  windchillFchar[ROWS][COLS];

    herr_t err;
    
    TESTING("data transform, no data type conversion (char->char)")

    SKIPPED();
   /* if((err = H5Dread(dset_id_char, H5T_NATIVE_CHAR, H5S_ALL, H5S_ALL, dxpl_id_c_to_f, windchillFchar))<0) TEST_ERROR;
    if( (compare_char(*windchillFchar, *windchillFfloat, 2)) == 0)
    {   
	H5_FAILED();
	fprintf(stderr, "    ERROR: Conversion failed to match computed data\n");
	goto error;
    }
    else
       PASSED();
*/
	
    return 0;

error:
    return -1;
}




int test_uint(const hid_t dxpl_id_utrans_inv)
{
    unsigned int transformDatauint[ROWS][COLS];

    herr_t err;
    
    TESTING("data transform, no data type conversion (uint->uint)")
    if((err = H5Dread(dset_id_uint, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, dxpl_id_utrans_inv, transformDatauint))<0) TEST_ERROR;
    if( (compare_uint(*transformDatauint, *transformData, 4)) == 0)
    {   
	H5_FAILED();
	fprintf(stderr, "    ERROR: Conversion failed to match computed data\n");
	goto error;
    }
    else
       PASSED();

    return 0;

error:
    return -1;
}


int test_ushort(const hid_t dxpl_id_utrans_inv)
{
    unsigned short transformDataushort[ROWS][COLS];

    herr_t err;
    
    TESTING("data transform, no data type conversion (ushort->ushort)")
    if((err = H5Dread(dset_id_ushort, H5T_NATIVE_USHORT, H5S_ALL, H5S_ALL, dxpl_id_utrans_inv, transformDataushort))<0) TEST_ERROR;
    if( (compare_ushort(*transformDataushort, *transformData, 4)) == 0)
    {   
	H5_FAILED();
	fprintf(stderr, "    ERROR: Conversion failed to match computed data\n");
	goto error;
    }
    else
       PASSED();

    return 0;

error:
    return -1;
}


int test_short(const hid_t dxpl_id_c_to_f)
{
    short windchillFshort[ROWS][COLS];

    herr_t err;
    
    TESTING("data transform, no data type conversion (short->short)")
    if((err = H5Dread(dset_id_short, H5T_NATIVE_SHORT, H5S_ALL, H5S_ALL, dxpl_id_c_to_f, windchillFshort))<0) TEST_ERROR;
    if( (compare_short(*windchillFshort, *windchillFfloat, 2)) == 0)
    {   
	H5_FAILED();
	fprintf(stderr, "    ERROR: Conversion failed to match computed data\n");
	goto error;
    }
    else
       PASSED();

    return 0;

error:
    return -1;
}




int test_long(const hid_t dxpl_id_c_to_f)
{
    long windchillFlong[ROWS][COLS];

    herr_t err;
    
    TESTING("data transform, no data type conversion (long->long)")
    if((err = H5Dread(dset_id_long, H5T_NATIVE_LONG, H5S_ALL, H5S_ALL, dxpl_id_c_to_f, windchillFlong))<0) TEST_ERROR;
    if( (compare_long(*windchillFlong, *windchillFfloat, 2)) == 0)
    {   
	H5_FAILED();
	fprintf(stderr, "    ERROR: Conversion failed to match computed data\n");
	goto error;
    }
    else
       PASSED();

    return 0;

error:
    return -1;
}

int test_ulong(const hid_t dxpl_id_utrans_inv)
{
    unsigned long transformDataulong[ROWS][COLS];

    herr_t err;
    
    TESTING("data transform, no data type conversion (ulong->ulong)")
    if((err = H5Dread(dset_id_ulong, H5T_NATIVE_ULONG, H5S_ALL, H5S_ALL, dxpl_id_utrans_inv, transformDataulong))<0) TEST_ERROR;
    if( (compare_ulong(*transformDataulong, *transformData, 4)) == 0)
    {   
	H5_FAILED();
	fprintf(stderr, "    ERROR: Conversion failed to match computed data\n");
	goto error;
    }
    else
       PASSED();

    return 0;

error:
    return -1;
}


int test_llong(const hid_t dxpl_id_c_to_f)
{
    long_long windchillFllong[ROWS][COLS];

    herr_t err;
    
    TESTING("data transform, no data type conversion (llong->llong)")
    if((err = H5Dread(dset_id_llong, H5T_NATIVE_LLONG, H5S_ALL, H5S_ALL, dxpl_id_c_to_f, windchillFllong))<0) TEST_ERROR;
    if( (compare_llong(*windchillFllong, *windchillFfloat, 2)) == 0)
    {   
	H5_FAILED();
	fprintf(stderr, "    ERROR: Conversion failed to match computed data\n");
	goto error;
    }
    else
       PASSED();

    return 0;

error:
    return -1;
}

int test_ullong(const hid_t dxpl_id_utrans_inv)
{
    unsigned long_long transformDataullong[ROWS][COLS];

    herr_t err;  
    
    TESTING("data transform, no data type conversion (ullong->ullong)")

    if((err = H5Dread(dset_id_ullong, H5T_NATIVE_ULLONG, H5S_ALL, H5S_ALL, dxpl_id_utrans_inv, transformDataullong))<0) TEST_ERROR;
    if( (compare_ullong(*transformDataullong, *transformData, 4)) == 0)
    {   
	H5_FAILED();
	fprintf(stderr, "    ERROR: Conversion failed to match computed data\n");
	goto error;
    }
    else
       PASSED();

    return 0;

error:
    return -1; 
}


int test_double(const hid_t dxpl_id_c_to_f)
{
    double windchillFdouble[ROWS][COLS];

    herr_t err;
    
    TESTING("data transform, no data type conversion (double->double)")
    if((err = H5Dread(dset_id_double, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, dxpl_id_c_to_f, windchillFdouble))<0) TEST_ERROR;
    if( (compare_double(*windchillFdouble, *windchillFfloat, 2.0)) == 0)
    {   
	H5_FAILED();
	fprintf(stderr, "    ERROR: Conversion failed to match computed data\n");
	goto error;
    }
    else
       PASSED();

    return 0;

error:
    return -1;
}

int test_ldouble(const hid_t dxpl_id_c_to_f)
{
    long double windchillFldouble[ROWS][COLS];

    herr_t err;
    
    TESTING("data transform, no data type conversion (ldouble->ldouble)")
    if((err = H5Dread(dset_id_ldouble, H5T_NATIVE_LDOUBLE, H5S_ALL, H5S_ALL, dxpl_id_c_to_f, windchillFldouble))<0) TEST_ERROR;
    if( (compare_ldouble(*windchillFldouble, *windchillFfloat, 2.0)) == 0)
    {   
	H5_FAILED();
	fprintf(stderr, "    ERROR: Conversion failed to match computed data\n");
	goto error;
    }
    else
       PASSED();

    return 0;

error:
    return -1;
}



int test_copy(const hid_t dxpl_id_c_to_f_copy, const hid_t dxpl_id_polynomial_copy)
{
    int windchillCint[ROWS][COLS];
    float polyflres[ROWS][COLS];
    int polyintread[ROWS][COLS];
    int windchillFintread[ROWS][COLS];

    herr_t err;

    int row, col;

    for(row = 0; row<ROWS; row++)
    {
	for(col = 0; col<COLS; col++)
	    windchillCint[row][col] = (5/9.0)*(windchillFfloat[row][col] - 32);
    }

    for(row = 0; row<ROWS; row++)
    {
	for(col = 0; col<COLS; col++)
	    polyflres[row][col] = (2+windchillCint[row][col])*((windchillCint[row][col]-8)/2);
    }

  
   TESTING("data transform, linear transform w/ copied property")
    if((err = H5Dread(dset_id_float, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl_id_c_to_f_copy, windchillFintread))<0) TEST_ERROR;
    if( (compare_int(*windchillFintread, *windchillFfloat, 2)) == 0)
    {   
	H5_FAILED();	
	fprintf(stderr, "    ERROR: Conversion failed to match computed data\n");
	goto error;
    }
    else
       PASSED();

    TESTING("data transform, polynomial transform w/ copied property")
    if((err = H5Dread(dset_id_float, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl_id_polynomial_copy, polyintread))<0) TEST_ERROR;
    if( (compare_int(*polyintread, *polyflres, 2)) == 0)
    {   
	H5_FAILED();	
	fprintf(stderr, "    ERROR: Conversion failed to match computed data\n");
	goto error;
    }
    else
       PASSED();

    return 0;

error:
    return -1;
}
   
int test_trivial(const hid_t dxpl_id_simple)
{
    float windchillFfloatread[ROWS][COLS];
    int windchillFintread[ROWS][COLS];

    herr_t err;
    int row, col;

    TESTING("data transform, trivial transform, without type conversion")
    if((err = H5Dread(dset_id_float, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, dxpl_id_simple, windchillFfloatread))<0) TEST_ERROR;
    for(row = 0; row<ROWS; row++)
    {
	for(col = 0; col<COLS; col++)
	{
	    if((windchillFfloatread[row][col] - 4.8) > 0.0001)
	    {
		H5_FAILED();
		fprintf(stderr, "    ERROR: Conversion failed to match computed data\n");
		goto error;
	    }
	}
    }
    PASSED();

    TESTING("data transform, trivial transform, with type conversion")
    if((err = H5Dread(dset_id_float, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl_id_simple, windchillFintread))<0) TEST_ERROR;
    for(row = 0; row<ROWS; row++)
    {
	for(col = 0; col<COLS; col++)
	{
	    if(windchillFintread[row][col] != 4)
	    {
		H5_FAILED();
		fprintf(stderr, "    ERROR: Conversion failed to match computed data\n");
		goto error;
	    }
	}
    }
    PASSED();

    return 0;
error:
    return -1;
}

int compare_int(const int* a, const float* b, int tol)
{
    int i;
    for(i=0; i<ROWS*COLS; i++)
    {
	if( !((a[i] <= ((int)b[i] + tol)) && (a[i] >= ((int)b[i] - tol))))
	    return 0;
    }
    return 1;
   
}
    
int compare_float(const float* a,const  float* b, double tol)
{
    int i;
    for(i=0; i<ROWS*COLS; i++)
    {
	if( !((a[i] <= (b[i] + tol)) && (a[i] >= (b[i] - tol))) )
	    return 0;
    }
    return 1;
}


int compare_char(const char* a,const float* b, int tol)
{
    int i;
    for(i=0; i<ROWS*COLS; i++)
    {
	if( !((a[i] <= ((char)b[i] + tol)) && (a[i] >= ((char)b[i] - tol))) )
	    return 0;
    }
    return 1;
}

int compare_uchar(const unsigned char* a, const int* b, int tol)
{
    int i;
    for(i=0; i<ROWS*COLS; i++)
    {
	if(!( (  (a[i] >= (unsigned char)b[i]) && ( (a[i] - tol) < (unsigned char)b[i])) || (  (a[i] <= (unsigned char)b[i]) && ( (a[i] + tol) > (unsigned char)b[i])) ))
	    return 0;
    }
    return 1;
}

int compare_schar(const signed char* a,const float* b, int tol)
{
    int i;
    for(i=0; i<ROWS*COLS; i++)
    {
	if( !((a[i] <= ((signed char)b[i] + tol)) && (a[i] >= ((signed char)b[i] - tol))) )
	    return 0;
    }
    return 1;
}

int compare_double(const double* a, const float* b, double tol)
{
    int i;
    for(i=0; i<ROWS*COLS; i++)
    {
	if( !((a[i] <= ((double)b[i] + tol)) && (a[i] >= ((double)b[i] - tol))) )
	    return 0;
    }
    return 1;
}

int compare_ldouble(const long double* a, const float* b, double tol)
{
    int i;
    for(i=0; i<ROWS*COLS; i++)
    {
	if( !((a[i] <= ((long double)b[i] + tol)) && (a[i] >= ((long double)b[i] - tol))) )
	    return 0;
    }
    return 1;
}

int compare_short(const short* a, const float* b, int tol)
{
    int i;
    for(i=0; i<ROWS*COLS; i++)
    {
	if( !((a[i] <= ((short)b[i] + tol)) && (a[i] >= ((short)b[i] - tol))) )
	    return 0;
    }
    return 1;
}

int compare_ushort(const unsigned short* a, const int* b, int tol)
{
    int i;
    for(i=0; i<ROWS*COLS; i++)
    {
	if(!( (  (a[i] >= (unsigned short)b[i]) && ( (a[i] - tol) < (unsigned short)b[i])) || (  (a[i] <= (unsigned short)b[i]) && ( (a[i] + tol) > (unsigned short)b[i])) ))
	    return 0;
    }
    return 1;
}

int compare_uint(const unsigned int* a, const int* b, int tol)
{
    int i;
    for(i=0; i<ROWS*COLS; i++)
    {	
	if(!( (  (a[i] >= (unsigned int)b[i]) && ( (a[i] - tol) < (unsigned int)b[i])) || (  (a[i] <= (unsigned int)b[i]) && ( (a[i] + tol) > (unsigned int)b[i])) ))
	    return 0;
    }
    return 1;
}

int compare_long(const long* a, const float* b, int tol)
{
    int i;
    for(i=0; i<ROWS*COLS; i++)
    {
	if( !((a[i] <= ((long)b[i] + tol)) && (a[i] >= ((long)b[i] - tol))) )
	    return 0;
    }
    return 1;
}

int compare_llong(const long_long* a, const float* b, int tol)
{
    int i;
    for(i=0; i<ROWS*COLS; i++)
    {
	if( !((a[i] <= ((long_long)b[i] + tol)) && (a[i] >= ((long_long)b[i] - tol))) )
	    return 0;
    }
    return 1;
}


int compare_ullong(const unsigned long_long* a, const int* b, int tol)
{
    int i;
    for(i=0; i<ROWS*COLS; i++)
    {
	if(!( (  (a[i] >= (unsigned long_long)b[i]) && ( (a[i] - tol) < (unsigned long_long)b[i])) || (  (a[i] <= (unsigned long_long)b[i]) && ( (a[i] + tol) > (unsigned long_long)b[i])) ))
	    return 0;
    }
    return 1;
}

int compare_ulong(const unsigned long* a, const int* b, int tol)
{
    int i;
    for(i=0; i<ROWS*COLS; i++)
    {	
	if(!( (  (a[i] >= (unsigned long)b[i]) && ( (a[i] - tol) < (unsigned long)b[i])) || (  (a[i] <= (unsigned long)b[i]) && ( (a[i] + tol) > (unsigned long)b[i])) ))
	    return 0;
    }
    return 1;
}

