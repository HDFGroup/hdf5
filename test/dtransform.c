#include "h5test.h"

#define ROWS    12
#define COLS    18

int compare_int(int* a, int* b, int tol);
int compare_float(float* a, float* b, double tol);

int main(void)
{
    hid_t file_id, dxpl_id_f_to_c, dxpl_id_c_to_f, dxpl_id_c_to_f_copy, dxpl_id_simple, dxpl_id_polynomial, dxpl_id_polynomial_copy, dset_id_int, dset_id_float, datatype, dataspace;
    hsize_t dim[2] = {ROWS, COLS};
    const char* f_to_c = "(5/9.0)*(x-32)";
    const char* c_to_f = "(9/5.0)*x + 32";
    const char* polynomial = "(2+x)* ((x-8)/2)";
    const char* simple = "(4/2) * ( (2 + 4)/(5 - 2.5))"; /* this equals 4.8 */ 
    int windchillFint[ROWS][COLS] =
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
    int windchillFintread[ROWS][COLS];
    float windchillFfloat[ROWS][COLS] = 
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
    float windchillFfloatread[ROWS][COLS];
    float polyflres[ROWS][COLS];
    int   polyintres[ROWS][COLS];
    float polyflread[ROWS][COLS];
    int   polyintread[ROWS][COLS];
    int windchillCint[ROWS][COLS];
    herr_t err;
    int row, col;

    TESTING("data transform, setting up")

    for(row = 0; row<ROWS; row++)
    {
	for(col = 0; col<COLS; col++)
	    windchillCint[row][col] = (5/9.0)*(windchillFint[row][col] - 32);
    }

    for(row = 0; row<ROWS; row++)
    {
	for(col = 0; col<COLS; col++)
	    polyflres[row][col] = (2.0+windchillCint[row][col])*((windchillCint[row][col]-8.0)/2.0);
    }

    for(row = 0; row<ROWS; row++)
    {
	for(col = 0; col<COLS; col++)
	    polyintres[row][col] = (2+windchillCint[row][col])*((windchillCint[row][col]-8)/2);
    }

    if((dxpl_id_f_to_c = H5Pcreate(H5P_DATASET_XFER))<0) TEST_ERROR;
    if((dxpl_id_c_to_f = H5Pcreate(H5P_DATASET_XFER))<0) TEST_ERROR;
    if((dxpl_id_simple = H5Pcreate(H5P_DATASET_XFER))<0) TEST_ERROR;
    if((dxpl_id_polynomial =  H5Pcreate(H5P_DATASET_XFER))<0) TEST_ERROR;
    
    if((err= H5Pset_data_transform(dxpl_id_f_to_c, f_to_c))<0) TEST_ERROR;
    if((err= H5Pset_data_transform(dxpl_id_c_to_f, c_to_f))<0) TEST_ERROR;
/* Temporary disabled. AKC
    if((err = H5Pset_data_transform(dxpl_id_simple, simple))<0) TEST_ERROR;
*/
    if((err = H5Pset_data_transform(dxpl_id_polynomial, polynomial))<0) TEST_ERROR;
    if((dxpl_id_polynomial_copy = H5Pcopy(dxpl_id_polynomial)) < 0) TEST_ERROR;
    if((dxpl_id_c_to_f_copy = H5Pcopy(dxpl_id_c_to_f)) < 0) TEST_ERROR;

    if((file_id = H5Fcreate("dtransform.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT))<0) TEST_ERROR;

    if((dataspace = H5Screate_simple(2, dim, NULL))<0) TEST_ERROR;

    PASSED();

    TESTING("data transform, writing integer data")

    /* Write out the integer dataset to the file, converting it to c */
    if((datatype = H5Tcopy(H5T_NATIVE_INT))<0) TEST_ERROR;
    if((dset_id_int = H5Dcreate(file_id, "/transformtest_int", datatype, dataspace, H5P_DEFAULT))<0) TEST_ERROR;
    if((err = H5Dwrite(dset_id_int, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl_id_f_to_c, windchillFint))<0) TEST_ERROR;
    if((err = H5Tclose(datatype))<0) TEST_ERROR;

    PASSED();

    TESTING("data transform, writing float data")
    
    /* Write out a floating point version to the file, converting it to c */
    if((datatype = H5Tcopy(H5T_NATIVE_FLOAT))<0) TEST_ERROR;
    if((dset_id_float = H5Dcreate(file_id, "/transformtest_float", datatype, dataspace, H5P_DEFAULT))<0) TEST_ERROR;
    if((err = H5Dwrite(dset_id_float, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl_id_f_to_c, windchillFint))<0) TEST_ERROR;
    if((err = H5Tclose(datatype))<0) TEST_ERROR;

    PASSED();

    TESTING("data transform, no data type conversion (int->int)")
    /* Read in the integer data with a data transform. */
    if((err = H5Dread(dset_id_int, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl_id_c_to_f, windchillFintread))<0) TEST_ERROR;
    
    if( (compare_int(*windchillFintread, *windchillFint, 2)) == 0)
    {
        fprintf(stderr, "ERROR: Conversion failed to match computed data\n");
        TEST_ERROR;
    }
    else
	PASSED();

    TESTING("data transform, no data type conversion (float->float)")
    if((err = H5Dread(dset_id_float, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, dxpl_id_c_to_f, windchillFfloatread))<0) TEST_ERROR;
    if( (compare_float(*windchillFfloat, *windchillFfloatread, 2.0)) == 0)
    {
	fprintf(stderr, "ERROR: Conversion failed to match computed data\n");
	TEST_ERROR;
    }
    else
      PASSED();

   TESTING("data transform, with data type conversion (int->float)")
    if((err = H5Dread(dset_id_int, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, dxpl_id_c_to_f, windchillFfloatread))<0) TEST_ERROR;
    if( (compare_float(*windchillFfloat, *windchillFfloatread, 2.0)) == 0)
         {   
             fprintf(stderr, "ERROR: Conversion failed to match computed data\n");
        TEST_ERROR;
    }
    else
       PASSED();

   TESTING("data transform, with data type conversion (float->int)")
    if((err = H5Dread(dset_id_float, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl_id_c_to_f, windchillFintread))<0) TEST_ERROR;
    if( (compare_int(*windchillFint, *windchillFintread, 2)) == 0)
         {   
             fprintf(stderr, "ERROR: Conversion failed to match computed data\n");
        TEST_ERROR;
    }
    else
       PASSED();

    TESTING("data transform, linear transform w/ copied property")
    if((err = H5Dread(dset_id_float, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl_id_c_to_f_copy, windchillFintread))<0) TEST_ERROR;
    if( (compare_int(*windchillFint, *windchillFintread, 2)) == 0)
    {   
	fprintf(stderr, "ERROR: Conversion failed to match computed data\n");
        TEST_ERROR;
    }
    else
       PASSED();

    TESTING("data transform, polynomial transform (float->int)")
    if((err = H5Dread(dset_id_float, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl_id_polynomial, polyintread))<0) TEST_ERROR;
    if( (compare_int(*polyintres, *polyintread, 2)) == 0)
    {   
	fprintf(stderr, "ERROR: Conversion failed to match computed data\n");
        TEST_ERROR;
    }
    else
       PASSED();

    TESTING("data transform, polynomial transform w/ copied property")
    if((err = H5Dread(dset_id_float, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl_id_polynomial_copy, polyintread))<0) TEST_ERROR;
    if( (compare_int(*polyintres, *polyintread, 2)) == 0)
    {   
	fprintf(stderr, "ERROR: Conversion failed to match computed data\n");
        TEST_ERROR;
    }
    else
       PASSED();


    TESTING("data transform, polynomial transform (int-float)")
    if((err = H5Dread(dset_id_int, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, dxpl_id_polynomial, polyflread))<0) TEST_ERROR;
    if( (compare_float(*polyflres, *polyflread, 2.0)) == 0)
    {   
	fprintf(stderr, "ERROR: Conversion failed to match computed data\n");
        TEST_ERROR;
    }
    else
       PASSED();


    
    TESTING("data transform, trivial transform, without type conversion")
#if 0
	/* temporary disabled. AKC */
    if((err = H5Dread(dset_id_float, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, dxpl_id_simple, windchillFfloatread))<0) TEST_ERROR;
    for(row = 0; row<ROWS; row++)
    {
	for(col = 0; col<COLS; col++)
	{
	    if((windchillFfloatread[row][col] - 4.8) > 0.0001)
	    {
		fprintf(stderr, "ERROR: Conversion failed to match computed data\n");
		TEST_ERROR;
	    }
	}
    }
    PASSED();
#else
    SKIPPED();
#endif

    TESTING("data transform, trivial transform, with type conversion")
#if 0
	/* temporary disabled. AKC */
    if((err = H5Dread(dset_id_float, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl_id_simple, windchillFintread))<0) TEST_ERROR;
    for(row = 0; row<ROWS; row++)
    {
	for(col = 0; col<COLS; col++)
	{
	    if(windchillFintread[row][col] != 4)
	    {
		fprintf(stderr, "ERROR: Conversion failed to match computed data\n");
		TEST_ERROR;
	    }
	}
    }
    PASSED();
#else
    SKIPPED();
#endif


    /* Close the objects we opened/created */
   if((err = H5Sclose(dataspace))<0) TEST_ERROR;
   if((err = H5Dclose(dset_id_int))<0) TEST_ERROR;
   if((err = H5Dclose(dset_id_float))<0) TEST_ERROR;
   if((err = H5Fclose(file_id))<0) TEST_ERROR;
   if((err = H5Pclose(dxpl_id_f_to_c))<0) TEST_ERROR;
   if((err = H5Pclose(dxpl_id_c_to_f))<0) TEST_ERROR;

   return 0;

error:
   return -1;
}

int compare_int(int* a, int* b, int tol)
{
    int i;
    for(i=0; i<ROWS*COLS; i++)
    {
	if( !((a[i] <= (b[i] + tol)) && (a[i] >= (b[i] - tol))))
	    return 0;
    }
    return 1;
   
}
    
int compare_float(float* a, float* b, double tol)
{
    int i;
    for(i=0; i<ROWS*COLS; i++)
    {
	if( !((a[i] <= (b[i] + tol)) && (a[i] >= (b[i] - tol))) )
	    return 0;
    }
    return 1;
}


