

#include <stdio.h>
#include <stdlib.h>

#include "hdf5.h"


#ifndef FALSE
#define FALSE	0
#endif
#ifndef TRUE
#define TRUE	1
#endif

int do_test_files();
int h5diff_dataset( hid_t file1_id, hid_t file2_id, const char *dset_name );
void array_diff( void *buf1, void *buf2, hsize_t tot_cnt, hid_t type_id );




/*-------------------------------------------------------------------------
 * Function: usage
 *
 * Purpose: print a usage message  
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October 8, 2002
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

void usage( const char *progname )
{
#define USAGE   "\
  [-d dset]         Compare only the specified dataset(s)\n\
  file1             File name of the first HDF5 file\n\
  file2             File name of the second HDF5 file\n"

 fprintf(stderr,
  "%s [-d] file1 file2\n%s",
  progname,
  USAGE);
 exit(EXIT_FAILURE);
}


/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: H5diff 
 *
 * Return: Success: 0, Failure: 1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October 8, 2002
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

int main(int argc, const char *argv[])
{
 
 const char *progname = argv[0];
	int        dset_only = FALSE;
	int        argno;
	const char	*s = NULL;
	const char *file1_name = NULL;
	const char *file2_name = NULL;
	const char *dset_name = NULL;
	hid_t      file1_id, file2_id; 
	herr_t     status;

 do_test_files();


/*-------------------------------------------------------------------------
 * parse command line options
 *-------------------------------------------------------------------------
 */
 
 if (argc < 2) {
  usage( progname );
  exit(EXIT_FAILURE);
 }
 
	/* parse command line options */
 for (argno=1; argno<argc ; argno++) 
 {
  
  /* get the single-letter switches */
  if ( '-'==argv[argno][0] )
  {
   
   for (s=argv[argno]+1; *s; s++) 
   {
    switch (*s) {
    case 'h': 
     usage(progname);
     exit(EXIT_SUCCESS);
    case 'd': 
     dset_only = TRUE;
     dset_name = argv[argno+1];
     break;
    } /*switch*/
   } /*for*/ 
  } /*if*/

 }/*for*/


/*-------------------------------------------------------------------------
 * process the files
 *-------------------------------------------------------------------------
 */
 
 file1_name = argv[argno-2];
 file2_name = argv[argno-1];
 
 /* Open the files */
 if ((file1_id=H5Fopen(file1_name,H5F_ACC_RDONLY,H5P_DEFAULT))<0 ||
     (file2_id=H5Fopen(file2_name,H5F_ACC_RDONLY,H5P_DEFAULT))<0)
  exit(EXIT_FAILURE);

 if ( dset_only )
	{

		h5diff_dataset(file1_id,file2_id,dset_name);

	}


	/* Close */
	status = H5Fclose(file1_id);
	status = H5Fclose(file2_id);



}



/*-------------------------------------------------------------------------
 * Function: h5diff_dataset
 *
 * Purpose: 
 *
 * Return: Success: 0, Failure: -11
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October 8, 2002
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */



int h5diff_dataset( hid_t file1_id, hid_t file2_id, const char *dset_name )
{

	hid_t   dset1_id, dset2_id; 
	hid_t   space1_id, space2_id; 
	hid_t   type1_id, type2_id;
	hid_t   rank1, rank2; 
	void    *buf1, *buf2;
	hsize_t tot_cnt, tot_cnt1, tot_cnt2;
	hsize_t dims1[32], dims2[32];
	int     i;
	herr_t  status;

/*-------------------------------------------------------------------------
 * open the handles
 *-------------------------------------------------------------------------
 */

	/* Open the datasets */
	if ( (dset1_id = H5Dopen(file1_id,dset_name)) < 0 )
  return -1;

	if ( (dset2_id = H5Dopen(file2_id,dset_name)) < 0 )
  return -1;

	/* Get the datatype */
 if ( (type1_id = H5Dget_type(dset1_id)) < 0 )
  goto out;

	/* Get the datatype */
 if ( (type2_id = H5Dget_type(dset2_id)) < 0 )
  goto out;

	/* Get the dataspace handle */
 if ( (space1_id = H5Dget_space(dset1_id)) < 0 )
  return -1;

 /* Get rank */
 if ( (rank1 = H5Sget_simple_extent_ndims(space1_id)) < 0 )
  return -1;

	/* Get the dataspace handle */
 if ( (space2_id = H5Dget_space(dset2_id)) < 0 )
  return -1;

 /* Get rank */
 if ( (rank2 = H5Sget_simple_extent_ndims(space2_id)) < 0 )
  return -1;

	/* Get dimensions */
 if ( H5Sget_simple_extent_dims(space1_id,dims1,NULL) < 0 )
  goto out;

	/* Get dimensions */
 if ( H5Sget_simple_extent_dims(space2_id,dims2,NULL) < 0 )
  goto out;


/*-------------------------------------------------------------------------
 * compare
 *-------------------------------------------------------------------------
 */


	printf("\n---------------------------\n");
 printf("Dataset Name: %s .... Comparing\n", dset_name);

	if ( rank1 != rank2 )
	{
  goto out;
	}

 tot_cnt1 = 1;
 for (i = 0; i < rank1; i++) 
 {
  tot_cnt1 *= dims1[i];
 }
 
 tot_cnt2 = 1;
 for (i = 0; i < rank2; i++) 
 {
  tot_cnt2 *= dims2[i];
 }

	buf1 = (void *) malloc((unsigned) (tot_cnt1*H5Tget_size(type1_id)));
 buf2 = (void *) malloc((unsigned) (tot_cnt2*H5Tget_size(type2_id)));


 if ( H5Dread(dset1_id,type1_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf1) < 0 )
  goto out;

	if ( H5Dread(dset2_id,type2_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf2) < 0 )
  goto out;
 
 if (tot_cnt1 > tot_cnt2)
  tot_cnt = tot_cnt2;
 else
  tot_cnt = tot_cnt1; 

	array_diff(buf1,buf2,tot_cnt,type1_id);


	free((char *) buf1);
 free((char *) buf2);

/*-------------------------------------------------------------------------
 * close
 *-------------------------------------------------------------------------
 */


out:
 
	/* Close */
	status = H5Dclose(dset1_id);
	status = H5Dclose(dset2_id);
	status = H5Sclose(space1_id);
	status = H5Sclose(space2_id);
	status = H5Tclose(type1_id);
	status = H5Tclose(type2_id);
 
	return 0;

}



/*-------------------------------------------------------------------------
 * Function: array_diff
 *
 * Purpose: 
 *
 * Return: Success: 0, Failure: -11
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October 8, 2002
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

 
void array_diff( void *buf1, void *buf2, hsize_t tot_cnt, hid_t type_id )
{

#if 0
 char   *i1ptr1, *i1ptr2;
 short  *i2ptr1, *i2ptr2;
 float  *fptr1, *fptr2;
 double *dptr1, *dptr2;
#endif

	int    *i4ptr1, *i4ptr2;
	int i;

	H5T_class_t type_class;
	size_t      type_size;

	/* Get the class. */
 type_class = H5Tget_class( type_id );

 /* Get the size. */
 type_size = H5Tget_size( type_id );


 switch(type_class)
 {
  case H5T_INTEGER:


			switch(type_size)
			{
    case 4:

					 i4ptr1 = (int *) buf1;
      i4ptr2 = (int *) buf2;
      for ( i = 0; i < tot_cnt; i++)
      {
        if (*i4ptr1 != *i4ptr2)
        {
         printf("Index: %d,   File1: %d,   File2: %d\n", i, *i4ptr1, *i4ptr2);
        }                                               
        i4ptr1++;  i4ptr2++;
      }



					break;

			} /*switch*/


   
   break; /* H5T_INTEGER */


			
	} /*switch*/
   
 
  
}





/*-------------------------------------------------------------------------
 * do some test files 
 *-------------------------------------------------------------------------
 */

int do_test_files()
{

	hid_t   file_id; 
 hid_t   dataset_id;
 hid_t   space_id;  
 hsize_t dims1[1] = { 5 };
 int     data1[5] = {1,2,3,4,5};
	hsize_t dims2[1] = { 5 };
 int     data2[5] = {6,7,8,9,10};
 herr_t  status;

/*-------------------------------------------------------------------------
 * Create one file
 *-------------------------------------------------------------------------
 */
  
 /* Create a file */
 file_id = H5Fcreate ("h5diff_test1.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT); 

 /* Create a data space  */
 space_id = H5Screate_simple(1,dims1,NULL);

 /* Create a dataset "dset" */
 dataset_id = H5Dcreate(file_id,"dset",H5T_NATIVE_INT,space_id,H5P_DEFAULT);
  
 /* Write the data */
	status = H5Dwrite(dataset_id,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,data1);

	/* Close */
	status = H5Dclose(dataset_id);
 status = H5Sclose(space_id);
	status = H5Fclose(file_id);

/*-------------------------------------------------------------------------
 * Create another file
 *-------------------------------------------------------------------------
 */

	/* Create a file */
 file_id = H5Fcreate ("h5diff_test2.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT); 

 /* Create a data space  */
 space_id = H5Screate_simple(1,dims2,NULL);

 /* Create a dataset "dset" */
 dataset_id = H5Dcreate(file_id,"dset",H5T_NATIVE_INT,space_id,H5P_DEFAULT);

	/* Write the data */
	status = H5Dwrite(dataset_id,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,data2);

	/* Close */
	status = H5Dclose(dataset_id);
 status = H5Sclose(space_id);
	status = H5Fclose(file_id);


	return 0;


}
