/***********************************************************************/
/*                                                                     */
/*  PROGRAM:   h5_copy.c                                               */
/*  PURPOSE:   Shows how to use the H5SCOPY function.                  */
/*  DESCRIPTION:                                                       */
/*             This program creates two files, copy1.h5, and copy2.h5. */
/*             In copy1.h5, it creates a 3x4 dataset called 'Copy1',   */
/*             and write 0's to this dataset.                          */
/*             In copy2.h5, it create a 3x4 dataset called 'Copy2',    */
/*             and write 1's to this dataset.                          */
/*             It closes both files, reopens both files, selects two   */
/*             points in copy1.h5 and writes values to them.  Then it  */
/*             does an H5Scopy from the first file to the second, and  */
/*             writes the values to copy2.h5.  It then closes the      */
/*             files, reopens them, and prints the contents of the     */
/*             two datasets.                                           */
/*                                                                     */
/***********************************************************************/
 
#include "hdf5.h"
#define FILE1 "copy1.h5"
#define FILE2 "copy2.h5"

#define RANK  2
#define DIM1  3
#define DIM2  4
#define NUMP  2 

int main (void)
{
     hid_t   file1, file2, dataset1, dataset2;
     hid_t   mid1, mid2, fid1, fid2;
     hsize_t fdim[] = {DIM1, DIM2};
     hsize_t mdim[] = {DIM1, DIM2};
     hsize_t start[2], stride[2], count[2], block[2];
     int buf1[DIM1][DIM2];
     int buf2[DIM1][DIM2];
     int bufnew[DIM1][DIM2];
     int val[] = {53, 59};
     hsize_t marray[] = {2};
     hssize_t coord[NUMP][RANK];
     herr_t ret;
     uint  i, j;

/***********************************************************************/
/*                                                                     */
/* Create two files containing identical datasets. Write 0's to one    */
/* and 1's to the other.                                               */
/*                                                                     */
/***********************************************************************/

     for ( i = 0; i < DIM1; i++ ) 
         for ( j = 0; j < DIM2; j++ )
             buf1[i][j] = 0;

     for ( i = 0; i < DIM1; i++ ) 
         for ( j = 0; j < DIM2; j++ )
             buf2[i][j] = 1;

     file1 = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
     file2 = H5Fcreate(FILE2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

     fid1 = H5Screate_simple (RANK, fdim, NULL);
     fid2 = H5Screate_simple (RANK, fdim, NULL);

     dataset1 = H5Dcreate (file1, "Copy1", H5T_NATIVE_INT, fid1, H5P_DEFAULT);
     dataset2 = H5Dcreate (file2, "Copy2", H5T_NATIVE_INT, fid2, H5P_DEFAULT);

     ret = H5Dwrite(dataset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf1);
     ret = H5Dwrite(dataset2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf2);

     ret = H5Dclose (dataset1);
     ret = H5Dclose (dataset2);

     ret = H5Sclose (fid1);
     ret = H5Sclose (fid2);

     ret = H5Fclose (file1);
     ret = H5Fclose (file2);

/***********************************************************************/
/*                                                                     */
/* Open the two files.  Select two points in one file, write values to */
/* those point locations, then do H5Scopy and write the values to the  */
/* other file.  Close files.                                           */
/*                                                                     */
/***********************************************************************/
 
     file1 = H5Fopen (FILE1, H5F_ACC_RDWR, H5P_DEFAULT);
     file2 = H5Fopen (FILE2, H5F_ACC_RDWR, H5P_DEFAULT);
     dataset1 = H5Dopen (file1, "Copy1");
     dataset2 = H5Dopen (file2, "Copy2");
     fid1 = H5Dget_space (dataset1);
     mid1 = H5Screate_simple(1, marray, NULL); 
     coord[0][0] = 0; coord[0][1] = 3;
     coord[1][0] = 0; coord[1][1] = 1;

     ret = H5Sselect_elements (fid1, H5S_SELECT_SET, NUMP, (const hssize_t **)coord);

     ret = H5Dwrite (dataset1, H5T_NATIVE_INT, mid1, fid1, H5P_DEFAULT, val); 

     fid2 = H5Scopy (fid1);

     ret = H5Dwrite (dataset2, H5T_NATIVE_INT, mid1, fid2, H5P_DEFAULT, val); 

     ret = H5Dclose (dataset1);
     ret = H5Dclose (dataset2);
     ret = H5Sclose (fid1);
     ret = H5Sclose (fid2);
     ret = H5Fclose (file1);
     ret = H5Fclose (file2);
     ret = H5Sclose (mid1);

/***********************************************************************/
/*                                                                     */
/* Open both files and print the contents of the datasets.             */
/*                                                                     */
/***********************************************************************/

     file1 = H5Fopen (FILE1, H5F_ACC_RDWR, H5P_DEFAULT);
     file2 = H5Fopen (FILE2, H5F_ACC_RDWR, H5P_DEFAULT);
     dataset1 = H5Dopen (file1, "Copy1");
     dataset2 = H5Dopen (file2, "Copy2");

     ret = H5Dread (dataset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
                    H5P_DEFAULT, bufnew);
     
     printf ("\nDataset 'Copy1' in file 'copy1.h5' contains: \n");
     for (i=0;i<DIM1; i++) {
        for (j=0;j<DIM2;j++) printf ("%3d  ", bufnew[i][j]);
        printf("\n");
     }

     printf ("\nDataset 'Copy2' in file 'copy2.h5' contains: \n");

     ret = H5Dread (dataset2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
                    H5P_DEFAULT, bufnew);

     for (i=0;i<DIM1; i++) {
        for (j=0;j<DIM2;j++) printf ("%3d  ", bufnew[i][j]);
        printf("\n");
     }
     ret = H5Dclose (dataset1);
     ret = H5Dclose (dataset2);
     ret = H5Fclose (file1);
     ret = H5Fclose (file2);

}
