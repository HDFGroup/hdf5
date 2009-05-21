
#include <stdlib.h>
#include "hdf5.h"
#include "hdf5_hl.h"

#define filename "NPOESS_EDR.h5"
#define DIM0            4
#define DIM1            7
#define num_flags       4

int main(void)
{
  int qf_data[DIM0][DIM1][num_flags];
  hid_t qf_dset;
  unsigned int offset[4] = {0,2,4,6};
  unsigned int length[4] = {2,2,2,2};
  hid_t file, dset, space;
  int ndims;
  herr_t status;
  int rank = 2;
  hsize_t dims[2] =  {DIM0,DIM1};
  hid_t file_id, space_id, dset_id, group_id;
  unsigned char wdata[DIM0][DIM1];
  int i, j, k, icnt;
  
  /*
   * Initialize data.  We will manually fill four 2-bit integers into
   * each unsigned char data element.
   */
  for (i=0; i<DIM0; i++)
    for (j=0; j<DIM1; j++) {
      wdata[i][j] = 0;
      wdata[i][j] |= (i * j - j) & 0x03;          /* Field "A" */
      wdata[i][j] |= (i & 0x03) << 2;             /* Field "B" */
      wdata[i][j] |= (j & 0x03) << 4;             /* Field "C" */
      wdata[i][j] |= ( (i + j) & 0x03 ) <<6;      /* Field "D" */
    }
  /*
   * Create file with default file access and file creation properties.
   */
  file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  
  space_id = H5Screate_simple(rank, dims, NULL);
  
  dset_id = H5Dcreate2(file_id, "Granule 1",  H5T_NATIVE_UCHAR, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  
  /*
   * Write data to the dataset.
   */
  status = H5Dwrite(dset_id, H5T_NATIVE_UCHAR, H5S_ALL , H5S_ALL, H5P_DEFAULT,wdata);
  status = H5Dclose(dset_id);
  status = H5Sclose(space_id);
  status = H5Fclose(file_id);
  
  /*
   * Open NPOESS product file and a granule dataset.
   */
  file = H5Fopen (filename, H5F_ACC_RDONLY, H5P_DEFAULT);
  qf_dset = H5Dopen (file, "Granule 1",H5P_DEFAULT );
  
  /*
   * Get dataspace and allocate memory for read buffer. Quality flags dataset
   * has the same dimensionality as corresponding product dataset;
   * we are using its dimensions for illustration purposes only.
   */
  space = H5Dget_space (qf_dset);
  ndims = H5Sget_simple_extent_dims (space, dims, NULL);
  
  status = H5Sclose (space);
  
  /*
   * For each element read the IST quality flag that takes first two bits and 
   * store it in a char buffer. This selects all the elements (H5S_ALL)
   */
  status = H5LTread_bitfield_value(qf_dset, num_flags, offset, length,
				   H5S_ALL, H5T_NATIVE_UCHAR, qf_data);
  printf("Bit Field:\n");
  for (i = 0; i<DIM0; i++) {
    printf (" [");
    for (j = 0; j<DIM1; j++) {
      printf(" {",i,j);
      for (k = 0; k<num_flags; k++){
	printf(" %d ", qf_data[i][j][k]);
      }
      printf("} ");
    }
    printf("]\n");
  }
  
}
