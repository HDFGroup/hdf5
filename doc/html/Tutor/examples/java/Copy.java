/******************************************************************
 * Copy.java (for HDF5 tutorial lesson 13)
 *
 *   -- Showing how to use the H5SCOPY function.
 *      (a java conversion from h5_copy.c)
 *
 ******************************************************************/

import ncsa.hdf.hdf5lib.*;
import ncsa.hdf.hdf5lib.exceptions.*;

public class Copy
{
 public static void main (String []argv) 
   {
      final String FILE1 = "copy1.h5";
      final String FILE2 = "copy2.h5";
    
      final int RANK = 2;
      final int DIM1 = 3;
      final int DIM2 = 4;
      final int NUMP = 2; 

      int file1, file2, dataset1, dataset2;
      int mid1, mid2, fid1, fid2;
      long[] fdim = new long[2];   
      fdim[0] = DIM1;
      fdim[1] = DIM2;
      long[] mdim = new long[2];   
      fdim[0] = DIM1;
      fdim[1] = DIM2;

      long[] start = new long[2]; 
      long[] stride = new long[2]; 
      long[] count = new long[2]; 
      long[] block = new long[2]; 
      
      int[][] buf1 = new int[DIM1][DIM2];
      int[][] buf2 = new int[DIM1][DIM2];
      int[][] bufnew = new int[DIM1][DIM2];

      int[] val = new int[2];
      val[0] = 53;
      val[1] =  59;

      long[] marray = {2};
      long[][] coord = new long[NUMP][RANK];
      int ret;
      int i, j;
  

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

     file1 = H5Fcreate_wrap (FILE1, HDF5Constants.H5F_ACC_TRUNC, 
			     HDF5Constants.H5P_DEFAULT, 
			     HDF5Constants.H5P_DEFAULT);
     file2 = H5Fcreate_wrap (FILE2, HDF5Constants.H5F_ACC_TRUNC, 
			     HDF5Constants.H5P_DEFAULT, 
			     HDF5Constants.H5P_DEFAULT);

     fid1 = H5Screate_simple_wrap (RANK, fdim, null);
     fid2 = H5Screate_simple_wrap (RANK, fdim, null);

     dataset1 = H5Dcreate_wrap 
	 (file1, "Copy1", H5.J2C (HDF5CDataTypes.JH5T_NATIVE_INT), fid1, 
	  HDF5Constants.H5P_DEFAULT);
    
     dataset2 = H5Dcreate_wrap 
	 (file2, "Copy2", H5.J2C (HDF5CDataTypes.JH5T_NATIVE_INT), fid2, 
	  HDF5Constants.H5P_DEFAULT);


     ret = H5Dwrite_wrap (dataset1, H5.J2C (HDF5CDataTypes.JH5T_NATIVE_INT), 
			  HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL, 
			  HDF5Constants.H5P_DEFAULT, buf1);

     ret = H5Dwrite_wrap (dataset2, H5.J2C (HDF5CDataTypes.JH5T_NATIVE_INT), 
			  HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL, 
			  HDF5Constants.H5P_DEFAULT, buf2);

     ret = H5Dclose_wrap (dataset1);
     ret = H5Dclose_wrap (dataset2);

     ret = H5Sclose_wrap (fid1);
     ret = H5Sclose_wrap (fid2);

     ret = H5Fclose_wrap (file1);
     ret = H5Fclose_wrap (file2);


/***********************************************************************/
/*                                                                     */
/* Open the two files.  Select two points in one file, write values to */
/* those point locations, then do H5Scopy and write the values to the  */
/* other file.  Close files.                                           */
/*                                                                     */
/***********************************************************************/
 
     file1 = H5Fopen_wrap (FILE1, HDF5Constants.H5F_ACC_RDWR, 
			   HDF5Constants.H5P_DEFAULT);
     
     file2 = H5Fopen_wrap (FILE2, HDF5Constants.H5F_ACC_RDWR, 
			   HDF5Constants.H5P_DEFAULT);

     dataset1 = H5Dopen_wrap (file1, "Copy1");
     dataset2 = H5Dopen_wrap (file2, "Copy2");

     fid1 = H5Dget_space_wrap (dataset1);
     mid1 = H5Screate_simple_wrap (1, marray, null); 

     coord[0][0] = 0; coord[0][1] = 3;
     coord[1][0] = 0; coord[1][1] = 1;

     ret = H5Sselect_elements_wrap (fid1, HDF5Constants.H5S_SELECT_SET, 
				    NUMP, coord);

     ret = H5Dwrite_wrap (dataset1, H5.J2C (HDF5CDataTypes.JH5T_NATIVE_INT),  
			  mid1, fid1, HDF5Constants.H5P_DEFAULT, val); 

     fid2 = H5Scopy_wrap (fid1);

     ret = H5Dwrite_wrap (dataset2, H5.J2C (HDF5CDataTypes.JH5T_NATIVE_INT),
			  mid1, fid2, HDF5Constants.H5P_DEFAULT, val); 

     ret = H5Dclose_wrap (dataset1);
     ret = H5Dclose_wrap (dataset2);
     ret = H5Sclose_wrap (fid1);
     ret = H5Sclose_wrap (fid2);
     ret = H5Fclose_wrap (file1);
     ret = H5Fclose_wrap (file2);
     ret = H5Sclose_wrap (mid1);


/***********************************************************************/
/*                                                                     */
/* Open both files and print the contents of the datasets.             */
/*                                                                     */
/***********************************************************************/

     file1 = H5Fopen_wrap (FILE1, HDF5Constants.H5F_ACC_RDWR, 
			   HDF5Constants.H5P_DEFAULT);
     file2 = H5Fopen_wrap (FILE2, HDF5Constants.H5F_ACC_RDWR, 
			   HDF5Constants.H5P_DEFAULT);
     dataset1 = H5Dopen_wrap (file1, "Copy1");
     dataset2 = H5Dopen_wrap (file2, "Copy2");

     ret = H5Dread_wrap (dataset1, H5.J2C (HDF5CDataTypes.JH5T_NATIVE_INT), 
			 HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
			 HDF5Constants.H5P_DEFAULT, bufnew);
     
     System.out.println ("\nDataset 'Copy1' in file 'copy1.h5' contains: ");
  
     for (i = 0;i < DIM1; i++) 
     {
        for (j = 0;j < DIM2; j++) 
	   System.out.print (bufnew[i][j]);
	System.out.println ();
     }

     System.out.println ("\nDataset 'Copy2' in file 'copy2.h5' contains: ");
     
     ret = H5Dread_wrap (dataset2, H5.J2C (HDF5CDataTypes.JH5T_NATIVE_INT), 
			 HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
			 HDF5Constants.H5P_DEFAULT, bufnew);

     for (i = 0;i < DIM1; i++) 
     {
        for (j = 0;j < DIM2; j++) 
	   System.out.print (bufnew[i][j]);
        System.out.println ();
     }

     ret = H5Dclose_wrap (dataset1);
     ret = H5Dclose_wrap (dataset2);
     ret = H5Fclose_wrap (file1);
     ret = H5Fclose_wrap (file2);
   }


   // Help function for creating a new file 
   public static int H5Fcreate_wrap (String name, int flags,
				     int create_id, int access_id)
   {
      int file_id = -1;    // file identifier 
      try 
      {
	 // Create a new file using default file properties.
	 file_id = H5.H5Fcreate (name, flags, create_id, access_id);
      }
      catch (HDF5Exception hdf5e)
      {
	 System.out.println 
	     ("Copy.H5Fcreate_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("Copy.H5Fcreate_wrap() with other Exception: " 
	      + e.getMessage());
      }
      return file_id;
   }  


   // Help function for opening an existing file
   public static int H5Fopen_wrap (String name, int flags, int access_id)
   {
      int file_id = -1;    // file identifier 
      try 
      {
	 // Create a new file using default file properties.
	 file_id = H5.H5Fopen (name, flags, access_id);
      }
      catch (HDF5Exception hdf5e)
      {
	 System.out.println 
	     ("Copy.H5Fopen_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("Copy.H5Fopen_wrap() with other Exception: " 
	      + e.getMessage());
      }
      return file_id;
   }


   // Help function for opening an existing dataset
   public static int H5Dopen_wrap (int loc_id, String name)
   {
      int dataset_id = -1;  // dataset identifier   
   
      try 
      {
	 // Opening an existing dataset
	 dataset_id = H5.H5Dopen (loc_id, name);
      }
      catch (HDF5Exception hdf5e)
      {
	 System.out.println 
	     ("Copy.H5Dopen_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("Copy.H5Dopen_wrap() with other Exception: "
	      + e.getMessage());
      }
      return dataset_id;
   }

    
   // Help function for creating a new simple dataspace and opening it
   // for access
   public static int H5Screate_simple_wrap (int rank, long dims[],
					    long maxdims[])
   {
      int dataspace_id = -1;  // dataspace identifier   
        
      try 
      {
	 // Create the data space for the dataset.
	 dataspace_id = H5.H5Screate_simple (rank, dims, maxdims);
      }
      catch (HDF5Exception hdf5e)
      {
	 System.out.println 
	     ("Copy.H5Screate_simple_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("Copy.H5Screate_simple_wrap() with other Exception: "
	      + e.getMessage());
      }
      return dataspace_id;
   }


   // Help function for getting an identifier for a copy of 
   // the dataspace for a dataset 
   public static int H5Dget_space_wrap (int dataset_id)
   {
      int dataspace_id = -1;

      try 
      {
         // Returning an identifier for a copy of the dataspace for a dataset
         dataspace_id = H5.H5Dget_space (dataset_id);
      }
      catch (HDF5Exception hdf5e)
      {
         System.out.println 
	     ("Copy.H5Dget_space_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("Copy.H5Dget_space_wrap() with other Exception: "
	      + e.getMessage());
      }
      return dataspace_id;
   }

  
   // Help function for selecting array elements to be included in 
   // the selection for the space_id dataspace.
   public static int H5Sselect_elements_wrap (int space_id, int op,
					      int num_elements,
					      long coord2D[][])
   {
      int status = -1;  
   
      try 
      {
	  status = H5.H5Sselect_elements (space_id, op, num_elements,
					  coord2D);
      }
      catch (HDF5Exception hdf5e)
      {
         System.out.println 
	     ("Copy.H5Sselect_elements_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
         System.out.println
	     ("Copy.H5Sselect_elements_wrap() with other Exception: "  
	      + e.getMessage());
      }
      return status;
   }


   // Help function for creating a new dataspace which is an exact 
   // copy of the dataspace identified by space_id.
   public static int H5Scopy_wrap (int space_id)
   {
      int dataspace_id = -1;  
   
      try 
      {
	 dataspace_id = H5.H5Scopy(space_id);
      }
      catch (HDF5Exception hdf5e)
      {
         System.out.println ("Copy.H5Scopy_wrap() with HDF5Exception: "
                             + hdf5e.getMessage());
      }
      catch (Exception e)
      {
         System.out.println ("Copy.H5Scopy_wrap() with other Exception: "
                             + e.getMessage());
      }
      return dataspace_id;
   }


   // Help function for creating a dataset
   public static int H5Dcreate_wrap (int loc_id, String name, int type_id, 
				     int space_id, int create_plist_id)
   {
      int dataset_id = -1;  // dataset identifier   
   
      try 
      {
	 // Create the dataset
	  dataset_id = H5.H5Dcreate (loc_id, name, type_id, space_id, 
				     create_plist_id);
      }
      catch (HDF5Exception hdf5e)
      {
	 System.out.println 
	     ("Copy.H5Dcreate_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("Copy.H5Dcreate_wrap() with other Exception: "
	      + e.getMessage());
      }
      return dataset_id;
   }


  // Help function for writing the dataset
   public static int H5Dwrite_wrap (int dataset_id, int mem_type_id,
				    int mem_space_id, int file_space_id,
				    int xfer_plist_id, Object buf)
   {
      int status = -1;
      
      try 
      {
	 // Write the dataset. 
	 status = H5.H5Dwrite (dataset_id, mem_type_id, mem_space_id, 
			       file_space_id, xfer_plist_id, buf);
      }
      catch (HDF5Exception hdf5e)
      {
	 System.out.println 
	     ("Copy.H5Dwrite_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("Copy.H5Dwrite_wrap() with other exception: " 
	      + e.getMessage());
      }
      return status;
   }


   // Help function for reading the dataset
   public static int H5Dread_wrap (int dataset_id, int mem_type_id,
				   int mem_space_id, int file_space_id,
				   int xfer_plist_id, Object obj)
   {
      int status = -1; 
      
      try 
      {
	 // Read the dataset. 
	 status = H5.H5Dread (dataset_id, mem_type_id, mem_space_id, 
			      file_space_id, xfer_plist_id, obj);
      }
      catch (HDF5Exception hdf5e)
      {
	 System.out.println 
	     ("Copy.H5Dread_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("Copy.H5Dread_wrap() with other exception: " 
	      + e.getMessage());
      }
      return status;
   }
 

   // Help function for terminating access to the data space. 
   public static int H5Sclose_wrap (int dataspace_id)
   {
      int status = -1;
      
      try 
      {
         // Terminate access to the data space. 
         status = H5.H5Sclose (dataspace_id);
      }
      catch (HDF5Exception hdf5e)
      {
         System.out.println 
	     ("Copy.H5Sclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
         System.out.println 
	     ("Copy.H5Sclose_wrap() with other exception: " 
	      + e.getMessage());
      }
      return status;
   }
 
 
   // Help function for ending access to the dataset and releasing 
   // resources used by it.
   public static int H5Dclose_wrap (int dataset_id)
   {
      int status = -1;
      
      try 
      {
	 // End access to the dataset and release resources used by it. 
	 status = H5.H5Dclose (dataset_id);
      }
      catch (HDF5Exception hdf5e)
      {
	 System.out.println 
	     ("Copy.H5Dclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("Copy.H5Dclose_wrap() with other exception: " 
	      + e.getMessage());
      }
      return status;
   }


   // Help function for terminating access to the file. 
   public static int H5Fclose_wrap (int file_id)
   {
      int status = -1;
      
      try 
      {
	 // Terminate access to the file. 
	 status = H5.H5Fclose (file_id);
      }
      catch (HDF5Exception hdf5e)
      { 
	 System.out.println 
	     ("Copy.H5Fclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("Copy.H5Fclose_wrap() with other exception: " 
	      + e.getMessage());
      }
      return status;
   }  
}
