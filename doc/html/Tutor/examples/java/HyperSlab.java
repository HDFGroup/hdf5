/******************************************************************
 * HyperSlab.java (for HDF5 tutorial lesson 12)
 *
 *   -- Writing and reading a hyperslab
 *      (a java conversion from h5_hyperslab.c)
 *
 ******************************************************************/

import ncsa.hdf.hdf5lib.*;
import ncsa.hdf.hdf5lib.exceptions.*;

public class HyperSlab
{
   public static void main (String []argv) 
   {
      final String FILE = "sds.h5";
      final String DATASETNAME = "IntArray";
      final int NX_SUB = 3;                /* hyperslab dimensions */ 
      final int NY_SUB = 4;
      final int NX = 7;                    /* output buffer dimensions */ 
      final int NY = 7; 
      final int NZ = 3; 
      final int RANK = 2;
      final int RANK_OUT = 3;
      final int X = 5;                     /* dataset dimensions */
      final int Y = 6;
  
      long[] dimsf = new long[2];          /* dataset dimensions */
      int[][] data = new int[X][Y];        /* data to write */

    /* 
     * Data  and output buffer initialization. 
     */
      int file, dataset;                    /* handles */
      int dataspace;   
      int memspace; 
      long[] dimsm = new long[3];           /* memory space dimensions */
      long[] dims_out = new long[2];        /* dataset dimensions */      
      int status;                             
      
      int[][][] data_out = new int[NX][NY][NZ]; /* output buffer */
      
      long[] count = new long[2];      /* size of the hyperslab in the file */
      long[] offset = new long[2];     /* hyperslab offset in the file */
      long[] count_out = new long[3];  /* size of the hyperslab in memory */
      long[] offset_out = new long[3]; /* hyperslab offset in memory */
      int i, j, k, status_n, rank;
       
      /*********************************************************  
		 This writes data to the HDF5 file.  
      *********************************************************/  
      
      /* 
       * Data  and output buffer initialization. 
       */
      for (j = 0; j < X; j++) 
      {
	 for (i = 0; i < Y; i++)
	    data[j][i] = i + j;
      }     
      /*
       * 0 1 2 3 4 5 
       * 1 2 3 4 5 6
       * 2 3 4 5 6 7
       * 3 4 5 6 7 8
       * 4 5 6 7 8 9
       */

      /*
       * Create a new file using H5F_ACC_TRUNC access,
       * the default file creation properties, and the default file
       * access properties.
       */
      file = H5Fcreate_wrap (FILE, HDF5Constants.H5F_ACC_TRUNC, 
			     HDF5Constants.H5P_DEFAULT, 
			     HDF5Constants.H5P_DEFAULT);
      
      /*
       * Describe the size of the array and create the data space for fixed
       * size dataset. 
       */
      dimsf[0] = X;
      dimsf[1] = Y;
      dataspace = H5Screate_simple_wrap (RANK, dimsf, null); 

      /*
       * Create a new dataset within the file using defined dataspace and
       * default dataset creation properties.
       */
      dataset = H5Dcreate_wrap 
	  (file, DATASETNAME, H5.J2C (HDF5CDataTypes.JH5T_STD_I32BE), 
	   dataspace, HDF5Constants.H5P_DEFAULT);
      
      /*
       * Write the data to the dataset using default transfer properties.
       */
      status = H5Dwrite_wrap 
	  (dataset, H5.J2C (HDF5CDataTypes.JH5T_NATIVE_INT), 
	   HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
	   HDF5Constants.H5P_DEFAULT, data);
      
      /*
       * Close/release resources.
       */
      H5Sclose_wrap (dataspace);
      H5Dclose_wrap (dataset);
      H5Fclose_wrap (file);
      
      /*************************************************************  

  This reads the hyperslab from the sds.h5 file just 
  created, into a 2-dimensional plane of the 3-dimensional 
  array.

      ************************************************************/  

      for (j = 0; j < NX; j++) 
      {
	 for (i = 0; i < NY; i++) 
	 {
	    for (k = 0; k < NZ ; k++)
               data_out[j][i][k] = 0;
	 }
      } 
 
      /*
       * Open the file and the dataset.
       */
      file = H5Fopen_wrap (FILE, HDF5Constants.H5F_ACC_RDONLY, 
			   HDF5Constants.H5P_DEFAULT);
      dataset = H5Dopen_wrap (file, DATASETNAME);

      dataspace = H5Dget_space_wrap (dataset);    /* dataspace handle */
      rank = H5Sget_simple_extent_ndims_wrap (dataspace);
      status_n = H5Sget_simple_extent_dims_wrap (dataspace, dims_out, null);

      System.out.println ("Rank: " + rank);
      System.out.println ("Dimensions: "+ dims_out[0] + " x " + dims_out[1]);
      
      /* 
       * Define hyperslab in the dataset. 
       */
      offset[0] = 1;
      offset[1] = 2;
      count[0]  = NX_SUB;
      count[1]  = NY_SUB;
      status = H5Sselect_hyperslab_wrap (dataspace, 
					 HDF5Constants.H5S_SELECT_SET, 
					 offset, null, count, null);
      
      /*
       * Define the memory dataspace.
       */
      dimsm[0] = NX;
      dimsm[1] = NY;
      dimsm[2] = NZ;
      memspace = H5Screate_simple_wrap (RANK_OUT, dimsm, null);   
      
      /* 
       * Define memory hyperslab. 
       */
      offset_out[0] = 3;
      offset_out[1] = 0;
      offset_out[2] = 0;
      count_out[0]  = NX_SUB;
      count_out[1]  = NY_SUB;
      count_out[2]  = 1;
      status = H5Sselect_hyperslab_wrap (memspace, 
					 HDF5Constants.H5S_SELECT_SET, 
					 offset_out, null, count_out, null);
      
      /*
       * Read data from hyperslab in the file into the hyperslab in 
       * memory and display.
       */
      status = 
	  H5Dread_wrap (dataset, H5.J2C (HDF5CDataTypes.JH5T_NATIVE_INT), 
			memspace, dataspace, HDF5Constants.H5P_DEFAULT, 
			data_out);

      System.out.println ("Data:");
      for (j = 0; j < NX; j++) 
      {
	 for (i = 0; i < NY; i++) 
	    System.out.print (data_out[j][i][0]);
	 System.out.println ();
      }
      System.out.println ();

      /*
       * 0 0 0 0 0 0 0
       * 0 0 0 0 0 0 0
       * 0 0 0 0 0 0 0
       * 3 4 5 6 0 0 0  
       * 4 5 6 7 0 0 0
       * 5 6 7 8 0 0 0
       * 0 0 0 0 0 0 0
       */
      
      /*
       * Close and release resources.
       */
      H5Dclose_wrap (dataset);
      H5Sclose_wrap (dataspace);
      H5Sclose_wrap (memspace);
      H5Fclose_wrap (file);
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
	     ("HyperSlab.H5Fcreate_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("HyperSlab.H5Fcreate_wrap() with other Exception: " 
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
	     ("HyperSlab.H5Fopen_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("HyperSlab.H5Fopen_wrap() with other Exception: " 
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
	     ("HyperSlab.H5Dopen_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("HyperSlab.H5Dopen_wrap() with other Exception: "
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
	     ("HyperSlab.H5Screate_simple_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("HyperSlab.H5Screate_simple_wrap() with other Exception: "
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
	     ("HyperSlab.H5Dget_space_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("HyperSlab.H5Dget_space_wrap() with other Exception: "
	      + e.getMessage());
      }
      return dataspace_id;
   }


   // Help function for determining the dimensionality (or rank) of 
   // a dataspace
   public static int H5Sget_simple_extent_ndims_wrap (int space_id)
   {
      int rank = -1;  
   
      try 
      {
         // Determine the dimensionality (or rank) of a dataspace.
         rank = H5.H5Sget_simple_extent_ndims (space_id); 
      }
      catch (HDF5Exception hdf5e)
      {
         System.out.println 
	   ("HyperSlab.H5Sget_simple_extent_ndims_wrap() with HDF5Exception: "
	    + hdf5e.getMessage());
      }
      catch (Exception e)
      {
         System.out.println
	 ("HyperSlab.H5Sget_simple_extent_ndims_wrap() with other Exception: "
	  + e.getMessage());
      }
      return rank;
   }


   // Help function for returning the size and maximum sizes of each 
   // dimension of a dataspace through the dims and maxdims parameters.
   public static int H5Sget_simple_extent_dims_wrap (int space_id, 
						     long dims[],
						     long maxdims[]) 
   {
      int dimension_number = -1;  
   
      try 
      {
	 dimension_number = H5.H5Sget_simple_extent_dims (space_id, dims, 
							  maxdims);
      }
      catch (HDF5Exception hdf5e)
      {
         System.out.println 
	   ("HyperSlab.H5Sget_simple_extent_dims_wrap() with HDF5Exception: "
	    + hdf5e.getMessage());
      }
      catch (Exception e)
      {
         System.out.println
	  ("HyperSlab.H5Sget_simple_extent_dims_wrap() with other Exception: "
	   + e.getMessage());
      }
      return dimension_number;
   }


   // Help function for selecting a hyperslab region to add to the 
   // current selected region for the dataspace specified by space_id. 
   public static int H5Sselect_hyperslab_wrap (int space_id, int op,
					       long start[], long stride[],
					       long count[], long block[])
    {
      int status = -1;  
   
      try 
      {
	  status = H5.H5Sselect_hyperslab (space_id, op, start, stride, 
					   count, block);
      }
      catch (HDF5Exception hdf5e)
      {
         System.out.println 
	     ("HyperSlab.H5Sselect_hyperslab_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
         System.out.println
	     ("HyperSlab.H5Sselect_hyperslab_wrap() with other Exception: "  
	      + e.getMessage());
      }
      return status;
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
	     ("HyperSlab.H5Dcreate_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("HyperSlab.H5Dcreate_wrap() with other Exception: "
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
	     ("HyperSlab.H5Dwrite_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("HyperSlab.H5Dwrite_wrap() with other exception: " 
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
	     ("HyperSlab.H5Dread_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("HyperSlab.H5Dread_wrap() with other exception: " 
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
	     ("HyperSlab.H5Sclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
         System.out.println 
	     ("HyperSlab.H5Sclose_wrap() with other exception: " 
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
	     ("HyperSlab.H5Dclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("HyperSlab.H5Dclose_wrap() with other exception: " 
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
	     ("HyperSlab.H5Fclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("HyperSlab.H5Fclose_wrap() with other exception: " 
	      + e.getMessage());
      }
      return status;
   }
}
