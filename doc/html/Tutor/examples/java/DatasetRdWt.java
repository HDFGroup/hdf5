/******************************************************************
 * DatasetRdWt.java (for HDF5 tutorial lesson 6)
 *
 *   -- Reading and Writing an existing Dataset
 *      (a java conversion from h5_rdwt.c)
 *
 ******************************************************************/

import ncsa.hdf.hdf5lib.*;
import ncsa.hdf.hdf5lib.exceptions.*;

public class DatasetRdWt
{
   public static void main(String []argv) 
   {
      final String FILE = "dset.h5";
      int file_id = -1;       // file identifier 
      int dataset_id = -1;    // dataset identifier
      int status = -1;
      int[][] dset_data = new int[4][6];
      
      // Initialize the dataset.
      for (int i = 0; i < 4; i++)
	 for (int j = 0; j < 6; j++)
	    dset_data[i][j] = i * 6 + j + 1;
     
      // Open an existing file
      file_id = H5Fopen_wrap (FILE, HDF5Constants.H5F_ACC_RDWR, 
			      HDF5Constants.H5P_DEFAULT);
	
      // Open an existing dataset.
      dataset_id = H5Dopen_wrap (file_id, "/dset");	 
      
      // Write the dataset. 
      status = H5Dwrite_wrap 
	  (dataset_id, H5.J2C (HDF5CDataTypes.JH5T_NATIVE_INT), 
	   HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL, 
	   HDF5Constants.H5P_DEFAULT, dset_data);

      status = H5Dread_wrap 
	  (dataset_id, H5.J2C (HDF5CDataTypes.JH5T_NATIVE_INT), 
	   HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL, 
	   HDF5Constants.H5P_DEFAULT, dset_data);

      // Close the dataset.
      status = H5Dclose_wrap (dataset_id);
	 
      // Close the file.
      status = H5Fclose_wrap (file_id);
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
	     ("DatasetRdWt.H5Fopen_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("DatasetRdWt.H5Fopen_wrap() with other Exception: " 
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
	     ("DatasetRdWt.H5Dopen_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("DatasetRdWt.H5Dopen_wrap() with other Exception: "
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
	     ("DatasetRdWt.H5Dwrite_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("DatasetRdWt.H5Dwrite_wrap() with other exception: " 
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
	     ("DatasetRdWt.H5Dread_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("DatasetRdWt.H5Dread_wrap() with other exception: " 
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
	     ("DatasetRdWt.H5Dclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("DatasetRdWt.H5Dclose_wrap() with other exception: " 
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
	     ("DatasetRdWt.H5Fclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("DatasetRdWt.H5Fclose_wrap() with other exception: " 
	      + e.getMessage());
      }
      return status;
   }  
}
