/******************************************************************
 * CreateDataset.java (for HDF5 tutorial lesson 5)
 *
 *   -- Creating a HDF5 Dataset
 *      (a java conversion from h5_crtdat.c)
 *
 ******************************************************************/

import ncsa.hdf.hdf5lib.*;
import ncsa.hdf.hdf5lib.exceptions.*;

public class CreateDataset
{
   public static void main(String []argv) 
   {
      final String FILE = "dset.h5";
      int file_id = -1;       // file identifier 
      int dataset_id = -1;    // dataset identifier
      int dataspace_id = -1;  // dataspace identifier   
      long[] dims = new long[2];
      int status = -1;
            
      // Create a new file using default properties.
      file_id = H5Fcreate_wrap (FILE, HDF5Constants.H5F_ACC_TRUNC, 
				HDF5Constants.H5P_DEFAULT, 
				HDF5Constants.H5P_DEFAULT);
      
      // Create the data space for the dataset.
      dims[0] = 4;
      dims[1] = 6;
      dataspace_id = H5Screate_simple_wrap (2, dims, null);
      
      // Create the dataset. 
      dataset_id = 
	  H5Dcreate_wrap (file_id, "/dset",
			  H5.J2C (HDF5CDataTypes.JH5T_STD_I32BE),
			  dataspace_id, HDF5Constants.H5P_DEFAULT);
      
      // End access to the dataset and release resources used by it.
      status = H5Dclose_wrap (dataset_id);
      
      // Terminate access to the data space. 
      status = H5Sclose_wrap (dataspace_id);
      
      // Close the file.
      status = H5Fclose_wrap (file_id);
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
	     ("CreateDataset.H5Fcreate_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateDataset.H5Fcreate_wrap() with other Exception: " 
	      + e.getMessage());
      }
      return file_id;
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
	     ("CreateDataset.H5Screate_simple_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateDataset.H5Screate_simple_wrap() with other Exception: "
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
	     ("CreateDataset.H5Dcreate_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateDataset.H5Dcreate_wrap() with other Exception: "
	      + e.getMessage());
      }
      return dataset_id;
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
	     ("CreateDataset.H5Dclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateDataset.H5Dclose_wrap() with other exception: " 
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
	     ("CreateDataset.H5Sclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateDataset.H5Sclose_wrap() with other exception: " 
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
	     ("CreateDataset.H5Fclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateDataset.H5Fclose_wrap() with other exception: " 
	      + e.getMessage());
      }
      return status;
   }
}

