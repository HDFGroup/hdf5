/******************************************************************
 * CreateGroupDataset.java (for HDF5 tutorial lesson 10)
 *
 *   -- Creating a dataset in a particular group
 *      (a java conversion from h5_crtgrpd.c)
 *
 ******************************************************************/

import ncsa.hdf.hdf5lib.*;
import ncsa.hdf.hdf5lib.exceptions.*;

public class CreateGroupDataset
{ 
   public static void main(String []argv) 
   {
      final String FILE = "groups.h5"; 
      int file_id = -1;       // file identifier 
      int group_id = -1;      // group identifier
      int dataset_id;
      int dataspace_id;
      int status = -1;
   
      long[] dims = new long[2];
      int[][] dset1_data = new int[3][3];
      int[][] dset2_data = new int[2][10];
      int i = -1, j = -1;
      
      // Initialize the first dataset.
      for (i = 0; i < 3; i++)
	 for (j = 0; j < 3; j++)
	    dset1_data[i][j] = j + 1;

      // Initialize the second dataset. 
      for (i = 0; i < 2; i++)
	 for (j = 0; j < 10; j++)
	    dset2_data[i][j] = j + 1;

      // Open an existing file.
      file_id = H5Fopen_wrap (FILE, HDF5Constants.H5F_ACC_RDWR, 
                              HDF5Constants.H5P_DEFAULT);

      // Create the data space for the first dataset. 
      dims[0] = 3;
      dims[1] = 3;
      dataspace_id = H5Screate_simple_wrap (2, dims, null);

      // Create a dataset in group "MyGroup". 
      dataset_id = 
	  H5Dcreate_wrap (file_id, "/MyGroup/dset1",
			  H5.J2C (HDF5CDataTypes.JH5T_STD_I32BE),
			  dataspace_id, HDF5Constants.H5P_DEFAULT);

      // Write the first dataset. 
      status = H5Dwrite_wrap 
          (dataset_id, 
           H5.J2C (HDF5CDataTypes.JH5T_NATIVE_INT), 
           HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL, 
           HDF5Constants.H5P_DEFAULT, dset1_data);

      // Close the data space for the first dataset. 
      status = H5Sclose_wrap (dataspace_id);

      // Close the first dataset. 
      status = H5Dclose_wrap (dataset_id);
   
      // Open an existing group of the specified file. 
      group_id = H5Gopen_wrap (file_id, "/MyGroup/Group_A");

      // Create the data space for the second dataset. 
      dims[0] = 2;
      dims[1] = 10;
      dataspace_id = H5Screate_simple_wrap (2, dims, null);
      
      // Create the second dataset in group "Group_A". 
      dataset_id = 
	  H5Dcreate_wrap (group_id, "dset2",
			  H5.J2C (HDF5CDataTypes.JH5T_STD_I32BE),
			  dataspace_id, HDF5Constants.H5P_DEFAULT);

      // Write the second dataset. 
      status = H5Dwrite_wrap 
          (dataset_id, 
           H5.J2C (HDF5CDataTypes.JH5T_NATIVE_INT), 
           HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL, 
           HDF5Constants.H5P_DEFAULT, dset2_data);

      // Close the data space for the second dataset.
      status = H5Sclose_wrap (dataspace_id);

      // Close the second dataset 
      status = H5Dclose_wrap (dataset_id);

      // Close the group.
      status = H5Gclose_wrap (group_id);

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
	     ("CreateGroupDataset.H5Fopen_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateGroupDataset.H5Fopen_wrap() with other Exception: " 
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
	  ("CreateGroupDataset.H5Screate_simple_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	  ("CreateGroupDataset.H5Screate_simple_wrap() with other Exception: "
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
	     ("CreateGroupDataset.H5Dcreate_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateGroupDataset.H5Dcreate_wrap() with other Exception: "
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
	     ("CreateGroupDataset.H5Dwrite_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateGroupDataset.H5Dwrite_wrap() with other exception: " 
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
	     ("CreateGroupDataset.H5Sclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
         System.out.println 
	     ("CreateGroupDataset.H5Sclose_wrap() with other exception: " 
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
	     ("CreateGroupDataset.H5Dclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateGroupDataset.H5Dclose_wrap() with other exception: " 
	      + e.getMessage());
      }
      return status;
   }
 

   // Help function for opening a group 
   public static int H5Gopen_wrap (int loc_id, String name)
   {
      int group_id = -1;    // group identifier 
      try 
      {
         // Create a group
         group_id = H5.H5Gopen (loc_id, name);

      }
      catch (HDF5Exception hdf5e)
      {
         System.out.println 
	     ("CreateGroupDataset.H5Gopen_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
         System.out.println 
	     ("CreateGroupDataset.H5Gopen_wrap() with other Exception: " 
	      + e.getMessage());
      }
      return group_id;
   }


   // Help function for closing the group
   public static int H5Gclose_wrap (int group_id)
   {
      int status = -1;
      
      try 
      {
	 // Close the group
	 status = H5.H5Gclose (group_id);
      }
      catch (HDF5Exception hdf5e)
      {
	 System.out.println 
	     ("CreateGroupDataset.H5Gclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateGroupDataset.H5Gclose_wrap() with other exception: " 
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
	     ("CreateGroupDataset.H5Fclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateGroupDataset.H5Fclose_wrap() with other exception: " 
	      + e.getMessage());
      }
      return status;
   }
}
