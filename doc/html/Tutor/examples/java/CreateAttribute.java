/******************************************************************
 * CreateAttribute.java (for HDF5 tutorial lesson 7)
 *
 *   -- Creating and Writing a dataset attribute
 *      (a java conversion from h5_crtatt.c)
 *
 ******************************************************************/

import ncsa.hdf.hdf5lib.*;
import ncsa.hdf.hdf5lib.exceptions.*;

public class CreateAttribute
{
   public static void main(String []argv) 
   {
      final String FILE = "dset.h5";
      int file_id = -1;       // file identifier 
      int dataset_id = -1;    // dataset identifier
      int attribute_id = -1;
      int dataspace_id = -1;  // dataspace identifier   
      long[] dims = new long[1];
      int[] attr_data = new int[2];
      int status = -1;
       
      // Initialize the attribute data.
      attr_data[0] = 100;
      attr_data[1] = 200;

      // Open an existing file.
      file_id = H5Fopen_wrap (FILE, HDF5Constants.H5F_ACC_RDWR, 
			      HDF5Constants.H5P_DEFAULT);
       
      // Open an existing dataset.
      dataset_id = H5Dopen_wrap (file_id, "/dset");

      // Create the data space for the attribute.
      dims[0] = 2;
      dataspace_id = H5Screate_simple_wrap (1, dims, null);

      // Create a dataset attribute. 
      attribute_id = H5Acreate_wrap 
	  (dataset_id, "attr", 
	   H5.J2C (HDF5CDataTypes.JH5T_STD_I32BE), 
	   dataspace_id, HDF5Constants.H5P_DEFAULT);
        
      // Write the attribute data. 
      status = H5Awrite_wrap 
	  (attribute_id,    
	   H5.J2C (HDF5CDataTypes.JH5T_NATIVE_INT), 
	   attr_data);
      
      // Close the attribute. 
      status = H5Aclose_wrap (attribute_id);
      
      // Close the dataspace. 
      status = H5Sclose_wrap (dataspace_id);
      
      // Close to the dataset.
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
	     ("CreateAttribute.H5Fopen_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateAttribute.H5Fopen_wrap() with other Exception: " 
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
	     ("CreateAttribute.H5Dopen_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateAttribute.H5Dopen_wrap() with other Exception: "
	      + e.getMessage());
      }
      return dataset_id;
   }
    

   // Create the data space for the attribute.
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
	     ("CreateAttribute.H5Screate_simple_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
	 
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateAttribute.H5Screate_simple_wrap() with other Exception: "
			     + e.getMessage());
      }
      return dataspace_id;   
   }


   // Help function for creating a dataset attribute. 
   public static int H5Acreate_wrap (int loc_id, String name, int type_id, 
				     int space_id, int create_plist)
   {
      int attribute_id = -1;  // attribute identifier   
   
      try 
      {
	 // Create the dataset 
	 attribute_id = H5.H5Acreate (loc_id, name, type_id, space_id, 
				      create_plist);
      }
      catch (HDF5Exception hdf5e)
      {
	 System.out.println 
	     ("CreateAttribute.H5Acreate_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
       }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateAttribute.H5Acreate_wrap() with other Exception: "
	      + e.getMessage());
      }
      return attribute_id;
   }


   // Help function for writing the attribute data.
   public static int H5Awrite_wrap (int attr_id, int mem_type_id,
				    Object buf)
   {
      int status = -1;
      
      try 
      {
	 // Write the attribute data.
	 status = H5.H5Awrite (attr_id, mem_type_id, buf);
      }
      catch (HDF5Exception hdf5e)
      {
	 System.out.println 
	     ("CreateAttribute.H5Awrite_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateAttribute.H5Awrite_wrap() with other exception: " 
	      + e.getMessage());
      }
      return status;
   }
 

   // Help function for closing the attribute
   public static int H5Aclose_wrap (int attribute_id)
   {
      int status = -1;
      
      try 
      {
	 // Close the dataset
	 status = H5.H5Aclose (attribute_id);
      }
      catch (HDF5Exception hdf5e)
      {
	 System.out.println 
	     ("CreateAttribute.H5Aclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateAttribute.H5Aclose_wrap() with other exception: " 
	      + e.getMessage());
      }
      return status;
   }
 

   // Help function for closing the dataset
   public static int H5Dclose_wrap (int dataset_id)
   {
      int status = -1;
      
      try 
      {
	 // Close the dataset
	 status = H5.H5Dclose (dataset_id);
      }
      catch (HDF5Exception hdf5e)
      {
	 System.out.println 
	     ("CreateAttribute.H5Dclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateAttribute.H5Dclose_wrap() with other exception: " 
	      + e.getMessage());
      }
      return status;
   }
 

   // Help function for closing the dataspace
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
	     ("CreateAttribute.H5Sclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateAttribute.H5Sclose_wrap() with other exception: " 
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
	     ("CreateAttribute.H5Fclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateAttribute.H5Fclose_wrap() with other exception: " 
	      + e.getMessage());
      }
      return status;
   }  
}
