/******************************************************************
 * CreateFile.java (for HDF5 tutorial lesson 4)
 *
 *   -- Creating a HDF5 file
 *      (a java conversion from h5_crtfile.c)
 *
 ******************************************************************/

import ncsa.hdf.hdf5lib.*;
import ncsa.hdf.hdf5lib.exceptions.*;

public class CreateFile
{    
   public static void main(String []argv) 
   {  
      final String FILE = "file.h5";
      int file_id = -1;    // file identifier 
      int status = -1;

      file_id = H5Fcreate_wrap (FILE, HDF5Constants.H5F_ACC_TRUNC,
				HDF5Constants.H5P_DEFAULT, 
				HDF5Constants.H5P_DEFAULT);
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
	     ("CreateFile.H5Fcreate_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateFile.H5Fcreate_wrap() with other Exception: " 
	      + e.getMessage());
      }

      System.out.println ("\nThe file name is: " + name);
      System.out.println ("The file ID is: " + file_id);

      return file_id;
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
	     ("CreateFile.H5Fclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateFile.H5Fclose_wrap() with other exception: " 
	      + e.getMessage());
      }
      return status;
   }
}
          
 
