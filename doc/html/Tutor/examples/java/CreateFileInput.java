/******************************************************************
 * CreateFileInput.java (for HDF5 tutorial Lesson 4)
 *
 *   -- Creating a HDF5 file
 *      (another java conversion from h5_crtfile.c, give user two options:
 *       one for library path and one for file name, if user chooses 
 *       nothing, then the default file name is used.)
 *
 ******************************************************************/

import java.lang.System;
import java.util.*;
import ncsa.hdf.hdf5lib.*;
import ncsa.hdf.hdf5lib.exceptions.*;

public class CreateFileInput
{    
   // The run command should be like:
   // "./runCreateFileInput -l /usr/lib/hdf5.dll -f ./open.h5"
   public static void main(String []argv) 
   {  
      int file_id = -1;    // file identifier 
      int status = -1;
      String libpath = null;
      String filename = null;

      for (int i = 0; i < argv.length; i++)
      {
	 if ("-l".equalsIgnoreCase (argv[i]))
	    libpath = argv[++i];
	 
	 if ("-f".equalsIgnoreCase (argv[i]))
	    filename = argv[++i];
      }
     
      if (libpath != null)
      { 
	  Properties pros = System.getProperties ();
	  pros.put (H5.H5PATH_PROPERTY_KEY, libpath);
	  
	  /*
	  this function call could be used in Java 1.2
	  System.setProperty (H5.H5PATH_PROPERTY_KEY, libpath);
	  */
      }
      
      if (filename == null)
      {	 
	 filename = "file.h5"; // if no input file name, use the default name
      }

      file_id = H5Fcreate_wrap (filename, 
				HDF5Constants.H5F_ACC_TRUNC,
				HDF5Constants.H5P_DEFAULT, 
				HDF5Constants.H5P_DEFAULT);
      status = H5Fclose_wrap (filename, file_id);
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
	     ("CreateFileInput.H5Fcreate_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateFileInput.H5Fcreate_wrap() with other Exception: " 
	      + e.getMessage());
      }

      System.out.println ("\nThe file name is: " + name);
      System.out.println ("The file ID is: " + file_id);

      return file_id;
   }  


   // Help function for terminating access to the file. 
   public static int H5Fclose_wrap (String name, int file_id)
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
	     ("CreateFileInput.H5Fclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateFileInput.H5Fclose_wrap() with other exception: " 
	      + e.getMessage());
      }
      
      return status;
   }
}
          
 
