/******************************************************************
 * CreateGroup.java (for HDF5 tutorial lesson 8)
 *
 *   -- Creating and closing a group
 *      (a java conversion from h5_crtgrp.c)
 *
 ******************************************************************/

import ncsa.hdf.hdf5lib.*;
import ncsa.hdf.hdf5lib.exceptions.*;

public class CreateGroup
{  
   public static void main(String []argv) 
   {
      final String FILE = "group.h5";
      int file_id = -1;       // file identifier 
      int group_id = -1;      // group identifier
      int status = -1;
   
      // Create a new file using default properties.
      file_id = H5Fcreate_wrap (FILE, HDF5Constants.H5F_ACC_TRUNC, 
				HDF5Constants.H5P_DEFAULT, 
				HDF5Constants.H5P_DEFAULT);

      // Create a group named "/MyGroup" in the file.
      group_id = H5Gcreate_wrap (file_id, "/MyGroup", 0);

      // Close the group.
      status = H5Gclose_wrap (group_id);

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
	     ("CreateGroup.H5Fcreate_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateGroup.H5Fcreate_wrap() with other Exception: " 
	      + e.getMessage());
      }
      return file_id;
   }  


   // Help function for creating a group named "/MyGroup" in the file.
   public static int H5Gcreate_wrap (int loc_id, String name, int size_hint)
   {
      int group_id = -1;    // group identifier 
      try 
      {
         // Create a group
         group_id = H5.H5Gcreate (loc_id, name, size_hint);

      }
      catch (HDF5Exception hdf5e)
      {
         System.out.println 
	     ("CreateGroup.H5Gcreate_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
         System.out.println 
	     ("CreateGroup.H5Gcreate_wrap() with other Exception: " 
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
	     ("CreateGroup.H5Gclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateGroup.H5Gclose_wrap() with other exception: " 
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
	     ("CreateGroup.H5Fclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateGroup.H5Fclose_wrap() with other exception: " 
	      + e.getMessage());
      }
      return status;
   }
}
