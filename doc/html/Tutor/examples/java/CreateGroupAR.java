/******************************************************************
 * CreateGroupAR.java (for HDF5 tutorial lesson 9)
 *
 *   -- Creating groups using absolute and relative names.
 *      (a java conversion from h5_crtgrpar.c)
 *
 ******************************************************************/

import ncsa.hdf.hdf5lib.*;
import ncsa.hdf.hdf5lib.exceptions.*;

public class CreateGroupAR
{
   public static void main(String []argv) 
   {
      final String FILE = "groups.h5";
      int file_id = -1;        // file identifier 
      int group1_id = -1;      // group identifier
      int group2_id = -1;  
      int group3_id = -1;
  
      int status = -1;
   
      // Create a new file using default properties.
      file_id = H5Fcreate_wrap (FILE, HDF5Constants.H5F_ACC_TRUNC, 
				HDF5Constants.H5P_DEFAULT, 
				HDF5Constants.H5P_DEFAULT);

      // Create group "MyGroup" in the root group using absolute name.
      group1_id = H5Gcreate_wrap (file_id, "/MyGroup", 0);


      // Create group "Group_A" in group "MyGroup" using absolute name. 
      group2_id = H5Gcreate_wrap (file_id, "/MyGroup/Group_A", 0);
      
      // Create group "Group_B" in group "MyGroup" using relative name. 
      group3_id = H5Gcreate_wrap (group1_id, "Group_B", 0);
   
      // Close groups. 
      status = H5Gclose_wrap (group1_id);
      status = H5Gclose_wrap (group2_id);
      status = H5Gclose_wrap (group3_id);
      
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
	      ("CreateGroupAR.H5Fcreate_wrap() with HDF5Exception: "
	       + hdf5e.getMessage());
      }
      catch (Exception e)
      {
         System.out.println 
	     ("CreateGroupAR.H5Fcreate_wrap() with other Exception: " 
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
	     ("CreateGroupAR.H5Gcreate_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
         System.out.println 
	     ("CreateGroupAR.H5Gcreate_wrap() with other Exception: " 
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
	     ("CreateGroupAR.H5Gclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateGroupAR.H5Gclose_wrap() with other exception: " 
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
	     ("CreateGroupAR.H5Fclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("CreateGroupAR.H5Fclose_wrap() with other exception: " 
	      + e.getMessage());
      }
      return status;
   }  
}
