/******************************************************************
 * Compound.java (for HDF5 tutorial lesson 11)
 *
 *   -- Creating a compound data type
 *      (a java conversion from compound.c)
 *
 ******************************************************************/

import ncsa.hdf.hdf5lib.*;
import ncsa.hdf.hdf5lib.exceptions.*;

public class Compound
{
   public static void main (String []argv) 
   {    
      final String FILE = "SDScompound.h5";
      final String DATASETNAME = "ArrayOfStructures";
      final int LENGTH = 10;
      final int RANK = 1;

      /* First structure and dataset */
      /* an array of LENGTH 'complex' numbers */
      byte[] data1 = new byte[LENGTH * 16]; 

      int[] AR = new int[1];
      float[] BR = new float[1];
      double[] CR = new double[1];

      byte [] ARec = new byte[4];
      byte [] BRec = new byte[4];
      byte [] CRec = new byte[8];
      
      int s1_tid;     /* File datatype identifier */

      /* Second structure (subset of s1_t) and dataset*/
      byte[] data2 = new byte[LENGTH * 12]; 
      int s2_tid;    /* Memory datatype handle */
      
      /* Third "structure" ( will be used to read float field of s1) */
      int s3_tid;   /* Memory datatype handle */
      float[] s3 = new float[LENGTH];
      
      int i;
      int file, dataset, space; /* Handles */
      int status;
      long[] dim = new long[1];   /* Dataspace dimensions */
      dim[0] = LENGTH;
    
      /*
       * Initialize the data
       */
      for (i = 0; i < LENGTH; i++) 
      {
	 AR[0] = (int) i;
	 BR[0] = (float) i * i;
	 CR[0] = (double) 1. / (i + 1);

	 ARec = HDFNativeData.intToByte (0, 1, AR);
	 BRec = HDFNativeData.floatToByte (0, 1, BR);
	 CRec = HDFNativeData.doubleToByte (0, 1, CR);

	 System.arraycopy (ARec, 0, data1, (i * 16), 4);
	 System.arraycopy (BRec, 0, data1, (i * 16) + 4, 4); 
	 System.arraycopy (CRec, 0, data1, (i * 16) + 8, 8);
      }
      
      /*
       * Create the data space.
       */
      space = H5Screate_simple_wrap (RANK, dim, null);
      
      /*
       * Create the file.
       */
      file = H5Fcreate_wrap (FILE, HDF5Constants.H5F_ACC_TRUNC, 
			     HDF5Constants.H5P_DEFAULT, 
			     HDF5Constants.H5P_DEFAULT);
      
      /*
       * Create the memory data type. 
       */
      s1_tid = H5Tcreate_wrap (HDF5Constants.H5T_COMPOUND, 16);
      H5Tinsert_wrap (s1_tid, "a_name", 0,  
		      H5.J2C (HDF5CDataTypes.JH5T_NATIVE_INT));
      H5Tinsert_wrap (s1_tid, "b_name", 4,
		      H5.J2C (HDF5CDataTypes.JH5T_NATIVE_FLOAT));
      H5Tinsert_wrap (s1_tid, "c_name", 8, 
		      H5.J2C (HDF5CDataTypes.JH5T_NATIVE_DOUBLE));

      /* 
       * Create the dataset.
       */
      dataset = H5Dcreate_wrap (file, DATASETNAME, s1_tid, 
				space, HDF5Constants.H5P_DEFAULT);

      /*
       * Wtite data to the dataset; 
       */
      status = H5Dwrite_wrap (dataset, s1_tid, 
			      HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL, 
			      HDF5Constants.H5P_DEFAULT, data1);

      /*
       * Release resources
       */
      H5Tclose_wrap (s1_tid);
      H5Sclose_wrap (space);
      H5Dclose_wrap (dataset);
      H5Fclose_wrap (file);
 
      /*
       * Open the file and the dataset.
       */
      file = H5Fopen_wrap (FILE, HDF5Constants.H5F_ACC_RDONLY, 
			   HDF5Constants.H5P_DEFAULT);

      dataset = H5Dopen_wrap (file, DATASETNAME);
      
      /* 
       * Create a data type for s2
       */
      s2_tid = H5Tcreate_wrap (HDF5Constants.H5T_COMPOUND, 12);
      H5Tinsert_wrap (s2_tid, "c_name", 0,  
		      H5.J2C (HDF5CDataTypes.JH5T_NATIVE_DOUBLE));
      H5Tinsert_wrap (s2_tid, "a_name", 8,  
		      H5.J2C (HDF5CDataTypes.JH5T_NATIVE_INT));

      /*
       * Read two fields c and a from s1 dataset. Fields in the file
       * are found by their names "c_name" and "a_name".
       */
      status = H5Dread_wrap (dataset, s2_tid, HDF5Constants.H5S_ALL, 
			     HDF5Constants.H5S_ALL, 
			     HDF5Constants.H5P_DEFAULT, data2);
      
      /*
       * Display the fields.  Convert from bytes into numbers.
       */
      System.out.println ("\nField c : ");     
      for( i = 0; i < LENGTH; i++)  {
	 System.arraycopy (data2, (i*12), CRec, 0, 8);
	 CR = HDFNativeData.byteToDouble(0, 1, CRec);
	 System.out.print (CR[0]+" ");
      }
      System.out.println ();

      System.out.println("\nField a :");
      for( i = 0; i < LENGTH; i++) {
	 System.arraycopy (data2, (i*12)+8, ARec, 0, 4);
	 AR = HDFNativeData.byteToInt(0, 1, ARec);
	 System.out.print (AR[0]+" ");
	}
      System.out.println ();

      /* 
       * Create a data type for s3.
       */
      s3_tid = H5Tcreate_wrap (HDF5Constants.H5T_COMPOUND, 4);
      
      status = 
	  H5Tinsert_wrap (s3_tid, "b_name", 0,  
			  H5.J2C (HDF5CDataTypes.JH5T_NATIVE_FLOAT));
      
      /*
       * Read field b from s1 dataset. Field in the file is found by its name.
       */
      status = H5Dread_wrap (dataset, s3_tid, HDF5Constants.H5S_ALL, 
			     HDF5Constants.H5S_ALL, 
			     HDF5Constants.H5P_DEFAULT, s3);
      
      /*
       * Display the field.  Data is read directly into array of 'float'.
       */
      System.out.println ();
      System.out.println ("Field b :");
      for( i = 0; i < LENGTH; i++) {
	 System.out.print (s3[i]+" ");
	}
      System.out.println ();
      
      /*
       * Release resources
       */
      H5Tclose_wrap (s2_tid);
      H5Tclose_wrap (s3_tid);
      H5Dclose_wrap (dataset);
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
	     ("Compound.H5Fcreate_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("Compound.H5Fcreate_wrap() with other Exception: " 
	      + e.getMessage());
      }
      return file_id;
   }  


   // Help function for adding another member to the compound 
   // datatype datatype_id.
   public static int H5Tinsert_wrap (int type_id, String name,
				     long offset, int field_id)
   {
      int status = -1;  
      try 
      {
         // Adding another member to the compound datatype datatype_id.
	  status = H5.H5Tinsert (type_id, name, offset, field_id);
	  
      }
      catch (HDF5Exception hdf5e)
      {
         System.out.println 
	     ("Compound.H5Tinsert_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
         System.out.println 
	     ("Compound.H5Tinsert_wrap() with HDF5Exception: "
	      + e.getMessage());
      }
      return status;
   }


   // Help function for creating the memory data type.
   public static int H5Tcreate_wrap (int dclass, int size)
   {
      int datatype_id = -1;    // memory data type identifier 
      try 
      {
         // Create the memory data type. 
         datatype_id = H5.H5Tcreate (dclass, size);

      }
      catch (HDF5Exception hdf5e)
      {
         System.out.println 
	     ("Compound.H5Tcreate_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
         System.out.println 
	     ("Compound.H5Tcreate_wrap() with other Exception: " 
	      + e.getMessage());
      }
      return datatype_id;
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
	     ("Compound.H5Fopen_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("Compound.H5Fopen_wrap() with other Exception: " 
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
	     ("Compound.H5Dopen_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("Compound.H5Dopen_wrap() with other Exception: "
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
	     ("Compound.H5Screate_simple_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("Compound.H5Screate_simple_wrap() with other Exception: "
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
	     ("Compound.H5Dcreate_wrap() with HDF5Exception: "
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("Compound.H5Dcreate_wrap() with other Exception: "
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
	     ("Compound.H5Dwrite_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("Compound.H5Dwrite_wrap() with other exception: " 
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
	     ("Compound.H5Dread_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("Compound.H5Dread_wrap() with other exception: " 
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
	      ("Compound.H5Sclose_wrap() with HDF5Exception: " 
	       + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("Compound.H5Sclose_wrap() with other exception: " 
	      + e.getMessage());
      }
      return status;
   }
 

   // Help function for releasing a datatype.
   public static int H5Tclose_wrap (int type_id)
   {
      int status = -1;
      
      try 
      {
         // Releasing a datatype.
         status = H5.H5Tclose (type_id);
      }
      catch (HDF5Exception hdf5e)
      {
         System.out.println 
	     ("Compound.H5Tclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
         System.out.println 
	     ("Compound.H5Tclose_wrap() with other exception: " 
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
	     ("Compound.H5Dclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("Compound.H5Dclose_wrap() with other exception: " 
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
	     ("Compound.H5Fclose_wrap() with HDF5Exception: " 
	      + hdf5e.getMessage());
      }
      catch (Exception e)
      {
	 System.out.println 
	     ("Compound.H5Fclose_wrap() with other exception: " 
	      + e.getMessage());
      }
      return status;
   }  
}
