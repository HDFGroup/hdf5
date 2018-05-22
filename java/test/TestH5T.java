/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

package test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.exceptions.HDF5Exception;
import hdf.hdf5lib.exceptions.HDF5LibraryException;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5T {
    @Rule public TestName testname = new TestName();
    private static final String H5_FILE = "testT.h5";
    long H5fid = -1;
    long H5strdid = -1;

    private final void _deleteFile(String filename) {
        File file = null;
        try {
            file = new File(filename);
        }
        catch (Throwable err) {}

        if (file.exists()) {
            try {file.delete();} catch (SecurityException e) {}
        }
    }

    @Before
    public void createH5file() throws NullPointerException, HDF5Exception {
        assertTrue("H5 open ids is 0", H5.getOpenIDCount()==0);
        System.out.print(testname.getMethodName());

        H5fid = H5.H5Fcreate(H5_FILE, HDF5Constants.H5F_ACC_TRUNC,
                HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        assertTrue("H5.H5Fcreate", H5fid > 0);
        H5strdid = H5.H5Tcopy(HDF5Constants.H5T_C_S1);
        assertTrue("H5.H5Tcopy", H5strdid > 0);

        H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
    }

    @After
    public void deleteH5file() throws HDF5LibraryException {
        if (H5strdid >= 0)
            try {H5.H5Tclose(H5strdid);} catch (Exception ex) {}
        if (H5fid > 0)
            try {H5.H5Fclose(H5fid);} catch (Exception ex) {}

        _deleteFile(H5_FILE);
        System.out.println();
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Tequal_type_error() throws Throwable {
        H5.H5Tequal(HDF5Constants.H5T_INTEGER, H5strdid);
    }

    @Test
    public void testH5Tget_class() {
        try {
            int result = H5.H5Tget_class(H5strdid);
            assertTrue("H5.H5Tget_class", result > 0);
            String class_name = H5.H5Tget_class_name(result);
            assertTrue("H5.H5Tget_class", class_name.compareTo("H5T_STRING")==0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Tget_class: " + err);
        }
    }

    @Test
    public void testH5Tget_size() {
        long dt_size = -1;

        try {
            dt_size = H5.H5Tget_size(H5strdid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Tget_size:H5.H5Tget_size " + err);
        }
        assertTrue("testH5Tget_size", dt_size > 0);
    }

    @Test
    public void testH5Tset_size() {
        long dt_size = 5;

        try {
            H5.H5Tset_size(H5strdid, dt_size);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Tset_size:H5.H5Tset_size " + err);
        }
        try {
            dt_size = H5.H5Tget_size(H5strdid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Tget_size:H5.H5Tget_size " + err);
        }
        assertTrue("testH5Tget_size", dt_size == 5);
    }

    @Test
    public void testH5Tarray_create() {
       long filetype_id = -1;
       long[] adims = { 3, 5 };

       try {
           filetype_id = H5.H5Tarray_create(HDF5Constants.H5T_STD_I64LE, 2, adims);
           assertTrue("testH5Tarray_create", filetype_id >= 0);
       }
       catch (Throwable err) {
           err.printStackTrace();
           fail("testH5Tarray_create.H5Tarray_create " + err);
       }
       finally {
           if (filetype_id >= 0)
               try {H5.H5Tclose(filetype_id);} catch (Exception ex) {}
       }
    }

    @Test
    public void testH5Tget_array_ndims() {
       long filetype_id = -1;
       int ndims = 0;
       long[] adims = { 3, 5 };

       try {
           filetype_id = H5.H5Tarray_create(HDF5Constants.H5T_STD_I64LE, 2, adims);
       }
       catch (Throwable err) {
           err.printStackTrace();
           fail("testH5Tarray_create.H5Tarray_create " + err);
       }
       assertTrue("testH5Tget_array_ndims:H5Tarray_create", filetype_id >= 0);
       try {
           ndims = H5.H5Tget_array_ndims(filetype_id);
           assertTrue("testH5Tget_array_ndims", ndims == 2);
       }
       catch (Throwable err) {
           err.printStackTrace();
           fail("testH5Tget_array_ndims.H5Tget_array_ndims " + err);
       }
       finally {
           if (filetype_id >= 0)
               try {H5.H5Tclose(filetype_id);} catch (Exception ex) {}
       }
    }

    @Test
    public void testH5Tget_array_dims() {
       long filetype_id = -1;
       int ndims = 0;
       long[] adims = { 3, 5 };
       long[] rdims = new long[2];

       try {
           filetype_id = H5.H5Tarray_create(HDF5Constants.H5T_STD_I64LE, 2, adims);
       }
       catch (Throwable err) {
           err.printStackTrace();
           fail("testH5Tarray_create.H5Tarray_create " + err);
       }
       assertTrue("testH5Tget_array_dims:H5Tarray_create", filetype_id >= 0);
       try {
           ndims = H5.H5Tget_array_dims(filetype_id, rdims);
           assertTrue("testH5Tget_array_dims", ndims == 2);
           assertTrue("testH5Tget_array_dims", adims[0] == rdims[0]);
           assertTrue("testH5Tget_array_dims", adims[1] == rdims[1]);
       }
       catch (Throwable err) {
           err.printStackTrace();
           fail("testH5Tget_array_dims.H5Tget_array_dims " + err);
       }
       finally {
           if (filetype_id >= 0)
               try {H5.H5Tclose(filetype_id);} catch (Exception ex) {}
       }
    }

    @Test
    public void testH5Tenum_functions() {
        long       filetype_id =-1;
        String    enum_type ="Enum_type";
        byte[]    enum_val = new byte[1];
        String    enum_name = null;

        // Create a enumerate datatype
        try {
            filetype_id = H5.H5Tcreate(HDF5Constants.H5T_ENUM, (long)1);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Tenum_functions:H5Tcreate " + err);
        }
        assertTrue("testH5Tenum_functions:H5Tcreate", filetype_id >= 0);
        try {
            enum_val[0]=10;
            H5.H5Tenum_insert(filetype_id, "RED", enum_val);
            enum_val[0]=11;
            H5.H5Tenum_insert(filetype_id, "GREEN", enum_val);
            enum_val[0]=12;
            H5.H5Tenum_insert(filetype_id, "BLUE", enum_val);
            enum_val[0]=13;
            H5.H5Tenum_insert(filetype_id, "ORANGE", enum_val);
            enum_val[0]=14;
            H5.H5Tenum_insert(filetype_id, "YELLOW", enum_val);

            // Query member number and member index by member name, for enumeration type.
            assertTrue("Can't get member number", H5.H5Tget_nmembers(filetype_id) == 5);
            assertTrue("Can't get correct index number", H5.H5Tget_member_index(filetype_id, "ORANGE") == 3);

            // Commit enumeration datatype and close it */
            H5.H5Tcommit(H5fid, enum_type, filetype_id, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);

            H5.H5Tclose(filetype_id);

            // Open the dataytpe for query
            filetype_id = H5.H5Topen(H5fid, enum_type, HDF5Constants.H5P_DEFAULT);
            assertTrue("testH5Tenum_functions:H5Tcreate", filetype_id >= 0);

            // Query member number and member index by member name, for enumeration type
            assertTrue("Can't get member number", H5.H5Tget_nmembers(filetype_id) == 5);
            assertTrue("Can't get correct index number", H5.H5Tget_member_index(filetype_id, "ORANGE") == 3);

            // Query member value by member name, for enumeration type
            H5.H5Tenum_valueof (filetype_id, "ORANGE", enum_val);
            assertTrue("Incorrect value for enum member", enum_val[0]==13);

            // Query member value by member index, for enumeration type
            H5.H5Tget_member_value (filetype_id, 2, enum_val);
            assertTrue("Incorrect value for enum member", enum_val[0]==12);

            // Query member name by member value, for enumeration type
            enum_val[0] = 14;
            enum_name = H5.H5Tenum_nameof(filetype_id, enum_val, 16);
            assertTrue("Incorrect name for enum member", enum_name.compareTo("YELLOW")==0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Tenum_functions:query " + err);
        }
        finally {
            if (filetype_id >= 0)
                try {H5.H5Tclose(filetype_id);} catch (Exception ex) {}
        }
    }

    @Test
    public void testH5Tenum_create_functions() {
        long      filetype_id = -1;
        byte[]    enum_val = new byte[1];

        // Create a enumerate datatype
        try {
            filetype_id = H5.H5Tenum_create(HDF5Constants.H5T_NATIVE_INT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Tenum_create_functions:H5Tcreate " + err);
        }
        assertTrue("testH5Tenum_create_functions:H5Tcreate", filetype_id >= 0);
        try {
            enum_val[0]=10;
            H5.H5Tenum_insert(filetype_id, "RED", enum_val);
            enum_val[0]=11;
            H5.H5Tenum_insert(filetype_id, "GREEN", enum_val);
            enum_val[0]=12;
            H5.H5Tenum_insert(filetype_id, "BLUE", enum_val);
            enum_val[0]=13;
            H5.H5Tenum_insert(filetype_id, "ORANGE", enum_val);
            enum_val[0]=14;
            H5.H5Tenum_insert(filetype_id, "YELLOW", enum_val);

            // Query member number and member index by member name, for enumeration type.
            assertTrue("Can't get member number", H5.H5Tget_nmembers(filetype_id) == 5);
            assertTrue("Can't get correct index number", H5.H5Tget_member_index(filetype_id, "ORANGE") == 3);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Tenum_create_functions:H5Tget_nmembers " + err);
        }
        finally {
            if (filetype_id >= 0)
                try {H5.H5Tclose(filetype_id);} catch (Exception ex) {}
        }
    }

    @Test
    public void testH5Topaque_functions() {
        long       filetype_id = -1;
        String    opaque_name = null;

        // Create a opaque datatype
        try {
            filetype_id = H5.H5Tcreate(HDF5Constants.H5T_OPAQUE, (long)4);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Topaque_functions:H5Tcreate " + err);
        }
        assertTrue("testH5Topaque_functions:H5Tcreate", filetype_id >= 0);

        try {
            H5.H5Tset_tag(filetype_id, "opaque type");
            opaque_name = H5.H5Tget_tag(filetype_id);
            assertTrue("Incorrect tag for opaque type", opaque_name.compareTo("opaque type")==0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Topaque_functions:H5Tset_get_tag " + err);
        }
        finally {
            if (filetype_id >= 0)
                try {H5.H5Tclose(filetype_id);} catch (Exception ex) {}
        }
    }

    @Test
    public void testH5Tvlen_create() {
        long filetype_id = -1;

       try {
           filetype_id = H5.H5Tvlen_create(HDF5Constants.H5T_C_S1);
           assertTrue("testH5Tvlen_create", filetype_id >= 0);

           // Check if datatype is VL type
           int vlclass = H5.H5Tget_class(filetype_id);
           assertTrue("testH5Tvlen_create:H5Tget_class", vlclass == HDF5Constants.H5T_VLEN);
           assertFalse("testH5Tis_variable_str:H5Tget_class", vlclass == HDF5Constants.H5T_STRING);
       }
       catch (Throwable err) {
           err.printStackTrace();
           fail("testH5Tvlen_create.H5Tvlen_create " + err);
       }
       finally {
           if (filetype_id >= 0)
               try {H5.H5Tclose(filetype_id);} catch (Exception ex) {}
       }
    }

    @Test
    public void testH5Tis_variable_str() {
       long filetype_id = -1;

       try {
           filetype_id = H5.H5Tcopy(HDF5Constants.H5T_C_S1);
           assertTrue("testH5Tis_variable_str.H5Tcopy: ", filetype_id >= 0);

           // Convert to variable-length string
           H5.H5Tset_size(filetype_id, HDF5Constants.H5T_VARIABLE);

           // Check if datatype is VL string
           int vlclass = H5.H5Tget_class(filetype_id);
           assertTrue("testH5Tis_variable_str:H5Tget_class", vlclass == HDF5Constants.H5T_STRING);
           assertFalse("testH5Tvlen_create:H5Tget_class", vlclass == HDF5Constants.H5T_VLEN);

           assertTrue("testH5Tis_variable_str:H5Tis_variable_str", H5.H5Tis_variable_str(filetype_id));

           // Verify that the class detects as a string
           assertTrue("testH5Tis_variable_str:H5Tdetect_class", H5.H5Tdetect_class(filetype_id, HDF5Constants.H5T_STRING));
       }
       catch (Throwable err) {
           err.printStackTrace();
           fail("testH5Tis_variable_str " + err);
       }
       finally {
           if (filetype_id >= 0)
               try {H5.H5Tclose(filetype_id);} catch (Exception ex) {}
       }
    }

    @Test
    public void testH5Tcompound_functions() {
        long       filetype_id =-1;

        // Create a compound datatype
        try {
            filetype_id = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND, (long)16);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Tcompound_functions:H5Tcreate " + err);
        }
        assertTrue("testH5Tcompound_functions:H5Tcreate", filetype_id >= 0);
        try {
            H5.H5Tinsert(filetype_id, "Lon", 0, HDF5Constants.H5T_NATIVE_DOUBLE);
            H5.H5Tinsert(filetype_id, "Lat", 8, HDF5Constants.H5T_NATIVE_DOUBLE);

            // Query member number and member index by member name, for enumeration type.
            assertTrue("Can't get member number", H5.H5Tget_nmembers(filetype_id) == 2);
            assertTrue("Can't get correct index number", H5.H5Tget_member_index(filetype_id, "Lat") == 1);

            // We started to support this function for compound type in 1.8.6 release.
            int order = H5.H5Tget_order(filetype_id);
            assertFalse("Can't get order for compound type.", order == HDF5Constants.H5T_ORDER_ERROR);
            assertTrue("Wrong order for this type.", (order == HDF5Constants.H5T_ORDER_LE) || (order == HDF5Constants.H5T_ORDER_BE));

            // Make certain that the correct classes can be detected
            assertTrue("Can't get correct class", H5.H5Tdetect_class(filetype_id, HDF5Constants.H5T_COMPOUND));
            assertTrue("Can't get correct class", H5.H5Tdetect_class(filetype_id, HDF5Constants.H5T_FLOAT));
            // Make certain that an incorrect class is not detected
            assertFalse("Can get incorrect class", H5.H5Tdetect_class(filetype_id, HDF5Constants.H5T_TIME));

            // Query member name by member index
            String index_name = H5.H5Tget_member_name (filetype_id, 0);
            assertTrue("Incorrect name for member index", index_name.compareTo("Lon")==0);

            // Query member offset by member no
            long index_offset = H5.H5Tget_member_offset (filetype_id, 1);
            assertTrue("Incorrect offset for member no", index_offset == 8);

            // Query member type by member index
            long index_type = H5.H5Tget_member_type (filetype_id, 0);
            assertTrue("Incorrect type for member index", H5.H5Tequal(HDF5Constants.H5T_NATIVE_DOUBLE, index_type));
            if (index_type >= 0)
                try {H5.H5Tclose(index_type);} catch (Exception ex) {}

        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Tcompound_functions:query " + err);
        }
        finally {
            if (filetype_id >= 0)
                try {H5.H5Tclose(filetype_id);} catch (Exception ex) {}
        }
    }

}
