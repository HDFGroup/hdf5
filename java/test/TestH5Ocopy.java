/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.exceptions.HDF5Exception;
import hdf.hdf5lib.exceptions.HDF5LibraryException;

import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5Ocopy {
    @Rule
    public TestName testname             = new TestName();
    private static final String FILENAME = "testRefsattribute.h5";
    private static final int DIM_X       = 4;
    private static final int DIM_Y       = 6;
    long H5fid                           = HDF5Constants.H5I_INVALID_HID;
    long H5dsid                          = HDF5Constants.H5I_INVALID_HID;
    long H5did1                          = HDF5Constants.H5I_INVALID_HID;
    long H5did2                          = HDF5Constants.H5I_INVALID_HID;
    long H5gcpl                          = HDF5Constants.H5I_INVALID_HID;
    long H5gid                           = HDF5Constants.H5I_INVALID_HID;
    long H5dsid2                         = HDF5Constants.H5I_INVALID_HID;
    long[] dims                          = {2};

    private final void _deleteFile(String filename)
    {
        File file = new File(filename);

        if (file.exists()) {
            try {
                file.delete();
            }
            catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    private final long _createDataset(long fid, long dsid, String name, long dapl)
    {
        long did = HDF5Constants.H5I_INVALID_HID;
        try {
            did = H5.H5Dcreate(fid, name, HDF5Constants.H5T_STD_I32BE, dsid, HDF5Constants.H5P_DEFAULT,
                               HDF5Constants.H5P_DEFAULT, dapl);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Dcreate: " + err);
        }
        assertTrue("TestH5O._createDataset: ", did >= 0);

        return did;
    }

    private final long _createGroup(long fid, String name)
    {
        long gid = HDF5Constants.H5I_INVALID_HID;
        try {
            H5gcpl = HDF5Constants.H5P_DEFAULT;
            gid    = H5.H5Gcreate(fid, name, HDF5Constants.H5P_DEFAULT, H5gcpl, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Gcreate: " + err);
        }
        assertTrue("TestH5O._createGroup: ", gid >= 0);

        return gid;
    }

    @Before
    public void createH5file() throws NullPointerException, HDF5Exception
    {
        assertTrue("H5 open ids is 0", H5.getOpenIDCount() == 0);
        System.out.print(testname.getMethodName());
        try {
            H5fid   = H5.H5Fcreate(FILENAME, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                                   HDF5Constants.H5P_DEFAULT);
            H5dsid2 = H5.H5Screate(HDF5Constants.H5S_SCALAR);
            H5did1  = _createDataset(H5fid, H5dsid2, "DS2", HDF5Constants.H5P_DEFAULT);
            H5dsid  = H5.H5Screate_simple(1, dims, null);
            H5gid   = _createGroup(H5fid, "/G1");
            H5did2  = _createDataset(H5gid, H5dsid, "DS1", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5O.createH5file: " + err);
        }
        assertTrue("TestH5O.createH5file: H5.H5Fcreate: ", H5fid >= 0);
        assertTrue("TestH5O.createH5file: H5.H5Screate_simple: ", H5dsid >= 0);
        assertTrue("TestH5O.createH5file: H5.H5Gcreate: ", H5gid >= 0);

        H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
    }

    @After
    public void deleteH5file() throws HDF5LibraryException
    {
        if (H5gid > 0)
            try {
                H5.H5Gclose(H5gid);
            }
            catch (Exception ex) {
            }
        if (H5did2 > 0)
            try {
                H5.H5Dclose(H5did2);
            }
            catch (Exception ex) {
            }
        if (H5dsid > 0)
            try {
                H5.H5Sclose(H5dsid);
            }
            catch (Exception ex) {
            }
        if (H5dsid2 > 0)
            try {
                H5.H5Sclose(H5dsid2);
            }
            catch (Exception ex) {
            }
        if (H5did1 > 0)
            try {
                H5.H5Dclose(H5did1);
            }
            catch (Exception ex) {
            }
        if (H5fid > 0)
            try {
                H5.H5Fclose(H5fid);
            }
            catch (Exception ex) {
            }

        _deleteFile(FILENAME);
        System.out.println();
    }

    @Test
    public void testH5OcopyRefsAttr()
    {
        long ocp_plist_id  = HDF5Constants.H5I_INVALID_HID;
        byte[][] dset_data = new byte[2][HDF5Constants.H5R_REF_BUF_SIZE];
        long attribute_id  = HDF5Constants.H5I_INVALID_HID;

        try {
            try {
                dset_data[0] = H5.H5Rcreate_object(H5fid, "/G1", HDF5Constants.H5P_DEFAULT);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5OcopyRefsAttr: H5Rcreate_object " + err);
            }

            try {
                dset_data[1] = H5.H5Rcreate_object(H5fid, "DS2", HDF5Constants.H5P_DEFAULT);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5OcopyRefsAttr: H5Rcreate_object " + err);
            }

            try {
                attribute_id = H5.H5Acreate(H5did2, "A1", HDF5Constants.H5T_STD_REF, H5dsid,
                                            HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
                assertTrue("testH5OcopyRefsAttr.H5Acreate: ", attribute_id >= 0);
                H5.H5Awrite(attribute_id, HDF5Constants.H5T_STD_REF, dset_data);

                H5.H5Aclose(attribute_id);
            }
            catch (Exception ex) {
                fail("testH5OcopyRefsAttr: H5Awrite failed");
            }
            finally {
                try {
                    H5.H5Aclose(attribute_id);
                }
                catch (Exception exx) {
                }
            }
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }
        finally {
            try {
                H5.H5Rdestroy(dset_data[1]);
            }
            catch (Exception ex) {
            }
            try {
                H5.H5Rdestroy(dset_data[0]);
            }
            catch (Exception ex) {
            }
        }

        try {
            ocp_plist_id = H5.H5Pcreate(HDF5Constants.H5P_OBJECT_COPY);
            assertTrue("testH5OcopyRefsAttr.H5Pcreate: ", ocp_plist_id >= 0);
            H5.H5Pset_copy_object(ocp_plist_id, HDF5Constants.H5O_COPY_EXPAND_REFERENCE_FLAG);
            H5.H5Ocopy(H5fid, ".", H5fid, "CPYREF", ocp_plist_id, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception ex) {
            fail("testH5OcopyRefsAttr: H5Ocopy failed");
        }
        finally {
            try {
                H5.H5Pclose(ocp_plist_id);
            }
            catch (Exception ex) {
            }
        }
    }

    @Test
    public void testH5OcopyRefsDatasettodiffFile()
    {
        byte[][] dset_data = new byte[2][HDF5Constants.H5R_REF_BUF_SIZE];
        long ocp_plist_id  = HDF5Constants.H5I_INVALID_HID;
        long dataset_id    = HDF5Constants.H5I_INVALID_HID;
        long H5fid2        = HDF5Constants.H5I_INVALID_HID;

        try {
            try {
                dset_data[0] = H5.H5Rcreate_object(H5fid, "/G1", HDF5Constants.H5P_DEFAULT);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5OcopyRefsDatasettodiffFile: H5Rcreate_object " + err);
            }
            try {
                dset_data[1] = H5.H5Rcreate_object(H5fid, "DS2", HDF5Constants.H5P_DEFAULT);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5OcopyRefsDatasettodiffFile: H5Rcreate_object " + err);
            }

            try {
                dataset_id =
                    H5.H5Dcreate(H5fid, "DSREF", HDF5Constants.H5T_STD_REF, H5dsid, HDF5Constants.H5P_DEFAULT,
                                 HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
                assertTrue("testH5OcopyRefsDatasettodiffFile.H5Dcreate: ", dataset_id >= 0);
                H5.H5Dwrite(dataset_id, HDF5Constants.H5T_STD_REF, HDF5Constants.H5S_ALL,
                            HDF5Constants.H5S_ALL, HDF5Constants.H5P_DEFAULT, dset_data);
                H5.H5Dclose(dataset_id);
            }
            catch (Exception ex) {
                fail("testH5OcopyRefsDatasettodiffFile: create dataset failed");
            }
            finally {
                try {
                    H5.H5Dclose(dataset_id);
                }
                catch (Exception exx) {
                }
            }
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }
        finally {
            try {
                H5.H5Rdestroy(dset_data[0]);
            }
            catch (Exception ex) {
            }
            try {
                H5.H5Rdestroy(dset_data[1]);
            }
            catch (Exception ex) {
            }
        }

        try {
            // create new file
            H5fid2 = H5.H5Fcreate("copy.h5", HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                                  HDF5Constants.H5P_DEFAULT);
            assertTrue("testH5OcopyRefsDatasettodiffFile.H5Fcreate: ", H5fid2 >= 0);
            H5.H5Fflush(H5fid2, HDF5Constants.H5F_SCOPE_LOCAL);
        }
        catch (Exception ex) {
            try {
                H5.H5Fclose(H5fid2);
            }
            catch (Exception exx) {
            }
            fail("testH5OcopyRefsDatasettodiffFile: H5Fcreate failed");
        }

        try {
            // create object copy property list id and set the flags.
            ocp_plist_id = H5.H5Pcreate(HDF5Constants.H5P_OBJECT_COPY);
            assertTrue("testH5OcopyRefsDatasettodiffFile.H5Pcreate: ", ocp_plist_id >= 0);
            H5.H5Pset_copy_object(ocp_plist_id, HDF5Constants.H5O_COPY_EXPAND_REFERENCE_FLAG);

            // Perform copy function.
            H5.H5Ocopy(H5fid, ".", H5fid2, "CPYREFD", ocp_plist_id, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception ex) {
            ex.printStackTrace();
            fail("testH5OcopyRefsDatasettodiffFile: H5Ocopy failed");
        }
        finally {
            try {
                H5.H5Pclose(ocp_plist_id);
            }
            catch (Exception ex) {
            }
            try {
                H5.H5Fclose(H5fid2);
            }
            catch (Exception ex) {
            }
        }
        _deleteFile("copy.h5");
    }

    @Test
    public void testH5OcopyRefsDatasettosameFile()
    {
        byte[][] dset_data = new byte[2][HDF5Constants.H5R_REF_BUF_SIZE];
        byte[][] read_data = new byte[2][HDF5Constants.H5R_REF_BUF_SIZE];
        long ocp_plist_id  = HDF5Constants.H5I_INVALID_HID;
        long dataset_id    = HDF5Constants.H5I_INVALID_HID;
        long did           = HDF5Constants.H5I_INVALID_HID;
        int obj_type       = -1;

        try {
            try {
                dset_data[0] = H5.H5Rcreate_object(H5fid, "/G1", HDF5Constants.H5P_DEFAULT);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5OcopyRefsDatasettosameFile: H5Rcreate_object " + err);
            }

            try {
                dset_data[1] = H5.H5Rcreate_object(H5fid, "DS2", HDF5Constants.H5P_DEFAULT);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("testH5OcopyRefsDatasettosameFile: H5Rcreate_object " + err);
            }

            try {
                // Create a dataset and write object references to it.
                dataset_id =
                    H5.H5Dcreate(H5fid, "DSREF", HDF5Constants.H5T_STD_REF, H5dsid, HDF5Constants.H5P_DEFAULT,
                                 HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
                assertTrue("testH5OcopyRefsDatasettosameFile.H5Dcreate: ", dataset_id >= 0);
                H5.H5Dwrite(dataset_id, HDF5Constants.H5T_STD_REF, HDF5Constants.H5S_ALL,
                            HDF5Constants.H5S_ALL, HDF5Constants.H5P_DEFAULT, dset_data);
                // Close the dataset.
                H5.H5Dclose(dataset_id);
            }
            catch (Exception ex) {
                fail("testH5OcopyRefsDatasettosameFile: create dataset failed");
            }
            finally {
                try {
                    H5.H5Dclose(dataset_id);
                }
                catch (Exception exx) {
                }
            }
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }
        finally {
            try {
                H5.H5Rdestroy(dset_data[1]);
            }
            catch (Exception ex) {
            }
            try {
                H5.H5Rdestroy(dset_data[0]);
            }
            catch (Exception ex) {
            }
        }

        try {
            ocp_plist_id = H5.H5Pcreate(HDF5Constants.H5P_OBJECT_COPY);
            assertTrue("testH5OcopyRefsDatasettosameFile.H5Pcreate: ", ocp_plist_id >= 0);
            H5.H5Pset_copy_object(ocp_plist_id, HDF5Constants.H5O_COPY_EXPAND_REFERENCE_FLAG);
            // Perform copy function.
            try {
                H5.H5Ocopy(H5fid, "DSREF", H5fid, "CPYREFD", ocp_plist_id, HDF5Constants.H5P_DEFAULT);
            }
            catch (Exception ex) {
                fail("testH5OcopyRefsDatasettosameFile: H5Ocopy failed");
            }
        }
        catch (Exception ex) {
            fail("testH5OcopyRefsDatasettosameFile: H5Pset_copy_object failed");
        }
        finally {
            try {
                H5.H5Pclose(ocp_plist_id);
            }
            catch (Exception exx) {
            }
        }

        try {
            // Open the dataset that has been copied
            try {
                did = H5.H5Dopen(H5fid, "DSREF", HDF5Constants.H5P_DEFAULT);
                assertTrue("testH5OcopyRefsDatasettosameFile.H5Dopen: ", did >= 0);
            }
            catch (Exception e) {
                e.printStackTrace();
                fail("testH5OcopyRefsDatasettosameFile: H5Dopen failed");
            }

            // Read the dataset object references in the read_data buffer.
            try {
                H5.H5Dread(did, HDF5Constants.H5T_STD_REF, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                           HDF5Constants.H5P_DEFAULT, read_data);
            }
            catch (Exception e) {
                e.printStackTrace();
                fail("testH5OcopyRefsDatasettosameFile: H5Dread failed");
            }
        }
        catch (Exception ex) {
            ex.printStackTrace();
            fail("testH5OcopyRefsDatasettosameFile: open and read dataset failed");
        }
        finally {
            try {
                H5.H5Dclose(did);
            }
            catch (Exception ex) {
            }
        }

        try {
            // Get the type of object the reference points to.
            obj_type = H5.H5Rget_obj_type3(read_data[1], HDF5Constants.H5R_OBJECT);
            assertEquals(obj_type, HDF5Constants.H5O_TYPE_DATASET);

            obj_type = H5.H5Rget_obj_type3(read_data[0], HDF5Constants.H5R_OBJECT);
            assertEquals(obj_type, HDF5Constants.H5O_TYPE_GROUP);
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }
        finally {
            try {
                H5.H5Rdestroy(read_data[1]);
            }
            catch (Exception ex) {
            }
            try {
                H5.H5Rdestroy(read_data[0]);
            }
            catch (Exception ex) {
            }
        }
    }

    @Test
    public void testH5OcopyNullRef() throws Throwable
    {
        final long _pid_ = HDF5Constants.H5P_DEFAULT;
        long sid         = HDF5Constants.H5I_INVALID_HID;
        long did         = HDF5Constants.H5I_INVALID_HID;
        long aid         = HDF5Constants.H5I_INVALID_HID;

        try {
            sid = H5.H5Screate_simple(1, new long[] {1}, null);
            assertTrue("testH5OcopyNullRef.H5Screate_simple: ", sid >= 0);
            did = H5.H5Dcreate(H5fid, "Dataset_with_null_Ref", HDF5Constants.H5T_NATIVE_INT, sid, _pid_,
                               _pid_, _pid_);
            assertTrue("testH5OcopyNullRef.H5Dcreate: ", did > 0);
            aid = H5.H5Acreate(did, "Null_Ref", HDF5Constants.H5T_STD_REF, sid, _pid_, _pid_);
            assertTrue("testH5OcopyNullRef.H5Acreate: ", aid > 0);
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }
        finally {
            try {
                H5.H5Dclose(did);
            }
            catch (Exception exx) {
            }
            try {
                H5.H5Aclose(aid);
            }
            catch (Exception exx) {
            }
            try {
                H5.H5Sclose(sid);
            }
            catch (Exception exx) {
            }
        }

        long ocp_plist_id = H5.H5Pcreate(HDF5Constants.H5P_OBJECT_COPY);
        assertTrue("testH5OcopyNullRef.H5Pcreate: ", ocp_plist_id >= 0);
        H5.H5Pset_copy_object(ocp_plist_id, HDF5Constants.H5O_COPY_EXPAND_REFERENCE_FLAG);
        try {
            H5.H5Ocopy(H5fid, "/Dataset_with_null_Ref", H5fid, "/Dataset_with_null_Ref_cp", ocp_plist_id,
                       _pid_);
        }
        finally {
            try {
                H5.H5Pclose(ocp_plist_id);
            }
            catch (Exception exx) {
            }
        }
    }
}
