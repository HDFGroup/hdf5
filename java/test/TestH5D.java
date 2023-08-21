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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.HDFNativeData;
import hdf.hdf5lib.callbacks.H5D_iterate_cb;
import hdf.hdf5lib.callbacks.H5D_iterate_t;
import hdf.hdf5lib.exceptions.HDF5Exception;
import hdf.hdf5lib.exceptions.HDF5LibraryException;

import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5D {
    @Rule
    public TestName testname            = new TestName();
    private static final String H5_FILE = "testD.h5";
    private static final int DIM_X      = 4;
    private static final int DIM_Y      = 6;
    private static final int RANK       = 2;
    long H5fid                          = HDF5Constants.H5I_INVALID_HID;
    long H5faplid                       = HDF5Constants.H5I_INVALID_HID;
    long H5dsid                         = HDF5Constants.H5I_INVALID_HID;
    long H5dtid                         = HDF5Constants.H5I_INVALID_HID;
    long H5did                          = HDF5Constants.H5I_INVALID_HID;
    long H5did0                         = HDF5Constants.H5I_INVALID_HID;
    long H5dcpl_id                      = HDF5Constants.H5I_INVALID_HID;
    long[] H5dims                       = {DIM_X, DIM_Y};

    // Values for the status of space allocation
    enum H5D_space_status {
        H5D_SPACE_STATUS_ERROR(-1),
        H5D_SPACE_STATUS_NOT_ALLOCATED(0),
        H5D_SPACE_STATUS_PART_ALLOCATED(1),
        H5D_SPACE_STATUS_ALLOCATED(2);

        private int code;

        H5D_space_status(int space_status) { this.code = space_status; }

        public int getCode() { return this.code; }
    }

    private final void _deleteFile(String filename)
    {
        File file = new File(filename);

        if (file.exists()) {
            try {
                file.delete();
            }
            catch (SecurityException e) {
            }
        }
    }

    private final void _createPDataset(long fid, long dsid, String name, long dcpl_val)
    {

        try {
            H5dcpl_id = H5.H5Pcreate(dcpl_val);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("H5.H5Pcreate: " + err);
        }
        assertTrue("testH5D._createPDataset: H5.H5Pcreate: ", H5dcpl_id >= 0);

        // Set the allocation time to "early". This way we can be sure
        // that reading from the dataset immediately after creation will
        // return the fill value.
        try {
            H5.H5Pset_alloc_time(H5dcpl_id, HDF5Constants.H5D_ALLOC_TIME_EARLY);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            H5did0 = H5.H5Dcreate(fid, name, HDF5Constants.H5T_STD_I32BE, dsid, HDF5Constants.H5P_DEFAULT,
                                  H5dcpl_id, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Dcreate: " + err);
        }
        assertTrue("TestH5D._createPDataset.H5Dcreate: ", H5did0 >= 0);
    }

    private final void _createChunkDataset(long fid, long dsid, String name, long dapl)
    {

        try {
            H5dcpl_id = H5.H5Pcreate(HDF5Constants.H5P_DATASET_CREATE);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("H5.H5Pcreate: " + err);
        }
        assertTrue("testH5D._createChunkDataset: H5.H5Pcreate: ", H5dcpl_id >= 0);

        // Set the chunking.
        long[] chunk_dim = {4, 4};

        try {
            H5.H5Pset_chunk(H5dcpl_id, RANK, chunk_dim);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            H5did = H5.H5Dcreate(fid, name, HDF5Constants.H5T_STD_I32BE, dsid, HDF5Constants.H5P_DEFAULT,
                                 H5dcpl_id, dapl);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Dcreate: " + err);
        }
        assertTrue("TestH5D._createChunkDataset.H5Dcreate: ", H5did >= 0);
    }

    private final void _createDataset(long fid, long dsid, String name, long dapl)
    {
        try {
            H5did = H5.H5Dcreate(fid, name, HDF5Constants.H5T_STD_I32BE, dsid, HDF5Constants.H5P_DEFAULT,
                                 HDF5Constants.H5P_DEFAULT, dapl);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Dcreate: " + err);
        }
        assertTrue("TestH5D._createDataset.H5Dcreate: ", H5did >= 0);
    }

    private final void _createVLStrDataset(String name, long dapl)
    {
        try {
            H5dtid = H5.H5Tcopy(HDF5Constants.H5T_C_S1);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Tcopy: " + err);
        }
        assertTrue("TestH5D._createVLStrDataset.H5Tcopy: ", H5dtid >= 0);
        try {
            H5.H5Tset_size(H5dtid, HDF5Constants.H5T_VARIABLE);
            assertTrue("TestH5D._createVLStrDataset.H5Tis_variable_str", H5.H5Tis_variable_str(H5dtid));
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Tset_size: " + err);
        }
        try {
            H5did = H5.H5Dcreate(H5fid, name, H5dtid, H5dsid, HDF5Constants.H5P_DEFAULT,
                                 HDF5Constants.H5P_DEFAULT, dapl);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Dcreate: " + err);
        }
        assertTrue("TestH5D._createVLStrDataset.H5Dcreate: ", H5did >= 0);
    }

    private final void _closeH5file() throws HDF5LibraryException
    {
        if (H5dcpl_id >= 0)
            try {
                H5.H5Pclose(H5dcpl_id);
            }
            catch (Exception ex) {
            }
        if (H5did0 >= 0)
            try {
                H5.H5Dclose(H5did0);
            }
            catch (Exception ex) {
            }
        if (H5did >= 0)
            try {
                H5.H5Dclose(H5did);
            }
            catch (Exception ex) {
            }
        if (H5dtid > 0)
            try {
                H5.H5Tclose(H5dtid);
            }
            catch (Exception ex) {
            }
        if (H5dsid > 0)
            try {
                H5.H5Sclose(H5dsid);
            }
            catch (Exception ex) {
            }
        if (H5fid > 0)
            try {
                H5.H5Fclose(H5fid);
            }
            catch (Exception ex) {
            }
    }

    private final void _openH5file(String filename, String dsetname, long dapl)
    {
        try {
            H5fid = H5.H5Fopen(filename, HDF5Constants.H5F_ACC_RDONLY, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5D._openH5file: " + err);
        }
        assertTrue("TestH5D._openH5file: H5.H5Fopen: ", H5fid >= 0);
        try {
            H5did = H5.H5Dopen(H5fid, dsetname, dapl);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5D._openH5file: " + err);
        }
        assertTrue("TestH5D._openH5file: H5.H5Dopen: ", H5did >= 0);
        try {
            H5dsid = H5.H5Dget_space(H5did);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5D._openH5file: " + err);
        }
        assertTrue("TestH5D._openH5file: H5.H5Screate_simple: ", H5dsid > 0);
    }

    @Before
    public void createH5file() throws NullPointerException, HDF5Exception
    {
        assertTrue("H5 open ids is 0", H5.getOpenIDCount() == 0);
        System.out.print(testname.getMethodName());

        try {
            H5faplid = H5.H5Pcreate(HDF5Constants.H5P_FILE_ACCESS);
            H5fid  = H5.H5Fcreate(H5_FILE, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT, H5faplid);
            H5dsid = H5.H5Screate_simple(RANK, H5dims, null);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5D.createH5file: " + err);
        }
        assertTrue("TestH5D.createH5file: H5.H5Fcreate: ", H5fid >= 0);
        assertTrue("TestH5D.createH5file: H5.H5Screate_simple: ", H5dsid >= 0);

        H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
    }

    @After
    public void deleteH5file() throws HDF5LibraryException
    {
        if (H5dcpl_id >= 0)
            try {
                H5.H5Pclose(H5dcpl_id);
            }
            catch (Exception ex) {
            }
        if (H5did0 >= 0)
            try {
                H5.H5Dclose(H5did0);
            }
            catch (Exception ex) {
            }
        if (H5did >= 0)
            try {
                H5.H5Dclose(H5did);
            }
            catch (Exception ex) {
            }
        if (H5dtid > 0)
            try {
                H5.H5Tclose(H5dtid);
            }
            catch (Exception ex) {
            }
        if (H5dsid > 0)
            try {
                H5.H5Sclose(H5dsid);
            }
            catch (Exception ex) {
            }
        if (H5faplid >= 0)
            try {
                H5.H5Pclose(H5faplid);
            }
            catch (Exception ex) {
            }
        if (H5fid > 0)
            try {
                H5.H5Fclose(H5fid);
            }
            catch (Exception ex) {
            }

        _deleteFile(H5_FILE);
        System.out.println();
    }

    @Test
    public void testH5Dcreate()
    {
        long dataset_id = HDF5Constants.H5I_INVALID_HID;
        try {
            dataset_id =
                H5.H5Dcreate(H5fid, "dset", HDF5Constants.H5T_STD_I32BE, H5dsid, HDF5Constants.H5P_DEFAULT,
                             HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Dcreate: " + err);
        }
        assertTrue(dataset_id >= 0);

        // End access to the dataset and release resources used by it.
        try {
            if (dataset_id >= 0)
                H5.H5Dclose(dataset_id);
        }
        catch (Exception err) {
            err.printStackTrace();
        }
    }

    @Test
    public void testH5Dcreate_anon()
    {
        long dataset_id = HDF5Constants.H5I_INVALID_HID;
        try {
            dataset_id = H5.H5Dcreate_anon(H5fid, HDF5Constants.H5T_STD_I32BE, H5dsid,
                                           HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Dcreate_anon: " + err);
        }
        assertTrue(dataset_id >= 0);

        // End access to the dataset and release resources used by it.
        try {
            if (dataset_id >= 0)
                H5.H5Dclose(dataset_id);
        }
        catch (Exception err) {
            err.printStackTrace();
        }
    }

    @Test
    public void testH5Dopen()
    {
        long dataset_id = HDF5Constants.H5I_INVALID_HID;
        _createDataset(H5fid, H5dsid, "dset", HDF5Constants.H5P_DEFAULT);

        try {
            H5.H5Dclose(H5did);
            H5did      = HDF5Constants.H5I_INVALID_HID;
            dataset_id = H5.H5Dopen(H5fid, "dset", HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Dopen: " + err);
        }
        assertTrue("testH5Dopen: ", dataset_id >= 0);

        // End access to the dataset and release resources used by it.
        try {
            if (dataset_id >= 0)
                H5.H5Dclose(dataset_id);
        }
        catch (Exception err) {
            err.printStackTrace();
        }
    }

    @Test
    public void testH5Dget_storage_size_empty()
    {
        long storage_size = 0;
        _createDataset(H5fid, H5dsid, "dset", HDF5Constants.H5P_DEFAULT);

        try {
            storage_size = H5.H5Dget_storage_size(H5did);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Dget_storage_size: " + err);
        }
        assertTrue("testH5Dget_storage_size: ", storage_size == 0);
    }

    @Test
    public void testH5Dget_storage_size()
    {
        long storage_size = 0;
        int[][] dset_data = new int[DIM_X][DIM_Y];
        int FILLVAL       = 99;
        _createDataset(H5fid, H5dsid, "dset", HDF5Constants.H5P_DEFAULT);

        // Initialize the dataset.
        for (int indx = 0; indx < DIM_X; indx++)
            for (int jndx = 0; jndx < DIM_Y; jndx++)
                dset_data[indx][jndx] = FILLVAL;

        try {
            if (H5did >= 0)
                H5.H5Dwrite(H5did, HDF5Constants.H5T_NATIVE_INT, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                            HDF5Constants.H5P_DEFAULT, dset_data[0]);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            storage_size = H5.H5Dget_storage_size(H5did);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Dget_storage_size: " + err);
        }
        assertTrue("testH5Dget_storage_size: " + storage_size, storage_size == DIM_X * DIM_Y * 4);
    }

    @Test
    public void testH5Dget_access_plist()
    {
        long dapl_id        = HDF5Constants.H5I_INVALID_HID;
        long test_dapl_id   = HDF5Constants.H5I_INVALID_HID;
        int[] mdc_nelmts1   = {0};
        int[] mdc_nelmts2   = {0};
        long[] rdcc_nelmts1 = {0};
        long[] rdcc_nelmts2 = {0};
        long[] rdcc_nbytes1 = {0};
        long[] rdcc_nbytes2 = {0};
        double[] rdcc_w01   = {0};
        double[] rdcc_w02   = {0};

        try {
            test_dapl_id = H5.H5Pcreate(HDF5Constants.H5P_DATASET_ACCESS);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Dget_access_plist: H5.H5Pcreate: " + err);
        }
        assertTrue("testH5Dget_access_plist: test_dapl_id: ", test_dapl_id >= 0);

        try {
            H5.H5Pget_cache(H5faplid, mdc_nelmts1, rdcc_nelmts1, rdcc_nbytes1, rdcc_w01);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Dget_access_plist: H5.H5Pget_cache: " + err);
        }

        _createChunkDataset(H5fid, H5dsid, "dset", test_dapl_id);

        try {
            dapl_id = H5.H5Dget_access_plist(H5did);
            assertTrue("testH5Dget_access_plist: dapl_id: ", dapl_id >= 0);
            H5.H5Pget_chunk_cache(dapl_id, rdcc_nelmts2, rdcc_nbytes2, rdcc_w02);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Dget_access_plist: H5.H5Dget_access_plist: " + err);
        }

        // End access to the dataset and release resources used by it.
        try {
            if (dapl_id >= 0)
                H5.H5Pclose(dapl_id);
        }
        catch (Exception err) {
            err.printStackTrace();
        }
        try {
            if (test_dapl_id >= 0)
                H5.H5Pclose(test_dapl_id);
        }
        catch (Exception err) {
            err.printStackTrace();
        }
        assertTrue("testH5Dget_access_plist: ", rdcc_nelmts2 == rdcc_nelmts2 && rdcc_nbytes2 == rdcc_nbytes2);
    }

    @Test
    public void testH5Dget_space_status()
    {
        int[][] write_dset_data = new int[DIM_X][DIM_Y];
        int space_status        = -1;
        int space_status0       = -1;

        // Initialize the dataset.
        for (int indx = 0; indx < DIM_X; indx++)
            for (int jndx = 0; jndx < DIM_Y; jndx++)
                write_dset_data[indx][jndx] = indx * jndx - jndx;

        _createPDataset(H5fid, H5dsid, "dset0", HDF5Constants.H5P_DATASET_CREATE);
        _createDataset(H5fid, H5dsid, "dset", HDF5Constants.H5P_DEFAULT);

        // Retrieve and print space status and storage size for dset0.
        try {
            space_status0 = H5.H5Dget_space_status(H5did0);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        assertTrue("testH5Dget_space_status0 - H5.H5Dget_space_status: ",
                   space_status0 == H5D_space_status.H5D_SPACE_STATUS_ALLOCATED.getCode());

        // Retrieve and print space status and storage size for dset.
        try {
            space_status = H5.H5Dget_space_status(H5did);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        assertFalse("testH5Dget_space_status - H5.H5Dget_space_status: ",
                    space_status == H5D_space_status.H5D_SPACE_STATUS_ALLOCATED.getCode());

        // Write the data to the dataset.
        try {
            H5.H5Dwrite(H5did, HDF5Constants.H5T_NATIVE_INT, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                        HDF5Constants.H5P_DEFAULT, write_dset_data);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Retrieve and print space status and storage size for dset.
        try {
            space_status = H5.H5Dget_space_status(H5did);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        assertTrue("testH5Dget_space_status - H5.H5Dget_space_status: ",
                   space_status == H5D_space_status.H5D_SPACE_STATUS_ALLOCATED.getCode());
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Dget_space_closed() throws Throwable
    {
        long dataset_id = HDF5Constants.H5I_INVALID_HID;
        try {
            dataset_id =
                H5.H5Dcreate(H5fid, "dset", HDF5Constants.H5T_STD_I32BE, H5dsid, HDF5Constants.H5P_DEFAULT,
                             HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Dcreate: " + err);
        }
        assertTrue("TestH5D.testH5Dget_space_closed: ", dataset_id >= 0);
        H5.H5Dclose(dataset_id);

        H5.H5Dget_space(dataset_id);
    }

    @Test
    public void testH5Dget_space()
    {
        long dataspace_id = HDF5Constants.H5I_INVALID_HID;
        _createDataset(H5fid, H5dsid, "dset", HDF5Constants.H5P_DEFAULT);

        try {
            dataspace_id = H5.H5Dget_space(H5did);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("H5.H5Dget_space: " + err);
        }
        assertTrue("TestH5D.testH5Dget_space: ", dataspace_id >= 0);

        // End access to the dataspace and release resources used by it.
        try {
            if (dataspace_id >= 0)
                H5.H5Sclose(dataspace_id);
        }
        catch (Exception err) {
            err.printStackTrace();
        }
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Dget_type_closed() throws Throwable
    {
        long dataset_id = HDF5Constants.H5I_INVALID_HID;
        try {
            dataset_id =
                H5.H5Dcreate(H5fid, "dset", HDF5Constants.H5T_STD_I32BE, H5dsid, HDF5Constants.H5P_DEFAULT,
                             HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Dcreate: " + err);
        }
        assertTrue("TestH5D.testH5Dget_type_closed: ", dataset_id >= 0);
        H5.H5Dclose(dataset_id);

        H5.H5Dget_type(dataset_id);
    }

    @Test
    public void testH5Dget_type()
    {
        long datatype_id = HDF5Constants.H5I_INVALID_HID;
        _createDataset(H5fid, H5dsid, "dset", HDF5Constants.H5P_DEFAULT);

        try {
            datatype_id = H5.H5Dget_type(H5did);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("H5.H5Dget_type: " + err);
        }
        assertTrue("TestH5D.testH5Dget_type: ", datatype_id >= 0);

        // End access to the datatype and release resources used by it.
        try {
            if (datatype_id >= 0)
                H5.H5Tclose(datatype_id);
        }
        catch (Exception err) {
            err.printStackTrace();
        }
    }

    @Test
    public void testH5Dget_offset()
    {
        int[][] write_dset_data = new int[DIM_X][DIM_Y];
        long dset_address       = 0;
        _createDataset(H5fid, H5dsid, "dset", HDF5Constants.H5P_DEFAULT);

        try {
            // Test dataset address.  Should be undefined.
            dset_address = H5.H5Dget_offset(H5did);
        }
        catch (HDF5LibraryException hdfex) {
            ;
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("H5.H5Dget_offset: " + err);
        }
        // Write the data to the dataset.
        try {
            H5.H5Dwrite(H5did, HDF5Constants.H5T_NATIVE_INT, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                        HDF5Constants.H5P_DEFAULT, write_dset_data);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            // Test dataset address.
            dset_address = H5.H5Dget_offset(H5did);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("H5.H5Dget_offset: " + err);
        }

        assertTrue("TestH5D.testH5Dget_offset: ", dset_address >= 0);
    }

    @Test
    public void testH5Dfill_null()
    {
        int[] buf_data = new int[DIM_X * DIM_Y];

        // Initialize memory buffer
        for (int indx = 0; indx < DIM_X; indx++)
            for (int jndx = 0; jndx < DIM_Y; jndx++) {
                buf_data[(indx * DIM_Y) + jndx] = indx * jndx - jndx;
            }
        byte[] buf_array = HDFNativeData.intToByte(0, DIM_X * DIM_Y, buf_data);

        // Fill selection in memory
        try {
            H5.H5Dfill(null, HDF5Constants.H5T_NATIVE_UINT, buf_array, HDF5Constants.H5T_NATIVE_UINT, H5dsid);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("H5.H5Dfill: " + err);
        }
        buf_data = HDFNativeData.byteToInt(buf_array);

        // Verify memory buffer the hard way
        for (int indx = 0; indx < DIM_X; indx++)
            for (int jndx = 0; jndx < DIM_Y; jndx++)
                assertTrue("H5.H5Dfill: [" + indx + "," + jndx + "] ", buf_data[(indx * DIM_Y) + jndx] == 0);
    }

    @Test
    public void testH5Dfill()
    {
        int[] buf_data    = new int[DIM_X * DIM_Y];
        byte[] fill_value = HDFNativeData.intToByte(254);

        // Initialize memory buffer
        for (int indx = 0; indx < DIM_X; indx++)
            for (int jndx = 0; jndx < DIM_Y; jndx++) {
                buf_data[(indx * DIM_Y) + jndx] = indx * jndx - jndx;
            }
        byte[] buf_array = HDFNativeData.intToByte(0, DIM_X * DIM_Y, buf_data);

        // Fill selection in memory
        try {
            H5.H5Dfill(fill_value, HDF5Constants.H5T_NATIVE_UINT, buf_array, HDF5Constants.H5T_NATIVE_UINT,
                       H5dsid);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("H5.H5Dfill: " + err);
        }
        buf_data = HDFNativeData.byteToInt(buf_array);

        // Verify memory buffer the hard way
        for (int indx = 0; indx < DIM_X; indx++)
            for (int jndx = 0; jndx < DIM_Y; jndx++)
                assertTrue("H5.H5Dfill: [" + indx + "," + jndx + "] ",
                           buf_data[(indx * DIM_Y) + jndx] == 254);
    }

    @Test
    public void testH5Diterate()
    {
        final int SPACE_RANK = 2;
        final int SPACE_FILL = 254;

        class H5D_iter_data implements H5D_iterate_t {
            public int fill_value;       /* The fill value to check */
            public long fill_curr_coord; /* Current coordinate to examine */
            public long[] fill_coords;   /* Pointer to selection's coordinates */
        }

        H5D_iterate_t iter_data = new H5D_iter_data();

        class H5D_iter_callback implements H5D_iterate_cb {
            public int callback(byte[] elem_buf, long elem_id, int ndim, long[] point, H5D_iterate_t op_data)
            {
                // Check value in current buffer location
                int element = HDFNativeData.byteToInt(elem_buf, 0);
                if (element != ((H5D_iter_data)op_data).fill_value)
                    return -1;
                // Check number of dimensions
                if (ndim != SPACE_RANK)
                    return (-1);
                // Check Coordinates
                long[] fill_coords = new long[2];
                fill_coords[0] =
                    ((H5D_iter_data)op_data).fill_coords[(int)(2 * ((H5D_iter_data)op_data).fill_curr_coord)];
                fill_coords[1] = ((H5D_iter_data)op_data)
                                     .fill_coords[(int)(2 * ((H5D_iter_data)op_data).fill_curr_coord) + 1];
                ((H5D_iter_data)op_data).fill_curr_coord++;
                if (fill_coords[0] != point[0])
                    return (-1);
                if (fill_coords[1] != point[1])
                    return (-1);

                return (0);
            }
        }

        int[] buf_data    = new int[DIM_X * DIM_Y];
        byte[] fill_value = HDFNativeData.intToByte(SPACE_FILL);

        // Initialize memory buffer
        for (int indx = 0; indx < DIM_X; indx++)
            for (int jndx = 0; jndx < DIM_Y; jndx++) {
                buf_data[(indx * DIM_Y) + jndx] = indx * jndx - jndx;
            }
        byte[] buf_array = HDFNativeData.intToByte(0, DIM_X * DIM_Y, buf_data);

        // Fill selection in memory
        try {
            H5.H5Dfill(fill_value, HDF5Constants.H5T_NATIVE_UINT, buf_array, HDF5Constants.H5T_NATIVE_UINT,
                       H5dsid);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("H5.H5Diterate: " + err);
        }

        // Initialize the iterator structure
        ((H5D_iter_data)iter_data).fill_value      = SPACE_FILL;
        ((H5D_iter_data)iter_data).fill_curr_coord = 0;
        // Set the coordinates of the selection
        ((H5D_iter_data)iter_data).fill_coords =
            new long[DIM_X * DIM_Y * SPACE_RANK]; /* Coordinates of selection */
        for (int indx = 0; indx < DIM_X; indx++)
            for (int jndx = 0; jndx < DIM_Y; jndx++) {
                ((H5D_iter_data)iter_data).fill_coords[2 * (indx * DIM_Y + jndx)]     = indx;
                ((H5D_iter_data)iter_data).fill_coords[2 * (indx * DIM_Y + jndx) + 1] = jndx;
            } /* end for */

        // Iterate through selection, verifying correct data
        H5D_iterate_cb iter_cb = new H5D_iter_callback();
        int op_status          = -1;
        try {
            op_status = H5.H5Diterate(buf_array, HDF5Constants.H5T_NATIVE_UINT, H5dsid, iter_cb, iter_data);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Diterate: " + err);
        }
        assertTrue("H5Diterate ", op_status == 0);
    }

    @Test
    public void testH5Diterate_write()
    {
        final int SPACE_RANK = 2;
        final int SPACE_FILL = 254;

        class H5D_iter_data implements H5D_iterate_t {
            public int fill_value;       /* The fill value to check */
            public long fill_curr_coord; /* Current coordinate to examine */
            public long[] fill_coords;   /* Pointer to selection's coordinates */
        }

        H5D_iterate_t iter_data = new H5D_iter_data();

        class H5D_iter_callback implements H5D_iterate_cb {
            public int callback(byte[] elem_buf, long elem_id, int ndim, long[] point, H5D_iterate_t op_data)
            {
                // Check value in current buffer location
                int element = HDFNativeData.byteToInt(elem_buf, 0);
                if (element != ((H5D_iter_data)op_data).fill_value)
                    return -1;
                // Check number of dimensions
                if (ndim != SPACE_RANK)
                    return (-1);
                // Check Coordinates
                long[] fill_coords = new long[2];
                fill_coords[0] =
                    ((H5D_iter_data)op_data).fill_coords[(int)(2 * ((H5D_iter_data)op_data).fill_curr_coord)];
                fill_coords[1] = ((H5D_iter_data)op_data)
                                     .fill_coords[(int)(2 * ((H5D_iter_data)op_data).fill_curr_coord) + 1];
                ((H5D_iter_data)op_data).fill_curr_coord++;
                if (fill_coords[0] != point[0])
                    return (-1);
                if (fill_coords[1] != point[1])
                    return (-1);
                element -= 128;
                byte[] new_elembuf = HDFNativeData.intToByte(element);
                elem_buf[0]        = new_elembuf[0];
                elem_buf[1]        = new_elembuf[1];
                elem_buf[2]        = new_elembuf[2];
                elem_buf[3]        = new_elembuf[3];
                return (0);
            }
        }

        int[] buf_data    = new int[DIM_X * DIM_Y];
        byte[] fill_value = HDFNativeData.intToByte(SPACE_FILL);

        // Initialize memory buffer
        for (int indx = 0; indx < DIM_X; indx++)
            for (int jndx = 0; jndx < DIM_Y; jndx++) {
                buf_data[(indx * DIM_Y) + jndx] = indx * jndx - jndx;
            }
        byte[] buf_array = HDFNativeData.intToByte(0, DIM_X * DIM_Y, buf_data);

        // Fill selection in memory
        try {
            H5.H5Dfill(fill_value, HDF5Constants.H5T_NATIVE_UINT, buf_array, HDF5Constants.H5T_NATIVE_UINT,
                       H5dsid);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("H5.H5Diterate: " + err);
        }

        // Initialize the iterator structure
        ((H5D_iter_data)iter_data).fill_value      = SPACE_FILL;
        ((H5D_iter_data)iter_data).fill_curr_coord = 0;
        // Set the coordinates of the selection
        ((H5D_iter_data)iter_data).fill_coords =
            new long[DIM_X * DIM_Y * SPACE_RANK]; /* Coordinates of selection */
        for (int indx = 0; indx < DIM_X; indx++)
            for (int jndx = 0; jndx < DIM_Y; jndx++) {
                ((H5D_iter_data)iter_data).fill_coords[2 * (indx * DIM_Y + jndx)]     = indx;
                ((H5D_iter_data)iter_data).fill_coords[2 * (indx * DIM_Y + jndx) + 1] = jndx;
            } /* end for */

        // Iterate through selection, verifying correct data
        H5D_iterate_cb iter_cb = new H5D_iter_callback();
        int op_status          = -1;
        try {
            op_status = H5.H5Diterate(buf_array, HDF5Constants.H5T_NATIVE_UINT, H5dsid, iter_cb, iter_data);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Diterate: " + err);
        }
        assertTrue("H5Diterate ", op_status == 0);

        buf_data = HDFNativeData.byteToInt(buf_array);

        // Verify memory buffer the hard way
        for (int indx = 0; indx < DIM_X; indx++)
            for (int jndx = 0; jndx < DIM_Y; jndx++)
                assertTrue("H5.H5Diterate: [" + indx + "," + jndx + "] " + buf_data[(indx * DIM_Y) + jndx],
                           buf_data[(indx * DIM_Y) + jndx] == 126);
    }

    @Ignore
    public void testH5Dvlen_get_buf_size()
    {
        String[] str_data   = {"Parting", "is such", "sweet", "sorrow.", "Testing",  "one", "two",   "three.",
                             "Dog,",    "man's",   "best",  "friend.", "Diamonds", "are", "a",     "girls!",
                             "S A",     "T U R",   "D A Y", "night",   "That's",   "all", "folks", "!!!"};
        long vl_size        = -1; /* Number of bytes used */
        long str_data_bytes = 0;
        for (int idx = 0; idx < str_data.length; idx++)
            str_data_bytes += str_data[idx].length() + 1; // Account for terminating null

        _createVLStrDataset("dset", HDF5Constants.H5P_DEFAULT);

        try {
            if ((H5did >= 0) && (H5dtid >= 0))
                H5.H5DwriteVL(H5did, H5dtid, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                              HDF5Constants.H5P_DEFAULT, str_data);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        try {
            vl_size = H5.H5Dvlen_get_buf_size(H5did, H5dtid, H5dsid);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        assertTrue("H5Dvlen_get_buf_size " + vl_size + " == " + str_data_bytes, vl_size == str_data_bytes);
    }

    @Ignore
    public void testH5Dvlen_string_buffer() throws Throwable
    {
        String dset_str_name = "VLStringdata";
        long dset_str_id     = HDF5Constants.H5I_INVALID_HID;
        long dtype_str_id    = HDF5Constants.H5I_INVALID_HID;
        long dspace_id       = HDF5Constants.H5I_INVALID_HID;
        long[] dims          = {64};
        long lsize           = 1;

        String[] str_data0 = {"Parting", "is such", "sweet", "sorrow."};
        String[] str_data1 = {"Testing", "one", "two", "three."};
        String[] str_data2 = {"Dog,", "man's", "best", "friend."};
        String[] str_data3 = {"Diamonds", "are", "a", "girls!"};
        String[] str_data4 = {"S A", "T U R", "D A Y", "night"};
        String[] str_data5 = {"That's", "all", "folks", "!!!"};

        ArrayList[] vl_str_data = new ArrayList[6];
        vl_str_data[0]          = new ArrayList<String>(Arrays.asList(str_data0));
        vl_str_data[1]          = new ArrayList<String>(Arrays.asList(str_data1));
        vl_str_data[2]          = new ArrayList<String>(Arrays.asList(str_data2));
        vl_str_data[3]          = new ArrayList<String>(Arrays.asList(str_data3));
        vl_str_data[4]          = new ArrayList<String>(Arrays.asList(str_data4));
        vl_str_data[5]          = new ArrayList<String>(Arrays.asList(str_data5));

        try {
            H5dtid = H5.H5Tcopy(HDF5Constants.H5T_C_S1);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Dvlen_string_buffer.H5.H5Tcopy: " + err);
        }
        assertTrue("testH5Dvlen_string_buffer.H5Tcopy: ", H5dtid >= 0);
        try {
            H5.H5Tset_size(H5dtid, HDF5Constants.H5T_VARIABLE);
            assertTrue("testH5Dvlen_string_buffer.H5Tis_variable_str", H5.H5Tis_variable_str(H5dtid));
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Dvlen_string_buffer.H5Tset_size: " + err);
        }
        try {
            dtype_str_id = H5.H5Tvlen_create(H5dtid);
            assertTrue("testH5Dvlen_string_buffer.H5Tvlen_create: ", dtype_str_id >= 0);
        }
        catch (Exception err) {
            if (dtype_str_id > 0)
                try {
                    H5.H5Tclose(dtype_str_id);
                }
                catch (Exception ex) {
                }
            err.printStackTrace();
            fail("testH5Dvlen_string_buffer: " + err);
        }

        try {
            dspace_id = H5.H5Screate_simple(1, dims, null);
            assertTrue(dspace_id > 0);
            dset_str_id =
                H5.H5Dcreate(H5fid, dset_str_name, dtype_str_id, dspace_id, HDF5Constants.H5P_DEFAULT,
                             HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
            assertTrue("testH5Dvlen_string_buffer: ", dset_str_id >= 0);

            H5.H5DwriteVL(dset_str_id, dtype_str_id, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                          HDF5Constants.H5P_DEFAULT, vl_str_data);
        }
        catch (Exception err) {
            if (dset_str_id > 0)
                try {
                    H5.H5Dclose(dset_str_id);
                }
                catch (Exception ex) {
                }
            if (dtype_str_id > 0)
                try {
                    H5.H5Tclose(dtype_str_id);
                }
                catch (Exception ex) {
                }
            err.printStackTrace();
            fail("testH5Dvlen_string_buffer: " + err);
        }
        finally {
            if (dspace_id > 0)
                try {
                    H5.H5Sclose(dspace_id);
                }
                catch (Exception ex) {
                }
        }

        H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);

        for (int j = 0; j < dims.length; j++)
            lsize *= dims[j];

        ArrayList[] vl_readbuf = new ArrayList[4];
        for (int j = 0; j < lsize; j++)
            vl_readbuf[j] = new ArrayList<String>();

        try {
            H5.H5DreadVL(dset_str_id, dtype_str_id, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                         HDF5Constants.H5P_DEFAULT, vl_readbuf);
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }
        assertTrue("testH5Dvlen_string_buffer:" + vl_readbuf[0].get(0),
                   vl_str_data[0].get(0).equals(vl_readbuf[0].get(0)));
        assertTrue("testH5Dvlen_string_buffer:" + vl_readbuf[1].get(0),
                   vl_str_data[1].get(0).equals(vl_readbuf[1].get(0)));
        assertTrue("testH5Dvlen_string_buffer:" + vl_readbuf[2].get(0),
                   vl_str_data[2].get(0).equals(vl_readbuf[2].get(0)));
        assertTrue("testH5Dvlen_string_buffer:" + vl_readbuf[3].get(0),
                   vl_str_data[3].get(0).equals(vl_readbuf[3].get(0)));
    }

    @Test
    public void testH5Dvlen_write_read()
    {
        String[] str_wdata = {"Parting", "is such", "sweet", "sorrow.", "Testing",  "one", "two",   "three.",
                              "Dog,",    "man's",   "best",  "friend.", "Diamonds", "are", "a",     "girls!",
                              "S A",     "T U R",   "D A Y", "night",   "That's",   "all", "folks", "!!!"};
        String[] str_rdata = new String[DIM_X * DIM_Y];

        _createVLStrDataset("dset", HDF5Constants.H5P_DEFAULT);

        try {
            if ((H5did >= 0) && (H5dtid >= 0))
                H5.H5DwriteVL(H5did, H5dtid, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                              HDF5Constants.H5P_DEFAULT, str_wdata);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            if ((H5did >= 0) && (H5dtid >= 0))
                H5.H5DreadVL(H5did, H5dtid, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                             HDF5Constants.H5P_DEFAULT, str_rdata);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        for (int v = 0; v < DIM_X * DIM_Y; v++)
            assertTrue("testH5Dvlen_write_read " + str_wdata[v] + " == " + str_rdata[v],
                       str_wdata[v] == str_wdata[v]);
    }

    @Test
    public void testH5DVLwr()
    {
        String dset_int_name = "VLIntdata";
        String dset_dbl_name = "VLDbldata";
        long dset_int_id     = HDF5Constants.H5I_INVALID_HID;
        long dset_dbl_id     = HDF5Constants.H5I_INVALID_HID;
        long dtype_int_id    = HDF5Constants.H5I_INVALID_HID;
        long dtype_dbl_id    = HDF5Constants.H5I_INVALID_HID;
        long dspace_id       = HDF5Constants.H5I_INVALID_HID;
        long[] dims          = {4};
        long lsize           = 1;

        ArrayList[] vl_int_data = new ArrayList[4];
        ArrayList[] vl_dbl_data = new ArrayList[4];
        try {
            // Write Integer data
            vl_int_data[0]  = new ArrayList<Integer>(Arrays.asList(1));
            vl_int_data[1]  = new ArrayList<Integer>(Arrays.asList(2, 3));
            vl_int_data[2]  = new ArrayList<Integer>(Arrays.asList(4, 5, 6));
            vl_int_data[3]  = new ArrayList<Integer>(Arrays.asList(7, 8, 9, 10));
            Class dataClass = vl_int_data.getClass();
            assertTrue("testH5DVLwr.getClass: " + dataClass, dataClass.isArray());

            try {
                dtype_int_id = H5.H5Tvlen_create(HDF5Constants.H5T_STD_U32LE);
                assertTrue("testH5DVLwr.H5Tvlen_create: ", dtype_int_id >= 0);
            }
            catch (Exception err) {
                if (dtype_int_id > 0)
                    try {
                        H5.H5Tclose(dtype_int_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("H5.testH5DVLwr: " + err);
            }

            try {
                dspace_id = H5.H5Screate_simple(1, dims, null);
                assertTrue(dspace_id > 0);
                dset_int_id =
                    H5.H5Dcreate(H5fid, dset_int_name, dtype_int_id, dspace_id, HDF5Constants.H5P_DEFAULT,
                                 HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
                assertTrue("testH5DVLwr: ", dset_int_id >= 0);

                H5.H5DwriteVL(dset_int_id, dtype_int_id, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                              HDF5Constants.H5P_DEFAULT, vl_int_data);
            }
            catch (Exception err) {
                if (dset_int_id > 0)
                    try {
                        H5.H5Dclose(dset_int_id);
                    }
                    catch (Exception ex) {
                    }
                if (dtype_int_id > 0)
                    try {
                        H5.H5Tclose(dtype_int_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("H5.testH5DVLwr: " + err);
            }
            finally {
                if (dspace_id > 0)
                    try {
                        H5.H5Sclose(dspace_id);
                    }
                    catch (Exception ex) {
                    }
            }

            // Write Double data
            vl_dbl_data[0] = new ArrayList<Double>(Arrays.asList(1.1));
            vl_dbl_data[1] = new ArrayList<Double>(Arrays.asList(2.2, 3.3));
            vl_dbl_data[2] = new ArrayList<Double>(Arrays.asList(4.4, 5.5, 6.6));
            vl_dbl_data[3] = new ArrayList<Double>(Arrays.asList(7.7, 8.8, 9.9, 10.0));
            dataClass      = vl_dbl_data.getClass();
            assertTrue("testH5DVLwr.getClass: " + dataClass, dataClass.isArray());

            try {
                dtype_dbl_id = H5.H5Tvlen_create(HDF5Constants.H5T_NATIVE_DOUBLE);
                assertTrue("testH5DVLwr.H5Tvlen_create: ", dtype_dbl_id >= 0);
            }
            catch (Exception err) {
                if (dtype_dbl_id > 0)
                    try {
                        H5.H5Tclose(dtype_dbl_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("H5.testH5DVLwr: " + err);
            }

            try {
                dspace_id = H5.H5Screate_simple(1, dims, null);
                assertTrue(dspace_id > 0);
                dset_dbl_id =
                    H5.H5Dcreate(H5fid, dset_dbl_name, dtype_dbl_id, dspace_id, HDF5Constants.H5P_DEFAULT,
                                 HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
                assertTrue("testH5DVLwr: ", dset_dbl_id >= 0);

                H5.H5DwriteVL(dset_dbl_id, dtype_dbl_id, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                              HDF5Constants.H5P_DEFAULT, vl_dbl_data);
            }
            catch (Exception err) {
                if (dset_dbl_id > 0)
                    try {
                        H5.H5Dclose(dset_dbl_id);
                    }
                    catch (Exception ex) {
                    }
                if (dtype_dbl_id > 0)
                    try {
                        H5.H5Tclose(dtype_dbl_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("H5.testH5DVLwr: " + err);
            }
            finally {
                if (dspace_id > 0)
                    try {
                        H5.H5Sclose(dspace_id);
                    }
                    catch (Exception ex) {
                    }
            }

            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);

            for (int j = 0; j < dims.length; j++)
                lsize *= dims[j];

            // Read Integer data
            ArrayList[] vl_int_readbuf = new ArrayList[4];
            for (int j = 0; j < lsize; j++)
                vl_int_readbuf[j] = new ArrayList<Integer>();

            try {
                H5.H5DreadVL(dset_int_id, dtype_int_id, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                             HDF5Constants.H5P_DEFAULT, vl_int_readbuf);
            }
            catch (Exception ex) {
                ex.printStackTrace();
            }
            assertTrue("testH5DVLwr:" + vl_int_readbuf[0].get(0),
                       vl_int_data[0].get(0).equals(vl_int_readbuf[0].get(0)));
            assertTrue("testH5DVLwr:" + vl_int_readbuf[1].get(0),
                       vl_int_data[1].get(0).equals(vl_int_readbuf[1].get(0)));
            assertTrue("testH5DVLwr:" + vl_int_readbuf[2].get(0),
                       vl_int_data[2].get(0).equals(vl_int_readbuf[2].get(0)));
            assertTrue("testH5DVLwr:" + vl_int_readbuf[3].get(0),
                       vl_int_data[3].get(0).equals(vl_int_readbuf[3].get(0)));

            // Read Double data
            ArrayList[] vl_dbl_readbuf = new ArrayList[4];
            for (int j = 0; j < lsize; j++)
                vl_dbl_readbuf[j] = new ArrayList<Double>();

            try {
                H5.H5DreadVL(dset_dbl_id, dtype_dbl_id, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                             HDF5Constants.H5P_DEFAULT, vl_dbl_readbuf);
            }
            catch (Exception ex) {
                ex.printStackTrace();
            }
            assertTrue("testH5DVLwr:" + vl_dbl_readbuf[0].get(0),
                       vl_dbl_data[0].get(0).equals(vl_dbl_readbuf[0].get(0)));
            assertTrue("testH5DVLwr:" + vl_dbl_readbuf[1].get(0),
                       vl_dbl_data[1].get(0).equals(vl_dbl_readbuf[1].get(0)));
            assertTrue("testH5DVLwr:" + vl_dbl_readbuf[2].get(0),
                       vl_dbl_data[2].get(0).equals(vl_dbl_readbuf[2].get(0)));
            assertTrue("testH5DVLwr:" + vl_dbl_readbuf[3].get(0),
                       vl_dbl_data[3].get(0).equals(vl_dbl_readbuf[3].get(0)));
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.testH5DVLwr: " + err);
        }
        finally {
            if (dset_dbl_id > 0)
                try {
                    H5.H5Dclose(dset_dbl_id);
                }
                catch (Exception ex) {
                }
            if (dset_int_id > 0)
                try {
                    H5.H5Dclose(dset_int_id);
                }
                catch (Exception ex) {
                }
            if (dtype_dbl_id > 0)
                try {
                    H5.H5Tclose(dtype_dbl_id);
                }
                catch (Exception ex) {
                }
            if (dtype_int_id > 0)
                try {
                    H5.H5Tclose(dtype_int_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5DVLwrVL()
    {
        String dset_int_name   = "VLIntdata";
        long dset_int_id       = HDF5Constants.H5I_INVALID_HID;
        long dtype_int_id      = HDF5Constants.H5I_INVALID_HID;
        long base_dtype_int_id = HDF5Constants.H5I_INVALID_HID;
        long dspace_id         = HDF5Constants.H5I_INVALID_HID;
        long[] dims            = {4};
        long lsize             = 1;

        ArrayList[] base_vl_int_data = new ArrayList[4];
        ArrayList[] vl_int_data      = new ArrayList[4];
        try {
            // Write Integer data
            vl_int_data[0]  = new ArrayList<Integer>(Arrays.asList(1));
            vl_int_data[1]  = new ArrayList<Integer>(Arrays.asList(2, 3));
            vl_int_data[2]  = new ArrayList<Integer>(Arrays.asList(4, 5, 6));
            vl_int_data[3]  = new ArrayList<Integer>(Arrays.asList(7, 8, 9, 10));
            Class dataClass = vl_int_data.getClass();
            assertTrue("testH5DVLwrVL.getClass: " + dataClass, dataClass.isArray());

            // Write VL data
            base_vl_int_data[0] = new ArrayList<ArrayList<Integer>>();
            base_vl_int_data[0].add(vl_int_data[0]);
            base_vl_int_data[1] = new ArrayList<ArrayList<Integer>>();
            base_vl_int_data[1].add(vl_int_data[0]);
            base_vl_int_data[1].add(vl_int_data[1]);
            base_vl_int_data[2] = new ArrayList<ArrayList<Integer>>();
            base_vl_int_data[2].add(vl_int_data[0]);
            base_vl_int_data[2].add(vl_int_data[1]);
            base_vl_int_data[2].add(vl_int_data[2]);
            base_vl_int_data[3] = new ArrayList<ArrayList<Integer>>();
            base_vl_int_data[3].add(vl_int_data[0]);
            base_vl_int_data[3].add(vl_int_data[1]);
            base_vl_int_data[3].add(vl_int_data[2]);
            base_vl_int_data[3].add(vl_int_data[3]);

            try {
                dtype_int_id = H5.H5Tvlen_create(HDF5Constants.H5T_STD_U32LE);
                assertTrue("testH5DVLwrVL.H5Tvlen_create: ", dtype_int_id >= 0);
                base_dtype_int_id = H5.H5Tvlen_create(dtype_int_id);
                assertTrue("testH5DVLwrVL.H5Tvlen_create: ", base_dtype_int_id >= 0);
            }
            catch (Exception err) {
                if (base_dtype_int_id > 0)
                    try {
                        H5.H5Tclose(base_dtype_int_id);
                    }
                    catch (Exception ex) {
                    }
                if (dtype_int_id > 0)
                    try {
                        H5.H5Tclose(dtype_int_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("H5.testH5DVLwrVL: " + err);
            }

            try {
                dspace_id = H5.H5Screate_simple(1, dims, null);
                assertTrue(dspace_id > 0);
                dset_int_id = H5.H5Dcreate(H5fid, dset_int_name, base_dtype_int_id, dspace_id,
                                           HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT,
                                           HDF5Constants.H5P_DEFAULT);
                assertTrue("testH5DVLwrVL: ", dset_int_id >= 0);

                H5.H5DwriteVL(dset_int_id, base_dtype_int_id, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                              HDF5Constants.H5P_DEFAULT, base_vl_int_data);
            }
            catch (Exception err) {
                if (dset_int_id > 0)
                    try {
                        H5.H5Dclose(dset_int_id);
                    }
                    catch (Exception ex) {
                    }
                if (dtype_int_id > 0)
                    try {
                        H5.H5Tclose(dtype_int_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("H5.testH5DVLwrVL: " + err);
            }
            finally {
                if (dspace_id > 0)
                    try {
                        H5.H5Sclose(dspace_id);
                    }
                    catch (Exception ex) {
                    }
            }

            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);

            for (int j = 0; j < dims.length; j++)
                lsize *= dims[j];

            // Read Integer data
            ArrayList[] base_vl_readbuf = new ArrayList[4];
            for (int j = 0; j < lsize; j++)
                base_vl_readbuf[j] = new ArrayList<ArrayList<Integer>>();

            try {
                H5.H5DreadVL(dset_int_id, base_dtype_int_id, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                             HDF5Constants.H5P_DEFAULT, base_vl_readbuf);
            }
            catch (Exception ex) {
                ex.printStackTrace();
            }
            ArrayList<ArrayList<Integer>> vl_readbuf = (ArrayList<ArrayList<Integer>>)base_vl_readbuf[0];
            assertTrue("vl_readbuf 0 exists", vl_readbuf != null);
            ArrayList<Integer> vl_readbuf_int = (ArrayList<Integer>)(vl_readbuf.get(0));
            /*
             * System.out.println(); System.out.println("vl_readbuf: " + vl_readbuf);
             * System.out.println("vl_readbuf_int: " + vl_readbuf_int);
             */
            assertTrue("testH5DVLwrVL:" + vl_readbuf_int.get(0),
                       vl_int_data[0].get(0).equals(vl_readbuf_int.get(0)));

            vl_readbuf     = (ArrayList<ArrayList<Integer>>)base_vl_readbuf[1];
            vl_readbuf_int = (ArrayList<Integer>)(vl_readbuf.get(1));
            /*
             * System.out.println("vl_readbuf: " + vl_readbuf); System.out.println("vl_readbuf_int: " +
             * vl_readbuf_int);
             */
            assertTrue("testH5DVLwrVL:" + vl_readbuf_int.get(1),
                       vl_int_data[1].get(1).equals(vl_readbuf_int.get(1)));

            vl_readbuf     = (ArrayList<ArrayList<Integer>>)base_vl_readbuf[2];
            vl_readbuf_int = (ArrayList<Integer>)(vl_readbuf.get(2));
            /*
             * System.out.println("vl_readbuf: " + vl_readbuf); System.out.println("vl_readbuf_int: " +
             * vl_readbuf_int);
             */
            assertTrue("testH5DVLwrVL:" + vl_readbuf_int.get(2),
                       vl_int_data[2].get(2).equals(vl_readbuf_int.get(2)));

            vl_readbuf     = (ArrayList<ArrayList<Integer>>)base_vl_readbuf[3];
            vl_readbuf_int = (ArrayList<Integer>)(vl_readbuf.get(3));
            /*
             * System.out.println("vl_readbuf: " + vl_readbuf); System.out.println("vl_readbuf_int: " +
             * vl_readbuf_int);
             */
            assertTrue("testH5DVLwrVL:" + vl_readbuf_int.get(3),
                       vl_int_data[3].get(3).equals(vl_readbuf_int.get(3)));
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.testH5DVLwrVL: " + err);
        }
        finally {
            if (dset_int_id > 0)
                try {
                    H5.H5Dclose(dset_int_id);
                }
                catch (Exception ex) {
                }
            if (dtype_int_id > 0)
                try {
                    H5.H5Tclose(dtype_int_id);
                }
                catch (Exception ex) {
                }
            if (base_dtype_int_id > 0)
                try {
                    H5.H5Tclose(base_dtype_int_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5DArraywr()
    {
        String dset_int_name = "ArrayIntdata";
        long dset_int_id     = HDF5Constants.H5I_INVALID_HID;
        long dtype_int_id    = HDF5Constants.H5I_INVALID_HID;
        long dspace_id       = HDF5Constants.H5I_INVALID_HID;
        long[] dims          = {4};
        long lsize           = 1;

        ArrayList[] arr_int_data = new ArrayList[4];
        try {
            // Write Integer data
            arr_int_data[0] = new ArrayList<Integer>(Arrays.asList(1, 2, 3, 4));
            arr_int_data[1] = new ArrayList<Integer>(Arrays.asList(2, 3, 4, 5));
            arr_int_data[2] = new ArrayList<Integer>(Arrays.asList(4, 5, 6, 7));
            arr_int_data[3] = new ArrayList<Integer>(Arrays.asList(7, 8, 9, 10));
            Class dataClass = arr_int_data.getClass();
            assertTrue("testH5DArraywr.getClass: " + dataClass, dataClass.isArray());

            try {
                dtype_int_id = H5.H5Tarray_create(HDF5Constants.H5T_STD_U32LE, 1, dims);
                assertTrue("testH5DArraywr.H5Tarray_create: ", dtype_int_id >= 0);
            }
            catch (Exception err) {
                if (dtype_int_id > 0)
                    try {
                        H5.H5Tclose(dtype_int_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("H5.testH5DArraywr: " + err);
            }

            try {
                dspace_id = H5.H5Screate_simple(1, dims, null);
                assertTrue(dspace_id > 0);
                dset_int_id =
                    H5.H5Dcreate(H5fid, dset_int_name, dtype_int_id, dspace_id, HDF5Constants.H5P_DEFAULT,
                                 HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
                assertTrue("testH5DVLwr: ", dset_int_id >= 0);

                H5.H5DwriteVL(dset_int_id, dtype_int_id, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                              HDF5Constants.H5P_DEFAULT, arr_int_data);
            }
            catch (Exception err) {
                if (dset_int_id > 0)
                    try {
                        H5.H5Dclose(dset_int_id);
                    }
                    catch (Exception ex) {
                    }
                if (dtype_int_id > 0)
                    try {
                        H5.H5Tclose(dtype_int_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("H5.testH5DVLwr: " + err);
            }
            finally {
                if (dspace_id > 0)
                    try {
                        H5.H5Sclose(dspace_id);
                    }
                    catch (Exception ex) {
                    }
            }

            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);

            for (int j = 0; j < dims.length; j++)
                lsize *= dims[j];

            // Read Integer data
            ArrayList[] arr_readbuf = new ArrayList[4];
            for (int j = 0; j < lsize; j++)
                arr_readbuf[j] = new ArrayList<Integer>();

            try {
                H5.H5DreadVL(dset_int_id, dtype_int_id, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                             HDF5Constants.H5P_DEFAULT, arr_readbuf);
            }
            catch (Exception ex) {
                ex.printStackTrace();
            }
            assertTrue("testH5DVLwr:" + arr_readbuf[0].get(0),
                       arr_int_data[0].get(0).equals(arr_readbuf[0].get(0)));
            assertTrue("testH5DVLwr:" + arr_readbuf[1].get(0),
                       arr_int_data[1].get(0).equals(arr_readbuf[1].get(0)));
            assertTrue("testH5DVLwr:" + arr_readbuf[2].get(0),
                       arr_int_data[2].get(0).equals(arr_readbuf[2].get(0)));
            assertTrue("testH5DVLwr:" + arr_readbuf[3].get(0),
                       arr_int_data[3].get(0).equals(arr_readbuf[3].get(0)));
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.testH5DArraywr: " + err);
        }
        finally {
            if (dset_int_id > 0)
                try {
                    H5.H5Dclose(dset_int_id);
                }
                catch (Exception ex) {
                }
            if (dtype_int_id > 0)
                try {
                    H5.H5Tclose(dtype_int_id);
                }
                catch (Exception ex) {
                }
        }
    }

    @Test
    public void testH5DArray_string_buffer() throws Throwable
    {
        String dset_str_name = "ArrayStringdata";
        long dset_str_id     = HDF5Constants.H5I_INVALID_HID;
        long dtype_str_id    = HDF5Constants.H5I_INVALID_HID;
        long dspace_id       = HDF5Constants.H5I_INVALID_HID;
        long[] strdims       = {4};
        long[] dims          = {6};
        long lsize           = 1;

        String[] str_data0 = {"Parting", "is such", "sweet", "sorrow."};
        String[] str_data1 = {"Testing", "one", "two", "three."};
        String[] str_data2 = {"Dog,", "man's", "best", "friend."};
        String[] str_data3 = {"Diamonds", "are", "a", "girls!"};
        String[] str_data4 = {"S A", "T U R", "D A Y", "night"};
        String[] str_data5 = {"That's", "all", "folks", "!!!"};

        ArrayList[] arr_str_data = new ArrayList[6];
        arr_str_data[0]          = new ArrayList<String>(Arrays.asList(str_data0));
        arr_str_data[1]          = new ArrayList<String>(Arrays.asList(str_data1));
        arr_str_data[2]          = new ArrayList<String>(Arrays.asList(str_data2));
        arr_str_data[3]          = new ArrayList<String>(Arrays.asList(str_data3));
        arr_str_data[4]          = new ArrayList<String>(Arrays.asList(str_data4));
        arr_str_data[5]          = new ArrayList<String>(Arrays.asList(str_data5));

        try {
            H5dtid = H5.H5Tcopy(HDF5Constants.H5T_C_S1);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5DArray_string_buffer.H5.H5Tcopy: " + err);
        }
        assertTrue("testH5DArray_string_buffer.H5Tcopy: ", H5dtid >= 0);
        try {
            H5.H5Tset_size(H5dtid, HDF5Constants.H5T_VARIABLE);
            assertTrue("testH5DArray_string_buffer.H5Tis_variable_str", H5.H5Tis_variable_str(H5dtid));
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5DArray_string_buffer.H5Tset_size: " + err);
        }
        try {
            dtype_str_id = H5.H5Tarray_create(H5dtid, 1, strdims);
            assertTrue("testH5DArray_string_buffer.H5Tarray_create: ", dtype_str_id >= 0);
        }
        catch (Exception err) {
            if (dtype_str_id > 0)
                try {
                    H5.H5Tclose(dtype_str_id);
                }
                catch (Exception ex) {
                }
            err.printStackTrace();
            fail("testH5DArray_string_buffer: " + err);
        }

        try {
            dspace_id = H5.H5Screate_simple(1, dims, null);
            assertTrue(dspace_id > 0);
            dset_str_id =
                H5.H5Dcreate(H5fid, dset_str_name, dtype_str_id, dspace_id, HDF5Constants.H5P_DEFAULT,
                             HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
            assertTrue("testH5DArray_string_buffer: ", dset_str_id >= 0);

            H5.H5DwriteVL(dset_str_id, dtype_str_id, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                          HDF5Constants.H5P_DEFAULT, arr_str_data);
        }
        catch (Exception err) {
            if (dset_str_id > 0)
                try {
                    H5.H5Dclose(dset_str_id);
                }
                catch (Exception ex) {
                }
            if (dtype_str_id > 0)
                try {
                    H5.H5Tclose(dtype_str_id);
                }
                catch (Exception ex) {
                }
            err.printStackTrace();
            fail("testH5DArray_string_buffer: " + err);
        }
        finally {
            if (dspace_id > 0)
                try {
                    H5.H5Sclose(dspace_id);
                }
                catch (Exception ex) {
                }
        }

        H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);

        for (int j = 0; j < dims.length; j++)
            lsize *= dims[j];

        ArrayList[] arr_readbuf = new ArrayList[6];
        for (int j = 0; j < lsize; j++)
            arr_readbuf[j] = new ArrayList<String>();

        try {
            H5.H5DreadVL(dset_str_id, dtype_str_id, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                         HDF5Constants.H5P_DEFAULT, arr_readbuf);
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }
        finally {
            if (dset_str_id > 0)
                try {
                    H5.H5Dclose(dset_str_id);
                }
                catch (Exception ex) {
                }
            if (dtype_str_id > 0)
                try {
                    H5.H5Tclose(dtype_str_id);
                }
                catch (Exception ex) {
                }
        }
        assertTrue("testH5DArray_string_buffer:" + arr_readbuf[0].get(0),
                   arr_str_data[0].get(0).equals(arr_readbuf[0].get(0)));
        assertTrue("testH5DArray_string_buffer:" + arr_readbuf[1].get(0),
                   arr_str_data[1].get(0).equals(arr_readbuf[1].get(0)));
        assertTrue("testH5DArray_string_buffer:" + arr_readbuf[2].get(0),
                   arr_str_data[2].get(0).equals(arr_readbuf[2].get(0)));
        assertTrue("testH5DArray_string_buffer:" + arr_readbuf[3].get(0),
                   arr_str_data[3].get(0).equals(arr_readbuf[3].get(0)));
    }

    @Test
    public void testH5DArrayenum_rw()
    {
        String dset_enum_name  = "ArrayEnumdata";
        long dset_enum_id      = HDF5Constants.H5I_INVALID_HID;
        long dtype_enum_id     = HDF5Constants.H5I_INVALID_HID;
        long dtype_arr_enum_id = HDF5Constants.H5I_INVALID_HID;
        long dspace_id         = HDF5Constants.H5I_INVALID_HID;
        long[] dims            = {4};
        long lsize             = 1;
        String enum_type       = "Enum_type";
        byte[] enum_val        = new byte[1];
        String enum_name       = null;

        // Create a enumerate datatype
        try {
            dtype_enum_id = H5.H5Tcreate(HDF5Constants.H5T_ENUM, (long)1);
            assertTrue("testH5DArrayenum_wr.H5Tarray_create: ", dtype_enum_id >= 0);
        }
        catch (Throwable err) {
            if (dtype_enum_id > 0)
                try {
                    H5.H5Tclose(dtype_enum_id);
                }
                catch (Exception ex) {
                }
            err.printStackTrace();
            fail("testH5DArrayenum_rw:H5Tcreate " + err);
        }

        try {
            enum_val[0] = 10;
            H5.H5Tenum_insert(dtype_enum_id, "RED", enum_val);
            enum_val[0] = 11;
            H5.H5Tenum_insert(dtype_enum_id, "GREEN", enum_val);
            enum_val[0] = 12;
            H5.H5Tenum_insert(dtype_enum_id, "BLUE", enum_val);
            enum_val[0] = 13;
            H5.H5Tenum_insert(dtype_enum_id, "ORANGE", enum_val);
            enum_val[0] = 14;
            H5.H5Tenum_insert(dtype_enum_id, "YELLOW", enum_val);

            // Query member number and member index by member name, for enumeration type.
            assertTrue("Can't get member number", H5.H5Tget_nmembers(dtype_enum_id) == 5);
            assertTrue("Can't get correct index number",
                       H5.H5Tget_member_index(dtype_enum_id, "ORANGE") == 3);

            // Commit enumeration datatype and close it */
            H5.H5Tcommit(H5fid, enum_type, dtype_enum_id, HDF5Constants.H5P_DEFAULT,
                         HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);

            H5.H5Tclose(dtype_enum_id);

            // Open the dataytpe for query
            dtype_enum_id = H5.H5Topen(H5fid, enum_type, HDF5Constants.H5P_DEFAULT);
            assertTrue("testH5DArrayenum_rw:H5Tcreate", dtype_enum_id >= 0);

            // Query member number and member index by member name, for enumeration type
            assertTrue("Can't get member number", H5.H5Tget_nmembers(dtype_enum_id) == 5);
            assertTrue("Can't get correct index number",
                       H5.H5Tget_member_index(dtype_enum_id, "ORANGE") == 3);

            // Query member value by member name, for enumeration type
            H5.H5Tenum_valueof(dtype_enum_id, "ORANGE", enum_val);
            assertTrue("Incorrect value for enum member", enum_val[0] == 13);

            // Query member value by member index, for enumeration type
            H5.H5Tget_member_value(dtype_enum_id, 2, enum_val);
            assertTrue("Incorrect value for enum member", enum_val[0] == 12);

            // Query member name by member value, for enumeration type
            enum_val[0] = 14;
            enum_name   = H5.H5Tenum_nameof(dtype_enum_id, enum_val, 16);
            assertTrue("Incorrect name for enum member", enum_name.compareTo("YELLOW") == 0);

            ArrayList[] arr_enum_data = new ArrayList[4];
            try {
                // Write Integer data
                arr_enum_data[0] = new ArrayList<Integer>(Arrays.asList(10, 11, 12, 13));
                arr_enum_data[1] = new ArrayList<Integer>(Arrays.asList(11, 12, 13, 14));
                arr_enum_data[2] = new ArrayList<Integer>(Arrays.asList(12, 13, 14, 10));
                arr_enum_data[3] = new ArrayList<Integer>(Arrays.asList(13, 14, 10, 11));
                Class dataClass  = arr_enum_data.getClass();
                assertTrue("testH5DArrayenum_wr.getClass: " + dataClass, dataClass.isArray());

                try {
                    dtype_arr_enum_id = H5.H5Tarray_create(HDF5Constants.H5T_STD_U32LE, 1, dims);
                    assertTrue("testH5DArrayenum_wr.H5Tarray_create: ", dtype_arr_enum_id >= 0);
                }
                catch (Exception err) {
                    if (dtype_arr_enum_id > 0)
                        try {
                            H5.H5Tclose(dtype_arr_enum_id);
                        }
                        catch (Exception ex) {
                        }
                    err.printStackTrace();
                    fail("H5.testH5DArrayenum_wr: " + err);
                }

                dspace_id = H5.H5Screate_simple(1, dims, null);
                assertTrue(dspace_id > 0);
                dset_enum_id = H5.H5Dcreate(H5fid, dset_enum_name, dtype_arr_enum_id, dspace_id,
                                            HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT,
                                            HDF5Constants.H5P_DEFAULT);
                assertTrue("testH5DVLwr: ", dset_enum_id >= 0);

                H5.H5DwriteVL(dset_enum_id, dtype_arr_enum_id, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                              HDF5Constants.H5P_DEFAULT, arr_enum_data);
            }
            catch (Throwable err) {
                if (dset_enum_id > 0)
                    try {
                        H5.H5Dclose(dset_enum_id);
                    }
                    catch (Exception ex) {
                    }
                if (dtype_enum_id > 0)
                    try {
                        H5.H5Tclose(dtype_enum_id);
                    }
                    catch (Exception ex) {
                    }
                if (dtype_arr_enum_id > 0)
                    try {
                        H5.H5Tclose(dtype_arr_enum_id);
                    }
                    catch (Exception ex) {
                    }
                err.printStackTrace();
                fail("testH5DArrayenum_rw:query " + err);
            }
            finally {
                if (dspace_id > 0)
                    try {
                        H5.H5Sclose(dspace_id);
                    }
                    catch (Exception ex) {
                    }
            }

            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);

            for (int j = 0; j < dims.length; j++)
                lsize *= dims[j];

            // Read Integer data
            ArrayList[] arr_readbuf = new ArrayList[4];
            for (int j = 0; j < lsize; j++)
                arr_readbuf[j] = new ArrayList<Integer>();

            try {
                H5.H5DreadVL(dset_enum_id, dtype_arr_enum_id, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                             HDF5Constants.H5P_DEFAULT, arr_readbuf);
            }
            catch (Exception ex) {
                ex.printStackTrace();
            }
            assertTrue("testH5DVLArrayenum_wr:" + arr_readbuf[0].get(0),
                       arr_enum_data[0].get(0).equals(arr_readbuf[0].get(0)));
            assertTrue("testH5DVLArrayenum_wr:" + arr_readbuf[1].get(0),
                       arr_enum_data[1].get(0).equals(arr_readbuf[1].get(0)));
            assertTrue("testH5DVLArrayenum_wr:" + arr_readbuf[2].get(0),
                       arr_enum_data[2].get(0).equals(arr_readbuf[2].get(0)));
            assertTrue("testH5DVLArrayenum_wr:" + arr_readbuf[3].get(0),
                       arr_enum_data[3].get(0).equals(arr_readbuf[3].get(0)));
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.testH5DArrayenum_wr: " + err);
        }
        finally {
            if (dset_enum_id > 0)
                try {
                    H5.H5Dclose(dset_enum_id);
                }
                catch (Exception ex) {
                }
            if (dtype_enum_id > 0)
                try {
                    H5.H5Tclose(dtype_enum_id);
                }
                catch (Exception ex) {
                }
            if (dtype_arr_enum_id > 0)
                try {
                    H5.H5Tclose(dtype_arr_enum_id);
                }
                catch (Exception ex) {
                }
        }
    }
}
