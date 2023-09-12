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

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.HDFNativeData;
import hdf.hdf5lib.callbacks.H5A_iterate_cb;
import hdf.hdf5lib.callbacks.H5A_iterate_t;
import hdf.hdf5lib.exceptions.HDF5Exception;
import hdf.hdf5lib.exceptions.HDF5LibraryException;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5Arw {
    @Rule
    public TestName testname                 = new TestName();
    private static final String H5_INTS_FILE = "tintsattrs.h5";
    private static final String H5_FLTS_FILE = "tfloatsattrs.h5";
    private static final int DIM_X           = 8;
    private static final int DIM8_Y          = 8;
    private static final int DIM16_Y         = 16;
    private static final int DIM32_Y         = 32;
    private static final int DIM64_Y         = 64;
    private static final int DIM128_Y        = 128;
    private static final String DATASETU08   = "DU08BITS";
    private static final String DATASETS08   = "DS08BITS";
    private static final String DATASETU16   = "DU16BITS";
    private static final String DATASETS16   = "DS16BITS";
    private static final String DATASETU32   = "DU32BITS";
    private static final String DATASETS32   = "DS32BITS";
    private static final String DATASETU64   = "DU64BITS";
    private static final String DATASETS64   = "DS64BITS";
    private static final String DATASETF32   = "DS32BITS";
    private static final String DATASETF64   = "DS64BITS";
    private static final String DATASETF128  = "DS128BITS";
    private static final int RANK            = 2;
    long H5fid                               = HDF5Constants.H5I_INVALID_HID;
    long H5aid                               = HDF5Constants.H5I_INVALID_HID;
    long H5did                               = HDF5Constants.H5I_INVALID_HID;

    private final void _closeH5file() throws HDF5LibraryException
    {
        if (H5aid >= 0)
            try {
                H5.H5Aclose(H5aid);
            }
            catch (Exception ex) {
            }
        if (H5did >= 0)
            try {
                H5.H5Dclose(H5did);
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

    public void openH5file(String filename, String dsetname)
    {
        try {
            H5fid = H5.H5Fopen(filename, HDF5Constants.H5F_ACC_RDONLY, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5Arw._openH5file: " + err);
        }
        assertTrue("TestH5Arw._openH5file: H5.H5Fopen: ", H5fid >= 0);
        try {
            H5did = H5.H5Dopen(H5fid, dsetname, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5Arw._openH5file: " + err);
        }
        assertTrue("TestH5Arw._openH5file: H5.H5Dopen: ", H5did >= 0);
        try {
            H5aid = H5.H5Aopen(H5did, dsetname, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5Arw._openH5file: " + err);
        }
        assertTrue("TestH5Arw._openH5file: H5.H5Aopen: ", H5aid >= 0);
    }

    @After
    public void closeH5file() throws HDF5LibraryException
    {
        if (H5aid >= 0)
            try {
                H5.H5Aclose(H5aid);
            }
            catch (Exception ex) {
            }
        if (H5did >= 0)
            try {
                H5.H5Dclose(H5did);
            }
            catch (Exception ex) {
            }
        if (H5fid > 0)
            try {
                H5.H5Fclose(H5fid);
            }
            catch (Exception ex) {
            }
        H5fid = HDF5Constants.H5I_INVALID_HID;
        H5did = HDF5Constants.H5I_INVALID_HID;
        H5aid = HDF5Constants.H5I_INVALID_HID;
        System.out.println();
    }

    @Before
    public void verifyCount() throws NullPointerException, HDF5Exception
    {
        assertTrue("H5 open ids is 0", H5.getOpenIDCount() == 0);
        System.out.print(testname.getMethodName());
    }

    @Test
    public void testH5Aread_8bit_ints()
    {
        byte[][] attr_data = new byte[DIM_X][DIM8_Y];

        try {
            openH5file(H5_INTS_FILE, DATASETU08);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Aread_8bit_ints: openH5file: " + err);
        }

        // Read data.
        try {
            H5.H5Aread(H5aid, HDF5Constants.H5T_NATIVE_UINT8, attr_data);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Aread_8bit_ints: H5Aread: " + err);
        }

        // End access to the attribute and release resources used by it.
        try {
            H5.H5Aclose(H5aid);
        }
        catch (Exception err) {
            err.printStackTrace();
        }

        // End access to the dataset and release resources used by it.
        try {
            H5.H5Dclose(H5did);
        }
        catch (Exception err) {
            err.printStackTrace();
        }

        // Open an existing dataset.
        try {
            H5did = H5.H5Dopen(H5fid, DATASETS08, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Aread_8bit_ints: H5Dopen: " + err);
        }

        // Open an existing attribute.
        try {
            H5aid = H5.H5Aopen(H5did, DATASETS08, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Aread_8bit_ints: H5Aopen: " + err);
        }

        // Read data.
        try {
            H5.H5Aread(H5aid, HDF5Constants.H5T_NATIVE_INT8, attr_data);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Aread_8bit_ints: H5Aread: " + err);
        }
    }

    @Test
    public void testH5Aread_16bit_ints()
    {
        short[][] attr_data = new short[DIM_X][DIM16_Y];

        try {
            openH5file(H5_INTS_FILE, DATASETU16);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Aread_16bit_ints: openH5file: " + err);
        }

        // Read data.
        try {
            H5.H5Aread(H5aid, HDF5Constants.H5T_NATIVE_UINT16, attr_data);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Aread_16bit_ints: H5Aread: " + err);
        }

        // End access to the attribute and release resources used by it.
        try {
            H5.H5Aclose(H5aid);
        }
        catch (Exception err) {
            err.printStackTrace();
        }

        // End access to the dataset and release resources used by it.
        try {
            H5.H5Dclose(H5did);
        }
        catch (Exception err) {
            err.printStackTrace();
        }

        // Open an existing dataset.
        try {
            H5did = H5.H5Dopen(H5fid, DATASETS16, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Aread_16bit_ints: H5Dopen: " + err);
        }

        // Open an existing attribute.
        try {
            H5aid = H5.H5Aopen(H5did, DATASETS16, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Aread_16bit_ints: H5Aopen: " + err);
        }

        // Read data.
        try {
            H5.H5Aread(H5aid, HDF5Constants.H5T_NATIVE_INT16, attr_data);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Aread_16bit_ints: H5Aread: " + err);
        }
    }

    @Test
    public void testH5Aread_32bit_ints()
    {
        int[][] attr_data = new int[DIM_X][DIM32_Y];

        try {
            openH5file(H5_INTS_FILE, DATASETU32);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Aread_32bit_ints: openH5file: " + err);
        }

        // Read data.
        try {
            H5.H5Aread(H5aid, HDF5Constants.H5T_NATIVE_UINT32, attr_data);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Aread_32bit_ints: H5Aread: " + err);
        }

        // End access to the attribute and release resources used by it.
        try {
            H5.H5Aclose(H5aid);
        }
        catch (Exception err) {
            err.printStackTrace();
        }

        // End access to the dataset and release resources used by it.
        try {
            H5.H5Dclose(H5did);
        }
        catch (Exception err) {
            err.printStackTrace();
        }

        // Open an existing dataset.
        try {
            H5did = H5.H5Dopen(H5fid, DATASETS32, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Aread_32bit_ints: H5Dopen: " + err);
        }

        // Open an existing attribute.
        try {
            H5aid = H5.H5Aopen(H5did, DATASETS32, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Aread_32bit_ints: H5Aopen: " + err);
        }

        // Read data.
        try {
            H5.H5Aread(H5aid, HDF5Constants.H5T_NATIVE_INT32, attr_data);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Aread_32bit_ints: H5Aread: " + err);
        }
    }

    @Test
    public void testH5Aread_64bit_ints()
    {
        long[][] attr_data = new long[DIM_X][DIM64_Y];

        try {
            openH5file(H5_INTS_FILE, DATASETU64);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Aread_64bit_ints: openH5file: " + err);
        }

        // Read data.
        try {
            H5.H5Aread(H5aid, HDF5Constants.H5T_NATIVE_UINT64, attr_data);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Aread_64bit_ints: H5Aread: " + err);
        }

        // End access to the attribute and release resources used by it.
        try {
            H5.H5Aclose(H5aid);
        }
        catch (Exception err) {
            err.printStackTrace();
        }

        // End access to the dataset and release resources used by it.
        try {
            H5.H5Dclose(H5did);
        }
        catch (Exception err) {
            err.printStackTrace();
        }

        // Open an existing dataset.
        try {
            H5did = H5.H5Dopen(H5fid, DATASETS64, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Aread_64bit_ints: H5Dopen: " + err);
        }

        // Open an existing attribute.
        try {
            H5aid = H5.H5Aopen(H5did, DATASETS64, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Aread_64bit_ints: H5Aopen: " + err);
        }

        // Read data.
        try {
            H5.H5Aread(H5aid, HDF5Constants.H5T_NATIVE_INT64, attr_data);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Aread_64bit_ints: H5Aread: " + err);
        }
    }

    @Test
    public void testH5Aread_32bit_floats()
    {
        float[][] attr_data = new float[DIM_X][DIM32_Y];

        try {
            openH5file(H5_FLTS_FILE, DATASETF32);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Aread_32bit_floats: openH5file: " + err);
        }

        // Read data.
        try {
            H5.H5Aread(H5aid, HDF5Constants.H5T_NATIVE_FLOAT, attr_data);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Aread_32bit_floats: H5Aread: " + err);
        }
        for (int i = 0; i < DIM_X; i++)
            assertTrue("testH5Aread_32bit_floats - H5.H5Aread: ", attr_data[i][0] == (32 - i));
    }

    @Test
    public void testH5Aread_64bit_floats()
    {
        double[][] attr_data = new double[DIM_X][DIM64_Y];

        try {
            openH5file(H5_FLTS_FILE, DATASETF64);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Aread_64bit_floats: openH5file: " + err);
        }

        // Read data.
        try {
            H5.H5Aread(H5aid, HDF5Constants.H5T_NATIVE_DOUBLE, attr_data);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Aread_64bit_floats: H5Aread: " + err);
        }
        for (int i = 0; i < DIM_X; i++)
            assertTrue("testH5Aread_64bit_floats - H5.H5Aread: ", attr_data[i][0] == (64 - i));
    }

    @Test
    public void testH5Aread_128bit_floats()
    {
        byte[][][] attr_data = new byte[DIM_X][DIM128_Y][8];

        try {
            openH5file(H5_FLTS_FILE, DATASETF128);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Aread_128bit_floats: openH5file: " + err);
        }

        // Read data.
        try {
            H5.H5Aread(H5aid, HDF5Constants.H5T_NATIVE_LDOUBLE, attr_data);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("testH5Aread_128bit_floats: H5Aread: " + err);
        }
    }
}
