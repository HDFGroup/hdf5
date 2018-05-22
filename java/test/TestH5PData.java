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

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.text.DecimalFormat;
import java.text.NumberFormat;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.exceptions.HDF5Exception;
import hdf.hdf5lib.exceptions.HDF5LibraryException;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5PData {
    @Rule public TestName testname = new TestName();

    private static final String H5_FILE = "testPD.h5";
    private static final int DIM_X = 12;
    private static final int DIM_Y = 18;
    long H5fid = -1;
    long H5dsid = -1;
    long H5did = -1;
    long plist_id = -1;
    long[] H5dims = { DIM_X, DIM_Y };
    double windchillF[][] =
    {{36.0, 31.0, 25.0, 19.0, 13.0, 7.0, 1.0, -5.0, -11.0, -16.0, -22.0, -28.0, -34.0, -40.0, -46.0, -52.0, -57.0, -63.0},
     {34.0, 27.0, 21.0, 15.0, 9.0, 3.0, -4.0, -10.0, -16.0, -22.0, -28.0, -35.0, -41.0, -47.0, -53.0, -59.0, -66.0, -72.0},
     {32.0, 25.0, 19.0, 13.0, 6.0, 0.0, -7.0, -13.0, -19.0, -26.0, -32.0, -39.0, -45.0, -51.0, -58.0, -64.0, -71.0, -77.0},
     {30.0, 24.0, 17.0, 11.0, 4.0, -2.0, -9.0, -15.0, -22.0, -29.0, -35.0, -42.0, -48.0, -55.0, -61.0, -68.0, -74.0, -81.0},
     {29.0, 23.0, 16.0, 9.0, 3.0, -4.0, -11.0, -17.0, -24.0, -31.0, -37.0, -44.0, -51.0, -58.0, -64.0, -71.0, -78.0, -84.0},
     {28.0, 22.0, 15.0, 8.0, 1.0, -5.0, -12.0, -19.0, -26.0, -33.0, -39.0, -46.0, -53.0, -60.0, -67.0, -73.0, -80.0, -87.0},
     {28.0, 21.0, 14.0, 7.0, 0.0, -7.0, -14.0, -21.0, -27.0, -34.0, -41.0, -48.0, -55.0, -62.0, -69.0, -76.0, -82.0, -89.0},
     {27.0, 20.0, 13.0, 6.0, -1.0, -8.0, -15.0, -22.0, -29.0, -36.0, -43.0, -50.0, -57.0, -64.0, -71.0, -78.0, -84.0, -91.0},
     {26.0, 19.0, 12.0, 5.0, -2.0, -9.0, -16.0, -23.0, -30.0, -37.0, -44.0, -51.0, -58.0, -65.0, -72.0, -79.0, -86.0, -93.0},
     {26.0, 19.0, 12.0, 4.0, -3.0, -10.0, -17.0, -24.0, -31.0, -38.0, -45.0, -52.0, -60.0, -67.0, -74.0, -81.0, -88.0, -95.0},
     {25.0, 18.0, 11.0, 4.0, -3.0, -11.0, -18.0, -25.0, -32.0, -39.0, -46.0, -54.0, -61.0, -68.0, -75.0, -82.0, -89.0, -97.0},
     {25.0, 17.0, 10.0, 3.0, -4.0, -11.0, -19.0, -26.0, -33.0, -40.0, -48.0, -55.0, -62.0, -69.0, -76.0, -84.0, -91.0, -98.0}
    };

    private final void _deleteFile(String filename) {
        File file = new File(filename);

        if (file.exists()) {
            try {file.delete();} catch (SecurityException e) {}
        }
    }

    private final long _createFloatDataset(long fid, long dsid, String name, long dapl) {
        long did = -1;
        try {
            did = H5.H5Dcreate(fid, name, HDF5Constants.H5T_NATIVE_FLOAT, dsid,
                    HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, dapl);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Dcreate: " + err);
        }
        assertTrue("TestH5PData._createFloatDataset: ", did > 0);

        return did;
    }

    @Before
    public void createH5file()
            throws NullPointerException, HDF5Exception {
        assertTrue("H5 open ids is 0",H5.getOpenIDCount()==0);
        System.out.print(testname.getMethodName());

        try {
            H5fid = H5.H5Fcreate(H5_FILE, HDF5Constants.H5F_ACC_TRUNC,
                    HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
            H5dsid = H5.H5Screate_simple(2, H5dims, null);
            H5did = _createFloatDataset(H5fid, H5dsid, "dset", HDF5Constants.H5P_DEFAULT);
            plist_id = H5.H5Pcreate(HDF5Constants.H5P_DATASET_XFER);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5PData.createH5file: " + err);
        }
        assertTrue("TestH5PData.createH5file: H5.H5Fcreate: ",H5fid > 0);
        assertTrue("TestH5PData.createH5file: H5.H5Screate_simple: ",H5dsid > 0);
        assertTrue("TestH5PData.createH5file: _createFloatDataset: ",H5did > 0);
        assertTrue(plist_id > 0);

        H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
    }

    @After
    public void deleteH5file() throws HDF5LibraryException {
        if (H5dsid > 0)
            try {H5.H5Sclose(H5dsid);} catch (Exception ex) {}
        if (H5did > 0)
            try {H5.H5Dclose(H5did);} catch (Exception ex) {}
        if (H5fid > 0)
            try {H5.H5Fclose(H5fid);} catch (Exception ex) {}

        _deleteFile(H5_FILE);

        if (plist_id > 0)
            try {H5.H5Pclose(plist_id);} catch (Exception ex) {}
        System.out.println();
    }

    @Test
    public void testH5Pdata_transform() {
        String f_to_c = "(5/9.0)*(x-32)";
        double windchillFread[][] = new double[DIM_X][DIM_Y];
        double windchillC;
        NumberFormat formatter = new DecimalFormat("#0.000");

        try {
            H5.H5Pset_data_transform(plist_id, f_to_c);
            H5.H5Dwrite(H5did, HDF5Constants.H5T_NATIVE_DOUBLE, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                    plist_id, windchillF);
            H5.H5Dread(H5did, HDF5Constants.H5T_NATIVE_DOUBLE, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                    HDF5Constants.H5P_DEFAULT, windchillFread);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pdata_transform: " + err);
        }
        for(int row = 0; row < DIM_X; row++)
            for(int col = 0; col < DIM_Y; col++) {
                windchillC = (5/9.0)*(windchillF[row][col]-32);
                String Cstr = formatter.format(windchillC);
                String Fread = formatter.format(windchillFread[row][col]);
                assertTrue("H5Pdata_transform: <"+row+","+col+">"+Fread+"="+Cstr, Fread.compareTo(Cstr)==0);
            }
    }

    @Test
    public void testH5P_buffer() {
        long default_size = 0;
        long size = 0;

        try {
            default_size = H5.H5Pget_buffer_size(plist_id);
            H5.H5Pset_buffer_size(plist_id, DIM_X*DIM_Y);
            size = H5.H5Pget_buffer_size(plist_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5P_buffer fail: " + err);
        }
        assertTrue("H5P_buffer default: "+default_size, default_size==1024*1024);
        assertTrue("H5P_buffer size: "+size, size==DIM_X*DIM_Y);
    }
}
