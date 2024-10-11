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

import static org.hdfgroup.javahdf5.hdf5_h.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Reader;
import java.io.StreamTokenizer;
import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;
import java.nio.charset.StandardCharsets;

import org.hdfgroup.javahdf5.*;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

/**
 * @author xcao
 *
 */
public class TestH5 {
    @Rule
    public TestName testname                       = new TestName();
    private static final String H5_FILE            = "testData.h5";
    private static final int DIM_X                 = 4;
    private static final int DIM_Y                 = 6;
    private static final int DIM_BLKS              = 36;
    private static final int DIM_PNTS              = 10;
    private static final int DIM_ATTR              = 12;
    private static final int RANK                  = 2;
    long H5fid                                     = H5I_INVALID_HID();
    long H5dsid                                    = H5I_INVALID_HID();
    long H5did                                     = H5I_INVALID_HID();
    long[] H5dims                                  = {DIM_X, DIM_Y};

    private final void _deleteFile(String filename)
    {
        File file = null;
        try {
            file = new File(filename);
        }
        catch (Throwable err) {
        }

        if (file.exists()) {
            try {
                file.delete();
            }
            catch (SecurityException e) {
            }
        }
    }

    private final long _createDataset(long fid, long dsid, String name, long dapl)
    {
        long did = H5I_INVALID_HID();
        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment name_segment = arena.allocateFrom(name);
                did = H5Dcreate2(fid, name_segment, H5T_STD_I32LE_g(), dsid, H5P_DEFAULT(), H5P_DEFAULT(),
                                 dapl);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("Arena: " + err);
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Dcreate: " + err);
        }
        assertTrue("TestH5._createDataset: ", did > 0);

        return did;
    }

    private final void _createH5File()
    {
        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment filename_segment = arena.allocateFrom(H5_FILE);
                H5fid = H5Fcreate(filename_segment, H5F_ACC_TRUNC(), H5P_DEFAULT(), H5P_DEFAULT());
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("Arena: " + err);
            }
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the dims bytes
                MemorySegment H5dims_segment = MemorySegment.ofArray(H5dims);
                H5dsid                       = H5Screate_simple(2, H5dims_segment, null);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("Arena: " + err);
            }
            H5did = _createDataset(H5fid, H5dsid, "dset", H5P_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5.createH5file: " + err);
        }
        assertTrue("TestH5.createH5file: H5Fcreate: ", H5fid > 0);
        assertTrue("TestH5.createH5file: H5Screate_simple: ", H5dsid > 0);
        assertTrue("TestH5.createH5file: _createDataset: ", H5did > 0);

        try {
            H5Fflush(H5fid, H5F_SCOPE_LOCAL());
        }
        catch (Throwable err) {
            err.printStackTrace();
        }
    }

    private final void _closeH5File()
    {
        if (H5did >= 0)
            try {
                H5Dclose(H5did);
            }
            catch (Exception ex) {
            }
        if (H5dsid > 0)
            try {
                H5Sclose(H5dsid);
            }
            catch (Exception ex) {
            }
        if (H5fid > 0)
            try {
                H5Fclose(H5fid);
            }
            catch (Exception ex) {
            }
        H5fid  = H5I_INVALID_HID();
        H5dsid = H5I_INVALID_HID();
        H5did  = H5I_INVALID_HID();
    }

    public void _openH5File(String filename, String dsetname)
    {
        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment filename_segment = arena.allocateFrom(filename);
                H5fid                          = H5Fopen(filename_segment, H5F_ACC_RDONLY(), H5P_DEFAULT());
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("TestH5._openH5file: " + err);
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5._openH5file: " + err);
        }
        assertTrue("TestH5._openH5file: H5Fopen: ", H5fid >= 0);
        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the string bytes
                MemorySegment dsetname_segment = arena.allocateFrom(dsetname);
                H5did                          = H5Dopen2(H5fid, dsetname_segment, H5P_DEFAULT());
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("TestH5._openH5file: " + err);
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5._openH5file: " + err);
        }
        assertTrue("TestH5._openH5file: H5Dopen: ", H5did >= 0);
        try {
            H5dsid = H5Dget_space(H5did);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5._openH5file: " + err);
        }
        assertTrue("TestH5._openH5file: H5Dget_space: ", H5dsid > 0);
    }

    public final void _deleteH5file()
    {
        _closeH5File();
        _deleteFile(H5_FILE);
    }

    @After
    public void closeH5File() throws Exception
    {
        _closeH5File();
        System.out.println();
    }

    @Before
    public void verifyCount() throws NullPointerException, Exception
    {
        System.out.print(testname.getMethodName());
    }

    /**
     * NOTE:
     * H5F_ACC_DEBUG no longer prints any special debug info. Even though the symbol is
     * being retained hdf java does not access the symbol.
     */
    @Test
    public void testJ2C()
    {
        int H5F_ACC_RDONLY   = 0x0000;
        int H5F_ACC_RDWR     = 0x0001;
        int H5F_ACC_TRUNC    = 0x0002;
        int H5F_ACC_EXCL     = 0x0004;
        int H5F_ACC_CREAT    = 0x0010;
        int H5F_OBJ_FILE     = 0x0001;
        int H5F_OBJ_DATASET  = 0x0002;
        int H5F_OBJ_GROUP    = 0x0004;
        int H5F_OBJ_DATATYPE = 0x0008;
        int H5F_OBJ_ATTR     = 0x0010;
        int H5F_OBJ_ALL   = H5F_OBJ_FILE | H5F_OBJ_DATASET | H5F_OBJ_GROUP | H5F_OBJ_DATATYPE | H5F_OBJ_ATTR;
        int H5F_OBJ_LOCAL = 0x0020;

        int definedValues[] = {H5F_ACC_RDONLY,   H5F_ACC_RDWR, H5F_ACC_TRUNC,   H5F_ACC_EXCL,
                               H5F_ACC_CREAT,    H5F_OBJ_FILE, H5F_OBJ_DATASET, H5F_OBJ_GROUP,
                               H5F_OBJ_DATATYPE, H5F_OBJ_ATTR, H5F_OBJ_ALL,     H5F_OBJ_LOCAL};

        int j2cValues[] = {H5F_ACC_RDONLY(),   H5F_ACC_RDWR(), H5F_ACC_TRUNC(),   H5F_ACC_EXCL(),
                           H5F_ACC_CREAT(),    H5F_OBJ_FILE(), H5F_OBJ_DATASET(), H5F_OBJ_GROUP(),
                           H5F_OBJ_DATATYPE(), H5F_OBJ_ATTR(), H5F_OBJ_ALL(),     H5F_OBJ_LOCAL()};

        for (int i = 0; i < definedValues.length; i++) {
            assertEquals(definedValues[i], j2cValues[i]);
        }

        assertFalse(H5F_ACC_RDONLY == H5F_ACC_RDWR());
        assertFalse(H5F_OBJ_FILE == H5F_OBJ_GROUP());
    }

    /**
     * Test method for H5Eget_auto2.
     */
    @Ignore
    public void testH5Eget_auto2()
    {
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the array bytes
//            MemorySegment efunc = arena.allocateFrom(ValueLayout.ADDRESS);
//            MemorySegment edata = arena.allocateFrom(ValueLayout.ADDRESS);
//            try {
//                H5Eget_auto2(H5E_DEFAULT(), efunc, edata);
//                H5Eset_auto2(H5E_DEFAULT(), null, null);
//                H5Eset_auto2(H5E_DEFAULT(), efunc, edata);
//            }
//            catch (Throwable err) {
//                fail("H5Eget_auto2 failed: " + err);
//            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("Arena: " + err);
        }
    }

    /**
     * Test method for H5open().
     */
    @Test
    public void testH5open()
    {
        try {
            H5open();
        }
        catch (Throwable err) {
            fail("H5.H5open failed: " + err);
        }
    }

    /**
     * Test method for H5garbage_collect().
     */
    @Test
    public void testH5garbage_collect()
    {
        try {
            H5garbage_collect();
        }
        catch (Throwable err) {
            fail("H5garbage_collect failed: " + err);
        }
    }

    /**
     * Test method for H5set_free_list_limits(int, int, int, int, int, int).
     */
    @Test
    public void testH5set_free_list_limits()
    {
        int reg_global_lim = 1;
        int reg_list_lim   = 1;
        int arr_global_lim = 1;
        int arr_list_lim   = 1;
        int blk_global_lim = 1;
        int blk_list_lim   = 1;

        try {
            H5set_free_list_limits(reg_global_lim, reg_list_lim, arr_global_lim, arr_list_lim, blk_global_lim,
                                   blk_list_lim);
        }
        catch (Throwable err) {
            fail("H5set_free_list_limits failed: " + err);
        }
    }

    /**
     * Test method for H5get_libversion(int*, int*, int*).
     */
    @Test
    public void testH5get_libversion()
    {
        int libversion[] = {1, 17, 0};

        try {
            try (Arena arena = Arena.ofConfined()) {
                // Allocate a MemorySegment to hold the array bytes
                MemorySegment majnum_segment = arena.allocateFrom(ValueLayout.JAVA_INT, libversion[0]);
                MemorySegment minnum_segment = arena.allocateFrom(ValueLayout.JAVA_INT, libversion[1]);
                MemorySegment relnum_segment = arena.allocateFrom(ValueLayout.JAVA_INT, libversion[2]);
                H5get_libversion(majnum_segment, minnum_segment, relnum_segment);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("Arena: " + err);
            }
        }
        catch (Throwable err) {
            fail("H5get_libversion: " + err);
        }

        assertEquals(H5_VERS_MAJOR(), libversion[0]);
        assertEquals(H5_VERS_MINOR(), libversion[1]);
        assertEquals(H5_VERS_RELEASE(), libversion[2]);
    }

    /**
     * Test method for H5get_libversion(int[])
     * to ensure a null libversion parameter causes the function to
     * fail.
     */
    @Test
    public void testH5get_libversion_null_param()
    {
        try {
            H5get_libversion(null, null, null);
        }
        catch (Throwable err) {
            return;
        }

        fail("H5get_libversion: succeeded with a null libversion parameter!");
    }

    /**
     * Test method for H5check_version(int, int, int).
     */
    @Test
    public void testH5check_version()
    {
        int majnum = 1, minnum = 17, relnum = 0;

        try {
            H5check_version(majnum, minnum, relnum);
        }
        catch (Throwable err) {
            fail("H5check_version failed: " + err);
        }

        try {
            H5check_version(-1, 0, 0);
        }
        catch (Throwable err) {
            fail("H5check_version failed: " + err);
        }
    }
}
