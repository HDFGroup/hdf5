/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;

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
    @Rule public TestName testname = new TestName();
    @Before
    public void showTestName() {
        System.out.print(testname.getMethodName());
    }
    @After
    public void nextTestName() {
        System.out.println();
    }

    /**
     * Test method for {@link hdf.hdf5lib.H5#J2C(int)}.
     * NOTE:
     * H5F_ACC_DEBUG no longer prints any special debug info. The symbol is
     * being retained and will be listed as deprecated in HDF5 1.10.0.
     */
    @Test
    public void testJ2C() {
        int H5F_ACC_RDONLY = 0x0000;
        int H5F_ACC_RDWR = 0x0001;
        int H5F_ACC_TRUNC = 0x0002;
        int H5F_ACC_EXCL = 0x0004;
        int H5F_ACC_DEBUG =  0x0000; // HDFFV-1074 was 0x0008;
        int H5F_ACC_CREAT = 0x0010;
        int H5F_OBJ_FILE = 0x0001;
        int H5F_OBJ_DATASET = 0x0002;
        int H5F_OBJ_GROUP = 0x0004;
        int H5F_OBJ_DATATYPE = 0x0008;
        int H5F_OBJ_ATTR = 0x0010;
        int H5F_OBJ_ALL = H5F_OBJ_FILE | H5F_OBJ_DATASET | H5F_OBJ_GROUP
                | H5F_OBJ_DATATYPE | H5F_OBJ_ATTR;
        int H5F_OBJ_LOCAL = 0x0020;

        int definedValues[] = { H5F_ACC_RDONLY, H5F_ACC_RDWR, H5F_ACC_TRUNC,
                H5F_ACC_EXCL, H5F_ACC_DEBUG, H5F_ACC_CREAT, H5F_OBJ_FILE,
                H5F_OBJ_DATASET, H5F_OBJ_GROUP, H5F_OBJ_DATATYPE, H5F_OBJ_ATTR,
                H5F_OBJ_ALL, H5F_OBJ_LOCAL };

        int j2cValues[] = { HDF5Constants.H5F_ACC_RDONLY,
                HDF5Constants.H5F_ACC_RDWR, HDF5Constants.H5F_ACC_TRUNC,
                HDF5Constants.H5F_ACC_EXCL, H5F_ACC_DEBUG,
                HDF5Constants.H5F_ACC_CREAT, HDF5Constants.H5F_OBJ_FILE,
                HDF5Constants.H5F_OBJ_DATASET, HDF5Constants.H5F_OBJ_GROUP,
                HDF5Constants.H5F_OBJ_DATATYPE, HDF5Constants.H5F_OBJ_ATTR,
                HDF5Constants.H5F_OBJ_ALL, HDF5Constants.H5F_OBJ_LOCAL };

        for (int i = 0; i < definedValues.length; i++) {
            assertEquals(definedValues[i], j2cValues[i]);
        }

        assertFalse(H5F_ACC_RDONLY == HDF5Constants.H5F_ACC_RDWR);
        assertFalse(H5F_OBJ_FILE == HDF5Constants.H5F_OBJ_GROUP);
    }

    /**
     * Test method for {@link hdf.hdf5lib.H5#H5error_off()}.
     */
    @Test
    public void testH5error_off() {
        try {
            H5.H5error_off();
        }
        catch (Throwable err) {
            fail("H5.H5error_off failed: " + err);
        }
    }

    /**
     * Test method for {@link hdf.hdf5lib.H5#H5open()}.
     */
    @Test
    public void testH5open() {
        try {
            H5.H5open();
        }
        catch (Throwable err) {
            fail("H5.H5open failed: " + err);
        }
    }

    /**
     * Test method for {@link hdf.hdf5lib.H5#H5garbage_collect()}.
     */
    @Test
    public void testH5garbage_collect() {
        try {
            H5.H5garbage_collect();
        }
        catch (Throwable err) {
            fail("H5.H5garbage_collect failed: " + err);
        }
    }

    /**
     * Test method for
     * {@link hdf.hdf5lib.H5#H5set_free_list_limits(int, int, int, int, int, int)}
     * .
     */
    @Test
    public void testH5set_free_list_limits() {
        int reg_global_lim = 1;
        int reg_list_lim = 1;
        int arr_global_lim = 1;
        int arr_list_lim = 1;
        int blk_global_lim = 1;
        int blk_list_lim = 1;

        try {
            H5.H5set_free_list_limits(reg_global_lim, reg_list_lim,
                    arr_global_lim, arr_list_lim, blk_global_lim, blk_list_lim);
        }
        catch (Throwable err) {
            fail("H5.H5set_free_list_limits failed: " + err);
        }
    }

    /**
     * Test method for {@link hdf.hdf5lib.H5#H5get_libversion(int[])}.
     */
    @Test
    public void testH5get_libversion() {
        int libversion[] = { 1, 9, 0 };

        try {
            H5.H5get_libversion(libversion);
        }
        catch (Throwable err) {
            fail("H5.H5get_libversion: " + err);
        }

        for (int i = 0; i < 2; i++)
            assertEquals(H5.LIB_VERSION[i], libversion[i]);

        for (int i = 0; i < 2; i++)
            assertFalse(libversion[i] == 0);
    }

    /**
     * Test method for
     * {@link hdf.hdf5lib.H5#H5check_version(int, int, int)}.
     */
    @Test
    public void testH5check_version() {
        int majnum = 1, minnum = 9, relnum = 0;

        try {
            H5.H5check_version(majnum, minnum, relnum);
        }
        catch (Throwable err) {
            fail("H5.H5check_version failed: " + err);
        }

        try {
            H5.H5check_version(-1, 0, 0);
        }
        catch (Throwable err) {
            fail("H5.H5check_version failed: " + err);
        }
    }

    @Test
    public void testIsSerializable() {
        H5 test = new H5();
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        ObjectOutputStream oos;
        try {
            oos = new ObjectOutputStream(out);
            oos.writeObject(test);
            oos.close();
        }
        catch (IOException err) {
            err.printStackTrace();
            fail("ObjectOutputStream failed: " + err);
        }
        assertTrue(out.toByteArray().length > 0);

    }

    @SuppressWarnings("static-access")
    @Test
    public void serializeToDisk()
    {
        try {
            H5 test = new H5();

            FileOutputStream fos = new FileOutputStream("temph5.ser");
            ObjectOutputStream oos = new ObjectOutputStream(fos);
            oos.writeObject(test);
            oos.close();
        }
        catch (Exception ex) {
            fail("Exception thrown during test: " + ex.toString());
        }

        try {
            FileInputStream fis = new FileInputStream("temph5.ser");
            ObjectInputStream ois = new ObjectInputStream(fis);
            H5 test = (hdf.hdf5lib.H5) ois.readObject();
            ois.close();

            assertTrue("H5.LIB_VERSION[0]", test.LIB_VERSION[0]==H5.LIB_VERSION[0]);
            assertTrue("H5.LIB_VERSION[1]", test.LIB_VERSION[1]==H5.LIB_VERSION[1]);
//            assertTrue("H5.LIB_VERSION[2]", test.LIB_VERSION[2]==H5.LIB_VERSION[2]);

            // Clean up the file
            new File("temph5.ser").delete();
        }
        catch (Exception ex) {
            fail("Exception thrown during test: " + ex.toString());
        }
    }
}
