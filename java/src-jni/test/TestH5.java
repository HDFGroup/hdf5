/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

package test;

import static org.junit.Assert.assertArrayEquals;
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
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;

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

/**
 * @author xcao
 *
 */
public class TestH5 {
    @Rule
    public TestName testname                       = new TestName();
    private static final String H5_FILE            = "testData.h5";
    private static final String EXPORT_FILE        = "testExport.txt";
    private static final String H5_REGION_FILE     = "trefer_reg.h5";
    private static final String EXPORT_REGION_FILE = "testExportReg.txt";
    private static final String H5_ATTR_FILE       = "trefer_attr.h5";
    private static final String EXPORT_ATTR_FILE   = "testExportAttr.txt";
    private static final String H5_DREG_FILE       = "tdatareg.h5";
    private static final String EXPORT_DREG_FILE   = "testExportDReg.txt";
    private static final String H5_AREG_FILE       = "tattrreg.h5";
    private static final String EXPORT_AREG_FILE   = "testExportAReg.txt";
    private static final String H5_TYPES_FILE      = "texporttypes.h5";
    private static final String EXPORT_TYPES_TEXT  = "testExportTypes.txt";
    private static final String EXPORT_TYPES_BIN   = "testExportTypes.bin";
    private static final int DIM_X                 = 4;
    private static final int DIM_Y                 = 6;
    private static final int DIM_BLKS              = 36;
    private static final int DIM_PNTS              = 10;
    private static final int DIM_ATTR              = 12;
    private static final int RANK                  = 2;
    long H5fid                                     = HDF5Constants.H5I_INVALID_HID;
    long H5dsid                                    = HDF5Constants.H5I_INVALID_HID;
    long H5did                                     = HDF5Constants.H5I_INVALID_HID;
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
        long did = HDF5Constants.H5I_INVALID_HID;
        try {
            did = H5.H5Dcreate(fid, name, HDF5Constants.H5T_STD_I32LE, dsid, HDF5Constants.H5P_DEFAULT,
                               HDF5Constants.H5P_DEFAULT, dapl);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Dcreate: " + err);
        }
        assertTrue("TestH5._createDataset: ", did > 0);

        return did;
    }

    private final void _createH5File()
    {
        try {
            H5fid  = H5.H5Fcreate(H5_FILE, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                                  HDF5Constants.H5P_DEFAULT);
            H5dsid = H5.H5Screate_simple(2, H5dims, null);
            H5did  = _createDataset(H5fid, H5dsid, "dset", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5Pfapl.createH5file: " + err);
        }
        assertTrue("TestH5.createH5file: H5.H5Fcreate: ", H5fid > 0);
        assertTrue("TestH5.createH5file: H5.H5Screate_simple: ", H5dsid > 0);
        assertTrue("TestH5.createH5file: _createDataset: ", H5did > 0);

        try {
            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
        }
        catch (Throwable err) {
            err.printStackTrace();
        }
    }

    private final void _closeH5File()
    {
        if (H5did >= 0)
            try {
                H5.H5Dclose(H5did);
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
        H5fid  = HDF5Constants.H5I_INVALID_HID;
        H5dsid = HDF5Constants.H5I_INVALID_HID;
        H5did  = HDF5Constants.H5I_INVALID_HID;
    }

    public void _openH5File(String filename, String dsetname)
    {
        try {
            H5fid = H5.H5Fopen(filename, HDF5Constants.H5F_ACC_RDONLY, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5._openH5file: " + err);
        }
        assertTrue("TestH5._openH5file: H5.H5Fopen: ", H5fid >= 0);
        try {
            H5did = H5.H5Dopen(H5fid, dsetname, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5._openH5file: " + err);
        }
        assertTrue("TestH5._openH5file: H5.H5Dopen: ", H5did >= 0);
        try {
            H5dsid = H5.H5Dget_space(H5did);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5._openH5file: " + err);
        }
        assertTrue("TestH5._openH5file: H5.H5Screate_simple: ", H5dsid > 0);
    }

    public final void _deleteH5file()
    {
        _closeH5File();
        _deleteFile(H5_FILE);
    }

    @After
    public void closeH5File() throws HDF5LibraryException
    {
        _closeH5File();
        assertTrue("H5 open ids is 0", H5.getOpenIDCount() == 0);
        System.out.println();
    }

    @Before
    public void verifyCount() throws NullPointerException, HDF5Exception
    {
        assertTrue("H5 open ids is 0", H5.getOpenIDCount() == 0);
        System.out.print(testname.getMethodName());
    }

    /**
     * Test method for {@link hdf.hdf5lib.H5#J2C(int)}.
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

        int j2cValues[] = {
            HDF5Constants.H5F_ACC_RDONLY,  HDF5Constants.H5F_ACC_RDWR,  HDF5Constants.H5F_ACC_TRUNC,
            HDF5Constants.H5F_ACC_EXCL,    HDF5Constants.H5F_ACC_CREAT, HDF5Constants.H5F_OBJ_FILE,
            HDF5Constants.H5F_OBJ_DATASET, HDF5Constants.H5F_OBJ_GROUP, HDF5Constants.H5F_OBJ_DATATYPE,
            HDF5Constants.H5F_OBJ_ATTR,    HDF5Constants.H5F_OBJ_ALL,   HDF5Constants.H5F_OBJ_LOCAL};

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
    public void testH5error_off()
    {
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
    public void testH5open()
    {
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
    public void testH5garbage_collect()
    {
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
    public void testH5set_free_list_limits()
    {
        int reg_global_lim = 1;
        int reg_list_lim   = 1;
        int arr_global_lim = 1;
        int arr_list_lim   = 1;
        int blk_global_lim = 1;
        int blk_list_lim   = 1;

        try {
            H5.H5set_free_list_limits(reg_global_lim, reg_list_lim, arr_global_lim, arr_list_lim,
                                      blk_global_lim, blk_list_lim);
        }
        catch (Throwable err) {
            fail("H5.H5set_free_list_limits failed: " + err);
        }
    }

    /**
     * Test method for {@link hdf.hdf5lib.H5#H5get_libversion(int[])}.
     */
    @Test
    public void testH5get_libversion()
    {
        int libversion[] = {2, 0, 1};

        try {
            H5.H5get_libversion(libversion);
        }
        catch (Throwable err) {
            fail("H5.H5get_libversion: " + err);
        }

        for (int i = 0; i < 3; i++)
            assertEquals(H5.LIB_VERSION[i], libversion[i]);
    }

    /**
     * Test method for {@link hdf.hdf5lib.H5#H5get_libversion(int[])}
     * to ensure a null libversion parameter causes the function to
     * fail.
     */
    @Test
    public void testH5get_libversion_null_param()
    {
        try {
            H5.H5get_libversion(null);
        }
        catch (Throwable err) {
            return;
        }

        fail("H5.H5get_libversion: succeeded with a null libversion parameter!");
    }

    /**
     * Test method for
     * {@link hdf.hdf5lib.H5#H5check_version(int, int, int)}.
     */
    @Test
    public void testH5check_version()
    {
        int majnum = 2, minnum = 0, relnum = 1;

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
    public void testIsSerializable()
    {
        H5 test                   = new H5();
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

            FileOutputStream fos   = new FileOutputStream("temph5.ser");
            ObjectOutputStream oos = new ObjectOutputStream(fos);
            oos.writeObject(test);
            oos.close();
        }
        catch (Exception ex) {
            fail("Exception thrown during test: " + ex.toString());
        }

        try {
            FileInputStream fis   = new FileInputStream("temph5.ser");
            ObjectInputStream ois = new ObjectInputStream(fis);
            H5 test               = (hdf.hdf5lib.H5)ois.readObject();
            ois.close();

            assertTrue("H5.LIB_VERSION[0]", test.LIB_VERSION[0] == H5.LIB_VERSION[0]);
            assertTrue("H5.LIB_VERSION[1]", test.LIB_VERSION[1] == H5.LIB_VERSION[1]);
            //            assertTrue("H5.LIB_VERSION[2]", test.LIB_VERSION[2]==H5.LIB_VERSION[2]);

            // Clean up the file
            new File("temph5.ser").delete();
        }
        catch (Exception ex) {
            fail("Exception thrown during test: " + ex.toString());
        }
    }

    @Ignore
    public void testH5export_dataset()
    {
        int[][] dset_data   = new int[DIM_X][DIM_Y];
        int[][] dset_indata = new int[DIM_X][DIM_Y];
        int FILLVAL         = 99;

        _createH5File();

        // Initialize the dataset.
        for (int indx = 0; indx < DIM_X; indx++)
            for (int jndx = 0; jndx < DIM_Y; jndx++)
                dset_data[indx][jndx] = FILLVAL;

        try {
            if (H5did >= 0)
                H5.H5Dwrite(H5did, HDF5Constants.H5T_STD_I32LE, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                            HDF5Constants.H5P_DEFAULT, dset_data);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        _closeH5File();

        _openH5File(H5_FILE, "/dset");

        try {
            H5.H5export_dataset(EXPORT_FILE, H5fid, "/dset", 99);
        }
        catch (HDF5LibraryException err) {
            err.printStackTrace();
            fail("H5export_dataset failed: " + err);
        }

        File file = new File(EXPORT_FILE);

        try {
            Reader reader                   = new FileReader(EXPORT_FILE);
            StreamTokenizer streamTokenizer = new StreamTokenizer(reader);
            int indx                        = 0;
            int jndx                        = 0;
            while (streamTokenizer.nextToken() != StreamTokenizer.TT_EOF) {
                if (streamTokenizer.ttype == StreamTokenizer.TT_NUMBER) {
                    dset_indata[indx][jndx] = (int)streamTokenizer.nval;
                    jndx++;
                    if (jndx >= DIM_Y) {
                        jndx = 0;
                        indx++;
                    }
                }
            }
            reader.close();
        }
        catch (IOException err) {
            err.printStackTrace();
            fail("read file failed: " + err);
        }
        for (int row = 0; row < DIM_X; row++)
            for (int col = 0; col < DIM_Y; col++) {
                assertTrue("H5export_dataset: <" + row + "," + col + ">" + dset_indata[row][col] + "=99",
                           dset_indata[row][col] == 99);
            }
        _deleteH5file();
    }

    @Ignore
    public void testH5export_region()
    {
        int[] dset_data_expect = {66,  69,  72,  75,  78,  81,  96,  99,  102, 105, 108, 111,
                                  126, 129, 132, 135, 138, 141, 156, 159, 162, 165, 168, 171,
                                  186, 189, 192, 195, 198, 201, 216, 219, 222, 225, 228, 231,
                                  207, 66,  252, 48,  84,  96,  12,  14,  213, 99};
        int[] dset_indata      = new int[DIM_BLKS + DIM_PNTS];
        String objName         = "/Dataset1";

        _openH5File(H5_REGION_FILE, objName);

        try {
            H5.H5export_dataset(EXPORT_REGION_FILE, H5fid, objName, 99);
        }
        catch (HDF5LibraryException err) {
            err.printStackTrace();
            fail("H5export_dataset failed: " + err);
        }

        File file = new File(EXPORT_REGION_FILE);

        try {
            Reader reader                   = new FileReader(EXPORT_REGION_FILE);
            StreamTokenizer streamTokenizer = new StreamTokenizer(reader);
            int indx                        = 0;
            while (streamTokenizer.nextToken() != StreamTokenizer.TT_EOF) {
                if (streamTokenizer.ttype == StreamTokenizer.TT_NUMBER) {
                    dset_indata[indx] = (int)streamTokenizer.nval;
                    indx++;
                }
            }
            reader.close();
        }
        catch (IOException err) {
            err.printStackTrace();
            fail("read file failed: " + err);
        }
        for (int row = 0; row < DIM_X; row++)
            assertTrue("testH5export_region: <" + row + ">" + dset_indata[row],
                       dset_indata[row] == dset_data_expect[row]);
    }

    @Ignore
    public void testH5export_attribute()
    {
        int[] dset_data_expect = {0, 3, 6, 9, 1, 4, 7, 10, 2, 5, 8, 11};
        int[] dset_indata      = new int[DIM_ATTR];
        String objName         = "/Dataset3";

        _openH5File(H5_ATTR_FILE, objName);

        try {
            H5.H5export_dataset(EXPORT_ATTR_FILE, H5did, objName, 99);
        }
        catch (HDF5LibraryException err) {
            err.printStackTrace();
            fail("H5export_dataset failed: " + err);
        }

        File file = new File(EXPORT_ATTR_FILE);

        try {
            Reader reader                   = new FileReader(EXPORT_ATTR_FILE);
            StreamTokenizer streamTokenizer = new StreamTokenizer(reader);
            int indx                        = 0;
            int jndx                        = 0;
            while (streamTokenizer.nextToken() != StreamTokenizer.TT_EOF) {
                if (streamTokenizer.ttype == StreamTokenizer.TT_NUMBER) {
                    dset_indata[indx] = (int)streamTokenizer.nval;
                    indx++;
                }
            }
            reader.close();
        }
        catch (IOException err) {
            err.printStackTrace();
            fail("read file failed: " + err);
        }
        for (int row = 0; row < DIM_X; row++)
            assertTrue("testH5export_attribute: <" + row + ">" + dset_indata[row],
                       dset_indata[row] == dset_data_expect[row]);
    }

    @Ignore
    public void testH5export_regdataset()
    {
        int[] dset_data_expect = {66,  69,  72,  75,  78,  81,  96,  99,  102, 105, 108, 111,
                                  126, 129, 132, 135, 138, 141, 156, 159, 162, 165, 168, 171,
                                  186, 189, 192, 195, 198, 201, 216, 219, 222, 225, 228, 231,
                                  207, 66,  252, 48,  84,  96,  12,  14,  213, 99};
        int[] dset_indata      = new int[DIM_BLKS + DIM_PNTS];
        String objName         = "/Dataset1";

        _openH5File(H5_DREG_FILE, objName);

        try {
            H5.H5export_dataset(EXPORT_DREG_FILE, H5fid, objName, 99);
        }
        catch (HDF5LibraryException err) {
            err.printStackTrace();
            fail("H5export_dataset failed: " + err);
        }

        File file = new File(EXPORT_DREG_FILE);

        try {
            Reader reader                   = new FileReader(EXPORT_DREG_FILE);
            StreamTokenizer streamTokenizer = new StreamTokenizer(reader);
            int indx                        = 0;
            while (streamTokenizer.nextToken() != StreamTokenizer.TT_EOF) {
                if (streamTokenizer.ttype == StreamTokenizer.TT_NUMBER) {
                    dset_indata[indx] = (int)streamTokenizer.nval;
                    indx++;
                }
            }
            reader.close();
        }
        catch (IOException err) {
            err.printStackTrace();
            fail("read file failed: " + err);
        }
        for (int row = 0; row < DIM_X; row++)
            assertTrue("testH5export_regdataset: <" + row + ">" + dset_indata[row],
                       dset_indata[row] == dset_data_expect[row]);
    }

    @Ignore
    public void testH5export_attrdataset()
    {
        int[] dset_data_expect = {66,  69,  72,  75,  78,  81,  96,  99,  102, 105, 108, 111,
                                  126, 129, 132, 135, 138, 141, 156, 159, 162, 165, 168, 171,
                                  186, 189, 192, 195, 198, 201, 216, 219, 222, 225, 228, 231,
                                  207, 66,  252, 48,  84,  96,  12,  14,  213, 99};
        int[] dset_indata      = new int[DIM_BLKS + DIM_PNTS];
        String dsetName        = "/Dataset1";
        String objName         = "Attribute1";

        _openH5File(H5_AREG_FILE, dsetName);

        try {
            H5.H5export_attribute(EXPORT_AREG_FILE, H5did, objName, 99);
        }
        catch (HDF5LibraryException err) {
            err.printStackTrace();
            fail("H5export_attribute failed: " + err);
        }

        File file = new File(EXPORT_AREG_FILE);

        try {
            Reader reader                   = new FileReader(EXPORT_AREG_FILE);
            StreamTokenizer streamTokenizer = new StreamTokenizer(reader);
            int indx                        = 0;
            int jndx                        = 0;
            while (streamTokenizer.nextToken() != StreamTokenizer.TT_EOF) {
                if (streamTokenizer.ttype == StreamTokenizer.TT_NUMBER) {
                    dset_indata[indx] = (int)streamTokenizer.nval;
                    indx++;
                }
            }
            reader.close();
        }
        catch (IOException err) {
            err.printStackTrace();
            fail("read file failed: " + err);
        }
        for (int row = 0; row < DIM_X; row++)
            assertTrue("testH5export_attrdataset: <" + row + ">" + dset_indata[row],
                       dset_indata[row] == dset_data_expect[row]);
    }

    private static ArrayList<Object> list(Object... items) { return new ArrayList<>(Arrays.asList(items)); }

    /**
     * Creates H5_TYPES_FILE holding one dataset of fileTid written from data through H5DwriteVL,
     * or, when attrName is given, an empty dataset carrying an attribute of fileTid written from data.
     */
    private final void _writeTypesFile(String dsetName, String attrName, long fileTid, long memTid,
                                       long[] dims, Object[] data) throws Exception
    {
        long fid = HDF5Constants.H5I_INVALID_HID, sid = HDF5Constants.H5I_INVALID_HID;
        long did = HDF5Constants.H5I_INVALID_HID, aid = HDF5Constants.H5I_INVALID_HID;
        try {
            fid = H5.H5Fcreate(H5_TYPES_FILE, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                               HDF5Constants.H5P_DEFAULT);
            sid = H5.H5Screate_simple(dims.length, dims, null);
            did =
                H5.H5Dcreate(fid, dsetName, (attrName == null) ? fileTid : HDF5Constants.H5T_NATIVE_INT, sid,
                             HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
            if (attrName == null)
                H5.H5DwriteVL(did, memTid, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                              HDF5Constants.H5P_DEFAULT, data);
            else {
                aid = H5.H5Acreate(did, attrName, fileTid, sid, HDF5Constants.H5P_DEFAULT,
                                   HDF5Constants.H5P_DEFAULT);
                H5.H5AwriteVL(aid, memTid, data);
            }
        }
        finally {
            if (aid >= 0)
                H5.H5Aclose(aid);
            if (did >= 0)
                H5.H5Dclose(did);
            if (sid >= 0)
                H5.H5Sclose(sid);
            if (fid >= 0)
                H5.H5Fclose(fid);
        }
    }

    /**
     * Exports a dataset of H5_TYPES_FILE, or its attribute attrName, as text and as native order
     * binary and compares both.
     */
    private final void _checkExport(String dsetName, String attrName, String expectedText,
                                    byte[] expectedBinary)
    {
        try {
            H5fid = H5.H5Fopen(H5_TYPES_FILE, HDF5Constants.H5F_ACC_RDONLY, HDF5Constants.H5P_DEFAULT);
            if (attrName == null) {
                H5.H5export_dataset(EXPORT_TYPES_TEXT, H5fid, dsetName, 99);
                H5.H5export_dataset(EXPORT_TYPES_BIN, H5fid, dsetName, 1);
            }
            else {
                H5did = H5.H5Dopen(H5fid, dsetName, HDF5Constants.H5P_DEFAULT);
                H5.H5export_attribute(EXPORT_TYPES_TEXT, H5did, attrName, 99);
                H5.H5export_attribute(EXPORT_TYPES_BIN, H5did, attrName, 1);
            }

            assertEquals(dsetName + " text", Arrays.asList(expectedText),
                         Files.readAllLines(Paths.get(EXPORT_TYPES_TEXT)));
            assertArrayEquals(dsetName + " binary", expectedBinary,
                              Files.readAllBytes(Paths.get(EXPORT_TYPES_BIN)));
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("H5export_dataset " + dsetName + ": " + err);
        }
        finally {
            _closeH5File();
            _deleteFile(H5_TYPES_FILE);
            _deleteFile(EXPORT_TYPES_TEXT);
            _deleteFile(EXPORT_TYPES_BIN);
        }
    }

    /**
     * Returns a buffer filled with the bytes a native order binary export writes
     * for the provided values.
     */
    private static byte[] binaryExportOf(Object... values)
    {
        ByteBuffer buf = ByteBuffer.allocate(1024).order(ByteOrder.nativeOrder());
        for (Object v : values) {
            if (v instanceof Integer)
                buf.putInt((Integer)v);
            else if (v instanceof Long)
                buf.putLong((Long)v);
            else if (v instanceof Double)
                buf.putDouble((Double)v);
            else if (v instanceof byte[])
                buf.put((byte[])v);
            else
                buf.put(((String)v).getBytes(StandardCharsets.US_ASCII));
        }
        return Arrays.copyOf(buf.array(), buf.position());
    }

    /** Several variable-length strings in one dataset. */
    @Test
    public void testH5export_dataset_strings() throws Exception
    {
        long str = H5.H5Tcopy(HDF5Constants.H5T_C_S1);
        try {
            H5.H5Tset_size(str, HDF5Constants.H5T_VARIABLE);
            _writeTypesFile("strings", null, str, str, new long[] {3},
                            new Object[] {"displacement", "b", "cc"});
        }
        finally {
            H5.H5Tclose(str);
        }
        _checkExport("/strings", null, "displacement, b, cc", binaryExportOf("displacement", "b", "cc"));
    }

    /** Several variable-length strings in one attribute. */
    @Test
    public void testH5export_attribute_strings() throws Exception
    {
        long str = H5.H5Tcopy(HDF5Constants.H5T_C_S1);
        try {
            H5.H5Tset_size(str, HDF5Constants.H5T_VARIABLE);
            _writeTypesFile("dset", "strings", str, str, new long[] {3},
                            new Object[] {"displacement", "b", "cc"});
        }
        finally {
            H5.H5Tclose(str);
        }
        _checkExport("/dset", "strings", "displacement, b, cc", binaryExportOf("displacement", "b", "cc"));
    }

    /** COMPOUND{id:int, names:VLEN of variable-length string}, with two strings in one sequence. */
    @Test
    public void testH5export_dataset_nested() throws Exception
    {
        long str = HDF5Constants.H5I_INVALID_HID, names = HDF5Constants.H5I_INVALID_HID;
        long cmpd = HDF5Constants.H5I_INVALID_HID;
        try {
            str = H5.H5Tcopy(HDF5Constants.H5T_C_S1);
            H5.H5Tset_size(str, HDF5Constants.H5T_VARIABLE);
            names = H5.H5Tvlen_create(str);
            cmpd  = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND, 8 + H5.H5Tget_size(names));
            H5.H5Tinsert(cmpd, "id", 0, HDF5Constants.H5T_NATIVE_INT32);
            H5.H5Tinsert(cmpd, "names", 8, names);

            _writeTypesFile("nested", null, cmpd, cmpd, new long[] {2},
                            new Object[] {list(1, list("displacement", "stress")), list(2, list("x"))});
        }
        finally {
            for (long tid : new long[] {cmpd, names, str})
                if (tid >= 0)
                    H5.H5Tclose(tid);
        }
        _checkExport("/nested", null, "{1, (displacement, stress)}, {2, (x)}",
                     binaryExportOf(1, "displacement", "stress", 2, "x"));
    }

    /**
     * COMPOUND{id:int, items:VLEN of COMPOUND{tag:variable-length string,
     * pt:COMPOUND{x:int, y:double}}}, with one NULL tag.
     */
    @Test
    public void testH5export_dataset_multi_nested() throws Exception
    {
        long str = HDF5Constants.H5I_INVALID_HID, pt = HDF5Constants.H5I_INVALID_HID;
        long item = HDF5Constants.H5I_INVALID_HID, items = HDF5Constants.H5I_INVALID_HID;
        long cmpd = HDF5Constants.H5I_INVALID_HID;
        try {
            str = H5.H5Tcopy(HDF5Constants.H5T_C_S1);
            H5.H5Tset_size(str, HDF5Constants.H5T_VARIABLE);
            pt = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND, 16);
            H5.H5Tinsert(pt, "x", 0, HDF5Constants.H5T_NATIVE_INT32);
            H5.H5Tinsert(pt, "y", 8, HDF5Constants.H5T_NATIVE_DOUBLE);
            long strSize = H5.H5Tget_size(str);
            item         = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND, strSize + 16);
            H5.H5Tinsert(item, "tag", 0, str);
            H5.H5Tinsert(item, "pt", strSize, pt);
            items = H5.H5Tvlen_create(item);
            cmpd  = H5.H5Tcreate(HDF5Constants.H5T_COMPOUND, 8 + H5.H5Tget_size(items));
            H5.H5Tinsert(cmpd, "id", 0, HDF5Constants.H5T_NATIVE_INT32);
            H5.H5Tinsert(cmpd, "items", 8, items);

            _writeTypesFile("multi_nested", null, cmpd, cmpd, new long[] {2},
                            new Object[] {list(1, list(list("a", list(2, 0.5)), list(null, list(3, 1.5)))),
                                          list(2, list(list("ccc", list(4, 2.5))))});
        }
        finally {
            for (long tid : new long[] {cmpd, items, item, pt, str})
                if (tid >= 0)
                    H5.H5Tclose(tid);
        }
        _checkExport("/multi_nested", null, "{1, ({a, {2, 0.5}}, {NULL, {3, 1.5}})}, {2, ({ccc, {4, 2.5}})}",
                     binaryExportOf(1, "a", 2, 0.5, 3, 1.5, 2, "ccc", 4, 2.5));
    }
}
