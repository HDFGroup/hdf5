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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5Tbasic {
    @Rule public TestName testname = new TestName();

    @Before
    public void checkOpenIDs() {
        assertTrue("H5 open ids is 0",H5.getOpenIDCount()==0);
        System.out.print(testname.getMethodName());
    }
    @After
    public void nextTestName() {
        System.out.println();
    }

    @Test
    public void testH5Tcopy() {
        long H5strdid = -1;
        try {
            H5strdid = H5.H5Tcopy(HDF5Constants.H5T_C_S1);
            assertTrue("H5.H5Tcopy",H5strdid > 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Tcopy: " + err);
        }
        finally {
            if (H5strdid >= 0)
                try {H5.H5Tclose(H5strdid);} catch (Exception ex) {}
        }
    }

    @Test
    public void testH5Tequal() {
        long H5strdid = -1;
        try {
            H5strdid = H5.H5Tcopy(HDF5Constants.H5T_C_S1);
            assertTrue("H5.H5Tcopy",H5strdid > 0);
            boolean teq = H5.H5Tequal(HDF5Constants.H5T_C_S1, H5strdid);
            assertTrue("H5.H5Tequal",teq);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Tequal: " + err);
        }
        finally {
            if (H5strdid >= 0)
                try {H5.H5Tclose(H5strdid);} catch (Exception ex) {}
        }
    }

    @Test
    public void testH5Tequal_not() {
        long H5strdid = -1;
        try {
            H5strdid = H5.H5Tcopy(HDF5Constants.H5T_STD_U64LE);
            assertTrue("H5.H5Tcopy",H5strdid > 0);
            boolean teq = H5.H5Tequal(HDF5Constants.H5T_IEEE_F32BE, H5strdid);
            assertFalse("H5.H5Tequal",teq);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Tequal_not: " + err);
        }
        finally {
            if (H5strdid >= 0)
                try {H5.H5Tclose(H5strdid);} catch (Exception ex) {}
        }
    }

    @Test
    public void testH5Tconvert() {
        String[] strs = {"a1234","b1234"};
        int srcLen = 5;
        int dstLen = 10;
        long srcId = -1;
        long dstId = -1;
        int dimSize = strs.length;
        byte[]   buf = new byte[dimSize*dstLen];

        for (int i=0; i<dimSize; i++)
            System.arraycopy(strs[i].getBytes(), 0, buf, i*srcLen, 5);

        try {
            srcId = H5.H5Tcopy(HDF5Constants.H5T_C_S1);
            H5.H5Tset_size(srcId, (long)srcLen);

            dstId = H5.H5Tcopy(HDF5Constants.H5T_C_S1);
            H5.H5Tset_size(dstId, (long)dstLen);

            H5.H5Tconvert(srcId, dstId, dimSize, buf, null, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Tconvert: " + err);
        }
        finally {
            try {H5.H5Tclose(srcId);} catch (Exception ex) {}
            try {H5.H5Tclose(dstId);} catch (Exception ex) {}
        }

        for (int i=0; i<strs.length; i++) {
            assertTrue((new String(buf, i*dstLen, dstLen)).startsWith(strs[i]));
        }
    }

    @Test
    public void testH5Torder_size() {
        long H5strdid = -1;
        try {
            // Fixed length string
            H5strdid = H5.H5Tcopy(HDF5Constants.H5T_C_S1);
            assertTrue("H5.H5Tcopy",H5strdid > 0);
            H5.H5Tset_size(H5strdid, (long)5);
            assertTrue(HDF5Constants.H5T_ORDER_NONE == H5.H5Tget_order(H5strdid));
            H5.H5Tset_order(H5strdid, HDF5Constants.H5T_ORDER_NONE);
            assertTrue(HDF5Constants.H5T_ORDER_NONE == H5.H5Tget_order(H5strdid));
            assertTrue(5 == H5.H5Tget_size(H5strdid));

            // Variable length string
            H5.H5Tset_size(H5strdid, HDF5Constants.H5T_VARIABLE);
            H5.H5Tset_order(H5strdid, HDF5Constants.H5T_ORDER_BE);
            assertTrue(HDF5Constants.H5T_ORDER_BE == H5.H5Tget_order(H5strdid));
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Torder: " + err);
        }
        finally {
            if (H5strdid >= 0)
                try {H5.H5Tclose(H5strdid);} catch (Exception ex) {}
        }
    }
}
