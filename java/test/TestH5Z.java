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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.exceptions.HDF5LibraryException;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5Z {
    @Rule
    public TestName testname = new TestName();

    @Before
    public void checkOpenIDs()
    {
        assertTrue("H5 open ids is 0", H5.getOpenIDCount() == 0);
        System.out.print(testname.getMethodName());
    }
    @After
    public void nextTestName()
    {
        System.out.println();
    }

    @Test
    public void testH5Zfilter_avail()
    {
        try {
            int filter_found;

            filter_found = H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_FLETCHER32);
            assertTrue("H5.H5Zfilter_avail_FLETCHER32", filter_found > 0);
            filter_found = H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_NBIT);
            assertTrue("H5.H5Zfilter_avail_NBIT", filter_found > 0);
            filter_found = H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_SCALEOFFSET);
            assertTrue("H5.H5Zfilter_avail_SCALEOFFSET", filter_found > 0);
            filter_found = H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_SHUFFLE);
            assertTrue("H5.H5Zfilter_avail_SHUFFLE", filter_found > 0);

            // Just make sure H5Zfilter_avail() doesn't fail with szip/zlib
            // since there is no way for us to determine if they should be present
            // or not.
            filter_found = H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_DEFLATE);
            filter_found = H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_SZIP);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Zfilter_avail " + err);
        }
    }

    @Test
    public void testH5Zget_filter_info()
    {
        try {
            int filter_flag;

            filter_flag = H5.H5Zget_filter_info(HDF5Constants.H5Z_FILTER_FLETCHER32);
            assertTrue("H5.H5Zget_filter_info_FLETCHER32_DECODE_ENABLED",
                       (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_DECODE_ENABLED) > 0);
            assertTrue("H5.H5Zget_filter_info_FLETCHER32_ENCODE_ENABLED",
                       (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_ENCODE_ENABLED) > 0);
            filter_flag = H5.H5Zget_filter_info(HDF5Constants.H5Z_FILTER_NBIT);
            assertTrue("H5.H5Zget_filter_info_NBIT_DECODE_ENABLED",
                       (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_DECODE_ENABLED) > 0);
            assertTrue("H5.H5Zget_filter_info_NBIT_ENCODE_ENABLED",
                       (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_ENCODE_ENABLED) > 0);
            filter_flag = H5.H5Zget_filter_info(HDF5Constants.H5Z_FILTER_SCALEOFFSET);
            assertTrue("H5.H5Zget_filter_info_SCALEOFFSET_DECODE_ENABLED",
                       (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_DECODE_ENABLED) > 0);
            assertTrue("H5.H5Zget_filter_info_SCALEOFFSET_ENCODE_ENABLED",
                       (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_ENCODE_ENABLED) > 0);
            filter_flag = H5.H5Zget_filter_info(HDF5Constants.H5Z_FILTER_SHUFFLE);
            assertTrue("H5.H5Zget_filter_info_DECODE_SHUFFLE_ENABLED",
                       (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_DECODE_ENABLED) > 0);
            assertTrue("H5.H5Zget_filter_info_ENCODE_SHUFFLE_ENABLED",
                       (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_ENCODE_ENABLED) > 0);

            if (1 == H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_DEFLATE)) {
                filter_flag = H5.H5Zget_filter_info(HDF5Constants.H5Z_FILTER_DEFLATE);
                assertTrue("H5.H5Zget_filter_info_DEFLATE_DECODE_ENABLED",
                           (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_DECODE_ENABLED) > 0);
                assertTrue("H5.H5Zget_filter_info_DEFLATE_ENCODE_ENABLED",
                           (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_ENCODE_ENABLED) > 0);
            }

            if (1 == H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_SZIP)) {
                filter_flag = H5.H5Zget_filter_info(HDF5Constants.H5Z_FILTER_SZIP);
                // Decode should always be available, but we have no way of determining
                // if encode is so don't assert on that.
                assertTrue("H5.H5Zget_filter_info_DECODE_SZIP_ENABLED",
                           (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_DECODE_ENABLED) > 0);
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Zget_filter_info " + err);
        }
    }

    @Test
    public void testH5Zget_filter_info2()
    {
        try {
            hdf.hdf5lib.structs.H5Z_class_info_t info;

            // DEFLATE (if available)
            if (1 == H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_DEFLATE)) {
                info = H5.H5Zget_filter_info2(HDF5Constants.H5Z_FILTER_DEFLATE);
                assertTrue("H5Zget_filter_info2: info must not be null", info != null);
                assertTrue("H5Zget_filter_info2: DEFLATE id", info.id == HDF5Constants.H5Z_FILTER_DEFLATE);
                assertTrue("H5Zget_filter_info2: DEFLATE decode flag",
                           (info.config_flags & HDF5Constants.H5Z_FILTER_CONFIG_DECODE_ENABLED) != 0);
            }

            // SHUFFLE
            info = H5.H5Zget_filter_info2(HDF5Constants.H5Z_FILTER_SHUFFLE);
            assertTrue("H5Zget_filter_info2: SHUFFLE info must not be null", info != null);
            assertTrue("H5Zget_filter_info2: SHUFFLE id", info.id == HDF5Constants.H5Z_FILTER_SHUFFLE);
            assertTrue("H5Zget_filter_info2: SHUFFLE decode flag",
                       (info.config_flags & HDF5Constants.H5Z_FILTER_CONFIG_DECODE_ENABLED) != 0);

            // FLETCHER32
            info = H5.H5Zget_filter_info2(HDF5Constants.H5Z_FILTER_FLETCHER32);
            assertTrue("H5Zget_filter_info2: FLETCHER32 info must not be null", info != null);
            assertTrue("H5Zget_filter_info2: FLETCHER32 id", info.id == HDF5Constants.H5Z_FILTER_FLETCHER32);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Zget_filter_info2 " + err);
        }
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Zget_filter_info2_invalid() throws Throwable
    {
        H5.H5Zget_filter_info2(32999);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Zunregister_predefined() throws Throwable
    {
        int filter_found = H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_SHUFFLE);
        assertTrue("H5.H5Zfilter_avail", filter_found > 0);

        H5.H5Zunregister(HDF5Constants.H5Z_FILTER_SHUFFLE);
    }

    @Test
    public void testH5Pappend_filter_string()
    {
        long dcpl_id = HDF5Constants.H5I_INVALID_HID;
        try {
            dcpl_id = H5.H5Pcreate(HDF5Constants.H5P_DATASET_CREATE);
            assertTrue("H5Pcreate", dcpl_id >= 0);

            int ret = H5.H5Pappend_filter(dcpl_id, HDF5Constants.H5Z_FILTER_SHUFFLE, 0, "");
            assertTrue("H5Pappend_filter (string, shuffle)", ret >= 0);

            int nfilters = H5.H5Pget_nfilters(dcpl_id);
            assertEquals("nfilters after H5Pappend_filter", 1, nfilters);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Pappend_filter_string: " + err);
        }
        finally {
            if (dcpl_id != HDF5Constants.H5I_INVALID_HID)
                try { H5.H5Pclose(dcpl_id); } catch (Exception e) { /* ignore */ }
        }
    }

    @Test
    public void testH5Pappend_filter_cdvalues()
    {
        long dcpl_id = HDF5Constants.H5I_INVALID_HID;
        try {
            dcpl_id = H5.H5Pcreate(HDF5Constants.H5P_DATASET_CREATE);
            assertTrue("H5Pcreate", dcpl_id >= 0);

            int[] cd_values = new int[] {9};
            if (1 == H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_DEFLATE)) {
                int ret = H5.H5Pappend_filter(dcpl_id, HDF5Constants.H5Z_FILTER_DEFLATE, 0, cd_values);
                assertTrue("H5Pappend_filter (cd_values, deflate)", ret >= 0);

                int nfilters = H5.H5Pget_nfilters(dcpl_id);
                assertEquals("nfilters after H5Pappend_filter", 1, nfilters);

                // Verify the cd_values round-trip via H5Pget_filter2.
                int[] cd_out  = new int[1];
                int[] flags_out  = new int[1];
                long[] cd_nelmts = new long[] {1};
                String[] name_out = new String[] {""};
                int filter_id = H5.H5Pget_filter2(dcpl_id, 0, flags_out, cd_nelmts, cd_out, 256, name_out, null);
                assertEquals("filter id from H5Pget_filter2", HDF5Constants.H5Z_FILTER_DEFLATE, filter_id);
                assertEquals("cd_nelmts", 1L, cd_nelmts[0]);
                assertEquals("cd_values[0] (deflate level)", 9, cd_out[0]);
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Pappend_filter_cdvalues: " + err);
        }
        finally {
            if (dcpl_id != HDF5Constants.H5I_INVALID_HID)
                try { H5.H5Pclose(dcpl_id); } catch (Exception e) { /* ignore */ }
        }
    }

    @Test
    public void testH5Pget_filter_params_by_idx()
    {
        long dcpl_id = HDF5Constants.H5I_INVALID_HID;
        try {
            dcpl_id = H5.H5Pcreate(HDF5Constants.H5P_DATASET_CREATE);
            assertTrue("H5Pcreate", dcpl_id >= 0);

            int ret = H5.H5Pappend_filter(dcpl_id, HDF5Constants.H5Z_FILTER_SHUFFLE, 0, "");
            assertTrue("H5Pappend_filter shuffle", ret >= 0);

            String[] params = new String[1];
            int retval = H5.H5Pget_filter_params_by_idx(dcpl_id, 0, params);
            assertTrue("H5Pget_filter_params_by_idx returned non-negative", retval >= 0);
            assertNotNull("params[0] is non-null", params[0]);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Pget_filter_params_by_idx: " + err);
        }
        finally {
            if (dcpl_id != HDF5Constants.H5I_INVALID_HID)
                try { H5.H5Pclose(dcpl_id); } catch (Exception e) { /* ignore */ }
        }
    }

    @Test
    public void testH5Zconfig_get_param_int()
    {
        try {
            String cfg = "level = 6";
            long[] val = new long[1];
            int ret = H5.H5Zconfig_get_int(cfg, "level", val);
            assertTrue("H5Zconfig_get_int returned non-negative", ret >= 0);
            assertEquals("H5Zconfig_get_int value", 6L, val[0]);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Zconfig_get_param_int: " + err);
        }
    }

    @Test
    public void testH5Zconfig_get_param_double()
    {
        try {
            String cfg = "scale = 1.5";
            double[] val = new double[1];
            int ret = H5.H5Zconfig_get_double(cfg, "scale", val);
            assertTrue("H5Zconfig_get_double returned non-negative", ret >= 0);
            assertEquals("H5Zconfig_get_double value", 1.5, val[0], 1e-10);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Zconfig_get_param_double: " + err);
        }
    }

    @Test
    public void testH5Zconfig_get_param_bool()
    {
        try {
            String cfg = "lossless = true";
            boolean[] val = new boolean[1];
            int ret = H5.H5Zconfig_get_bool(cfg, "lossless", val);
            assertTrue("H5Zconfig_get_bool returned non-negative", ret >= 0);
            assertTrue("H5Zconfig_get_bool value", val[0]);

            String cfg2 = "lossless = false";
            int ret2 = H5.H5Zconfig_get_bool(cfg2, "lossless", val);
            assertTrue("H5Zconfig_get_bool (false) returned non-negative", ret2 >= 0);
            assertFalse("H5Zconfig_get_bool value (false)", val[0]);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Zconfig_get_param_bool: " + err);
        }
    }

    @Test
    public void testH5Zconfig_get_param_str()
    {
        try {
            String cfg = "mode = \"fast\"";
            String[] val = new String[1];
            int ret = H5.H5Zconfig_get_str(cfg, "mode", val);
            assertTrue("H5Zconfig_get_str returned non-negative", ret >= 0);
            assertEquals("H5Zconfig_get_str value", "fast", val[0]);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Zconfig_get_param_str: " + err);
        }
    }

    @Test
    public void testH5Zconfig_get_param_missing_key()
    {
        try {
            String cfg = "level = 5";
            long[] val = new long[1];
            int ret = H5.H5Zconfig_get_int(cfg, "missing_key", val);
            assertTrue("H5Zconfig_get_int with missing key returns 0 (not found)", ret == 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Zconfig_get_param_missing_key: " + err);
        }
    }
}
