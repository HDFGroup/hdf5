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
import hdf.hdf5lib.H5;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5Eregister {
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

    @Test(expected = NullPointerException.class)
    public void testH5Eregister_class_cls_name_null() throws Throwable {
        H5.H5Eregister_class(null, "libname", "version");
    }

    @Test(expected = NullPointerException.class)
    public void testH5Eregister_class_lib_name_null() throws Throwable {
        H5.H5Eregister_class("clsname", null, "version");
    }

    @Test(expected = NullPointerException.class)
    public void testH5Eregister_class_version_null() throws Throwable {
        H5.H5Eregister_class("clsname", "libname", null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Eunregister_class_invalid_classid() throws Throwable {
        H5.H5Eunregister_class(-1);
    }

    @Test
    public void testH5Eregister_class() {
        long hdf_java_classid = -1;
        try {
            hdf_java_classid = H5.H5Eregister_class("HDF-Java-Error", "hdf-java", "2.5");
        }
        catch (Throwable err) {
            fail("H5.H5Eregister_class: " + err);
        }
        try {
            H5.H5Eunregister_class(hdf_java_classid);
        }
        catch (Throwable err) {
            fail("H5.H5Eunregister_class: " + err);
        }
    }
}
