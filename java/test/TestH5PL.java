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

public class TestH5PL {
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
    public void TestH5PLplugins() {
        try {
            int plugin_flags = H5.H5PLget_loading_state();
            assertTrue("H5.H5PLget_loading_state: "+plugin_flags, plugin_flags == HDF5Constants.H5PL_ALL_PLUGIN);
            int new_setting = plugin_flags & ~HDF5Constants.H5PL_FILTER_PLUGIN;
            H5.H5PLset_loading_state (new_setting);
            int changed_flags = H5.H5PLget_loading_state();
            assertTrue("H5.H5PLget_loading_state: "+changed_flags, changed_flags == new_setting);
            H5.H5PLset_loading_state (plugin_flags);
            changed_flags = H5.H5PLget_loading_state();
            assertTrue("H5.H5PLget_loading_state: "+changed_flags, changed_flags == HDF5Constants.H5PL_ALL_PLUGIN);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5PLplugins " + err);
        }
    }
}
