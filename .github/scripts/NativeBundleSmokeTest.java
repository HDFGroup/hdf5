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

import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.attribute.AclEntry;
import java.nio.file.attribute.AclFileAttributeView;
import java.nio.file.attribute.PosixFileAttributeView;
import java.nio.file.attribute.PosixFilePermissions;
import java.nio.file.attribute.UserPrincipal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.Hdf5NativeLoader;

/**
 * Checks that the native Maven JARs on the classpath are enough to run HDF5: every bundled library
 * is loaded from the JARs, the built-in filters are available, and a gzip-compressed chunked
 * dataset round-trips. Run by .github/scripts/test-native-bundles.sh.
 *
 * System properties (true/false): expect.zlib, expect.szip, expect.jni
 */
public class NativeBundleSmokeTest {
    private static final List<String> failures = new ArrayList<>();

    private static void check(boolean ok, String what)
    {
        System.out.println((ok ? "PASS " : "FAIL ") + what);
        if (!ok)
            failures.add(what);
    }

    private static boolean expected(String name) { return Boolean.getBoolean("expect." + name); }

    /* The directory the bundled libraries were copied into must be accessible to its owner only */
    private static void checkOwnerOnly(Path dir) throws Exception
    {
        PosixFileAttributeView posix =
            Files.getFileAttributeView(dir, PosixFileAttributeView.class, LinkOption.NOFOLLOW_LINKS);
        if (posix != null) {
            String perms = PosixFilePermissions.toString(posix.readAttributes().permissions());
            check(perms.equals("rwx------"), "extraction directory is owner-only (" + perms + ")");
            return;
        }
        AclFileAttributeView acl =
            Files.getFileAttributeView(dir, AclFileAttributeView.class, LinkOption.NOFOLLOW_LINKS);
        if (acl == null) {
            System.out.println("SKIP extraction directory permissions (no POSIX or ACL view)");
            return;
        }
        UserPrincipal owner = acl.getOwner();
        boolean ownerOnly   = true;
        for (AclEntry entry : acl.getAcl()) {
            if (!entry.principal().equals(owner)) {
                System.out.println("  ACL entry for another principal: " + entry);
                ownerOnly = false;
            }
        }
        check(ownerOnly,
              "extraction directory ACL grants access to its owner (" + owner.getName() + ") only");
    }

    public static void main(String[] args) throws Exception
    {
        H5.loadH5Lib();

        check(Hdf5NativeLoader.bundledHdf5LoadSucceeded(), "libhdf5 loaded from hdf5-native");
        if (Hdf5NativeLoader.bundledHdf5Path() != null)
            checkOwnerOnly(Hdf5NativeLoader.bundledHdf5Path().getParent());
        if (expected("zlib"))
            check(Hdf5NativeLoader.bundledZlibLoadSucceeded(),
                  "bundled zlib loaded (libhdf5 may use a zlib loaded earlier)");
        if (expected("szip"))
            check(Hdf5NativeLoader.bundledSzipLoadSucceeded(), "libaec/libsz loaded from hdf5-native");
        if (expected("jni"))
            check(Hdf5NativeLoader.bundledHdf5JavaLoadSucceeded(), "hdf5_java loaded from hdf5-jni-native");

        if (expected("zlib"))
            check(H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_DEFLATE) > 0, "deflate filter available");
        if (expected("szip"))
            check(H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_SZIP) > 0, "szip filter available");

        if (expected("zlib")) {
            String file     = "native_bundle_smoke.h5";
            long[] dims     = {8, 12};
            int[] writeData = new int[8 * 12];
            for (int i = 0; i < writeData.length; i++)
                writeData[i] = i;
            int[] readData = new int[writeData.length];

            long fid  = H5.H5Fcreate(file, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                                     HDF5Constants.H5P_DEFAULT);
            long sid  = H5.H5Screate_simple(2, dims, null);
            long dcpl = H5.H5Pcreate(HDF5Constants.H5P_DATASET_CREATE);
            H5.H5Pset_chunk(dcpl, 2, new long[] {4, 6});
            H5.H5Pset_deflate(dcpl, 6);
            long did = H5.H5Dcreate(fid, "gzip", HDF5Constants.H5T_NATIVE_INT, sid, HDF5Constants.H5P_DEFAULT,
                                    dcpl, HDF5Constants.H5P_DEFAULT);
            H5.H5Dwrite(did, HDF5Constants.H5T_NATIVE_INT, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                        HDF5Constants.H5P_DEFAULT, writeData);
            H5.H5Dclose(did);
            H5.H5Pclose(dcpl);
            H5.H5Sclose(sid);
            H5.H5Fclose(fid);

            fid = H5.H5Fopen(file, HDF5Constants.H5F_ACC_RDONLY, HDF5Constants.H5P_DEFAULT);
            did = H5.H5Dopen(fid, "gzip", HDF5Constants.H5P_DEFAULT);
            H5.H5Dread(did, HDF5Constants.H5T_NATIVE_INT, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                       HDF5Constants.H5P_DEFAULT, readData);
            H5.H5Dclose(did);
            H5.H5Fclose(fid);
            check(Arrays.equals(writeData, readData), "gzip chunked dataset round-trip");
        }

        if (!failures.isEmpty()) {
            System.out.println(failures.size() + " check(s) failed");
            System.exit(1);
        }
        System.out.println("All native bundle checks passed");
    }
}
