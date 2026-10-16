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

package hdf.hdf5lib;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;

import org.scijava.nativelib.JniExtractor;
import org.scijava.nativelib.NativeLibraryUtil;
import org.scijava.nativelib.NativeLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Loads bundled HDF5 shared libraries from the {@code hdf5-native}, {@code hdf5-zlib-native},
 * {@code hdf5-szip-native}, and {@code hdf5-jni-native} Maven JARs using SciJava
 * native-lib-loader ({@code natives/<platform>/} layout).
 * <p>
 * Only classpath native JARs are consulted, not {@code java.library.path} as
 * {@link org.scijava.nativelib.NativeLoader#loadLibrary(String, String...)} would do. A library
 * that is bundled but fails to load is logged as a warning with the reason.
 * <p>
 * No Panama/JNI types are referenced here, so this class can initialize before
 * jextract-generated classes (FFM) or {@code System.loadLibrary("hdf5_java")} (JNI).
 */
public final class Hdf5NativeLoader {
    /**
     * Set this system property to {@code true} to skip the bundled JARs and rely on a system
     * install instead: libhdf5 found by the OS loader ({@code LD_LIBRARY_PATH},
     * {@code DYLD_LIBRARY_PATH}, {@code PATH}) and, for JNI, hdf5_java on {@code java.library.path}.
     */
    public static final String SKIP_PROPERTY = "hdf.hdf5lib.Hdf5NativeLoader.skip";

    private static final Logger log = LoggerFactory.getLogger(Hdf5NativeLoader.class);

    private static final boolean IS_WINDOWS =
        System.getProperty("os.name", "").toLowerCase().startsWith("windows");

    /* Names used by Windows zlib builds: z.dll (zlib's CMake build), zlib.dll, zlib1.dll */
    private static final String[] ZLIB_NAMES =
        IS_WINDOWS ? new String[] {"z", "zlib", "zlib1"} : new String[] {"z"};
    /* libaec names its libsz compatibility library szip.dll for MSVC builds */
    private static final String[] SZ_NAMES = IS_WINDOWS ? new String[] {"szip", "sz"} : new String[] {"sz"};

    private static boolean attemptedZlib     = false;
    private static boolean attemptedSzip     = false;
    private static boolean attemptedHdf5     = false;
    private static boolean attemptedHdf5Java = false;

    private static volatile boolean loadedZlib     = false;
    private static volatile boolean loadedSzip     = false;
    private static volatile boolean loadedHdf5     = false;
    private static volatile boolean loadedHdf5Java = false;

    /* File the bundled libhdf5 was loaded from */
    private static volatile Path hdf5Path = null;

    private Hdf5NativeLoader() {}

    private static boolean skipBundledLoad()
    {
        return Boolean.parseBoolean(System.getProperty(SKIP_PROPERTY, "false"));
    }

    /**
     * Load the first of {@code names} found in the bundled native Maven JARs
     * ({@code natives/<platform>/} on the classpath).
     * <p>
     * Unlike {@link org.scijava.nativelib.NativeLoader#loadLibrary(String, String...)}, this does
     * not call {@code System.loadLibrary} first, so in-tree CI with {@code java.library.path} does
     * not preload build-tree libs before {@code hdf5_java}.
     *
     * @return the file that was loaded, or null if none of {@code names} is bundled or the bundled
     *         library could not be loaded
     */
    private static File loadFirstBundled(String... names)
    {
        if (NativeLibraryUtil.getArchitecture() == NativeLibraryUtil.Architecture.UNKNOWN) {
            log.debug("No bundled native libraries for this platform");
            return null;
        }
        JniExtractor extractor = NativeLoader.getJniExtractor();
        String libPath = NativeLibraryUtil.getPlatformLibraryPath(NativeLibraryUtil.DEFAULT_SEARCH_PATH);
        for (String name : names) {
            File lib;
            try {
                lib = extractor.extractJni(libPath, name);
            }
            catch (IOException err) {
                log.warn("Bundled native library {}{} could not be extracted", libPath,
                         System.mapLibraryName(name), err);
                return null;
            }
            if (lib == null)
                continue; // not bundled under this name
            try {
                System.load(lib.getAbsolutePath());
                log.debug("Loaded bundled native library {}", lib);
                return lib;
            }
            catch (UnsatisfiedLinkError err) {
                log.warn("Bundled native library {} could not be loaded: {}", lib, err.getMessage());
                return null;
            }
        }
        log.debug("No bundled native library {} under {}", String.join("/", names), libPath);
        return null;
    }

    /**
     * Loads bundled zlib when present. Safe to call multiple times.
     * Must be called before loading libhdf5 when libhdf5 links zlib dynamically.
     *
     * @return true if bundled zlib was loaded successfully
     */
    public static synchronized boolean loadBundledZlibIfPresent()
    {
        if (!attemptedZlib) {
            attemptedZlib = true;
            loadedZlib    = !skipBundledLoad() && loadFirstBundled(ZLIB_NAMES) != null;
        }
        return loadedZlib;
    }

    /** @return true if bundled zlib was loaded successfully this session */
    public static boolean bundledZlibLoadSucceeded() { return loadedZlib; }

    /**
     * Loads the bundled libaec and libsz pair when present. Safe to call multiple times.
     * Must be called before loading libhdf5 when libhdf5 links libaec dynamically. libaec is
     * loaded first because libsz, the szip-compatible API used by libhdf5, depends on it.
     *
     * @return true if both libaec and libsz were loaded successfully
     */
    public static synchronized boolean loadBundledSzipIfPresent()
    {
        if (!attemptedSzip) {
            attemptedSzip = true;
            loadedSzip =
                !skipBundledLoad() && loadFirstBundled("aec") != null && loadFirstBundled(SZ_NAMES) != null;
        }
        return loadedSzip;
    }

    /** @return true if bundled libaec and libsz were loaded successfully this session */
    public static boolean bundledSzipLoadSucceeded() { return loadedSzip; }

    /**
     * Loads the bundled HDF5 library when present. Safe to call multiple times.
     * Call {@link #loadBundledDependenciesBeforeHdf5()} instead so that bundled filter
     * libraries are loaded first.
     *
     * @return true if bundled hdf5 was loaded successfully
     */
    public static synchronized boolean loadBundledHdf5IfPresent()
    {
        if (!attemptedHdf5) {
            attemptedHdf5 = true;
            File lib      = skipBundledLoad() ? null : loadFirstBundled("hdf5");
            hdf5Path      = (lib == null) ? null : lib.toPath();
            loadedHdf5    = lib != null;
        }
        return loadedHdf5;
    }

    /** @return true if bundled hdf5 library was loaded successfully this session */
    public static boolean bundledHdf5LoadSucceeded() { return loadedHdf5; }

    /**
     * Returns the extracted path of the bundled HDF5 library, for FFM symbol lookup.
     *
     * @return the path, or null if the bundled HDF5 library was not loaded
     */
    public static Path bundledHdf5Path() { return hdf5Path; }

    /**
     * Loads the bundled hdf5_java JNI library when present. Safe to call multiple times.
     *
     * @return true if bundled hdf5_java was loaded successfully
     */
    public static synchronized boolean loadBundledHdf5JavaIfPresent()
    {
        if (!attemptedHdf5Java) {
            attemptedHdf5Java = true;
            loadedHdf5Java    = !skipBundledLoad() && loadFirstBundled("hdf5_java") != null;
        }
        return loadedHdf5Java;
    }

    /** @return true if bundled hdf5_java library was loaded successfully this session */
    public static boolean bundledHdf5JavaLoadSucceeded() { return loadedHdf5Java; }

    /**
     * Loads bundled filter dependencies and libhdf5 when present (zlib, then libaec/libsz, then
     * hdf5). Safe to call multiple times.
     */
    public static void loadBundledDependenciesBeforeHdf5()
    {
        loadBundledZlibIfPresent();
        loadBundledSzipIfPresent();
        loadBundledHdf5IfPresent();
    }
}
