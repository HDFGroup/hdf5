package hdf.hdf5lib;

import org.scijava.nativelib.NativeLibraryUtil;
import org.scijava.nativelib.NativeLoader;

/**
 * Loads bundled HDF5 shared libraries from the {@code hdf5-native}, {@code hdf5-zlib-native},
 * {@code hdf5-szip-native}, and {@code hdf5-jni-native} Maven JARs using SciJava
 * native-lib-loader ({@code natives/&lt;platform&gt;/} layout).
 * <p>
 * Extraction uses {@link org.scijava.nativelib.NativeLibraryUtil#loadNativeLibrary}, so only
 * classpath native JARs are consulted, not {@code java.library.path} as
 * {@link org.scijava.nativelib.NativeLoader#loadLibrary(String, String...)} would do.
 * <p>
 * No Panama/JNI types are referenced here, so this class can initialize before
 * jextract-generated classes (FFM) or {@code System.loadLibrary("hdf5_java")} (JNI).
 */
public final class Hdf5NativeLoader {
    /**
     * Skip loading from bundled JARs (use a system install / {@code java.library.path} only). Kept identical
     * to the former {@code NativeLibraryBootstrap} property name for backward compatibility.
     */
    public static final String SKIP_PROPERTY = "hdf.hdf5lib.NativeLibraryBootstrap.skip";

    private static volatile boolean attemptedHdf5     = false;
    private static volatile boolean attemptedHdf5Java = false;
    private static volatile boolean attemptedZlib     = false;
    private static volatile boolean attemptedSzip     = false;
    private static volatile boolean loadedHdf5        = false;
    private static volatile boolean loadedHdf5Java    = false;
    private static volatile boolean loadedZlib        = false;
    private static volatile boolean loadedSzip        = false;

    private Hdf5NativeLoader() {}

    private static boolean skipBundledLoad()
    {
        return "true".equalsIgnoreCase(System.getProperty(SKIP_PROPERTY, ""));
    }

    /**
     * Load {@code libName} only from bundled native Maven JARs ({@code natives/&lt;platform&gt;/}
     * on the classpath).
     * <p>
     * Unlike {@link org.scijava.nativelib.NativeLoader#loadLibrary(String, String...)}, this does
     * not call {@code System.loadLibrary} first, so in-tree CI with {@code java.library.path} does
     * not preload build-tree libs before {@code hdf5_java}.
     */
    private static boolean loadBundledFromClasspathJars(String libName)
    {
        return NativeLibraryUtil.loadNativeLibrary(NativeLoader.getJniExtractor(), libName);
    }

    /**
     * Loads bundled zlib when present. Safe to call multiple times.
     * This should be called before loading libhdf5 when libhdf5 was built with dynamic zlib support.
     *
     * @return true if bundled zlib was loaded successfully
     */
    public static synchronized boolean loadBundledZlibIfPresent()
    {
        if (attemptedZlib)
            return loadedZlib;
        attemptedZlib = true;

        if (skipBundledLoad())
            return false;

        // SciJava maps "z" -> libz.so / z.dll. Some Windows zlib builds export zlib.dll or
        // zlib1.dll instead; try those names so libhdf5's PE import table resolves before load.
        final String[] zlibCandidates;
        if (System.getProperty("os.name", "").toLowerCase().contains("win")) {
            zlibCandidates = new String[] {"zlib1", "zlib", "z"};
        }
        else {
            zlibCandidates = new String[] {"z"};
        }

        boolean any = false;
        for (String zlibName : zlibCandidates) {
            if (loadBundledFromClasspathJars(zlibName)) {
                any = true;
            }
        }
        loadedZlib = any;
        return any;
    }

    /** @return true if bundled zlib was loaded successfully this session */
    public static boolean bundledZlibLoadSucceeded() { return loadedZlib; }

    /**
     * Loads the bundled libaec/szip pair when present. Safe to call multiple times.
     * This should be called before loading libhdf5 when libhdf5 was built with dynamic
     * SZIP (libaec) support. libaec is loaded first because libsz depends on it; libsz
     * exposes the szip-compatible API used by libhdf5's SZIP filter.
     *
     * @return true if at least the szip-compatible library was loaded successfully
     */
    public static synchronized boolean loadBundledSzipIfPresent()
    {
        if (attemptedSzip)
            return loadedSzip;
        attemptedSzip = true;

        if (skipBundledLoad())
            return false;

        boolean any = false;
        for (String name : new String[] {"aec", "sz", "szip"}) {
            if (loadBundledFromClasspathJars(name)) {
                any = true;
            }
        }

        loadedSzip = any;
        return any;
    }

    /** @return true if bundled szip (libaec) was loaded successfully this session */
    public static boolean bundledSzipLoadSucceeded() { return loadedSzip; }

    /**
     * Loads the bundled HDF5 library when present. Safe to call multiple times.
     * This should be called early before attempting to load hdf5_java.
     *
     * @return true if bundled hdf5 was loaded successfully
     */
    public static synchronized boolean loadBundledHdf5IfPresent()
    {
        if (attemptedHdf5)
            return loadedHdf5;
        attemptedHdf5 = true;

        if (skipBundledLoad())
            return false;

        loadedHdf5 = loadBundledFromClasspathJars("hdf5");
        return loadedHdf5;
    }

    /** @return true if bundled hdf5 library was loaded successfully this session */
    public static boolean bundledHdf5LoadSucceeded() { return loadedHdf5; }

    /**
     * Loads the bundled hdf5_java JNI library when present. Safe to call multiple times.
     * This should be called as a last resort fallback after attempting to load hdf5_java
     * from system paths.
     *
     * @return true if bundled hdf5_java was loaded successfully
     */
    public static synchronized boolean loadBundledHdf5JavaIfPresent()
    {
        if (attemptedHdf5Java)
            return loadedHdf5Java;
        attemptedHdf5Java = true;

        if (skipBundledLoad())
            return false;

        loadedHdf5Java = loadBundledFromClasspathJars("hdf5_java");
        return loadedHdf5Java;
    }

    /** @return true if bundled hdf5_java library was loaded successfully this session */
    public static boolean bundledHdf5JavaLoadSucceeded() { return loadedHdf5Java; }

    /**
     * Loads bundled filter dependencies and libhdf5 when present (zlib, then libaec/szip, then hdf5).
     * Safe to call multiple times.
     */
    public static void loadBundledDependenciesBeforeHdf5()
    {
        loadBundledZlibIfPresent();
        loadBundledSzipIfPresent();
        loadBundledHdf5IfPresent();
    }
}
