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

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.attribute.UserPrincipal;
import java.util.Locale;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Loads bundled HDF5 shared libraries from the {@code hdf5-native} and {@code hdf5-jni-native}
 * Maven JARs, which store them under {@code natives/<platform>/} (for example
 * {@code natives/linux_64/libhdf5.so}).
 * <p>
 * Each library is copied into a new temporary directory created for this JVM and loaded with
 * {@code System.load}; {@code java.library.path} is not consulted. A library that is bundled but
 * fails to load is logged as a warning with the reason.
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

    private static final String OS_NAME     = System.getProperty("os.name", "").toLowerCase(Locale.ROOT);
    private static final boolean IS_WINDOWS = OS_NAME.startsWith("windows");

    /* Prefix of the per-JVM directory the bundled libraries are copied into */
    private static final String EXTRACT_DIR_PREFIX = "hdf5-native-";

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

    /* SKIP_PROPERTY as read on first use, so all libraries follow the same setting */
    private static Boolean skipBundled = null;

    /* Directory the bundled libraries of this JVM are copied into, created on first use */
    private static Path extractDir = null;

    private Hdf5NativeLoader() {}

    private static synchronized boolean skipBundledLoad()
    {
        if (skipBundled == null)
            skipBundled = Boolean.parseBoolean(System.getProperty(SKIP_PROPERTY, "false"));
        return skipBundled;
    }

    /**
     * Name of the {@code natives/} subdirectory for this platform, matching the directories that
     * HDF5JavaNativeBundles.cmake stages libraries into.
     *
     * @return the directory name, or null for an unsupported platform
     */
    private static String platformDirectory()
    {
        String os;
        if (IS_WINDOWS)
            os = "windows";
        else if (OS_NAME.startsWith("mac"))
            os = "osx";
        else if (OS_NAME.startsWith("linux"))
            os = "linux";
        else
            return null;

        switch (System.getProperty("os.arch", "").toLowerCase(Locale.ROOT)) {
        case "amd64":
        case "x86_64":
            return os + "_64";
        case "aarch64":
        case "arm64":
            return os + "_arm64";
        case "x86":
        case "i386":
        case "i486":
        case "i586":
        case "i686":
            return os + "_32";
        default:
            return null;
        }
    }

    /* All bundled libraries go into one directory, so a library finds its dependencies next to it */
    private static synchronized Path extractionDirectory() throws IOException
    {
        if (extractDir == null) {
            extractDir = Files.createTempDirectory(EXTRACT_DIR_PREFIX);
            extractDir.toFile().deleteOnExit();
            if (IS_WINDOWS)
                removeEarlierExtractionDirectories(extractDir);
        }
        return extractDir;
    }

    /*
     * Windows cannot delete a loaded DLL when the JVM exits, so earlier runs leave their extraction
     * directories behind in the per-user temporary directory. Remove those that have the same
     * owner as this JVM's directory {@code current}, deleting only the regular files directly in
     * them; files still in use by a running JVM cannot be deleted and are left alone.
     */
    private static void removeEarlierExtractionDirectories(Path current)
    {
        try (DirectoryStream<Path> dirs =
                 Files.newDirectoryStream(current.getParent(), EXTRACT_DIR_PREFIX + "*")) {
            UserPrincipal self = Files.getOwner(current, LinkOption.NOFOLLOW_LINKS);
            for (Path dir : dirs) {
                if (dir.equals(current) || !Files.isDirectory(dir, LinkOption.NOFOLLOW_LINKS))
                    continue;
                if (!self.equals(Files.getOwner(dir, LinkOption.NOFOLLOW_LINKS)))
                    continue;
                try (DirectoryStream<Path> files = Files.newDirectoryStream(dir)) {
                    for (Path file : files) {
                        if (Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS))
                            Files.deleteIfExists(file);
                    }
                    Files.deleteIfExists(dir);
                }
                catch (IOException err) {
                    log.debug("Could not remove {}: {}", dir, err.getMessage());
                }
            }
        }
        catch (IOException | SecurityException err) {
            log.debug("Could not look for earlier extraction directories: {}", err.getMessage());
        }
    }

    /**
     * Load the first of {@code names} found in the bundled native Maven JARs
     * ({@code natives/<platform>/} on the classpath).
     * <p>
     * {@code System.loadLibrary} is not tried first, so in-tree CI with {@code java.library.path}
     * does not preload build-tree libs before {@code hdf5_java}.
     *
     * @return the file that was loaded, or null if none of {@code names} is bundled or the bundled
     *         library could not be loaded
     */
    private static Path loadFirstBundled(String... names)
    {
        String platform = platformDirectory();
        if (platform == null) {
            log.debug("No bundled native libraries for this platform");
            return null;
        }
        ClassLoader loader = Hdf5NativeLoader.class.getClassLoader();
        for (String name : names) {
            String mapped   = System.mapLibraryName(name);
            String resource = "natives/" + platform + "/" + mapped;
            URL url =
                (loader != null) ? loader.getResource(resource) : ClassLoader.getSystemResource(resource);
            if (url == null)
                continue; // not bundled under this name

            Path lib;
            try (InputStream in = url.openStream()) {
                lib = extractionDirectory().resolve(mapped);
                Files.copy(in, lib);
                lib.toFile().deleteOnExit();
            }
            catch (IOException | SecurityException err) {
                log.warn("Bundled native library {} could not be extracted", resource, err);
                return null;
            }
            try {
                System.load(lib.toAbsolutePath().toString());
                log.debug("Loaded bundled native library {}", lib);
                return lib;
            }
            catch (UnsatisfiedLinkError err) {
                log.warn("Bundled native library {} could not be loaded: {}", lib, err.getMessage());
                return null;
            }
        }
        log.debug("No bundled native library {} under natives/{}/", String.join("/", names), platform);
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
            hdf5Path      = skipBundledLoad() ? null : loadFirstBundled("hdf5");
            loadedHdf5    = hdf5Path != null;
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
    public static synchronized void loadBundledDependenciesBeforeHdf5()
    {
        loadBundledZlibIfPresent();
        loadBundledSzipIfPresent();
        loadBundledHdf5IfPresent();
    }
}
