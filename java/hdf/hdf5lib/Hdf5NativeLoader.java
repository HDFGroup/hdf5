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
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.AclEntry;
import java.nio.file.attribute.AclEntryFlag;
import java.nio.file.attribute.AclEntryPermission;
import java.nio.file.attribute.AclEntryType;
import java.nio.file.attribute.AclFileAttributeView;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.UserPrincipal;
import java.nio.file.attribute.UserPrincipalLookupService;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.helpers.MessageFormatter;
import org.slf4j.helpers.NOPLoggerFactory;

/**
 * Loads bundled HDF5 shared libraries from the {@code hdf5-native} and {@code hdf5-jni-native}
 * Maven JARs, which store them under {@code natives/hdf5/<platform>/} (for example
 * {@code natives/hdf5/linux_64/libhdf5.so}). The {@code hdf5} level keeps them apart from other
 * projects' native JARs that use the common {@code natives/<platform>/} layout.
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

    /* Classpath directory holding the bundled libraries, one subdirectory per platform */
    private static final String RESOURCE_ROOT = "natives/hdf5/";

    /* Prefix of the per-JVM directory the bundled libraries are copied into */
    private static final String EXTRACT_DIR_PREFIX = "hdf5-native-";

    /* Earlier extraction directories modified more recently than this may belong to a starting JVM */
    private static final long EARLIER_DIR_MIN_AGE_MS = 10 * 60 * 1000;

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

    /*
     * Report a bundled library that could not be extracted or loaded. The HDF5 POMs depend only on
     * slf4j-api, and without an SLF4J provider the warning would be dropped, so print it to
     * standard error instead.
     */
    private static void warn(String format, Object... args)
    {
        if (LoggerFactory.getILoggerFactory() instanceof NOPLoggerFactory)
            System.err.println("HDF5: " + MessageFormatter.arrayFormat(format, args).getMessage());
        else
            log.warn(format, args);
    }

    /**
     * Name of the {@code natives/hdf5/} subdirectory for this platform, matching the directories that
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
            Path parent = Paths.get(System.getProperty("java.io.tmpdir"));
            Path dir;
            if (IS_WINDOWS) {
                UserPrincipal self = currentWindowsUser();
                dir = Files.createTempDirectory(parent, EXTRACT_DIR_PREFIX, ownerOnlyAcl(self));
                dir.toFile().deleteOnExit();
                try {
                    checkOwnerOnlyAcl(dir, self);
                }
                catch (IOException | RuntimeException err) {
                    Files.deleteIfExists(dir);
                    throw err;
                }
                if (self.equals(Files.getOwner(parent)))
                    removeEarlierExtractionDirectories(dir);
            }
            else {
                checkPosixParent(parent);
                // On POSIX file systems the new directory is accessible only to its owner
                dir = Files.createTempDirectory(parent, EXTRACT_DIR_PREFIX);
                dir.toFile().deleteOnExit();
            }
            extractDir = dir;
        }
        return extractDir;
    }

    /*
     * A user who can rename entries in the temporary directory could replace the extraction
     * directory between copying a library and loading it. Accept only a directory owned by the
     * current user or root that other users cannot write to, or that has the sticky bit set.
     * Group write access is allowed when the group is the owner's own private group (same name as
     * the owner), which is how directories are created under umask 0002 on many Linux systems.
     */
    private static void checkPosixParent(Path parent) throws IOException
    {
        Map<String, Object> attrs;
        try {
            attrs = Files.readAttributes(parent, "unix:uid,mode,group");
        }
        catch (UnsupportedOperationException | IllegalArgumentException err) {
            return; // no POSIX ownership and mode to check
        }
        int uid             = (Integer)attrs.get("uid");
        int mode            = (Integer)attrs.get("mode");
        String owner        = Files.getOwner(parent).getName();
        String group        = ((UserPrincipal)attrs.get("group")).getName();
        boolean ownedBySelf = uid == 0 || owner.equals(System.getProperty("user.name"));
        boolean sticky      = (mode & 01000) != 0;
        boolean worldWrite  = (mode & 0002) != 0;
        boolean groupWrite  = (mode & 0020) != 0 && !(uid != 0 && group.equals(owner));
        if (!ownedBySelf || (!sticky && (worldWrite || groupWrite)))
            throw new IOException("temporary directory " + parent + " (owner " + owner + ", group " + group +
                                  ", mode " + Integer.toOctalString(mode & 07777) +
                                  ") can be modified by other users; set java.io.tmpdir to a "
                                  + "directory that you own and others cannot write to (chmod go-w)");
    }

    /* The Windows account this JVM runs as */
    private static UserPrincipal currentWindowsUser() throws IOException
    {
        UserPrincipalLookupService lookup = FileSystems.getDefault().getUserPrincipalLookupService();
        String user                       = System.getProperty("user.name");
        String domain                     = System.getenv("USERDOMAIN");
        if (domain != null && !domain.isEmpty()) {
            try {
                return lookup.lookupPrincipalByName(domain + "\\" + user);
            }
            catch (IOException err) {
                log.debug("Could not look up {}\\{}: {}", domain, user, err.getMessage());
            }
        }
        return lookup.lookupPrincipalByName(user);
    }

    /*
     * On Windows Files.createTempDirectory would let the new directory inherit the ACL of the
     * temporary directory, which for a shared one lets other users create files in it. Create it
     * with a single entry for the current user instead; the libraries copied into the directory
     * inherit it.
     */
    private static FileAttribute<List<AclEntry>> ownerOnlyAcl(UserPrincipal user)
    {
        final List<AclEntry> acl =
            Collections.singletonList(AclEntry.newBuilder()
                                          .setType(AclEntryType.ALLOW)
                                          .setPrincipal(user)
                                          .setPermissions(EnumSet.allOf(AclEntryPermission.class))
                                          .setFlags(AclEntryFlag.FILE_INHERIT, AclEntryFlag.DIRECTORY_INHERIT)
                                          .build());
        return new FileAttribute<List<AclEntry>>() {
            @Override
            public String name()
            {
                return "acl:acl";
            }

            @Override
            public List<AclEntry> value()
            {
                return acl;
            }
        };
    }

    private static void checkOwnerOnlyAcl(Path dir, UserPrincipal user) throws IOException
    {
        AclFileAttributeView view =
            Files.getFileAttributeView(dir, AclFileAttributeView.class, LinkOption.NOFOLLOW_LINKS);
        if (view == null)
            throw new IOException("cannot read the ACL of " + dir);
        for (AclEntry entry : view.getAcl()) {
            if (!entry.principal().equals(user))
                throw new IOException(dir + " has an ACL entry for " + entry.principal().getName());
        }
    }

    /*
     * Windows cannot delete a loaded DLL when the JVM exits, so earlier runs leave their extraction
     * directories behind. Called only when the temporary directory belongs to the current user.
     * Remove the directories with the same owner as this JVM's directory {@code current} that have
     * not changed for EARLIER_DIR_MIN_AGE_MS, deleting only the library files this loader extracts;
     * files still in use by a running JVM cannot be deleted and are left alone. Failures never stop
     * the libraries from being loaded.
     */
    private static void removeEarlierExtractionDirectories(Path current)
    {
        Set<String> libraryFiles = new HashSet<>();
        for (String name : new String[] {"z", "zlib", "zlib1", "aec", "szip", "sz", "hdf5", "hdf5_java"})
            libraryFiles.add(System.mapLibraryName(name).toLowerCase(Locale.ROOT));
        long newest = System.currentTimeMillis() - EARLIER_DIR_MIN_AGE_MS;

        try (DirectoryStream<Path> dirs =
                 Files.newDirectoryStream(current.getParent(), EXTRACT_DIR_PREFIX + "*")) {
            UserPrincipal self = Files.getOwner(current, LinkOption.NOFOLLOW_LINKS);
            for (Path dir : dirs) {
                try {
                    if (dir.equals(current) || !Files.isDirectory(dir, LinkOption.NOFOLLOW_LINKS))
                        continue;
                    if (!self.equals(Files.getOwner(dir, LinkOption.NOFOLLOW_LINKS)))
                        continue;
                    if (Files.getLastModifiedTime(dir, LinkOption.NOFOLLOW_LINKS).toMillis() > newest)
                        continue;
                    try (DirectoryStream<Path> files = Files.newDirectoryStream(dir)) {
                        for (Path file : files) {
                            String name = file.getFileName().toString().toLowerCase(Locale.ROOT);
                            if (libraryFiles.contains(name) &&
                                Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS))
                                Files.deleteIfExists(file);
                        }
                    }
                    Files.deleteIfExists(dir);
                }
                catch (IOException | RuntimeException err) {
                    log.debug("Could not remove {}: {}", dir, err.getMessage());
                }
            }
        }
        catch (IOException | RuntimeException err) {
            log.debug("Could not look for earlier extraction directories: {}", err.getMessage());
        }
    }

    /**
     * Load the first of {@code names} found in the bundled native Maven JARs
     * ({@code natives/hdf5/<platform>/} on the classpath).
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
            String resource = RESOURCE_ROOT + platform + "/" + mapped;
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
            catch (IOException | RuntimeException | LinkageError err) {
                warn("Bundled native library {} could not be extracted: {}", resource, err.toString());
                return null;
            }
            try {
                System.load(lib.toAbsolutePath().toString());
                log.debug("Loaded bundled native library {}", lib);
                return lib;
            }
            catch (IllegalCallerException err) {
                warn("Bundled native library {} could not be loaded: native access is not enabled "
                         + "(run with --enable-native-access=ALL-UNNAMED)",
                     lib);
                return null;
            }
            catch (LinkageError | RuntimeException err) {
                String reason = err.getMessage();
                // glibc's message when the file system is mounted noexec (common for /tmp)
                if (reason != null && reason.contains("failed to map segment"))
                    warn("Bundled native library {} could not be loaded: {}. The temporary directory "
                             + "may be on a file system mounted noexec; set -Djava.io.tmpdir to a "
                             + "directory from which programs can be executed",
                         lib, reason);
                else
                    warn("Bundled native library {} could not be loaded: {}", lib, reason);
                return null;
            }
        }
        log.debug("No bundled native library {} under {}{}/", String.join("/", names), RESOURCE_ROOT,
                  platform);
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
     * The JNI library is built against the bundled libhdf5, so it is loaded only if the bundled
     * libhdf5 is loaded; {@link #loadBundledDependenciesBeforeHdf5()} is called first if it has
     * not been yet.
     *
     * @return true if bundled hdf5_java was loaded successfully
     */
    public static synchronized boolean loadBundledHdf5JavaIfPresent()
    {
        if (!attemptedHdf5Java) {
            if (!attemptedHdf5)
                loadBundledDependenciesBeforeHdf5();
            attemptedHdf5Java = true;
            if (!loadedHdf5)
                log.debug("Bundled hdf5_java not loaded: the bundled libhdf5 is not loaded");
            loadedHdf5Java = !skipBundledLoad() && loadedHdf5 && loadFirstBundled("hdf5_java") != null;
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
