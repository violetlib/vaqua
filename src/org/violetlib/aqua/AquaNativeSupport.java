/*
 * Copyright (c) 2015-2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.io.*;
import java.security.AccessControlException;
import java.util.StringTokenizer;

/**
 * Manages the native support library.
 */
public class AquaNativeSupport {

    /**
     * Expected version of the native code library.
     */
    private final static int EXPECTED_NATIVE_CODE_VERSION = 3;

    private static final String libraryName = "vaqua";

    private static boolean isAvailable;
    private static boolean isInitialized;

    /**
     * Load the native library, if not already loaded.
     *
     * @return true if the library has been loaded, false if the library cannot be loaded.
     */
    public static synchronized boolean load() {
        if (!isInitialized) {
            isInitialized = true;
            loadNativeSupport();
            if (isAvailable) {
                KeyWindowPatch.installIfNeeded();
                WindowStylePatch.installIfNeeded();
            }
        }
        return isAvailable;
    }

    /**
     * Load the native library.
     */
    private static void loadNativeSupport() {
        try {
            String fn = findNativeLibrary(AquaNativeSupport.class, libraryName);
            if (fn == null) {
                reportError("Library not found");
                return;
            }

            System.load(fn);

            int nativeCodeVersion = nativeGetNativeCodeVersion();
            if (nativeCodeVersion != EXPECTED_NATIVE_CODE_VERSION) {
                reportError("Incorrect library version " + nativeCodeVersion + " instead of " + EXPECTED_NATIVE_CODE_VERSION);
                return;
            }

            setup();
            isAvailable = true;
        } catch (UnsatisfiedLinkError e) {
            reportError(e.getMessage());
        } catch (AccessControlException e) {
            reportError("permission denied: " + e.getMessage());
        } catch (Throwable e) {
            reportError(e.toString());
            e.printStackTrace();
        }
    }

    private static void reportError(String msg) {
        String p = System.mapLibraryName(libraryName);
        String s = "AquaNativeSupport: Unable to load library " + p + ": " + msg;
        System.err.println(s);
    }

    public static String findNativeLibrary(Class<?> root, String name) throws IllegalArgumentException {
        File lf = findNativeLibraryOnPath(name);
        if (lf != null) {
            return lf.getPath();
        }

        String prefix = "lib" + name;
        String suffix = ".dylib";
        String libfn = prefix + suffix;

        // If we do not find the library using the library path, see if we can find it as a resource of the specified class.
        // If we find it, copy the resource to a temporary file.

        InputStream s;

        try {
            s = root.getClassLoader().getResourceAsStream(libfn);
        } catch (SecurityException ex) {
            return null;
        }

        if (s == null) {
            return null;
        }

        try {
            try {
                File f = File.createTempFile(prefix, suffix).getAbsoluteFile();

                try (FileOutputStream fs = new FileOutputStream(f)) {
                    internalInitializeFile(s, fs);
                }

                return f.getPath();

            } catch (IOException ex) {
                System.err.println("Unable to extract native library resource: " + ex.getMessage());
                return null;

            }
        } finally {
            try {
                s.close();
            } catch (IOException ex) {
            }
        }
    }

    private static File findNativeLibraryOnPath(String name) throws IllegalArgumentException {
        if (name.isEmpty()) {
            throw new IllegalArgumentException("Invalid library name: name is empty");
        }

        if (name.indexOf(' ') >= 0 || name.indexOf('.') >= 0 || name.indexOf(File.pathSeparatorChar) >= 0) {
            throw new IllegalArgumentException("Invalid library name");
        }

        String libfn = "lib" + name + ".dylib";

        // First try finding the library using the Java library path.

        String lp = System.getProperty("java.library.path");
        if (lp != null) {
            StringTokenizer st = new StringTokenizer(lp, ":");
            while (st.hasMoreTokens()) {
                String prefix = st.nextToken();
                File f = new File(prefix + File.separator + libfn).getAbsoluteFile();
                if (f.isFile()) {
                    return f;
                }
            }
        }

        return null;
    }

    private static void internalInitializeFile(InputStream sin, OutputStream sout) throws IOException {
        byte[] buf = new byte[1024];
        for (;;) {
            int count = sin.read(buf);
            if (count <= 0) {
                break;
            }
            sout.write(buf, 0, count);
        }
    }

    private static void setup() {
        int javaVersion = AquaUtils.getJavaVersion();
        setup(javaVersion);
    }

    private static native void setup(int javaVersion);

    /**
     * Returns the version of the native code library. If the version does not match with the version that we expect, we
     * can not use it.
     * @return The version number of the native code.
     */
    private static native int nativeGetNativeCodeVersion();
}
