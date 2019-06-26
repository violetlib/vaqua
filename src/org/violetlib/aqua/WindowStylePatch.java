/*
 * Copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.security.AccessControlException;

/**
 * A patch to prevent AWT from writing over window style bits set by VAqua.
 * This patch does not work in JDK 12+ (and is not used there).
 */

public class WindowStylePatch
{
    private static Boolean isInstalled; // false if install failed, true if install succeeded, null if not tried
    private static Boolean isNeeded;    // false if not needed, true if needed, null if not known

    public static void installIfNeeded() {
        if (isInstalled == null && isNeeded()) {
            loadNativeSupport();
        }
    }

    public static boolean isNeeded() {
        if (isNeeded == null) {
            isNeeded = computeIfNeeded();
        }
        return Boolean.TRUE.equals(isNeeded);
    }

    private static Boolean computeIfNeeded() {
        int version = AquaUtils.getJavaVersion();
        return version < 1200000;
    }

    private static void loadNativeSupport() {
        isInstalled = false;
        try {
            String fn = AquaNativeSupport.findNativeLibrary(AquaNativeSupport.class, "windowstylepatch");
            if (fn == null) {
                reportError("Library not found");
                return;
            }

            System.load(fn);
            isInstalled = true;
            AquaUtils.logDebug("VAqua: installed patch for preserving window style bits");
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
        String s = "WindowStylePatch: Unable to load library: " + msg;
        AquaUtils.logError(s);
    }
}
