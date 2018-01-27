/*
 * Copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.Window;
import java.security.AccessControlException;

/**
 * A very hairy patch to support coexistence with native key-only windows, such as the NSColorPanel.
 */

public class KeyWindowPatch
{
    private static Boolean isInstalled; // false if install failed, true if install succeeded, null if not tried
    private static Boolean isNeeded;    // false if not needed, true if needed, null if not known

    public static void installIfNeeded() {
        if (isInstalled == null && isNeeded()) {
            loadNativeSupport();
        }
    }

    public static void applyPatchIfNeeded(Window w) {
        if (isNeededAndIsInstalled()) {
            AquaUtils.ensureWindowPeer(w);
            AquaUtils.execute(w, ptr -> ensureWindowDelegateInstalled(w, ptr));
        }
    }

    private static boolean isNeeded() {
        if (isNeeded == null) {
            isNeeded = computeIfNeeded();
        }
        return Boolean.TRUE.equals(isNeeded);
    }

    private static boolean isNeededAndIsInstalled() {
        if (isNeeded()) {
            if (isInstalled == null) {
                loadNativeSupport();
            }
            return Boolean.TRUE.equals(isInstalled);
        }
        return false;
    }

    private static Boolean computeIfNeeded() {
        int result = nativeIsPatchNeeded();
        return result != 0;
    }

    private static void loadNativeSupport() {
        isInstalled = false;
        try {
            String fn = AquaNativeSupport.findNativeLibrary(AquaNativeSupport.class, "keywindowpatch");
            if (fn == null) {
                reportError("Library not found");
                return;
            }

            System.load(fn);
            isInstalled = true;
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
        String s = "KeyWindowPatch: Unable to load library: " + msg;
        System.err.println(s);
    }

    private static long ensureWindowDelegateInstalled(Window w, long wptr)
    {
        return nativeEnsureWindowDelegateInstalled(wptr);
    }

    private static native int nativeIsPatchNeeded();
    private static native int nativeEnsureWindowDelegateInstalled(long w);
}
