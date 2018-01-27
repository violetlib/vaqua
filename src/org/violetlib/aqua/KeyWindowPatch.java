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
	private static Boolean isInstalled;
	private static Boolean isNeeded;

	public static void applyPatchIfNeeded(Window w) {
		if (isNeeded(w)) {
			ensureWindowDelegateInstalled(w);
		}
	}

	private static boolean isNeeded(Window w) {
		if (isNeeded == null) {
			isNeeded = computeIfNeeded(w);
		}
		return Boolean.TRUE.equals(isNeeded);
	}

	private static Boolean computeIfNeeded(Window w) {
		if (isInstalled == null) {
			loadNativeSupport();
		}

		if (Boolean.TRUE.equals(isInstalled)) {
			try {
                long result = AquaUtils.execute(w, ptr -> nativeIsPatchNeeded(ptr));
                return result != 0;
            } catch (UnsupportedOperationException ex) {
                // Could be that the window is not yet displayable.
				// Probably this should not happen.
                return null;
            }
		} else {
			return false;
		}
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

	private static void ensureWindowDelegateInstalled(Window w)
	{
		AquaUtils.ensureWindowPeer(w);
		AquaUtils.execute(w, ptr -> ensureWindowDelegateInstalled(w, ptr));
	}

	private static long ensureWindowDelegateInstalled(Window w, long wptr)
	{
		return nativeEnsureWindowDelegateInstalled(wptr);
	}

	private static native int nativeIsPatchNeeded(long w);
	private static native int nativeEnsureWindowDelegateInstalled(long w);
}
