/*
 * Copyright (c) 2015-2023 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Access to potentially dynamic user preferences.
 */
public class OSXSystemProperties {

    public static int simulatedOSVersion = 0;  // must come before OSVersion

    public final static int OSVersion = getOSVersion();  // for example: 1014 = macOS 10.14

    private static boolean hasBeenSynchronized;
    private static boolean isFullKeyboardAccessEnabled; // cached value
    private static boolean useOverlayScrollBars;        // cached value
    private static boolean reduceTransparency;          // cached value

    private static final List<ChangeListener> changeListeners = new ArrayList<>();

    private static int getOSVersion() {

        if (simulatedOSVersion > 0) {
            Utils.logDebug("Using simulated OS Version: " + simulatedOSVersion);
            return simulatedOSVersion;
        }

        String versionString = System.getProperty("os.version");
        int version = parseSystemVersion(versionString);
        if (version == 0) {
            Utils.logError("Unable to parse macOS version: " + versionString);
        } else if (version == 1016) {
            // Liar, there is no macOS 10.16!
            String s = readSystemVersion();
            if (s != null) {
                int v = parseSystemVersion(s);
                if (v > 0) {
                    version = v;
                    Utils.logDebug("Substituting macOS version: " + version);
                } else {
                    Utils.logError("Unable to parse macOS version: " + s);
                }
            }
        }
        return version;
    }

    private static @Nullable String readSystemVersion()
    {
        String[] cmd = { "/usr/bin/sw_vers", "-productVersion" };
        String[] env = { "SYSTEM_VERSION_COMPAT=0" };
        String s = command("sw_vers", cmd, env);
        if (s != null) {
            return s;
        }
        return null;
    }

    private static @Nullable String command(@NotNull String name, String @NotNull [] command, String @Nullable [] env)
    {
        try {
            Process p = Runtime.getRuntime().exec(command, env);
            InputStreamReader stdout = new InputStreamReader(p.getInputStream());
            StringBuffer sb = new StringBuffer();
            while (true) {
                int n = stdout.read();
                if (n == -1 || n == 10) {
                    break;
                }
                sb.append((char) n);
            }
            return sb.toString();
        } catch (Throwable th) {
            Utils.logError("Unable to run " + name, th);
            return null;
        }
    }

    private static int parseSystemVersion(@NotNull String version)
    {
        try {
            int p = version.indexOf('.');
            if (p > 0) {
                int major = Integer.parseInt(version.substring(0, p));
                if (major > 0) {
                    String s = version.substring(p + 1);
                    p = s.indexOf('.');
                    int minor = Integer.parseInt(p >= 0 ? s.substring(0, p) : s);
                    if (minor >= 0 && minor < 100) {
                        return major * 100 + minor;
                    }
                }
            }
        } catch (Exception ignore) {
        }
        return 0;
    }

    public static boolean useInsetViewStyle() {
        return OSVersion >= 1016;
    }

    public static boolean isFullKeyboardAccessEnabled() {
        ensureSynchronized();
        return isFullKeyboardAccessEnabled;
    }

    public static boolean isReduceTransparency() {
        ensureSynchronized();
        return reduceTransparency;
    }

    public static boolean doScrollPanesSupportRTL() {
        // OS X scroll bars do not change in RTL orientation
        // Perhaps this may change...
        return false;
    }

    /**
     * Indicate whether a file chooser should display all files or hide ones of less interest to naive users.
     * @return true to show all files, false otherwise.
     */
    public static boolean isShowAllFiles() {
        // This is a Finder preference. There is no notification for updates.
        // Even Finder only reads the value at start up.
        // We read the value each time the value is requested.
        // The assumption is that this does not happen frequently, e.g. only when a file dialog is opened.
        if (AquaNativeSupport.load()) {
            return nativeGetShowAllFiles();
        } else {
            return false;
        }
    }

    /**
     * Indicate whether overlay scroll bars should be used.
     * @return true to use overlay scroll bars, false to use legacy scroll bars.
     */
    public static boolean useOverlayScrollBars() {
        ensureSynchronized();
        return useOverlayScrollBars;
    }

    /**
     * Indicate whether clicking in the scroll bar track should jump to the clicked location or scroll by a page.
     * @return true to jump, false to page.
     */
    public static boolean isScrollToClick() {
        if (AquaNativeSupport.load()) {
            return nativeGetScrollToClick();
        } else {
            return false;
        }
    }

    public static synchronized void addChangeListener(ChangeListener l) {
        if (!changeListeners.contains(l)) {
            changeListeners.add(l);
        }
    }

    public static synchronized void removeChangeListener(ChangeListener l) {
        changeListeners.remove(l);
    }

    private static void fireChangeEvent() {
        if (!changeListeners.isEmpty()) {
            SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    ChangeEvent event = new ChangeEvent(OSXSystemProperties.class);
                    for (ChangeListener listener : new ArrayList<>(changeListeners)) {
                        listener.stateChanged(event);
                    }
                }
            });
        }
    }

    private static void ensureSynchronized() {
        if (!hasBeenSynchronized) {
            if (AquaNativeSupport.load()) {
                // Avoid potential timing issue by registering the callback first
                Runnable synchronizer = () -> SwingUtilities.invokeLater(OSXSystemProperties::synchronize);
                enableCallback(synchronizer);
                synchronize();
            } else {
                hasBeenSynchronized = true;
            }
        }
    }

    private static synchronized void synchronize() {
        // this method is called from native code when a monitored property changes
        boolean oldFullKeyboardAccessEnabled = isFullKeyboardAccessEnabled;
        isFullKeyboardAccessEnabled = nativeGetFullKeyboardAccessEnabled();

        boolean oldUseOverlayScrollBars = useOverlayScrollBars;
        useOverlayScrollBars = nativeGetUseOverlayScrollBars();

        boolean oldReduceTransparency = reduceTransparency;
        reduceTransparency = nativeGetReduceTransparency();

        if (hasBeenSynchronized &&
                (isFullKeyboardAccessEnabled != oldFullKeyboardAccessEnabled)
                || (useOverlayScrollBars != oldUseOverlayScrollBars)
                || (reduceTransparency != oldReduceTransparency)) {
            fireChangeEvent();
        }

        hasBeenSynchronized = true;
    }

    private static native boolean nativeGetFullKeyboardAccessEnabled();
    private static native boolean nativeGetShowAllFiles();
    private static native boolean nativeGetScrollToClick();
    private static native boolean nativeGetUseOverlayScrollBars();
    private static native boolean nativeGetReduceTransparency();
    private static native void enableCallback(Runnable synchronizer);
}
