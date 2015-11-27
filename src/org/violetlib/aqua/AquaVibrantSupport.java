/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;

/**
 * Support for vibrant backgrounds. A vibrant background is implemented by a special NSView that is installed as a
 * sibling behind the AWT content view. It is visible only where Java does not paint over it.
 */
public class AquaVibrantSupport {

    public static final int LIGHT_STYLE = 0;
    public static final int DARK_STYLE = 1;
    public static final int SIDEBAR_STYLE = 2;

    public static String VIBRANT_EFFECTS_KEY = "AquaInternal.vibrantEffects";

    public static int parseVibrantStyle(Object s) {
        if (s instanceof String) {
            if (s.equals("light")) {
                return AquaVibrantSupport.LIGHT_STYLE;
            }
            if (s.equals("dark")) {
                return AquaVibrantSupport.DARK_STYLE;
            }
            if (s.equals("sidebar")) {
                return AquaVibrantSupport.SIDEBAR_STYLE;
            }
        }
        return -1;
    }

    public static boolean isVibrant(JComponent c) {
        Object o = c.getClientProperty(AquaVibrantSupport.VIBRANT_EFFECTS_KEY);
        return o != null;
    }

    public static void installVibrantStyle(JComponent c, int style, SelectionBoundsTracker bt) {
        Object o = c.getClientProperty(AquaVibrantSupport.VIBRANT_EFFECTS_KEY);
        if (o != null) {
            if (o instanceof VisualEffectView) {
                VisualEffectView effects = (VisualEffectView) o;
                if (effects.getStyle() == style) {
                    return;
                }
                effects.dispose();
            }
            c.putClientProperty(AquaVibrantSupport.VIBRANT_EFFECTS_KEY, null);
        }

        VisualEffectView v = new ComponentVibrantEffects(c, style, bt);
        c.putClientProperty(AquaVibrantSupport.VIBRANT_EFFECTS_KEY, v);
    }

    public static void uninstallVibrantStyle(JComponent c) {
        Object o = c.getClientProperty(AquaVibrantSupport.VIBRANT_EFFECTS_KEY);
        if (o != null) {
            if (o instanceof VisualEffectView) {
                VisualEffectView effects = (VisualEffectView) o;
                effects.dispose();
            }
            c.putClientProperty(AquaVibrantSupport.VIBRANT_EFFECTS_KEY, null);
        }
    }

    /**
     * Add a full window sized visual effect view as a sibling behind the content view of the specified window.
     * If a full window sized visual effect view is already present, its style is updated as needed.
     * @param w The window.
     * @param style The vibrant style ({@link #LIGHT_STYLE} or {@link #DARK_STYLE}).
     */
    public static void addFullWindowVibrantView(Window w, int style) {
        if (style == SIDEBAR_STYLE) {
            throw new IllegalArgumentException("Sidebar style not supported for full window");
        }

        int rc = setupVisualEffectWindow(w, style);
        if (rc != 0) {
            System.err.println("Unable to install visual effect view");
        }
    }

    public static void removeFullWindowVibrantView(Window w) {
        int rc = removeVisualEffectWindow(w);
        if (rc != 0) {
            System.err.println("Unable to remove visual effect view");
        }
    }

    public static VisualEffectViewPeer createVisualEffectView(Window w, int style, boolean supportSelections) {
        long ptr = nativeCreateVisualEffectView(w, style, supportSelections);
        return ptr != 0 ? new VisualEffectViewPeerImpl(ptr) : null;
    }

    private static class VisualEffectViewPeerImpl implements VisualEffectViewPeer {
        private long nativeNSViewPointer;

        public VisualEffectViewPeerImpl(long nativeNSViewPointer) {
            this.nativeNSViewPointer = nativeNSViewPointer;
        }

        @Override
        public void dispose() {
            int rc = disposeVisualEffectView(nativeNSViewPointer);
            nativeNSViewPointer = 0;
            if (rc != 0) {
                System.err.println("disposeVisualEffectView failed");
            }
        }

        @Override
        public void setFrame(int x, int y, int width, int height) {
            int rc = setViewFrame(nativeNSViewPointer, x, y, width, height);
            if (rc != 0) {
                System.err.println("setViewFrame failed");
            }
        }

        public void updateSelectionBackgrounds(SelectionBoundsDescription sd) {
            int rc = nativeUpdateSelectionBackgrounds(nativeNSViewPointer, sd != null ? sd.getData() : null);
            if (rc != 0) {
                System.err.println("updateSelectionBackgrounds failed");
            }
        }
    }

    private static native int setupVisualEffectWindow(Window w, int style);
    private static native int removeVisualEffectWindow(Window w);
    private static native long nativeCreateVisualEffectView(Window w, int style, boolean supportSelections);
    private static native int setViewFrame(long viewPtr, int x, int y, int width, int height);
    private static native int nativeUpdateSelectionBackgrounds(long viewPtr, int[] data);
    private static native int disposeVisualEffectView(long viewPtr);
}
