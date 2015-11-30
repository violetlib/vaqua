/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.beans.PropertyChangeEvent;
import javax.swing.*;

/**
 * Support for vibrant backgrounds. A vibrant background is implemented by a special NSView that is installed as a
 * sibling behind the AWT content view. It is visible only where Java does not paint over it. We use the magic eraser
 * (see AquaUtils) to knock out any Java background where we want the vibrant background to show through.
 */
public class AquaVibrantSupport {

    public static final int LIGHT_STYLE = 0;
    public static final int DARK_STYLE = 1;
    public static final int SIDEBAR_STYLE = 2;
    public static final int TITLE_BAR_STYLE = 3;
    public static final int MENU_STYLE = 4;
    public static final int POPOVER_STYLE = 5;
    public static final int MEDIUM_LIGHT_STYLE = 6;
    public static final int ULTRA_DARK_STYLE = 7;

    /** This client property allows the client to request a vibrant background style on certain components. */
    public static final String BACKGROUND_STYLE_KEY = "Aqua.backgroundStyle";

    /** This internal client property stores our state for vibrant components, allowing use of a singleton component UI. */
    public static final String VIBRANT_EFFECTS_KEY = "AquaInternal.vibrantEffects";

    /** This internal client property records when a window has a full size vibrant background installed. */
    public static final String VIBRANT_WINDOW_KEY = "AquaInternal.vibrantWindow";

    public static int parseVibrantStyle(Object s, boolean allowSidebar) {
        if (s instanceof String) {
            if (s.equals("vibrantLight")) {
                return LIGHT_STYLE;
            }
            if (s.equals("vibrantDark")) {
                return DARK_STYLE;
            }
            if (s.equals("vibrantSidebar")) {
                return allowSidebar ? SIDEBAR_STYLE : LIGHT_STYLE;
            }
            if (s.equals("vibrantTitleBar")) {
                return TITLE_BAR_STYLE;
            }
            if (s.equals("vibrantMenu")) {
                return MENU_STYLE;
            }
            if (s.equals("vibrantPopover")) {
                return POPOVER_STYLE;
            }
            if (s.equals("vibrantMediumLight")) {
                return MEDIUM_LIGHT_STYLE;
            }
            if (s.equals("vibrantUltraDark")) {
                return ULTRA_DARK_STYLE;
            }
        }
        return -1;
    }

    public static boolean isVibrant(JComponent c) {
        Object o = c.getClientProperty(AquaVibrantSupport.VIBRANT_EFFECTS_KEY);
        if (o != null) {
            return true;
        }
        if (c instanceof JRootPane) {
            o = c.getClientProperty(AquaVibrantSupport.VIBRANT_WINDOW_KEY);
            if (o != null) {
                return true;
            }
        }
        return false;
    }

    /**
     * Recognize and implement a change to the component background style client property.
     * @param evt The property change event.
     * @return true if and only if the event was for the background style client property.
     */
    public static boolean processVibrantStyleChange(PropertyChangeEvent evt) {
        String prop = evt.getPropertyName();
        JComponent c = (JComponent) evt.getSource();
        if (BACKGROUND_STYLE_KEY.equals(prop)) {
            updateVibrantStyle(c);
            return true;
        }
        return false;
    }

    /**
     * Update the background style for a component to be consistent with its background style client property.
     * @param c The component.
     */
    public static void updateVibrantStyle(JComponent c) {
        Object o = c.getClientProperty(BACKGROUND_STYLE_KEY);
        if (o instanceof String) {
            int style = AquaVibrantSupport.parseVibrantStyle(o, true);
            if (style >= 0) {
                installVibrantStyle(c, style, null);
                return;
            }
        }
        uninstallVibrantStyle(c);
    }

    /**
     * Install the implementation of a vibrant component background.
     * @param c The component.
     * @param style The vibrant style.
     * @param bt An optional selection bounds tracker, to support regions displaying a vibrant selection background.
     */
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

    /**
     * Remove any implementation of a vibrant component background.
     * @param c The component.
     */
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
     * @param style The vibrant style ({@link #SIDEBAR_STYLE} is not supported).
     */
    public static void addFullWindowVibrantView(Window w, int style) {
        if (style == SIDEBAR_STYLE) {
            throw new IllegalArgumentException("Sidebar style not supported for full window");
        }

        int rc = setupVisualEffectWindow(w, style);
        if (rc != 0) {
            System.err.println("Unable to install visual effect view");
        } else {
            JRootPane rp = AquaUtils.getRootPane(w);
            AquaUtils.setWindowBackgroundClear(w, true); // suppress Java window background
            if (rp != null) {
                rp.putClientProperty(VIBRANT_WINDOW_KEY, Boolean.TRUE);
                rp.repaint();
                // The goal of the following is to transfer the new clear background to the AWTView layer immediately so
                // that when a vibrant window is first displayed, it shows the vibrant background immediately instead of
                // showing the default window background and an instant later replacing it with the vibrant background.
                AquaUtils.syncAWTView(w);
            }
        }
    }

    /**
     * Remove any full window sized visual effect view installed on the specified window.
     * @param w The window.
     */
    public static void removeFullWindowVibrantView(Window w) {
        int rc = removeVisualEffectWindow(w);
        if (rc != 0) {
            System.err.println("Unable to remove visual effect view");
        }
        JRootPane rp = AquaUtils.getRootPane(w);
        if (rp != null) {
            if (rp.getClientProperty(VIBRANT_WINDOW_KEY) != null) {
                AquaUtils.setWindowBackgroundClear(w, false);
                rp.repaint();
            }
        }
    }

    /**
     * Create a visual effect view behind the content view of the specified window.
     * @param w The window.
     * @param style The vibrant style.
     * @param supportSelections If true, support is enabled for additional visual effect views to implement regions
     *                          with a vibrant selection background.
     * @return a peer that can be used to specify the bounds of the background view and the bounds of the regions
     *         displaying a vibrant selection background.
     */
    public static VisualEffectViewPeer createVisualEffectView(Window w, int style, boolean supportSelections) {
        long ptr = nativeCreateVisualEffectView(w, style, supportSelections);
        if (ptr != 0) {
            AquaUtils.setWindowBackgroundClear(w, true); // suppress Java window background
            return new VisualEffectViewPeerImpl(w, ptr);
        }
        return null;
    }

    private static class VisualEffectViewPeerImpl implements VisualEffectViewPeer {
        private Window w;
        private long nativeNSViewPointer;

        public VisualEffectViewPeerImpl(Window w, long nativeNSViewPointer) {
            this.w = w;
            this.nativeNSViewPointer = nativeNSViewPointer;
        }

        @Override
        public void dispose() {
            if (nativeNSViewPointer != 0) {
                int rc = disposeVisualEffectView(nativeNSViewPointer);
                nativeNSViewPointer = 0;
                if (rc != 0) {
                    System.err.println("disposeVisualEffectView failed");
                }
                AquaUtils.setWindowBackgroundClear(w, false); // restore Java window background
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
