/*
 * Copyright (c) 2015-2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.*;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import static org.violetlib.aqua.AquaUtils.execute;

/**
 * Support for vibrant backgrounds. A vibrant background is implemented by a special NSView that is installed as a
 * sibling behind the AWT content view. It is visible only where Java does not paint over it. We use the magic eraser
 * (see AquaUtils) to knock out any Java background where we want the vibrant background to show through.
 */
public class AquaVibrantSupport {

    public static final int NO_VIBRANT_STYLE = -1;

    public static final int LIGHT_STYLE = 0;    // deprecated in macOS 10.14
    public static final int DARK_STYLE = 1;     // deprecated in macOS 10.14
    public static final int SIDEBAR_STYLE = 2;
    public static final int TITLE_BAR_STYLE = 3;
    public static final int MENU_STYLE = 4;
    public static final int POPOVER_STYLE = 5;
    public static final int MEDIUM_LIGHT_STYLE = 6; // deprecated in macOS 10.14
    public static final int ULTRA_DARK_STYLE = 7;   // deprecated in macOS 10.14
    public static final int SELECTION_STYLE = 9;

    // Materials defined in macOS 10.14
    public static final int SHEET_STYLE = 8;    // also used as a way of indicating that the window will be displayed as a sheet
    public static final int HEADER_STYLE = 10;
    public static final int WINDOW_BACKGROUND_STYLE = 11;
    public static final int HUD_WINDOW_STYLE = 12;
    public static final int FULL_SCREEN_MODAL_STYLE = 13;
    public static final int TOOL_TIP_STYLE = 14;
    public static final int CONTENT_BACKGROUND_STYLE = 15;
    public static final int UNDER_WINDOW_BACKGROUND_STYLE = 16;
    public static final int UNDER_PAGE_BACKGROUND_STYLE = 17;

    /** This client property allows the client to request a vibrant background style on certain components. */
    public static final String BACKGROUND_STYLE_KEY = "Aqua.backgroundStyle";

    /** This client property allows the client to request a vibrant background style on the top component of a popup. */
    public static final String POPUP_BACKGROUND_STYLE_KEY = "AquaPopup.backgroundStyle";

    /** This client property allows the client to request rounded corners on the top component of a popup. */
    public static final String POPUP_CORNER_RADIUS_KEY = "AquaPopup.cornerRadius";

    /** This internal client property stores our state for vibrant components, allowing use of a singleton component UI. */
    private static final String VIBRANT_EFFECTS_KEY = "AquaInternal.vibrantEffects";

    /** This internal client property records when a window has a full size vibrant background installed. */
    private static final String VIBRANT_WINDOW_KEY = "AquaInternal.vibrantWindow";

    private static final PropertyChangeListener vibrantStylePropertyChangeListener = new VibrantStylePropertyChangeListener();

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
            if (s.equals("vibrantSheet")) {
                return SHEET_STYLE;
            }
            if (s.equals("vibrantSelection")) {
                return SELECTION_STYLE;
            }
            if (s.equals("vibrantHeader")) {
                return HEADER_STYLE;
            }
            if (s.equals("vibrantWindowBackground")) {
                return WINDOW_BACKGROUND_STYLE;
            }
            if (s.equals("vibrantHUDWindow")) {
                return HUD_WINDOW_STYLE;
            }
            if (s.equals("vibrantFullScreenUI")) {
                return FULL_SCREEN_MODAL_STYLE;
            }
            if (s.equals("vibrantToolTip")) {
                return TOOL_TIP_STYLE;
            }
            if (s.equals("vibrantContentBackground")) {
                return CONTENT_BACKGROUND_STYLE;
            }
            if (s.equals("vibrantUnderWindowBackground")) {
                return UNDER_WINDOW_BACKGROUND_STYLE;
            }
            if (s.equals("vibrantUnderPageBackground")) {
                return UNDER_PAGE_BACKGROUND_STYLE;
            }
        }
        return NO_VIBRANT_STYLE;
    }

    /**
     * Test a component to see if the background style is vibrant.
     */
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
     * Install support for vibrant background styles on a component.
     * @param c The component.
     */
    public static void installVibrantStyle(@NotNull JComponent c) {
        c.addPropertyChangeListener(vibrantStylePropertyChangeListener);
        updateVibrantStyle(c);
    }

    /**
     * Remove support for vibrant background styles on a component.
     * @param c The component.
     */
    public static void uninstallVibrantStyle(@NotNull JComponent c) {
        c.removePropertyChangeListener(vibrantStylePropertyChangeListener);
        internalUninstallVibrantStyle(c);
    }

    /**
     * Recognize and implement a change to the component background style client property.
     * @param evt The property change event.
     */
    private static void processVibrantStyleChange(@NotNull PropertyChangeEvent evt) {
        String prop = evt.getPropertyName();
        JComponent c = (JComponent) evt.getSource();
        if (BACKGROUND_STYLE_KEY.equals(prop)) {
            updateVibrantStyle(c);
        }
    }

    /**
     * Update the background style for a component to be consistent with its background style client property.
     * @param c The component.
     */
    private static void updateVibrantStyle(@NotNull JComponent c) {
        Object o = c.getClientProperty(BACKGROUND_STYLE_KEY);
        if (o instanceof String) {
            int style = AquaVibrantSupport.parseVibrantStyle(o, true);
            if (style >= 0) {
                installVibrantStyle(c, style, null);
                return;
            }
        }
        internalUninstallVibrantStyle(c);
    }

    /**
     * Install the implementation of a vibrant component background.
     * @param c The component.
     * @param style The vibrant style.
     * @param bt An optional selection bounds tracker, to support regions displaying a vibrant selection background.
     */
    private static void installVibrantStyle(@NotNull JComponent c, int style, @Nullable SelectionBoundsTracker bt) {
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
     * Uninstall the implementation of a vibrant component background.
     * @param c The component.
     */
    private static void internalUninstallVibrantStyle(@NotNull JComponent c) {
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
     * @param style The vibrant style.
     */
    public static void addFullWindowVibrantView(@NotNull Window w, int style) {
        // If a window can never become active, then we should force visual effect view to display the active state.
        // Otherwise, there is no point to enabling a vibrant style.

        boolean forceActive = w.getType() == Window.Type.POPUP || !AquaUtils.isDecorated(w);
        long rc = execute(w, ptr -> setupVisualEffectWindow(ptr, style, forceActive));
        if (rc != 0) {
            Utils.logError("Unable to install visual effect view");
        } else {
            JRootPane rp = AquaUtils.getRootPane(w);
            if (rp != null) {
                AquaUtils.enableTranslucency(w);
                rp.putClientProperty(VIBRANT_WINDOW_KEY, Boolean.TRUE);
                AquaUtils.paintImmediately(w, rp);
                // The goal of the following is to transfer the new clear background to the AWTView layer immediately so
                // that when a vibrant window is first made visible, it shows the vibrant background immediately instead
                // of showing the default window background first and an instant later replacing it with the vibrant
                // background.
                AquaUtils.syncAWTView(w);
            }
        }
    }

    /**
     * Remove any full window sized visual effect view installed on the specified window.
     * @param w The window.
     */
    public static void removeFullWindowVibrantView(@NotNull Window w) {
        JRootPane rp = AquaUtils.getRootPane(w);
        if (rp != null) {
            if (rp.getClientProperty(VIBRANT_WINDOW_KEY) != null) {
                rp.putClientProperty(VIBRANT_WINDOW_KEY, null);
                if (w.isDisplayable()) {
                    long rc = execute(w, AquaVibrantSupport::removeVisualEffectWindow);
                    if (rc != 0) {
                        Utils.logError("Unable to remove visual effect view");
                    } else {
                        rp.repaint();
                    }
                }
            }
        }
    }

    /**
     * Create a visual effect view behind the content view of the specified window.
     * @param w The window.
     * @param style The vibrant style.
     * @param supportSelections If true, support is enabled for additional visual effect views to implement regions with
     *                          a vibrant selection background.
     * @return a peer that can be used to specify the bounds of the background view and the bounds of the regions
     *         displaying a vibrant selection background.
     */
    public static VisualEffectViewPeer createVisualEffectView(@NotNull Window w, int style, boolean supportSelections) {
        long ptr = execute(w, wptr -> nativeCreateVisualEffectView(wptr, style, supportSelections));
        if (ptr != 0) {
            AquaUtils.enableTranslucency(w);
            return new VisualEffectViewPeerImpl(w, ptr);
        }
        return null;
    }

    private static class VibrantStylePropertyChangeListener implements PropertyChangeListener {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {
            AquaVibrantSupport.processVibrantStyleChange(evt);
        }
    }

    private static class VisualEffectViewPeerImpl implements VisualEffectViewPeer {
        private final @NotNull Window w;
        private long nativeNSViewPointer;

        public VisualEffectViewPeerImpl(@NotNull Window w, long nativeNSViewPointer) {
            this.w = w;
            this.nativeNSViewPointer = nativeNSViewPointer;
        }

        @Override
        public void dispose() {
            if (nativeNSViewPointer != 0) {
                if (w.isDisplayable()) {
                    int rc = disposeVisualEffectView(nativeNSViewPointer);
                    if (rc != 0) {
                        Utils.logError("disposeVisualEffectView failed");
                    }
                }
                nativeNSViewPointer = 0;
                //AquaUtils.setWindowBackgroundClear(w, false); // restore Java window background
            }
        }

        @Override
        public void setFrame(int x, int y, int width, int height) {
            // The specified bounds are in Java window coordinates. Native code might need the distance between the
            // bottom of the frame and the bottom of the window. It may not be able to determine that distance because
            // the native window height might be (temporarily) different that the Java window height. We resolve by
            // calculating that distance here and passing it to the native code.

            int yflipped = w.getHeight() - (y + height);
            int rc = setViewFrame(nativeNSViewPointer, x, y, width, height, yflipped);
            if (rc != 0) {
                Utils.logError("setViewFrame failed");
            }
        }

        public void updateSelectionBackgrounds(SelectionBoundsDescription sd) {
            int rc = nativeUpdateSelectionBackgrounds(nativeNSViewPointer, sd != null ? sd.getData() : null);
            if (rc != 0) {
                Utils.logError("updateSelectionBackgrounds failed");
            }
        }
    }

    private static native int setupVisualEffectWindow(long w, int style, boolean forceActive);
    private static native int removeVisualEffectWindow(long w);
    private static native long nativeCreateVisualEffectView(long w, int style, boolean supportSelections);
    private static native int setViewFrame(long viewPtr, int x, int y, int width, int height, int yflipped);
    private static native int nativeUpdateSelectionBackgrounds(long viewPtr, int[] data);
    private static native int disposeVisualEffectView(long viewPtr);
}
