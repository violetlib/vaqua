/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;

/**
 * Support for vibrant backgrounds. A vibrant background is implemented by a special NSView that is installed as a
 * sibling behind the AWT content view. It is visible only where Java does not paint over it.
 */

public class AquaVibrantSupport {

    public static final int LIGHT_STYLE = 0;
    public static final int DARK_STYLE = 1;

    /**
     * Add a full window sized visual effect view as a sibling behind the content view of the specified window.
     * @param w The window.
     * @param style The vibrant style ({@link #LIGHT_STYLE} or {@link #DARK_STYLE}).
     */
    public static void addFullWindowVibrantView(Window w, int style) {
        int rc = setupVisualEffectWindow(w, style);
        if (rc != 0) {
            System.err.println("Unable to install visual effect view");
        }
    }

    private static native int setupVisualEffectWindow(Window w, int style);
    public static native long createVisualEffectView(Window w, int style);
    public static native int setVisualEffectViewFrame(long viewPtr, int x, int y, int width, int height);
    public static native int disposeVisualEffectView(long viewPtr);
}
