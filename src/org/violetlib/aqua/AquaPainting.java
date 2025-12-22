/*
 * Copyright (c) 2015-2025 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import org.jetbrains.annotations.NotNull;
import org.violetlib.jnr.aqua.AquaNativeRendering;
import org.violetlib.jnr.aqua.AquaUIPainter;

/**
 * Provides access to the native painter.
 */
public class AquaPainting {

    private static int cachedRenderingVersion;

    public static int getVersion()
    {
        if (cachedRenderingVersion > 0) {
            return cachedRenderingVersion;
        }
        int version = OSXSystemProperties.OSVersion;
        if (version >= 1600) {
            try {
                version = AquaNativeRendering.getSystemRenderingVersion() / 100;
            } catch (NoSuchMethodError ignore) {
            }
        }
        return cachedRenderingVersion = version;
    }

    public static boolean isSidebarVibrant()
    {
        // TBD: the current VisualEffectView code does not support rounded corners.

        return !useLiquidGlassSidebar();
    }

    public static boolean useLiquidGlassSidebar()
    {
        return getVersion() >= 1600;
    }

    public static @NotNull AquaUIPainter create() {
        return AquaNativeRendering.createPainter();
    }
}
