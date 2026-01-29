/*
 * Copyright (c) 2018-2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import javax.swing.*;
import javax.swing.event.ChangeEvent;

import org.jetbrains.annotations.*;
import org.violetlib.jnr.aqua.AquaNativeRendering;
import org.violetlib.vappearances.VAppearance;
import org.violetlib.vappearances.VAppearances;

/**
 * All methods must be called on the UI event thread.
 */

public class AquaAppearances {
    private static final @NotNull Map<String,AquaAppearance> appearances = new HashMap<>();
    private static final @NotNull String defaultAppearanceName = "NSAppearanceNameAqua";

    /**
     * There can be at most one set of colors for each native appearance. Currently, that means one set of colors for
     * the light appearance and one set of colors for the dark appearance. When appearance settings change, all cached
     * colors become invalid.
     */

    private static final @NotNull Map<String,Colors> appearanceColors = new HashMap<>();

    private static @Nullable AquaAppearance cachedApplicationEffectiveAppearance;

    static {
        VAppearances.addEffectiveAppearanceChangeListener(AquaAppearances::effectiveAppearanceChanged);
    }

    /**
     * Return the appearance with the specified name.
     * @param appearanceName The appearance name.
     * @return the appearance, or the default appearance if an appearance with this name is not available.
     * @throws UnsupportedOperationException if this appearance and the default appearance are not available.
     */

    public static @NotNull AquaAppearance get(@NotNull String appearanceName) throws UnsupportedOperationException {
        AquaAppearance appearance = getOptional(appearanceName);
        return appearance != null ? appearance : getDefaultAppearance();
    }

    /**
     * Return the appearance with the specified name, if available.
     * @param appearanceName The appearance name.
     * @return the appearance, or null if an appearance with this name is not available.
     */

    public static @Nullable AquaAppearance getOptional(@NotNull String appearanceName) {
        AquaAppearance appearance = appearances.get(appearanceName);
        if (appearance == null) {
            try {
                VAppearance a = VAppearances.getAppearance(appearanceName);
                AquaAppearance aa = new AquaAppearance(a, Utils::logDebug);
                appearances.put(appearanceName, aa);
                return aa;
            } catch (IOException ex) {
                AquaUtils.syslog("Unable to get " + appearanceName + ": " + ex.getMessage());
            }
        }
        return appearance;
    }

    /**
     * Return the application effective appearance.
     * @throws UnsupportedOperationException if the application effective appearance is not available and the
     * default appearance is unavailable.
     */

    public static @NotNull AquaAppearance getApplicationEffectiveAppearance()
      throws UnsupportedOperationException
    {
        if (cachedApplicationEffectiveAppearance == null) {
            AquaAppearance a = determineApplicationEffectiveAppearance();
            if (a == null) {
                a = getDefaultAppearance();
            }
            cachedApplicationEffectiveAppearance = a;
        }
        return cachedApplicationEffectiveAppearance;
    }

    public static void setApplicationAppearance(@Nullable String appearanceName)
    {
        try {
            VAppearances.setApplicationAppearance(appearanceName);
            cachedApplicationEffectiveAppearance = null;
        } catch (IOException ignore) {
        }
    }

    private static @Nullable AquaAppearance determineApplicationEffectiveAppearance()
    {
        try {
            VAppearance appearance = VAppearances.getApplicationEffectiveAppearance();
            String appearanceName = appearance.getName();
            AquaAppearance a = appearances.get(appearanceName);
            if (a != null) {
                return a;
            }
            a = new AquaAppearance(appearance, Utils::logDebug);
            appearances.put(appearanceName, a);
            return a;
        } catch (IOException e) {
            AquaUtils.syslog("Unable to get application effective appearance: " + e.getMessage());
            return null;
        }
    }

    /**
     * Return a default appearance. This appearance is used only in case of an unexpected error.
     * @return the default appearance.
     * @throws UnsupportedOperationException if the default appearance is not available.
     */

    public static @NotNull AquaAppearance getDefaultAppearance()
      throws UnsupportedOperationException
    {
        AquaAppearance appearance = appearances.get(defaultAppearanceName);
        if (appearance == null) {
            try {
                VAppearance a = VAppearances.getAppearance(defaultAppearanceName);
                AquaAppearance aa = new AquaAppearance(a, Utils::logDebug);
                appearances.put(defaultAppearanceName, aa);
                return aa;
            } catch (IOException ex) {
                AquaUtils.syslog("Unable to get " + defaultAppearanceName + ": " + ex.getMessage());
                ex.printStackTrace();
                throw new UnsupportedOperationException("Default appearance " + defaultAppearanceName + " is not available");
            }
        }
        return appearance;
    }

    private static void effectiveAppearanceChanged(@NotNull ChangeEvent ev)
    {
        // invoked by VAppearances on the AWT event thread
        resetNativeRendering();
        AquaImageFactory.flushAppearanceDependentImages();
        appearanceColors.clear();
        cachedApplicationEffectiveAppearance = null;

        // Surprisingly, the following is not redundant. The window effective appearance change notification is not
        // called when the accent color is changed.

        try {
            Window[] windows = Window.getWindows();
            for (Window w : windows) {
                if (w instanceof RootPaneContainer) {
                    RootPaneContainer rpc = (RootPaneContainer) w;
                    JRootPane rp = rpc.getRootPane();
                    AquaRootPaneUI ui = AquaUtils.getUI(rp, AquaRootPaneUI.class);
                    if (ui != null) {
                        ui.effectiveAppearanceChanged();
                    }
                }
            }
        } catch (UnsupportedOperationException ignore) {
        }
    }

    private static void resetNativeRendering()
    {
        AquaNativeRendering.clearCache();
        try {
            AquaNativeRendering.invalidateAppearances();
        } catch (Throwable ex) {
            // Must be an older release
        }
    }

    public static @NotNull Colors getColorsForAppearance(@NotNull VAppearance appearance)
    {
        String name = appearance.getName();
        Colors colors = appearanceColors.get(name);
        if (colors == null) {
            colors = createColorsForAppearance(appearance);
            appearanceColors.put(name, colors);
        }
        return colors;
    }

    private static @NotNull Colors createColorsForAppearance(@NotNull VAppearance appearance)
    {
        int OSVersion = AquaPainting.getVersion();
        Map<String,Color> nativeColors = AquaNativeRendering.createPainter().getColors(appearance);
        return new AppearanceColorsBuilder(appearance, OSVersion, nativeColors, null, Utils::logDebug).getResult();
    }
}
