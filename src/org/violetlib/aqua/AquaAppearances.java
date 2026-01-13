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
    public static final Object APPEARANCE_CHANGE_TYPE = "AppearanceChange";

    /**
     * There can be at most one set of colors for each native appearance. Currently, that means one set of colors for
     * the light appearance and one set of colors for the dark appearance. When appearance settings change, all cached
     * colors become invalid.
     */

    private static final @NotNull Map<String,Colors> appearanceColors = new HashMap<>();

    static {
        VAppearances.addEffectiveAppearanceChangeListener(AquaAppearances::appearancesChanged);
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

    /**
     * Register a component whose UI is to be notified when the system appearance has changed or the colors associated
     * with the existing system appearance may have changed.
     */

    public static void register(@NotNull JComponent jc) {
        SystemPropertyChangeManager.register(jc);
    }

    public static void unregister(@NotNull JComponent jc) {
        SystemPropertyChangeManager.unregister(jc);
    }

    private static void appearancesChanged(@NotNull ChangeEvent ev) {
        // invoked by VAppearances on the AWT event thread
        resetNativeRendering();
        AquaImageFactory.flushAppearanceDependentImages();
        appearanceColors.clear();
        SwingUtilities.invokeLater(() -> {
            SystemPropertyChangeManager.notifyChange(APPEARANCE_CHANGE_TYPE);
        });
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
