/*
 * Copyright (c) 2018-2023 Alan Snyder.
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

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.jnr.aqua.AquaNativeRendering;
import org.violetlib.vappearances.VAppearance;
import org.violetlib.vappearances.VAppearances;

import static org.violetlib.aqua.OSXSystemProperties.OSVersion;

/**
 * All methods must be called on the UI event thread.
 */

public class AquaAppearances {
    private static final @NotNull Map<String,AquaAppearance> appearances = new HashMap<>();
    private static final @NotNull String defaultAppearanceName = "NSAppearanceNameAqua";
    public static final Object APPEARANCE_CHANGE_TYPE = "AppearanceChange";

    static {
        VAppearances.addChangeListener(AquaAppearances::appearanceChanged);
    }

    /**
     * Return the current appearance with the specified name.
     * @param appearanceName The appearance name.
     * @return the appearance, or the default appearance if an appearance with this name is not available.
     * @throws UnsupportedOperationException if this appearance and the default appearance are not available.
     */

    public static @NotNull AquaAppearance get(@NotNull String appearanceName) throws UnsupportedOperationException {
        AquaAppearance appearance = getOptional(appearanceName);
        return appearance != null ? appearance : getDefaultAppearance();
    }

    /**
     * Return the current appearance with the specified name, if available.
     * @param appearanceName The appearance name.
     * @return the appearance, or null if an appearance with this name is not available.
     */

    public static @Nullable AquaAppearance getOptional(@NotNull String appearanceName) {
        AquaAppearance appearance = appearances.get(appearanceName);
        if (appearance == null) {
            try {
                VAppearance a = VAppearances.getAppearance(appearanceName);
                return getAquaAppearance(a);
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

    public static @NotNull AquaAppearance getDefaultAppearance() throws UnsupportedOperationException {
        AquaAppearance appearance = appearances.get(defaultAppearanceName);
        if (appearance == null) {
            try {
                VAppearance a = VAppearances.getAppearance(defaultAppearanceName);
                appearance = getAquaAppearance(a);
            } catch (IOException ex) {
                AquaUtils.syslog("Unable to get " + defaultAppearanceName + ": " + ex.getMessage());
                ex.printStackTrace();
                throw new UnsupportedOperationException("Default appearance " + defaultAppearanceName + " is not available");
            }
        }
        return appearance;
    }

    /**
     * Return the vibrant appearance corresponding to the specified appearance.
     * @param a The specified appearance.
     * @return the vibrant appearance corresponding to {@code a}.
     */

    public static @NotNull AquaAppearance getVibrantAppearance(@NotNull AquaAppearance a) {
        String name = a.getName();
        if (name.contains("Vibrant")) {
            return a;
        }
        if (name.contains("Dark")) {
            return get(VAppearances.vibrantDarkAppearance);
        }
        return get(VAppearances.vibrantLightAppearance);
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

    private static void appearanceChanged(@NotNull ChangeEvent ev) {
        if (ev instanceof VAppearances.AppearanceChangeEvent) {
            VAppearances.AppearanceChangeEvent ace = (VAppearances.AppearanceChangeEvent) ev;
            VAppearance a = ace.getAppearance();
            String name = a.getName();
            if (false) {
                Utils.logDebug("AquaAppearances: appearance " + name + " updated");
            }
            try {
                AquaNativeRendering.invalidateAppearances();
            } catch (Throwable ex) {
                // Must be an older release
            }
            AquaAppearance appearance = getAquaAppearance(a);
            SwingUtilities.invokeLater(() -> {
                SystemPropertyChangeManager.notifyChange(APPEARANCE_CHANGE_TYPE);
            });
        } else {
            throw new RuntimeException("Unexpected change event: " + ev);
        }
    }

    private static @NotNull AquaAppearance getAquaAppearance(@NotNull VAppearance a) {
        Map<String,Color> nativeColors = AquaNativeRendering.createPainter().getColors(a);
        Colors colors = new AppearanceColorsBuilder(a, OSVersion, nativeColors, null, Utils::logDebug).getResult();
        AquaAppearance appearance = new AquaAppearance(a, colors, Utils::logDebug);
        appearances.put(a.getName(), appearance);
        return appearance;
    }
}
