/*
 * Copyright (c) 2018-2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

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
    private static final @NotNull List<ChangeListener> changeListeners = new ArrayList<>();

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
     * Add a change listener to be called if the system appearance has changed or the colors associated with the
     * existing system appearance may have changed.
     */

    public static void addChangeListener(@NotNull ChangeListener listener) {
        changeListeners.add(listener);
    }

    public static void removeChangeListener(@NotNull ChangeListener listener) {
        changeListeners.remove(listener);
    }

    private static void appearanceChanged(@NotNull ChangeEvent ev) {
        if (ev instanceof VAppearances.AppearanceChangeEvent) {
            VAppearances.AppearanceChangeEvent ace = (VAppearances.AppearanceChangeEvent) ev;
            VAppearance a = ace.getAppearance();
            String name = a.getName();
            if (false) {
                AquaUtils.logDebug("AquaAppearances: appearance " + name + " updated");
            }
            AquaAppearance appearance = getAquaAppearance(a);
            for (ChangeListener listener : changeListeners) {
                listener.stateChanged(ev);
            }
        } else {
            throw new RuntimeException("Unexpected change event: " + ev);
        }
    }

    private static @NotNull AquaAppearance getAquaAppearance(@NotNull VAppearance a) {
        Map<String,Color> nativeColors = AquaNativeRendering.createPainter().getColors(a);
        Colors colors = new AppearanceColorsBuilder(a, OSVersion, nativeColors, null, AquaUtils::logDebug).getResult();
        AquaAppearance appearance = new AquaAppearance(a, colors, AquaUtils::logDebug);
        appearances.put(a.getName(), appearance);
        return appearance;
    }
}
