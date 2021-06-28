/*
 * Copyright (c) 2018-2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.util.HashMap;
import java.util.Map;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.jnr.aqua.AquaNativeRendering;
import org.violetlib.vappearances.VAppearance;

/**
 * Build a set of color definitions for an appearance.
 */

public class AppearanceColorsBuilder {

    private final @Nullable ColorsInstrumentation instrumentation;
    private final @NotNull Colors result;

    public AppearanceColorsBuilder(
            @NotNull VAppearance appearance,
            int OSVersion,
            @Nullable Map<String,Color> nativeColors,
            @Nullable ColorsInstrumentation instrumentation,
            @Nullable Logger log) {

        this.instrumentation = instrumentation;

        if (log == null) {
            log = Utils::logDebug;
        }

        if (nativeColors == null) {
            nativeColors = AquaNativeRendering.createPainter().getColors(appearance);
        }

        String appearanceName = appearance.getName();
        SystemColors systemColors = getSystemColors(OSVersion, log);
        ColorsBuilder colors = new ColorsBuilder(appearanceName, instrumentation, log);
        colors.add(systemColors.defaultColors, "Default");
        colors.addAll(appearance.getColors());
        colors.addAll(nativeColors);
        colors.add("controlText_disabled", "disabledControlText");

        if (appearance.isDark()) {
            colors.add(systemColors.darkColors, "Dark");
            if (appearance.isHighContrast()) {
                colors.add(systemColors.highContrastDarkColors, "High Contrast Dark");
            }
        } else {
            colors.add(systemColors.lightColors, "Light");
            if (appearance.isHighContrast()) {
                colors.add(systemColors.highContrastLightColors, "High Contrast Light");
            }
        }
        installFixups(colors, appearance, OSVersion);
        result = colors.getColors();
        if (instrumentation != null) {
            instrumentation.colorsReady(new MyAccess(result));
        }
    }

    private static class MyAccess
      implements ColorsInstrumentation.Access
    {
        private final @NotNull Colors colors;

        public MyAccess(@NotNull Colors colors) {
            this.colors = colors;
        }

        @Override
        public @NotNull String[] getColorNames() {
            return colors.getColorNames().toArray(new String[0]);
        }

        @Override
        public @Nullable Color getColor(@NotNull String name) {
            return colors.get(name);
        }

        @Override
        public @Nullable String getSynonym(@NotNull String name) {
            return null;
        }
    }

    public @NotNull Colors getResult() {
        return result;
    }

    // This map supports testing, not needed in production!

    private static final Map<Integer,SystemColors> systemColorsMap = new HashMap<>();

    private @NotNull SystemColors getSystemColors(int OSVersion, @NotNull Logger log) {
        return systemColorsMap.computeIfAbsent(OSVersion, (v) -> new SystemColors(v, instrumentation, log));
    }

    // Fixups are alterations that depend on existing definitions being present.

    private void installFixups(@NotNull ColorsBuilder colors, @NotNull VAppearance appearance, int OSVersion) {

        if (OSVersion < 1014) {
            // Some appearance based colors prior to 10.14 are not represented as system colors.
            // These colors are not used in 10.14 or later.
            Color c = colors.get("controlAccent");
            boolean isGraphite = c != null && (c.getBlue() - c.getRed()) < 30;
            if (isGraphite) {
                colors.add("menuSelectedBackground", 162, 162, 168);
                colors.add("menuBackground", 240);
            } else {
                colors.add("menuSelectedBackground", 54, 148, 253);
                colors.add("menuBackground", 0, 0);
            }
            colors.add("menuForeground", 61);
        }

        if (appearance.isDark()) {
            // A workaround for an apparently bogus color:
            {
                Color c = colors.get("alternatingContentBackground_1_disabled");
                if (c != null && c.getRed() == 255 && c.getAlpha() == 128) {
                    colors.add("alternatingContentBackground_1_disabled", 128, 13);
                }
            }
        }
    }
}
