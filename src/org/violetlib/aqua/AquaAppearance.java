/*
 * Copyright (c) 2018-2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.util.Map;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.vappearances.VAppearance;

/**
 * An object representing a specific appearance, including the current accent and highlight colors.
 */

public class AquaAppearance extends BasicAquaAppearance {

    public AquaAppearance(@NotNull VAppearance appearance, int OSVersion, @NotNull Map<String,Color> nativeColors) {
        super(appearance, OSVersion, nativeColors);
    }

    /**
     * Return the color with the specified name.
     * @param colorName The color name.
     * @return the color, as a ColorUIResource, or null if the color name not defined in this appearance.
     */

    public @Nullable Color getColor(@NotNull String colorName) {
        Color color = super.getColor(colorName);
        if (AquaColors.isDebugging()) {
            AquaUtils.logDebug("  Color " + colorName + ": " + AquaColors.toString(color));
        }
        return color;
    }

    /**
     * Return a color modified for a specified effect. If no color is defined for that effect, the basic color is
     * returned.
     * @param colorName The color name.
     * @param effectName The effect name.
     * @return the color as defined by the color name and effect, or the basic color if there is no variable color
     * defined for the specified effect, or null if the color name is note defined in this appearance.
     */

    public @Nullable Color getColorForEffect(@NotNull String colorName, @NotNull EffectName effectName) {
        if (effectName == EffectName.EFFECT_NONE) {
            return getColor(colorName);
        }

        String extendedName = colorName + "_" + effectName;
        Color c = getColor(extendedName);
        if (c != null) {
            return c;
        }
        return getColor(colorName);
    }

    /**
     * Return a color modified for a specified effect. If no color is defined for that effect, the basic color is
     * returned.
     * @param colorName The color name.
     * @param effectName The effect name.
     * @return the color as defined by the color name and effect, or the basic color if there is no variable color
     * defined for the specified effect, or null if the color name is note defined in this appearance.
     */

    public @Nullable Color getColorForOptionalEffect(@NotNull String colorName, @NotNull EffectName effectName) {
        if (effectName == EffectName.EFFECT_NONE) {
            return getColor(colorName);
        }

        String extendedName = colorName + "_" + effectName;
        Color c = getColor(extendedName);
        return c != null ? c : getColor(colorName);
    }

    @Override
    protected void debug(@NotNull String message) {
        AquaUtils.logDebug(message);
    }
}
