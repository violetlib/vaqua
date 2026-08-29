/*
 * Copyright (c) 2018-2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;

import org.jetbrains.annotations.*;
import org.violetlib.vappearances.AppearanceSettings;
import org.violetlib.vappearances.VAppearance;

/**
 * An object representing a specific appearance. It provides access to the colors based on the current appearance
 * settings.
 */

public class AquaAppearance
{
    private final @NotNull VAppearance appearance;
    private final @NotNull Logger log;

    public AquaAppearance(@NotNull VAppearance appearance, @NotNull Logger log)
    {
        this.appearance = appearance;
        this.log = log;
    }

    public @NotNull VAppearance getBase()
    {
        return appearance;
    }

    public @NotNull String getName()
    {
        return appearance.getName();
    }

    public boolean isDark()
    {
        return appearance.isDark();
    }

    public boolean isHighContrast()
    {
        return appearance.isHighContrast();
    }

    public boolean isTinted()
    {
        try {
            return appearance.isTinted();
        } catch (NoSuchMethodError e) {
            return false;
        }
    }

    public @NotNull AppearanceSettings getSettings()
    {
        return appearance.getSettings();
    }

    /**
     * Return the color with the specified name.
     * @param colorName The color name.
     * @return the color, as a ColorUIResource, or null if the color name not defined in this appearance.
     */

    public @Nullable Color getColor(@NotNull String colorName)
    {
        Colors colors = AquaAppearances.getColorsForAppearance(appearance);
        Color color = colors.get(colorName);
        if (AquaColors.isDebugging()) {
            log.log("  Color " + colorName + ": " + AquaColors.toString(color));
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

    public @Nullable Color getColorForEffect(@NotNull String colorName, @NotNull EffectName effectName)
    {
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

    public @Nullable Color getColorForOptionalEffect(@NotNull String colorName, @NotNull EffectName effectName)
    {
        if (effectName == EffectName.EFFECT_NONE) {
            return getColor(colorName);
        }

        String extendedName = colorName + "_" + effectName;
        Color c = getColor(extendedName);
        return c != null ? c : getColor(colorName);
    }

    public @NotNull VAppearance getAppearance()
    {
        return appearance;
    }

    @Override
    public @NotNull String toString()
    {
        return super.toString() + "[" + appearance.getName() + "]";
    }
}
