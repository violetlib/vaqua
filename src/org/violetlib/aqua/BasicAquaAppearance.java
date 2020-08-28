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

public class BasicAquaAppearance implements VAppearance {

    private final @NotNull VAppearance appearance;
    private final @NotNull Colors colors;
    private final @NotNull Logger log;

    public BasicAquaAppearance(@NotNull VAppearance appearance,
                               @NotNull Colors colors,
                               @NotNull Logger log) {
        this.appearance = appearance;
        this.colors = colors;
        this.log = log;
    }

    @Override
    public @NotNull String getName() {
        return appearance.getName();
    }

    @Override
    public boolean isDark() {
        return appearance.isDark();
    }

    @Override
    public boolean isHighContrast() {
        return appearance.isHighContrast();
    }

    @Override
    public @NotNull Map<String,Color> getColors() {
        return appearance.getColors();
    }

    /**
     * Return the color with the specified name.
     * @param colorName The color name.
     * @return the color, as a ColorUIResource, or null if the color name not defined in this appearance.
     */

    public @Nullable Color getColor(@NotNull String colorName) {
        return colors.get(colorName);
    }

    public boolean isBasedOn(@NotNull VAppearance va) {
        return va == appearance;
    }

    @Override
    public @NotNull String toString() {
        return super.toString() + "[" + appearance.getName() + "]";
    }
}
