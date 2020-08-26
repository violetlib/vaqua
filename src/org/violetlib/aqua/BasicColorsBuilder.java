/*
 * Copyright (c) 2018-2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.util.List;
import java.util.*;
import javax.swing.plaf.ColorUIResource;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Construct a collection of color definitions. Color names are defined in terms of specific colors or as synonyms.
 */

public class BasicColorsBuilder {

    public static final int NO_INACTIVE = 1 << 0;  // colors do not change when inactive

    private static final List<String> allColorSuffixes = Collections.unmodifiableList(Arrays.asList(
            "_rollover",
            "_pressed",
            "_inactive",
            "_disabled",
            "_inactive_disabled",
            "_focused"
    ));

    public final @NotNull Map<String,Color> colors = new HashMap<>();
    public final @NotNull Map<String,String> synonyms = new HashMap<>();
    protected final @NotNull Logger log;

    public BasicColorsBuilder(@NotNull Logger log) {
        this.log = log;
    }

    public static @NotNull List<String> getAllColorSuffixes()
    {
        return allColorSuffixes;
    }

    public static @Nullable String withoutInactive(@NotNull String suffix)
    {
        if (suffix.equals("_inactive")) {
            return "";
        }
        if (suffix.equals("_inactive_disabled")) {
            return "_disabled";
        }
        return null;
    }

    public @NotNull BasicColors get() {
        Map<String,Color> cs = Collections.unmodifiableMap(new HashMap<>(colors));
        Map<String,String> ss = Collections.unmodifiableMap(new HashMap<>(synonyms));
        return new BasicColors(cs, ss);
    }

    protected void internalAdd(@NotNull String name, @NotNull Color color) {
        synonyms.remove(name);
        if (!(color instanceof ColorUIResource)) {
            color = new ColorUIResource(color);
        }
        colors.put(name, color);
    }

    public void add(@NotNull String name, int color) {
        Color c = new ColorUIResource(color, color, color);
        internalAdd(name, c);
    }

    public void add(@NotNull String name, int red, int green, int blue) {
        Color c = new ColorUIResource(red, green, blue);
        internalAdd(name, c);
    }

    public void add(@NotNull String name, int red, int green, int blue, int alpha) {
        Color c = new ColorUIResource(new Color(red, green, blue, alpha));
        internalAdd(name, c);
    }

    public void add(@NotNull String name, int intensity, int alpha) {
        Color c = new ColorUIResource(new Color(intensity, intensity, intensity, alpha));
        internalAdd(name, c);
    }

    public void addColorGradient(@NotNull String name, int start, int finish, int alpha) {
        Color startColor = new Color(start, start, start, alpha);
        Color finishColor = new Color(finish, finish, finish, alpha);
        Color gradientColor = new GradientColor(startColor, finishColor, log);
        internalAdd(name, gradientColor);
    }

    public void addMagicColorGradient(@NotNull String name, int start, int finish, int alpha) {
        Color startColor = new Color(start, start, start, alpha);
        Color finishColor = new Color(finish, finish, finish, alpha);
        Color gradientColor = new GradientColor(startColor, finishColor, true, log);
        internalAdd(name, gradientColor);
    }

    public void addAlphaGradient(@NotNull String name, int intensity, int startAlpha, int finishAlpha) {
        Color startColor = new Color(intensity, intensity, intensity, startAlpha);
        Color finishColor = new Color(intensity, intensity, intensity, finishAlpha);
        Color gradientColor = new GradientColor(startColor, finishColor, log);
        internalAdd(name, gradientColor);
    }

    public void addMagicAlphaGradient(@NotNull String name, int intensity, int startAlpha, int finishAlpha) {
        Color startColor = new Color(intensity, intensity, intensity, startAlpha);
        Color finishColor = new Color(intensity, intensity, intensity, finishAlpha);
        Color gradientColor = new GradientColor(startColor, finishColor, true, log);
        internalAdd(name, gradientColor);
    }

    public void add(@NotNull String name, @NotNull Color color) {
        internalAdd(name, color);
    }

    public void add(@NotNull String name, @NotNull String synonym) {
        internalAdd(name, synonym);
    }

    public void addAll(@NotNull String root, @NotNull String synonymRoot) {
        add(root, synonymRoot);
        for (String suffix : getAllColorSuffixes()) {
            addDerived(root, synonymRoot, suffix);
        }
        String selectedRoot = AquaColors.createSelectedColorName(root);
        String selectedSynonymRoot = AquaColors.createSelectedColorName(synonymRoot);
        add(selectedRoot, selectedSynonymRoot);
        for (String suffix : getAllColorSuffixes()) {
            addDerived(selectedRoot, selectedSynonymRoot, suffix);
        }
    }

    private void addDerived(@NotNull String root, @NotNull String synonymRoot, @NotNull String suffix) {
        add(root + suffix, synonymRoot + suffix);
    }

    public void addAll(@NotNull String root, @NotNull String synonymRoot, int option) {
        add(root, synonymRoot);
        for (String suffix : getAllColorSuffixes()) {
            addDerived(root, synonymRoot, suffix, option);
        }
        String selectedRoot = AquaColors.createSelectedColorName(root);
        String selectedSynonymRoot = AquaColors.createSelectedColorName(synonymRoot);
        add(selectedRoot, selectedSynonymRoot);
        for (String suffix : getAllColorSuffixes()) {
            addDerived(selectedRoot, selectedSynonymRoot, suffix, option);
        }
    }

    private void addDerived(@NotNull String root, @NotNull String synonymRoot, @NotNull String suffix, int option) {
        if ((option & NO_INACTIVE) != 0) {
            // define inactive variants in terms of the non-inactive variant
            String nonInactiveSuffix = withoutInactive(suffix);
            if (nonInactiveSuffix != null) {
                add(root + suffix, root + nonInactiveSuffix);
                return;
            }
        }

        add(root + suffix, synonymRoot + suffix);
    }

    public void defineNoInactive(@NotNull String root) {
        // define inactive variants in terms of the non-inactive variant
        for (String suffix : getAllColorSuffixes()) {
            String nonInactiveSuffix = withoutInactive(suffix);
            if (nonInactiveSuffix != null) {
                add(root + suffix, root + nonInactiveSuffix);
            }
        }

        String selectedRoot = AquaColors.createSelectedColorName(root);
        for (String suffix : getAllColorSuffixes()) {
            String nonInactiveSuffix = withoutInactive(suffix);
            if (nonInactiveSuffix != null) {
                add(selectedRoot + suffix, selectedRoot + nonInactiveSuffix);
            }
        }
    }

    protected void internalAdd(@NotNull String name, @NotNull String synonym) {
        colors.remove(name);
        synonyms.put(name, synonym);
    }

    public void remove(@NotNull String name) {
        colors.remove(name);
    }
}
