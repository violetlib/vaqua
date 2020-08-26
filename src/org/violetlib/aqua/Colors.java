/*
 * Copyright (c) 2018-2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import javax.swing.plaf.ColorUIResource;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 *
 */
public class Colors {

    public static final int NO_INACTIVE = 1 << 0;  // colors do not change when inactive

    public final @NotNull Map<String, Color> colors = new HashMap<>();
    public final @NotNull Map<String, String> synonyms = new HashMap<>();
    protected final @NotNull Logger log;

    public Colors(@NotNull Logger log) {
        this.log = log;
    }

    /**
     * This method is for final consumption only.
     */
    public @NotNull Map<String, Color> getColors() {
        applySynonyms(synonyms);
        return Collections.unmodifiableMap(colors);
    }

    protected void applySynonyms(@NotNull Map<String, String> synonyms) {
        // The goal is to support (short) chains of synonyms without risk of infinite loop

        Map<String, Color> results = new HashMap<>();

        for (String name : synonyms.keySet()) {
            Color c = getIndirect(name, 5);
            if (c != null) {
                results.put(name, c);
            } else if (false) {
                String s = getIndirectPath(name, 5);
                log.log("Color " + name + " has no indirect definition: " + s);
            }
        }

        for (String name : results.keySet()) {
            Color c = results.get(name);
            colors.put(name, c);
        }
    }

    protected @Nullable Color getIndirect(@NotNull String name, int limit) {
        if (limit < 0) {
            return null;
        }
        String nextName = synonyms.get(name);
        if (nextName != null) {
            return getIndirect(nextName, limit - 1);
        }

        return colors.get(name);
    }

    protected @NotNull String getIndirectPath(@NotNull String name, int limit) {
        if (limit < 0) {
            return "...";
        }
        String nextName = synonyms.get(name);
        if (nextName != null) {
            return name + " " + getIndirectPath(nextName, limit - 1);
        }
        return name;
    }

    public @Nullable Color get(@NotNull String name) {
        return colors.get(name);
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

    public void addAll(@NotNull Map<String, Color> cs) {
        for (String name : cs.keySet()) {
            Color c = cs.get(name);
            internalAdd(name, c);
        }
    }

    protected void internalAdd(@NotNull String name, @NotNull String synonym) {
        colors.remove(name);
        synonyms.put(name, synonym);
    }

    public void add(@NotNull String name, @NotNull String synonym) {
        internalAdd(name, synonym);
    }

    public void addAll(@NotNull String root, @NotNull String synonymRoot) {
        add(root, synonymRoot);
        for (String suffix : BasicAquaAppearance.getAllColorSuffixes()) {
            addDerived(root, synonymRoot, suffix);
        }
        String selectedRoot = BasicAquaAppearance.createSelectedColorName(root);
        String selectedSynonymRoot = BasicAquaAppearance.createSelectedColorName(synonymRoot);
        add(selectedRoot, selectedSynonymRoot);
        for (String suffix : BasicAquaAppearance.getAllColorSuffixes()) {
            addDerived(selectedRoot, selectedSynonymRoot, suffix);
        }
    }

    private void addDerived(@NotNull String root, @NotNull String synonymRoot, @NotNull String suffix) {
        add(root + suffix, synonymRoot + suffix);
    }

    public void addAll(@NotNull String root, @NotNull String synonymRoot, int option) {
        add(root, synonymRoot);
        for (String suffix : BasicAquaAppearance.getAllColorSuffixes()) {
            addDerived(root, synonymRoot, suffix, option);
        }
        String selectedRoot = BasicAquaAppearance.createSelectedColorName(root);
        String selectedSynonymRoot = BasicAquaAppearance.createSelectedColorName(synonymRoot);
        add(selectedRoot, selectedSynonymRoot);
        for (String suffix : BasicAquaAppearance.getAllColorSuffixes()) {
            addDerived(selectedRoot, selectedSynonymRoot, suffix, option);
        }
    }

    private void addDerived(@NotNull String root, @NotNull String synonymRoot, @NotNull String suffix, int option) {
        if ((option & NO_INACTIVE) != 0) {
            // define inactive variants in terms of the non-inactive variant
            String nonInactiveSuffix = BasicAquaAppearance.withoutInactive(suffix);
            if (nonInactiveSuffix != null) {
                add(root + suffix, root + nonInactiveSuffix);
                return;
            }
        }

        add(root + suffix, synonymRoot + suffix);
    }

    public void defineNoInactive(@NotNull String root) {
        // define inactive variants in terms of the non-inactive variant
        for (String suffix : BasicAquaAppearance.getAllColorSuffixes()) {
            String nonInactiveSuffix = BasicAquaAppearance.withoutInactive(suffix);
            if (nonInactiveSuffix != null) {
                add(root + suffix, root + nonInactiveSuffix);
            }
        }

        String selectedRoot = BasicAquaAppearance.createSelectedColorName(root);
        for (String suffix : BasicAquaAppearance.getAllColorSuffixes()) {
            String nonInactiveSuffix = BasicAquaAppearance.withoutInactive(suffix);
            if (nonInactiveSuffix != null) {
                add(selectedRoot + suffix, selectedRoot + nonInactiveSuffix);
            }
        }
    }

    public void remove(@NotNull String name) {
        colors.remove(name);
    }

    public void add(@NotNull Colors cs) {
        Map<String, Color> colorsToAdd = cs.colors;
        for (String name : colorsToAdd.keySet()) {
            Color c = colorsToAdd.get(name);
            internalAdd(name, c);
        }
        Map<String, String> synonymsToAdd = cs.synonyms;
        for (String name : synonymsToAdd.keySet()) {
            String ref = synonymsToAdd.get(name);
            internalAdd(name, ref);
        }
    }
}
