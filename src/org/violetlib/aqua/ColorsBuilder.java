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
 * Build a set of color definitions by combining basic sets of color definitions.
 */

public class ColorsBuilder {

    private final @NotNull Map<String,Color> colors = new HashMap<>();
    private final @NotNull Map<String,String> synonyms = new HashMap<>();
    protected final @NotNull Logger log;

    public ColorsBuilder(@NotNull Logger log) {
        this.log = log;
    }

    public @NotNull Colors getColors() {
        Map<String,Color> result = new HashMap<>(colors);
        applySynonyms(result, synonyms);
        return new Colors(Collections.unmodifiableMap(result));
    }

    protected void applySynonyms(@NotNull Map<String,Color> results, @NotNull Map<String, String> synonyms) {
        // The goal is to support (short) chains of synonyms without risk of infinite loop

        Map<String,Color> definitions = new HashMap<>();
        for (String name : synonyms.keySet()) {
            Color c = getIndirect(name, 5);
            if (c != null) {
                definitions.put(name, c);
            }
        }

        for (String name : definitions.keySet()) {
            Color c = definitions.get(name);
            results.put(name, c);
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
        for (String suffix : BasicColorsBuilder.getAllColorSuffixes()) {
            addDerived(root, synonymRoot, suffix);
        }
        String selectedRoot = AquaColors.createSelectedColorName(root);
        String selectedSynonymRoot = AquaColors.createSelectedColorName(synonymRoot);
        add(selectedRoot, selectedSynonymRoot);
        for (String suffix : BasicColorsBuilder.getAllColorSuffixes()) {
            addDerived(selectedRoot, selectedSynonymRoot, suffix);
        }
    }

    private void addDerived(@NotNull String root, @NotNull String synonymRoot, @NotNull String suffix) {
        add(root + suffix, synonymRoot + suffix);
    }

    public void add(@NotNull BasicColors cs) {
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
