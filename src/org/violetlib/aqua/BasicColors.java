/*
 * Copyright (c) 2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.util.Map;

import org.jetbrains.annotations.NotNull;

/**
 * A collection of color definitions. Color names are defined in terms of specific colors or as synonyms.
 */

public class BasicColors {
    public final @NotNull Map<String,Color> colors;
    public final @NotNull Map<String,String> synonyms;

    public BasicColors(@NotNull Map<String,Color> colors, @NotNull Map<String,String> synonyms) {
        this.colors = colors;
        this.synonyms = synonyms;
    }
}
