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
import java.util.Set;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * A collection of color definitions.
 */

public class Colors {
    private final @NotNull Map<String,Color> colors;

    public Colors(@NotNull Map<String,Color> colors) {
        this.colors = colors;
    }

    public @Nullable Color get(@NotNull String name) {
        return colors.get(name);
    }

    public @NotNull Set<String> getColorNames() {
        return colors.keySet();
    }
}
