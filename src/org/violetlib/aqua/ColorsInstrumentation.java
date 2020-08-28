/*
 * Copyright (c) 2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 *
 */

public interface ColorsInstrumentation {

    interface Access {
        @NotNull String[] getColorNames();
        @Nullable Color getColor(@NotNull String name);
        @Nullable String getSynonym(@NotNull String name);
    }

    void addingColor(@NotNull String context, @NotNull String name, @NotNull Color color, @NotNull Access access);

    void addingSynonym(@NotNull String context, @NotNull String name, @NotNull String synonym, @NotNull Access access);

    void removingColor(@NotNull String context, @NotNull String name, @NotNull Access access);

    void applyingSynonym(@NotNull String context, @NotNull String name, @Nullable Color color, @NotNull Access access);

    void colorsReady(@NotNull Access access);
}
