/*
 * Copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;

import org.jetbrains.annotations.NotNull;

/**
 * A contextual color is a source of colors based on context.
 */

public interface ContextualColor {

    /**
     * Return the name of this contextual color (for debugging).
     */

    @NotNull String getColorName();

    /**
     * Return the color to use in a specified context.
     * @param context The context.
     * @return the color.
     * @throws UnsupportedOperationException if no color is defined.
     */

    @NotNull Color get(@NotNull AppearanceContext context);
}
