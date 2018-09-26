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
 * Identify contextually determined foreground and background colors.
 */

public class BasicContextualColorsImpl implements BasicContextualColors {

    protected @NotNull ContextualColor background;
    protected @NotNull ContextualColor foreground;

    public BasicContextualColorsImpl(@NotNull ContextualColor background,
                                     @NotNull ContextualColor foreground) {
        this.background = background;
        this.foreground = foreground;
    }

    @Override
    public @NotNull Color getBackground(@NotNull AppearanceContext context) {
        AquaColors.setupDebugging(this);
        Color color = background.get(context);
        AquaColors.clearDebugging();
        return color;
    }

    @Override
    public @NotNull Color getForeground(@NotNull AppearanceContext context) {
        AquaColors.setupDebugging(this);
        Color color = foreground.get(context);
        AquaColors.clearDebugging();
        return color;
    }
}
