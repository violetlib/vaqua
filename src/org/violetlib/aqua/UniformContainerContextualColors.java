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

 */

public class UniformContainerContextualColors implements ContainerContextualColors {

    protected @NotNull ContextualColor background;
    protected @NotNull ContextualColor foreground;
    protected @NotNull ContextualColor grid;
    protected boolean isRowSelected;

    /**
     * Create colors for a uniform (not striped) container.
     */

    public UniformContainerContextualColors(@NotNull ContextualColor background,
                                            @NotNull ContextualColor foreground,
                                            @NotNull ContextualColor grid) {
        this.background = background;
        this.foreground = foreground;
        this.grid = grid;
    }

    public void configureForContainer() {
        isRowSelected = false;
    }

    public void configureForRow(int rowIndex, boolean isRowSelected) {
        this.isRowSelected = isRowSelected;
    }

    public void configureForRow(boolean isRowSelected) {
        this.isRowSelected = isRowSelected;
    }

    public boolean isStriped() {
        return false;
    }

    @Override
    public @NotNull Color getBackground(@NotNull AppearanceContext context) {
        context = context.withSelected(isRowSelected || context.isSelected());
        return background.get(context);
    }

    @Override
    public @NotNull Color getForeground(@NotNull AppearanceContext context) {
        context = context.withSelected(isRowSelected || context.isSelected());
        return foreground.get(context);
    }

    public @NotNull Color getGrid(@NotNull AppearanceContext context) {
        context = context.withSelected(isRowSelected || context.isSelected());
        return grid.get(context);
    }
}
