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

public class StripedContainerContextualColors implements ContainerContextualColors {

    protected @NotNull ContextualColor containerBackground;
    protected @NotNull ContextualColor evenRowBackground;
    protected @NotNull ContextualColor oddRowBackground;
    protected @NotNull ContextualColor grid;
    protected boolean isRowSelected;
    protected @NotNull ContextualColor background;
    protected @NotNull ContextualColor foreground;

    /**
     * Create colors for a striped container.
     */
    public StripedContainerContextualColors(@NotNull ContextualColor containerBackground,
                                            @NotNull ContextualColor evenRowBackground,
                                            @NotNull ContextualColor oddRowBackground,
                                            @NotNull ContextualColor foreground,
                                            @NotNull ContextualColor grid) {
        this.containerBackground = containerBackground;
        this.evenRowBackground = evenRowBackground;
        this.oddRowBackground = oddRowBackground;
        this.grid = grid;
        this.background = containerBackground;
        this.foreground = foreground;
    }

    public void configureForContainer() {
        isRowSelected = false;
        this.background = containerBackground;
    }

    public void configureForRow(int rowIndex, boolean isRowSelected) {
        this.isRowSelected = isRowSelected;
        this.background = rowIndex % 2 == 0 ? evenRowBackground : oddRowBackground;
    }

    public void configureForRow(boolean isRowSelected) {
        this.isRowSelected = isRowSelected;
        this.background = containerBackground;
    }

    public boolean isStriped() {
        return true;
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
