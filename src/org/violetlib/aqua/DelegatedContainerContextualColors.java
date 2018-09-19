/*
 * Copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;

import javax.swing.plaf.ColorUIResource;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**

 */

public class DelegatedContainerContextualColors implements ContainerContextualColors {

    protected @NotNull BasicContextualColors source;
    protected @Nullable ContextualColor grid;
    protected boolean isRowSelected;

    /**
     * Create colors for a uniform (not striped) container.
     */

    public DelegatedContainerContextualColors(@NotNull BasicContextualColors source) {
        this.source = source;
        this.grid = null;
    }

    /**
     * Create colors for a uniform (not striped) container.
     */

    public DelegatedContainerContextualColors(@NotNull BasicContextualColors source,
                                              @NotNull ContextualColor grid) {
        this.source = source;
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
        return source.getBackground(context);
    }

    @Override
    public @NotNull Color getForeground(@NotNull AppearanceContext context) {
        context = context.withSelected(isRowSelected || context.isSelected());
        return source.getForeground(context);
    }

    public @NotNull Color getGrid(@NotNull AppearanceContext context) {
        if (grid != null) {
            context = context.withSelected(isRowSelected || context.isSelected());
            return grid.get(context);
        } else {
            throw new UnsupportedOperationException("No grid color has been defined");
        }
    }
}
