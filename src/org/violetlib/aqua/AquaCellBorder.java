/*
 * Copyright (c) 2020-2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;
import javax.swing.border.AbstractBorder;
import javax.swing.plaf.UIResource;

import org.jetbrains.annotations.NotNull;

/**
 * A context dependent border for list cells.
 */
@SuppressWarnings("serial")
public class AquaCellBorder extends AbstractBorder implements UIResource {

    public static final @NotNull String RENDERER_CONTAINER_KEY = "JComponent.rendererContainer";

    public Insets getBorderInsets(@NotNull Component c, @NotNull Insets insets) {
        boolean isInset = computeIsInset((JComponent)c);
        int top = isInset ? 5 : 1;
        int side = isInset ? 20 : 1;
        insets.top = insets.bottom = top;
        insets.left = insets.right = side;
        return insets;
    }

    private boolean computeIsInset(@NotNull JComponent c) {
        Container parent = c.getParent();
        if (parent instanceof CellRendererPane) {
            parent = parent.getParent();
        }

        AquaViewStyleContainerUI ui = AquaUtils.getUI((JComponent) parent, AquaViewStyleContainerUI.class);
        if (ui != null) {
            return ui.isInset();
        }

        Object o = c.getClientProperty(RENDERER_CONTAINER_KEY);
        if (o instanceof JComponent) {
            ui = AquaUtils.getUI((JComponent) o, AquaViewStyleContainerUI.class);
            if (ui != null) {
                return ui.isInset();
            }
        }

        return false;
    }
}
