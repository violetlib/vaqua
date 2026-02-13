/*
 * Copyright (c) 2020-2026 Alan Snyder.
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

import org.jetbrains.annotations.*;

/**
 * A context dependent border for list cells.
 */

@SuppressWarnings("serial")
public class AquaCellBorder extends AbstractBorder implements UIResource {

    public static final @NotNull String RENDERER_CONTAINER_KEY = "JComponent.rendererContainer";

    public Insets getBorderInsets(@NotNull Component c, @NotNull Insets insets) {
        AquaViewStyleContainerUI ui = getContainerUI((JComponent) c);
        if (ui != null) {
            return ui.getContentInsets();
        }
        insets.top = insets.bottom = 1;
        insets.left = insets.right = 1;
        return insets;
    }

    protected @Nullable AquaViewStyleContainerUI getContainerUI(@NotNull JComponent c)
    {
        Container parent = c.getParent();
        if (parent instanceof CellRendererPane) {
            parent = parent.getParent();
        }
        AquaViewStyleContainerUI ui = AquaUtils.getUI(parent, AquaViewStyleContainerUI.class);
        if (ui != null) {
            return ui;
        }
        Object o = c.getClientProperty(RENDERER_CONTAINER_KEY);
        if (o instanceof Component) {
            return AquaUtils.getUI((Component) o, AquaViewStyleContainerUI.class);
        }
        return null;
    }
}
