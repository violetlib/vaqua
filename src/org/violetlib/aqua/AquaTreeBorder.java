/*
 * Copyright (c) 2020-2025 Alan Snyder.
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
 * A context dependent border for trees that supports side margins when the inset view style is in use.
 */
@SuppressWarnings("serial")
public class AquaTreeBorder extends AbstractBorder implements UIResource {

    public static final @NotNull String RENDERER_CONTAINER_KEY = "JComponent.rendererContainer";

    @Override
    public Insets getBorderInsets(@NotNull Component c, @NotNull Insets insets) {
        AquaTreeUI ui = getTreeUI((JComponent) c);
        if (ui != null) {
            return ui.getInsets();
        }
        insets.top = 0;
        insets.bottom = 0;
        insets.left = 1;
        insets.right = 1;
        return insets;
    }

    private @Nullable AquaTreeUI getTreeUI(@NotNull JComponent jc)
    {
        AquaTreeUI ui = AquaUtils.getUI(jc, AquaTreeUI.class);
        if (ui != null) {
            return ui;
        }
        Object o = jc.getClientProperty(RENDERER_CONTAINER_KEY);
        if (o instanceof Component) {
            return AquaUtils.getUI((Component) o, AquaTreeUI.class);
        }
        return null;
    }
}
