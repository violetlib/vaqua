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
 * A context dependent border for trees that supports side margins when the inset view style is in use.
 */
@SuppressWarnings("serial")
public class AquaTreeBorder extends AbstractBorder implements UIResource {

    public Insets getBorderInsets(@NotNull Component c, @NotNull Insets insets) {
        AquaTreeUI ui = AquaUtils.getUI((JComponent) c, AquaTreeUI.class);
        if (ui != null) {
            return ui.getInsets();
        }
        insets.top = 0;
        insets.bottom = 0;
        insets.left = 1;
        insets.right = 1;
        return insets;
    }
}
