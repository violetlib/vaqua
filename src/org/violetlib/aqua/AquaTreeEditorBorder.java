/*
 * Copyright (c) 2021-2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;

import org.jetbrains.annotations.*;

/**
 * The default border for a tree cell editor.
 */
public final class AquaTreeEditorBorder
  extends AquaBorder
  implements AquaBackgroundBorder
{
    public AquaTreeEditorBorder()
    {
    }

    @Override
    public void paintBackground(@NotNull Component c, Graphics g, @Nullable Color background)
    {
        Color bc = getBackground(c);
        if (bc != null) {
            int width = c.getWidth();
            int height = c.getHeight();
            g.setColor(bc);
            g.fillRect(0, 0, width, height);
        }
    }

    private @Nullable Color getBackground(@NotNull Component c)
    {
        JTree tree = getTree(c);
        if (tree != null) {
            AquaTreeUI ui = AquaUtils.getUI(tree, AquaTreeUI.class);
            if (ui != null) {
                AquaAppearance a = AppearanceManager.getAppearance(tree);
                if (a.isDark()) {
                    Color b = AquaColors.getCellEditorBackground(tree);
                    if (b != null) {
                        return b;
                    }
                }
            }
        }
        return c.getBackground();
    }

    private @Nullable JTree getTree(@NotNull Component c)
    {
        Container parent = c.getParent();
        if (parent instanceof JTree) {
            return (JTree) parent;
        }
        if (parent == null) {
            return null;
        }
        parent = parent.getParent();
        return parent instanceof JTree ? (JTree) parent : null;
    }

    @Override
    protected void paint(@NotNull JComponent c, @NotNull Graphics2D g, int x, int y, int width, int height)
    {
    }

    @Override
    public @NotNull Insets getBorderInsets(@NotNull Component c)
    {
        return new Insets(0, 0, 0, 0);
    }
}
