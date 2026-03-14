/*
 * Copyright (c) 2014-2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.SeparatorUI;

import org.jetbrains.annotations.*;
import org.violetlib.jnr.aqua.AquaUIPainter;

/**
 * A customized UI for JSeparator, matching Yosemite.
 */
public class AquaSeparatorUI extends SeparatorUI implements AquaComponentUI {

    private static int thickness;

    public static ComponentUI createUI(JComponent c) {
        return new AquaSeparatorUI();
    }

    private final @NotNull BasicContextualColors colors;

    public AquaSeparatorUI() {
        colors = AquaColors.SEPARATOR_COLORS;
    }

    public void installUI(JComponent c) {
        if (thickness == 0) {
            thickness = UIManager.getInt("Separator.width");
        }
        installDefaults((JSeparator) c);
    }

    public void uninstallUI(JComponent c) {
        uninstallDefaults((JSeparator) c);
    }

    protected void installDefaults(JSeparator s) {
        LookAndFeel.installProperty(s, "opaque", false);
        installListeners(s);
    }

    protected void uninstallDefaults(JSeparator s) {
        uninstallListeners(s);
    }

    protected void installListeners(JSeparator s) {
        AppearanceManager.install(s);
    }

    protected void uninstallListeners(JSeparator s) {
        AppearanceManager.uninstall(s);
    }

    @Override
    public void update(Graphics g, JComponent c) {
        paint(g, c);
    }

    @Override
    public void paint(Graphics g, JComponent c) {
        AppearanceManager.withContext(g, c, this::paint);
    }

    public void paint(Graphics2D g, JComponent c, @NotNull PaintingContext pc) {
        AquaUIPainter.State state = AquaUIPainter.State.ACTIVE;
        AppearanceContext appearanceContext = new AppearanceContext(pc.appearance, state, false, false);
        AquaColors.installColors(c, appearanceContext, colors);
        Dimension size = c.getSize();
        Insets s = c.getInsets();
        g.setColor(c.getForeground());
        int x = s.left;
        int y = s.top;
        int w = Math.max(0, size.width - s.left - s.right);
        int h = Math.max(0, size.height - s.top - s.bottom);
        if (((JSeparator) c).getOrientation() == JSeparator.VERTICAL) {
            int w1 = Math.min(w, thickness);
            w = w1 + (w - w1) / 2;
        } else {
            int h1 = Math.min(h, thickness);
            h = h1 + (h - h1) / 2;
        }
        g.fillRect(x, y, w, h);
    }

    public Dimension getMinimumSize(JComponent c) {
        return getPreferredSize(c);
    }

    public Dimension getPreferredSize(JComponent c) {
        Insets insets = c.getInsets();
        if (((JSeparator) c).getOrientation() == JSeparator.VERTICAL) {
            return new Dimension(thickness + insets.left + insets.right, insets.top + insets.bottom);
        } else {
            return new Dimension(insets.left + insets.right, thickness + insets.top + insets.bottom);
        }
    }

    public Dimension getMaximumSize(JComponent c) {
        Dimension d = getPreferredSize(c);
        if (((JSeparator) c).getOrientation() == JSeparator.VERTICAL)
            d.height = AquaUtils.INFINITY;
        else {
            d.width = AquaUtils.INFINITY;
        }
        return d;
    }
}
