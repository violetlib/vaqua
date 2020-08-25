/*
 * Copyright (c) 2014-2016 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.SeparatorUI;
import java.awt.*;

/**
 * A customized UI for JSeparator, matching Yosemite.
 */
public class AquaSeparatorUI extends SeparatorUI {

    private static int thickness;

    public static ComponentUI createUI(JComponent c) {
        return new AquaSeparatorUI();
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
        LookAndFeel.installColors(s, "Separator.background", "Separator.foreground");
        LookAndFeel.installProperty(s, "opaque", Boolean.FALSE);
    }

    protected void uninstallDefaults(JSeparator s) {
    }

    public void paint(Graphics g, JComponent c) {
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
            d.height = Integer.MAX_VALUE;
        else {
            d.width = Integer.MAX_VALUE;
        }
        return d;
    }
}
