/*
 * Copyright (c) 2014-2015 Alan Snyder.
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

    public static ComponentUI createUI(JComponent c) {
        return new AquaSeparatorUI();
    }

    public void installUI(JComponent c) {
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
        if (((JSeparator) c).getOrientation() == JSeparator.VERTICAL) {
            g.drawLine(s.left, s.top, s.left, size.height - s.bottom - 1);
        } else {
            g.drawLine(s.left, s.top, size.width - s.right - 1, s.top);
        }
    }

    public Dimension getMinimumSize(JComponent c) {
        return getPreferredSize(c);
    }

    public Dimension getPreferredSize(JComponent c) {
        Insets insets = c.getInsets();
        if (((JSeparator) c).getOrientation() == JSeparator.VERTICAL) {
            return new Dimension(1 + insets.left + insets.right, insets.top + insets.bottom);
        } else {
            return new Dimension(insets.left + insets.right, 1 + insets.top + insets.bottom);
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
