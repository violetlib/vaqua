/*
 * Copyright (c) 2015-2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;
import javax.swing.border.Border;

/**
 * The container that wraps a component to support rounded corners. Used for contextual menus, pop up and pull down
 * menus, but not for editable combo box menus.
 */
public class AquaBasicPopupMenuWrapper extends JPanel {

    public AquaBasicPopupMenuWrapper(JComponent c, Border border) {
        super(null);

        setLayout(new BorderLayout());
        add(c);
        setBorder(border);
        setOpaque(false);
    }

    @Override
    public void paintComponent(Graphics g) {

        // The following code supports pop up menus with rounded corners. The key requirement is that the popup must
        // not paint near the corners.

        Border border = getBorder();
        if (border instanceof BackgroundPainter) {
            BackgroundPainter p = (BackgroundPainter) border;
            Rectangle bounds = getBounds();
            p.paintBackground(this, g, bounds.x, bounds.y, bounds.width, bounds.height);
        }

        super.paintComponent(g);
    }
}
