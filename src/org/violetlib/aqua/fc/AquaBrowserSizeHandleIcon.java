/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.awt.*;
import javax.swing.*;
import javax.swing.plaf.UIResource;

/**
 * The column size handle used with legacy scroll bars.
 */
public class AquaBrowserSizeHandleIcon implements Icon, UIResource {

    // This rendering is specific to 10.10 and 10.11...

    @Override
    public int getIconHeight() {
        return 15;
    }

    @Override
    public int getIconWidth() {
        return 15;
    }

    @Override
    public void paintIcon(Component c, Graphics g, int x, int y) {
        g.setColor(new Color(250, 250, 250));
        g.fillRect(x, y, 15, 15);
        g.setColor(new Color(237, 237, 237));
        g.fillRect(x, y, 1, 15);
        g.fillRect(x + 15 - 1, y, 1, 15);
        g.setColor(new Color(163, 163, 163));
        g.fillRect(x + 5, y + 4, 1, 7);
        g.fillRect(x + 9, y + 4, 1, 7);
    }
}
