/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;

/**
 * A border for a menu item.
 */
public class AquaMenuItemBorder extends AquaBorder {
    @Override
    public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
    }

    @Override
    public Insets getBorderInsets(Component c) {
        return new Insets(1, 5, 1, 5);
    }
}
