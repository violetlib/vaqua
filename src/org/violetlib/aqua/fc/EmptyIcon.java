/*
 * @(#)EmptyIcon.java
 *
 * Copyright 2010 Werner Randelshofer, Switzerland.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the
 * license agreement you entered into with Werner Randelshofer.
 * For details see accompanying license terms.
 */

package org.violetlib.aqua.fc;

import javax.swing.*;
import java.awt.*;
import java.io.Serializable;

/**
 * {@code EmptyIcon}.
 *
 * @author Werner Randelshofer
 * @version $Id$
 */
public class EmptyIcon implements Icon, Serializable {
    private int width;
    private int height;

    public EmptyIcon(int width, int height) {
        this.width = width;
        this.height = height;
    }

    public void paintIcon(Component c, Graphics g, int x, int y) {
        // empty
    }

    public int getIconWidth() {
        return width;
    }

    public int getIconHeight() {
        return height;
    }
}
