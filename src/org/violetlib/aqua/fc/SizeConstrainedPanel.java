/*
 * @(#)SizeConstrainedPanel.java  1.0  02 January 2005
 *
 * Copyright (c) 2004-2013 Werner Randelshofer, Switzerland.
 * You may not use, copy or modify this file, except in compliance with the
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.awt.*;

/**
 * A JPanel which has a fixed preferred width or height.
 *
 * @author  Werner Randelshofer
 * @version 1.0  02 January 2005  Created.
 */
public class SizeConstrainedPanel extends javax.swing.JPanel {
    private int preferredWidth = -1;
    private int preferredHeight = -1;

    public SizeConstrainedPanel() {
        setLayout(new java.awt.BorderLayout());
    }

    /**
     * Sets the preferred width of the panel, without affecting its preferred height.
     * @param w Preferred width. The value -1 clears the preferred width.
     */
    public void setPreferredWidth(int w) {
        this.preferredWidth = w;
    }

    /**
     * Sets the preferred height of the panel, without affecting its preferred width.
     * @param h Preferred height. The value -1 clears the preferred height.
     */
    public void setPreferredHeight(int h) {
        this.preferredHeight = h;
    }

    public Dimension getPreferredSize() {
        Dimension dim = super.getPreferredSize();
        if (preferredWidth != -1) dim.width = preferredWidth;
        if (preferredHeight != -1) dim.height = preferredHeight;
        return dim;
    }
}
