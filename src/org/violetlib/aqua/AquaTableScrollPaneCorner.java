/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;
import javax.swing.plaf.UIResource;
import javax.swing.table.JTableHeader;

/**
 * Fills out the table header when a legacy vertical scroll bar is used.
 */
public class AquaTableScrollPaneCorner extends JComponent implements UIResource {

    private int borderHeight = UIManager.getInt("TableHeader.borderHeight");
    private Color borderColor = UIManager.getColor("TableHeader.borderColor");

    @Override
    protected void paintComponent(Graphics g) {
        int width = getWidth();
        int height = getHeight();

        Object parent = getParent();
        if (parent instanceof JScrollPane) {
            JScrollPane sp = (JScrollPane) parent;
            JViewport vp = sp.getColumnHeader();
            if (vp != null) {
                Component c = vp.getView();
                if (c instanceof JTableHeader) {
                    JTableHeader th = (JTableHeader) c;
                    if (th.isOpaque()) {
                        Color bc = th.getBackground();
                        if (bc != null) {
                            g.setColor(bc);
                            g.fillRect(0, 0, width, height);
                        }
                    }

                }
            }
        }

        g.setColor(borderColor);
        g.fillRect(0, height-borderHeight, width, borderHeight);
    }
}
