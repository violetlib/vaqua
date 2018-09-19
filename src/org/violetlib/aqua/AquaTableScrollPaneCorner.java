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
import javax.swing.plaf.UIResource;
import javax.swing.table.JTableHeader;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Fills out the table header when a legacy vertical scroll bar is used.
 */
public class AquaTableScrollPaneCorner extends JComponent implements UIResource {

    @Override
    protected void paintComponent(@NotNull Graphics g) {
        int width = getWidth();
        int height = getHeight();

        JTableHeader header = getHeader();
        if (header != null) {
            Color bc = header.getBackground();
            if (bc != null) {
                g.setColor(bc);
                g.fillRect(0, 0, width, height);
            }
            AquaTableHeaderUI ui = AquaUtils.getUI(header, AquaTableHeaderUI.class);
            if (ui != null) {
                Color borderColor = ui.getSeparatorColor();
                if (borderColor != null) {
                    int borderHeight = ui.getBorderHeight();
                    if (borderHeight > 0) {
                        g.setColor(borderColor);
                        g.fillRect(0, height - borderHeight, width, borderHeight);
                    }
                }
            }
        }
    }

    protected @Nullable JTableHeader getHeader() {
        Object parent = getParent();
        if (parent instanceof JScrollPane) {
            JScrollPane sp = (JScrollPane) parent;
            JViewport vp = sp.getColumnHeader();
            if (vp != null) {
                Component c = vp.getView();
                if (c instanceof JTableHeader) {
                    return (JTableHeader) c;
                }
            }
        }
        return null;
    }
}
