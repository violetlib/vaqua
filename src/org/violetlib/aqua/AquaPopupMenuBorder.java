/*
 * Changes Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

/*
 * Copyright (c) 2011, 2012, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.geom.RoundRectangle2D;
import javax.swing.*;

/**
 * A common base class for contextual menu borders.
 */
public class AquaPopupMenuBorder extends AquaBorder implements BackgroundPainter {
    public AquaPopupMenuBorder() { }

    public final void paintBorder(final Component c, final Graphics g, final int x, final int y, final int width, final int height) {
        // A menu border is partly a background that paints below the menu contents and partly a normal outline border.
        paintMenuBorder(c, g, x, y, width, height);
    }

    /**
     * Returns the insets of the border.
     * @param c the component for which this border insets value applies
     */
    public Insets getBorderInsets(final Component c) {
//        // for more info on this issue, see AquaComboBoxPopup.updateContents()
//        final JPopupMenu menu = (JPopupMenu)c;
//        final int nChildren = menu.getComponentCount();
//        if (nChildren > 0) {
//            final Component firstChild = menu.getComponent(0);
//            if (firstChild instanceof Box.Filler) return getEmptyInsets();
//            if (firstChild instanceof JScrollPane) return getEmptyInsets();
//        }

        // The insets are designed to prevent the menu items from painting over the menu outline border.
        // To work with the popup menu borders implemented by AquaPopupMenuUI, popup borders must have a minimum 1 pixel
        // left and right inset. Not sure what the minimum top and bottom are, but 4 works.
        return new Insets(5, 1, 5, 1);
    }

    @Override
    public void paintBackground(Component c, Graphics g, int x, int y, int width, int height) {
        if (g instanceof Graphics2D) {
            Graphics2D g2d = (Graphics2D) g;
            paintMenuBackground(c, g2d, x, y, width, height);
        }
    }

    /**
     * Paint the menu background. In Yosemite, the background is gray and has rounded corners. The rest of the menu is
     * transparent.
     */
    protected void paintMenuBackground(Component c, Graphics2D g2d, int x, int y, int width, int height) {
        Graphics2D g = (Graphics2D) g2d.create();
        g.translate(x, y);
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g.setColor(UIManager.getColor("Menu.background"));
        RoundRectangle2D b = new RoundRectangle2D.Double(0, 0, width, height, 9, 9);
        g.fill(b);
        g.dispose();
    }

    /**
     * Paint the menu border. The menu border is painted over the menu items.
     */
    protected void paintMenuBorder(Component c, Graphics gg, int x, int y, int width, int height) {
        Graphics2D g = (Graphics2D) gg.create();
        g.translate(x, y);
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g.setColor(UIManager.getColor("Menu.borderColor"));
        g.setStroke(new BasicStroke(1));
        RoundRectangle2D b = new RoundRectangle2D.Double(0, 0, width-0.5, height-0.5, 9, 9);
        g.draw(b);
        g.dispose();
    }
}
