/*
 * Changes copyright (c) 2015-2018 Alan Snyder.
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
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.UIResource;

import org.violetlib.jnr.aqua.AquaUIPainter.TitleBarWidget;

/**
 * The primary responsibility of this class is to know the title bar bounds relative to this border.
 */
public class AquaInternalFrameBorder implements Border, UIResource {
    public static final int kCloseButton = 0;
    public static final int kIconButton = 1;
    public static final int kGrowButton = 2;

    protected final JInternalFrame frame;
    protected final AquaTitleBar titleBar;
    protected int titleBarX;
    protected int titleBarY;

    public AquaInternalFrameBorder(JInternalFrame frame, TitleBarWidget widget) {
        this.frame = frame;
        titleBar = new AquaTitleBar(frame, widget);
    }

    public void invalidateLayout() {
        titleBar.invalidateLayout();
    }

    // Border interface
    @Override
    public boolean isBorderOpaque() {
        return false;
    }

    // Border interface
    @Override
    public Insets getBorderInsets(Component c) {
        int titleBarHeight = titleBar.getTitleBarHeight();
        return new Insets(titleBarHeight, 0, 0, 0);
    }

    // Border interface
    @Override
    public void paintBorder(Component c, Graphics g, int x, int y, int w, int h) {
        // For expanded InternalFrames, the frame & component are the same object
        titleBarX = x;
        titleBarY = y;
        titleBar.setWidth(w);
        g.translate(x, y);
        titleBar.paint(g);
        g.translate(-x, -y);
    }

    public int getWhichButtonHit(int x, int y) {
        return titleBar.getWhichButtonHit(x - titleBarX, y - titleBarY);
    }

    public boolean getWithinRolloverArea(int x, int y) {
        return titleBar.getWithinRolloverArea(x - titleBarX, y - titleBarY);
    }

    public void repaintButtonArea() {
        Rectangle r = titleBar.getButtonArea();
        r.x += titleBarX;
        r.y += titleBarY;
        frame.repaint(r);
    }

    protected int getTitleHeight() {
        return titleBar.getTitleBarHeight();
    }
}
