/*
 * Changes Copyright (c) 2015-2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

/*
 * Copyright (c) 2011, 2015, Oracle and/or its affiliates. All rights reserved.
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

import org.violetlib.jnr.aqua.AquaUIPainter;

/**
 * A common base class for borders.
 */
public abstract class AquaBorder implements Border, UIResource {
    protected final AquaUIPainter painter = AquaPainting.create();

    protected AquaBorder() {
    }

    public static void repaintBorder(JComponent c) {
        JComponent borderedComponent = c;
        Border border = c.getBorder();
        if (border == null) {
            // See if it's inside a JScrollpane or something
            Container p = c.getParent();
            if (p instanceof JViewport) {
                borderedComponent = (JComponent)p.getParent();
                if (borderedComponent != null) {
                    border = borderedComponent.getBorder();
                }
            }
        }

        // If we really don't have a border, then bail
        // It might be a compound border with a ThemeBorder inside
        // The check for that case is tricky, so we just go ahead and repaint any border
        if (border == null || borderedComponent == null) {
            return;
        }

        int width = borderedComponent.getWidth();
        int height = borderedComponent.getHeight();
        Insets i = borderedComponent.getInsets();

        borderedComponent.repaint(0, 0, width, i.top); // Top edge
        borderedComponent.repaint(0, 0, i.left, height); // Left edge
        borderedComponent.repaint(0, height - i.bottom, width, i.bottom); // Bottom edge
        borderedComponent.repaint(width - i.right, 0, i.right, height); // Right edge
    }

    @Override
    public boolean isBorderOpaque() { return false; }
}
