/*
 * Changes copyright (c) 2018 Alan Snyder.
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

import java.awt.event.*;

import javax.swing.JComponent;
import javax.swing.plaf.ComponentUI;
import javax.swing.text.JTextComponent;

/**
 * This class exists only as a hack to work around a Sun bug which parks the
 * insertion caret at the beginning of a text field when it gets clicked on.
 */
public class AquaTextFieldFormattedUI extends AquaTextFieldUI implements MouseListener {

    public static ComponentUI createUI(JComponent c) {
        return new AquaTextFieldFormattedUI();
    }

    @Override
    protected String getPropertyPrefix() {
        return "FormattedTextField";
    }

    protected void installListeners() {
        super.installListeners();
        getComponent().addMouseListener(this);
    }

    protected void uninstallListeners() {
        getComponent().removeMouseListener(this);
        super.uninstallListeners();
    }

    public void mouseClicked(MouseEvent e) {
        if (e.getClickCount() != 1) return;

        JTextComponent c = getComponent();
        // apparently, focus has already been granted by the time this mouse listener fires
    //    if (c.hasFocus()) return;

        c.setCaretPosition(viewToModel(c, e.getPoint()));
    }

    public void mouseEntered(MouseEvent e) { }
    public void mouseExited(MouseEvent e) { }
    public void mousePressed(MouseEvent e) { }
    public void mouseReleased(MouseEvent e) { }
}
