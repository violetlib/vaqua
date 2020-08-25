/*
 * Changes Copyright (c) 2015-2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

/*
 * Copyright (c) 2011, 2014, Oracle and/or its affiliates. All rights reserved.
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
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.*;

import org.violetlib.jnr.aqua.AquaUIPainter;

@SuppressWarnings("serial") // Superclass is not serializable across versions
class AquaComboBoxButton extends JButton {

    protected final JComboBox<Object> comboBox;
    protected final AquaComboBoxUI ui;
    protected final AquaUIPainter painter = AquaPainting.create();
    protected boolean isRollover;

    @SuppressWarnings("serial") // anonymous class
    protected AquaComboBoxButton(AquaComboBoxUI ui, JComboBox<Object> comboBox) {
        super("");

        this.ui = ui;
        this.comboBox = comboBox;

        setModel(new DefaultButtonModel() {
            public void setArmed(boolean armed) {
                super.setArmed(isPressed() || armed);
            }
        });

        setEnabled(comboBox.isEnabled());

        addMouseListener(new RolloverMouseListener());
    }

    public boolean isEnabled() {
        return comboBox == null || comboBox.isEnabled();
    }

    @SuppressWarnings("deprecation")
    public boolean isFocusTraversable() {
        return false;
    }

    @Override
    public void paintComponent(Graphics g) {
    }

    private class RolloverMouseListener extends MouseAdapter {
        @Override
        public void mouseEntered(MouseEvent e) {
            isRollover = true;
            comboBox.repaint();
        }

        @Override
        public void mouseExited(MouseEvent e) {
            isRollover = false;
            comboBox.repaint();
        }
    }
}
