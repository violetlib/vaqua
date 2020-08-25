/*
 * Copyright (c) 2018 Alan Snyder.
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
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicSeparatorUI;

import org.jetbrains.annotations.NotNull;
import org.violetlib.aqua.AquaUtils.RecyclableSingletonFromDefaultConstructor;

public class AquaPopupMenuSeparatorUI extends BasicSeparatorUI implements AquaComponentUI {
    protected static RecyclableSingletonFromDefaultConstructor<AquaPopupMenuSeparatorUI> instance = new RecyclableSingletonFromDefaultConstructor<AquaPopupMenuSeparatorUI>(AquaPopupMenuSeparatorUI.class);

    public static ComponentUI createUI(JComponent c) {
        return instance.get();
    }

    @Override
    public void appearanceChanged(@NotNull JComponent c, @NotNull AquaAppearance appearance) {
    }

    @Override
    public void activeStateChanged(@NotNull JComponent c, boolean isActive) {
    }

    public void update(Graphics g, JComponent c) {
        paint(g, c);
    }

    public void paint(Graphics g, JComponent c) {
        Dimension s = c.getSize();

        AquaAppearance appearance = AppearanceManager.ensureAppearance(c);
        Color color = appearance.getColor("separator");

        g.setColor(color);
        if (((JSeparator)c).getOrientation() == SwingConstants.VERTICAL) {
            g.fillRect(5, 0, 2, s.height);
        } else {
            g.fillRect(0, 5, s.width, 2);
        }
    }

    public Dimension getPreferredSize(JComponent c) {
        if (((JSeparator)c).getOrientation() == SwingConstants.VERTICAL) {
            return new Dimension(12, 0);
        }

        return new Dimension(0, 12);
    }
}
