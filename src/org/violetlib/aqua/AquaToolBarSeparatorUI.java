/*
 * Changes Copyright (c) 2016-2021 Alan Snyder.
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
import javax.swing.plaf.basic.BasicToolBarSeparatorUI;

import org.jetbrains.annotations.NotNull;
import org.violetlib.aqua.AquaUtils.RecyclableSingleton;
import org.violetlib.aqua.AquaUtils.RecyclableSingletonFromDefaultConstructor;

public class AquaToolBarSeparatorUI extends BasicToolBarSeparatorUI implements AquaComponentUI {
    protected static RecyclableSingleton<AquaToolBarSeparatorUI> instance = new RecyclableSingletonFromDefaultConstructor<AquaToolBarSeparatorUI>(AquaToolBarSeparatorUI.class);

    public static ComponentUI createUI(JComponent c) {
        return instance.get();
    }

    public AquaToolBarSeparatorUI() {
    }

    @Override
    public void appearanceChanged(@NotNull JComponent c, @NotNull AquaAppearance appearance) {
    }

    @Override
    public void activeStateChanged(@NotNull JComponent c, boolean isActive) {
    }

    @Override
    public void update(Graphics g, JComponent c) {
        AppearanceManager.registerCurrentAppearance(c);
        super.update(g, c);
    }

    public void paint(Graphics g, JComponent c) {
        // The separator item style was removed in OS 10.7. We implement as a space.
    }

    public Dimension getMinimumSize(JComponent c) {
        JToolBar.Separator sep = (JToolBar.Separator)c;
        if (sep.getOrientation() == SwingConstants.HORIZONTAL) {
            return new Dimension(1, 11);
        }
        return new Dimension(11, 1);
    }

    public Dimension getPreferredSize(JComponent c) {
        JToolBar.Separator sep = (JToolBar.Separator)c;
        if (sep.getOrientation() == SwingConstants.HORIZONTAL) {
            return new Dimension(1, 11);
        }
        return new Dimension(11, 1);
    }

    public Dimension getMaximumSize(JComponent c) {
        JToolBar.Separator sep = (JToolBar.Separator)c;
        if (sep.getOrientation() == SwingConstants.HORIZONTAL) {
            return new Dimension(Integer.MAX_VALUE, 11);
        }
        return new Dimension(11, Integer.MAX_VALUE);
    }
}
