/*
 * Changes copyright (c) 2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;
import javax.swing.plaf.UIResource;

import org.jetbrains.annotations.Nullable;

/**
 * A scroll bar that supports smooth scrolling.
 */

// Based on JScrollPane.ScrollBar

/*
 * Copyright (c) 1997, 2021, Oracle and/or its affiliates. All rights reserved.
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

public class AquaScrollBar extends JScrollBar implements UIResource {
    private boolean unitIncrementSet;
    private boolean blockIncrementSet;

    public AquaScrollBar(int orientation) {
        super(orientation);
        this.putClientProperty("JScrollBar.fastWheelScrolling", Boolean.TRUE);
    }

    public void setUnitIncrement(int unitIncrement) {
        unitIncrementSet = true;
        this.putClientProperty("JScrollBar.fastWheelScrolling", null);
        super.setUnitIncrement(unitIncrement);
    }

    public int getUnitIncrement(int direction) {
        if (unitIncrementSet) {
            return super.getUnitIncrement(direction);
        }
        return 1;
    }

    public void setBlockIncrement(int blockIncrement) {
        blockIncrementSet = true;
        this.putClientProperty("JScrollBar.fastWheelScrolling", null);
        super.setBlockIncrement(blockIncrement);
    }

    public int getBlockIncrement(int direction) {
        if (blockIncrementSet) {
            return super.getBlockIncrement(direction);
        }
        JViewport vp = getViewport();
        if (vp == null) {
            return super.getBlockIncrement(direction);
        }

        if (vp.getView() instanceof Scrollable) {
            Scrollable view = (Scrollable)(vp.getView());
            Rectangle vr = vp.getViewRect();
            return view.getScrollableBlockIncrement(vr, getOrientation(), direction);
        } else if (getOrientation() == VERTICAL) {
            return vp.getExtentSize().height;
        } else {
            return vp.getExtentSize().width;
        }
    }

    private @Nullable JViewport getViewport() {
        JScrollPane sp = getScrollPane();
        return sp != null ? sp.getViewport() : null;
    }

    private @Nullable JScrollPane getScrollPane() {
        Container parent = getParent();
        if (parent instanceof OverlayScrollPaneHack.AquaOverlayViewportHolder) {
            parent = parent.getParent();
        }
        if (parent instanceof JScrollPane) {
            return (JScrollPane) parent;
        }
        return null;
    }
}
