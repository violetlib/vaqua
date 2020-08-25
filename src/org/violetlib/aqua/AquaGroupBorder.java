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

import javax.swing.border.Border;

import org.violetlib.aqua.AquaUtils.RecyclableSingletonFromDefaultConstructor;
import org.violetlib.jnr.aqua.AquaUIPainter;
import org.violetlib.jnr.aqua.Configuration;
import org.violetlib.jnr.aqua.GroupBoxConfiguration;

/**
 * A group border has a painted border with insets around it. It also has a margin that independently defines the content region.
 */
public class AquaGroupBorder extends AquaBorder {
    static final RecyclableSingletonFromDefaultConstructor<? extends Border> tabbedPaneGroupBorder = new RecyclableSingletonFromDefaultConstructor<TabbedPane>(TabbedPane.class);
    public static Border getTabbedPaneGroupBorder() {
        return tabbedPaneGroupBorder.get();
    }

    static final RecyclableSingletonFromDefaultConstructor<? extends Border> titleBorderGroupBorder = new RecyclableSingletonFromDefaultConstructor<Titled>(Titled.class);
    public static Border getBorderForTitledBorder() {
        return titleBorderGroupBorder.get();
    }

    static final RecyclableSingletonFromDefaultConstructor<? extends Border> titlelessGroupBorder = new RecyclableSingletonFromDefaultConstructor<Titleless>(Titleless.class);
    public static Border getTitlelessBorder() {
        return titlelessGroupBorder.get();
    }

    protected final Insets insets;
    protected final Insets margins;

    public AquaGroupBorder(Insets insets, Insets margins) {
        this.insets = insets;
        this.margins = margins;
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        return (Insets) margins.clone();
    }

    public void paintBorder(final Component c, final Graphics g, int x, int y, int width, int height) {
        // sg2d.setColor(Color.MAGENTA);
        // sg2d.drawRect(x, y, width - 1, height - 1);

        final Insets internalInsets = insets;
        x += internalInsets.left;
        y += internalInsets.top;
        width -= (internalInsets.left + internalInsets.right);
        height -= (internalInsets.top + internalInsets.bottom);

        // TBD: state is not currently used, but perhaps someday it will be...

        painter.configure(width, height);
        Configuration bg = getConfiguration();
        painter.getPainter(bg).paint(g, x, y);

        // sg2d.setColor(Color.ORANGE);
        // sg2d.drawRect(x, y, width, height);
    }

    protected Configuration getConfiguration() {
        return new GroupBoxConfiguration(AquaUIPainter.State.ACTIVE, false);
    }

    protected static class TabbedPane extends AquaGroupBorder {
        public TabbedPane() {
            super(new Insets(5, 5, 5, 5), new Insets(8, 12, 8, 12));
        }
    }

    protected static class Titled extends AquaGroupBorder {
        public Titled() {
            super(new Insets(16, 5, 4, 5), new Insets(16, 20, 16, 20));
        }
    }

    protected static class Titleless extends AquaGroupBorder {
        public Titleless() {
            super(new Insets(3, 5, 1, 5), new Insets(8, 12, 8, 12));
        }
    }
}
