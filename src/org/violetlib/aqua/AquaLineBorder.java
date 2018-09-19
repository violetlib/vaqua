/*
 * Changes copyright (c) 2018 Alan Snyder.
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
import javax.swing.border.Border;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * The class renders a line border using an appearance based color.
 */
public class AquaLineBorder implements Border {

    private static final Insets borderInsets = new Insets(1, 1, 1, 1);

    private final String colorName;

    public AquaLineBorder(@NotNull String colorName) {
        this.colorName = colorName;
    }

    @Override
    public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
        Color color = getColor(c);
        if (color == null) {
            color = Color.gray;
        }

        g.setColor(color);
        g.fillRect(x, y, width, 1);
        g.fillRect(x, y+1, 1, height-2);
        g.fillRect(x, y + height - 1, width, 1);
        g.fillRect(x + width - 1, y+1, 1, height-2);
    }

    protected @Nullable Color getColor(@NotNull Component c) {
        AquaAppearance appearance = AppearanceManager.getRegisteredAppearance(c);
        if (appearance != null) {
            return appearance.getColor(colorName);
        }
        return null;
    }

    @Override
    public @NotNull Insets getBorderInsets(Component c) {
        return borderInsets;
    }

    @Override
    public boolean isBorderOpaque() {
        return false;
    }
}
