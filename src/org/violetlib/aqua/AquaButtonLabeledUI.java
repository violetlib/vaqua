/*
 * Changes copyright (c) 2015-2020 Alan Snyder.
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

import org.jetbrains.annotations.NotNull;

/**
 * The base class for check box and radio button UIs. These UIs are shared.
 */
public abstract class AquaButtonLabeledUI extends AquaButtonUI {

    protected final @NotNull AquaLabeledButtonBorder widgetBorder;

    public AquaButtonLabeledUI(@NotNull AquaLabeledButtonBorder border) {
        widgetBorder = border;
    }

    @Override
    protected @NotNull Border getDefaultBorder(@NotNull AbstractButton b, boolean isToolbar) {
        return widgetBorder;
    }

    @Override
    protected void paint(@NotNull Graphics2D g, @NotNull AbstractButton b) {
        Rectangle viewRect = new Rectangle(b.getWidth(), b.getHeight());
        widgetBorder.paintButton(g, b, null, viewRect);
    }
}
