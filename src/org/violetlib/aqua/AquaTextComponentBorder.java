/*
 * Changes Copyright (c) 2015-2026 Alan Snyder.
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
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;
import javax.swing.text.JTextComponent;

import org.jetbrains.annotations.*;
import org.violetlib.jnr.Insets2D;
import org.violetlib.jnr.Insets2DUIResource;
import org.violetlib.jnr.Insetter;
import org.violetlib.jnr.aqua.AquaUIPainter.State;

/**
 * A border that is associated with a text component but can be attached to a text component or the scroll pane that
 * displays the text component.
 */
public class AquaTextComponentBorder extends AquaBorder implements AquaBackgroundBorder, FocusRingOutlineProvider, Border2D {

    // This border is really a background. It paints as background, not as a border.
    // It needs to be a Border as a signal to the UI that the user has not installed a custom border.
    // It also provides the insets to the component UI.

    protected final @NotNull JTextComponent tc;

    /**
     * Create a border whose configuration is based on the specified text component.
     */
    public AquaTextComponentBorder(@NotNull JTextComponent tc) {
        this.tc = tc;
    }

    @Override
    protected void paint(JComponent c, Graphics2D g, int x, int y, int width, int height) {
        // The border is a background. It is not painted as a border.
    }

    @Override
    public void paintBackground(@NotNull Component c, Graphics g, @Nullable Color background) {
        boolean isCellComponent = AquaUtils.isCellComponent(c);
        if (background != null
          && (!isCellComponent || !(background instanceof UIResource) || AquaFocusHandler.hasFocus(c))) {
            g.setColor(background);
            int width = c.getWidth();
            int height = c.getHeight();
            g.fillRect(0, 0, width, height);
        }
    }

    public @Nullable Shape getFocusRingOutline(@NotNull JComponent c) {

        // If the text component is scrollable, the text component defines the outline shape and the scroll pane
        // defines the outline size.

        JComponent cc = getComponentForFocusRing(c);
        if (cc != null) {
            return AquaDefaultFocusRingProvider.getDefaultFocusRing(c);
        }
        return null;
    }

    private @Nullable JComponent getComponentForFocusRing(@NotNull JComponent c) {
        if (c == tc) {
            return tc;
        }
        if (c instanceof JScrollPane) {
            JScrollPane sp = (JScrollPane) c;
            if (sp.getViewport().getView() == tc) {
                return sp;
            }
        }
        return null;
    }

    public @NotNull Insets getBorderInsets(@NotNull Component c) {

        if (AquaCellEditorPolicy.getInstance().getCellStatus(tc) != null) {
            return new InsetsUIResource(0, 0, 0, 0);
        }

        Insetter s = getTextInsets();
        Insets2D n = s.asInsets2D();
        if (n != null) {
            // We want to handle non-integer top and bottom insets by shifting the contents
            int left = (int) Math.ceil(n.getLeft());
            int right = (int) Math.ceil(n.getRight());
            int top = (int) Math.floor(n.getTop());
            int bottom = (int) Math.ceil(n.getBottom());
            return new InsetsUIResource(top, left, bottom, right);
        }

        return new InsetsUIResource(3, 3, 3, 3);
    }

    public @NotNull Insets2D getBorderInsets2D(@NotNull Component c) {

        if (AquaCellEditorPolicy.getInstance().getCellStatus(tc) != null) {
            return new Insets2D(0, 0, 0, 0);
        }

        Insetter s = getTextInsets();
        Insets2D n = s.asInsets2D();
        if (n != null) {
            return new Insets2DUIResource(n);
        }

        return new Insets2DUIResource(3, 3, 3, 3);
    }

    public @NotNull Insetter getTextInsets() {
        return Insetter.trivial();
    }

    protected @NotNull State getState() {
        if (!AquaFocusHandler.isActive(tc)) {
            return tc.isEnabled() ? State.INACTIVE : State.DISABLED_INACTIVE;
        } else {
            return tc.isEnabled() ? State.ACTIVE : State.DISABLED;
        }
    }
}
