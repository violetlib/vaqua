/*
 * Changes Copyright (c) 2015-2021 Alan Snyder.
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

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.jnr.*;
import org.violetlib.jnr.Painter;
import org.violetlib.jnr.aqua.AquaUIPainter;
import org.violetlib.jnr.aqua.AquaUIPainter.Size;
import org.violetlib.jnr.aqua.AquaUIPainter.State;
import org.violetlib.jnr.aqua.AquaUIPainter.TextFieldWidget;
import org.violetlib.jnr.aqua.TextFieldConfiguration;
import org.violetlib.jnr.aqua.TextFieldLayoutConfiguration;

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
    public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
        // The border is a background. It is not painted as a border.
    }

    @Override
    public void paintBackground(@NotNull Component c, Graphics g, @Nullable Color background) {

        boolean isCellComponent = AquaUtils.isCellComponent(c);

        if (c.isOpaque()) {
            if (background != null
                    && (!isCellComponent || !(background instanceof UIResource) || AquaFocusHandler.hasFocus(c))) {
                g.setColor(background);
                int width = c.getWidth();
                int height = c.getHeight();
                g.fillRect(0, 0, width, height);
            }
        } else if (!isCellComponent) {
            Painter p = getConfiguredPainter(c);
            p.paint(g, 0, 0);
        }
    }

    public @NotNull LayoutInfo getLayoutInfo() {
        TextFieldLayoutConfiguration g = getLayoutConfiguration();
        return painter.getLayoutInfo().getLayoutInfo(g);
    }

    public @Nullable Shape getFocusRingOutline(@NotNull JComponent c) {
        if (c == tc) {
            TextFieldLayoutConfiguration g = getLayoutConfiguration();
            AppearanceManager.ensureAppearance(tc);
            AquaUtils.configure(painter, tc, tc.getWidth(), tc.getHeight());
            return painter.getOutline(g);
        } else {
            return null;
        }
    }

    /**
     * Return the text margin. The text margin is like a border margin, except that it is part of the text display and
     * therefore scrolls left and right along with the text. The result is that the full width of the text field
     * (exclusive of the actual border margin) is available for editing.
     */
    public int getTextMargin() {
        // The cell editor test probably fails during the initial construction of the text component.
        // It should be performed again when the text field is added as a component of the table.
        if (AquaCellEditorPolicy.getInstance().getCellStatus(tc) != null) {
            return 1;
        } else {
            // The goal is to give the appearance of a specific left and right margin, but the portion of the margin
            // that is not reserved by the border is available for editing.
            TextFieldWidget widget = getWidget();
            int target = widget == TextFieldWidget.TEXT_FIELD_ROUND ? 11 : 3;
            Insets s = getBorderInsets(tc);
            int averageInset = (s.left + s.right) / 2;
            return Math.max(0, target - averageInset);
        }
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
        TextFieldLayoutConfiguration g = getLayoutConfiguration();
        return painter.getLayoutInfo().getTextFieldTextInsets(g);
    }

    protected int getExtraHeight() {
        TextFieldLayoutConfiguration g = getLayoutConfiguration();
        TextFieldWidget widget = g.getWidget();
        switch (widget) {
            case TEXT_FIELD:
            case TEXT_FIELD_ROUND:
                return 3;
            default:
                return 0;
        }
    }

    protected @NotNull Painter getConfiguredPainter(@NotNull Component c) {
        int width = c.getWidth();
        int height = c.getHeight();
        AppearanceManager.ensureAppearance(tc);
        AquaUtils.configure(painter, tc, width, height);
        TextFieldConfiguration tg = getConfiguration();
        return painter.getPainter(tg);
    }

    protected @NotNull TextFieldLayoutConfiguration getLayoutConfiguration() {
        TextFieldWidget widget = getWidget();
        Size defaultSize = getSpecialDefaultSize();
        Size size = AquaUtilControlSize.getUserSizeFrom(tc, defaultSize);
        AquaUIPainter.UILayoutDirection ld = AquaUtils.getLayoutDirection(tc);
        return new TextFieldLayoutConfiguration(widget, size, ld);
    }

    protected @Nullable Size getSpecialDefaultSize() {
        return null;
    }

    protected @NotNull TextFieldConfiguration getConfiguration() {
        TextFieldWidget widget = getWidget();
        Size defaultSize = getSpecialDefaultSize();
        Size size = AquaUtilControlSize.getUserSizeFrom(tc, defaultSize);
        State state = getState();
        boolean isFocused = State.ACTIVE == state && tc.hasFocus();
        AquaUIPainter.UILayoutDirection ld = AquaUtils.getLayoutDirection(tc);
        return new TextFieldConfiguration(widget, size, state, isFocused, ld);
    }

    protected @NotNull TextFieldWidget getWidget() {
        Object o = tc.getClientProperty(AquaTextFieldUI.TEXT_FIELD_STYLE_KEY);
        if ("round".equals(o)) {
            return AquaUtils.isOnToolbar(tc) ? TextFieldWidget.TEXT_FIELD_ROUND_TOOLBAR : TextFieldWidget.TEXT_FIELD_ROUND;
        } else if (AquaUtils.isOnToolbar(tc)) {
            return TextFieldWidget.TEXT_FIELD_ROUND_TOOLBAR;
        }
        return TextFieldWidget.TEXT_FIELD;
    }

    protected @NotNull State getState() {
        if (!AquaFocusHandler.isActive(tc)) {
            return tc.isEnabled() ? State.INACTIVE : State.DISABLED_INACTIVE;
        } else {
            return tc.isEnabled() ? State.ACTIVE : State.DISABLED;
        }
    }
}
