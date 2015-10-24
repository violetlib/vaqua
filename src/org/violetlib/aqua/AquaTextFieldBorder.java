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
import javax.swing.*;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.text.JTextComponent;

import org.violetlib.aqua.AquaUtils.RecyclableSingleton;
import org.violetlib.aqua.AquaUtils.RecyclableSingletonFromDefaultConstructor;
import org.violetlib.jnr.*;
import org.violetlib.jnr.Painter;
import org.violetlib.jnr.aqua.AquaUIPainter;
import org.violetlib.jnr.aqua.AquaUIPainter.Size;
import org.violetlib.jnr.aqua.AquaUIPainter.State;
import org.violetlib.jnr.aqua.AquaUIPainter.TextFieldWidget;
import org.violetlib.jnr.aqua.TextFieldConfiguration;
import org.violetlib.jnr.aqua.TextFieldLayoutConfiguration;

public class AquaTextFieldBorder extends AquaBorder implements FocusRingOutlineProvider, Border2D {

    // This border is really a background. It paints as background, not as a border.
    // It needs to be a Border as a signal to the UI that the user has not installed a custom border.
    // It also provides the insets to the base text field UI.

    protected static final RecyclableSingleton<AquaTextFieldBorder> instance = new RecyclableSingletonFromDefaultConstructor<AquaTextFieldBorder>(AquaTextFieldBorder.class);
    public static AquaTextFieldBorder getTextFieldBorder() {
        return instance.get();
    }

    public AquaTextFieldBorder() {
    }

    public AquaTextFieldBorder(final AquaTextFieldBorder other) {
    }

    @Override
    public void paintBorder(final Component c, final Graphics g, int x, int y, int width, int height) {
        // The text field border is now a background
    }

    public void paintBackground(final JComponent c, final Graphics g) {
        if (c instanceof JTextComponent) {
            JTextComponent tc = (JTextComponent) c;
            int width = tc.getWidth();
            int height = tc.getHeight();

//        g.setColor(Color.MAGENTA);
//        g.drawRect(x, y, width - 1, height - 1);

            g.setColor(tc.getBackground());
            if (tc.isOpaque()) {
                g.fillRect(0, 0, width, height);
                return;
            }

            Painter p = getConfiguredPainter(tc);
            p.paint(g, 0, 0);

//        g.setColor(Color.ORANGE);
//        g.drawRect(x, y, width - 1, height - 1);
        }
    }

    public LayoutInfo getLayoutInfo(JTextComponent tc) {
        TextFieldLayoutConfiguration g = getLayoutConfiguration(tc);
        return painter.getLayoutInfo().getLayoutInfo(g);
    }

    public Shape getFocusRingOutline(JComponent c) {
        if (c instanceof JTextComponent) {
            JTextComponent tc = (JTextComponent) c;
            TextFieldLayoutConfiguration g = getLayoutConfiguration(tc);
            painter.configure(tc.getWidth(), tc.getHeight());
            return painter.getOutline(g);
        }
        return null;
    }

    /**
     * Return the text margin. The text margin is like a border margin, except that it is part of the text display and
     * therefore scrolls left and right along with the text. The result is that the full width of the text field
     * (exclusive of the actual border margin) is available for editing.
     */
    public int getTextMargin(JTextComponent tc) {
        // The cell editor test probably fails during the initial construction of the text component.
        // It should be performed again when the text field is added as a component of the table.
        if (AquaCellEditorPolicy.getInstance().isCellEditor(tc)) {
            return 1;
        } else {
            // The goal is to give the appearance of a specific left and right margin, but the portion of the margin
            // that is not reserved by the border is available for editing.
            TextFieldWidget widget = getWidget(tc);
            int target = widget == TextFieldWidget.TEXT_FIELD_ROUND ? 11 : 3;
            Insets s = getBorderInsets(tc);
            int averageInset = (s.left + s.right) / 2;
            return Math.max(0, target - averageInset);
        }
    }

    public Insets getBorderInsets(final Component c) {
        if (c instanceof JTextComponent) {
            JTextComponent tc = (JTextComponent) c;

            if (AquaCellEditorPolicy.getInstance().isCellEditor(tc)) {
                return new InsetsUIResource(0, 0, 0, 0);
            }

            Insetter s = getTextInsets(tc);
            if (s != null) {
                Insets2D n = s.asInsets2D();
                if (n != null) {
                    // We want to handle non-integer top and bottom insets by shifting the contents
                    int left = (int) Math.ceil(n.getLeft());
                    int right = (int) Math.ceil(n.getRight());
                    int top = (int) Math.floor(n.getTop());
                    int bottom = (int) Math.ceil(n.getBottom());
                    return new InsetsUIResource(top, left, bottom, right);
                }
            }
        }

        return new InsetsUIResource(3, 3, 3, 3);
    }

    public Insets2D getBorderInsets2D(final Component c) {
        if (c instanceof JTextComponent) {
            JTextComponent tc = (JTextComponent) c;

            if (AquaCellEditorPolicy.getInstance().isCellEditor(tc)) {
                return new Insets2D(0, 0, 0, 0);
            }

            Insetter s = getTextInsets(tc);
            if (s != null) {
                Insets2D n = s.asInsets2D();
                if (n != null) {
                    return new Insets2DUIResource(n);
                }
            }
        }

        return new Insets2DUIResource(3, 3, 3, 3);
    }

    public Insetter getTextInsets(JTextComponent tc) {
        TextFieldLayoutConfiguration g = getLayoutConfiguration(tc);
        return painter.getLayoutInfo().getTextFieldTextInsets(g);
    }

    public int getExtraHeight(JTextComponent tc) {
        TextFieldLayoutConfiguration g = getLayoutConfiguration(tc);
        TextFieldWidget widget = g.getWidget();
        switch (widget) {
            case TEXT_FIELD:
            case TEXT_FIELD_ROUND:
                return 3;
            default:
                return 0;
        }
    }

    protected Painter getConfiguredPainter(final JTextComponent tc) {
        int width = tc.getWidth();
        int height = tc.getHeight();
        painter.configure(width, height);
        TextFieldConfiguration tg = getConfiguration(tc);
        return painter.getPainter(tg);
    }

    protected TextFieldLayoutConfiguration getLayoutConfiguration(JTextComponent tc) {
        TextFieldWidget widget = getWidget(tc);
        Size size = AquaUtilControlSize.getUserSizeFrom(tc);
        AquaUIPainter.UILayoutDirection ld = AquaUtils.getLayoutDirection(tc);
        return new TextFieldLayoutConfiguration(widget, size, ld);
    }

    protected TextFieldConfiguration getConfiguration(JTextComponent tc) {
        TextFieldWidget widget = getWidget(tc);
        Size size = AquaUtilControlSize.getUserSizeFrom(tc);
        State state = getStateFor(tc);
        boolean isFocused = State.ACTIVE == state && tc.hasFocus();
        AquaUIPainter.UILayoutDirection ld = AquaUtils.getLayoutDirection(tc);
        return new TextFieldConfiguration(widget, size, state, isFocused, ld);
    }

    protected TextFieldWidget getWidget(final JTextComponent tc) {
        Object o = tc.getClientProperty(AquaTextFieldUI.TEXT_FIELD_STYLE_KEY);
        if ("round".equals(o)) {
            return TextFieldWidget.TEXT_FIELD_ROUND;
        }

        return TextFieldWidget.TEXT_FIELD;
    }

    protected State getStateFor(final JTextComponent tc) {
        if (!AquaFocusHandler.isActive(tc)) {
            return State.INACTIVE;
        }

        if (!tc.isEnabled()) {
            return State.DISABLED;
        }

        if (!tc.isEditable()) {
            return State.DISABLED;
        }

        return State.ACTIVE;
    }
}
