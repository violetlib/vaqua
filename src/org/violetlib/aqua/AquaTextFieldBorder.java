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
import java.awt.geom.Rectangle2D;
import javax.swing.*;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;

import org.jetbrains.annotations.*;
import org.violetlib.jnr.*;
import org.violetlib.jnr.Painter;
import org.violetlib.jnr.aqua.AquaUIPainter;
import org.violetlib.jnr.aqua.AquaUIPainter.Size;
import org.violetlib.jnr.aqua.AquaUIPainter.State;
import org.violetlib.jnr.aqua.AquaUIPainter.TextFieldWidget;
import org.violetlib.jnr.aqua.TextFieldConfiguration;
import org.violetlib.jnr.aqua.TextFieldLayoutConfiguration;

import static org.violetlib.aqua.OSXSystemProperties.macOS26;

/**
 * A border that is associated with a text field but can be attached to a text field or the scroll pane that
 * displays the text field. (This code is associated with text components with a natively rendered border or background.
 * Text field is the only example of such a text component.)
 */
public class AquaTextFieldBorder extends AquaTextComponentBorder {

    // This border is really a background. It paints as background, not as a border.
    // It needs to be a Border as a signal to the UI that the user has not installed a custom border.
    // It also provides the insets to the component UI.

    protected final @NotNull JTextField tf;

    /**
     * Create a border whose configuration is based on the specified text component.
     */
    public AquaTextFieldBorder(@NotNull JTextField tf) {
        super(tf);
        this.tf = tf;
    }

    @Override
    public void paintBackground(@NotNull Component c,
                                @NotNull Graphics g,
                                @Nullable Color background,
                                @Nullable Color borderColor) {
        if (AquaUtils.isCellComponent(c)) {
            return;
        }
        // An application-specified background supersedes the native background unless the text field has focus
        if (background != null && !(background instanceof UIResource) && !AquaFocusHandler.hasFocus(c)) {
            if (background.getAlpha() > 0) {
                g.setColor(background);
                int width = c.getWidth();
                int height = c.getHeight();
                Shape outline = getFocusRingOutline((JComponent)c);
                if (outline != null && !(outline instanceof Rectangle2D)) {
                    Graphics2D gg = (Graphics2D) g;
                    AquaUtils.fillAntiAliased(gg, outline);
                } else {
                    g.fillRect(0, 0, width, height);
                }
            }
        } else {
            Painter p = getConfiguredPainter(c);
            p.paint(g, 0, 0);
        }
    }

    public boolean isOpaque() {
        // Prior to macOS 26, text fields are not opaque regardless of their border.
        if (AquaPainting.getVersion() < macOS26) {
            return false;
        }

        // A rounded border is not opaque.
        TextFieldLayoutConfiguration g = getLayoutConfiguration();
        AquaUtils.configure(painter, null, tf, tf.getWidth(), tf.getHeight());
        Shape s = painter.getOutline(g);
        return s instanceof Rectangle2D;
    }

    public @NotNull LayoutInfo getLayoutInfo() {
        TextFieldLayoutConfiguration g = getLayoutConfiguration();
        return painter.getLayoutInfo().getLayoutInfo(g);
    }

    public @Nullable Shape getFocusRingOutline(@NotNull JComponent c) {

        // If the text component is scrollable, the text component defines the outline shape and the scroll pane
        // defines the outline size.

        JComponent cc = getComponentForFocusRing(c);
        if (cc != null) {
            TextFieldLayoutConfiguration g = getLayoutConfiguration();
            AquaUtils.configure(painter, null, tf, cc.getWidth(), cc.getHeight());
            return painter.getOutline(g);
        }
        return null;
    }

    private @Nullable JComponent getComponentForFocusRing(@NotNull JComponent c) {
        if (c == tf) {
            return tf;
        }
        if (c instanceof JScrollPane) {
            JScrollPane sp = (JScrollPane) c;
            if (sp.getViewport().getView() == tf) {
                return sp;
            }
        }
        return null;
    }

    /**
     * Return the text margin. The text margin is like a border margin, except that it is part of the text display and
     * therefore scrolls left and right along with the text. The result is that the full width of the text field
     * (exclusive of the actual border margin) is available for editing.
     */
    public int getTextMargin() {
        // The cell editor test probably fails during the initial construction of the text component.
        // It should be performed again when the text field is added as a component of the table.
        if (AquaCellEditorPolicy.getInstance().getCellStatus(tf) != null) {
            return 1;
        } else {
            // The goal is to give the appearance of a specific left and right margin, but the portion of the margin
            // that is not reserved by the border is available for editing.
            TextFieldWidget widget = getWidget();
            int target = widget == TextFieldWidget.TEXT_FIELD_ROUND ? 11 : 3;
            Insets s = getBorderInsets(tf);
            int averageInset = (s.left + s.right) / 2;
            return Math.max(0, target - averageInset);
        }
    }

    @Override
    public @NotNull Insets getBorderInsets(@NotNull Component c) {
        if (AquaCellEditorPolicy.getInstance().getCellStatus(tf) != null) {
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

    @Override
    public @NotNull Insets2D getBorderInsets2D(@NotNull Component c) {
        if (AquaCellEditorPolicy.getInstance().getCellStatus(tf) != null) {
            return new Insets2D(0, 0, 0, 0);
        }

        Insetter s = getTextInsets();
        Insets2D n = s.asInsets2D();
        if (n != null) {
            return new Insets2DUIResource(n);
        }

        return new Insets2DUIResource(3, 3, 3, 3);
    }

    @Override
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
        PaintingContext pc = PaintingContext.getDefault();
        AquaUtils.configure(painter, pc.appearance, tf, width, height);
        TextFieldConfiguration tg = getConfiguration();
        return painter.getPainter(tg);
    }

    protected @NotNull TextFieldLayoutConfiguration getLayoutConfiguration() {
        TextFieldWidget widget = getWidget();
        Size size = getControlSize();
        AquaUIPainter.UILayoutDirection ld = AquaUtils.getLayoutDirection(tf);
        return new TextFieldLayoutConfiguration(widget, size, ld);
    }

    protected @NotNull TextFieldConfiguration getConfiguration() {
        TextFieldWidget widget = getWidget();
        Size size = getControlSize();
        State state = getState();
        boolean isFocused = State.ACTIVE == state && tf.hasFocus();
        AquaUIPainter.UILayoutDirection ld = AquaUtils.getLayoutDirection(tf);
        return new TextFieldConfiguration(widget, size, state, isFocused, ld);
    }

    protected @NotNull Size getControlSize() {
        TextFieldWidget widget = getWidget();
        if (widget != null) {
            boolean isToolbar = AquaUtils.isOnToolbar(tf);
            return AquaUtils.getSize(tf, isToolbar, widget);
        }
        return null;
    }

    protected @NotNull TextFieldWidget getWidget() {
        Object o = tf.getClientProperty(AquaTextFieldUI.TEXT_FIELD_STYLE_KEY);
        if ("round".equals(o)) {
            return AquaUtils.isOnToolbar(tf) ? TextFieldWidget.TEXT_FIELD_ROUND_TOOLBAR : TextFieldWidget.TEXT_FIELD_ROUND;
        } else if (AquaUtils.isOnToolbar(tf)) {
            return TextFieldWidget.TEXT_FIELD_ROUND_TOOLBAR;
        }
        return TextFieldWidget.TEXT_FIELD;
    }
}
