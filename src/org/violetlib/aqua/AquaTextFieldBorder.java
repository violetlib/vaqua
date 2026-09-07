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

import org.jetbrains.annotations.*;
import org.violetlib.jnr.*;
import org.violetlib.jnr.Painter;
import org.violetlib.jnr.aqua.AquaUIPainter.TextFieldWidget;
import org.violetlib.jnr.aqua.TextFieldLayoutConfiguration;

import static org.violetlib.aqua.OSXSystemProperties.macOS26;
import static org.violetlib.jnr.aqua.AquaUIPainter.TextFieldWidget.*;

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
    public void paintBackground(@NotNull JComponent c,
                                @NotNull Graphics g,
                                @Nullable Color background,
                                @Nullable Color borderColor) {
        if (AquaUtils.isCellComponent(c)) {
            return;
        }
        // An application-specified background supersedes the native background unless the text field has focus
        if (AquaUtils.isPriority(background) && !AquaFocusHandler.hasFocus(c)) {
            if (background.getAlpha() > 0) {
                g.setColor(background);
                int width = c.getWidth();
                int height = c.getHeight();
                Shape outline = getFocusRingOutline(c);
                if (outline != null && !(outline instanceof Rectangle2D)) {
                    Graphics2D gg = (Graphics2D) g;
                    AquaUtils.fillAntiAliased(gg, outline);
                } else {
                    g.fillRect(0, 0, width, height);
                }
            }
        } else {
            TextFieldWidget w = getWidget();
            Painter p = getConfiguredPainter(c, w);
            p.paint(g, 0, 0);
        }
    }

    public boolean isOpaque() {
        // Prior to macOS 26, text fields are not opaque regardless of their border.
        if (AquaPainting.getVersion() < macOS26) {
            return false;
        }

        // A rounded border is not opaque.
        TextFieldWidget w = getWidget();
        TextFieldLayoutConfiguration g = getLayoutConfiguration(w);
        AquaUtils.configure(painter, null, tf, tf.getWidth(), tf.getHeight());
        Shape s = painter.getOutline(g);
        return s instanceof Rectangle2D;
    }

    public @NotNull LayoutInfo getLayoutInfo() {
        TextFieldWidget w = getWidget();
        TextFieldLayoutConfiguration g = getLayoutConfiguration(w);
        return painter.getLayoutInfo().getLayoutInfo(g);
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
            int target = isTextFieldRounded(widget) ? 11 : 3;
            Insets s = getBorderInsets(tf);
            int averageInset = (s.left + s.right) / 2;
            return Math.max(0, target - averageInset);
        }
    }

    private boolean isTextFieldRounded(@NotNull TextFieldWidget widget)
    {
        return widget != TEXT_FIELD_SQUARE;
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

    protected int getExtraHeight() {
        TextFieldWidget widget = getWidget();
        switch (widget) {
            case TEXT_FIELD_SQUARE:
            case TEXT_FIELD_ROUND:
                return 3;
            default:
                return 0;
        }
    }

    @Override
    public @NotNull TextFieldWidget getWidget() {
        TextFieldWidget w = getConfiguredTextWidget();
        if (w == null) {
            w = getDefaultTextWidget();
        }
        if (w == TEXT_FIELD_ROUND && AquaUtils.isOnToolbar(tf)) {
            return TEXT_FIELD_ROUND_TOOLBAR;
        }
        return w;
    }
}
