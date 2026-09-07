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
import javax.swing.text.JTextComponent;

import org.jetbrains.annotations.*;
import org.violetlib.jnr.Insets2D;
import org.violetlib.jnr.Insets2DUIResource;
import org.violetlib.jnr.Insetter;
import org.violetlib.jnr.Painter;
import org.violetlib.jnr.aqua.AquaUIPainter;
import org.violetlib.jnr.aqua.AquaUIPainter.State;
import org.violetlib.jnr.aqua.AquaUIPainter.TextFieldWidget;
import org.violetlib.jnr.aqua.TextFieldConfiguration;
import org.violetlib.jnr.aqua.TextFieldLayoutConfiguration;

import static org.violetlib.aqua.OSXSystemProperties.macOS26;
import static org.violetlib.jnr.aqua.AquaUIPainter.TextFieldWidget.TEXT_FIELD_ROUND;
import static org.violetlib.jnr.aqua.AquaUIPainter.TextFieldWidget.TEXT_FIELD_SQUARE;

/**
 * A border that is associated with a text component but can be attached to a text component or the scroll pane that
 * displays the text component.
 */
public class AquaTextComponentBorder extends AquaBorder implements AquaBackgroundBorder, FocusRingOutlineProvider, Border2D {

    // This border is really a background. It paints as background, not as a border.
    // It needs to be a Border as a signal to the UI that the user has not installed a custom border.
    // It also provides the insets to the component UI.

    protected final @NotNull JTextComponent tc;
    private final @Nullable JScrollPane scrollPane;

    public AquaTextComponentBorder(@NotNull JTextComponent tc, @Nullable JScrollPane sp)
    {
        this.tc = tc;
        this.scrollPane = sp;
    }

    /**
     * Create a border whose configuration is based on the specified text component.
     */
    protected AquaTextComponentBorder(@NotNull JTextComponent tc) {
        this.tc = tc;
        this.scrollPane = null;
    }

    @Override
    protected void paint(JComponent c, Graphics2D g, int x, int y, int width, int height) {
        // The border is a background. It is not painted as a border.
    }

    @Override
    public void paintBackground(@NotNull JComponent c,
                                @NotNull Graphics g,
                                @Nullable Color background,
                                @Nullable Color borderColor) {
        if (AquaUtils.isCellComponent(c)) {
            return;
        }

        // An application-specified background supersedes the native background unless the component has focus
        if (AquaUtils.isPriority(background) && !AquaFocusHandler.hasFocus(tc)) {
            paintBasicBackground(c, g, background, borderColor);
        } else {
            TextFieldWidget w = getWidget();
            if (w == null && c instanceof JScrollPane) {
                w = TEXT_FIELD_SQUARE;
            }
            if (w != null) {
                paintNativeBackground(c, g, w);
            } else {
                paintBasicBackground(c, g, background, borderColor);
            }
        }
    }

    /**
     * Paint a background that does not involve native code or appearance.
     */
    protected void paintBasicBackground(@NotNull JComponent c,
                                        @NotNull Graphics g,
                                        @Nullable Color background,
                                        @Nullable Color borderColor)
    {
        Shape outline = null;
        if (background != null && background.getAlpha() > 0 || borderColor != null) {
            outline = getFocusRingOutline(c);
        }
        if (background != null && background.getAlpha() > 0) {
            g.setColor(background);
            if (outline != null && !(outline instanceof Rectangle2D)) {
                Graphics2D gg = (Graphics2D) g;
                AquaUtils.fillAntiAliased(gg, outline);
            } else {
                int width = c.getWidth();
                int height = c.getHeight();
                g.fillRect(0, 0, width, height);
            }
        }
        if (borderColor != null) {
            if (outline != null && !(outline instanceof Rectangle2D)) {
                Graphics2D gg = (Graphics2D) g.create();
                gg.setStroke(new BasicStroke(1));
                AquaUtils.drawAntiAliased(gg, outline);
                gg.dispose();
            } else {
                g.setColor(borderColor);
                int x = 0;
                int y = 0;
                int width = c.getWidth();
                int height = c.getHeight();
                g.fillRect(x, y, width, 1);
                g.fillRect(x, y+1, 1, height-2);
                g.fillRect(x, y + height - 1, width, 1);
                g.fillRect(x + width - 1, y+1, 1, height-2);
            }
        }
    }

    protected void paintNativeBackground(@NotNull JComponent c, @NotNull Graphics g, @NotNull TextFieldWidget w)
    {
        Painter p = getConfiguredPainter(c, w);
        p.paint(g, 0, 0);
    }

    protected @NotNull Painter getConfiguredPainter(@NotNull JComponent c, @NotNull TextFieldWidget w) {
        int width = c.getWidth();
        int height = c.getHeight();
        PaintingContext pc = AppearanceManager.getPaintingContext(c);
        AquaUtils.configure(painter, pc.appearance, tc, width, height);
        TextFieldConfiguration tg = getConfiguration(w);
        return painter.getPainter(tg);
    }

    protected @NotNull TextFieldLayoutConfiguration getLayoutConfiguration(@NotNull TextFieldWidget w) {
        AquaUIPainter.Size size = getControlSize();
        AquaUIPainter.UILayoutDirection ld = AquaUtils.getLayoutDirection(tc);
        return new TextFieldLayoutConfiguration(w, size, ld);
    }

    protected @NotNull TextFieldConfiguration getConfiguration(@NotNull TextFieldWidget w) {
        AquaUIPainter.Size size = getControlSize();
        State state = getState();
        boolean isFocused = State.ACTIVE == state && tc.hasFocus();
        AquaUIPainter.UILayoutDirection ld = AquaUtils.getLayoutDirection(tc);
        return new TextFieldConfiguration(w, size, state, isFocused, ld);
    }

    protected @NotNull AquaUIPainter.Size getControlSize() {
        TextFieldWidget w = getWidget();
        boolean isToolbar = AquaUtils.isOnToolbar(tc);
        return AquaUtils.getSize(tc, isToolbar, w);
    }

    public @Nullable Shape getFocusRingOutline(@NotNull JComponent c) {

        // If the text component is scrollable, the text component defines the outline shape and the scroll pane
        // defines the outline size.

        JComponent cc = getComponentForFocusRing(c);
        if (cc != null) {
            TextFieldWidget w = getWidget();
            if (w != null) {
                TextFieldLayoutConfiguration g = getLayoutConfiguration(w);
                AquaUtils.configure(painter, null, tc, cc.getWidth(), cc.getHeight());
                return painter.getOutline(g);
            }
            return AquaDefaultFocusRingProvider.getDefaultFocusRing(c);
        }
        return null;
    }

    private @Nullable JComponent getComponentForFocusRing(@NotNull Component c) {
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

    @Override
    public @NotNull Insets getBorderInsets(@NotNull Component c) {

        if (AquaCellEditorPolicy.getInstance().getCellStatus(tc) != null) {
            return new InsetsUIResource(0, 0, 0, 0);
        }

        if (scrollPane != null) {
            if (isRoundedBorder()) {
                Insets s = getTextInsets().asInsets();
                if (s != null) {
                    return s;
                }
                return new InsetsUIResource(3, 3, 3, 3);
            }
            return new InsetsUIResource(1, 1, 1, 1);
        }

        Insets2D s = getTextInsets().asInsets2D();
        if (s != null) {
            // We want to handle non-integer top and bottom insets by shifting the contents
            int left = (int) Math.ceil(s.getLeft());
            int right = (int) Math.ceil(s.getRight());
            int top = (int) Math.floor(s.getTop());
            int bottom = (int) Math.ceil(s.getBottom());
            return new InsetsUIResource(top, left, bottom, right);
        }

        return new InsetsUIResource(3, 3, 3, 3);
    }

    @Override
    public @NotNull Insets2D getBorderInsets2D(@NotNull Component c) {
        if (AquaCellEditorPolicy.getInstance().getCellStatus(tc) != null) {
            return new Insets2D(0, 0, 0, 0);
        }
        Insets2D s = getTextInsets().asInsets2D();
        if (s != null) {
            return new Insets2DUIResource(s);
        }
        return new Insets2DUIResource(3, 3, 3, 3);
    }

    public @NotNull Insetter getTextInsets() {
        TextFieldWidget w = getWidget();
        if (w != null) {
            TextFieldLayoutConfiguration g = getLayoutConfiguration(w);
            return painter.getLayoutInfo().getTextFieldTextInsets(g);
        }
        return Insetter.trivial();
    }

    protected @NotNull State getState() {
        if (!AquaFocusHandler.isActive(tc)) {
            return tc.isEnabled() ? State.INACTIVE : State.DISABLED_INACTIVE;
        } else {
            return tc.isEnabled() ? State.ACTIVE : State.DISABLED;
        }
    }

    /**
     * If the text component should use a native background, return the corresponding widget.
     * @return the widget, or null if a native background should not be used.
     */
    public @Nullable TextFieldWidget getWidget()
    {
        if (AquaPainting.getVersion() < macOS26) {
            // Older releases support rounded text fields only at fixed heights.
            return null;
        }

        TextFieldWidget w = getConfiguredTextWidget();
        if (w != null) {
            return w;
        }
        if (tc instanceof JTextArea) {
            return TEXT_FIELD_ROUND;
        }
        return null;
    }

    /**
     * Return the configured widget for this component, ignoring toolbar status.
     * @return the widget, or null if no style has been configured.
     */
    protected @Nullable TextFieldWidget getConfiguredTextWidget()
    {
        Object o = tc.getClientProperty(AquaTextFieldUI.TEXT_FIELD_STYLE_KEY);
        if (o != null) {
            if (o.equals("round")) {
                return TEXT_FIELD_ROUND;
            }
            if (o.equals("square")) {
                return TEXT_FIELD_SQUARE;
            }
            return getDefaultTextWidget();
        }
        return null;
    }

    protected @NotNull TextFieldWidget getDefaultTextWidget()
    {
        return AquaPainting.getVersion() < macOS26 ? TEXT_FIELD_SQUARE : TEXT_FIELD_ROUND;
    }

    protected boolean isRoundedBorder()
    {
        TextFieldWidget w = getWidget();
        return w == TEXT_FIELD_ROUND;
    }
}
