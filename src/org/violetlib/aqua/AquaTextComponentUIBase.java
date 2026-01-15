/*
 * Copyright (c) 2018-2026 Alan Snyder.
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
import java.beans.PropertyChangeEvent;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.text.Caret;

import org.jetbrains.annotations.*;
import org.violetlib.jnr.Insets2D;
import org.violetlib.jnr.aqua.AquaUIPainter;

/**
 * Common code for text components.
 */
public abstract class AquaTextComponentUIBase
  extends AquaTextComponentDelegatedUIBase
  implements FocusRingOutlineProvider, AquaComponentUI {

    private AquaFocusHandler handler;
    private boolean oldDragState;
    protected @NotNull BasicContextualColors colors;
    protected @Nullable AppearanceContext appearanceContext;

    protected AquaTextComponentUIBase(@NotNull AquaTextComponentUIDelegate delegate) {
        super(delegate);
        colors = AquaColors.TEXT_COLORS;
    }

    @Override
    public void installUI(@NotNull JComponent c) {
        super.installUI(c);
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        handler = createFocusHandler();
        editor.addFocusListener(handler);
        editor.addPropertyChangeListener(handler);
        AquaUtilControlSize.addSizePropertyListener(editor);
        AppearanceManager.install(editor);
    }

    @Override
    protected void uninstallListeners() {
        AppearanceManager.uninstall(editor);
        AquaUtilControlSize.removeSizePropertyListener(editor);
        editor.removeFocusListener(handler);
        editor.removePropertyChangeListener(handler);
        handler = null;
        super.uninstallListeners();
    }

    @Override
    protected void installDefaults() {
        if (!GraphicsEnvironment.isHeadless()) {
            oldDragState = editor.getDragEnabled();
            editor.setDragEnabled(true);
        }
        super.installDefaults();
    }

    @Override
    protected void uninstallDefaults() {
        if (!GraphicsEnvironment.isHeadless()) {
            editor.setDragEnabled(oldDragState);
        }
        super.uninstallDefaults();
    }

    // Install a default keypress action which handles Cmd and Option keys properly
    @Override
    protected void installKeyboardActions() {
        super.installKeyboardActions();
        AquaKeyBindings bindings = AquaKeyBindings.instance();
        bindings.setDefaultAction(getKeymapName());
    }

    protected @NotNull AquaFocusHandler createFocusHandler() {
        return new AquaFocusHandler();
    }

    @Override
    protected void propertyChange(@NotNull PropertyChangeEvent evt) {
        super.propertyChange(evt);
        String prop = evt.getPropertyName();
        if ("enabled".equals(prop) || "editable".equals(prop)) {
            editor.repaint();
        }
    }

    @Override
    public void appearanceChanged(@NotNull JComponent c, @NotNull AquaAppearance appearance) {
    }

    @Override
    public void activeStateChanged(@NotNull JComponent c, boolean isActive) {
    }

    protected @NotNull AquaUIPainter.State getState() {
        if (editor.isEnabled()) {
            boolean isActive = AquaFocusHandler.isActive(editor);
            boolean hasFocus = AquaFocusHandler.hasFocus(editor);
            return isActive
              ? (hasFocus ? AquaUIPainter.State.ACTIVE_DEFAULT : AquaUIPainter.State.ACTIVE)
              : AquaUIPainter.State.INACTIVE;
        } else {
            return AquaUIPainter.State.DISABLED;
        }
    }

    @Override
    public void update(Graphics g, JComponent c) {
        AppearanceManager.withContext(g, c, this::paint);
    }

    public void paint(Graphics2D g, JComponent c, @NotNull PaintingContext pc) {
        AquaUIPainter.State state = getState();
        appearanceContext = new AppearanceContext(pc.appearance, state, false, false);
        AquaColors.installColors(editor, appearanceContext, colors);
        paint(g, c);
    }

    protected void paintSafely(@NotNull Graphics g) {

        Color background = getBackground();
        paintBackgroundSafely(g, background);

        // If the painter has specified a non-integer top inset, attempt to support that by translation.
        // This works with AquaTextComponentBorder, which will have rounded down the top inset.

        if (g instanceof Graphics2D) {
            Border b = editor.getBorder();
            Border2D ab = AquaBorderSupport.get(b, Border2D.class);
            if (ab != null) {
                Insets2D n = ab.getBorderInsets2D(editor);
                float top = n.getTop();
                float floor = (float) Math.floor(top);
                if (top - floor > 0.001f) {
                    Graphics2D gg = (Graphics2D) g.create();
                    gg.translate(0, top - floor);
                    super.paintSafely(gg);
                    gg.dispose();
                    return;
                }
            }
        }

        super.paintSafely(g);
    }

    protected @Nullable Color getBackground() {
        return editor.getBackground();
    }

    protected abstract void paintBackgroundSafely(@NotNull Graphics g, @Nullable Color background);

    protected void paintBackground(@NotNull Graphics g) {
        // we have already ensured that the background is painted to our liking
        // by paintBackgroundSafely(), called from paintSafely().
    }

    @Override
    protected @NotNull Caret createCaret() {
        return new AquaCaret();
    }

    @Override
    public @Nullable Shape getFocusRingOutline(@NotNull JComponent c) {
        Border b = c.getBorder();
        FocusRingOutlineProvider p = AquaBorderSupport.get(b, FocusRingOutlineProvider.class);
        if (p != null) {
            return p.getFocusRingOutline(c);
        }
        return new Rectangle(0, 0, c.getWidth(), c.getHeight());
    }
}
