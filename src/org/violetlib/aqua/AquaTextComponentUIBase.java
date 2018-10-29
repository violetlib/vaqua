/*
 * Copyright (c) 2018 Alan Snyder.
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

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.jnr.Insets2D;
import org.violetlib.jnr.aqua.AquaUIPainter;

public class AquaTextComponentUIBase extends AquaTextComponentDelegatedUIBase implements FocusRingOutlineProvider, AquaComponentUI {

    private AquaFocusHandler handler;
    private boolean oldDragState = false;
    protected @NotNull BasicContextualColors colors;
    protected @Nullable AppearanceContext appearanceContext;

    protected AquaTextComponentUIBase(@NotNull AquaTextComponentUIDelegate delegate) {
        super(delegate);
        colors = AquaColors.TEXT_COLORS;
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        handler = createFocusHandler();
        editor.addFocusListener(handler);
        editor.addPropertyChangeListener(handler);
        AquaUtilControlSize.addSizePropertyListener(editor);
        AppearanceManager.installListener(editor);
    }

    @Override
    protected void uninstallListeners() {
        AppearanceManager.uninstallListener(editor);
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
        LookAndFeel.installProperty(editor, "opaque", false);
        configureAppearanceContext(null);
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
        if ("enabled".equals(prop)) {
            configureAppearanceContext(null);
        }
    }

    @Override
    public void appearanceChanged(@NotNull JComponent c, @NotNull AquaAppearance appearance) {
        configureAppearanceContext(appearance);
    }

    @Override
    public void activeStateChanged(@NotNull JComponent c, boolean isActive) {
        configureAppearanceContext(null);
    }

    protected void configureAppearanceContext(@Nullable AquaAppearance appearance) {
        if (appearance == null) {
            appearance = AppearanceManager.ensureAppearance(editor);
        }
        AquaUIPainter.State state = getState();
        appearanceContext = new AppearanceContext(appearance, state, false, false);
        AquaColors.installColors(editor, appearanceContext, colors);
        editor.repaint();
    }

    protected AquaUIPainter.State getState() {
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
        AquaAppearance appearance = AppearanceManager.registerCurrentAppearance(c);
        super.update(g, c);
        AppearanceManager.restoreCurrentAppearance(appearance);
    }

    protected void paintSafely(Graphics g) {

        Color background = getBackground();
        paintBackgroundSafely(g, background);

        // If the painter has specified a non-integer top inset, attempt to support that by translation.
        // This works with AquaTextComponentBorder, which will have rounded down the top inset.

        if (g instanceof Graphics2D) {
            Border b = editor.getBorder();
            if (b instanceof Border2D) {
                Border2D ab = (Border2D) b;
                Insets2D n = ab.getBorderInsets2D(editor);
                if (n != null) {
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
        }

        super.paintSafely(g);
    }

    protected @Nullable Color getBackground() {
        return editor.getBackground();
    }

    protected void paintBackgroundSafely(@NotNull Graphics g, @Nullable Color background) {
        Border b = editor.getBorder();
        if (b instanceof AquaTextComponentBorder) {
            AquaTextComponentBorder tb = (AquaTextComponentBorder) b;
            tb.paintBackground(editor, g, background);
        }
    }

    protected void paintBackground(Graphics g) {
        // we have already ensured that the background is painted to our liking
        // by paintBackgroundSafely(), called from paintSafely().
    }

    @Override
    protected Caret createCaret() {
        return new AquaCaret();
    }

    @Override
    public Shape getFocusRingOutline(JComponent c) {
        Border b = c.getBorder();
        if (b instanceof FocusRingOutlineProvider) {
            FocusRingOutlineProvider p = (FocusRingOutlineProvider) b;
            return p.getFocusRingOutline(c);
        }
        return new Rectangle(0, 0, c.getWidth(), c.getHeight());
    }
}
