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
import java.awt.event.*;
import java.awt.geom.*;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.*;
import javax.swing.text.*;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.aqua.AquaUtils.RecyclableSingleton;
import org.violetlib.aqua.AquaUtils.RecyclableSingletonFromDefaultConstructor;

public class AquaTextPasswordFieldUI extends AquaTextFieldUI {
    static final RecyclableSingleton<CapsLockSymbolPainter> capsLockPainter = new RecyclableSingletonFromDefaultConstructor<CapsLockSymbolPainter>(CapsLockSymbolPainter.class);
    static CapsLockSymbolPainter getCapsLockPainter() {
        return capsLockPainter.get();
    }

    public static ComponentUI createUI(JComponent c) {
        return new AquaTextPasswordFieldUI();
    }

    @Override
    protected String getPropertyPrefix() {
        return "PasswordField";
    }

    @Override
    protected @NotNull View createBasicView(@NotNull Element elem) {
        return new AquaPasswordView(elem);
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        getComponent().addKeyListener(getCapsLockPainter());
    }

    @Override
    protected void uninstallListeners() {
        getComponent().removeKeyListener(getCapsLockPainter());
        super.uninstallListeners();
    }

    @Override
    protected void paintBackgroundSafely(@NotNull Graphics g, @Nullable Color background) {
        super.paintBackgroundSafely(g, background);

        JTextComponent component = getComponent();
        if (component == null) return;
        if (!component.isFocusOwner()) return;

        boolean capsLockDown = Toolkit.getDefaultToolkit().getLockingKeyState(KeyEvent.VK_CAPS_LOCK);
        if (!capsLockDown) return;

        Rectangle bounds = component.getBounds();
        getCapsLockPainter().paintBorder(component, g, bounds.x, bounds.y, bounds.width, bounds.height);
    }

    protected class AquaPasswordView extends PasswordView {
        public AquaPasswordView(Element elem) {
            super(elem);
            setupDefaultEchoCharacter();
        }

        protected void setupDefaultEchoCharacter() {
            // this allows us to change the echo character in CoreAquaLookAndFeel.java
            Character echoChar = (Character)UIManager.getDefaults().get(getPropertyPrefix() + ".echoChar");
            if (echoChar != null) {
                LookAndFeel.installProperty(getComponent(), "echoChar", echoChar);
            }
        }
    }

    static class CapsLockSymbolPainter extends KeyAdapter implements Border, UIResource {
        protected Shape capsLockShape;
        protected Shape getCapsLockShape() {
            if (capsLockShape != null) return capsLockShape;

            RoundRectangle2D rect = new RoundRectangle2D.Double(0.5, 0.5, 16, 16, 8, 8);
            GeneralPath shape = new GeneralPath(rect);
            shape.setWindingRule(Path2D.WIND_EVEN_ODD);

            // arrow
            shape.moveTo( 8.50,  2.00);
            shape.lineTo( 4.00,  7.00);
            shape.lineTo( 6.25,  7.00);
            shape.lineTo( 6.25, 10.25);
            shape.lineTo(10.75, 10.25);
            shape.lineTo(10.75,  7.00);
            shape.lineTo(13.00,  7.00);
            shape.lineTo( 8.50,  2.00);

            // base line
            shape.moveTo(10.75, 12.00);
            shape.lineTo( 6.25, 12.00);
            shape.lineTo( 6.25, 14.25);
            shape.lineTo(10.75, 14.25);
            shape.lineTo(10.75, 12.00);

            return capsLockShape = shape;
        }

        @Override
        public Insets getBorderInsets(Component c) {
            return new Insets(0, 0, 0, 0);
        }

        @Override
        public boolean isBorderOpaque() {
            return false;
        }

        @Override
        public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {

            AquaAppearance appearance = AppearanceManager.ensureAppearance(c);
            Color color = appearance.getColor("capsLockIcon");

            g = g.create(width - 23, height / 2 - 8, 18, 18);
            g.setColor(color);
            ((Graphics2D)g).setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            ((Graphics2D)g).fill(getCapsLockShape());
            g.dispose();
        }

        @Override
        public void keyPressed(KeyEvent e) {
            update(e);
        }

        @Override
        public void keyReleased(KeyEvent e) {
            update(e);
        }

        void update(KeyEvent e) {
            if (KeyEvent.VK_CAPS_LOCK != e.getKeyCode()) return;
            e.getComponent().repaint();
        }
    }
}
