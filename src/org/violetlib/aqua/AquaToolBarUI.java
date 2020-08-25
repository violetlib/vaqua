/*
 * Changes copyright (c) 2016-2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

/*
 * Copyright (c) 2011, 2014, Oracle and/or its affiliates. All rights reserved.
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
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.Serializable;
import javax.swing.*;
import javax.swing.border.AbstractBorder;
import javax.swing.border.Border;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicToolBarUI;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.jnr.aqua.AquaUIPainter;
import org.violetlib.jnr.aqua.ButtonLayoutConfiguration;
import org.violetlib.jnr.aqua.LayoutConfiguration;

public class AquaToolBarUI extends BasicToolBarUI implements SwingConstants, AquaComponentUI {

    public static ComponentUI createUI(JComponent c) {
        return new AquaToolBarUI();
    }

    public static Border getToolBarBorder(@NotNull JToolBar tb) {
        AquaToolBarUI ui = AquaUtils.getUI(tb, AquaToolBarUI.class);
        return ui != null ? ui.createBorder() : null;
    }

    private LayoutManager originalLayoutManager;

    protected @NotNull BasicContextualColors colors;
    protected @Nullable AppearanceContext appearanceContext;

    protected boolean isRendering;

    public AquaToolBarUI() {
        colors = AquaColors.CLEAR_CONTROL_COLORS;
    }

    @Override
    public void installUI(JComponent c) {
        super.installUI(c);
        LookAndFeel.installProperty(c, "opaque", false);
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        installBorder();
        originalLayoutManager = toolBar.getLayout();
        if (originalLayoutManager instanceof UIResource) {
            toolBar.setLayout(new AquaToolBarLayout());
        }
        toolBar.setFloatable(false);
        configureAppearanceContext(null);
    }

    protected void installBorder() {
        Border b = toolBar.getBorder();
        if (b == null || b instanceof UIResource) {
            toolBar.setBorder(createBorder());
        }
    }

    protected @NotNull Border createBorder() {
        return new ToolBarBorder();
    }

    @Override
    protected void uninstallDefaults() {
        LayoutManager lm = toolBar.getLayout();
        if (lm instanceof AquaToolBarLayout) {
            toolBar.setLayout(originalLayoutManager);
        }
        super.uninstallDefaults();
    }

    @Override
    protected void installListeners(){
        super.installListeners();
        AppearanceManager.installListener(toolBar);
    }

    @Override
    protected void uninstallListeners(){
        AppearanceManager.uninstallListener(toolBar);
        super.uninstallListeners();
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
            appearance = AppearanceManager.ensureAppearance(toolBar);
        }
        AquaUIPainter.State state = getState();
        appearanceContext = new AppearanceContext(appearance, state, false, false);
        AquaColors.installColors(toolBar, appearanceContext, colors);
        toolBar.repaint();
    }

    protected AquaUIPainter.State getState() {
        return AquaFocusHandler.isActive(toolBar) ? AquaUIPainter.State.ACTIVE : AquaUIPainter.State.INACTIVE;
    }

    protected void setBorderToNonRollover(Component c) { }
    protected void setBorderToNormal(Component c) { }
    protected void setBorderToRollover(Component c) { }

    protected RootPaneContainer createFloatingWindow(JToolBar toolbar) {
        RootPaneContainer window = super.createFloatingWindow(toolbar);
        window.getRootPane().putClientProperty("Window.style", "small");
        return window;
    }

    /* ToolBarBorder and drag-off handle, based loosely on MetalBumps */
    @SuppressWarnings("serial") // Superclass is not serializable across versions
    private class ToolBarBorder extends AbstractBorder implements UIResource, javax.swing.SwingConstants {

        public void paintBorder(Component c, Graphics g, int x, int y, int w, int h) {
            g.translate(x, y);

            if (!isRendering && c.isOpaque()) {
                AquaUtils.fillRect(g, c, AquaUtils.ERASE_IF_TEXTURED|AquaUtils.ERASE_IF_VIBRANT, 0, 0, w - 1, h - 1);
            }

            if (c instanceof JToolBar) {
                JToolBar tb = (JToolBar)c;
                if (tb.isFloatable()) {
                    paintHandle(tb, g, w, h);
                }
            }

            g.translate(-x, -y);
        }

        private void paintHandle(JToolBar tb, Graphics g, int w, int h) {
            Color oldColor = g.getColor();
            ComponentOrientation orient = tb.getComponentOrientation();
            boolean horizontal = tb.getOrientation() == SwingConstants.HORIZONTAL;

            if (horizontal) {
                if (orient.isLeftToRight()) {
                    fillHandle(g, 2, 2, 10, h - 2, true);
                } else {
                    fillHandle(g, w - 10, 2, w - 2, h - 2, true);
                }
            } else {
                fillHandle(g, 2, 2, w - 2, 10, false);
            }

            g.setColor(oldColor);
        }

        private void fillHandle(Graphics g, int x1, int y1, int x2, int y2, boolean horizontal) {
            g.setColor(UIManager.getColor("ToolBar.borderHandleColor"));
            if (horizontal) {
                int h = y2 - y1 - 2;
                g.fillRect(x1 + 2, y1 + 1, 1, h);
                g.fillRect(x1 + 5, y1 + 1, 1, h);
            } else {
                int w = x2 - x1 - 2;
                g.fillRect(x1 + 1, y1 + 2, w, 1);
                g.fillRect(x1 + 1, y1 + 5, w, 1);
            }
        }

        public Insets getBorderInsets(Component c) {
            Insets borderInsets = new Insets(5, 5, 5, 5);
            return getBorderInsets(c, borderInsets);
        }

        public Insets getBorderInsets(Component c, Insets borderInsets) {

            JToolBar tb = (JToolBar) c;

            boolean isTallFormat = isTallFormatToolBar(tb);

            borderInsets.left = 4;
            borderInsets.right = 4;
            borderInsets.top = 4;
            borderInsets.bottom = isTallFormat ? 0 : 4;

            if (tb.isFloatable()) {
                if (tb.getOrientation() == HORIZONTAL) {
                    borderInsets.left = 12;
                    // We don't have to adjust for right-to-left
                } else { // vertical
                    borderInsets.top = 12;
                }
            }

            Insets margin = tb.getMargin();

            if (margin != null) {
                borderInsets.left += margin.left;
                borderInsets.top += margin.top;
                borderInsets.right += margin.right;
                borderInsets.bottom += margin.bottom;
            }

            return borderInsets;
        }

        public boolean isBorderOpaque() {
            return false;
        }
    }

    /**
     * Determine if the toolbar or toolbar panel contains a tall format button. A tall format button is a toggle button
     * that uses the toolbar item button style. A tall format button should have no space below it in the toolbar.
     */
    public static boolean isTallFormatToolBar(@NotNull JComponent tb) {
        int count = tb.getComponentCount();
        for (int i = 0; i < count; i++) {
            Component c = tb.getComponent(i);
            if (c instanceof AbstractButton) {
                AbstractButton b = (AbstractButton) c;
                if (isTallFormatButton(b)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Determine if the specified button is a tall format button. A tall format button is a toggle button that uses the
     * toolbar item button style. A tall format button should have no space below it in the toolbar.
     */
    protected static boolean isTallFormatButton(AbstractButton b) {
        Border border = b.getBorder();

        if (border instanceof AquaButtonBorder) {
            AquaButtonBorder bb = (AquaButtonBorder) border;
            LayoutConfiguration g = bb.getLayoutConfiguration(b);
            if (g instanceof ButtonLayoutConfiguration) {
                ButtonLayoutConfiguration bg = (ButtonLayoutConfiguration) g;
                AquaUIPainter.ButtonWidget bw = bg.getButtonWidget();
                return bw == AquaUIPainter.ButtonWidget.BUTTON_TOOLBAR_ITEM;
            }
        }

        return false;
    }

    @Override
    public void update(Graphics g, JComponent c) {
        AquaAppearance appearance = AppearanceManager.registerCurrentAppearance(c);
        if (!isRendering && c.isOpaque()) {
            Color bc = c.getBackground();
            AquaUtils.fillRect(g, c, bc, AquaUtils.ERASE_IF_TEXTURED|AquaUtils.ERASE_IF_VIBRANT);
        }
        paint(g, c);
        AppearanceManager.restoreCurrentAppearance(appearance);
    }

    @Override
    protected void paintDragWindow(Graphics g) {
        g.setColor(dragWindow.getBackground());
        int w = dragWindow.getWidth();
        int h = dragWindow.getHeight();
        g.fillRect(0, 0, w, h);
        if (dragWindow.getOrientation() == toolBar.getOrientation()) {
            isRendering = true;
            toolBar.paint(g);
            isRendering = false;
        }
    }

    @Override
    protected MouseInputListener createDockingListener() {
        return new AquaDockingListener(toolBar);
    }

    protected class AquaDockingListener extends DockingListener {
        private boolean pressedInBumps = false;

        public AquaDockingListener(JToolBar t) {
            super(t);
        }

        public void mousePressed(MouseEvent e) {
            super.mousePressed(e);
            if (!toolBar.isEnabled()) {
                return;
            }
            pressedInBumps = false;
            Rectangle bumpRect = new Rectangle();

            if (toolBar.getOrientation() == JToolBar.HORIZONTAL) {
                int x = AquaUtils.isLeftToRight(toolBar) ? 0 : toolBar.getSize().width-14;
                bumpRect.setBounds(x, 0, 14, toolBar.getSize().height);
            } else {  // vertical
                bumpRect.setBounds(0, 0, toolBar.getSize().width, 14);
            }
            if (bumpRect.contains(e.getPoint())) {
                pressedInBumps = true;
                Point dragOffset = e.getPoint();
                if (!AquaUtils.isLeftToRight(toolBar)) {
                    dragOffset.x -= (toolBar.getSize().width
                                     - toolBar.getPreferredSize().width);
                }
                setDragOffset(dragOffset);
            }
        }

        public void mouseDragged(MouseEvent e) {
            if (pressedInBumps) {
                super.mouseDragged(e);
            }
        }
    }

    /**
     * Sets the offset of the mouse cursor inside the DragWindow.
     *
     * @param p the offset
     */
    protected void setDragOffset(Point p) {
        if (!GraphicsEnvironment.isHeadless()) {
            if (dragWindow == null) {
                dragWindow = createDragWindow(toolBar);
            }
            dragWindow.setOffset(p);
        }
    }

    @Override
    public Color getDockingColor() {
        return AquaUtils.getWindowBackground(toolBar);
    }

    @Override
    public Color getFloatingColor() {
        return getDockingColor();
    }

    private class AquaToolBarLayout
        implements LayoutManager2, Serializable, PropertyChangeListener, UIResource {

        GroupLayout gl;
        GroupLayout.SequentialGroup major;
        GroupLayout.ParallelGroup minor;
        boolean isConfigured;

        AquaToolBarLayout() {
        }

        private void configure() {
            if (!isConfigured) {
                isConfigured = true;
            }

            gl = new GroupLayout(toolBar);
            major = gl.createSequentialGroup();
            minor = gl.createParallelGroup(GroupLayout.Alignment.CENTER);

            int count = toolBar.getComponentCount();
            for (int i = 0; i < count; i++) {
                Component c = toolBar.getComponent(i);
                if (i > 0) {
                    major.addGap(5); // 2 is fine for large icon buttons, but regular buttons need a larger gap
                }
                major.addComponent(c);
                minor.addComponent(c);
            }

            if (toolBar.getOrientation() == JToolBar.HORIZONTAL) {
                gl.setHorizontalGroup(major);
                gl.setVerticalGroup(minor);
            } else {
                gl.setVerticalGroup(major);
                gl.setHorizontalGroup(minor);
            }
        }

        public void addLayoutComponent(String name, Component comp) {
            invalidateLayout(toolBar);
        }

        public void addLayoutComponent(Component comp, Object constraints) {
            invalidateLayout(toolBar);
        }

        public void removeLayoutComponent(Component comp) {
            invalidateLayout(toolBar);
        }

        public Dimension preferredLayoutSize(Container target) {
            configure();
            return gl.preferredLayoutSize(target);
        }

        public Dimension minimumLayoutSize(Container target) {
            configure();
            return gl.minimumLayoutSize(target);
        }

        public Dimension maximumLayoutSize(Container target) {
            configure();
            return gl.maximumLayoutSize(target);
        }

        public void layoutContainer(Container target) {
            configure();
            gl.layoutContainer(target);
        }

        public float getLayoutAlignmentX(Container target) {
            configure();
            return gl.getLayoutAlignmentX(target);
        }

        public float getLayoutAlignmentY(Container target) {
            configure();
            return gl.getLayoutAlignmentY(target);
        }

        public void invalidateLayout(Container target) {
            isConfigured = false;
        }

        public void propertyChange(PropertyChangeEvent e) {
            String name = e.getPropertyName();
            if( name.equals("orientation") ) {
                isConfigured = false;
            }
        }
    }
}
