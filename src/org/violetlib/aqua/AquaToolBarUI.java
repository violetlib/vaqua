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
import javax.swing.border.*;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.*;
import javax.swing.plaf.basic.BasicToolBarUI;

import org.violetlib.aqua.AquaUtils.RecyclableSingleton;
import org.violetlib.aqua.AquaUtils.RecyclableSingletonFromDefaultConstructor;

public class AquaToolBarUI extends BasicToolBarUI implements SwingConstants {

    private static RecyclableSingleton<ToolBarBorder> toolBarBorder = new RecyclableSingletonFromDefaultConstructor<ToolBarBorder>(ToolBarBorder.class);

    public static Border getToolBarBorder() {
        return toolBarBorder.get();
    }

    public static ComponentUI createUI(final JComponent c) {
        return new AquaToolBarUI();
    }

    private LayoutManager originalLayoutManager;

    protected void setBorderToNonRollover(final Component c) { }
    protected void setBorderToNormal(final Component c) { }
    protected void setBorderToRollover(final Component c) { }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        originalLayoutManager = toolBar.getLayout();
        if (originalLayoutManager instanceof UIResource) {
            toolBar.setLayout(new AquaToolBarLayout());
        }
    }

    @Override
    protected void uninstallDefaults() {
        LayoutManager lm = toolBar.getLayout();
        if (lm instanceof AquaToolBarLayout) {
            toolBar.setLayout(originalLayoutManager);
        }
        super.uninstallDefaults();
    }

    protected RootPaneContainer createFloatingWindow(final JToolBar toolbar) {
        final RootPaneContainer window = super.createFloatingWindow(toolbar);
        window.getRootPane().putClientProperty("Window.style", "small");
        return window;
    }

    /* ToolBarBorder and drag-off handle, based loosely on MetalBumps */
    @SuppressWarnings("serial") // Superclass is not serializable across versions
    static class ToolBarBorder extends AbstractBorder implements UIResource, javax.swing.SwingConstants {
        protected void fillHandle(final Graphics g, final int x1, final int y1, final int x2, final int y2, final boolean horizontal) {
            g.setColor(UIManager.getColor("ToolBar.borderHandleColor"));
            if (horizontal) {
                final int h = y2 - y1 - 2;
                g.fillRect(x1 + 2, y1 + 1, 1, h);
                g.fillRect(x1 + 5, y1 + 1, 1, h);
            } else {
                final int w = x2 - x1 - 2;
                g.fillRect(x1 + 1, y1 + 2, w, 1);
                g.fillRect(x1 + 1, y1 + 5, w, 1);
            }
        }

        public void paintBorder(final java.awt.Component c, final Graphics g, int x, int y, final int w, final int h) {
            g.translate(x, y);

            if (c.isOpaque()) {
                AquaUtils.fillRect(g, c, AquaUtils.ERASE_IF_TEXTURED|AquaUtils.ERASE_IF_VIBRANT, 0, 0, w - 1, h - 1);
            }

            final Color oldColor = g.getColor();

            final JToolBar jtb = (JToolBar)c;
            final ComponentOrientation orient = jtb.getComponentOrientation();
            final boolean horizontal = jtb.getOrientation() == SwingConstants.HORIZONTAL;

            if (jtb.isFloatable()) {
                if (horizontal) {
                    if (orient.isLeftToRight()) {
                        fillHandle(g, 2, 2, 10, h - 2, true);
                    } else {
                        fillHandle(g, w - 10, 2, w - 2, h - 2, true);
                    }
                } else {
                    fillHandle(g, 2, 2, w - 2, 10, false);
                }
            }

            g.setColor(oldColor);

            g.translate(-x, -y);
        }

        public Insets getBorderInsets(final java.awt.Component c) {
            final Insets borderInsets = new Insets(5, 5, 5, 5);
            return getBorderInsets(c, borderInsets);
        }

        public Insets getBorderInsets(final java.awt.Component c, final Insets borderInsets) {
            borderInsets.left = 4;
            borderInsets.right = 4;
            borderInsets.top = 2;
            borderInsets.bottom = 2;

            if (((JToolBar)c).isFloatable()) {
                if (((JToolBar)c).getOrientation() == HORIZONTAL) {
                    borderInsets.left = 12;
                    // We don't have to adjust for right-to-left
                } else { // vertical
                    borderInsets.top = 12;
                }
            }

            final Insets margin = ((JToolBar)c).getMargin();

            if (margin != null) {
                borderInsets.left += margin.left;
                borderInsets.top += margin.top;
                borderInsets.right += margin.right;
                borderInsets.bottom += margin.bottom;
            }

            return borderInsets;
        }

        public boolean isBorderOpaque() {
            return true;
        }
    }

    @Override
    public final void update(final Graphics g, final JComponent c) {
        if (c.isOpaque()) {
            AquaUtils.fillRect(g, c, AquaUtils.ERASE_IF_TEXTURED|AquaUtils.ERASE_IF_VIBRANT);
        }
        paint(g, c);
    }

    @Override
    protected void paintDragWindow(Graphics g) {
        super.paintDragWindow(g);
        if (dragWindow.getOrientation() == toolBar.getOrientation()) {
            toolBar.paint(g);
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
                    major.addGap(5);    // 2 is fine for large icon buttons, but regular buttons need a larger gap
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
