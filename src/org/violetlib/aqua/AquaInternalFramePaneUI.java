/*
 * Copyright (c) 2018-2021 Alan Snyder.
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
import java.awt.event.*;
import java.beans.PropertyVetoException;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.*;
import javax.swing.plaf.basic.BasicDesktopPaneUI;

import org.jetbrains.annotations.NotNull;

public class AquaInternalFramePaneUI extends BasicDesktopPaneUI implements MouseListener, AquaComponentUI {

    JComponent fDock;
    DockLayoutManager fLayoutMgr;

    public static ComponentUI createUI(JComponent c) {
        return new AquaInternalFramePaneUI();
    }

    public void update(Graphics g, JComponent c) {
        AppearanceManager.registerCurrentAppearance(c);
        super.update(g, c);
    }

    public void installUI(JComponent c) {
        super.installUI(c);
        fLayoutMgr = new DockLayoutManager();
        c.setLayout(fLayoutMgr);

        c.addMouseListener(this);
    }

    public void uninstallUI(JComponent c) {
        c.removeMouseListener(this);

        if (fDock != null) {
            c.remove(fDock);
            fDock = null;
        }
        if (fLayoutMgr != null) {
            c.setLayout(null);
            fLayoutMgr = null;
        }
        super.uninstallUI(c);
    }

    @Override
    public void appearanceChanged(@NotNull JComponent c, @NotNull AquaAppearance appearance) {
    }

    @Override
    public void activeStateChanged(@NotNull JComponent c, boolean isActive) {
    }

    // Our superclass hardcodes DefaultDesktopManager - how rude!
    protected void installDesktopManager() {
        if (desktop.getDesktopManager() == null) {
            desktopManager = new AquaDockingDesktopManager();
            desktop.setDesktopManager(desktopManager);
        }
    }

    protected void uninstallDesktopManager() {
        DesktopManager manager = desktop.getDesktopManager();
        if (manager instanceof AquaDockingDesktopManager) {
            desktop.setDesktopManager(null);
        }
    }

    JComponent getDock() {
        if (fDock == null) {
            fDock = new Dock(desktop);
            desktop.add(fDock, new Integer(399)); // Just below the DRAG_LAYER
        }
        return fDock;
    }

    class DockLayoutManager implements LayoutManager {
        public void addLayoutComponent(String name, Component comp) {
        }

        public void removeLayoutComponent(Component comp) {
        }

        public Dimension preferredLayoutSize(Container parent) {
            return parent.getSize();
        }

        public Dimension minimumLayoutSize(Container parent) {
            return parent.getSize();
        }

        public void layoutContainer(Container parent) {
            if (fDock != null) ((Dock)fDock).updateSize();
        }
    }

    @SuppressWarnings("serial") // Superclass is not serializable across versions
    class Dock extends JComponent implements Border {
        public static final int DOCK_EDGE_SLACK = 8;

        Dock(JComponent parent) {
            setBorder(this);
            setLayout(new FlowLayout(FlowLayout.CENTER, 0, 0));
            setVisible(false);
        }

        public void removeNotify() {
            fDock = null;
            super.removeNotify();
        }

        void updateSize() {
            Dimension d = getPreferredSize();
            setBounds((getParent().getWidth() - d.width) / 2, getParent().getHeight() - d.height, d.width, d.height);
        }

        public Component add(Component c) {
            super.add(c);
            if (!isVisible()) {
                setVisible(true);
            }

            updateSize();
            validate();
            return c;
        }

        public void remove(Component c) {
            super.remove(c);
            if (getComponentCount() == 0) {
                setVisible(false);
            } else {
                updateSize();
                validate();
            }
        }

        public Insets getBorderInsets(Component c) {
            return new Insets(DOCK_EDGE_SLACK / 4, DOCK_EDGE_SLACK, 0, DOCK_EDGE_SLACK);
        }

        public boolean isBorderOpaque() {
            return false;
        }

        public void paintBorder(Component c, Graphics g, int x, int y, int w, int h) {
            if (!(g instanceof Graphics2D)) return;
            Graphics2D g2d = (Graphics2D)g;

            int height = getHeight();
            int width = getWidth();

            Object priorAA = g2d.getRenderingHint(RenderingHints.KEY_ANTIALIASING);
            g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

            g2d.setColor(UIManager.getColor("DesktopIcon.borderColor"));
            g2d.fillRoundRect(4, 4, width - 9, height + DOCK_EDGE_SLACK, DOCK_EDGE_SLACK, DOCK_EDGE_SLACK);

            g2d.setColor(UIManager.getColor("DesktopIcon.borderRimColor"));
            g2d.setStroke(new BasicStroke(2.0f));
            g2d.drawRoundRect(4, 4, width - 9, height + DOCK_EDGE_SLACK, DOCK_EDGE_SLACK, DOCK_EDGE_SLACK);

            if (priorAA != null) g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, priorAA);
        }
    }

    @SuppressWarnings("serial") // JDK implementation class
    class AquaDockingDesktopManager extends AquaInternalFrameManager {
        public void openFrame(JInternalFrame f) {
            JInternalFrame.JDesktopIcon desktopIcon = f.getDesktopIcon();
            Container dock = desktopIcon.getParent();
            if (dock == null) return;

            if (dock.getParent() != null) dock.getParent().add(f);
            removeIconFor(f);
        }

        public void deiconifyFrame(JInternalFrame f) {
            JInternalFrame.JDesktopIcon desktopIcon = f.getDesktopIcon();
            Container dock = desktopIcon.getParent();
            if (dock == null) return;

            if (dock.getParent() != null) dock.getParent().add(f);
            removeIconFor(f);
            // <rdar://problem/3712485> removed f.show(). show() is now deprecated and
            // it wasn't sending our frame to front nor selecting it. Now, we move it
            // to front and select it manualy. (vm)
            f.moveToFront();
            try {
                f.setSelected(true);
            } catch(PropertyVetoException pve) { /* do nothing */ }
        }

        public void iconifyFrame(JInternalFrame f) {
            JInternalFrame.JDesktopIcon desktopIcon = f.getDesktopIcon();
            // paint the frame onto the icon before hiding the frame, else the contents won't show
            ((AquaInternalFrameDockIconUI)desktopIcon.getUI()).updateIcon();
            super.iconifyFrame(f);
        }

        void addIcon(Container c, JInternalFrame.JDesktopIcon desktopIcon) {
            DesktopPaneUI ui = ((JDesktopPane)c).getUI();
            ((AquaInternalFramePaneUI)ui).getDock().add(desktopIcon);
        }
    }

    public void mousePressed(MouseEvent e) {
        JInternalFrame selectedFrame = desktop.getSelectedFrame();
        if (selectedFrame != null) {
            try {
                selectedFrame.setSelected(false);
            } catch (PropertyVetoException ex) {}
            desktop.getDesktopManager().deactivateFrame(selectedFrame);
        }
    }

    public void mouseReleased(MouseEvent e) { }
    public void mouseClicked(MouseEvent e) { }
    public void mouseEntered(MouseEvent e) { }
    public void mouseExited(MouseEvent e) { }
}
