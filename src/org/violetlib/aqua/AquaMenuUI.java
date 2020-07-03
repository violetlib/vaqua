/*
 * Copyright (c) 2018-2020 Alan Snyder.
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
import java.awt.event.MouseEvent;
import javax.swing.*;
import javax.swing.event.ChangeListener;
import javax.swing.event.MenuDragMouseEvent;
import javax.swing.event.MenuDragMouseListener;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicMenuUI;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class AquaMenuUI extends BasicMenuUI implements AquaComponentUI {

    public static ComponentUI createUI(JComponent x) {
        return new AquaMenuUI();
    }

    protected @NotNull BasicContextualColors colors;
    protected @Nullable AppearanceContext appearanceContext;

    public AquaMenuUI() {
        colors = AquaColors.getMenuColors();
    }

    protected void installDefaults() {
        super.installDefaults();

        // [3361625]
        // In Aqua, the menu delay is 8 ticks, according to Eric Schlegel.
        // That makes the millisecond delay 8 ticks * 1 second / 60 ticks * 1000 milliseconds/second
        ((JMenu)menuItem).setDelay(8 * 1000 / 60);
        configureAppearanceContext(null);
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        AppearanceManager.installListener(menuItem);
    }

    @Override
    protected void uninstallListeners() {
        AppearanceManager.uninstallListener(menuItem);
        super.uninstallListeners();
    }

    @Override
    protected ChangeListener createChangeListener(JComponent c) {
        return new ChangeHandler((JMenu)c, this);
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
            appearance = AppearanceManager.ensureAppearance(menuItem);
        }
        appearanceContext = AquaMenuSupport.instance().getAppearanceContext(menuItem, appearance);
        AquaColors.installColors(menuItem, appearanceContext, colors);
        menuItem.repaint();
    }

    @Override
    public void update(Graphics g, JComponent c) {
        AquaAppearance appearance = AppearanceManager.registerCurrentAppearance(c);
        super.update(g, c);
        AppearanceManager.restoreCurrentAppearance(appearance);
    }

    @Override
    public void paint(Graphics g, JComponent c) {
        appearanceContext = AquaMenuSupport.instance().getAppearanceContext(menuItem, null);
        Color background = colors.getBackground(appearanceContext);
        g.setColor(background);
        AquaUtils.paintInsetMenuItemSelection((Graphics2D)g, 0, 0, c.getWidth(), c.getHeight());
        //g.fillRect(0, 0, c.getWidth(), c.getHeight());
        AquaMenuSupport.instance().paintMenuItem((Graphics2D) g, menuItem, checkIcon, arrowIcon,
                appearanceContext, colors, defaultTextIconGap, acceleratorFont);
    }

    @Override
    protected Dimension getPreferredMenuItemSize(JComponent c, Icon localCheckIcon, Icon localArrowIcon, int localDefaultTextIconGap) {
        Dimension d = AquaMenuSupport.instance().getPreferredMenuItemSize(c, localCheckIcon, localArrowIcon, localDefaultTextIconGap, acceleratorFont);
        if (c.getParent() instanceof JMenuBar) {
            d.height = Math.max(d.height, 21);
        }
        return d;
    }

    @Override
    protected MouseInputListener createMouseInputListener(JComponent c) {
        return new AquaMouseInputHandler();
    }

    @Override
    protected MenuDragMouseListener createMenuDragMouseListener(JComponent c) {
        //return super.createMenuDragMouseListener(c);
        return new MenuDragMouseHandler();
    }

    class MenuDragMouseHandler implements MenuDragMouseListener {
        @Override
        public void menuDragMouseDragged(MenuDragMouseEvent e) {
            if (!menuItem.isEnabled()) {
                return;
            }

            MenuSelectionManager manager = e.getMenuSelectionManager();
            MenuElement path[] = e.getPath();

            // In Aqua, we always respect the menu's delay, if one is set.
            // Doesn't matter how the menu is clicked on or otherwise moused over.
            Point p = e.getPoint();
            if (p.x >= 0 && p.x < menuItem.getWidth() && p.y >= 0 && p.y < menuItem.getHeight()) {
                JMenu menu = (JMenu)menuItem;
                MenuElement selectedPath[] = manager.getSelectedPath();
                if (!(selectedPath.length > 0 && selectedPath[selectedPath.length - 1] == menu.getPopupMenu())) {
                    if (menu.getDelay() == 0) {
                        appendPath(path, menu.getPopupMenu());
                    } else {
                        manager.setSelectedPath(path);
                        setupPostTimer(menu);
                    }
                }
            } else if (e.getID() == MouseEvent.MOUSE_RELEASED) {
                Component comp = manager.componentForPoint(e.getComponent(), e.getPoint());
                if (comp == null) manager.clearSelectedPath();
            }
        }

        public void menuDragMouseEntered(MenuDragMouseEvent e) { }
        public void menuDragMouseExited(MenuDragMouseEvent e) { }
        public void menuDragMouseReleased(MenuDragMouseEvent e) { }
    }

    private static void appendPath(MenuElement[] path, MenuElement elem) {
        MenuElement newPath[] = new MenuElement[path.length + 1];
        System.arraycopy(path, 0, newPath, 0, path.length);
        newPath[path.length] = elem;
        MenuSelectionManager.defaultManager().setSelectedPath(newPath);
    }

    protected class AquaMouseInputHandler extends MouseInputHandler {
        /**
         * Invoked when the cursor enters the menu. This method sets the selected
         * path for the MenuSelectionManager and handles the case
         * in which a menu item is used to pop up an additional menu, as in a
         * hierarchical menu system.
         *
         * @param e the mouse event; not used
         */
        @Override
        public void mouseEntered(MouseEvent e) {
            JMenu menu = (JMenu)menuItem;
            if (!menu.isEnabled()) return;

            MenuSelectionManager manager = MenuSelectionManager.defaultManager();
            MenuElement selectedPath[] = manager.getSelectedPath();

            // In Aqua, we always have a menu delay, regardless of where the menu is.
            if (!(selectedPath.length > 0 && selectedPath[selectedPath.length - 1] == menu.getPopupMenu())) {
                // the condition below prevents from activating menu in other frame
                if (!menu.isTopLevelMenu() || (selectedPath.length > 0 &&
                        selectedPath[0] == menu.getParent())) {
                    if (menu.getDelay() == 0) {
                        appendPath(getPath(), menu.getPopupMenu());
                    } else {
                        manager.setSelectedPath(getPath());
                        setupPostTimer(menu);
                    }
                }
            }
        }
    }
}
