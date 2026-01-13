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
import java.awt.event.MouseEvent;
import javax.swing.*;
import javax.swing.event.ChangeListener;
import javax.swing.event.MenuDragMouseEvent;
import javax.swing.event.MenuDragMouseListener;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.FontUIResource;
import javax.swing.plaf.basic.BasicMenuUI;

import org.jetbrains.annotations.*;

public class AquaMenuUI extends BasicMenuUI implements AquaComponentUI {

    public static ComponentUI createUI(JComponent x) {
        return new AquaMenuUI();
    }

    protected @Nullable JMenu menu;
    protected @NotNull BasicContextualColors colors;
    protected int textIconGap;
    protected @NotNull Insets menuBarMargin;

    public AquaMenuUI() {
        colors = AquaColors.getMenuColors();
    }

    @Override
    public void installUI(JComponent c)
    {
        menu = (JMenu) c;
        super.installUI(c);

        assert menu != null;
        textIconGap = 4;
        menuBarMargin = UIManager.getInsets("MenuBar.margin");
        if (menuBarMargin == null) {
            menuBarMargin = new Insets(0, 8, 0, 8);
        }
    }

    @Override
    public void uninstallUI(JComponent c)
    {
        super.uninstallUI(c);

        menu = null;
    }

    protected void installDefaults() {
        super.installDefaults();

        // [3361625]
        // In Aqua, the menu delay is 8 ticks, according to Eric Schlegel.
        // That makes the millisecond delay 8 ticks * 1 second / 60 ticks * 1000 milliseconds/second
        assert menu != null;
        menu.setDelay(8 * 1000 / 60);
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        assert menu != null;
        AppearanceManager.installListeners(menu);
    }

    @Override
    protected void uninstallListeners() {
        assert menu != null;
        AppearanceManager.uninstallListeners(menu);
        super.uninstallListeners();
    }

    @Override
    protected ChangeListener createChangeListener(JComponent c) {
        return new ChangeHandler((JMenu)c, this);
    }

    @Override
    public void appearanceChanged(@NotNull JComponent c, @NotNull AquaAppearance appearance) {
    }

    @Override
    public void activeStateChanged(@NotNull JComponent c, boolean isActive) {
    }

    @Override
    public void update(Graphics g, JComponent c) {
        paint(g, c);
    }

    @Override
    public void paint(Graphics g, JComponent c) {
        AppearanceSupport.withContext(g, c, this::paint);
    }

    public void paint(Graphics2D g, JComponent c, @NotNull PaintingContext pc) {
        assert menu != null;
        AppearanceContext appearanceContext = AquaMenuSupport.instance().getAppearanceContext(menu, pc.appearance);
        AquaColors.installColors(c, appearanceContext, colors);

        MenuDescription md = getMenuDescription();
        if (md != null) {
            if (AquaLookAndFeel.USE_VIBRANT_MENU) {
                Object o = getParent().getClientProperty(AquaPopupMenuUI.POP_UP_TRACKER);
                if (o instanceof MenuSelectionBoundsTracker) {
                    MenuSelectionBoundsTracker tracker = (MenuSelectionBoundsTracker) o;
                    tracker.paintingItem((JMenuItem) c, appearanceContext);
                }
            } else if (menu.isOpaque()) {
                Color background = md.colors.getBackground(appearanceContext);
                g.setColor(background);
                if (OSXSystemProperties.useInsetViewStyle()) {
                    AquaUtils.paintInsetMenuItemSelection((Graphics2D) g, 0, 0, menu.getWidth(), menu.getHeight());
                } else {
                    g.fillRect(0, 0, menu.getWidth(), menu.getHeight());
                }
            }
            AquaMenuSupport.instance().paintMenuItem(g, menu, appearanceContext, md);
        } else {
            // A top level menu (an element of a menu bar)
            md = getTopLevelMenuDescription();
            AquaMenuSupport.instance().paintMenuItem(g, menu, appearanceContext, md);
        }
    }

    private @Nullable MenuDescription getMenuDescription() {
        AquaPopupMenuUI ui = AquaUtils.getUI(menuItem.getParent(), AquaPopupMenuUI.class);
        return ui != null ? ui.getMenuDescription() : null;
    }

    private @NotNull MenuDescription getTopLevelMenuDescription() {
        Font f = getTopLevelMenuFont();

        MenuLayoutInfo layoutInfo = getTopLevelMenuLayoutInfo(f);
        Icon dummy = new EmptyIcon(0, 0);
        return new MenuDescription(layoutInfo, dummy, dummy, dummy, dummy, f, f, colors);
    }

    private @NotNull MenuLayoutInfo getTopLevelMenuLayoutInfo(@NotNull Font f) {
        FontMetrics fm = menuItem.getFontMetrics(f);
        return new MenuLayoutInfo(menuBarMargin, fm, textIconGap);
    }

    private @NotNull Font getTopLevelMenuFont() {
        Font f = UIManager.getFont("Menu.font");
        if (f == null) {
            return new FontUIResource(null, Font.PLAIN, 13);
        }
        return f;
    }

    private @NotNull JPopupMenu getParent() {
        assert menu != null;
        Object parent = menu.getParent();
        if (parent instanceof JPopupMenu) {
            return (JPopupMenu) parent;
        }
        throw new AssertionError("JMenu does not have a JPopupMenu parent");
    }

    @Override
    protected Dimension getPreferredMenuItemSize(JComponent c,
                                                 Icon ignoreCheckIcon,
                                                 Icon ignoreArrowIcon,
                                                 int ignoreDefaultTextIconGap) {
        assert menu != null;
        AquaPopupMenuUI ui = AquaUtils.getUI(menu.getParent(), AquaPopupMenuUI.class);
        if (ui != null) {
            // This method determines the preferred size of a menu item for this menu (as a submenu).
            MenuDescription md = ui.getMenuDescription();
            Dimension d = AquaMenuSupport.instance().getPreferredMenuItemSize(menu, md.layoutInfo);
            if (c.getParent() instanceof JMenuBar) {
                d.height = Math.max(d.height, 21);
            }
            return d;
        }
        // Presumably the menu is an element of a menu bar
        MenuLayoutInfo layoutInfo = getTopLevelMenuLayoutInfo(getTopLevelMenuFont());
        return AquaMenuSupport.instance().getPreferredMenuItemSize(menu, layoutInfo);
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
            if (menu == null || !menu.isEnabled()) {
                return;
            }

            MenuSelectionManager manager = e.getMenuSelectionManager();
            MenuElement[] path = e.getPath();

            // In Aqua, we always respect the menu's delay, if one is set.
            // Doesn't matter how the menu is clicked on or otherwise moused over.
            Point p = e.getPoint();
            if (p.x >= 0 && p.x < menu.getWidth() && p.y >= 0 && p.y < menu.getHeight()) {
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
                if (comp == null) {
                    manager.clearSelectedPath();
                }
            }
        }

        public void menuDragMouseEntered(MenuDragMouseEvent e) { }
        public void menuDragMouseExited(MenuDragMouseEvent e) { }
        public void menuDragMouseReleased(MenuDragMouseEvent e) { }
    }

    private static void appendPath(MenuElement[] path, MenuElement elem) {
        MenuElement[] newPath = new MenuElement[path.length + 1];
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
            if (menu == null || !menu.isEnabled()) {
                return;
            }

            MenuSelectionManager manager = MenuSelectionManager.defaultManager();
            MenuElement[] selectedPath = manager.getSelectedPath();

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
