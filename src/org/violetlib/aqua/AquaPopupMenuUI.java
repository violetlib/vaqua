/*
 * Changes Copyright (c) 2015-2021 Alan Snyder.
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
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicPopupMenuUI;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.jnr.aqua.AquaUIPainter;

import static org.violetlib.aqua.AquaContextualPopup.getContextualMenuBorder;

/**
 * UI for all kinds of pop up menus.
 */
public class AquaPopupMenuUI extends BasicPopupMenuUI implements AquaComponentUI {

    public static final String POP_UP_SCREEN_BOUNDS = "Aqua.PopupMenu.ScreenBounds";
    public static final String POP_UP_SELECTED_REGION = "Aqua.PopupMenu.SelectedRegion";
    public static final String POP_UP_SELECTED_REGION_LOCATION = "Aqua.PopupMenu.SelectedRegionLocation";

    public static final int ORDINARY_CONTEXTUAL_MENU_STYLE = 0;
    public static final int SIMPLE_CONTEXTUAL_MENU_STYLE = 1;   // used in macOS 10.14+ for editable combo boxes
    public static final int FANCY_CONTEXTUAL_MENU_STYLE = 2;    // rounded corners and fancy scrolling

    public static ComponentUI createUI(JComponent x) {
        return new AquaPopupMenuUI();
    }

    private static Object HIDE_POPUP_KEY;

    private AquaContextualPopup cp;
    private ScrollingMouseListener scrollingMouseListener = new ScrollingMouseListener();
    protected @NotNull BasicContextualColors colors;
    protected @Nullable AppearanceContext appearanceContext;

    public AquaPopupMenuUI() {
        colors = AquaColors.getMenuColors();
    }

    @Override
    public void installDefaults() {
        super.installDefaults();
        LookAndFeel.installProperty(popupMenu, "opaque", false);
        configureAppearanceContext(null);
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        popupMenu.addMouseListener(scrollingMouseListener);
        popupMenu.addMouseMotionListener(scrollingMouseListener);
        popupMenu.addMouseWheelListener(scrollingMouseListener);
        AppearanceManager.installListeners(popupMenu);
    }

    @Override
    protected void uninstallListeners() {
        AppearanceManager.uninstallListeners(popupMenu);
        popupMenu.removeMouseListener(scrollingMouseListener);
        popupMenu.removeMouseMotionListener(scrollingMouseListener);
        popupMenu.removeMouseWheelListener(scrollingMouseListener);
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
            appearance = AppearanceManager.ensureAppearance(popupMenu);
        }
        AquaUIPainter.State state = AquaUIPainter.State.ACTIVE;
        appearanceContext = new AppearanceContext(appearance, state, false, false);
        AquaColors.installColors(popupMenu, appearanceContext, colors);
        popupMenu.repaint();
    }

    @Override
    public void update(Graphics g, JComponent c) {
        AppearanceManager.registerCurrentAppearance(c);
        super.update(g, c);
    }

    public boolean isPopupTrigger(MouseEvent e) {
        // Use the awt popup trigger code since this only runs on our OS!
        return e.isPopupTrigger();
    }

    @Override
    public Popup getPopup(JPopupMenu popup, int x, int y) {

        // Unfortunately, the path by which the pop up location is transmitted from the popup to the popup menu UI is
        // interrupted by code in JPopupMenu that thinks it knows more than we do about where popup menus should appear
        // on the screen. The following is a workaround.

        int width = 0;  // 0 => fit to content
        int height = 0; // 0 => fit to content

        Object o = popup.getClientProperty(POP_UP_SCREEN_BOUNDS);
        if (o instanceof Rectangle) {
            Rectangle r = (Rectangle) o;
            x = r.x;
            y = r.y;
            width = r.width;
            height = r.height;
        }

        Component owner = popup.getInvoker();
        int menuStyle = getContextualMenuStyle(owner);

        if (menuStyle == FANCY_CONTEXTUAL_MENU_STYLE) {

            Rectangle selectedRegion = null;
            Point selectedRegionLocation = null;

            {
                Object sr = popup.getClientProperty(POP_UP_SELECTED_REGION);
                if (sr instanceof Rectangle) {
                    selectedRegion = (Rectangle) sr;
                    Object loc = popup.getClientProperty(POP_UP_SELECTED_REGION_LOCATION);
                    if (loc instanceof Point) {
                        selectedRegionLocation = (Point) loc;
                    }
                }
            }

            cp = new AquaContextualPopup(popup, owner, selectedRegion, selectedRegionLocation, x, y, width, height);
            return cp.getPopup();

        } else if (menuStyle == SIMPLE_CONTEXTUAL_MENU_STYLE) {

            Border border = getContextualMenuBorder();
            AquaBasicPopupMenuWrapper wrapper = new AquaBasicPopupMenuWrapper(popup, border);
            wrapper.putClientProperty(AquaVibrantSupport.POPUP_BACKGROUND_STYLE_KEY, "vibrantMenu");
            wrapper.putClientProperty(AquaVibrantSupport.POPUP_CORNER_RADIUS_KEY, 6);
            popup.setBorder(null);
            PopupFactory f = PopupFactory.getSharedInstance();
            y += 2;
            return f.getPopup(owner, wrapper, x, y);

        } else {

            PopupFactory f = PopupFactory.getSharedInstance();
            return f.getPopup(owner, popup, x, y);
        }
    }

    protected int getContextualMenuStyle(Component c) {
        return FANCY_CONTEXTUAL_MENU_STYLE;
    }

    public static Object getHidePopupKey() {
        if (HIDE_POPUP_KEY == null) {
            JComboBox cb = new JComboBox();
            HIDE_POPUP_KEY = cb.getClientProperty("doNotCancelPopup");
        }
        return HIDE_POPUP_KEY;
    }

    private class ScrollingMouseListener implements MouseListener, MouseMotionListener, MouseWheelListener {
        @Override
        public void mouseClicked(MouseEvent e) {
        }

        @Override
        public void mousePressed(MouseEvent e) {
        }

        @Override
        public void mouseReleased(MouseEvent e) {
        }

        @Override
        public void mouseEntered(MouseEvent e) {
            if (cp != null) {
                cp.dispatchEvent(e);
            }
        }

        @Override
        public void mouseExited(MouseEvent e) {
            if (cp != null) {
                cp.dispatchEvent(e);
            }
        }

        @Override
        public void mouseDragged(MouseEvent e) {
            if (cp != null) {
                cp.dispatchEvent(e);
            }
        }

        @Override
        public void mouseMoved(MouseEvent e) {
            if (cp != null) {
                cp.dispatchEvent(e);
            }
        }

        @Override
         public void mouseWheelMoved(MouseWheelEvent e) {
            if (cp != null) {
                cp.dispatchEvent(e);
            }
        }
    }
}
