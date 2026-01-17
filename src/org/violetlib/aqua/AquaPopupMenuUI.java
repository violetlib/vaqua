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
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.FontUIResource;
import javax.swing.plaf.basic.BasicPopupMenuUI;

import org.jetbrains.annotations.*;
import org.violetlib.jnr.aqua.AquaUIPainter;

import static org.violetlib.aqua.AquaContextualPopup.getContextualMenuBorder;

/**
 * UI for all kinds of pop up menus.
 */
public class AquaPopupMenuUI extends BasicPopupMenuUI implements AquaComponentUI {

    public static final String POP_UP_SCREEN_BOUNDS = "Aqua.PopupMenu.ScreenBounds";
    public static final String POP_UP_SELECTED_REGION = "Aqua.PopupMenu.SelectedRegion";
    public static final String POP_UP_SELECTED_REGION_LOCATION = "Aqua.PopupMenu.SelectedRegionLocation";
    public static final String POP_UP_TRACKER = "Aqua.PopupMenu.Tracker";

    public static final int ORDINARY_CONTEXTUAL_MENU_STYLE = 0;
    public static final int SIMPLE_CONTEXTUAL_MENU_STYLE = 1;   // used in macOS 10.14+ for editable combo boxes
    public static final int FANCY_CONTEXTUAL_MENU_STYLE = 2;    // rounded corners and fancy scrolling

    public static ComponentUI createUI(JComponent x) {
        return new AquaPopupMenuUI();
    }

    private static Object HIDE_POPUP_KEY;

    private AquaContextualPopup cp;
    private ScrollingMouseListener scrollingMouseListener = new ScrollingMouseListener();
    private PropertyChangeListener componentOrientationListener = new MyComponentOrientationListener();
    protected @Nullable MenuDescription menuDescription;

    private final @NotNull BasicContextualColors colors;
    private @Nullable Dimension selectionIconSize;
    private @Nullable Dimension arrowIconSize;

    private @Nullable Icon checkIcon;
    private @Nullable Icon radioIcon;
    private @Nullable Icon indeterminateIcon;
    private @Nullable Icon arrowIcon;
    private int textItemGap;
    private int selectionIconSeparation;
    private int arrowIconSeparation;
    private Font labelFont;
    private Font acceleratorFont;

    public AquaPopupMenuUI() {
        colors = AquaColors.getMenuColors();
    }

    @Override
    public void installDefaults() {
        super.installDefaults();
        LookAndFeel.installProperty(popupMenu, "opaque", false);

        int version = AquaPainting.getVersion();

        selectionIconSize = computeSelectionIconSize();
        arrowIconSize = computeArrowIconSize();

        checkIcon = AquaImageFactory.getMenuSelectionIcon(popupMenu, selectionIconSize);
        radioIcon = checkIcon;
        indeterminateIcon = AquaImageFactory.getMenuIndeterminateSelectionIcon(popupMenu, selectionIconSize);
        arrowIcon = AquaImageFactory.getSubmenuArrow(popupMenu, arrowIconSize);
        textItemGap = version >= 1600 ? 8 : 7;
        selectionIconSeparation = 4;
        arrowIconSeparation = 25;
        labelFont = computeLabelFont();
        acceleratorFont = computeAcceleratorFont(labelFont);
    }

    private @NotNull Dimension computeSelectionIconSize() {
        return new Dimension(10, 10);
    }

    private @NotNull Dimension computeArrowIconSize() {
        return new Dimension(10, 10);
    }

//    private @NotNull Dimension computePotentialIconSize() {
//        int width = checkIcon != null ? checkIcon.getIconWidth() : 0;
//        int height = checkIcon != null ? checkIcon.getIconHeight() : 0;
//        if (radioIcon != null) {
//            width = Math.max(width, radioIcon.getIconWidth());
//            height = Math.max(height, radioIcon.getIconHeight());
//        }
//        if (indeterminateIcon != null) {
//            width = Math.max(width, indeterminateIcon.getIconWidth());
//            height = Math.max(height, indeterminateIcon.getIconHeight());
//        }
//        return new Dimension(width, height);
//    }

    private @NotNull Font computeLabelFont() {
        Font f = UIManager.getFont("MenuItem.font");
        if (f != null) {
            return f;
        }
        return new FontUIResource(null, Font.PLAIN, 13);
    }

    private @NotNull Font computeAcceleratorFont(@NotNull Font labelFont) {
        Font f = UIManager.getFont("MenuItem.font");
        if (f != null) {
            return f;
        }
        return labelFont;
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        popupMenu.addMouseListener(scrollingMouseListener);
        popupMenu.addMouseMotionListener(scrollingMouseListener);
        popupMenu.addMouseWheelListener(scrollingMouseListener);
        popupMenu.addPropertyChangeListener("componentOrientation", componentOrientationListener);
        AppearanceManager.install(popupMenu);
    }

    @Override
    protected void uninstallListeners() {
        AppearanceManager.uninstall(popupMenu);
        popupMenu.removePropertyChangeListener("componentOrientation", componentOrientationListener);
        popupMenu.removeMouseListener(scrollingMouseListener);
        popupMenu.removeMouseMotionListener(scrollingMouseListener);
        popupMenu.removeMouseWheelListener(scrollingMouseListener);
        super.uninstallListeners();
    }

    @Override
    public void update(Graphics g, JComponent c) {
        paint(g, c);
    }

    @Override
    public void paint(Graphics g, JComponent c) {
        AppearanceManager.withContext(g, c, this::paint);
    }

    public void paint(Graphics2D g, JComponent c, @NotNull PaintingContext pc) {
        AquaUIPainter.State state = AquaUIPainter.State.ACTIVE;
        AppearanceContext appearanceContext = new AppearanceContext(pc.appearance, state, false, false);
        BasicContextualColors colors = getMenuColors(popupMenu.getInvoker());
        AquaColors.installColors(popupMenu, appearanceContext, colors);
        super.paint(g, c);
    }

    public boolean isPopupTrigger(MouseEvent e) {
        // Use the awt popup trigger code since this only runs on our OS!
        return e.isPopupTrigger();
    }

    @Override
    public Popup getPopup(JPopupMenu popup, int x, int y) {

        // Unfortunately, the path by which the popup location is transmitted from the popup to the popup menu UI is
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

            boolean installVibrantSelection = AquaLookAndFeel.USE_VIBRANT_MENU && isVibrantSelectionSupportNeeded(owner);
            AquaUIPainter.Size size = getSize(owner);

            cp = new AquaContextualPopup(popup, owner, installVibrantSelection,
              selectedRegion, selectedRegionLocation, x, y, width, height, size);
            return cp.getPopup();

        } else if (menuStyle == SIMPLE_CONTEXTUAL_MENU_STYLE) {

            Border border = getContextualMenuBorder();
            AquaBasicPopupMenuWrapper wrapper = new AquaBasicPopupMenuWrapper(popup, border);
            wrapper.putClientProperty(AquaVibrantSupport.POPUP_BACKGROUND_STYLE_KEY, "vibrantMenu");
            wrapper.putClientProperty(AquaVibrantSupport.POPUP_CORNER_RADIUS_KEY, 6);
            AquaBorderSupport.installBorder(popup, null);
            PopupFactory f = PopupFactory.getSharedInstance();
            y += 2;
            return f.getPopup(owner, wrapper, x, y);

        } else {

            PopupFactory f = PopupFactory.getSharedInstance();
            return f.getPopup(owner, popup, x, y);
        }
    }

    protected @NotNull BasicContextualColors getMenuColors(Component c) {
        return AquaColors.getMenuColors();
    }

    protected boolean isVibrantSelectionSupportNeeded(Component owner) {
        return owner instanceof JComponent;
    }

    protected int getContextualMenuStyle(Component owner) {
        return FANCY_CONTEXTUAL_MENU_STYLE;
    }

    protected @NotNull AquaUIPainter.Size getSize(@NotNull Component owner) {

        return AquaUtils.getSize(owner, false, null);
    }

    public @NotNull MenuDescription getMenuDescription() {
        if (menuDescription == null) {
            menuDescription = computeMenuDescription();
        }
        return menuDescription;
    }

    private @NotNull MenuDescription computeMenuDescription() {
        MenuLayoutInfo layoutInfo = computeMenuLayoutInfo();
        return new MenuDescription(layoutInfo, checkIcon, radioIcon, arrowIcon, indeterminateIcon,
          labelFont, acceleratorFont, colors);
    }

    protected @NotNull MenuLayoutInfo computeMenuLayoutInfo() {
        boolean isSelectionIconNeeded = false;
        boolean isArrowIconNeeded = false;
        int count = popupMenu.getComponentCount();
        for (int i = 0; i < count; i++) {
            Component c = popupMenu.getComponent(i);
            if (c instanceof JMenuItem) {
                JMenuItem item = (JMenuItem) c;
                AquaMenuItemUI ui = AquaUtils.getUI(item, AquaMenuItemUI.class);
                if (ui != null && ui.isSelectable()) {
                    isSelectionIconNeeded = true;
                }
                if (item instanceof JMenu) {
                    isArrowIconNeeded = true;
                }
            }
        }

        assert selectionIconSize != null;
        Dimension selectionIconSize = isSelectionIconNeeded ? this.selectionIconSize : new Dimension(0, 0);
        Dimension arrowIconSize = isArrowIconNeeded && arrowIcon != null
          ? new Dimension(arrowIcon.getIconWidth(), arrowIcon.getIconHeight())
          : new Dimension(0, 0);
        FontMetrics labelFM = popupMenu.getFontMetrics(labelFont);
        FontMetrics accelFM = popupMenu.getFontMetrics(acceleratorFont);
        Insets menuItemInsets = isSelectionIconNeeded ? new Insets(1, 10, 1, 14) : new Insets(1, 14, 1, 14);

        return new MenuLayoutInfo(menuItemInsets, selectionIconSize, arrowIconSize, labelFM, accelFM,
          textItemGap, selectionIconSeparation, arrowIconSeparation);
    }

    public static Object getHidePopupKey() {
        if (HIDE_POPUP_KEY == null) {
            JComboBox cb = new JComboBox();
            HIDE_POPUP_KEY = cb.getClientProperty("doNotCancelPopup");
        }
        return HIDE_POPUP_KEY;
    }

    private class MyComponentOrientationListener
      implements PropertyChangeListener
    {
        @Override
        public void propertyChange(PropertyChangeEvent e) {
            if (arrowIconSize != null) {  // should not be null
                arrowIcon = AquaImageFactory.getSubmenuArrow(popupMenu, arrowIconSize);
                menuDescription = null; // force recomputation when needed
            }
        }
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
