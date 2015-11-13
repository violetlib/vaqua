/*
 * Changes Copyright (c) 2015 Alan Snyder.
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

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicPopupMenuUI;
import java.awt.*;
import java.awt.event.*;

/**
 * UI for all kinds of pop up menus.
 */
public class AquaPopupMenuUI extends BasicPopupMenuUI {

    public static final String POP_UP_SCREEN_BOUNDS = "Aqua.PopupMenu.ScreenBounds";
    public static final String POP_UP_SELECTED_REGION = "Aqua.PopupMenu.SelectedRegion";
    public static final String POP_UP_SELECTED_REGION_LOCATION = "Aqua.PopupMenu.SelectedRegionLocation";

    public static ComponentUI createUI(final JComponent x) {
        return new AquaPopupMenuUI();
    }

    private static Object HIDE_POPUP_KEY;

    private AquaContextualPopup cp;
    private ScrollingMouseListener scrollingMouseListener = new ScrollingMouseListener();

    public boolean isPopupTrigger(final MouseEvent e) {
        // Use the awt popup trigger code since this only runs on our OS!
        return e.isPopupTrigger();
    }

    @Override
    public void installDefaults() {
        super.installDefaults();
        LookAndFeel.installProperty(popupMenu, "opaque", Boolean.FALSE);
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        popupMenu.addMouseListener(scrollingMouseListener);
        popupMenu.addMouseMotionListener(scrollingMouseListener);
        popupMenu.addMouseWheelListener(scrollingMouseListener);
    }

    @Override
    protected void uninstallListeners() {
        super.uninstallListeners();
        popupMenu.removeMouseListener(scrollingMouseListener);
        popupMenu.removeMouseMotionListener(scrollingMouseListener);
        popupMenu.removeMouseWheelListener(scrollingMouseListener);
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

        if (isContextualMenuStyle(owner)) {

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
        } else {
            return new AquaPopup(owner, popup, x, y, false);
        }
    }

    protected boolean isContextualMenuStyle(Component c) {
        return true;
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
