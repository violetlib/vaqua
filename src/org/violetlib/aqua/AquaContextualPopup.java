/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.event.MouseEvent;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.plaf.UIResource;

/**
 * A contextual style popup that (if necessary) scrolls without using a scroll bar and scrolls by growing taller when
 * possible.
 */
public class AquaContextualPopup {

    private JComponent wrapper;
    private AquaPopup p;

    /**
     * Create a contextual style popup.
     * @param content The content to be displayed in the popup.
     * @param owner The owner of the popup. Needed for proper mouse grabbing if this popup is heavyweight.
     * @param selectedRegion An optional region of the content that should be initially visible. The view may be
     *                       scrolled to make the region visible.
     * @param selectedRegionLocation An optional preferred screen location for the selected region. The view may be
     *                               scrolled to place the region at this location.
     * @param x The desired X screen location of the popup.
     * @param y The desired Y screen location of the popup. If off the top of the screen, the popup will be relocated
     *          with scrolling enabled.
     * @param width If positive, the requested popup width.
     * @param height If positive, determines the requested bottom edge of the popup;
     */
    public AquaContextualPopup(JComponent content,
                               Component owner,
                               Rectangle selectedRegion,
                               Point selectedRegionLocation,
                               int x, int y, int width, int height) {

        Border border = getContextualMenuBorder();
        Insets s = border.getBorderInsets(null);
        Dimension ps = content.getPreferredSize();

        if (width <= 0) {
            width = ps.width + s.left + s.right;
        }

        if (height <= 0) {
            height = ps.height + s.top + s.bottom;
        }

        Rectangle screen = AquaUtils.getScreenBounds(new Point(x, y), owner);

        if (y < screen.y) {
            int delta = screen.y - y;
            y = screen.y;
            height -= delta;
        }

        int screenWidth = screen.width;
        int screenHeight = screen.height;

        if (width > screenWidth) {
            width = screenWidth;
        }

        {
            int availableWidth = screen.x + screen.width - x;
            if (width > availableWidth) {
                x = screen.x + screen.width - width;
            }
        }

        if (height > screenHeight) {
            height = screenHeight;
        }

        {
            int availableHeight = screen.y + screen.height - y;
            if (height > availableHeight) {
                height = availableHeight;
            }
        }

        if (width < ps.width + s.left + s.right || height < ps.height + s.top + s.bottom) {
            if (selectedRegionLocation != null) {
                selectedRegionLocation = new Point(selectedRegionLocation.x - x, selectedRegionLocation.y - y);
            }
            wrapper = new MyScrollingWrapper(content, selectedRegion, selectedRegionLocation, width, height, border);
        } else {
            wrapper = new AquaBasicPopupMenuWrapper(content, border);
        }

        Border existingBorder = content.getBorder();
        if (existingBorder == null || existingBorder instanceof UIResource) {
            content.setBorder(null);
        }

        // Heavy weight popups are required for rounded corners and vibrant background.
        p = new AquaPopup(owner, wrapper, x, y, true);
    }

    public Popup getPopup() {
        return p;
    }

    public final void dispatchEvent(AWTEvent e) {
        wrapper.dispatchEvent(e);
    }

    public static Border getContextualMenuBorder() {
        return new EmptyBorder(5, 0, 5, 0);
    }

    protected class MyScrollingWrapper extends AquaScrollingPopupMenuWrapper {
        public MyScrollingWrapper(JComponent originalView, Rectangle selectedRegion, Point selectedRegionLocation,
                                  int width, int height, Border border) {
            super(originalView, selectedRegion, selectedRegionLocation, width, height, border);
            if (originalView instanceof JPopupMenu) {
                Object magicValue = AquaPopupMenuUI.getHidePopupKey();
                putClientProperty("doNotCancelPopup", magicValue);
                int count = getComponentCount();
                for (int i = 0; i < count; i++) {
                    JComponent cc = (JComponent) getComponent(i);
                    cc.putClientProperty("doNotCancelPopup", magicValue);
                }
            }
        }

        @Override
        protected void scroll(MouseEvent e, int delta) {
            // Before scrolling, try to enlarge the popup vertically
            Point loc = AquaUtils.getScreenLocation(wrapper);
            Rectangle screen = AquaUtils.getScreenBounds(loc, wrapper);
            Window w = p.getPopup();
            if (delta > 0) {
                // scrolling up
                int availableExtension = w.getY() - screen.y;
                if (availableExtension > 0) {
                    int extension = Math.min(delta, availableExtension);
                    w.setBounds(w.getX(), w.getY() - extension, w.getWidth(), w.getHeight() + extension);
                    height += extension;
                    delta -= extension;
                    configure(true);
                }
            } else {
                // scrolling down
                int availableExtension = screen.y + screen.height - (w.getY() + w.getHeight());
                if (availableExtension > 0) {
                    int extension = Math.min(-delta, availableExtension);
                    w.setBounds(w.getX(), w.getY(), w.getWidth(), w.getHeight() + extension);
                    delta += extension;
                    height += extension;
                    Point vp = viewport.getViewPosition();
                    viewport.setViewPosition(new Point(vp.x, vp.y - extension));
                    configure(true);
                }
            }

            if (delta != 0) {
                super.scroll(e, delta);
            }

            if (e != null && originalContent instanceof AquaExtendedPopup) {
                AquaExtendedPopup cbp = (AquaExtendedPopup) originalContent;
                cbp.updateSelection(e);
            }
        }
    }
}
