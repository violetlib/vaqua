/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

// based on javax.swing.ScrollPaneLayout

/*
 * Copyright (c) 1997, 2013, Oracle and/or its affiliates. All rights reserved.
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
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.UIResource;

/**
 * The layout manager for a scroll pane using overlay scroll bars.
 */
public class AquaOverlayScrollPaneLayout extends ScrollPaneLayout implements UIResource {

    protected boolean isHorizontalScrollBarNeeded;
    protected boolean isVerticalScrollBarNeeded;

    public boolean isHorizontalScrollBarNeeded() {
        return isHorizontalScrollBarNeeded;
    }

    public boolean isVerticalScrollBarNeeded() {
        return isVerticalScrollBarNeeded;
    }

    @Override
    public Dimension preferredLayoutSize(Container parent) {
        sync(parent);
        return super.preferredLayoutSize(parent);
    }

    @Override
    public Dimension minimumLayoutSize(Container parent) {
        sync(parent);
        return super.minimumLayoutSize(parent);
    }

    protected void sync(Container parent) {
        // The rearrangements possibly made by AquaScrollPaneUI may cause some cached values to be lost

        JScrollPane scrollPane = (JScrollPane)parent;
        vsbPolicy = scrollPane.getVerticalScrollBarPolicy();
        hsbPolicy = scrollPane.getHorizontalScrollBarPolicy();
        vsb = scrollPane.getVerticalScrollBar();
        hsb = scrollPane.getHorizontalScrollBar();
        viewport = scrollPane.getViewport();
    }

    @Override
    public void layoutContainer(Container parent)
    {
        JScrollPane scrollPane = (JScrollPane)parent;
        sync(parent);

        Rectangle availR = scrollPane.getBounds();
        availR.x = availR.y = 0;

        Insets insets = parent.getInsets();
        availR.x = insets.left;
        availR.y = insets.top;
        availR.width -= insets.left + insets.right;
        availR.height -= insets.top + insets.bottom;

        /* Get the scrollPane's orientation.
         */
        boolean isLeftToRight = !AquaScrollPaneUI.isRTLSupported || AquaUtils.isLeftToRight(scrollPane);

        /* If there's a visible column header remove the space it
         * needs from the top of availR.  The column header is treated
         * as if it were fixed height, arbitrary width.
         */

        Rectangle colHeadR = new Rectangle(0, availR.y, 0, 0);

        if ((colHead != null) && (colHead.isVisible())) {
            int colHeadHeight = Math.min(availR.height,
                                         colHead.getPreferredSize().height);
            colHeadR.height = colHeadHeight;
            availR.y += colHeadHeight;
            availR.height -= colHeadHeight;
        }

        /* If there's a visible row header remove the space it needs
         * from the left or right of availR.  The row header is treated
         * as if it were fixed width, arbitrary height.
         */

        Rectangle rowHeadR = new Rectangle(0, 0, 0, 0);

        if ((rowHead != null) && (rowHead.isVisible())) {
            int rowHeadWidth = Math.min(availR.width,
                                        rowHead.getPreferredSize().width);
            rowHeadR.width = rowHeadWidth;
            availR.width -= rowHeadWidth;
            if (isLeftToRight) {
                rowHeadR.x = availR.x;
                availR.x += rowHeadWidth;
            } else {
                rowHeadR.x = availR.x + availR.width;
            }
        }

        /* If there's a JScrollPane.viewportBorder, remove the
         * space it occupies for availR.
         */

        Border viewportBorder = scrollPane.getViewportBorder();
        Insets vpbInsets;
        if (viewportBorder != null) {
            vpbInsets = viewportBorder.getBorderInsets(parent);
            availR.x += vpbInsets.left;
            availR.y += vpbInsets.top;
            availR.width -= vpbInsets.left + vpbInsets.right;
            availR.height -= vpbInsets.top + vpbInsets.bottom;
        }
        else {
            vpbInsets = new Insets(0,0,0,0);
        }

        determineScrollBarsNeeded(availR);

        if (viewport != null) {
            viewport.setBounds(availR);
        }

        /* We now have the final size of the viewport: availR.
         * Now fixup the header and scrollbar widths/heights.
         */
        rowHeadR.height = availR.height + vpbInsets.top + vpbInsets.bottom;
        rowHeadR.y = availR.y - vpbInsets.top;
        colHeadR.width = availR.width + vpbInsets.left + vpbInsets.right;
        colHeadR.x = availR.x - vpbInsets.left;

        /* Set the bounds of the remaining components.
         */

        if (rowHead != null) {
            rowHead.setBounds(rowHeadR);
        }

        if (colHead != null) {
            colHead.setBounds(colHeadR);
        }

        if (vsb != null) {
            if (isVerticalScrollBarNeeded) {
                int width = vsb.getPreferredSize().width;
                vsb.setBounds(isLeftToRight ? availR.x + availR.width - width : availR.x,
                        availR.y, width, availR.height);
            } else {
                vsb.setBounds(0, 0, 0, 0);
            }
        }

        if (hsb != null) {
            if (isHorizontalScrollBarNeeded) {
                int height = hsb.getPreferredSize().height;
                hsb.setBounds(availR.x, availR.y + availR.height - height, availR.width, height);
            } else {
                hsb.setBounds(0, 0, 0, 0);
            }
        }

        // The only possible corner is between a row head and a column head.
        boolean needCorner = rowHead != null && colHead != null && rowHead.isVisible() && colHead.isVisible();

        if (lowerLeft != null) {
            lowerLeft.setBounds(0, 0, 0, 0);
        }

        if (lowerRight != null) {
            lowerRight.setBounds(0, 0, 0, 0);
        }

        if (upperLeft != null) {
            if (needCorner && isLeftToRight) {
                upperLeft.setBounds(rowHeadR.x, colHeadR.y, rowHeadR.width, colHeadR.height);
            } else {
                upperLeft.setBounds(0, 0, 0, 0);
            }
        }

        if (upperRight != null) {
            if (needCorner && !isLeftToRight) {
                upperRight.setBounds(rowHeadR.x, colHeadR.y, rowHeadR.width, colHeadR.height);
            } else {
                upperRight.setBounds(0, 0, 0, 0);
            }
        }
    }

    protected void determineScrollBarsNeeded(Rectangle availR) {

         /*
         * At this point availR is the space available for the viewport.
         * We'll decide about putting up scrollbars by comparing the
         * viewport views preferred size with the viewports extent
         * size (generally just its size).  Using the preferredSize is
         * reasonable because layout proceeds top down - so we expect
         * the viewport to be laid out next.  And we assume that the
         * viewports layout manager will give the view it's preferred
         * size.  One exception to this is when the view implements
         * Scrollable and Scrollable.getViewTracksViewport{Width,Height}
         * methods return true.  If the view is tracking the viewports
         * width we don't bother with a horizontal scrollbar, similarly
         * if view.getViewTracksViewport(Height) is true we don't bother
         * with a vertical scrollbar.
         */

        Component view = (viewport != null) ? SwingUtilities.getUnwrappedView(viewport) : null;
        Dimension viewPrefSize = (view != null) ? view.getPreferredSize() : new Dimension(0,0);

        Dimension extentSize = (viewport != null) ? viewport.toViewCoordinates(availR.getSize())
                : new Dimension(0,0);

        boolean viewTracksViewportWidth = false;
        boolean viewTracksViewportHeight = false;
        boolean isEmpty = (availR.width < 0 || availR.height < 0);

        // Don't bother checking the Scrollable methods if there is no room
        // for the viewport, we aren't going to show any scrollbars in this
        // case anyway.
        if (!isEmpty && view instanceof Scrollable) {
            Scrollable sv = (Scrollable)view;
            viewTracksViewportWidth = sv.getScrollableTracksViewportWidth();
            viewTracksViewportHeight = sv.getScrollableTracksViewportHeight();
        }

        isVerticalScrollBarNeeded = false;
        isHorizontalScrollBarNeeded = false;

        if (vsb != null && !isEmpty && vsbPolicy != VERTICAL_SCROLLBAR_NEVER) {
            isVerticalScrollBarNeeded = !viewTracksViewportHeight && (viewPrefSize.height > extentSize.height);
        }

        if (hsb != null && !isEmpty && hsbPolicy != HORIZONTAL_SCROLLBAR_NEVER) {
            isHorizontalScrollBarNeeded = !viewTracksViewportWidth && (viewPrefSize.width > extentSize.width);
        }
    }
}
