/*
 * Changes copyright (c) 2015-2016 Alan Snyder.
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
import java.beans.PropertyChangeEvent;

import javax.swing.*;
import javax.swing.plaf.basic.BasicSplitPaneDivider;

import org.violetlib.aqua.AquaUtils.LazyKeyedSingleton;

import org.violetlib.jnr.aqua.AquaUIPainter;
import org.violetlib.jnr.aqua.AquaUIPainter.State;
import org.violetlib.jnr.aqua.AquaUIPainter.DividerWidget;
import org.violetlib.jnr.aqua.SplitPaneDividerConfiguration;

@SuppressWarnings("serial") // Superclass is not serializable across versions
public class AquaSplitPaneDivider extends BasicSplitPaneDivider {
    final AquaUIPainter painter = AquaPainting.create();
    protected final AquaSplitPaneUI ui;

    public AquaSplitPaneDivider(final AquaSplitPaneUI ui) {
        super(ui);
        this.ui = ui;
        setLayout(new AquaSplitPaneDivider.DividerLayout());
    }

    /**
     * Property change event, presumably from the JSplitPane, will message
     * updateOrientation if necessary.
     */
    public void propertyChange(final PropertyChangeEvent e) {
        if (e.getSource() == splitPane) {
            final String propName = e.getPropertyName();
            if ("enabled".equals(propName)) {
                final boolean enabled = splitPane.isEnabled();
                if (leftButton != null) leftButton.setEnabled(enabled);
                if (rightButton != null) rightButton.setEnabled(enabled);
            } else if (JSplitPane.ORIENTATION_PROPERTY.equals(propName)) {
                // need to regenerate the buttons, since we bake the orientation into them
                if (rightButton  != null) {
                    remove(rightButton); rightButton = null;
                }
                if (leftButton != null) {
                    remove(leftButton); leftButton = null;
                }
                oneTouchExpandableChanged();
            }
        }
        super.propertyChange(e);
    }

    protected int getMinDividerSize() {
        int size = ui.getFixedDividerSize();
        return size > 0 ? size : 2;
    }

    protected int getMaxDividerSize() {
        int size = ui.getFixedDividerSize();
        return size > 0 ? size : 11;
    }

    /**
     * The divider extension is the "width" of the transparent area to add to either side of the divider to make it
     * easier to grab when dragging.
     */
    public int getDividerExtension() {
        return ui.getDividerExtension();
    }

    /**
     * Paints the divider.
     */
    public void paint(final Graphics g) {
        final Dimension size = getSize();
        int x = 0;
        int y = 0;

        final boolean isVerticalDivider = splitPane.getOrientation() == JSplitPane.HORIZONTAL_SPLIT;
        final int minSize = getMinDividerSize();
        final int maxSize = getMaxDividerSize();
        boolean doPaint = true;
        if (isVerticalDivider) {
            if (size.width > maxSize) {
                final int diff = size.width - maxSize;
                x = diff / 2;
                size.width = maxSize;
            }
            if (size.width < minSize) {
                doPaint = false;
            }
        } else {
            if (size.height > maxSize) {
                final int diff = size.height - maxSize;
                y = diff / 2;
                size.height = maxSize;
            }
            if (size.height < minSize) {
                doPaint = false;
            }
        }

        if (doPaint) {
            DividerWidget w = ui.getWidget();
            State state = getState();
            AquaUIPainter.Orientation orientation = isVerticalDivider ? AquaUIPainter.Orientation.VERTICAL : AquaUIPainter.Orientation.HORIZONTAL;
            painter.configure(size.width, size.height);
            SplitPaneDividerConfiguration dg = new SplitPaneDividerConfiguration(w, state, orientation, 0);
            painter.getPainter(dg).paint(g, x, y);
        } else {
            Color c = UIManager.getColor("SplitPane.dividerColor");
            if (c != null) {
                g.setColor(c);
                g.fillRect(x, y, size.width, size.height);
            }
        }

        super.paint(g); // Ends up at Container.paint, which paints our JButton children
    }

    protected State getState() {
        return splitPane.isEnabled() ? State.ACTIVE : State.DISABLED;
    }

    protected void dragDividerTo(int location) {
        location += getDividerExtension();
        ((AquaSplitPaneUI) splitPaneUI).dragDividerTo(location);
    }

    protected void finishDraggingTo(int location) {
        location += getDividerExtension();
        ((AquaSplitPaneUI) splitPaneUI).finishDraggingTo(location);
    }

    protected JButton createLeftOneTouchButton() {
        return createButtonForDirection(getDirection(true));
    }

    protected JButton createRightOneTouchButton() {
        return createButtonForDirection(getDirection(false));
    }

    static final LazyKeyedSingleton<Integer, Image> directionArrows = new LazyKeyedSingleton<Integer, Image>() {
        protected Image getInstance(final Integer direction) {
            final Image arrowImage = AquaImageFactory.getArrowImageForDirection(direction);
            final int h = (arrowImage.getHeight(null) * 5) / 7;
            final int w = (arrowImage.getWidth(null) * 5) / 7;
            return AquaUtils.generateLightenedImage(arrowImage.getScaledInstance(w, h, Image.SCALE_SMOOTH), 50);
        }
    };

    // separate static, because the divider needs to be serializable
    // see <rdar://problem/7590946> JSplitPane is not serializable when using Aqua look and feel
    static JButton createButtonForDirection(final int direction) {
        final JButton button = new JButton(new ImageIcon(directionArrows.get(Integer.valueOf(direction))));
        button.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        button.setFocusPainted(false);
        button.setRequestFocusEnabled(false);
        button.setFocusable(false);
        button.setBorder(BorderFactory.createEmptyBorder(1, 1, 1, 1));
        return button;
    }

    int getDirection(final boolean isLeft) {
        final boolean isVerticalDivider = splitPane.getOrientation() == JSplitPane.HORIZONTAL_SPLIT;
        if (isVerticalDivider) {
            return isLeft ? SwingConstants.WEST : SwingConstants.EAST;
        } else {
            return isLeft ? SwingConstants.NORTH : SwingConstants.SOUTH;
        }
    }

    protected class DividerLayout extends BasicSplitPaneDivider.DividerLayout {
        public void layoutContainer(final Container c) {
            final int maxSize = getMaxDividerSize();
            final Dimension size = getSize();

            if (leftButton == null || rightButton == null || c != AquaSplitPaneDivider.this) return;

            if (!splitPane.isOneTouchExpandable()) {
                leftButton.setBounds(-5, -5, 1, 1);
                rightButton.setBounds(-5, -5, 1, 1);
                return;
            }

            Icon leftIcon = leftButton.getIcon();
            Icon rightIcon = rightButton.getIcon();
            int buttonWidth = Math.max(leftIcon.getIconWidth(), rightIcon.getIconWidth());
            int buttonHeight = Math.max(leftIcon.getIconHeight(), rightIcon.getIconHeight());

            // put them at the right or the bottom
            final boolean isVerticalDivider = splitPane.getOrientation() == JSplitPane.HORIZONTAL_SPLIT;
            if (isVerticalDivider) {
                if (buttonWidth > size.width || buttonWidth > maxSize) {
                    leftButton.setBounds(-5, -5, 1, 1);
                    rightButton.setBounds(-5, -5, 1, 1);
                    return;
                }

                final int diff = size.width - buttonWidth;
                int xPosition = diff / 2;
                int yPosition = buttonHeight + ONE_TOUCH_OFFSET;

                rightButton.setBounds(xPosition, yPosition, buttonWidth, buttonHeight);

                yPosition -= (buttonHeight + ONE_TOUCH_OFFSET);
                leftButton.setBounds(xPosition, yPosition, buttonWidth, buttonHeight);
            } else {
                if (buttonHeight > size.height || buttonHeight > maxSize) {
                    leftButton.setBounds(-5, -5, 1, 1);
                    rightButton.setBounds(-5, -5, 1, 1);
                    return;
                }

                final int diff = size.height - buttonHeight;
                int yPosition = diff / 2;
                int xPosition = buttonWidth + ONE_TOUCH_OFFSET;

                rightButton.setBounds(xPosition, yPosition, buttonWidth, buttonHeight);

                xPosition -= (buttonWidth + ONE_TOUCH_OFFSET);
                leftButton.setBounds(xPosition, yPosition, buttonWidth, buttonHeight);
            }
        }
    }
}
