/*
 * Copyright (c) 2015-2018 Alan Snyder.
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
import javax.swing.*;
import javax.swing.border.AbstractBorder;

import org.violetlib.jnr.Insetter;
import org.violetlib.jnr.aqua.*;
import org.violetlib.jnr.aqua.AquaUIPainter.ColumnSortArrowDirection;
import org.violetlib.jnr.aqua.AquaUIPainter.State;

/**
 * The border installed on table header cell renderers. It paints the divider and the sort arrow.
 */
@SuppressWarnings("serial") // Superclass is not serializable across versions
public class AquaTableHeaderBorder extends AbstractBorder {

    private Insets editorInsetsLTR;
    private Insets editorInsetsRTL;
    private Insetter arrowInsetsLTR;    // arrow insets not used, the entire cell is mouse sensitive
    private Insetter arrowInsetsRTL;

    protected final AquaUIPainter painter = AquaPainting.create();

    protected ColumnSortArrowDirection sortArrowDirection = ColumnSortArrowDirection.NONE;
    protected JTable owner;

    protected static AquaTableHeaderBorder getListHeaderBorder() {
        // we don't want to share this, because the .setSelected() state
        // would persist to all other JTable instances
        return new AquaTableHeaderBorder();
    }

    protected AquaTableHeaderBorder() {
    }

    @Override
    public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {

        JComponent jc = (JComponent)c;

        // The painter may draw a divider on the left side as well as the right.
        // To compensate, we increase the width and offset the rendering.
        // The divider will either paint over the divider for the cell to the left or get clipped away.
        // Unlike the Aqua Look and Feel painter, our painter should not draw a top or bottom border.
        // Therefore we do not need to offset the rendering vertically.

        AppearanceManager.ensureAppearance(c);
        AquaUtils.configure(painter, c, width+1, height);
        Configuration tg = getConfiguration(jc);
        painter.getPainter(tg).paint(g, x-1, y);
    }

    protected Configuration getConfiguration(JComponent jc) {
        State state = getState(jc);
        boolean isFocused = jc.hasFocus();
        AquaUIPainter.UILayoutDirection ld = AquaUtils.getLayoutDirection(owner);
        return new TableColumnHeaderConfiguration(state, sortArrowDirection, false, isFocused, ld);
    }

    protected State getState(JComponent jc) {
        if (!jc.isEnabled()) return State.DISABLED;

        JRootPane rootPane = jc.getRootPane();
        if (rootPane == null) return State.ACTIVE;

        if (!AquaFocusHandler.isActive(rootPane)) return State.INACTIVE;

        return State.ACTIVE;
    }

    @Override
    public Insets getBorderInsets(Component c) {
        // bad to create new one each time. For debugging only.
        configureInsets();
        return AquaUtils.isLeftToRight(owner) ? editorInsetsLTR : editorInsetsRTL;
    }

    @Override
    public Insets getBorderInsets(Component c, Insets insets) {
        Insets s = getBorderInsets(c);
        insets.left = s.left;
        insets.top = s.top;
        insets.right = s.right;
        insets.bottom = s.bottom;
        return insets;
    }

    @Override
    public boolean isBorderOpaque() {
        return false;
    }

    public void setSortArrowDirection(ColumnSortArrowDirection d) {
        sortArrowDirection = d;
    }

    public void setOwner(JTable t) {
        owner = t;
    }

    private void configureInsets() {
        if (editorInsetsLTR != null) {
            return;
        }

        AquaUILayoutInfo layout = painter.getLayoutInfo();
        editorInsetsLTR = layout.getTableColumnHeaderLabelInsets(new TableColumnHeaderLayoutConfiguration(AquaUIPainter.UILayoutDirection.LEFT_TO_RIGHT, true)).asInsets();
        editorInsetsRTL = layout.getTableColumnHeaderLabelInsets(new TableColumnHeaderLayoutConfiguration(AquaUIPainter.UILayoutDirection.RIGHT_TO_LEFT, true)).asInsets();
        arrowInsetsLTR = layout.getTableColumnHeaderSortArrowInsets(new TableColumnHeaderLayoutConfiguration(AquaUIPainter.UILayoutDirection.LEFT_TO_RIGHT, true));
        arrowInsetsRTL = layout.getTableColumnHeaderSortArrowInsets(new TableColumnHeaderLayoutConfiguration(AquaUIPainter.UILayoutDirection.RIGHT_TO_LEFT, true));
    }
}
