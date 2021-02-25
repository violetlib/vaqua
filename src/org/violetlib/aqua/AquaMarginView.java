/*
 * Copyright (c) 2015-2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.text.*;
import java.awt.*;

import org.jetbrains.annotations.NotNull;

/**
 * A container view that adds a left and/or right margin and supports horizontal scrolling in a JTextField. This view
 * supports the Aqua text field margins, which are not attached to the text field when focused but instead scroll left
 * and right along with the text.
 */
public class AquaMarginView extends View {

    protected final @NotNull View base;
    protected int margin;

    protected final @NotNull Rectangle tempRect;

    /**
     * Create a margin view that supports scrolling and margins.
     * @param base The base view that is responsible for painting the text in the allocation provided by this view.
     *             The allocation given to the base view will always be large enough to contain the entire text.
     * @param margin The width of each margin.
     */

    public AquaMarginView(@NotNull View base, int margin) {
        super(base.getElement());
        this.base = base;
        this.margin = margin;
        tempRect = new Rectangle();
        base.setParent(this);
    }

    public void setMargin(int margin) {
        this.margin = margin;
    }

    @Override
    public float getMinimumSpan(int axis) {
        return base.getMinimumSpan(axis) + (axis == X_AXIS ? margin * 2 : 0);
    }

    @Override
    public float getPreferredSpan(int axis) {
        return base.getPreferredSpan(axis) + (axis == X_AXIS ? margin * 2 : 0);
    }

    @Override
    public float getMaximumSpan(int axis) {
        return base.getMaximumSpan(axis) + (axis == X_AXIS ? margin * 2 : 0);
    }

    @Override
    public float getAlignment(int axis) {
        return base.getAlignment(axis);
    }

    @Override
    public void setSize(float width, float height) {
        // Adjusting the size is not necessary or useful. The method ignores the size.
        base.setSize(width, height);
    }

    @Override
    public int getResizeWeight(int axis) {
        return base.getResizeWeight(axis);
    }

    @Override
    public int getStartOffset() {
        return base.getStartOffset();
    }

    @Override
    public int getEndOffset() {
        return base.getEndOffset();
    }

    @Override
    public void paint(Graphics g, Shape allocation) {
        g = g.create();
        Rectangle clipRegion = getClipRegion(allocation);
        g.clipRect(clipRegion.x, clipRegion.y, clipRegion.width, clipRegion.height);
        Rectangle baseAllocation = getBaseAllocation(allocation);
        base.paint(g, baseAllocation);
        g.dispose();
    }

    @Override
    public int getViewCount() {
        return base.getViewCount();
    }

    @Override
    public View getView(int n) {
        return base.getView(n);
    }

    @Override
    public void replace(int offset, int length, View[] views) {
        base.replace(offset, length, views);
    }

    @Override
    public int getViewIndex(float x, float y, Shape allocation) {
        return base.getViewIndex(x, y, allocation);
    }

    @Override
    public Shape getChildAllocation(int index, Shape a) {
        Rectangle baseAllocation = getBaseAllocation(a);
        return base.getChildAllocation(index, baseAllocation);
    }

    @Override
    public int getNextVisualPositionFrom(int pos, Position.Bias b, Shape a, int direction, Position.Bias[] biasRet) throws BadLocationException {
        Rectangle baseAllocation = getBaseAllocation(a);
        return base.getNextVisualPositionFrom(pos, b, baseAllocation, direction, biasRet);
    }

    @Override
    public Shape modelToView(int pos, Shape a, Position.Bias b) throws BadLocationException {
        Rectangle baseAllocation = getBaseAllocation(a);
        return base.modelToView(pos, baseAllocation, b);
    }

    @Override
    public Shape modelToView(int p0, Position.Bias b0, int p1, Position.Bias b1, Shape a) throws BadLocationException {
        Rectangle baseAllocation = getBaseAllocation(a);
        return base.modelToView(p0, b0, p1, b1, baseAllocation);
    }

    @Override
    public int viewToModel(float x, float y, Shape a, Position.Bias[] biasReturn) {
        Rectangle baseAllocation = getBaseAllocation(a);
        return base.viewToModel(x, y, baseAllocation, biasReturn);
    }

    @Override
    public void insertUpdate(DocumentEvent e, Shape a, ViewFactory f) {
        Rectangle baseAllocation = getBaseAllocation(a);
        base.insertUpdate(e, baseAllocation, f);
        Container c = getContainer();
        c.repaint();
    }

    @Override
    public void removeUpdate(DocumentEvent e, Shape a, ViewFactory f) {
        Rectangle baseAllocation = getBaseAllocation(a);
        base.removeUpdate(e, baseAllocation, f);
        Container c = getContainer();
        c.repaint();
    }

    @Override
    public void changedUpdate(DocumentEvent e, Shape a, ViewFactory f) {
        Rectangle baseAllocation = getBaseAllocation(a);
        base.changedUpdate(e, baseAllocation, f);
    }

    /**
     * Compute the allocation given to the base view, which is the view that actually paints the text. The allocation
     * is constructed to ensure that the text is painted in the correct location, taking into account scrolling and
     * margins. The horizontal scrolling model is updated as needed to be consistent with the current text and
     * allocation.
     *
     * @param a The allocation given to this view.
     */

    protected Rectangle getBaseAllocation(Shape a) {
        if (a == null) {
            return null;
        }

        tempRect.setBounds(a instanceof Rectangle ? (Rectangle) a : a.getBounds());

        Component c = getContainer();
        if (!(c instanceof JTextField)) {
            // unexpected, but not impossible
            return tempRect;
        }

        JTextField tf = (JTextField) c;
        boolean isLTR = tf.getComponentOrientation().isLeftToRight();
        int horizontalAlignment = tf.getHorizontalAlignment();

        int textWidth = (int) Math.ceil(base.getPreferredSpan(View.X_AXIS)) + 1;
        int availableWidth = tempRect.width;

        // The addition to the text width mirrors code in the JDK, which presumably is leaving room for
        // a thin caret. The JDK is not consistent in this regard, which turns out to be useful (see JDK-4818934).

        BoundedRangeModel vis = tf.getHorizontalVisibility();
        int scrollPos = vis.getValue();

        // There are several cases to consider. The easiest case is where the text and both margins fit in the available
        // space. In this case, scrolling is not needed. The space for the margins are removed from the allocation
        // given to the base view.

        if (textWidth + 2 * margin <= availableWidth) {
            tempRect.x += margin;
            tempRect.width -= 2 * margin;
            vis.setValue(0);
            return tempRect;
        }

        // If there is room for the text and one margin, then allocate the margin on the preferred side.

        if (textWidth + margin <= availableWidth) {
            boolean useLeft = horizontalAlignment == LEFT || horizontalAlignment == CENTER && isLTR;
            if (useLeft) {
                tempRect.x += margin;
            }
            tempRect.width -= margin;
            vis.setValue(0);
            return tempRect;
        }

        // If there is room for the text, provide the entire allocation to the base view.

        if (textWidth <= availableWidth) {
            vis.setValue(0);
            return tempRect;
        }

        // The remaining cases involve text that does not fit in the available space, which means that scrolling is
        // possible. The allocation given to the base view will match the text size (+1) and will be offset to
        // correspond to the scroll position and margin visibility.

        scrollPos = vis.getValue();
        tempRect.width = textWidth + 1;
        tempRect.x -= scrollPos;

        if (scrollPos == 0) {
            // If the left edge of the text is visible (by implication, the right edge is not), then the left margin
            // should be visible and the right margin should not. The left edge of the text should be located at the
            // right edge of the left margin.
            tempRect.x += margin;
            vis.setRangeProperties(scrollPos, availableWidth - margin, 0, textWidth, false);
        } else if (scrollPos >= textWidth - availableWidth - margin) {
            // If the right edge of the text is visible (by implication, the left edge is not), then the right margin
            // should be visible and the left margin should not. The right edge of the text should be located at the
            // left edge of the right margin.
            tempRect.x -= margin;
            vis.setRangeProperties(textWidth - availableWidth, availableWidth - margin, 0, textWidth, false);
        } else {
            // Neither edge of the text is visible. No margins should be displayed.
            vis.setRangeProperties(scrollPos, availableWidth, 0, textWidth, false);
        }

        return tempRect;
    }

    protected Rectangle getClipRegion(Shape a) {
        return a instanceof Rectangle ? (Rectangle) a : a.getBounds();
    }
}
