/*
 * Copyright (c) 2015-2018 Alan Snyder.
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

/**
 * A composite view that adds a left and right margin to the base view. This view supports the Aqua text field margins,
 * which are not attached to the text field when focused but instead scroll left and right along with the text. As the
 * content view of a text field, it supports scrolling.
 */
public class AquaMarginView extends View {

    protected final View base;
    protected int margin;

    protected Rectangle tempRect;

    public AquaMarginView(View base, int margin) {
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
        base.setSize(width - 2 * margin, height);
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
        updateVisibilityModel();
    }

    @Override
    public void removeUpdate(DocumentEvent e, Shape a, ViewFactory f) {
        Rectangle baseAllocation = getBaseAllocation(a);
        base.removeUpdate(e, baseAllocation, f);
        updateVisibilityModel();
    }

    @Override
    public void changedUpdate(DocumentEvent e, Shape a, ViewFactory f) {
        Rectangle baseAllocation = getBaseAllocation(a);
        base.changedUpdate(e, baseAllocation, f);
    }

    protected Rectangle getBaseAllocation(Shape a) {
        if (a != null) {
            Rectangle alloc = a instanceof Rectangle ? (Rectangle) a : a.getBounds();
            tempRect.setBounds(alloc);
            tempRect.x += margin;
            tempRect.width -= 2 * margin;
            return adjustAllocation(tempRect);
        } else {
            return null;
        }
    }

    protected Rectangle getClipRegion(Shape a) {
        return a instanceof Rectangle ? (Rectangle) a : a.getBounds();
    }

    protected Rectangle adjustAllocation(Rectangle bounds) {
        // Support horizontal scrolling
        Component c = getContainer();
        if (c instanceof JTextField) {
            JTextField field = (JTextField) c;
            BoundedRangeModel vis = field.getHorizontalVisibility();
            int hspan = (int) getPreferredSpan(X_AXIS) - 2 * margin;
            int max = Math.max(hspan, bounds.width);
            int value = vis.getValue();
            int extent = Math.min(max, bounds.width - 1);
            if ((value + extent) > max) {
                value = max - extent;
            }
            if (value <= margin) {
                value = 0;
            }

            // I have experienced an repaint loop with the extent going back and forth between adjacent values.
            // This is an attempt at a workaround.

            int oldExtent = vis.getExtent();
            int diff = extent - oldExtent;
            if (diff < -1 || diff > 1) {
                vis.setRangeProperties(value, extent, vis.getMinimum(), max, false);
            }

            if (hspan >= bounds.width) {
                // adjust the allocation to match the bounded range.
                bounds.width = hspan;
                bounds.x -= vis.getValue();
            }
        }
        return bounds;
    }

    void updateVisibilityModel() {
        Component c = getContainer();
        if (c instanceof JTextField) {
            JTextField field = (JTextField) c;
            BoundedRangeModel vis = field.getHorizontalVisibility();
            int hspan = (int) getPreferredSpan(X_AXIS) - 2 * margin;
            int extent = vis.getExtent();
            int maximum = Math.max(hspan, extent);
            extent = (extent == 0) ? maximum : extent;
            int value = maximum - extent;
            int oldValue = vis.getValue();
            if ((oldValue + extent) > maximum) {
                oldValue = maximum - extent;
            }
            value = Math.max(0, Math.min(value, oldValue));
            if (value <= margin) {
                value = 0;
            }
            vis.setRangeProperties(value, extent, 0, maximum, false);
        }
    }
}
