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
import javax.swing.event.MouseInputAdapter;

/**
 * Support window dragging.
 */
public class WindowDraggingMouseListener extends MouseInputAdapter {
    private Window w;
    private Point windowStartPosition;
    private Point initialDragPosition;

    public void attach(JComponent c) {
        if (c != null) {
            c.addMouseListener(this);
            c.addMouseMotionListener(this);
        }
    }

    public void detach(JComponent c) {
        if (c != null) {
            c.removeMouseListener(this);
            c.removeMouseMotionListener(this);
        }
    }

    @Override
    public void mousePressed(MouseEvent e) {
        Component c = e.getComponent();
        Window ancestor = SwingUtilities.getWindowAncestor(c);
        if (ancestor != null) {
            Point p = e.getPoint();
            if (isDragArea(c, p)) {
                w = ancestor;
                windowStartPosition = w.getLocation();
                SwingUtilities.convertPointToScreen(p, c);
                initialDragPosition = p;
                e.consume();
            }
        }
    }

    @Override
    public void mouseReleased(MouseEvent e) {
        if (w != null) {
            w = null;
            windowStartPosition = null;
            initialDragPosition = null;
            e.consume();
        }
    }

    @Override
    public void mouseDragged(MouseEvent e) {
        if (w != null) {
            Point p = e.getPoint();
            SwingUtilities.convertPointToScreen(p, e.getComponent());
            int deltaX = p.x - initialDragPosition.x;
            int deltaY = p.y - initialDragPosition.y;
            w.setLocation(windowStartPosition.x + deltaX, windowStartPosition.y + deltaY);
            e.consume();
        }
    }

    protected boolean isDragArea(Component c, Point p) {
        return true;
    }
}
