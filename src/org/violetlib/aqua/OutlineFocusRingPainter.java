/*
 * Copyright (c) 2014-2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.geom.Area;

/**
 * Paint a focus ring based on an inner and outer shape.
 */
public class OutlineFocusRingPainter implements FocusRingPainter {

    protected final Shape ringShape;

    /**
     * Create a focus ring painter based on an inner and outer shape.
     * @param innerShape The inner shape.
     * @param outerShape The outer shape.
     */
    public OutlineFocusRingPainter(Shape innerShape, Shape outerShape) {
        Area outerArea = new Area(outerShape);
        Area innerArea = new Area(innerShape);
        outerArea.subtract(innerArea);
        ringShape = outerArea;
    }

    public void paint(Graphics2D gg, Color c) {
        Graphics2D g = (Graphics2D) gg.create();
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g.setColor(c);
        g.fill(ringShape);
        g.dispose();
    }
}
