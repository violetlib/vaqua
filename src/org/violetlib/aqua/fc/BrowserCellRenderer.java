/*
 * @(#)BrowserCellRenderer.java
 *
 * Copyright (c) 2008-2013 Werner Randelshofer, Switzerland.
 * You may not use, copy or modify this file, except in compliance with the
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.awt.*;

/**
 * Defines the requirements for an object that displays a tree node in a JBrowser.
 *
 * @author Werner Randelshofer
 * @version $Id$
 */
public interface BrowserCellRenderer {

    /**
     * Sets the value of the current tree cell to {@code value}.
     * If {@code selected} is true, the cell will be drawn as if
     * selected. If {@code expanded} is true the node is currently
     * expanded and if {@code leaf} is true the node represents a
     * leaf and if {@code hasFocus} is true the node currently has
     * focus. {@code tree} is the {@code JTree} the receiver is being
     * configured for.  Returns the {@code Component} that the renderer
     * uses to draw the value.
     *
     * @param browser target
     * @param value value to be rendered
     * @param selected whether the cell is selected
     * @param expanded whether the cell is expanded
     * @param leaf whether the cell is a leaf
     * @param row row index
     * @param hasFocus whether the cell has focus
     * @return the {@code Component} that the renderer uses to draw the value
     */
    Component getBrowserCellRendererComponent(JBrowser browser, Object value,
                   boolean selected, boolean expanded,
                   boolean leaf, int row, boolean hasFocus);
}
