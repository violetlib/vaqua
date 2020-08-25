/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;

/**
 * The interface that supports a connection to the object that wants a color to be specified using a shared color
 * chooser.
 */
public interface SharedColorChooserOwner {
    /**
     * This method is called to apply a color selection. This method may be called multiple times while connected.
     * @param c
     */
    void applyColor(Color c);
    /**
     * This method is called when the connection is broken except by the explicit request of this owner.
     */
    void disconnected();
}
