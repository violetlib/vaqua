/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

/**
 * Provide access to the native support for visual effect views.
 */
public interface VisualEffectViewPeer {
    void setFrame(int x, int y, int width, int height);
    void updateSelectionBackgrounds(SelectionBoundsDescription sd);
    void dispose();
}
