/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.event.MouseEvent;

/**
 * Optional methods that may be supported by a popup.
 */
public interface AquaExtendedPopup {

    /**
     * Enter arrow scrolling mode.
     */
    void startArrowScroll();

    /**
     * Exit arrow scrolling mode.
     */
    void stopArrowScroll();

    /**
     * Update the selection display as appropriate for a change in the mouse position.
     */
    void updateSelection(MouseEvent e);
}
