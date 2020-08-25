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
