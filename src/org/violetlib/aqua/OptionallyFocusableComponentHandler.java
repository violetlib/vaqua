/*
 * Changes copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import javax.swing.*;

/**
 * A handler for a component whose focusability depends upon the state of the component. The handler should be stored
 * as a client property. See {@link AquaFullKeyboardFocusableHandler}.
 */
public interface OptionallyFocusableComponentHandler {
    void updateFocusability(JComponent c, boolean isFocusable);
}
