/*
 * Copyright (c) 2014-2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import javax.swing.*;
import java.awt.*;

import org.jetbrains.annotations.Nullable;

/**
 * An interface to allow a component UI to return a focus ring outline for a component.
 */
public interface FocusRingOutlineProvider {

    /**
     * Return the focus ring outline to use for the specified component. The actual focus ring shape will be determined
     * by the focus ring painter based on this outline. In most cases, the outline should correspond to the visual
     * outline of the component, excluding shadows.
     *
     * Most focus ring outlines depend only upon the component location and size. There are some components, such as
     * JSlider, where the focus ring outline also depends upon the state of the component. Providers for these
     * components must notify the focus ring manager when the outline may have changed, by calling
     * AquaFocusRingManager#focusRingOutlineChanged. This method only needs to be called when the focus ring is being
     * displayed.
     *
     * @param c The component.
     * @return the focus ring outline, relative to the component bounds, or null if the component should not display a
     * focus ring.
     */
    @Nullable Shape getFocusRingOutline(JComponent c);
}
