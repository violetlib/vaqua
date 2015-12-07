/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.util.function.Consumer;

/**
 * An interface for an object that determines the bounds of the regions of a component that correspond to selected items
 * and tracks changes in those bounds.
 */
public interface SelectionBoundsTracker {

    /**
     * Install the consumer to receive the initial selection bounds as well as any subsequent changes.
     * A null value is produced when there are no selected regions.
     */
    void setConsumer(Consumer<SelectionBoundsDescription> consumer);

    /**
     * Call this method to recompute the selection bounds.
     */
    void update();

    /**
     * Call this method to discard the current selection bounds.
     */
    void reset();

    /**
     * Call this method when selection bounds tracking is no longer needed.
     */
    void dispose();
}
