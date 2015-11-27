/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.util.Arrays;
import java.util.Objects;

/**
 * A description of the regions whose background should display a vibrant selection background.
 * Currently only full width regions are supported (no width information is stored).
 */
public class SelectionBoundsDescription {
    private int regionCount;
    private int lastY;
    private int[] data;

    /**
     * Create a selection bounds description with no regions.
     * @param maximumRegionCount The maximum number of regions that can be stored.
     */
    public SelectionBoundsDescription(int maximumRegionCount) {
        data = new int[maximumRegionCount*2+1];
    }

    /**
     * Add a full-width selection region.
     * @param y The y coordinate of the region.
     * @param h The height of the region.
     */
    public void addRegion(int y, int h) {
        if (regionCount > 0 && lastY == y) {
            // just extend the last region
            data[regionCount*2] += h;
        } else {
            ++regionCount;
            data[0] = regionCount;
            data[regionCount*2-1] = y;
            data[regionCount*2] = h;
        }
        lastY = y+h;
    }

    /**
     * Return the selection bounds description as an integer array for the purpose of passing to native code.
     */
    public int[] getData() {
        return data;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        SelectionBoundsDescription that = (SelectionBoundsDescription) o;
        return regionCount == that.regionCount && Arrays.equals(data, that.data);
    }

    @Override
    public int hashCode() {
        return Objects.hash(regionCount, data);
    }
}
