/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.util.Arrays;
import java.util.Objects;
import javax.swing.*;
import javax.swing.tree.TreeSelectionModel;

/**
 * Update the bounds of the selected rows of a JTree. The update() method must be called after potential changes to the
 * tree layout.
 */
public abstract class TreeSelectionBoundsTracker {
    protected JTree tree;

    private /* @Nullable */ SelectionDescription currentSelectionDescription;

    public TreeSelectionBoundsTracker(JTree tree) {
        this.tree = tree;

        update();
    }

    public void dispose() {
        tree = null;
    }

    public void reset() {
        currentSelectionDescription = null;
    }

    /**
     * Call this method when the tree layout may have changed.
     */
    public void update() {
        if (tree != null) {
            TreeSelectionModel sm = tree.getSelectionModel();
            int[] selectedRows = sm != null ? sm.getSelectionRows() : null;
            if (selectedRows != null && selectedRows.length == 0) {
                selectedRows = null;
            }
            updateFromSelectedRows(selectedRows);
        }
    }

    protected void updateFromSelectedRows(/* @Nullable */ int[] rows) {
        SelectionDescription newSelectionDescription = createSelectionDescription(rows);
        if (!Objects.equals(newSelectionDescription, currentSelectionDescription)) {
            currentSelectionDescription = newSelectionDescription;
            selectionDescriptionChanged(currentSelectionDescription);
        }
    }

    private /* @Nullable */ SelectionDescription createSelectionDescription(int[] rows) {
        if (rows == null) {
            return null;
        }
        SelectionDescription sd = new SelectionDescription(rows.length);
        for (int row : rows) {
            Rectangle bounds = tree.getRowBounds(row);
            int y = convertRowYCoordinateToSelectionDescription(bounds.y);
            sd.addRow(y, bounds.height);
        }
        return sd;
    }

    /**
     * A description of the number of selection regions and their vertical locations. Adjacent rows are combined.
     */
    public static class SelectionDescription {
        int regionCount;
        int lastY;
        int[] data;

        public SelectionDescription(int maximumRegionCount) {
            data = new int[maximumRegionCount*2+1];
        }

        public void addRow(int y, int h) {
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
         * Return the selection description as an integer array for the purpose of passing to native code.
         */
        public int[] getData() {
            return data;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            SelectionDescription that = (SelectionDescription) o;
            return regionCount == that.regionCount && Arrays.equals(data, that.data);
        }

        @Override
        public int hashCode() {
            return Objects.hash(regionCount, data);
        }
    }

    /**
     * Map the Y location of a row in the tree coordinate system to the Y location to be stored in the selection
     * description.
     */
    protected int convertRowYCoordinateToSelectionDescription(int y) {
        return y;
    }

    protected abstract void selectionDescriptionChanged(/* @Nullable */ SelectionDescription sd);
}
