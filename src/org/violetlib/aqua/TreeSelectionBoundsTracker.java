/*
 * Copyright (c) 2015-2023 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.util.Objects;
import java.util.function.Consumer;
import javax.swing.*;
import javax.swing.tree.TreePath;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Update the bounds of the selected rows of a JTree. The update() method must be called after potential changes to the
 * tree layout.
 */
public abstract class TreeSelectionBoundsTracker implements SelectionBoundsTracker {
    protected @Nullable JTree tree;
    protected @Nullable Consumer<SelectionBoundsDescription> consumer;
    private @Nullable SelectionBoundsDescription currentSelectionDescription;
    private int currentWidth;

    public TreeSelectionBoundsTracker(@NotNull JTree tree, @Nullable Consumer<SelectionBoundsDescription> consumer) {
        this.tree = tree;
        this.consumer = consumer;

        update();

        if (consumer != null && currentSelectionDescription == null) {
            consumer.accept(null);
        }
    }

    @Override
    public void dispose() {
        tree = null;
        consumer = null;
    }

    @Override
    public void reset() {
        if (currentSelectionDescription != null) {
            currentSelectionDescription = null;
            currentWidth = 0;
            if (consumer != null) {
                consumer.accept(null);
            }
        }
    }

    @Override
    public void setConsumer(@Nullable Consumer<SelectionBoundsDescription> consumer) {
        if (consumer != this.consumer) {
            this.consumer = consumer;
            if (consumer != null) {
                consumer.accept(currentSelectionDescription);
            }
        }
    }

    /**
     * Call this method when the tree layout (row bounds) may have changed.
     */
    @Override
    public void update() {
        if (tree != null) {
            SelectionBoundsDescription d = getSelectionBoundsDescription(tree);
            try {
                updateFromSelectedRows(d);
            } catch (IllegalComponentStateException ex) {
                // interaction with AquaUtils.paintImmediately()
            }
        }
    }

    private @NotNull SelectionBoundsDescription getSelectionBoundsDescription(@NotNull JTree tree) {
        int regionCount = 0;

        // If a drop target is active, the "drop on" row should be highlighted
        int dropTargetRow = -1;
        JTree.DropLocation loc = tree.getDropLocation();
        if (loc != null && loc.getChildIndex() < 0) {
            TreePath path = loc.getPath();
            if (path != null) {
                dropTargetRow = tree.getRowForPath(path);
                if (dropTargetRow >= 0) {
                    regionCount = 1;
                }
            }
        }

        int[] temp = null;
        int min = tree.getMinSelectionRow();
        int max = tree.getMaxSelectionRow();
        if ((min >= 0) && (max >= min)) {
            temp = new int[max - min + 1];
            int n = 0;
            for (int i = min; i <= max; i++) {
                if (tree.isRowSelected(i) && i != dropTargetRow) {
                    temp[n++] = i;
                    regionCount++;
                }
            }
        }

        SelectionBoundsDescription d = new SelectionBoundsDescription(regionCount);
        if (dropTargetRow >= 0) {
            Rectangle bounds = tree.getRowBounds(dropTargetRow);
            int y = convertRowYCoordinateToSelectionDescription(bounds.y);
            d.addDropTargetRegion(y, bounds.height);
            regionCount--;
        }
        if (regionCount > 0) {
            for (int i = 0; i < regionCount; i++) {
                int row = temp[i];
                Rectangle bounds = tree.getRowBounds(row);
                int y = convertRowYCoordinateToSelectionDescription(bounds.y);
                d.addRegion(y, bounds.height);
            }
        }
        return d;
    }

    protected void updateFromSelectedRows(@NotNull SelectionBoundsDescription newSelectionDescription) {
        int newWidth = tree != null ? tree.getWidth() : 0;
        if (newWidth != currentWidth || !Objects.equals(newSelectionDescription, currentSelectionDescription)) {
            currentSelectionDescription = newSelectionDescription;
            currentWidth = newWidth;
            if (consumer != null) {
                consumer.accept(currentSelectionDescription);
            }
        }
    }

    /**
     * Map the Y location of a row in the tree coordinate system to the Y location to be stored in the selection
     * description.
     */
    protected int convertRowYCoordinateToSelectionDescription(int y) {
        return y;
    }
}
