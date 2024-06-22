/*
 * Copyright (c) 2021-2024 Alan Snyder.
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

import org.jetbrains.annotations.*;

/**
 * Update the bounds of the selected rows of a JList. The update() method must be called after potential changes to the
 * list layout.
 */
public abstract class ListSelectionBoundsTracker implements SelectionBoundsTracker {
    protected @Nullable JList<?> list;
    protected @Nullable Consumer<SelectionBoundsDescription> consumer;
    private @Nullable SelectionBoundsDescription currentSelectionDescription;
    private int currentWidth;

    public ListSelectionBoundsTracker(@NotNull JList<?> list, @Nullable Consumer<SelectionBoundsDescription> consumer) {
        this.list = list;
        this.consumer = consumer;

        update();

        if (consumer != null && currentSelectionDescription == null) {
            consumer.accept(null);
        }
    }

    @Override
    public void dispose() {
        list = null;
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
     * Call this method when the list layout (row bounds) may have changed.
     */
    @Override
    public void update() {
        if (list != null) {
            SelectionBoundsDescription d = getSelectionBoundsDescription(list);
            try {
                updateFromSelectedRows(d);
            } catch (IllegalComponentStateException ex) {
                // interaction with AquaUtils.paintImmediately()
            }
        }
    }

    private @NotNull SelectionBoundsDescription getSelectionBoundsDescription(@NotNull JList<?> list) {
        int regionCount = 0;

        // If a drop target is active, the "drop on" row should be highlighted
        int dropTargetRow = -1;
        JList.DropLocation loc = list.getDropLocation();
        if (loc != null && !loc.isInsert()) {
            dropTargetRow = loc.getIndex();
            if (dropTargetRow >= 0) {
                regionCount = 1;
            }
        }

        int[] temp = null;
        int min = list.getMinSelectionIndex();
        int max = list.getMaxSelectionIndex();
        if ((min >= 0) && (max >= min)) {
            temp = new int[max - min + 1];
            int n = 0;
            for (int i = min; i <= max; i++) {
                if (list.isSelectedIndex(i) && i != dropTargetRow) {
                    temp[n++] = i;
                    regionCount++;
                }
            }
        }

        SelectionBoundsDescription d = new SelectionBoundsDescription(regionCount);
        if (dropTargetRow >= 0) {
            Rectangle bounds = list.getCellBounds(dropTargetRow, dropTargetRow);
            int y = convertRowYCoordinateToSelectionDescription(bounds.y);
            d.addDropTargetRegion(y, bounds.height);
            regionCount--;
        }
        if (regionCount > 0) {
            for (int i = 0; i < regionCount; i++) {
                int row = temp[i];
                Rectangle bounds = list.getCellBounds(row, row);
                int y = convertRowYCoordinateToSelectionDescription(bounds.y);
                d.addRegion(y, bounds.height);
            }
        }
        return d;
    }

    protected void updateFromSelectedRows(@NotNull SelectionBoundsDescription newSelectionDescription) {
        int newWidth = list != null ? list.getWidth() : 0;
        if (newWidth != currentWidth || !Objects.equals(newSelectionDescription, currentSelectionDescription)) {
            currentSelectionDescription = newSelectionDescription;
            currentWidth = newWidth;
            if (consumer != null) {
                consumer.accept(currentSelectionDescription);
            }
        }
    }

    /**
     * Map the Y location of a row in the list coordinate system to the Y location to be stored in the selection
     * description.
     */
    protected int convertRowYCoordinateToSelectionDescription(int y) {
        return y;
    }
}
