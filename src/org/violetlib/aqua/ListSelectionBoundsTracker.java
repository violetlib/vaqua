/*
 * Copyright (c) 2021 Alan Snyder.
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

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Update the bounds of the selected rows of a JList. The update() method must be called after potential changes to the
 * list layout.
 */
public abstract class ListSelectionBoundsTracker implements SelectionBoundsTracker {
    protected @Nullable JList<?> list;
    protected @Nullable Consumer<SelectionBoundsDescription> consumer;
    private @Nullable SelectionBoundsDescription currentSelectionDescription;

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
            int[] selectedRows = getSelectedIndices(list);
            try {
                updateFromSelectedRows(selectedRows);
            } catch (IllegalComponentStateException ex) {
                // interaction with AquaUtils.paintImmediately()
            }
        }
    }

    private int @NotNull [] getSelectedIndices(@NotNull JList<?> list) {
        int min = list.getMinSelectionIndex();
        int max = list.getMaxSelectionIndex();
        if ((min < 0) || (max < 0)) {
            return new int[0];
        }

        int[] temp = new int[max - min + 1];
        int n = 0;
        for (int i = min; i <= max; i++) {
            if (list.isSelectedIndex(i)) {
                temp[n++] = i;
            }
        }
        int[] result = new int[n];
        System.arraycopy(temp, 0, result, 0, n);
        return result;
    }

    protected void updateFromSelectedRows(int @NotNull [] rows) {
        SelectionBoundsDescription newSelectionDescription = createSelectionDescription(rows);
        if (!Objects.equals(newSelectionDescription, currentSelectionDescription)) {
            currentSelectionDescription = newSelectionDescription;
            if (consumer != null) {
                consumer.accept(currentSelectionDescription);
            }
        }
    }

    private @Nullable SelectionBoundsDescription createSelectionDescription(int @Nullable [] rows) {
        if (list == null || rows == null) {
            return null;
        }
        SelectionBoundsDescription sd = new SelectionBoundsDescription(rows.length);
        for (int row : rows) {
            Rectangle bounds = list.getCellBounds(row, row);
            int y = convertRowYCoordinateToSelectionDescription(bounds.y);
            sd.addRegion(y, bounds.height);
        }
        return sd;
    }

    /**
     * Map the Y location of a row in the list coordinate system to the Y location to be stored in the selection
     * description.
     */
    protected int convertRowYCoordinateToSelectionDescription(int y) {
        return y;
    }
}
