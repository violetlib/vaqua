/*
 * Copyright (c) 2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.util.function.Consumer;
import javax.swing.*;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Update the bounds of the selected item of a menu.
 */
public abstract class MenuSelectionBoundsTracker implements SelectionBoundsTracker {

    public static boolean isDebug = false;

    protected @Nullable JPopupMenu menu;
    protected @Nullable Consumer<SelectionBoundsDescription> consumer;
    private @Nullable SelectionBoundsDescription currentSelectionDescription;
    private @Nullable JMenuItem selectedItem;
    private @Nullable SelectionRegion selectedItemRegion;

    public MenuSelectionBoundsTracker(@NotNull JPopupMenu menu,
                                      @Nullable Consumer<SelectionBoundsDescription> consumer) {
        this.menu = menu;
        this.consumer = consumer;
    }

    @Override
    public void update() {
    }

    /**
     * This method is called when a menu item in a tracked menu is painted. Using this information, the menu item
     * that requires a selection background (if any) is identified.
     * @param item The menu item.
     * @param context The appearance attributes for painting the item.
     */

    public void paintingItem(@NotNull JMenuItem item, @NotNull AppearanceContext context) {
        if (context.isSelected()) {
            if (item != selectedItem) {
                selectedItem = item;
                selectedItemRegion = getSelectionRegion(item);
                if (isDebug) {
                    AquaUtils.syslog("new selected item: " + item.getText());
                }
                selectionRegionChanged();
            } else {
                assert selectedItemRegion != null;
                SelectionRegion r = getSelectionRegion(item);
                if (!r.matches(selectedItemRegion)) {
                    selectedItemRegion = r;
                    if (isDebug) {
                        AquaUtils.syslog("new selected item bounds: " + item.getText());
                    }
                    selectionRegionChanged();
                }
            }
        } else if (item == selectedItem) {
            if (isDebug) {
                AquaUtils.syslog("deselected item: " + item.getText());
            }
            selectedItem = null;
            selectedItemRegion = null;
            selectionRegionChanged();
        }
    }

    @Override
    public void dispose() {
        menu = null;
        consumer = null;
        selectedItem = null;
        selectedItemRegion = null;
        currentSelectionDescription = null;
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

    private void selectionRegionChanged() {
        currentSelectionDescription
          = selectedItemRegion != null? SelectionBoundsDescription.create(selectedItemRegion) : null;
        if (consumer != null) {
            consumer.accept(currentSelectionDescription);
        }
    }

    private @NotNull SelectionRegion getSelectionRegion(@NotNull Component c) {
        Rectangle bounds = c.getBounds();
        int y = convertRowYCoordinateToSelectionDescription(bounds.y);
        return new SelectionRegion(y, bounds.height);
    }

    /**
     * Map the Y location of a row in the menu coordinate system to the Y location to be stored in the selection
     * description.
     */
    protected int convertRowYCoordinateToSelectionDescription(int y) {
        return y;
    }
}
