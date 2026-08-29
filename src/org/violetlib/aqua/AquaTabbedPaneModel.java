/*
 * Copyright (c) 2025 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import org.jetbrains.annotations.*;

/**
 * An immutable description of the tabs in a tabbed pane after layout.
 * A source of specific model views.
 */

public class AquaTabbedPaneModel {

    // The tabs in the JTabbedPane define a semantic order, leading to trailing.

    public final boolean isVertical;  // true if tabs are arranged on the side of the component
    public final boolean isReversed;
    public final int tabPlacement;
    public final int tabCount;  // the total number of semantic tabs (scroll tabs not included)
    public final int leadingVisibleTab; // the semantic tab index of the leading visible tab
    public final int trailingVisibleTab; // the semantic tab index of the trailing visible tab
    public final int selectedIndex; // the semantic tab index of the selected tab

    public AquaTabbedPaneModel(boolean isVertical,
                               boolean isReversed,
                               int tabPlacement,
                               int tabCount,
                               int visibleTabCount,
                               int leadingVisibleTab,
                               int selectedIndex) {
        this.isVertical = isVertical;
        this.isReversed = isReversed;
        this.tabPlacement = tabPlacement;
        this.tabCount = tabCount;
        this.leadingVisibleTab = leadingVisibleTab;
        this.trailingVisibleTab = leadingVisibleTab + visibleTabCount - 1;
        this.selectedIndex = selectedIndex;

        if (visibleTabCount < 0) {
            throw new IllegalArgumentException("Visible tab count must not be negative");
        }
    }

    /**
     * Create a view of the tabs with the following properties.
     * @param isSemanticOrder If true, the tabs are arranged in semantic order (as specified by the JTabbedPane)
     *                        and all tabs are included.
     *                        If false, the tabs are arranged in visual order (left-to-right or top-to-bottom)
     *                        and only visible tabs are included.
     * @param includeScrollTabs If true, scroll tabs (if any) are included when the visual order is specified.
     */

    public @NotNull AquaTabsView getView(boolean isSemanticOrder, boolean includeScrollTabs)
    {
        if (isSemanticOrder) {
            return AquaTabsView.createSemantic(isVertical, tabCount, selectedIndex);
        }
        int visibleTabCount = trailingVisibleTab - leadingVisibleTab + 1;

        int visibleSelectedIndex = -1;
        if (visibleTabCount > 0) {
            if (selectedIndex >= leadingVisibleTab && selectedIndex <= trailingVisibleTab) {
                int n = selectedIndex - leadingVisibleTab;
                visibleSelectedIndex = isReversed ? visibleTabCount - n - 1 : n;
            }
        }

        if (includeScrollTabs) {
            boolean firstScrollTabExists = isReversed ? trailingVisibleTab < tabCount-1 : leadingVisibleTab > 0;
            boolean lastScrollTabExists = isReversed ? leadingVisibleTab > 0 : trailingVisibleTab < tabCount-1;
            if (firstScrollTabExists) {
                visibleSelectedIndex++;
            }
            return AquaTabsView.createVisible(isVertical, isReversed, tabCount, visibleTabCount, leadingVisibleTab,
              firstScrollTabExists, lastScrollTabExists, visibleSelectedIndex);
        }
        return AquaTabsView.createVisible(isVertical, isReversed, tabCount, visibleTabCount, leadingVisibleTab, visibleSelectedIndex);
    }
}
