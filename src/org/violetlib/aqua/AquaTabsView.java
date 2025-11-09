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
 * An immutable view of a sequence of tabs in a tabbed pane.
 * <p>
 * The tabs can be viewed in different ways.
 * <ul>
 * <li>The tabs can be in Leading to trailing order (the way they are specified in the tabbed pane, aka the semantic
 * order) or in visual order (left to right or top to bottom, depending upon the placement of the tabs in the
 * container).</li>
 * <li>If the view uses the visual order, it includes only the visible tabs.<li>
 * If only visible tabs are included, then the visible scroll tabs may or may not be included.<li>
 */

public class AquaTabsView {

    /**
     * Create a view that includes visible tabs only, without scroll tabs, in left to right or top to bottom order.
     * @param isVertical True if tabs are arranged on the side of the component.
     * @param isReversed True if the visual order is the reverse of the semantic order.
     * @param semanticCount The total number of semantic tabs, visible or invisible.
     * @param count The number of visible semantic tabs.
     * @param firstIndex The semantic index of the first visible tab as defined by the semantic tab order.
     * @param visibleSelectedIndex The index of the selected tab in this order, or -1 if the selected tab is not
     *                             visible (should not happen).
     */

    public static @NotNull AquaTabsView createVisible(boolean isVertical, boolean isReversed, int semanticCount,
                                                      int count, int firstIndex, int visibleSelectedIndex)
    {
        return new AquaTabsView(isVertical, isReversed, false, true, false, semanticCount, count, firstIndex,
          false, false, visibleSelectedIndex);
    }

    /**
     * Create a view that includes visible tabs only, including scroll tabs, where needed, in left to right or top to
     * bottom order.
     * @param isVertical True if tabs are arranged on the side of the component.
     * @param isReversed True if the visual order is the reverse of the semantic order.
     * @param semanticCount The total number of semantic tabs, visible or invisible.
     * @param count The number of visible semantic tabs.
     * @param firstIndex The semantic index of the first visible tab as defined by the semantic tab order.
     * @param hasLeftScrollTab True if there is a visible scroll tab to the left or top of the semantic tabs.
     * @param hasRightScrollTab True if there is a visible scroll tab to the right or bottom of the semantic tabs.
     * @param visibleSelectedIndex The index of the selected tab in this order, or -1 if the selected tab is not
     *                             visible (should not happen).
     */

    public static @NotNull AquaTabsView createVisible(boolean isVertical, boolean isReversed,
                                                      int semanticCount, int count, int firstIndex,
                                                      boolean hasLeftScrollTab,
                                                      boolean hasRightScrollTab, int visibleSelectedIndex)
    {
        return new AquaTabsView(isVertical, isReversed, false, true, true, semanticCount, count, firstIndex,
          hasLeftScrollTab, hasRightScrollTab, visibleSelectedIndex);
    }

    /**
     * Create a view that all tabs, excluding scroll tabs, in the same order as defined by the JTabbedPane.
     * @param isVertical True if tabs are arranged on the side of the component.
     * @param count The number of semantic tabs.
     * @param selectedIndex The index of the selected tab in the JTabbedPane, or -1 if no tab is selected (should not
     *                      happen).
     */

    public static @NotNull AquaTabsView createSemantic(boolean isVertical, int count, int selectedIndex)
    {
        return new AquaTabsView(isVertical, true, false, false, false, count, count, 0, false, false, selectedIndex);
    }

    public final boolean isVertical;  // true if tabs are arranged on the side of the component
    private final boolean isReversed;
    public final boolean isSemanticOrder;  // true if leading to trailing = left to right, or top to bottom
    public final boolean isVisibleOnly;  // true if only visible tabs are included
    public final boolean includesScrollTabs;  // true if scroll tabs are included

    private final int firstIndex;  // the semantic index of the first non-scroll tab in the sequence
    private final int lastIndex;
    private final boolean hasLeftScrollTab;
    private final boolean hasRightScrollTab;
    private final int selectedIndex;

    private final int semanticCount;  // the number of tabs defined by the JTabbedPane
    private final int count;

    public AquaTabsView(boolean isVertical, boolean isReversed, boolean isSemanticOrder, boolean isVisibleOnly,
                        boolean includesScrollTabs, int semanticCount, int count, int firstIndex,
                        boolean hasLeftScrollTab, boolean hasRightScrollTab, int selectedIndex) {

        if (semanticCount < 0 || count < 0) {
            throw new IllegalArgumentException("Tab count must not be negative");
        }

        if (count > semanticCount) {
            throw new IllegalArgumentException("Visible tab count must not be greater than the semantic tab count");
        }

        if (isReversed && isSemanticOrder) {
            throw new IllegalArgumentException("Semantic order conflicts with isReversed");
        }

        if (isReversed) {
            this.firstIndex = firstIndex + count - 1;
            this.lastIndex = firstIndex;
        } else {
            this.firstIndex = firstIndex;
            this.lastIndex = firstIndex + count - 1;
        }

        this.isVertical = isVertical;
        this.isSemanticOrder = isSemanticOrder;
        this.isVisibleOnly = isVisibleOnly;
        this.isReversed = isReversed;
        this.includesScrollTabs = includesScrollTabs;
        this.semanticCount = semanticCount;
        this.hasLeftScrollTab = hasLeftScrollTab;
        this.hasRightScrollTab = hasRightScrollTab;
        this.selectedIndex = selectedIndex;

        if (includesScrollTabs) {
            if (hasLeftScrollTab) {
                count++;
            }
            if (hasRightScrollTab) {
                count++;
            }
        }
        this.count = count;
    }

    /**
     * Return the number of tabs in this view, including scroll tabs.
     */
    public int getCount()
    {
        return count;
    }

    /**
     * Return the number of tabs in this view, excluding scroll tabs.
     */
    public int getCountExcludingScrollTabs()
    {
        int n = count;
        if (hasLeftScrollTab) {
            n--;
        }
        if (hasRightScrollTab) {
            n--;
        }
        return n;
    }

    /**
     * Return the index of the specified tab in the sequence of tabs defined by this view.
     * @param tab Identifies a semantic tab by its index in the tabbed pane or a scroll arrow tab.
     * @return the index of the tab in this sequence, or -1 if not present.
     */
    public int find(int tab)
    {
        if (tab == AquaTabbedPaneUI.LEFT_SCROLL_TAB) {
            return hasLeftScrollTab ? 0 : -1;
        }
        if (tab == AquaTabbedPaneUI.RIGHT_SCROLL_TAB) {
            return hasRightScrollTab ? count - 1 : -1;
        }
        if (tab < 0 || tab >= semanticCount) {
            return -1;
        }
        if (isSemanticOrder) {
            return tab;
        }
        if (isReversed) {
            if (tab >= lastIndex && tab <= firstIndex) {
                int n = firstIndex - tab;
                return hasLeftScrollTab ? n + 1 : n;
            }
        } else {
            if (tab >= firstIndex && tab <= lastIndex) {
                int n = tab - firstIndex;
                return hasLeftScrollTab ? n + 1 : n;
            }
        }
        return -1;
    }

    /**
     * Identify a tab in the sequence of tabs defined by this view.
     * @param i The index of the tab in this view.
     * @return
     * the index of the tab in the JTabbedPane, if the tab is a defined tab of the pane;
     * LEFT_SCROLL_TAB, if the tab is the left scroll arrow tab;
     * RIGHT_SCROLL_TAB, if the tab is a right scroll arrow tab;
     * NO_TAB, if there is no corresponding tab.
     */
    public int identifyTabAtIndex(int i)
    {
        if (i < 0 || i >= count) {
            return AquaTabbedPaneUI.NO_TAB;
        }
        if (isSemanticOrder) {
            return i;
        }
        if (i == 0 && hasLeftScrollTab) {
            return AquaTabbedPaneUI.LEFT_SCROLL_TAB;
        }
        if (i == count-1 && hasRightScrollTab) {
            return AquaTabbedPaneUI.RIGHT_SCROLL_TAB;
        }
        if (hasLeftScrollTab) {
            i--;
        }
        if (isReversed) {
            return firstIndex - i;
        }
        return firstIndex + i;
    }

    public boolean isSelected(int i)
    {
        return i >= 0 && i == selectedIndex;
    }
}
