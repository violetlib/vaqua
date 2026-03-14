/*
 * Changes Copyright (c) 2025 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;

import org.jetbrains.annotations.*;

/**
 * Information related to menu item layout and painting that should be shared by all items in a menu or menu bar.
 */
public class MenuLayoutInfo {
    /**
     * The content insets for a menu item.
     */
    public final @NotNull Insets insets;

    /**
     * The layout size for a selection icon (check mark). Zero if no selection icon is possible.
     * Does not include indentation or separation.
     */
    public final @NotNull Dimension selectionIconSize;

    /**
     * The layout size for a submenu arrow. Zero if no submenus are present.
     * Does not include indentation or separation.
     */
    public final @NotNull Dimension arrowSize;

    /**
     * Font metrics for computing the space required for a menu item label.
     */
    public final @NotNull FontMetrics labelFontMetrics;

    /**
     * Font metrics for computing the space required for a menu item accelerator.
     */
    public final @NotNull FontMetrics acceleratorFontMetrics;

    /**
     * The space between an icon and a label.
     */
    public final int textIconGap;

    /**
     * THe space between a selection icon and the icon or label.
     */
    public final int selectionIconSeparation;

    /**
     * THe space between the content and the arrow icon or accelerator.
     */
    public final int arrowIconSeparation;

    /**
     * Create a description of a menu item in a menu.
     * @param insets
     * @param selectionIconSize
     * @param arrowSize
     * @param labelFontMetrics
     * @param acceleratorFontMetrics
     * @param textIconGap
     * @param selectionIconSeparation
     * @param arrowIconSeparation
     */

    public MenuLayoutInfo(@NotNull Insets insets,
                          @NotNull Dimension selectionIconSize,
                          @NotNull Dimension arrowSize,
                          @NotNull FontMetrics labelFontMetrics,
                          @NotNull FontMetrics acceleratorFontMetrics,
                          int textIconGap,
                          int selectionIconSeparation,
                          int arrowIconSeparation) {
        this.insets = insets;
        this.selectionIconSize = selectionIconSize;
        this.arrowSize = arrowSize;
        this.labelFontMetrics = labelFontMetrics;
        this.acceleratorFontMetrics = acceleratorFontMetrics;
        this.textIconGap = textIconGap;
        this.selectionIconSeparation = selectionIconSeparation;
        this.arrowIconSeparation = arrowIconSeparation;
    }

    /**
     * Create a description of a menu item in a menu bar.
     * @param insets
     * @param labelFontMetrics
     * @param textIconGap
     */

    public MenuLayoutInfo(@NotNull Insets insets,
                          @NotNull FontMetrics labelFontMetrics,
                          int textIconGap) {
        this.insets = insets;
        this.selectionIconSize = new Dimension(0, 0);
        this.arrowSize = new Dimension(0, 0);
        this.labelFontMetrics = labelFontMetrics;
        this.acceleratorFontMetrics = labelFontMetrics;
        this.textIconGap = textIconGap;
        this.selectionIconSeparation = 0;
        this.arrowIconSeparation = 0;
    }
}
