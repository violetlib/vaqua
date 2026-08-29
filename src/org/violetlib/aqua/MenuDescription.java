/*
 * Changes Copyright (c) 2025 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;

import org.jetbrains.annotations.*;

/**
 * Information related to menu item painting that should be shared by all items in a menu.
 */
public class MenuDescription {
    /**
     * The layout information for menu items.
     */
    public final @NotNull MenuLayoutInfo layoutInfo;

    /**
     * The selection icon to use for a checkbox menu item.
     */
    public final @NotNull Icon checkIcon;

    /**
     * The selection icon to use for a radio button menu item.
     */
    public final @NotNull Icon radioIcon;

    /**
     * The selection icon to use for a submenu item.
     */
    public final @NotNull Icon arrowIcon;

    /**
     * The selection icon to use for a menu item with an indeterminate selection state.
     */
    public final @NotNull Icon indeterminateIcon;

    /**
     * The default font to use for a menu item label.
     */
    public final @NotNull Font labelFont;

    /**
     * The default font to use for a menu item accelerator.
     */
    public final @NotNull Font acceleratorFont;

    /**
     * The colors to use.
     */
    public final @NotNull BasicContextualColors colors;

    public MenuDescription(@NotNull MenuLayoutInfo layoutInfo,
                           @NotNull Icon checkIcon,
                           @NotNull Icon radioIcon,
                           @NotNull Icon arrowIcon,
                           @NotNull Icon indeterminateIcon,
                           @NotNull Font labelFont,
                           @NotNull Font acceleratorFont,
                           @NotNull BasicContextualColors colors) {
        this.layoutInfo = layoutInfo;
        this.checkIcon = checkIcon;
        this.radioIcon = radioIcon;
        this.arrowIcon = arrowIcon;
        this.indeterminateIcon = indeterminateIcon;
        this.labelFont = labelFont;
        this.acceleratorFont = acceleratorFont;
        this.colors = colors;
    }
}
