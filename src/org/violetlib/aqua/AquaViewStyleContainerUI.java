/*
 * Copyright (c) 2020-2025 Alan Snyder.
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
 * Methods supported by containers that support the inset view style or can be scrollable sidebars (lists and tables).
 */
public interface AquaViewStyleContainerUI
{
    boolean isSideBar();

    boolean isInset();

    /**
     * Return the additional side insets when using a sidebar border directly (not on a scroll pane).
     */

    @NotNull Insets getInsetViewInsets();

    void scrollPaneAncestorChanged(@Nullable JScrollPane sp);

    void scrollPaneRoundedBorderStatusChanged(boolean isRoundedBorder);

    void configureSidebarStyle();
}
