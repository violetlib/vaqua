/*
 * Copyright (c) 2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import javax.swing.*;

import org.jetbrains.annotations.Nullable;

/**
 * Methods supported by containers that support the inset view style (lists and tables).
 */
public interface AquaViewStyleContainerUI {

    boolean isInset();

    void scrollPaneAncestorChanged(@Nullable JScrollPane sp);
}
