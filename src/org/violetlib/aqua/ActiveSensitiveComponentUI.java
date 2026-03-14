/*
 * Copyright (c) 2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import javax.swing.*;

import org.jetbrains.annotations.*;

/**

 */

public interface ActiveSensitiveComponentUI
{
    void activeStateChanged(@NotNull JComponent c, boolean isActive);
}
