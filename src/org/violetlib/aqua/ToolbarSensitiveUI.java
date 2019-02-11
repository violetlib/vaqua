/*
 * Copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import javax.swing.*;

import org.jetbrains.annotations.NotNull;

/**

 */

public interface ToolbarSensitiveUI {

    /**
     * This method is called when the toolbar status of the component may have changed.
     * See {@link AquaUtils#installToolbarSensitivity(JComponent)}.
     */
    void toolbarStatusChanged(@NotNull JComponent c);
}
