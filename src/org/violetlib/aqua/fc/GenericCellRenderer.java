/*
 * Copyright (c) 2015-2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.awt.*;
import javax.swing.*;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.aqua.AquaAppearance;
import org.violetlib.aqua.ContainerContextualColors;

/**
 * A cell renderer that does not care what kind of container it is used in.
 */
public interface GenericCellRenderer {
    Component getCellRendererComponent(JComponent container,
                                       @NotNull AquaAppearance appearance,
                                       @NotNull ContainerContextualColors colors,
                                       Object value,
                                       boolean isSelected,
                                       boolean cellHasFocus);
}
