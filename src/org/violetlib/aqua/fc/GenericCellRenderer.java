/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import javax.swing.*;
import java.awt.*;

/**
 * A cell renderer that does not care what kind of container it is used in.
 */
public interface GenericCellRenderer {
    Component getCellRendererComponent(JComponent container,
                                       Object value,
                                       boolean isSelected,
                                       boolean cellHasFocus);
}
