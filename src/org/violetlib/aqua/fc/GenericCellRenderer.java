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
