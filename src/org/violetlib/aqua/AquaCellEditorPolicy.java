/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import javax.swing.*;
import javax.swing.event.CellEditorListener;
import java.awt.*;

/**
 * This policy determines which components should be displayed as cell editors. A component may display differently when
 * it is used as a cell editor. For example, it may use a smaller focus ring to avoid overlapping other cells. The
 * problem is that there is no standard protocol for recognizing a cell editor. This class uses heuristics as well as
 * allowing the application to provide client properties.
 */
public class AquaCellEditorPolicy {

    public static final String IS_COMBO_BOX_CELL_EDITOR_PROPERTY = "JComboBox.isTableCellEditor";
    public static final String IS_CELL_EDITOR_PROPERTY = "JComponent.isCellEditor";
    public static final String IS_CELL_CONTAINER_PROPERTY = "JComponent.isCellContainer";

    private static AquaCellEditorPolicy INSTANCE = new AquaCellEditorPolicy();

    public static AquaCellEditorPolicy getInstance() {
        return INSTANCE;
    }

    public static boolean isCellEditorProperty(String name) {
        return IS_COMBO_BOX_CELL_EDITOR_PROPERTY.equals(name) || IS_CELL_EDITOR_PROPERTY.equals(name);
    }

    /**
     * Decide whether a given component should be displayed as a cell editor.
     *
     * The problem is that there is no standard protocol for recognizing a cell editor. We use heuristics here as well
     * as allowing the application to provide client properties.
     */
    public boolean isCellEditor(JComponent c) {
        Object isSpecifiedCellEditor = c.getClientProperty(IS_CELL_EDITOR_PROPERTY);

        if (Boolean.TRUE.equals(isSpecifiedCellEditor)) {
            return true;
        } else if (Boolean.FALSE.equals(isSpecifiedCellEditor)) {
            return false;
        }

        // The following is a hack implemented by DefaultCellEditor for combo box editors
        // This test allows test programs to work on simulated combo box cell editors
        if (Boolean.TRUE.equals(c.getClientProperty(IS_COMBO_BOX_CELL_EDITOR_PROPERTY))) {
            return true;
        }

        return isContainedInCellContainer(c);
    }

    protected boolean isContainedInCellContainer(Component c) {
        for (;;) {
            Container parent = c.getParent();
            if (parent == null) {
                return false;
            }
            if (isCellContainer(parent)) {
                return true;
            }
            c = parent;
        }
    }

    protected boolean isCellContainer(Container c) {
        JComponent jc = c instanceof JComponent ? ((JComponent) c) : null;
        Object isSpecifiedCellContainer = jc != null ? jc.getClientProperty(IS_CELL_CONTAINER_PROPERTY) : null;

        if (Boolean.TRUE.equals(isSpecifiedCellContainer)) {
            return true;
        }

        if (c instanceof CellEditorListener && !Boolean.FALSE.equals(isSpecifiedCellContainer)) {
            return true;
        }

        // The following is a hack implemented by DefaultCellEditor for combo box editors
        // This test allows test programs to work on simulated combo box cell editors
        if (jc != null && Boolean.TRUE.equals(jc.getClientProperty(IS_COMBO_BOX_CELL_EDITOR_PROPERTY))) {
            return true;
        }

        return false;
    }
}
