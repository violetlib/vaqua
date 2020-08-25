/*
 * Copyright (c) 2015-2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;
import javax.swing.event.CellEditorListener;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * This policy determines which components should be displayed as cell renderers or editors. A component may display
 * differently when it is used as a cell editor or renderer. For example, an editor may use a smaller focus ring to
 * avoid overlapping other cells. A text field or combo box may be displayed without a border.
 *
 * The problem is that there is no standard protocol for recognizing a cell renderer or editor. This class uses
 * heuristics as well as allowing the application to provide client properties.
 */
public class AquaCellEditorPolicy {

    public static final String IS_COMBO_BOX_CELL_EDITOR_PROPERTY = "JComboBox.isTableCellEditor";
    public static final String IS_CELL_EDITOR_PROPERTY = "JComponent.isCellEditor";
    public static final String IS_CELL_CONTAINER_PROPERTY = "JComponent.isCellContainer";

    // TBD: There is some question whether the cell editor property is intended to be used by cell renderers as well
    // as cell editors.

    public enum CellStatus { CELL_RENDERER, CELL_EDITOR };

    private static AquaCellEditorPolicy INSTANCE = new AquaCellEditorPolicy();

    public static AquaCellEditorPolicy getInstance() {
        return INSTANCE;
    }

    public static boolean isCellEditorProperty(String name) {
        return IS_COMBO_BOX_CELL_EDITOR_PROPERTY.equals(name) || IS_CELL_EDITOR_PROPERTY.equals(name);
    }

    /**
     * Determine whether a given component is being used as a cell renderer or editor.
     * @return the cell status, or null if the component is not being used as a cell renderer or editor.
     */
    public @Nullable CellStatus getCellStatus(@NotNull JComponent c) {

        Object isSpecifiedCellEditor = c.getClientProperty(IS_CELL_EDITOR_PROPERTY);
        if (Boolean.FALSE.equals(isSpecifiedCellEditor)) {
            return null;
        }

        return getCellStatusFromParent(c.getParent());
    }

    private @Nullable CellStatus getCellStatusFromParent(@Nullable Container parent) {
        if (parent == null) {
            return null;
        }

        if (parent instanceof CellRendererPane) {
            return CellStatus.CELL_RENDERER;
        }

        if (parent instanceof JComboBox) {
            // A combo box is a cell container
            JComboBox comboBox = (JComboBox) parent;
            CellStatus parentStatus = getCellStatus(comboBox);
            if (parentStatus != null) {
                // The original component is part of a cell renderer or editor
                return parentStatus;
            }
            return comboBox.isEditable() ? CellStatus.CELL_EDITOR : CellStatus.CELL_RENDERER;
        }

        if (parent instanceof JComponent) {
            JComponent jc = (JComponent) parent;
            Object isSpecifiedCellContainer = jc.getClientProperty(IS_CELL_CONTAINER_PROPERTY);
            if (Boolean.TRUE.equals(isSpecifiedCellContainer)) {
                return CellStatus.CELL_EDITOR;
            }
            if (Boolean.FALSE.equals(isSpecifiedCellContainer)) {
                return null;
            }
        }

        if (parent instanceof CellEditorListener) {
            return CellStatus.CELL_EDITOR;
        }

        return getCellStatusFromParent(parent.getParent());
    }
}
