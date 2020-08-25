/*
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU Lesser General Public License as published
 *    by the Free Software Foundation, either version 3 of the License, or
 *    (at your option) any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.violetlib.treetable;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.util.EventObject;

import javax.swing.CellEditor;
import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.event.CellEditorListener;
import javax.swing.table.TableCellEditor;

/**
 * A container that positions an editor component to a
 * region of the cell.
 * <p>
 * For the tree column the bounds are shifted past the
 * node's icon.
 *
 * @param <E> The real CellEditor
 */
public class CellEditorContainer<E extends CellEditor> extends Container
        implements TreeTableCellEditor, TableCellEditor {

    public CellEditorContainer(E editor) {
        this.editor = editor;
    }

    protected E editor;

    protected transient TreeTableCellRenderer renderer;

    protected transient Component rendererComponent;

    protected transient Dimension rendererSize;

    protected transient Component editingComponent;

    protected transient int iconOffset;

    public void clearState() {
        removeAll();
        renderer = null;
        rendererSize = null;
        editingComponent = null;
    }

    @Override
    public Component getTreeTableCellEditorComponent(TreeTable treeTable,
                                                     Object value, boolean selected, int row, int column) {
        return getEditorComponent(treeTable, value, selected, row, column, false, false, false);
    }

    @Override
    public Component getTreeTableCellEditorComponent(TreeTable treeTable,
                                                     Object value, boolean selected, int row, int column,
                                                     boolean expanded, boolean leaf) {
        return getEditorComponent(treeTable, value, selected, row, column, true, expanded, leaf);
    }

    private Component getEditorComponent(TreeTable treeTable, Object value,
                                         boolean sel, int row, int col, boolean treeColumn, boolean exp, boolean leaf) {
        if (editingComponent != null)
            remove(editingComponent);

        renderer = treeTable.getCellRenderer(row, col);

        Component rc = rendererComponent = renderer.getTreeTableCellRendererComponent(
                treeTable, value, sel, true, row, col, exp, leaf);
        rendererSize = rc == null ? null : rc.getPreferredSize();

        if (treeColumn) {
            JLabel label = rc instanceof JLabel ? (JLabel)rc : null;
            Icon editingIcon = label == null ? null : label.getIcon();
            if (editingIcon != null) {
                iconOffset = label.getIconTextGap() +
                        editingIcon.getIconWidth();
            } else {
                iconOffset = label == null ? 4 : label.getIconTextGap();
            }
            iconOffset--;
        } else {
            iconOffset = 0;
        }

        editingComponent = treeColumn ?
                getCellEditorComponent(treeTable, value, sel, row, col, exp, leaf) :
                getCellEditorComponent(treeTable, value, sel, row, col);
        if (editingComponent != null)
            add(editingComponent);
        return this;
    }

    protected Component getCellEditorComponent(TreeTable treeTable,
                                               Object value, boolean selected, int row, int column, boolean expanded, boolean leaf) {
        if (editor instanceof TreeTableCellEditor)
            return ((TreeTableCellEditor)editor).getTreeTableCellEditorComponent(
                    treeTable, value, selected, row, column, expanded, leaf);
        return null;
    }

    protected Component getCellEditorComponent(TreeTable treeTable,
                                               Object value, boolean selected, int row, int column) {
        if (editor instanceof TreeTableCellEditor)
            return ((TreeTableCellEditor)editor).getTreeTableCellEditorComponent(
                    treeTable, value, selected, row, column);
        return null;
    }

    @Override
    public Object getCellEditorValue() {
        return editor.getCellEditorValue();
    }

    @Override
    public void addCellEditorListener(CellEditorListener l) {
        editor.addCellEditorListener(l);
    }

    @Override
    public void cancelCellEditing() {
        editor.cancelCellEditing();
    }

    @Override
    public boolean isCellEditable(EventObject e) {
        return editor.isCellEditable(e);
    }

    @Override
    public void removeCellEditorListener(CellEditorListener l) {
        editor.removeCellEditorListener(l);
    }

    @Override
    public boolean shouldSelectCell(EventObject anEvent) {
        return editor.shouldSelectCell(anEvent);
    }

    @Override
    public boolean stopCellEditing() {
        return editor.stopCellEditing();
    }

    public void doLayout() {
        if(editingComponent != null) {
            int x;
            int w;
            if (getComponentOrientation().isLeftToRight()) {
                x = iconOffset;
                w = getWidth() - x;
            } else {
                x = 0;
                w = getWidth()-iconOffset;
            }
            editingComponent.setBounds(x, 0, w, getHeight());
        }
    }

    public Dimension getPreferredSize() {
        if(editingComponent != null) {
            Dimension pSize = editingComponent.getPreferredSize();
            pSize.width += iconOffset + 5;
            if(rendererSize != null)
                pSize.height = Math.max(pSize.height, rendererSize.height);
            return pSize;
        }
        return new Dimension(0, 0);
    }

    @Override
    public Component getTableCellEditorComponent(JTable table, Object value, boolean selected, int row, int column) {
        throw new UnsupportedOperationException();
    }
}
