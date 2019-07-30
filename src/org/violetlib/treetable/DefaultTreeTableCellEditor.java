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

import javax.swing.DefaultCellEditor;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JTree;

public class DefaultTreeTableCellEditor extends DefaultCellEditor
        implements TreeTableCellEditor {

    public DefaultTreeTableCellEditor(JCheckBox checkBox) {
        super(checkBox);
        // TODO Auto-generated constructor stub
    }

    public DefaultTreeTableCellEditor(JComboBox comboBox) {
        super(comboBox);
        // TODO Auto-generated constructor stub
    }

    public DefaultTreeTableCellEditor(JTextField textField) {
        super(textField);
        // TODO Auto-generated constructor stub
    }

    @Override
    public Component getTreeTableCellEditorComponent(TreeTable treeTable,
                                                     Object value, boolean selected, int row, int column) {
        treeTable.getUI().configureCellEditor(this,
                treeTable, value, selected, row, column);
        return getComponent();
    }

    @Override
    public Component getTreeTableCellEditorComponent(TreeTable treeTable,
                                                     Object value, boolean selected, int row, int column,
                                                     boolean expanded, boolean leaf) {
        treeTable.getUI().configureCellEditor(this,
                treeTable, value, selected, row, column, expanded, leaf);
        return getComponent();
    }

    @Override
    public /* final */ Component getTableCellEditorComponent(JTable table,
                                                             Object value, boolean selected, int row, int column) {
        return super.getTableCellEditorComponent(
                table, value, selected, row, column);
    }

    @Override
    public /* final */ Component getTreeCellEditorComponent(JTree tree,
                                                            Object value, boolean selected, boolean expanded, boolean leaf, int row) {
        return super.getTreeCellEditorComponent(
                tree, value, selected, expanded, leaf, row);
    }
}
