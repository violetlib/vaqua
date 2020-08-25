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
import java.awt.Rectangle;

import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.plaf.basic.BasicHTML;
import javax.swing.table.TableCellRenderer;

public class DefaultTreeTableCellRenderer extends JLabel
        implements TreeTableCellRenderer, TableCellRenderer {

    public DefaultTreeTableCellRenderer() {
    }

    @Override
    public Component getTreeTableCellRendererComponent(TreeTable treeTable,
                                                       Object value, boolean selected, boolean hasFocus, int row, int column) {
        treeTable.getUI().configureCellRenderer(this, treeTable,
                value, selected, hasFocus, row, column);
        setValue(value);
        return this;
    }

    @Override
    public Component getTreeTableCellRendererComponent(TreeTable treeTable,
                                                       Object value, boolean selected, boolean hasFocus, int row,
                                                       int column, boolean expanded, boolean leaf) {
        treeTable.getUI().configureCellRenderer(this, treeTable,
                value, selected, hasFocus, row, column, expanded, leaf);
        setValue(value);
        return this;
    }

    protected void setValue(Object value) {
        setText(value == null ? "" : value.toString());
    }

    /**
     * This class implements the TableCellRenderer interface as a convenience
     * so that it can be stored as a renderer in a TableColumn.
     * <p>
     * This method is not actually implemented.
     *
     * @throws UnsupportedOperationException
     */
    @Override
    public Component getTableCellRendererComponent(JTable table, Object value,
                                                   boolean isSelected, boolean hasFocus, int row, int column) {
        throw new UnsupportedOperationException();
    }

    // Performance Overrides

    @Override
    public void invalidate() {}
    @Override
    public void validate() {}
    @Override
    public void revalidate() {}

    @Override
    public void repaint() {}
    @Override
    public void repaint(long tm, int x, int y, int w, int h) {}
    @Override
    public void repaint(Rectangle r) {}

    @Override
    protected void firePropertyChange(String propertyName, Object oldValue, Object newValue) {
        if (propertyName.equals("text")
                || propertyName.equals("labelFor")
                || propertyName.equals("displayedMnemonic")
                || ((propertyName.equals("font") || propertyName.equals("foreground"))
                && oldValue != newValue
                && getClientProperty(BasicHTML.propertyKey) != null)) {
            super.firePropertyChange(propertyName, oldValue, newValue);
        }
    }
    @Override
    public void firePropertyChange(String propertyName, byte oldValue, byte newValue) {}
    @Override
    public void firePropertyChange(String propertyName, char oldValue, char newValue) {}
    @Override
    public void firePropertyChange(String propertyName, short oldValue, short newValue) {}
    @Override
    public void firePropertyChange(String propertyName, int oldValue, int newValue) {}
    @Override
    public void firePropertyChange(String propertyName, long oldValue, long newValue) {}
    @Override
    public void firePropertyChange(String propertyName, float oldValue, float newValue) {}
    @Override
    public void firePropertyChange(String propertyName, double oldValue, double newValue) {}
    @Override
    public void firePropertyChange(String propertyName, boolean oldValue, boolean newValue) {}
}
