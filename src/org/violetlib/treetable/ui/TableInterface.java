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
package org.violetlib.treetable.ui;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.Rectangle;
import java.beans.PropertyChangeListener;

import javax.swing.DropMode;
import javax.swing.Scrollable;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellEditor;

public interface TableInterface extends Scrollable {

    int convertColumnIndexToView(int modelColumnIndex);

    int convertColumnIndexToModel(int viewColumnIndex);

    boolean getDragEnabled();

    void setDragEnabled(boolean dragEnabled);

    DropMode getDropMode();

    void setDropMode(DropMode dropMode);

    boolean getAutoCreateColumnsFromModel();

    void setAutoCreateColumnsFromModel(boolean autoCreateColumnsFromModel);

    int getAutoResizeMode();

    void setAutoResizeMode(int mode);

    boolean getCellSelectionEnabled();

    void setCellSelectionEnabled(boolean cellSelectionEnabled);

    boolean getColumnSelectionAllowed();

    void setColumnSelectionAllowed(boolean columnSelectionAllowed);

    Color getGridColor();

    void setGridColor(Color gridColor);

    Dimension getIntercellSpacing();

    void setIntercellSpacing(Dimension intercellSpacing);

    boolean getRowSelectionAllowed();

    void setRowSelectionAllowed(boolean rowSelectionAllowed);

    boolean getShowHorizontalLines();

    void setShowHorizontalLines(boolean showHorizontalLines);

    boolean getShowVerticalLines();

    void setShowVerticalLines(boolean showVerticalLines);

    void setShowGrid(boolean showGrid);

    JTableHeader getTableHeader();

    void setTableHeader(JTableHeader tableHeader);

    void changeSelection(int row, int column, boolean toggle, boolean extend);

    Color getSelectionForeground();

    void setSelectionForeground(Color selectionForeground);

    Color getSelectionBackground();

    void setSelectionBackground(Color selectionBackground);

    void addPropertyChangeListener(PropertyChangeListener l);

    void removePropertyChangeListener(PropertyChangeListener l);

    int columnAtPoint(Point pt);

    int rowAtPoint(Point pt);

    Rectangle getCellBounds(int row, int col, boolean includeSpacing);

    void setRowHeight(int height);

    int getRowHeight(int row);

    void setRowHeight(int row, int height);

    void doLayout();

    boolean editCellAt(int row, int column);

    TableCellEditor getCellEditor();

    boolean isEditing();

    Component getEditorComponent();

    int getEditingColumn();

    int getEditingRow();
}
