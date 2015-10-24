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

	public int convertColumnIndexToView(int modelColumnIndex);

	public int convertColumnIndexToModel(int viewColumnIndex);


	public boolean getDragEnabled();

	public void setDragEnabled(boolean dragEnabled);

	public DropMode getDropMode();

	public void setDropMode(DropMode dropMode);


    public boolean getAutoCreateColumnsFromModel();

	public void setAutoCreateColumnsFromModel(boolean autoCreateColumnsFromModel);

	public int getAutoResizeMode();

	public void setAutoResizeMode(int mode);

	public boolean getCellSelectionEnabled();

	public void setCellSelectionEnabled(boolean cellSelectionEnabled);

	public boolean getColumnSelectionAllowed();

	public void setColumnSelectionAllowed(boolean columnSelectionAllowed);

	public Color getGridColor();

	public void setGridColor(Color gridColor);

	public Dimension getIntercellSpacing();

	public void setIntercellSpacing(Dimension intercellSpacing);

	public boolean getRowSelectionAllowed();

	public void setRowSelectionAllowed(boolean rowSelectionAllowed);

	public boolean getShowHorizontalLines();

	public void setShowHorizontalLines(boolean showHorizontalLines);

	public boolean getShowVerticalLines();

	public void setShowVerticalLines(boolean showVerticalLines);

	public void setShowGrid(boolean showGrid);

	public JTableHeader getTableHeader();

	public void setTableHeader(JTableHeader tableHeader);

	public void changeSelection(int row, int column, boolean toggle, boolean extend);

	public Color getSelectionForeground();

	public void setSelectionForeground(Color selectionForeground);

	public Color getSelectionBackground();

	public void setSelectionBackground(Color selectionBackground);


	public void addPropertyChangeListener(PropertyChangeListener l);

	public void removePropertyChangeListener(PropertyChangeListener l);


	public int columnAtPoint(Point pt);

	public int rowAtPoint(Point pt);

	public Rectangle getCellBounds(int row, int col, boolean includeSpacing);

	public void setRowHeight(int height);

	public int getRowHeight(int row);

	public void setRowHeight(int row, int height);

	public void doLayout();


	public boolean editCellAt(int row, int column);

	public TableCellEditor getCellEditor();

	public boolean isEditing();

	public Component getEditorComponent();

	public int getEditingColumn();

	public int getEditingRow();



}
