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

import org.violetlib.treetable.event.TreeColumnModelListener;

public interface TreeColumnModel {

	String getColumnName(int column);

	Class<?> getColumnClass(int column);

	int getColumnCount();

	Object getValueAt(Object node, int column);

	void setValueAt(Object value, Object node, int column);

	boolean isCellEditable(Object node, int column);

	int getHierarchicalColumn();

	void addTreeColumnModelListener(TreeColumnModelListener l);

	void removeTreeColumnModelListener(TreeColumnModelListener l);

}
