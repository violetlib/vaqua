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

public interface TreeTableCellRenderer {

	Component getTreeTableCellRendererComponent(TreeTable treeTable,
			Object value, boolean selected, boolean hasFocus,
			int row, int column);

	Component getTreeTableCellRendererComponent(TreeTable treeTable,
			Object value, boolean selected, boolean hasFocus,
			int row, int column, boolean expanded, boolean leaf);

}
