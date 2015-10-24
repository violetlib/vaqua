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
package org.violetlib.treetable.event;

import java.util.EventObject;

import javax.swing.event.TableModelEvent;
import javax.swing.tree.TreePath;

import org.violetlib.treetable.TreeColumnModel;

public class TreeColumnModelEvent extends EventObject {

	public static final int ALL_COLUMNS = TableModelEvent.ALL_COLUMNS;

	public TreeColumnModelEvent(TreeColumnModel source, TreePath path, int column) {
		super(source);
		this.path = path;
		this.column = column;
	}

	private TreePath path;

	private int column;

	public TreePath getTreePath() {
		return path;
	}

	public int getColumn() {
		return column;
	}

}
