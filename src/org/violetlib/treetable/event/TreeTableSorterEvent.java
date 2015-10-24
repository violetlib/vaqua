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
import javax.swing.tree.TreePath;
import org.violetlib.treetable.TreeTableSorter;

public class TreeTableSorterEvent extends EventObject {

	public enum Type {
		SORT_ORDER_CHANGED, SORTED, NODE_SORTED
	}

	public TreeTableSorterEvent(TreeTableSorter<?,?> source) {
		super(source);
		type = Type.SORT_ORDER_CHANGED;
	}

	public TreeTableSorterEvent(TreeTableSorter<?,?> source, TreePath path) {
		super(source);
		type = path == null ? Type.SORTED : Type.NODE_SORTED;
		this.path = path;
	}

	private Type type;

	private TreePath path;

	public Type getType() {
		return type;
	}

	public TreePath getTreePath() {
		return path;
	}

	public TreeTableSorter<?,?> getSource() {
		return (TreeTableSorter<?,?>)super.getSource();
	}

}
