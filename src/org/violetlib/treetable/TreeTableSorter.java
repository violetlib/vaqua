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

import java.util.List;

import javax.swing.RowSorter;
import javax.swing.SortOrder;
import javax.swing.RowSorter.SortKey;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;

import org.violetlib.treetable.event.TreeTableSorterListener;

public interface TreeTableSorter<T extends TreeModel, C extends TreeColumnModel> {

    List<? extends SortKey> getSortKeys();

    void setSortKeys(List<? extends SortKey> keys);

    void toggleSortOrder(int column);

    /**
     * Retrieves the RowSorter for the specified path,
     * creates it if necessary.
     *
     * @see #getRowSorter(Object)
     */
    RowSorter<T> getRowSorter(TreePath path);

    /**
     * Differs from the TreePath variety as it won't
     * (lacks the necessary information) create
     * the row sorter if it doesn't exist.
     *
     * @see #getRowSorter(TreePath)
     */
    RowSorter<T> getRowSorter(Object node);

    void addTreeTableSorterListener(TreeTableSorterListener l);

    void removeTreeTableSorterListener(TreeTableSorterListener l);

    void setVisible(TreePath path, List<TreePath> subPaths, boolean visible);

    void structureChanged(TreePath path, boolean newRoot);

    void nodesRemoved(TreePath path, Object[] childNodes);

    interface SortCycle {

        void setSortCycle(List<SortOrder> cycle);

        List<SortOrder> getSortCycle();
    }
}
