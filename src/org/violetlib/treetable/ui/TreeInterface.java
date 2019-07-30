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

import java.awt.Rectangle;
import java.beans.PropertyChangeListener;
import java.util.Enumeration;

import javax.swing.Scrollable;
import javax.swing.event.TreeExpansionListener;
import javax.swing.event.TreeWillExpandListener;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

public interface TreeInterface extends Scrollable {

    void setModel(TreeModel model);

    Enumeration<TreePath> getExpandedDescendants(TreePath parent);

    void collapsePath(TreePath path);

    void expandPath(TreePath path);

    void makeVisible(TreePath path);

    void scrollPathToVisible(TreePath path);

    Rectangle getPathBounds(TreePath path);

    TreePath getClosestPathForLocation(int x, int y);

    TreePath getPathForLocation(int x, int y);

    TreePath getPathForRow(int row);

    int getRowCount();

    int getClosestRowForLocation(int x, int y);

    int getRowForLocation(int x, int y);

    int getRowForPath(TreePath path);

    boolean isCollapsed(TreePath path);

    boolean isExpanded(TreePath path);

    boolean hasBeenExpanded(TreePath path);

    boolean isFixedRowHeight();

    boolean isLargeModel();

    void setLargeModel(boolean largeModel);

    boolean isRootVisible();

    void setRootVisible(boolean rootVisible);

    boolean getScrollsOnExpand();

    void setScrollsOnExpand(boolean scrollsOnExpand);

    boolean getShowsRootHandles();

    void setShowsRootHandles(boolean newValue);

    void setToggleClickCount(int clickCount);

    int getToggleClickCount();

    int getVisibleRowCount();

    void setVisibleRowCount(int newCount);

    int getRowHeight();

    void setRowHeight(int rowHeight);

    TreeSelectionModel getSelectionModel();

    void setSelectionModel(TreeSelectionModel selectionModel);

    void clearSelection();

    boolean isSelectionEmpty();

    int getSelectionCount();

    int getMaxSelectionRow();

    int getMinSelectionRow();

    boolean isPathSelected(TreePath path);

    TreePath getAnchorSelectionPath();

    void setAnchorSelectionPath(TreePath newPath);

    TreePath getLeadSelectionPath();

    void setLeadSelectionPath(TreePath newPath);

    int getLeadSelectionRow();

    boolean getExpandsSelectedPaths();

    void setExpandsSelectedPaths(boolean newValue);

    TreePath getSelectionPath();

    void addSelectionInterval(int index0, int index1);

    void addSelectionPath(TreePath path);

    void addSelectionPaths(TreePath[] paths);

    void addSelectionRows(int[] rows);

    void removeSelectionInterval(int index0, int index1);

    void removeSelectionPath(TreePath path);

    void removeSelectionPaths(TreePath[] paths);

    void removeSelectionRows(int[] rows);

    void setSelectionPath(TreePath path);

    TreePath[] getSelectionPaths();

    void setSelectionPaths(TreePath[] paths);

    int[] getSelectionRows();

    void setSelectionRows(int[] rows);

    void setSelectionInterval(int index0, int index1);

    void addPropertyChangeListener(PropertyChangeListener l);

    void removePropertyChangeListener(PropertyChangeListener l);

    void addTreeExpansionListener(TreeExpansionListener l);

    void removeTreeExpansionListener(TreeExpansionListener l);

    void addTreeWillExpandListener(TreeWillExpandListener l);

    void removeTreeWillExpandListener(TreeWillExpandListener l);

    void doLayout();
}
