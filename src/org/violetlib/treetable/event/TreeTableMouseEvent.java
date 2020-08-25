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

import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;

import javax.swing.tree.TreePath;

import org.violetlib.treetable.TreeTable;

public class TreeTableMouseEvent extends MouseEvent {

    public TreeTableMouseEvent(TreeTable source, MouseEvent e) {
        super(source, e.getID(), e.getWhen(), e.getModifiers(),
                e.getX(), e.getY(), e.getXOnScreen(), e.getYOnScreen(),
                e.getClickCount(), e.isPopupTrigger(), e.getButton());
    }

    private TreePath path = null;

    private int row = Integer.MIN_VALUE;

    private int column = Integer.MIN_VALUE;

    private int treePosition = Integer.MIN_VALUE;

    @Override
    public Point getPoint() {
        return new Point(getX(), getY());
    }

    /**
     * @return the TreeTable source
     */
    public TreeTable getTreeTable() {
        return (TreeTable)getSource();
    }

    /**
     * @return the row for the location of this event
     */
    public int getRow() {
        if (row == Integer.MIN_VALUE) {
            row = getTreeTable().rowAtPoint(getPoint());
        }
        return row;
    }

    /**
     * @return the column for the location of this event
     */
    public int getColumn() {
        if (column == Integer.MIN_VALUE) {
            column = getTreeTable().columnAtPoint(getPoint());
        }
        return column;
    }

    /**
     * @return the path for the location of this event
     */
    public TreePath getTreePath() {
        if (path == null) {
            path = getTreeTable().getClosestPathForLocation(getX(), getY());
        }
        return path;
    }

    /**
     * Calculates the x distance from the tree handle. If the tree
     * handle isn't present, it is the distance from the start of the
     * path bounds.
     * <p>
     * A return value of 0 means the location is over the tree handle.
     * <p>
     * The return value will be negative for x locations that
     * fall in the leading region and positive for x locations
     * that fall in the trailing region.
     *
     * @return distance from the tree handle
     */
    public int getDistanceToTreeHandle() {
        if (treePosition == Integer.MIN_VALUE) {
            treePosition = getTreeTable().getDistanceToTreeHandle(getTreePath(), getX());
        }
        return treePosition;
    }

    /**
     * @return true if the location is over the leading margin
     * of the node bounds and not over the tree handle
     */
    public boolean isOverTreeMargin() {
        return getColumn() == getTreeTable().getHierarchicalColumn()
                && getDistanceToTreeHandle() < 0;
    }

    /**
     * @return true if the location is over the tree handle
     */
    public boolean isOverTreeHandle() {
        return getDistanceToTreeHandle() == 0;
    }

    /**
     * @return true if the location is over the path's bounds
     */
    public boolean isOverTreeNode() {
        int pos = getDistanceToTreeHandle();
        if (pos <= 0)
            return false;
        Rectangle nb = getTreeTable().getPathBounds(getTreePath());
        return pos < nb.width;
    }
}
