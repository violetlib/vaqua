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

import java.text.Collator;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

import javax.swing.DefaultRowSorter;
import javax.swing.RowFilter;
import javax.swing.SortOrder;
import javax.swing.RowSorter.SortKey;
import javax.swing.event.EventListenerList;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;

import org.violetlib.treetable.event.TreeTableSorterEvent;
import org.violetlib.treetable.event.TreeTableSorterListener;


public class DefaultTreeTableSorter<T extends TreeModel, C extends TreeColumnModel, I>
		implements TreeTableSorter<T,C>, TreeTableSorter.SortCycle {

	public static final List<SortOrder> ASCENDING_DESCENDING =
		Collections.unmodifiableList(Arrays.asList(
				SortOrder.ASCENDING, SortOrder.DESCENDING));

	public static final List<SortOrder> ASCENDING_DESCENDING_UNSORTED =
		Collections.unmodifiableList(Arrays.asList(
				SortOrder.ASCENDING, SortOrder.DESCENDING, SortOrder.UNSORTED));

	public static final Comparator<Object> COMPARABLE_COMPARATOR =
		new Comparator<Object>() {
			@SuppressWarnings("unchecked")
			public int compare(Object a, Object b) {
				return ((Comparable)a).compareTo((Comparable)b);
			}
		};

	public DefaultTreeTableSorter(T tm, C cm) {
		treeModel = tm;
		columnModel = cm;
		sorters = new IdentityHashMap<Object,NodeSorter>();
		sorters.put(tm.getRoot(), new NodeSorter(tm.getRoot()));
	}

	protected EventListenerList listenerList = new EventListenerList();

	private T treeModel;

	private C columnModel;

	private IdentityHashMap<Object,NodeSorter> sorters;

	private List<? extends SortKey> sortKeys = Collections.emptyList();

	private boolean[] isSortable;

	@SuppressWarnings("unchecked")
	private Comparator[] comparators;

	private RowFilter<? super T, ? super I> rowFilter;

	private List<SortOrder> sortCycle = ASCENDING_DESCENDING;

	private int maxSortKeys = 3;

	private boolean sortsOnUpdates;

	@Override
	public NodeSorter getRowSorter(Object node) {
		return sorters.get(node);
	}

	@Override
	public NodeSorter getRowSorter(TreePath path) {
		Map<Object,NodeSorter> sorterMap = sorters;
		NodeSorter sorter = sorterMap.get(path.getPathComponent(0));
		for (int idx=1, count=path.getPathCount(); idx<count; idx++) {
			Object node = path.getPathComponent(idx);
			sorter = sorter.getChildSorter(node, sorterMap);
		}
		return sorter;
	}


	public T getTreeModel() {
		return treeModel;
	}

	public C getTreeColumnModel() {
		return columnModel;
	}

	public boolean getSortsOnUpdates() {
		return sortsOnUpdates;
	}

	public void setSortsOnUpdates(boolean sorts) {
		sortsOnUpdates = sorts;
	}

	public int getMaxSortKeys() {
		return maxSortKeys;
	}

    public void setMaxSortKeys(int max) {
        if (max < 1)
            throw new IllegalArgumentException("Invalid max");
        maxSortKeys = max;
    }

    public void setSortable(int column, boolean sortable) {
    	checkColumn(column);
    	if (isSortable == null) {
    		if (sortable)
    			return;
    		isSortable = new boolean[columnModel.getColumnCount()];
    		Arrays.fill(isSortable, true);
    	}
    	isSortable[column] = sortable;
    }

    public boolean isSortable(int column) {
    	return isSortable == null || isSortable[column];
    }

    public void setComparator(int column, Comparator<?> comparator) {
    	checkColumn(column);
    	if (comparators == null) {
    		if (comparator == null)
    			return;
    		comparators = new Comparator[columnModel.getColumnCount()];
    	}
    	comparators[column] = comparator;
    }

    boolean isComparatorSet(int column) {
    	return comparators != null && comparators[column] != null;
    }

    public Comparator<?> getComparator(int column) {
    	if (isComparatorSet(column))
    		return comparators[column];
    	Class<?> cls = columnModel.getColumnClass(column);
    	if (cls == String.class)
    		return Collator.getInstance();
    	if (Comparable.class.isAssignableFrom(cls))
    		return COMPARABLE_COMPARATOR;
    	return Collator.getInstance();
    }

    public void setRowFilter(RowFilter<? super T, ? super I> filter) {
    	if (filter == null && rowFilter == null)
    		return;
    	rowFilter = filter;
    	sort();
    }

    public RowFilter<? super T, ? super I> getRowFilter() {
    	return rowFilter;
    }

	@Override
	public List<? extends SortKey> getSortKeys() {
		return sortKeys;
	}

	@Override
	public void setSortKeys(List<? extends SortKey> keys) {
		List<? extends SortKey> old = sortKeys;
		if (keys != null && !keys.isEmpty()) {
			sortKeys = Collections.unmodifiableList(
					new ArrayList<SortKey>(keys));
		} else {
			sortKeys = Collections.emptyList();
		}
		if (!sortKeys.equals(old)) {
			fireSortOrderChanged();
			sort();
		}
	}

	@Override
	public void toggleSortOrder(int column) {
		checkColumn(column);
		if (isSortable(column)) {
			List<SortKey> keys = toggleSortOrder(
					getSortKeys(), getSortCycle(), column, getMaxSortKeys());
			setSortKeys(keys);
		}
	}

	// adapted from DefaultRowSorter.toggleSortOrder
	static List<SortKey> toggleSortOrder(List<? extends SortKey> sortKeys,
			List<SortOrder> sortCycle, int column, int maxSortKeys) {
		List<SortKey> keys = new ArrayList<SortKey>(sortKeys);
		SortKey sortKey;
		int sortIndex;
		for (sortIndex = keys.size() - 1; sortIndex >= 0; sortIndex--) {
			if (keys.get(sortIndex).getColumn() == column) {
				break;
			}
		}
		if (sortIndex == -1) {
			// Key doesn't exist
			sortKey = new SortKey(column, sortCycle.get(0));
			keys.add(0, sortKey);
		} else if (sortIndex == 0) {
			// It's the primary sorting key, toggle it
			SortKey key = keys.get(0);
			int idx = sortCycle.indexOf(key.getSortOrder());
			if (idx < 0 || ++idx >= sortCycle.size())
				idx = 0;
			keys.set(0, new SortKey(key.getColumn(), sortCycle.get(idx)));
		} else {
			// It's not the first, but was sorted on, remove old
			// entry, insert as first with ascending.
			keys.remove(sortIndex);
			keys.add(0, new SortKey(column, sortCycle.get(0)));
		}
		if (keys.size() > maxSortKeys) {
			keys = keys.subList(0, maxSortKeys);
		}
		return keys;
	}

	public List<SortOrder> getSortCycle() {
		return sortCycle;
	}

	public void setSortCycle(List<SortOrder> sortCycle) {
		if (sortCycle.isEmpty())
			throw new IllegalArgumentException();
		this.sortCycle = sortCycle;
	}


	private void checkColumn(int column) {
		if (column < 0 || column >= columnModel.getColumnCount())
			throw new IndexOutOfBoundsException();
	}


	public void sort() {
		getRowSorter(treeModel.getRoot()).sort(true);
		fireSorterChanged();
	}


	public void addTreeTableSorterListener(TreeTableSorterListener l) {
		listenerList.add(TreeTableSorterListener.class, l);
	}

	public void removeTreeTableSorterListener(TreeTableSorterListener l) {
		listenerList.remove(TreeTableSorterListener.class, l);
	}

	protected void fireSortOrderChanged() {
		fire(new TreeTableSorterEvent(this));
	}

	protected void fireSorterChanged() {
		fire(new TreeTableSorterEvent(this, null));
	}

	protected void fireRowSorterChanged(TreePath path) {
		fire(new TreeTableSorterEvent(this, path));
	}

	private void fire(TreeTableSorterEvent e) {
		Object[] listeners = listenerList.getListenerList();
		for (int i = listeners.length - 2; i >= 0; i -= 2) {
			if (listeners[i] == TreeTableSorterListener.class) {
				((TreeTableSorterListener)listeners[i + 1]).sorterChanged(e);
			}
		}
	}

	@Override
	public void structureChanged(TreePath path, boolean newRoot) {
		if (newRoot) {
			sorters.clear();
			sorters.put(treeModel.getRoot(), new NodeSorter(treeModel.getRoot()));
		} else {
			NodeSorter s = getRowSorter(path.getLastPathComponent());
			s.removeAllChildren(sorters);
			// TODO: rebuild here?
		}
	}

	@Override
	public void nodesRemoved(TreePath path, Object[] childNodes) {
		NodeSorter sorter = getRowSorter(path.getLastPathComponent());
		if (sorter != null)
			sorter.remove(childNodes, sorters);
	}

	@Override
	public void setVisible(TreePath path, List<TreePath> subPaths, boolean visible) {
		NodeSorter sorter = getRowSorter(path);
		sorter.setVisible(visible);
		if (visible) {
			for (TreePath p : subPaths) {
				NodeSorter s = sorter;
				for (int idx=path.getPathCount(), count=p.getPathCount(); idx<count; idx++) {
					Object node = p.getPathComponent(idx);
					s = s.getChildSorter(node, sorters);
					s.setVisible(true);
				}
			}
			sorter.sort(true);
		}
	}


	public class NodeSorter extends DefaultRowSorter<T,I> implements SortCycle {

		public NodeSorter(Object root) {
			this(null, root);
			setVisible(true);
		}

		public NodeSorter(NodeSorter par, Object node) {
			parent = par;
			setModelWrapper(new TreeTableWrapper(node));
			children = createChildren();
			if (parent != null)
				setMaxSortKeys(Integer.MAX_VALUE);
		}

		private NodeSorter parent;

		private Map<Object,NodeSorter> children;

		private List<SortOrder> sortCycle = ASCENDING_DESCENDING_UNSORTED;

		private boolean visible;

		protected Map<Object,NodeSorter> createChildren() {
			return new IdentityHashMap<Object,NodeSorter>(
					getModel().getChildCount(getNode()));
		}

		public NodeSorter getParent() {
			return parent;
		}

		DefaultTreeTableSorter<T,C,I> getMaster() {
			return DefaultTreeTableSorter.this;
		}

		NodeSorter getChildSorter(Object node, Map<Object,NodeSorter> map) {
			NodeSorter s = children.get(node);
			if (s == null && map != null) {
				s = new NodeSorter(this, node);
				children.put(node, s);
				map.put(node, s);
			}
			return s;
		}

		protected TreeTableWrapper getTreeTableModelWrapper() {
			return (TreeTableWrapper)getModelWrapper();
		}

		public Object getNode() {
			return getTreeTableModelWrapper().getNode();
		}

		public C getColumnModel() {
			return getTreeTableModelWrapper().getColumnModel();
		}

		@Override
		public Comparator<?> getComparator(int column) {
			Comparator<?> c = super.getComparator(column);
			return c != null ? c : getMaster().getComparator(column);
		}

		@Override
		protected boolean useToString(int column) {
			if (super.getComparator(column) != null
					|| getMaster().isComparatorSet(column))
				return false;
	        Class<?> columnClass = getColumnModel().getColumnClass(column);
	        if (columnClass == String.class)
	            return false;
            return !Comparable.class.isAssignableFrom(columnClass);
        }

		@Override
		public List<? extends SortKey> getSortKeys() {
			List<? extends SortKey> k = super.getSortKeys();
			return !k.isEmpty() ? k : getMaster().getSortKeys();
		}

		@Override
		public int getMaxSortKeys() {
			int m = super.getMaxSortKeys();
			return m < Integer.MAX_VALUE ? m : getMaster().getMaxSortKeys();
		}

		@Override
		public RowFilter<? super T, ? super I> getRowFilter() {
			RowFilter<? super T, ? super I> f = super.getRowFilter();
			if (f != null)
				return f;
			return getMaster().getRowFilter();
		}

		@Override
		public boolean getSortsOnUpdates() {
			return getMaster().getSortsOnUpdates();
		}

		@Override
		public boolean isSortable(int column) {
			return getMaster().isSortable(column);
		}

		public void setSortCycle(List<SortOrder> sortCycle) {
			if (sortCycle.isEmpty())
				throw new IllegalArgumentException();
			this.sortCycle = sortCycle;
		}

		public List<SortOrder> getSortCycle() {
			return sortCycle;
		}

		@Override
		public void toggleSortOrder(int column) {
			checkColumn(column);
			if (isSortable(column)) {
				List<SortKey> keys = DefaultTreeTableSorter.toggleSortOrder(
						super.getSortKeys(), getSortCycle(), column, getMaxSortKeys());
				setSortKeys(keys);
			}
		}


		@Override
		public void setSortsOnUpdates(boolean sortsOnUpdates) {
			throw new UnsupportedOperationException();
		}

		@Override
		public void setSortable(int column, boolean sortable) {
			throw new UnsupportedOperationException();
		}

		private boolean firePathEvent = true;

		void sort(boolean sortChildren) {
			if (!isVisible())
				return;
			firePathEvent = false;
			try {
				super.sort();
			} finally {
				firePathEvent = true;
			}
			if (!sortChildren)
				return;
			for (NodeSorter sorter : children.values())
				sorter.sort(sortChildren);
		}

		@Override
		protected void fireRowSorterChanged(int[] lastRowIndexToModel) {
			super.fireRowSorterChanged(lastRowIndexToModel);
			if (firePathEvent)
				getMaster().fireRowSorterChanged(getPathToRoot());
		}

		private TreePath getPathToRoot() {
			if (parent == null)
				return new TreePath(getNode());
			return parent.getPathToRoot()
				.pathByAddingChild(getNode());
		}


		public void allRowsChanged() {
			getTreeTableModelWrapper().updateRowCount();
			super.allRowsChanged();
		}

		public void rowsDeleted(int firstRow, int endRow) {
			getTreeTableModelWrapper().updateRowCount();
			super.rowsDeleted(firstRow, endRow);
		}

		public void rowsInserted(int firstRow, int endRow) {
			getTreeTableModelWrapper().updateRowCount();
			super.rowsInserted(firstRow, endRow);
		}


		public void setVisible(boolean vis) {
			if (visible != vis) {
				visible = vis;
				if (vis)
					sort(true);
			}
		}

		public boolean isVisible() {
			return visible;
		}


		void removeAllChildren(Map<Object,NodeSorter> map) {
			for (Map.Entry<Object,NodeSorter> entry : children.entrySet()) {
				map.remove(entry.getKey());
				entry.getValue().removeAllChildren(map);
			}
			children.clear();
		}

		void remove(Object[] childNodes, Map<Object,NodeSorter> map) {
			for (Object node : childNodes) {
				NodeSorter s = children.remove(node);
				if (s != null)
					s.removeAllChildren(map);
			}
		}



		protected class TreeTableWrapper extends ModelWrapper<T,I> {

			public TreeTableWrapper(Object n) {
				node = n;
				updateRowCount();
			}

			private Object node;

			private int rowCount;

			public Object getNode() {
				return node;
			}

			public C getColumnModel() {
				return columnModel;
			}

			@Override
			public int getColumnCount() {
				return columnModel.getColumnCount();
			}

			@Override
			public I getIdentifier(int row) {
				return (I)treeModel.getChild(node, row);
			}

			@Override
			public T getModel() {
				return treeModel;
			}

			@Override
			public int getRowCount() {
				return rowCount;
			}

			/**
			 * The last row count must be cached until
			 * this method is called to update it.
			 */
			public void updateRowCount() {
				rowCount = treeModel.getChildCount(node);
			}

			@Override
			public Object getValueAt(int row, int column) {
				return columnModel.getValueAt(treeModel.getChild(node, row), column);
			}

		}

	}


}


