/*
 * @(#)SubtreeTreeModel.java
 *
 * Copyright (c) 2004-2013 Werner Randelshofer, Switzerland.
 * You may not use, copy or modify this file, except in compliance with the
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import javax.swing.event.EventListenerList;
import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.Arrays;
import java.util.LinkedList;

/**
 * SubtreeTreeModel.
 *
 * @author Werner Randelshofer
 * @version $Id$
 */
public class SubtreeTreeModel implements TreeModel, TreeModelListener {

    /** We store all our listeners here. */
    protected EventListenerList listenerList = new EventListenerList();
    protected PropertyChangeSupport changeSupport = new PropertyChangeSupport(this);
    private TreeModel target;
    private Object subtreeRoot;
    private Object[] rootPath;

    /** Creates a new instance. */
    public SubtreeTreeModel(TreeModel target) {
        this.target = target;
        this.subtreeRoot = target.getRoot();
        this.rootPath = new Object[]{subtreeRoot};
        target.addTreeModelListener(this);
    }

    public TreeModel getTargetModel() {
        return target;
    }

    public void setPathToRoot(TreePath newValue) {
        setPathToRoot(newValue.getPath());
    }

    /*
     * Note: For efficiency reasons this method stores the passed in array
     * internally without copying it. Do not modify the array after
     * invoking this method.
     */
    public void setPathToRoot(Object[] newValue) {
        Object[] oldValue = this.rootPath;
        if (!Arrays.equals(oldValue, newValue)) {
            this.rootPath = newValue;
            this.subtreeRoot = newValue[newValue.length - 1];
            changeSupport.firePropertyChange("rootPath", oldValue, newValue);
            fireTreeStructureChanged(subtreeRoot, new Object[]{rootPath[rootPath.length - 1]});
        }
    }

    public TreePath getPathToRoot() {
        return new TreePath(rootPath);
    }

    public Object getChild(Object parent, int index) {
        return target.getChild(parent, index);
    }

    public int getChildCount(Object parent) {
        return target.getChildCount(parent);
    }

    public int getIndexOfChild(Object parent, Object child) {
        return target.getIndexOfChild(parent, child);
    }

    public Object getRoot() {
        return subtreeRoot;
    }

    public boolean isLeaf(Object node) {
        return node!=getRoot()&&target.isLeaf(node);
    }

    public TreePath toFullPath(TreePath subtreePath) {
        return subtreePath == null ? new TreePath(rootPath) : new TreePath(toFullPath(subtreePath.getPath()));
    }

    public Object[] toFullPath(Object[] subtreePath) {
        /*
        if (subtreeRoot != target.getRoot()) {
        Object[] components = new Object[rootPath.length + subtreePath.length - 1];
        System.arraycopy(rootPath, 0, components, 0, rootPath.length);
        System.arraycopy(subtreePath, 1, components, rootPath.length, subtreePath.length - 1);
        return components;
        } else {
        return subtreePath;
        }*/
        LinkedList list = new LinkedList(Arrays.asList(subtreePath));
        if (subtreePath.length > 0) {
            TreeNode node = (TreeNode) subtreePath[0];
            while ((node = node.getParent()) != null) {
                list.addFirst(node);
            }
        }
        return list.toArray();
    }

    public void valueForPathChanged(TreePath path, Object newValue) {
        target.valueForPathChanged(toFullPath(path), newValue);
    }
    //
    //  Events
    //
    /**
     * Adds a listener for the TreeModelEvent posted after the tree changes.
     *
     * @see     #removeTreeModelListener
     * @param   l       the listener to add
     */
    public void addTreeModelListener(TreeModelListener l) {
        listenerList.add(TreeModelListener.class, l);
    }

    /**
     * Removes a listener previously added with <B>addTreeModelListener()</B>.
     *
     * @see     #addTreeModelListener
     * @param   l       the listener to remove
     */
    public void removeTreeModelListener(TreeModelListener l) {
        listenerList.remove(TreeModelListener.class, l);
    }

    /**
     * Adds a property change listener.
     */
    public void addPropertyChangeListener(PropertyChangeListener l) {
        changeSupport.addPropertyChangeListener(l);
    }

    /**
     * Removes a property change listener.
     */
    public void removePropertyChangeListener(PropertyChangeListener l) {
        changeSupport.removePropertyChangeListener(l);
    }

    /**
     * Notifies all listeners that have registered interest for
     * notification on this event type.  The event instance
     * is lazily created using the parameters passed into
     * the fire method.
     *
     * @param source the node being changed
     * @param path the path to the root node
     * @param childIndices the indices of the changed elements
     * @param children the changed elements
     * @see EventListenerList
     */
    protected void fireTreeNodesChanged(Object source, Object[] path,
            int[] childIndices,
            Object[] children) {
        // Guaranteed to return a non-null array
        Object[] listeners = listenerList.getListenerList();
        TreeModelEvent e = null;
        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length - 2; i >= 0; i -= 2) {
            if (listeners[i] == TreeModelListener.class) {
                // Lazily create the event:
                if (e == null) {
                    e = new TreeModelEvent(source, path,
                            childIndices, children);
                }
                ((TreeModelListener) listeners[i + 1]).treeNodesChanged(e);
            }
        }
    }

    /**
     * Notifies all listeners that have registered interest for
     * notification on this event type.  The event instance
     * is lazily created using the parameters passed into
     * the fire method.
     *
     * @param source the node where new elements are being inserted
     * @param path the path to the root node
     * @param childIndices the indices of the new elements
     * @param children the new elements
     * @see EventListenerList
     */
    protected void fireTreeNodesInserted(Object source, Object[] path,
            int[] childIndices,
            Object[] children) {
        // Guaranteed to return a non-null array
        Object[] listeners = listenerList.getListenerList();
        TreeModelEvent e = null;
        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length - 2; i >= 0; i -= 2) {
            if (listeners[i] == TreeModelListener.class) {
                // Lazily create the event:
                if (e == null) {
                    e = new TreeModelEvent(source, path,
                            childIndices, children);
                }
                ((TreeModelListener) listeners[i + 1]).treeNodesInserted(e);
            }
        }
    }

    /**
     * Notifies all listeners that have registered interest for
     * notification on this event type.  The event instance
     * is lazily created using the parameters passed into
     * the fire method.
     *
     * @param source the node where elements are being removed
     * @param path the path to the root node
     * @param childIndices the indices of the removed elements
     * @param children the removed elements
     * @see EventListenerList
     */
    protected void fireTreeNodesRemoved(Object source, Object[] path,
            int[] childIndices,
            Object[] children) {
        // Guaranteed to return a non-null array
        Object[] listeners = listenerList.getListenerList();
        TreeModelEvent e = null;
        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length - 2; i >= 0; i -= 2) {
            if (listeners[i] == TreeModelListener.class) {
                // Lazily create the event:
                if (e == null) {
                    e = new TreeModelEvent(source, path,
                            childIndices, children);
                }
                ((TreeModelListener) listeners[i + 1]).treeNodesRemoved(e);
            }
        }
    }

    /**
     * Notifies all listeners that have registered interest for
     * notification on this event type.  The event instance
     * is lazily created using the parameters passed into
     * the fire method.
     *
     * @param source the node where the tree model has changed
     * @param path the path to the root node
     * @see EventListenerList
     */
    protected void fireTreeStructureChanged(Object source, Object[] path) {
        // Guaranteed to return a non-null array
        Object[] listeners = listenerList.getListenerList();
        TreeModelEvent e = null;
        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length - 2; i >= 0; i -= 2) {
            if (listeners[i] == TreeModelListener.class) {
                // Lazily create the event:
                if (e == null) {
                    e = new TreeModelEvent(source, path);
                }
                ((TreeModelListener) listeners[i + 1]).treeStructureChanged(e);
            }
        }
    }

    /*
     * Notifies all listeners that have registered interest for
     * notification on this event type.  The event instance
     * is lazily created using the parameters passed into
     * the fire method.
     *
     * @param source the node where the tree model has changed
     * @param path the path to the root node
     * @see EventListenerList
     */
    private void fireTreeStructureChanged(Object source, TreePath path) {
        // Guaranteed to return a non-null array
        Object[] listeners = listenerList.getListenerList();
        TreeModelEvent e = null;
        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length - 2; i >= 0; i -= 2) {
            if (listeners[i] == TreeModelListener.class) {
                // Lazily create the event:
                if (e == null) {
                    e = new TreeModelEvent(source, path);
                }
                ((TreeModelListener) listeners[i + 1]).treeStructureChanged(e);
            }
        }
    }

    /**
     * Returns true, if the specified path if a descendant
     * path of the path to root of this subtree model.
     */
    public boolean isDescendant(Object[] fullpath) {
        if (fullpath.length >= rootPath.length) {
            for (int i = 0; i < rootPath.length; i++) {
                if (rootPath[i] != fullpath[i]) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    public Object[] toSubPath(Object[] fullPath) {
        if (isDescendant(fullPath)) {
            Object[] subtreePath = new Object[fullPath.length - rootPath.length + 1];
            System.arraycopy(fullPath, rootPath.length - 1, subtreePath, 0, subtreePath.length);
            return subtreePath;
        } else {
            return null;
        }
    }

    /**
     * Returns true, if the specified path if a descendant
     * path of the path to root of this subtree model.
     */
    public boolean isDescendant(TreePath fullpath) {
        if (fullpath.getPathCount() >= rootPath.length) {
            for (int i = 0; i < rootPath.length; i++) {
                if (rootPath[i] != fullpath.getPathComponent(i)) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    public TreePath toSubPath(TreePath fullPath) {
        Object[] subPath = toSubPath(fullPath.getPath());
        return (subPath == null) ? null : new TreePath(subPath);
    }

    public void treeNodesChanged(TreeModelEvent event) {
        Object[] subtreePath = toSubPath(event.getPath());
        if (subtreePath != null) {
            fireTreeNodesChanged(event.getSource(), subtreePath, event.getChildIndices(), event.getChildren());
        }
    }

    public void treeNodesInserted(TreeModelEvent event) {
        Object[] subtreePath = toSubPath(event.getPath());

        if (subtreePath != null) {
            fireTreeNodesInserted(event.getSource(), subtreePath, event.getChildIndices(), event.getChildren());
        }
    }

    public void treeNodesRemoved(TreeModelEvent event) {
        // XXX - We should check here if the rootPath was removed !!!
        Object[] subtreePath = toSubPath(event.getPath());
        if (subtreePath != null) {
            fireTreeNodesRemoved(event.getSource(), subtreePath, event.getChildIndices(), event.getChildren());
        }
    }

    public void treeStructureChanged(TreeModelEvent event) {
        // XXX - We should check here if the rootPath still exists !!!
        Object[] subtreePath = toSubPath(event.getPath());
        if (subtreePath != null) {
            fireTreeStructureChanged(event.getSource(), subtreePath);
        }
    }
}
