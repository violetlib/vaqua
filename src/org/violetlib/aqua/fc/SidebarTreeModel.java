/*
 * @(#)SidebarTreeModel.java
 *
 * Copyright (c) 2007-2013 Werner Randelshofer, Switzerland.
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * The copyright of this software is owned by Werner Randelshofer.
 * You may not use, copy or modify this software, except in
 * accordance with the license agreement you entered into with
 * Werner Randelshofer. For details see accompanying license terms.
 */

package org.violetlib.aqua.fc;

import org.violetlib.aqua.AquaUtils;
import org.violetlib.aqua.fc.OSXFile.SystemItemInfo;

import javax.swing.*;
import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;
import java.io.File;
import java.util.*;
import java.util.List;

/**
 * SidebarTreeModel.
 *
 * @author Werner Randelshofer
 * @version $Id$
 */
public class SidebarTreeModel extends DefaultTreeModel implements TreeModelListener {

    /**
     * Holds the path to the file system tree model node where volumes are obtained. Currently the computer node.
     */
    private TreePath volumesPath;
    /**
     * Holds the FileSystemTreeModel.
     */
    private FileSystemTreeModel model;
    /**
     * Represents the "Devices" node in the sidebar.
     */
    private DefaultMutableTreeNode devicesNode;
    /**
     * Represents the "Favorites" node in the sidebar.
     */
    private DefaultMutableTreeNode favoritesNode;
    /**
     * The file system tree model node that represents the Computer.
     */
    private FileSystemTreeModel.Node computerNode;
    /**
     * The JFileChooser.
     */
    private JFileChooser fileChooser;
    /**
     * Sequential dispatcher for the lazy creation of icons.
     */
    private SequentialDispatcher dispatcher = new SequentialDispatcher();
    /**
     * The defaultUserItems are used when we fail to read the user items from
     * the sidebarFile.
     */
    private final static File[] defaultUserItems;

    static {
        defaultUserItems = new File[]{
                new File(AquaUtils.getProperty("user.home"), "Desktop"),
                new File(AquaUtils.getProperty("user.home"), "Documents"),
                new File(AquaUtils.getProperty("user.home"))
        };
    }

    /** Creates a new instance. */
    public SidebarTreeModel(JFileChooser fileChooser, TreePath volumesPath, FileSystemTreeModel model) {
        super(new DefaultMutableTreeNode(), true);

        this.fileChooser = fileChooser;
        this.volumesPath = volumesPath;
        this.model = model;
        computerNode = model.getRoot();

        devicesNode = new DefaultMutableTreeNode(UIManager.getString("FileChooser.devices"));
        devicesNode.setAllowsChildren(true);
        favoritesNode = new DefaultMutableTreeNode(UIManager.getString("FileChooser.favorites"));
        favoritesNode.setAllowsChildren(true);

        DefaultMutableTreeNode r = (DefaultMutableTreeNode) getRoot();
        r.add(favoritesNode);
        r.add(devicesNode);

        updateUserNode();
        updateDevicesNode();

        model.addTreeModelListener(this);
    }

    public void lazyValidate() {
        // throw new UnsupportedOperationException("Not yet implemented");
    }

    /**
     * Invoke the runnable when validation is complete, either now or later. This method does not block.
     */
    public void invokeWhenValid(final Runnable r) {
        r.run();
    }

    /**
     * Update the user favorites subtree.
     */
    private void updateUserNode() {
        List<Node> favorites = getUserFavorites();
        replaceNodes(favoritesNode, favorites);
    }

    /**
     * Update the devices subtree.
     */
    private void updateDevicesNode() {
        Map<String,SystemItemInfo> volumes = readVolumes();

        List<SidebarViewToModelNode> devices = new ArrayList<>();
        FileSystemTreeModel.Node computerNode = this.computerNode;
        SystemItemInfo computerInfo = volumes.get("Computer");
        if (computerInfo != null && computerInfo.isVisible) {
            devices.add(createDeviceNode(computerNode, -2, computerInfo.icon));
        }

        for (SystemItemInfo info : volumes.values()) {
            if (info != computerInfo) {
                String path = info.path;
                if (path != null) {
                    File f = new File(path);
                    devices.add(createDeviceNode(f, info.sequenceNumber, info.icon));
                }
            }
        }

        Collections.sort(devices, new SideBarViewToModelNodeComparator());
        replaceNodes(devicesNode, devices);
    }

    private SidebarViewToModelNode createDeviceNode(FileSystemTreeModel.Node modelNode, int sequenceNumber, Icon icon) {
        return new SidebarViewToModelNode(modelNode, sequenceNumber, icon);
    }

    private SidebarViewToModelNode createDeviceNode(File f, int sequenceNumber, Icon icon) {
        TreePath tp = model.toPath(f, null);
        FileSystemTreeModel.Node modelNode = (FileSystemTreeModel.Node) tp.getLastPathComponent();
        return new SidebarViewToModelNode(modelNode, sequenceNumber, icon);
    }

    private void replaceNodes(DefaultMutableTreeNode parent, List<? extends DefaultMutableTreeNode> nodes) {
        int oldCount = parent.getChildCount();
         if (oldCount > 0) {
             int[] removedIndices = new int[oldCount];
             Object[] removedChildren = new Object[oldCount];
             for (int i = 0; i < oldCount; i++) {
                 removedIndices[i] = i;
                 removedChildren[i] = parent.getChildAt(i);
             }
             parent.removeAllChildren();
             fireTreeNodesRemoved(
               SidebarTreeModel.this,
               parent.getPath(),
               removedIndices,
               removedChildren);
         }

         if (nodes.size() > 0) {
             int[] insertedIndices = new int[nodes.size()];
             Object[] insertedChildren = new Object[nodes.size()];
             for (int i = 0; i < nodes.size(); i++) {
                 insertedIndices[i] = i;
                 insertedChildren[i] = nodes.get(i);
                 if (nodes.get(i) == null) {
                     parent.add(new DefaultMutableTreeNode("null?"));
                 } else {
                     parent.add(nodes.get(i));
                 }
             }
             fireTreeNodesInserted(
               SidebarTreeModel.this,
               parent.getPath(),
               insertedIndices,
               insertedChildren);
         }
    }

    private List<Node> getUserFavorites() {
        ArrayList<Node> result = new ArrayList<>();

        List<SystemItemInfo> sidebarFavorites = OSXFile.getSidebarFiles(OSXFile.SIDEBAR_FAVORITES);
        if (sidebarFavorites != null) {
            for (SystemItemInfo favorite : sidebarFavorites) {
                //System.err.println("Favorite: " + favorite);
                String name = favorite.name;
                String path = favorite.path;
                boolean isPath = path != null && !path.isEmpty();

                if (!isPath && (name.equals("AirDrop") || name.endsWith("-AirDrop"))) {
                    // TBD
                } else if (name.equals("iCloud")) {
                    // TBD
                } else if (name.equals("All My Files")) {
                    // TBD
                } else if (favorite.path != null) {
                    File f = new File(path);
                    FileNode node = new FileNode(f);
                    Icon icon = favorite.icon;
                    if (icon != null) {
                        node.icon = icon;
                    }
                    result.add(node);
                } else {
                    AliasNode node = new AliasNode(name);
                    result.add(node);
                }
            }
        }
        return result;
    }

    private Map<String,SystemItemInfo> readVolumes() {
        Map<String,SystemItemInfo> result = new HashMap<>();

        List<SystemItemInfo> sidebarVolumes = OSXFile.getSidebarFiles(OSXFile.SIDEBAR_VOLUMES);
        if (sidebarVolumes != null) {
            for (SystemItemInfo volume : sidebarVolumes) {
                String name = volume.name;
                if (volume.id == 1) {
                    name = "Computer";
                }
                //System.err.println("Volume: " + volume);
                result.put(name, volume);
            }
        }
        return result;
    }

    public void treeNodesChanged(TreeModelEvent e) {
        if (e.getTreePath().equals(volumesPath)) {
            updateDevicesNode();
        }
    }

    public void treeNodesInserted(TreeModelEvent e) {
        if (e.getTreePath().equals(volumesPath)) {
            updateDevicesNode();
        }
    }

    public void treeNodesRemoved(TreeModelEvent e) {
        if (e.getTreePath().equals(volumesPath)) {
            updateDevicesNode();
        }
    }

    public void treeStructureChanged(TreeModelEvent e) {
        if (e.getTreePath().equals(volumesPath)) {
            updateDevicesNode();
        }
    }

    private class FileNode extends Node {

        private File file;
        private Icon icon;
        private String userName;
        private boolean isTraversable;
        /**
         * Holds a Finder label for the file represented by this node.
         * The label is a value in the interval from 0 through 7.
         * The value -1 is used, if the label could not be determined.
         */
        protected int fileLabel = -1;

        public FileNode(File file) {
            this.file = file;
            // userName = fileChooser.getName(file);
            isTraversable = file.isDirectory();
        }

        public File getResolvedFile() {
            return file;
        }

        public File getFile() {
            return file;
        }

        public Icon getIcon() {
            if (icon == null) {
                icon = (isTraversable())
                        ? UIManager.getIcon("FileView.directoryIcon")
                        : UIManager.getIcon("FileView.fileIcon");
                //
                if (!UIManager.getBoolean("FileChooser.speed")) {
                    dispatcher.dispatch(new Worker<Icon>() {

                        public Icon construct() {
                            return fileChooser.getIcon(file);
                        }

                        @Override
                        public void done(Icon value) {
                            icon = value;
                            int[] changedIndices = {getParent().getIndex(FileNode.this)};
                            Object[] changedChildren = {FileNode.this};
                            SidebarTreeModel.this.fireTreeNodesChanged(
                                    SidebarTreeModel.this,
                                    favoritesNode.getPath(),
                                    changedIndices, changedChildren);
                        }
                    });
                }
            }
            return icon;
        }

        public String getUserName() {
            if (userName == null) {
                userName = fileChooser.getName(file);
            }
            return userName;
        }

        public boolean isTraversable() {
            return isTraversable;
        }

        @Override
        public String toString() {
            return getUserName();
        }
    }

    /**
     * An AliasNode is resolved as late as possible.
     */
    private abstract class Node extends DefaultMutableTreeNode implements SidebarTreeFileNode {

        @Override
        public boolean getAllowsChildren() {
            return false;
        }
    }

    /**
     * An AliasNode is resolved as late as possible.
     */
    private class AliasNode extends Node {

        private File file;
        private Icon icon;
        private String userName;
        private String aliasName;
        private boolean isTraversable;
        /**
         * Holds a Finder label for the file represented by this node.
         * The label is a value in the interval from 0 through 7.
         * The value -1 is used, if the label could not be determined.
         */
        protected int fileLabel = -1;

        public AliasNode(String aliasName) {
            this.file = null;
            this.aliasName = aliasName;
            isTraversable = true;
        }

        public File getResolvedFile() {
            if (file == null) {
                icon = null; // clear cached icon!
                file = OSXFile.resolveAlias(new File(aliasName), false);
            }
            return file;
        }

        public Icon getIcon() {
            if (icon == null) {
                // Note: We clear this icon, when we resolve the alias
                icon = (isTraversable())
                        ? UIManager.getIcon("FileView.directoryIcon")
                        : UIManager.getIcon("FileView.fileIcon");
                //
                if (file != null && !UIManager.getBoolean("FileChooser.speed")) {
                    dispatcher.dispatch(new Worker<Icon>() {

                        public Icon construct() {
                            return fileChooser.getIcon(file);
                        }

                        @Override
                        public void done(Icon value) {
                            icon = value;

                            int[] changedIndices = new int[]{getParent().getIndex(AliasNode.this)};
                            Object[] changedChildren = new Object[]{AliasNode.this};
                            SidebarTreeModel.this.fireTreeNodesChanged(
                                    SidebarTreeModel.this,
                                    ((DefaultMutableTreeNode) AliasNode.this.getParent()).getPath(),
                                    changedIndices, changedChildren);
                        }
                    });
                }
            }
            return icon;
        }

        public String getUserName() {
            if (userName == null) {
                if (file != null) {
                    userName = fileChooser.getName(file);
                }
            }
            return (userName == null) ? aliasName : userName;
        }

        public boolean isTraversable() {
            return isTraversable;
        }

        @Override
        public String toString() {
            return getUserName();
        }
    }

    /** Note: SidebaViewToModelNode must not implement Comparable and must
     * not override equals()/hashCode(), because this confuses the layout algorithm
     * in JTree.
     */
    private class SidebarViewToModelNode extends Node {

        private FileSystemTreeModel.Node target;
        private int sequenceNumber;
        private Icon icon;

        public SidebarViewToModelNode(FileSystemTreeModel.Node target, int sequenceNumber, Icon icon) {
            this.target = target;
            this.sequenceNumber = sequenceNumber;
            this.icon = icon;
        }

        public File getResolvedFile() {
            return target.getResolvedFile();
        }

        public String getUserName() {
            return target.getUserName();
        }

        public Icon getIcon() {
            if (icon != null) {
                return icon;
            }
            return target.getIcon();
        }

        public FileSystemTreeModel.Node getTarget() {
            return target;
        }

        public int getSequenceNumber() {
            return sequenceNumber;
        }

        @Override
        public String toString() {
            return target.toString();
        }
    }

    private class SideBarViewToModelNodeComparator implements Comparator<SidebarViewToModelNode> {

        public int compare(SidebarViewToModelNode n1, SidebarViewToModelNode n2) {
            return n1.getSequenceNumber() - n2.getSequenceNumber();
        }
    }
}
