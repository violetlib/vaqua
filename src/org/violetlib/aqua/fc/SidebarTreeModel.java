/*
 * @(#)SidebarTreeModel.java
 *
 * Copyright (c) 2007-2013 Werner Randelshofer, Switzerland.
 * Copyright (c) 2015-2018 Alan Snyder.
 * All rights reserved.
 *
 * The copyright of this software is owned by Werner Randelshofer.
 * You may not use, copy or modify this software, except in
 * accordance with the license agreement you entered into with
 * Werner Randelshofer. For details see accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.io.File;
import java.util.*;
import javax.swing.*;
import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;

import org.jetbrains.annotations.NotNull;
import org.violetlib.aqua.AquaUtils;
import org.violetlib.aqua.fc.OSXFile.SystemItemInfo;

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
    public void invokeWhenValid(Runnable r) {
        r.run();
    }

    /**
     * Update the user favorites subtree.
     */
    private void updateUserNode() {

        // TBD: Avoid replacing valid nodes because that might alter the selection.
        // Not important currently because we never update.

        List<SidebarTreeNode> favorites = getUserFavorites();
        replaceNodes(favoritesNode, favorites, false);
    }

    /**
     * Update the devices subtree.
     */
    private void updateDevicesNode() {
        Map<String,SystemItemInfo> volumes = readVolumes();

        // Avoid replacing valid nodes because that might alter the selection

        boolean[] someNodesUpdatedState = new boolean[1];

        List<SidebarTreeNode> devices = new ArrayList<>();
        SystemItemInfo computerInfo = volumes.get("Computer");
        if (computerInfo != null && computerInfo.isVisible()) {
            devices.add(createOrFindDeviceNode(computerInfo, someNodesUpdatedState));
        }

        for (SystemItemInfo info : volumes.values()) {
            if (info != computerInfo) {
                String path = info.getPath();
                File f = new File(path);
                SidebarTreeNode node = createOrFindDeviceNode(info, someNodesUpdatedState);
                if (!devices.contains(node)) {
                    devices.add(node);
                }
            }
        }

        Collections.sort(devices, new SidebarTreeNodeComparator());
        replaceNodes(devicesNode, devices, someNodesUpdatedState[0]);
    }

    private SidebarTreeNode createOrFindDeviceNode(@NotNull SystemItemInfo info, boolean[] updatedState) {
        File f = new File(info.getPath());
        int count = devicesNode.getChildCount();
        for (int index = 0; index < count; index++) {
            TreeNode node = devicesNode.getChildAt(index);
            if (node instanceof SidebarTreeNode) {
                SidebarTreeNode n = (SidebarTreeNode) node;
                if (f.equals(n.getResolvedFile())) {
                    // Unlikely, but the icon may have changed
                    Icon systemIcon = info.getIcon();
                    if (systemIcon != null && systemIcon != n.getBasicIcon()) {
                        updatedState[0] = true;
                    }
                    n.update(info);
                    return n;
                }
            }
        }
        return createNode(info);
    }

    private void replaceNodes(DefaultMutableTreeNode parent,
                              List<? extends DefaultMutableTreeNode> nodes,
                              boolean someNodesPossiblyUpdated) {
        List<DefaultMutableTreeNode> removedNodes = new ArrayList<>();
        List<DefaultMutableTreeNode> insertedNodes = new ArrayList<>();
        List<DefaultMutableTreeNode> changedNodes = new ArrayList<>();
        List<Integer> removedIndexes = new ArrayList<>();
        List<Integer> insertedIndexes = new ArrayList<>();
        List<Integer> changedIndexes = new ArrayList<>();

        int oldCount = parent.getChildCount();
        for (int index = 0; index < oldCount; index++) {
            DefaultMutableTreeNode node = (DefaultMutableTreeNode) parent.getChildAt(index);
            if (!(nodes.contains(node))) {
                removedNodes.add(node);
                removedIndexes.add(index);
            } else {
                changedNodes.add(node);
                changedIndexes.add(nodes.indexOf(node));
            }
        }

        int newCount = nodes.size();
        for (int index = 0; index < newCount; index++) {
            DefaultMutableTreeNode node = nodes.get(index);
            if (node != null && !changedNodes.contains(node)) {
                insertedNodes.add(node);
                insertedIndexes.add(index);
            }
        }

        if (!removedNodes.isEmpty() || !insertedNodes.isEmpty()) {
            parent.removeAllChildren();
            for (DefaultMutableTreeNode node : nodes) {
                parent.add(node);
            }
        }

        if (!removedNodes.isEmpty()) {
            int[] indexes = new int[removedNodes.size()];
            for (int i = 0; i < indexes.length; i++) {
                indexes[i] = removedIndexes.get(i);
            }
            Object[] ns = removedNodes.toArray();
            fireTreeNodesRemoved(SidebarTreeModel.this, parent.getPath(), indexes, ns);
        }

        if (!insertedNodes.isEmpty()) {
            int[] indexes = new int[insertedNodes.size()];
            for (int i = 0; i < indexes.length; i++) {
                indexes[i] = insertedIndexes.get(i);
            }
            Object[] ns = insertedNodes.toArray();
            fireTreeNodesInserted(SidebarTreeModel.this, parent.getPath(), indexes, ns);
        }

        if (someNodesPossiblyUpdated && !changedNodes.isEmpty()) {
            int[] indexes = new int[changedNodes.size()];
            for (int i = 0; i < indexes.length; i++) {
                indexes[i] = changedIndexes.get(i);
            }
            Object[] ns = changedNodes.toArray();
            fireTreeNodesChanged(SidebarTreeModel.this, parent.getPath(), indexes, ns);
        }
    }

    private List<SidebarTreeNode> getUserFavorites() {
        List<SidebarTreeNode> result = new ArrayList<>();
        List<SystemItemInfo> sidebarFavorites = OSXFile.getSidebarFiles(OSXFile.SIDEBAR_FAVORITES);
        for (SystemItemInfo favorite : sidebarFavorites) {
            SidebarTreeNode node = createNode(favorite);
            result.add(node);
        }
        return result;
    }

    private @NotNull SidebarTreeNode createNode(@NotNull SystemItemInfo info) {
        String path = info.getPath();
        File f = new File(path);
        Icon icon = info.getIcon();
        if (icon == null) {
            icon = isTraversable(f) ? UIManager.getIcon("FileView.directoryIcon")
                                    : UIManager.getIcon("FileView.fileIcon");
        }
        return new SidebarTreeNode(info, icon);
    }

    private Map<String,SystemItemInfo> readVolumes() {
        Map<String,SystemItemInfo> result = new HashMap<>();

        List<SystemItemInfo> sidebarVolumes = OSXFile.getSidebarFiles(OSXFile.SIDEBAR_VOLUMES);
        for (SystemItemInfo volume : sidebarVolumes) {
            String name = volume.getName();
            if (volume.isComputer()) {
                name = "Computer";
            }
            result.put(name, volume);
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

    private class SidebarTreeNodeComparator implements Comparator<SidebarTreeNode> {
        public int compare(@NotNull SidebarTreeNode n1, @NotNull SidebarTreeNode n2) {
            return n1.getSequenceNumber() - n2.getSequenceNumber();
        }
    }

    public boolean isTraversable(File f) {
        if (OSXFile.isAvailable()) {
            return OSXFile.isTraversable(f);
        } else {
            return f.isDirectory();
        }
    }
}
