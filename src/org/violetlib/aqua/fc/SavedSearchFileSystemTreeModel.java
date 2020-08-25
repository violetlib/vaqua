/*
 * Copyright (c) 2014-2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the
 * license agreement. For details see accompanying license terms.
 */

package org.violetlib.aqua.fc;

import javax.swing.*;
import javax.swing.tree.TreePath;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * A file system model for the results of a saved search.
 */
public class SavedSearchFileSystemTreeModel extends FileSystemTreeModel {

    private File savedSearchFile;

    public SavedSearchFileSystemTreeModel(JFileChooser fc, File savedSearchFile) {
        super(fc);
        this.savedSearchFile = savedSearchFile;
        root = new SavedSearchNode(savedSearchFile, false);
    }

    public TreePath toPath(File file, TreePath templatePath) {

        /*
          When this method is called, templatePath should be null or the root path and file should be absolute.
        */

        if (savedSearchFile.equals(file)) {
            return new TreePath(getRoot());
        }

        return searchForFile(root, file);
    }

    private TreePath searchForFile(FileSystemTreeModel.Node node, File f) {
        File nf = node.getResolvedFile();
        if (nf != null) {
            if (f.equals(nf)) {
                return new TreePath(node.getPath());
            }
            int count = node.getChildCount();
            for (int index = 0; index < count; index++) {
                FileSystemTreeModel.Node child = (FileSystemTreeModel.Node) node.getChildAt(index);
                if (child != null) {
                    TreePath path = searchForFile(child, f);
                    if (path != null) {
                        return path;
                    }
                }
            }
        }
        return null;
    }

    /**
      * A node representing a saved search. A saved search is represented as a file. However, to the user it appears
      * on the sidebar and acts like a root directory.
    */
    public class SavedSearchNode extends DirectoryNode {

        public SavedSearchNode(File savedSearchFile, boolean isHidden) {
            super(savedSearchFile, isHidden);
            setTraversable(true);
        }

        @Override
        public String getFileKind() {
            return "Saved Search";
        }

        @Override
        public boolean isAlias() {
            return false;
        }

        @Override
        protected File[] getFiles() {
            File savedSearchFile = getFile();

            File[] files = OSXFile.executedSavedSearch(savedSearchFile);

            if (DEBUG) {
                String msg = "SavedSearchNode getFiles " + savedSearchFile;
                if (files != null) {
                    msg += " returned " + files.length + " file(s)";
                } else {
                    msg += " failed";
                }

                System.out.println(msg);
            }

            JFileChooser fileChooser = getFileChooser();
            int fileSelectionMode = fileChooser.getFileSelectionMode();
            int dialogType = fileChooser.getDialogType();
            if (fileSelectionMode == JFileChooser.FILES_ONLY && dialogType == JFileChooser.OPEN_DIALOG) {
                files = removeDirectories(files);
            } else if (fileSelectionMode == JFileChooser.DIRECTORIES_ONLY || dialogType == JFileChooser.SAVE_DIALOG) {
                files = removeNonDirectories(files);
            }

            return files;
        }
    }

    private File[] removeDirectories(File[] fs) {
        List<File> result = new ArrayList<>();
        for (File f : fs) {
            if (!(f.isDirectory() && !OSXFile.isVirtualFile(f))) {
                result.add(f);
            }
        }
        return result.toArray(new File[0]);
    }

    private File[] removeNonDirectories(File[] fs) {
        List<File> result = new ArrayList<>();
        for (File f : fs) {
            if (f.isDirectory() && !OSXFile.isVirtualFile(f)) {
                result.add(f);
            }
        }
        return result.toArray(new File[0]);
    }
}
