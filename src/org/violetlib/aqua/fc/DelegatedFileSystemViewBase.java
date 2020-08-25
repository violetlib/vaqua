/*
 * @(#)DelegatedFileSystemViewBase.java
 *
 * Copyright (c) 2005-2013 Werner Randelshofer, Switzerland.
 * You may not use, copy or modify this file, except in compliance with the
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import javax.swing.*;
import javax.swing.filechooser.FileSystemView;
import java.io.File;
import java.io.IOException;

/**
 * A base class for implementing a file system view by delegation.
 *
 * @author  Werner Randelshofer
 * @version $Id$
 */
public abstract class DelegatedFileSystemViewBase extends FileSystemView {
    protected final FileSystemView target;

    public DelegatedFileSystemViewBase() {
        target = FileSystemView.getFileSystemView();
    }

    public File createNewFolder(File containingDir) throws IOException {
        return target.createNewFolder(containingDir);
    }

    @Override
    public boolean isRoot(File f) {
        return target.isRoot(f);
    }

    @Override
    public Boolean isTraversable(File f) {
        return target.isTraversable(f);
    }

    @Override
    public String getSystemDisplayName(File f) {
        return target.getSystemDisplayName(f);
    }

    @Override
    public String getSystemTypeDescription(File f) {
        return target.getSystemTypeDescription(f);
    }

    @Override
    public Icon getSystemIcon(File f) {
        return target.getSystemIcon(f);
    }

    @Override
    public boolean isParent(File folder, File file) {
        return target.isParent(folder, file);
    }

    @Override
    public File getChild(File parent, String fileName) {
        return target.getChild(parent, fileName);
    }

    @Override
    public boolean isFileSystem(File f) {
        return target.isFileSystem(f);
    }

    @Override
    public boolean isHiddenFile(File f) {
        return target.isHiddenFile(f);
    }

    @Override
    public boolean isFileSystemRoot(File dir) {
        return target.isFileSystemRoot(dir);
    }

    @Override
    public boolean isDrive(File dir) {
        return target.isDrive(dir);
    }

    @Override
    public boolean isFloppyDrive(File dir) {
        return target.isFloppyDrive(dir);
    }

    @Override
    public boolean isComputerNode(File dir) {
        return target.isComputerNode(dir);
    }

    @Override
    public File[] getRoots() {
        return target.getRoots();
    }

    @Override
    public File getHomeDirectory() {
        return target.getHomeDirectory();
    }

    @Override
    public abstract File getDefaultDirectory();

    @Override
    public File createFileObject(File dir, String filename) {
        return target.createFileObject(dir, filename);
    }

    @Override
    public File createFileObject(String path) {
        return target.createFileObject(path);
    }

    @Override
    public File[] getFiles(File dir, boolean useFileHiding) {
        return target.getFiles(dir, useFileHiding);
    }

    @Override
    public File getParentDirectory(File dir) {
        return target.getParentDirectory(dir);
    }
}
