/*
 * @(#)AquaFileView.java
 *
 * Copyright (c) 2005-2013 Werner Randelshofer, Switzerland.
 * Copyright (c) 2014-2015 Alan Snyder.
 * You may not use, copy or modify this file, except in compliance with the
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import javax.swing.*;
import javax.swing.filechooser.FileView;
import java.io.File;

/**
 * A FileView for OS X. It supports the file chooser options for traversing packages and applications. The trick is
 * that JFileChooser calls isTraversable() on a file view without knowing about the traversal options. We cache the
 * options here.
 *
 * @author  Werner Randelshofer
 * @version $Id$
 */
public class AquaFileView extends FileView {
    private AquaFileSystemView fsv;
    private boolean isPackageTraversable;
    private boolean isApplicationTraversable;

    public AquaFileView(AquaFileSystemView fsv) {
        this.fsv = fsv;
    }

    public boolean isPackageTraversable() {
        return isPackageTraversable;
    }

    public void setPackageTraversable(boolean b) {
        isPackageTraversable = b;
    }

    public boolean isApplicationTraversable() {
        return isApplicationTraversable;
    }

    public void setApplicationTraversable(boolean b) {
        isApplicationTraversable = b;
    }

    @Override
    public String getName(File f) {
        return fsv.getSystemDisplayName(f);
    }

    @Override
    public String getDescription(File f) {
        // This method is not used by the Aqua file chooser UI.
        return "";
    }

    @Override
    public String getTypeDescription(File f) {
        return fsv.getSystemTypeDescription(f);
    }

    @Override
    public Icon getIcon(File f) {
        return fsv.getSystemIcon(f);
    }

    @Override
    public Boolean isTraversable(File f) {
        return fsv.isTraversable(f, isPackageTraversable, isApplicationTraversable);
    }
}
