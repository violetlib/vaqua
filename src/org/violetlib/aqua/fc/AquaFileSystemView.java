/*
 * @(#)AquaFileSystemView.java
 *
 * Copyright (c) 2005-2013 Werner Randelshofer, Switzerland.
 * All rights reserved.
 *
 * The copyright of this software is owned by Werner Randelshofer.
 * You may not use, copy or modify this software, except in
 * accordance with the license agreement you entered into with
 * Werner Randelshofer. For details see accompanying license terms.
 */

package org.violetlib.aqua.fc;

import javax.swing.*;
import javax.swing.filechooser.FileView;
import java.awt.*;
import java.io.File;

/**
 * An enhanced FileSystemView, which provides additional information about a file system required for Aqua file
 * choosers. This class acts as a wrapper on platform specific file system views. The resulting view is an Aqua-style
 * view on the file system.
 *
 * @author  Werner Randelshofer
 * @version $Id$
 */
public abstract class AquaFileSystemView extends DelegatedFileSystemViewBase {

    /**
     * Creates a new instance.
     */
    public AquaFileSystemView() {
    }

    /**
     * Convert a file (path) to canonical form.
     */
    public File canonicalize(File f) {
        return f;
    }

    /**
     * Returns the file that represents this computer node.
     */
    public abstract File getComputer();

    /**
     * Returns the file that represents the system (boot) volume of this
     * computer.
     */
    public abstract File getSystemVolume();

    /**
     * Creates a system specific file view for the specified JFileChooser.
     */
    public FileView createFileView(JFileChooser chooser) {
        return new AquaFileView(this);
    }

    private static AquaFileSystemView fileSystemView;

    /**
     * Returns a FileSystemView that can be cast into AquaFileSystemView.
     */
    public static AquaFileSystemView getAquaFileSystemView() {
        if (fileSystemView == null) {
            fileSystemView = new OSXFileSystemView();
        }
        return fileSystemView;
    }

    @Override
    public Icon getSystemIcon(File f) {
        if (f.equals(getComputer())) {
            return UIManager.getIcon("FileView.computerIcon");
        } else {
            if (OSXFile.isAvailable()) {
                try {
                    Image im = OSXFile.getIconImage(f, 16, false);
                    return new ImageIcon(im);
                } catch (UnsupportedOperationException ex) {
                }
            }

            return target.getSystemIcon(f);
        }
    }

    @Override
    public String getSystemTypeDescription(File f) {
        if (OSXFile.isAvailable()) {
            return OSXFile.getKindString(f);
        } else {
        return target.getSystemTypeDescription(f);
        }
    }

    @Override
    public Boolean isTraversable(File f) {
        if (OSXFile.isAvailable()) {
            return OSXFile.isTraversable(f);
        } else {
            return target.isTraversable(f);
        }
    }

    public Boolean isTraversable(File f, boolean isPackageTraversable, boolean isApplicationTraversable) {
        if (OSXFile.isAvailable()) {
            return OSXFile.isTraversable(f, isPackageTraversable, isApplicationTraversable);
        } else {
            return target.isTraversable(f);
        }
    }

    @Override
    public String getSystemDisplayName(File f) {
        if (f.equals(getComputer())) {
            String name = OSXFile.getComputerName();
            return name != null ? name : getSystemVolume().getName();
        } else {
            if (OSXFile.isAvailable()) {
                return OSXFile.getDisplayName(f);
            } else {
                return target.getSystemDisplayName(f);
            }
        }
    }
}
