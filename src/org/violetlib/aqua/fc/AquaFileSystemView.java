/*
 * Copyright (c) 2005-2013 Werner Randelshofer, Switzerland.
 * Copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * The copyright of this software is owned by Werner Randelshofer.
 * You may not use, copy or modify this software, except in
 * accordance with the license agreement you entered into with
 * Werner Randelshofer. For details see accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.io.File;
import javax.swing.*;
import javax.swing.filechooser.FileView;

import org.jetbrains.annotations.NotNull;

/**
 * An enhanced FileSystemView, which provides additional information about a file system required for Aqua file
 * choosers. This class acts as a wrapper on platform specific file system views. The resulting view is an Aqua-style
 * view on the file system.
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
    public @NotNull FileView createFileView(JFileChooser chooser) {
        return new AquaFileView(this);
    }

    private static AquaFileSystemView fileSystemView;

    /**
     * Returns a FileSystemView that can be cast into AquaFileSystemView.
     */
    public static @NotNull AquaFileSystemView getAquaFileSystemView() {
        if (fileSystemView == null) {
            fileSystemView = new OSXFileSystemView();
        }
        return fileSystemView;
    }

    /**
     * Indicate whether a file (directory) can be visited.
     * @param f The file.
     * @param isPackageTraversable True if packages should be traversable.
     * @param isApplicationTraversable True if bundled applications should be traversable.
     * @return true if and only if the file can be visited.
     */
    public @NotNull Boolean isTraversable(File f, boolean isPackageTraversable, boolean isApplicationTraversable) {
        return target.isTraversable(f);
    }
}
