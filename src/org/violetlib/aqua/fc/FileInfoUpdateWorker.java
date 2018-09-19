/*
 * Copyright (c) 2015-2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import javax.swing.*;
import javax.swing.filechooser.FileView;
import javax.swing.plaf.FileChooserUI;
import java.awt.*;
import java.io.File;

/**
 * Update the tree node for a file with system provided display information that might be expensive to obtain.
 * This worker is intended to run on a background thread.
 */
public class FileInfoUpdateWorker implements Runnable {

    protected final FileSystemTreeModel.UpdatableFileNode node;
    protected final File f;
    protected final JFileChooser fc;

    public FileInfoUpdateWorker(FileSystemTreeModel.UpdatableFileNode node) {
        this.node = node;
        this.f = node.getFile();
        this.fc = node.getFileChooser();
        // Do not reference node in other methods except inside invokeLater()
    }

    @Override
    public void run() {
        int label = OSXFile.getLabel(f);
        updateFileLabel(label);
        updateIcon();
        updateCompleted();
    }

    protected void updateIcon() {

        // We have special code to get the file icon. However, the JFileChooser owns the mapping from file to icon.
        // Therefore we have to validate the use of our short circuit.

        if (!canGetFileIconDirectly()) {
            Icon icon = fc.getIcon(f);
            updateFileIcon(icon);
            return;
        }

        // Our plan is to use a Quick Look image, if one is available, and otherwise use the Launch Services icon for
        // the file. However, Quick Look can be very slow, so our plan is to install the Launch Services icon first,
        // and replace it if and when the Quick Look icon arrives.

        // Based on the above validation, this call will get the Launch Services icon, but also handles some special
        // cases.
        Icon icon = fc.getIcon(f);
        updateFileIcon(icon);

        try {
            Image quickLookImage = OSXFile.getIconImage(f, 16, true);
            updateFileIcon(new ImageIcon(quickLookImage));
        } catch (UnsupportedOperationException ex) {
        }
    }

    protected boolean canGetFileIconDirectly() {
        if (fc.getClass() != JFileChooser.class) {
            // If the application has subclassed JFileChooser, we cannot assume what it does.
            return false;
        }

        FileView fv = fc.getFileView();
        if (fv instanceof AquaFileView) {
            return true;
        }

        if (fv == null) {
            // The file chooser will ask the UI for a file view.
            FileChooserUI ui = fc.getUI();
            return ui instanceof AquaFileChooserUI;
        }

        // A custom file view is being used.
        return false;
    }

    protected void updateFileLabel(int label) {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                node.updateFileLabel(label);
            }
        });
    }

    protected void updateFileIcon(Icon icon) {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                node.updateFileIcon(icon);
            }
        });
    }

    protected void updateCompleted() {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                node.updateCompleted();
            }
        });
    }
}
