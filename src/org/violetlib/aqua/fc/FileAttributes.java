/*
 * Copyright (c) 2019 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.io.File;
import javax.swing.*;
import javax.swing.filechooser.FileView;
import javax.swing.plaf.FileChooserUI;

import org.jetbrains.annotations.NotNull;

/**
 * Provide file attributes for display in the file chooser.
 */
public class FileAttributes {

    private static final @NotNull ConcurrentDispatcher dispatcher = new ConcurrentDispatcher();
    private final @NotNull JFileChooser fc;

    /**
     * Create an object to return attributes for display in the specified file chooser.
     * @param fc The file chooser.
     */
    public FileAttributes(@NotNull JFileChooser fc) {
        this.fc = fc;
    }

    /**
     * Return the label for a file in a file chooser.
     * @param f The file.
     * @return the label.
     */
    public int getLabel(@NotNull File f) {
        return OSXFile.getLabel(f);
    }

    /**
     * Return the file kind for a file in a file chooser.
     * @param f The file.
     * @return the file kind.
     */
    public @NotNull String getKind(@NotNull File f) {
        String fileKind = OSXFile.getKindString(f);
        if (fileKind != null) {
            return fileKind;
        }

        if (f.isDirectory()) {
            String path = f.getPath();
            if (path.endsWith(".app")) {
                return "application";
            } else if (path.endsWith(".wdgt")) {
                return "widget";
            } else {
                return "folder";
            }
        } else {
            return "document";
        }
    }

    /**
     * Return the icon for a file in a file chooser. The icon may be an empty or generic icon
     * that updates itself when a better icon becomes available.
     * @param f The file.
     * @return the icon.
     */
    public @NotNull AquaFileIcon getIcon(@NotNull File f) {
        if (canGetFileIconDirectly()) {
            return AquaFileIcons.getThumbnail(f);
        }
        AquaFileIcon icon = new AquaFileIcon(16, 16);
        FileIconUpdater updater = new FileIconUpdater(fc, f, icon);
        dispatcher.dispatch(updater);
        return icon;
    }

    private static class FileIconUpdater
            implements Runnable
    {
        private final @NotNull JFileChooser fc;
        private final @NotNull File f;
        private final @NotNull AquaFileIcon icon;

        public FileIconUpdater(@NotNull JFileChooser fc, @NotNull File f, @NotNull AquaFileIcon icon) {
            this.fc = fc;
            this.f = f;
            this.icon = icon;
        }

        @Override
        public void run() {
            Icon ic = fc.getIcon(f);
            icon.installIcon(ic);
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
}
