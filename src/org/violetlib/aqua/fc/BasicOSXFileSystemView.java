/*
 * @(#)BasicOSXFileSystemView.java
 *
 * Copyright (c) 2009-2010 Werner Randelshofer, Switzerland.
 * Copyright (c) 2014-2018 Alan Snyder
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the
 * license agreement you entered into with Werner Randelshofer.
 * For details see accompanying license terms.
 */

package org.violetlib.aqua.fc;

import javax.swing.*;
import java.awt.*;
import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.List;

import org.jetbrains.annotations.NotNull;

/**
 * BasicOSXFileSystemView.
 *
 * A common base class for OS X file system views. The key feature of an OS X file system view is that the Unix root
 * directory is mapped to the system volume. Also, there is no OS X file that corresponds exactly to the Computer node
 * in the file chooser tree model, but the Volumes folder is the closest, so we use that.
 *
 * This class also supports custom lists of hidden files.
 */
public abstract class BasicOSXFileSystemView extends AquaFileSystemView {

    protected final File volumesFolder = new File("/Volumes");
    protected final File networkFolder = new File("/Network");
    protected final File unixRoot = new File("/");
    private final File computer = volumesFolder;
    protected File systemVolume;

    /**
     * This is a list of file names that are treated as invisible by the AWT
     * FileDialog when they are at the top directory level of a volume.
     * The file names are wrongly treated as visible by
     * Apple's implementation FileSystemView, so we use this HashSet here, to
     * hide them 'manually'.
     */
    protected final static Set<String> hiddenTopLevelNames = new HashSet<>();
    protected final static Set<String> hiddenDirectoryNames = new HashSet<>();
    protected final static Set<File> hiddenFiles = new HashSet<>();

    protected BasicOSXFileSystemView() {
    }

    public final File getSystemVolume() {
        if (systemVolume == null) {
            systemVolume = determineSystemVolume();
        }
        return systemVolume;
    }

    protected File determineSystemVolume() {
        File[] volumes = volumesFolder.listFiles();
        File sys = null;
        if (volumes != null) {
            for (File volume : volumes) {
                try {
                    if (volume.getCanonicalFile().equals(unixRoot)) {
                        sys = volume;
                        break;
                    }
                } catch (IOException e) {
                    // We get here because we can't determine the canonical path for the volume. We suppress this
                    // exception, in the hope that it did not happen for the system volume. If it happened for the
                    // system volume, there is fallback code in method getSystemVolume() that handles this problem.

                    // System.err.println(
                    //   "Unable to canonicalize volume "+volumes[i]
                    // );
                    // e.printStackTrace();
                } catch (SecurityException e) {
                    // We get here because we are not allowed to read the file. We suppress this exception, in the hope
                    // that it did not happen for the system volume. If it happened for the system volume, there is
                    // fallback code in method getSystemVolume() that handles this problem.
                }
            }
        }
        // If we couldn't determine the system volume, we use the root folder instead.
        return (sys == null) ? unixRoot : sys;
    }

    @Override
    public File getComputer() {
        return computer;
    }

    /**
     * Convert a file (path) to canonical form. A reference to the Unix root directory is mapped to the system volume.
     */
    @Override
    public File canonicalize(File f) {
        if (f.equals(unixRoot)) {
            return getSystemVolume();
        }
        return f;
    }

    /**
     * Returns the parent directory of dir.
     * This method returns the system volume instead of the root folder ("/").
     */
    @Override
    public File getParentDirectory(File dir) {
        File parent = (isRoot(dir)) ? null : super.getParentDirectory(dir);
        if (parent != null && parent.equals(unixRoot)) {
            parent = getSystemVolume();
        }
        return parent;
    }

    /**
     * Returns all root partitions on this system.
     * This method returns the contents of the volumes folder.
     * The root folder ("/") is considered as hidden and not
     * returned by this method.
     */
    protected List<File> getRootList() {
        List<File> result = new ArrayList<File>();
        File[] fs = volumesFolder.listFiles();
        if (fs != null) {
            result.addAll(Arrays.asList(fs));
        }

        /*
          Even if /Network is hidden when viewing /, it may be visible in the sidebar or under the Computer node.
        */

        result.add(networkFolder);
        return result;
    }

    /**
     * Returns all root partitions on this system.
     * This method returns the contents of the volumes folder.
     * The root folder ("/") is considered as hidden and not
     * returned by this method.
     */
    @Override
    public final File[] getRoots() {
        List<File> roots = getRootList();
        return roots.toArray(new File[roots.size()]);
    }

    @Override
    public boolean isHiddenFile(@NotNull File f) {
        if (OSXFile.isInvisible(f)) {
            return true;
        } else {
            String name = f.getName();
            if (name.length() == 0) {
                return false;
            } else if (name.charAt(name.length() - 1) == (char) 0x0d) {
                // File names ending with 0x0d are considered as hidden
                return true;
            } else if (name.charAt(0) == '.') {
                // File names starting with '.' are considered as hidden
                return true;
            } else if (hiddenTopLevelNames.contains(name) && (f.getParent() == null || isRoot(f.getParentFile()))) {
                return true;
            } else if (hiddenDirectoryNames.contains(name)) {
                return true;
            } else if (hiddenFiles.contains(f)) {
                return true;
            } else {
                return false;
            }
        }
    }

    @Override
    public boolean isRoot(File aFile) {
        return aFile.equals(unixRoot) || aFile.equals(networkFolder) || aFile.getParentFile() != null && aFile.getParentFile().equals(volumesFolder);
    }

    @Override
    public boolean isParent(File folder, File file) {
        if (folder == null || file == null) {
            return false;
        } else {
            return folder.equals(file.getParentFile());
        }
    }

    @Override
    public File getChild(File parent, String fileName) {
        return new File(parent, fileName);
    }

    @Override
    public boolean isFileSystemRoot(File dir) {
        File parentFile = dir.getParentFile();
        return parentFile == null || parentFile.equals(volumesFolder);
    }

    @Override
    public File getHomeDirectory() {
        return createFileObject(System.getProperty("user.home"));
    }

    @Override
    public File getDefaultDirectory() {
        return getHomeDirectory();
    }

    @Override
    public Icon getSystemIcon(File f) {
        if (f.equals(networkFolder)) {
            Icon icon = UIManager.getIcon("FileView.networkIcon");
            if (icon != null) {
                return icon;
            }
        }

        if (f.equals(getComputer())) {
            Icon icon = UIManager.getIcon("FileView.computerIcon");
            if (icon != null) {
                return icon;
            }
        }

        if (OSXFile.isAvailable()) {
            try {
                Image im = OSXFile.getIconImage(f, 16, false);
                return new ImageIcon(im);
            } catch (UnsupportedOperationException ex) {
            }
        }

        return super.getSystemIcon(f);
    }

    @Override
    public String getSystemTypeDescription(File f) {
        if (OSXFile.isAvailable()) {
            return OSXFile.getKindString(f);
        } else {
        return super.getSystemTypeDescription(f);
        }
    }

    @Override
    public Boolean isTraversable(File f) {
        if (OSXFile.isAvailable()) {
            return OSXFile.isTraversable(f);
        } else {
            return super.isTraversable(f);
        }
    }

    @Override
    public @NotNull Boolean isTraversable(File f, boolean isPackageTraversable, boolean isApplicationTraversable) {
        if (OSXFile.isAvailable()) {
            return OSXFile.isTraversable(f, isPackageTraversable, isApplicationTraversable);
        } else {
            return super.isTraversable(f);
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
                return super.getSystemDisplayName(f);
            }
        }
    }
}
