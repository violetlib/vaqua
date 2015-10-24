/*
 * @(#)FileInfo.java
 *
 * Copyright (c) 2005-2013 Werner Randelshofer, Switzerland.
 * You may not use, copy or modify this file, except in compliance with the
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import javax.swing.*;
import java.io.File;

/**
 * Provides information about a File object. FileInfo uses a worker thread for
 * validating the information that it provides. The quality of the information
 * returned increases over time.
 *
 * @author Werner Randelshofer
 * @version $Id$
 */
public interface FileInfo {
    /**
     * Returns the unresolved file object.
     */
    File getFile();
    /**
     * Returns the resolved file object.
     */
    File getResolvedFile();
    /**
     * Lazyily returns the resolved file object.
     * Returns null, if the file object has not been resolved yet.
     */
    File lazyGetResolvedFile();

    /**
     * Returns true, if the file object is traversable.
     */
    boolean isTraversable();
    /**
     * Returns true, if the file object is hidden.
     */
    boolean isHidden();
    /**
     * Returns true, if the file object is acceptable, i.e. selectable in
     * the JFileChooser.
     */
    boolean isAcceptable();

    /**
     * Returns the (color) label of the file.
     * Returns -1 if the label has not (yet) been determined.
     */
    int getFileLabel();

    /**
     * Returns the user name of the file.
     */
    String getUserName();

    /**
     * Returns the icon of the file.
     * Returns a proxy icon if the real icon has not yet been fetched from the
     * file system.
     */
    Icon getIcon();

    /**
     * Returns the length of the file.
     * Returns -1 if the length has not (yet) been determined.
     */
    long getFileLength();

    /**
     * Return true if the file is an alias or symbolic link.
     */
    boolean isAlias();

    /**
     * Returns the kind of the file.
     * Returns null if the kind has not (yet) been determined.
     */
    String getFileKind();

    /**
     * Returns true if a worker thread is validating the information provided
     * by this file info object.
     */
    boolean isValidating();
}
