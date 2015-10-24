/*
 * Copyright (c) 2014-2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import javax.swing.*;
import java.io.File;

/**
 * These are the methods supported on sidebar tree nodes that represent files.
 */
public interface SidebarTreeFileNode {
    /**
     * Returns the resolved file object.
     */
    File getResolvedFile();

    /**
     * Returns the user name of the file.
     */
    String getUserName();

    /**
     * Returns the icon of the file.
     * Returns a proxy icon if the real icon has not yet been fetched from the file system.
     */
    Icon getIcon();
}
