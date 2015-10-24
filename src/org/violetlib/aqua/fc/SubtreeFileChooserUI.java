/*
 * @(#)SubtreeFileChooserUI.java  1.0  2010-08-20
 *
 * Copyright (c) 2010 Werner Randelshofer, Switzerland.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the
 * license agreement you entered into with Werner Randelshofer.
 * For details see accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.io.File;

/**
 * SubtreeFileChooserUI for filechoosers which can change their filesystem root or select a directory within that
 * subtree.
 *
 * @author Werner Randelshofer
 * @version 1.0 2010-08-20 Created.
 */
public interface SubtreeFileChooserUI {
    /** Sets the root directory of the subtree. */
    void setRootDirectory(File file);

    /**
     * Set the current directory. If possible, the subtree root is not changed. Otherwise, the specified directory
     * becomes the new subtree root.
     */
    void selectDirectory(File file);
}
