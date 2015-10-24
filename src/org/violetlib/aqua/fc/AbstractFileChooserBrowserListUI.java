/*
 * Copyright (c) 2014-2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

/**
 * An abstract base class for a file chooser browser list UI.
 */
public interface AbstractFileChooserBrowserListUI {
    void setFileSelectionHandler(AquaFileChooserListMouseBehavior.FileSelectionHandler h);
}
