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

import org.jetbrains.annotations.NotNull;

/**
 * Platform support for obtaining file icons for use in the file chooser.
 */

public interface FileIconService {

    // Icon quality levels

    int ICON_GENERIC = 0;      // a generic icon for any file
    int ICON_TYPE = 10;        // an icon for a specific file type
    int ICON_CUSTOM_LOW = 20;  // a low-quality icon that may be custom for a specific file
    int ICON_CUSTOM = 30;      // the best quality icon for a specific file

    boolean debugFlag = false;

    /**
     * Request an icon for a file. Icons are delivered to the specified handler on an unspecified thread.
     * This method may return before any icons are delivered. Icons may also be delivered on this thread before this
     * method returns.
     * Multiple icons may be delivered, in order of increasing quality and specificity.
     * No guarantee is made on the number of icons that will be delivered. In an unlikely failure situation, the number
     * may be zero.
     *
     * @param f The file whose icon is requested. The file need not exist, if a generic icon can be delivered or the
     *          file type can be deduced from the file name.
     * @param size The requested icon size (in points). The delivered icons may be larger or smaller than this size.
     * @param scale The display scale of the display where the icon will be rendered.
     * @param handler The handler that will receive the icons.
     * @return an object that can be used to cancel the request.
     */
    @NotNull Request requestIcon(@NotNull File f, int size, float scale, @NotNull Handler handler);

    interface Handler {
        /**
         * Deliver a requested icon.
         * @param icon The icon.
         * @param quality The quality level of the icon.
         */
        void provideIcon(@NotNull ImageIcon icon, int quality);
    }

    interface Request {
        /**
         * Cancel the request, if possible.
         */
        void cancel();
    }
}
