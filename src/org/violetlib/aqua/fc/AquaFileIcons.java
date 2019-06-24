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
 * Support for file icons used in the file chooser.
 */

public class AquaFileIcons {

    private static final @NotNull FileIconService fis = new HybridFileIconServiceImpl();

    /**
     * Return a thumbnail icon for a file. The icon is dynamic. An image is installed into the icon when available.
     * Additional images may be installed if they have higher priority than the current image.
     */
    public static @NotNull AquaFileIcon getThumbnail(@NotNull File f)
    {
        AquaFileIcon result = new AquaFileIcon(16, 16);
        FileIconService.Request request = fis.requestIcon(f, 16, 2, (icon, quality) -> result.installIcon(icon));
        return result;
    }

    public static @NotNull AquaFileIcon getPreview(@NotNull File f)
    {
        AquaFileIcon result = new AquaFileIcon(1600, 1600);
        FileIconService.Request request = fis.requestIcon(f, 1600, 2, (icon, quality) -> result.installIcon(icon));
        return result;
    }

    /**
     * Obtain the icon or QuickLook image for a file.
     *
     * @param path the path to the file.
     * @param isQuickLook True to get the QuickLook image, false to get the file icon.
     * @param useIconMode True to use Icon mode when using Quick Look, false otherwise.
     * @param buffers 1x and 2x rasters stored are here (2x is optional)
     * @param w The width of the image.
     * @param h The height of the image.
     * @return true if successful, false otherwise.
     */
    public static native boolean nativeRenderFileImage(@NotNull String path, boolean isQuickLook,
                                                       boolean useIconMode, int[][] buffers, int w, int h);
}
