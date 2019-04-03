/*
 * Copyright (c) 2019 Alan Snyder.
 * You may not use, copy or modify this file, except in compliance with the accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.awt.*;
import java.io.File;
import javax.swing.*;

import org.jetbrains.annotations.NotNull;
import org.violetlib.aqua.AquaImageFactory;
import org.violetlib.aqua.AquaMultiResolutionImage;

/**
 * Support for file icons used in the file chooser.
 */

public class AquaFileIcons {

    private static final @NotNull ConcurrentDispatcher dispatcher = new ConcurrentDispatcher();

    /**
     * Return the icon for a file. The icon is dynamic. An image is installed into the icon when available.
     * Additional images may be installed if they have higher priority than the current image.
     * @param file The file.
     * @param size The desired icon size.
     * @throws UnsupportedOperationException if an image cannot be obtained.
     */
    public static @NotNull AquaFileIcon getFileIcon(@NotNull File file, int size) {
        AquaFileIcon icon = new AquaFileIcon(size, size);
        if (OSXFile.isAvailable()) {
            // Quick Look icons are better but take longer to compute.
            // For a small icon, it is best to use the Launch Services icon until the Quick Look icon is available.
            // For a preview icon, it is better to wait a few seconds to get the Quick Look icon.

            if (size <= 256) {
                dispatcher.dispatch(new FileIconUpdater(file, icon, false));
                dispatcher.dispatch(new FileIconUpdater(file, icon, true));
            } else {
                dispatcher.dispatch(new FileIconUpdater(file, icon, true));
                delay(10, () -> {
                    if (icon.getCurrentImage() == null) {
                        dispatcher.dispatch(new FileIconUpdater(file, icon, false));
                    }
                });
            }

        } else {
            // TBD: return a default image ?
        }

        return icon;
    }

    private static void delay(int secs, @NotNull Runnable r) {
        Timer t = new Timer(secs * 1000, ev -> r.run());
        t.setRepeats(false);
        t.start();
    }

    /**
     * Obtain the system icon for a file.
     * This object should be invoked on a background thread.
     */
    private static class FileIconUpdater implements Runnable {
        private final @NotNull File file;
        private final @NotNull AquaFileIcon icon;
        private final boolean useQuickLook;

        public FileIconUpdater(@NotNull File file, @NotNull AquaFileIcon icon, boolean useQuickLook) {
            this.file = file;
            this.icon = icon;
            this.useQuickLook = useQuickLook;
        }

        @Override
        public void run() {
            String path = file.getAbsolutePath();
            int width = icon.getIconWidth();
            int height = icon.getIconHeight();
            int[][] buffers = new int[2][];
            if (!nativeRenderFileImage(path, useQuickLook, true, buffers, width, height)) {
                if (AquaImageFactory.debugNativeRendering) {
                    String type = useQuickLook ? "Quick Look" : "Launch Services";
                    System.err.println("Failed to render " + type + " image for " + path);
                }
            } else {
                if (AquaImageFactory.debugNativeRendering) {
                    String type = useQuickLook ? "Quick Look" : "Launch Services";
                    System.err.println("Rendered " + type + " image for " + path);
                }
                Image image = AquaMultiResolutionImage.createImage(width, height, buffers[0], buffers[1]);
                int priority = useQuickLook ? AquaFileIcon.ICON_QUICK_LOOK : AquaFileIcon.ICON_LAUNCH_SERVICES;
                icon.installImage(image, priority);
            }
        }
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
    private static native boolean nativeRenderFileImage(@NotNull String path, boolean isQuickLook,
                                                        boolean useIconMode, int[][] buffers, int w, int h);
}
