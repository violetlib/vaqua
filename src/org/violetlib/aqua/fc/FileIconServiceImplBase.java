/*
 * Copyright (c) 2019 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.awt.*;
import java.io.File;
import javax.swing.*;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.aqua.AquaUtils;

import static org.violetlib.aqua.fc.FileIconService.debugFlag;

/**
 * Common code for implementations of the {@link FileIconService}. This class supports generic icons.
 */
public class FileIconServiceImplBase {

    private @Nullable ImageIcon genericFileIcon;
    private @Nullable ImageIcon genericFolderIcon;

    /**
     * Install a generic icon for a file in the specified request, if possible.
     * The installation must be performed quickly on the current thread.
     */
    protected void installGenericFileIcon(@NotNull File f, @NotNull RequestImpl request) {
        // We can deliver a generic icon if we can determine that the file is a folder or an ordinary file.
        if (f.isFile()) {
            if (genericFileIcon == null) {
                genericFileIcon = OSXFile.getFileIcon();
            }
            request.installIcon(genericFileIcon, FileIconService.ICON_GENERIC);
        } else if (f.isDirectory()) {
            if (genericFolderIcon == null) {
                genericFolderIcon = OSXFile.getDirectoryIcon();
            }
            request.installIcon(genericFolderIcon, FileIconService.ICON_GENERIC);
        }
    }

    /**
     * An implementation of the {@link FileIconService.Request} interface.
     */
    protected class RequestImpl
            implements FileIconService.Request {

        private final @NotNull File f;
        private final int highestAcceptedPriority;
        private @Nullable FileIconService.Handler handler;
        private int highestReceivedPriority = -1;

        public RequestImpl(@NotNull File f, @Nullable FileIconService.Handler handler) {
            this.f = f;
            this.handler = handler;
            this.highestAcceptedPriority = Integer.MAX_VALUE;
        }

        public RequestImpl(@NotNull File f, @Nullable FileIconService.Handler handler, int highestAcceptedPriority) {
            this.f = f;
            this.handler = handler;
            this.highestAcceptedPriority = highestAcceptedPriority;
        }

        public synchronized void installIcon(@NotNull ImageIcon icon, int priority)
        {
            if (priority > highestReceivedPriority && priority <= highestAcceptedPriority && handler != null) {
                debug(icon, priority);
                handler.provideIcon(icon, priority);
                highestReceivedPriority = priority;
            }
        }

        public synchronized void installImage(@NotNull Image image, int priority)
        {
            if (priority > highestReceivedPriority && handler != null) {
                ImageIcon icon = new ImageIcon(image);
                debug(icon, priority);
                handler.provideIcon(icon, priority);
                highestReceivedPriority = priority;
            }
        }

        private void debug(@NotNull Icon icon, int priority) {
            if (debugFlag) {
                AquaUtils.logDebug("Updating icon for " + f + " with priority " + priority
                  + " and size " + icon.getIconWidth() + "x" + icon.getIconHeight());

//                if (f.getName().equals("Users")) {
//                    Image im = AquaIcon.getImageForIcon(icon);
//                    if (im != null) {
//                        BufferedImage b = Images.toBufferedImage(im);
//                        try {
//                            File f = File.createTempFile("icon", ".png");
//                            ImageIO.write(b, "png", f);
//                            AquaUtils.logDebug("  saved to " + f);
//                        } catch (IOException ex) {
//                        }
//                    }
//                    try {
//                        Thread.sleep(5000);
//                    } catch (InterruptedException ex) {
//
//                    }
//                }
            }
        }

        @Override
        public synchronized void cancel() {
            handler = null;
        }
    }
}
