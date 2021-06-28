/*
 * Copyright (c) 2019-2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.awt.*;
import java.io.File;

import org.jetbrains.annotations.NotNull;
import org.violetlib.aqua.AquaImageFactory;
import org.violetlib.aqua.AquaMultiResolutionImage;
import org.violetlib.aqua.Utils;

/**
 * An implementation of the file icon service that uses Launch Services and Quick Look.
 */
public class HybridFileIconServiceImpl
        extends FileIconServiceImplBase
        implements FileIconService {
    private static final @NotNull ConcurrentDispatcher dispatcher = new ConcurrentDispatcher();

    public HybridFileIconServiceImpl() {
        if (debugFlag) {
            Utils.logDebug("File Icon Service: Using Launch Services and Quick Look");
        }
    }

    @Override
    public synchronized @NotNull Request requestIcon(@NotNull File f, int size, float scale, @NotNull Handler handler) {
        RequestImpl request = new RequestImpl(f, handler);

        // First deliver a generic icon, if we can determine that the file is a folder or an ordinary file.
        installGenericFileIcon(f, request);

        if (OSXFile.isAvailable()) {
            dispatcher.dispatch(new Task(f, size, request, false));
            dispatcher.dispatch(new Task(f, size, request, true));
        }

        return request;
    }

    private class Task
            implements Runnable {
        private final @NotNull File file;
        private final int size;
        private final @NotNull RequestImpl request;
        private final boolean useQuickLook;

        public Task(@NotNull File file, int size, @NotNull RequestImpl request, boolean useQuickLook) {
            this.file = file;
            this.size = size;
            this.request = request;
            this.useQuickLook = useQuickLook;
        }

        @Override
        public void run() {
            String path = file.getAbsolutePath();
            int[][] buffers = new int[2][];
            if (!AquaFileIcons.nativeRenderFileImage(path, useQuickLook, true, buffers, size, size)) {
                if (AquaImageFactory.debugNativeRendering) {
                    String type = useQuickLook ? "Quick Look" : "Launch Services";
                    Utils.logDebug("Failed to render " + type + " image for " + path);
                }
            } else {
                if (AquaImageFactory.debugNativeRendering) {
                    String type = useQuickLook ? "Quick Look" : "Launch Services";
                    Utils.logDebug("Rendered " + type + " image for " + path);
                }
                Image image = AquaMultiResolutionImage.createImage(size, size, buffers[0], buffers[1]);
                int priority = useQuickLook ? FileIconService.ICON_GENERIC : FileIconService.ICON_CUSTOM;
                request.installImage(image, priority);
            }
        }
    }
}
