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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.aqua.AquaImageFactory;
import org.violetlib.aqua.AquaMultiResolutionImage;
import org.violetlib.aqua.AquaUtils;
import org.violetlib.aqua.JavaSupport;

/**
 * An implementation of the file icon service that uses the Quick Look Thumbnailing framework, introduced in macOS
 * 10.15 (Catalina).
 */
public class CatalinaFileIconServiceImpl
        extends FileIconServiceImplBase
        implements FileIconService {

    private static final @NotNull ConcurrentDispatcher dispatcher = new ConcurrentDispatcher();

    public CatalinaFileIconServiceImpl() {
        if (debugFlag) {
            AquaUtils.logDebug("File Icon Service: Using Quick Look Thumbnailing");
        }
    }

    @Override
    public @NotNull Request requestIcon(@NotNull File f, int size, float scale, @NotNull Handler handler) {

        // Finder specific issues:
        // Custom icons are used for specific folders and for images. Other files use generic, type-specific icons.
        // Except for image files, the Finder apparently uses Launch Services to get the icon.
        // Launch Services returns type-specific icons for files that are clipped to a document shape and have the
        // file type name superimposed.
        // The icon for an image file is given a border, installed by Finder.
        // The border is 2 points wide. The image is painted over a white background.

        // TBD: the border is not implemented

        RequestImpl request = new RequestImpl(f, handler, FileIconService.ICON_CUSTOM_LOW);

        // Deliver a generic icon, if we can determine that the file is a folder or an ordinary file.
        // This icon is a backup in case the other attempts fail.
        installGenericFileIcon(f, request);

        if (OSXFile.isImageFile(f)) {
            long upcallID = upcallRegistry.registerRequest(request);
            if (debugFlag) {
                AquaUtils.logDebug("Thumbnail request #" + upcallID + ": " + f.getAbsolutePath());
            }
            installQuickLookFileIcon(f, size, scale, upcallID);
        } else {
            dispatcher.dispatch(() -> {
                installLaunchServicesFileIcon(f, size, scale, request, FileIconService.ICON_TYPE);
            });
        }

        return request;
    }

    private void installLaunchServicesFileIcon(@NotNull File f, int size, float scale,
                                               @NotNull RequestImpl request, int priority)
    {
        String path = f.getAbsolutePath();
        int[][] buffers = new int[2][];
        if (!AquaFileIcons.nativeRenderFileImage(path, false, true, buffers, size, size)) {
            if (AquaImageFactory.debugNativeRendering) {
                String type = "Launch Services";
                AquaUtils.logDebug("Failed to render " + type + " image for " + path);
            }
        } else {
            if (AquaImageFactory.debugNativeRendering) {
                String type = "Launch Services";
                AquaUtils.logDebug("Rendered " + type + " image for " + path);
            }
            Image image = AquaMultiResolutionImage.createImage(size, size, buffers[0], buffers[1]);
            request.installImage(image, priority);
        }
    }

    private void installQuickLookFileIcon(@NotNull File f, int size, float scale, long upcallID)
    {
        // Workaround: macOS 10.15 delivers an incorrect generic icon for a symlink.
        // This workaround does not produce a correct generic icon for a broken symlink.
        if (OSXFile.getFileType(f) == OSXFile.FILE_TYPE_ALIAS) {
            File rf = OSXFile.resolve(f);
            if (rf != null) {
                f = rf;
            }
        }

        nativeInstallThumbnails(f.getAbsolutePath(), size, scale, upcallID);
    }

    public static class MyHandler {
        // called from native code
        public void installImage(long upcallID, int width, int height, @NotNull int[] data, float scale, int priority) {
            RequestImpl request = upcallRegistry.getRequest(upcallID);
            if (request != null) {
                if (debugFlag) {
                    AquaUtils.logDebug("Received image " + priority + " for request #" + upcallID
                            + width + "x" + height + " " + data.length + " " + scale);
                }
                AquaMultiResolutionImage image = JavaSupport.createImage(width, height, data, scale);
                request.installImage(image, priority);
            } else {
                AquaUtils.logDebug("Image delivered to obsolete request #" + upcallID);
            }
        }
    }

    private static final @NotNull MyHandler myHandlerInstance = new MyHandler();

    static {
        nativeInstallThumbnailHandler(myHandlerInstance);
    }

    private static final @NotNull ScheduledExecutorService scheduler = new ScheduledThreadPoolExecutor(0);

    private static class UpcallRegistry {

        private final int delayMinutes = 5;
        private final @NotNull Map<Long,RequestImpl> map = new HashMap<>();
        private long nextID = 0;
        private @Nullable List<Long> requestsToRemove;

        public synchronized long registerRequest(@NotNull RequestImpl request) {
            long id = nextID++;
            map.put(id, request);

            if (requestsToRemove == null) {
                requestsToRemove = new ArrayList<>(map.keySet());
                scheduler.schedule(this::cleanup, delayMinutes, TimeUnit.MINUTES);
            }

            return id;
        }

        public synchronized @Nullable RequestImpl getRequest(long id) {
            return map.get(id);
        }

        private synchronized void cleanup() {
            assert requestsToRemove != null;

            if (debugFlag) {
                AquaUtils.logDebug("Removing " + requestsToRemove.size() + " thumbnail requests");
            }

            for (Long id : requestsToRemove) {
                map.remove(id);
            }
            if (!map.isEmpty()) {
                requestsToRemove = new ArrayList<>(map.keySet());
                scheduler.schedule(this::cleanup, delayMinutes, TimeUnit.MINUTES);
            } else {
                requestsToRemove = null;
            }
        }
    }

    private static @NotNull UpcallRegistry upcallRegistry = new UpcallRegistry();

    public static native boolean isAvailable();
    private static native void nativeInstallThumbnailHandler(@NotNull MyHandler handler);
    private static native void nativeInstallThumbnails(@NotNull String filePath, int size, float scale, long upcallID);
}
