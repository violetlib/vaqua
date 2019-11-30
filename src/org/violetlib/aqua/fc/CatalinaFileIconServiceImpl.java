/*
 * Copyright (c) 2019 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

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

    public CatalinaFileIconServiceImpl() {
        if (debugFlag) {
            AquaUtils.logDebug("File Icon Service: Using Quick Look Thumbnailing");
        }
    }

    @Override
    public @NotNull Request requestIcon(@NotNull File f, int size, float scale, @NotNull Handler handler) {
        RequestImpl request = new RequestImpl(f, handler);

        // First deliver a generic icon, if we can determine that the file is a folder or an ordinary file.
        installGenericFileIcon(f, request);

        long upcallID = upcallRegistry.registerRequest(request);
        if (debugFlag) {
            AquaUtils.logDebug("Thumbnail request #" + upcallID + ": " + f.getAbsolutePath());
        }

        nativeInstallThumbnails(f.getAbsolutePath(), size, scale, upcallID);
        return request;
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
