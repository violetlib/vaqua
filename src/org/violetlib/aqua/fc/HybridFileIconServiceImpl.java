package org.violetlib.aqua.fc;

import java.awt.*;
import java.io.File;

import javax.swing.*;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.aqua.AquaImageFactory;
import org.violetlib.aqua.AquaMultiResolutionImage;

/**
 * An implementation of the file icon service that uses Launch Services and Quick Look.
 */

public class HybridFileIconServiceImpl
  implements FileIconService
{
    private static final @NotNull ConcurrentDispatcher dispatcher = new ConcurrentDispatcher();

    private @Nullable ImageIcon genericFileIcon;
    private @Nullable ImageIcon genericFolderIcon;

    @Override
    public synchronized @NotNull Request requestIcon(@NotNull File f, int size, float scale, @NotNull Handler handler) {
        RequestImpl request = new RequestImpl(handler);

        // First deliver a generic icon, if we can determine that the file is a folder or not.
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

        if (OSXFile.isAvailable()) {
            dispatcher.dispatch(new Task(f, size, request, false));
            dispatcher.dispatch(new Task(f, size, request, true));
        }

        return request;
    }

    private class RequestImpl
      implements FileIconService.Request
    {
        private @Nullable FileIconService.Handler handler;
        private int highestReceivedPriority = -1;

        public RequestImpl(@Nullable Handler handler) {
            this.handler = handler;
        }

        public synchronized void installIcon(@NotNull ImageIcon icon, int priority)
        {
            if (priority > highestReceivedPriority && handler != null) {
                handler.provideIcon(icon, priority);
                highestReceivedPriority = priority;
            }
        }

        public synchronized void installImage(@NotNull Image image, int priority)
        {
            if (priority > highestReceivedPriority && handler != null) {
                ImageIcon icon = new ImageIcon(image);
                handler.provideIcon(icon, priority);
                highestReceivedPriority = priority;
            }
        }

        @Override
        public synchronized void cancel() {
            handler = null;
        }
    }

    private class Task
      implements Runnable
    {
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
                    System.err.println("Failed to render " + type + " image for " + path);
                }
            } else {
                if (AquaImageFactory.debugNativeRendering) {
                    String type = useQuickLook ? "Quick Look" : "Launch Services";
                    System.err.println("Rendered " + type + " image for " + path);
                }
                Image image = AquaMultiResolutionImage.createImage(size, size, buffers[0], buffers[1]);
                int priority = useQuickLook ? FileIconService.ICON_GENERIC : FileIconService.ICON_CUSTOM;
                request.installImage(image, priority);
            }
        }
    }
}
