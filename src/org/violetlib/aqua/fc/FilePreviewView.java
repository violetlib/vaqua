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

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.aqua.NativeOverlayView;

/**
 * Provides access to a native quick look preview view.
 */

public class FilePreviewView {

    private long vptr;
    private @Nullable NativeOverlayView overlayView;

    public FilePreviewView()
            throws UnsupportedOperationException {
        vptr = nativeCreatePreviewView();
        if (vptr == 0) {
            throw new UnsupportedOperationException("Unable to create file preview view");
        }
    }

    public void track(@NotNull Component trackingComponent) {
        if (vptr != 0) {
            if (overlayView != null) {
                overlayView.dispose();
                overlayView = null;
            }
            overlayView = new NativeOverlayView(trackingComponent, vptr);
        }
    }

    public void clear() {
        if (vptr != 0) {
            nativeShowPreview(vptr, null);
        }
    }

    public void show(@NotNull File f) {
        if (vptr != 0) {
            nativeShowPreview(vptr, f.getAbsolutePath());
        }
    }

    public void dispose() {
        if (vptr != 0) {
            if (overlayView != null) {
                overlayView.dispose();
                overlayView = null;
            }
            nativeDisposePreviewView(vptr);
            vptr = 0;
        }
    }

    private static native long nativeCreatePreviewView();
    private static native void nativeShowPreview(long vptr, @Nullable String path);
    private static native void nativeDisposePreviewView(long vptr);
}
