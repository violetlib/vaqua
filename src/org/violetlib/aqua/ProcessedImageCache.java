/*
 * Copyright (c) 2018-2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.lang.ref.SoftReference;
import java.util.HashMap;
import java.util.Map;
import java.util.WeakHashMap;
import javax.swing.*;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Softly cache processed images and image analysis results to avoid recomputation.
 */

public abstract class ProcessedImageCache {

    private final WeakHashMap<Object,ImageInfo> imageMap = new WeakHashMap<>();

    /**
     * Return the processed version of the specified icon.
     * @param icon The source icon.
     * @param operator The operation to be performed on the icon image, or null to return the actual icon image.
     * @return the processed version of {@code icon}, or null if the icon is not valid.
     */
    public @Nullable Image getProcessedImage(@NotNull Icon icon, @Nullable Object operator) {
        ImageInfo info = getIconImageInfo(icon);
        return info != null ? info.getProcessedImage(operator) : null;
    }

    /**
     * Return the processed version of the specified image.
     * @param image The source image.
     * @param operator The operation to be performed on the image.
     * @return the processed version of {@code image}.
     */
    public @NotNull Image getProcessedImage(@NotNull Image image, @NotNull Object operator) {
        ImageInfo info = getImageInfo(image);
        return info.getProcessedImage(operator);
    }

    /**
     * Determine if the specified image is a template image.
     * @param image The image.
     * @return true if and only if the image is a template image.
     */
    public boolean isTemplateImage(@NotNull Image image) {
        ImageInfo info = getImageInfo(image);
        return info.isTemplate;
    }

    /**
     * Determine if the specified icon is a template image.
     * @param icon The icon.
     * @return true if and only if the icon is a template image.
     */
    public boolean isTemplateIcon(@NotNull Icon icon) {
        ImageInfo info = getIconImageInfo(icon);
        return info != null && info.isTemplate;
    }

    private @Nullable ImageInfo getIconImageInfo(@NotNull Icon icon) {
        ImageInfo info = imageMap.get(icon);
        if (info == null) {
            Image image = AquaIcon.getImageForIcon(icon);
            if (image != null) {
                info = getImageInfo(image);
                imageMap.put(icon, info);
            }
        }
        return info;
    }

    private @NotNull ImageInfo getImageInfo(@NotNull Image image) {
        ImageInfo info = imageMap.get(image);
        if (info == null) {
            boolean isTemplate = determineTemplateImage(image);
            info = new ImageInfo(image, isTemplate);
            imageMap.put(image, info);
        }
        return info;
    }

    private class ImageInfo {
        @NotNull Image source;
        boolean isTemplate;
        @Nullable Map<Object,SoftReference<Image>> processedImageMap;

        private ImageInfo(@NotNull Image source, boolean isTemplate) {
            this.source = source;
            this.isTemplate = isTemplate;
        }

        public @NotNull Image getProcessedImage(@Nullable Object operator) {
            if (processedImageMap == null) {
                processedImageMap = new HashMap<>();
            }

            Image result = null;
            SoftReference<Image> imageRef = processedImageMap.get(operator);
            if (imageRef != null) {
                result = imageRef.get();
            }

            if (result != null) {
                return result;
            }

            if (operator == null) {
                result = source;
            } else if (operator instanceof Color) {
                Color color = (Color) operator;
                if (isTemplate) {
                    result = createImageFromTemplate(source, color);
                } else {
                    result = source;
                }
            } else {
                result = createProcessedImage(source, operator);
            }
            processedImageMap.put(operator, new SoftReference<>(result));
            return result;
        }
    }

    protected abstract boolean determineTemplateImage(@NotNull Image source);
    protected abstract @NotNull Image createImageFromTemplate(@NotNull Image source, @NotNull Color color);
    protected abstract @NotNull Image createProcessedImage(@NotNull Image source, @NotNull Object operator);
}
