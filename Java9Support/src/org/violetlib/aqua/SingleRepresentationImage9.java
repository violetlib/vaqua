/*
 * Copyright (c) 2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.image.*;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import org.jetbrains.annotations.NotNull;

/**
 * A multi-resolution image with a single representation. This class is designed for Java 9.
 */

public class SingleRepresentationImage9 extends Image implements MultiResolutionImage {

    protected final Image image;
    protected final int imageWidth;
    protected final int imageHeight;

    public SingleRepresentationImage9(@NotNull BufferedImage im, int width, int height) {
        this.image = im;
        this.imageWidth = width;
        this.imageHeight = height;
    }

    @Override
    public int getWidth(ImageObserver observer) {
        return imageWidth;
    }

    @Override
    public int getHeight(ImageObserver observer) {
        return imageHeight;
    }

    @Override
    public Object getProperty(String name, ImageObserver observer) {
        return image.getProperty(name, observer);
    }

    @Override
    public ImageProducer getSource() {
        return image.getSource();
    }

    @Override
    public Graphics getGraphics() {
        throw new UnsupportedOperationException();
    }

    @Override
    public Image getResolutionVariant(double width, double height) {
        return image;
    }

    @Override
    public List<Image> getResolutionVariants() {
        List<Image> result = new ArrayList<>();
        result.add(image);
        return result;
    }

    // The following is a workaround for a JDK 8 problem - trying to draw a MultiResolutionImage that is not ready
    // throws an exception.

    private static @NotNull Image waitForImage(@NotNull Image image, int width, int height) {
        final boolean[] mutex = new boolean[] { false };
        ImageObserver observer = (Image img, int infoflags, int x, int y, int w, int h) -> {
            int required = ImageObserver.ALLBITS;
            if ((infoflags & required) == required || (infoflags & (ImageObserver.ERROR | ImageObserver.ABORT | ImageObserver.FRAMEBITS)) != 0) {
                synchronized (mutex) {
                    mutex[0] = true;
                    mutex.notify();
                }
                return false;
            } else {
                return true;
            }
        };
        if (!Toolkit.getDefaultToolkit().prepareImage(image, width, height, observer)) {
            synchronized (mutex) {
                while (!mutex[0]) {
                    try {
                        mutex.wait();
                    } catch (InterruptedException e) {
                        break;
                    }
                }
            }
        }
        return image;
    }
}
