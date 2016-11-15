/*
 * Copyright (c) 2015-2016 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.image.*;
import java.util.function.Function;

/**
 * Abstract base class for multi-resolution images. This class is intended to (eventually) isolate the JDK 1.8/1.9 API
 * differences.
 */
public abstract class AquaMultiResolutionImage extends Image {

    /** A specialized image mapper interface */
    public interface Mapper {
        BufferedImage map(Image source, int scaleFactor);
    }

    /**
     * Create a multi-resolution image from a 1x raster and a 2x raster (created by native code).
     */
    public static Image createImage(int width, int height, int[] data1x, int[] data2x) {
        BufferedImage im1x = JavaSupport.createImage(width, height, data1x);
        if (data2x != null) {
            BufferedImage im2x = JavaSupport.createImage(width * 2, height * 2, data2x);
            return JavaSupport.createMultiResolutionImage(im1x, im2x);
        } else {
            return JavaSupport.createMultiResolutionImage(im1x);
        }
    }

    /**
     * Create an image by applying a generic mapper. Supports multi-resolution images.
     */
    public static Image apply(Image source, Function<Image,Image> mapper) {
        return JavaSupport.applyMapper(source, mapper);
    }

    /**
     * Create an image by applying a specialized mapper. Supports multi-resolution images.
     */
    public static Image apply(Image source, AquaMultiResolutionImage.Mapper mapper) {
        return JavaSupport.applyMapper(source, mapper);
    }

    /**
     * Create an image by applying a filter. Supports multi-resolution images.
     */
    public static Image apply(Image image, ImageFilter filter) {
        return JavaSupport.applyFilter(image, filter);
    }

    protected final Image baseImage;
    protected final int baseImageWidth;
    protected final int baseImageHeight;

    protected AquaMultiResolutionImage(Image baseImage) {
        this.baseImage = baseImage;
        this.baseImageWidth = baseImage.getWidth(null);
        this.baseImageHeight = baseImage.getHeight(null);
    }

    protected AquaMultiResolutionImage(Image baseRepresentation, int width, int height) {
        this.baseImage = baseRepresentation;
        this.baseImageWidth = width;
        this.baseImageHeight = height;
    }

    public abstract AquaMultiResolutionImage map(Function<Image,Image> mapper);

    public abstract AquaMultiResolutionImage map(Mapper mapper);

    @Override
    public int getWidth(ImageObserver observer) {
        return baseImageWidth;
    }

    @Override
    public int getHeight(ImageObserver observer) {
        return baseImageHeight;
    }

    @Override
    public Object getProperty(String name, ImageObserver observer) {
        return baseImage.getProperty(name, observer);
    }

    @Override
    public ImageProducer getSource() {
        return baseImage.getSource();
    }

    @Override
    public Graphics getGraphics() {
        throw new UnsupportedOperationException();
    }
}
