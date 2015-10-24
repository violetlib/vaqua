/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.image.*;

/**
 * Abstract base class for multi-resolution images. This class is intended to isolete the JDK 1.8/1.9 API differences.
 */
public abstract class AquaMultiResolutionImage extends Image {

    public interface Mapper {
        Image map(Image source, int scaleFactor);
    }

    /**
     * Create a multi-resolution image from a 2x image.
     */
    public static Image createImage2x(BufferedImage source) {
        int width = source.getWidth() / 2;
        int height = source.getHeight() / 2;
        return new Aqua18MultiResolutionImage(width, height, source);
    }

    /**
     * Create a multi-resolution image from a 1x raster and a 2x raster (created by native code).
     */
    public static Image createImage(int width, int height, int[] data1x, int[] data2x) {
        BufferedImage im1x = createImage(width, height, data1x);
        if (data2x != null) {
            BufferedImage im2x = createImage(width * 2, height * 2, data2x);
            return new Aqua18MultiResolutionImage2(im1x, im2x);
        } else {
            return new Aqua18MultiResolutionImage(im1x);
        }
    }

    /**
     * Create a buffered image from a raster (created by native code).
     */
    private static BufferedImage createImage(int width, int height, int[] data) {
        BufferedImage b = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB_PRE);
        final WritableRaster raster = b.getRaster();
        final DataBufferInt buffer = (DataBufferInt) raster.getDataBuffer();
        int[] rasterdata = sun.awt.image.SunWritableRaster.stealData(buffer, 0);
        System.arraycopy(data, 0, rasterdata, 0, width * height);
        sun.awt.image.SunWritableRaster.markDirty(buffer);
        return b;
    }

    /**
     * Create an image by applying a mapper. Supports some multi-resolution images.
     */
    public static Image apply(Image source, AquaMultiResolutionImage.Mapper mapper) {
        return Aqua18MultiResolutionImage.apply(source, mapper);
    }

    /**
     * Create an image by applying a filter. Supports some multi-resolution images.
     */
    public static Image apply(Image image, ImageFilter filter) {
        return Aqua18MultiResolutionImage.apply(image, filter);
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
