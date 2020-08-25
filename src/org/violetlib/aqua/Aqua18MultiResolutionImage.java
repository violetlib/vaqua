/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import sun.awt.image.MultiResolutionCachedImage;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.awt.image.FilteredImageSource;
import java.awt.image.ImageFilter;
import java.awt.image.ImageProducer;
import java.util.ArrayList;
import java.util.List;

/**
 * A multi-resolution image with a single representation. This class is designed for JDK 1.8.
 */
public class Aqua18MultiResolutionImage extends AquaMultiResolutionImage implements sun.awt.image.MultiResolutionImage {
    public Aqua18MultiResolutionImage(BufferedImage im) {
        super(im);
    }

    public Aqua18MultiResolutionImage(int width, int height, BufferedImage im) {
        super(im, width, height);
    }

    @Override
    public Image getResolutionVariant(int width, int height) {
        return baseImage;
    }

    @Override
    public List<Image> getResolutionVariants() {
        java.util.List<Image> result = new ArrayList<>();
        result.add(baseImage);
        return result;
    }

    @Override
    public AquaMultiResolutionImage map(Mapper mapper) {
        return new Aqua18MultiResolutionImage((BufferedImage) mapper.map(baseImage, 1));
    }

    /**
     * Create an image by applying a filter. Supports some multi-resolution images.
     */
    public static Image apply(Image image, ImageFilter filter) {
        return (image instanceof MultiResolutionCachedImage)
          ? ((MultiResolutionCachedImage) image).map(rv -> createFilteredImage(rv, filter))
          : createFilteredImage(image, filter);
    }

    private static Image createFilteredImage(Image image, ImageFilter filter) {
        final ImageProducer prod = new FilteredImageSource(image.getSource(), filter);
        return Toolkit.getDefaultToolkit().createImage(prod);
    }

    /**
     * Create an image by applying a mapper. Supports some multi-resolution images.
     */
    public static Image apply(Image source, AquaMultiResolutionImage.Mapper mapper) {
        if (source instanceof MultiResolutionCachedImage) {
            MultiResolutionCachedImage s = (MultiResolutionCachedImage) source;
            int width = s.getWidth(null);
            return s.map(rv -> mapper.map(rv, rv.getWidth(null) / width));
        }

        if (source instanceof AquaMultiResolutionImage) {
            AquaMultiResolutionImage s = (AquaMultiResolutionImage) source;
            return s.map(mapper);
        }

        return mapper.map(source, 1);
    }
}
