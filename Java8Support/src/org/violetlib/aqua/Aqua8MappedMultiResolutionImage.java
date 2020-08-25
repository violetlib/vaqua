/*
 * Copyright (c) 2016 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.image.ImageObserver;
import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

import sun.awt.image.MultiResolutionCachedImage;
import sun.awt.image.MultiResolutionImage;

/**
 * This class supports mapping of arbitrary multi-resolution images in Java 8. Note that caching is a necessity in the
 * current AWT design. If a new deferred image is returned each time a resolution variant is asked for by the graphics
 * context, the result is endless repainting that never succeeds. With caching, the image contents will eventually be
 * ready.
 */
public class Aqua8MappedMultiResolutionImage extends MultiResolutionCachedImage {

    // Implementation note: we are using MultiResolutionCachedImage to take advantage of its caching code.
    // Arguably a crock.

    private final Image baseImage;
    private final Image mappedBaseImage;
    private final Function<Image,Image> mapper;

    public Aqua8MappedMultiResolutionImage(MultiResolutionImage source, Function<Image,Image> mapper) {
        super(getImageWidth(source), getImageHeight(source), new MyVariantMapper(source, mapper));

        this.baseImage = (Image) source;
        this.mappedBaseImage = mapper.apply(baseImage);
        this.mapper = mapper;
    }

    private static int getImageWidth(MultiResolutionImage source) {
        int width = ((Image) source).getWidth(null);
        if (width < 0) {
            throw new IllegalStateException("Multiresolution image has unknown width");
        }
        return width;
    }

    private static int getImageHeight(MultiResolutionImage source) {
        int height = ((Image) source).getHeight(null);
        if (height < 0) {
            throw new IllegalStateException("Multiresolution image has unknown height");
        }
        return height;
    }

    private static class MyVariantMapper implements BiFunction<Integer,Integer,Image> {
        private final MultiResolutionImage source;
        private final Function<Image,Image> mapper;

        public MyVariantMapper(MultiResolutionImage source, Function<Image,Image> mapper) {
            this.source = source;
            this.mapper = mapper;
        }

        @Override
        public Image apply(Integer width, Integer height) {
            Image sourceVariant = source.getResolutionVariant(width, height);
            return mapper.apply(sourceVariant);
        }
    }

    /**
     * This method is redefined because the superclass method depends upon knowing the variant sizes, which we do not
     * know.
     */
    @Override
    public List<Image> getResolutionVariants() {
        List<Image> result = new ArrayList<>();
        MultiResolutionImage source = (MultiResolutionImage) baseImage;
        for (Image sourceVariant : source.getResolutionVariants()) {
            Image mappedVariant = mapper.apply(sourceVariant);
            result.add(mappedVariant);
        }
        return result;
    }

    /**
     * This method is redefined because the superclass method depends upon knowing the variant sizes, which we do not
     * know.
     */
    @Override
    public Aqua8MappedMultiResolutionImage map(Function<Image, Image> mapper) {
        return new Aqua8MappedMultiResolutionImage(this, mapper);
    }

    /**
     * This method is redefined because the superclass method depends upon knowing the resolution to ask for to get the
     * base image, which we do not know.
     */
    @Override
    protected Image getBaseImage() {
        return mappedBaseImage;
    }

    /**
     * This method is redefined because the superclass method would use the mapped base image, which does not know the
     * virtual dimensions of the source image.
     */
    @Override
    public int getWidth(ImageObserver imageObserver) {
        return baseImage.getWidth(imageObserver);
    }

    /**
     * This method is redefined because the superclass method would use the mapped base image, which does not know the
     * virtual dimensions of the source image.
     */
    @Override
    public int getHeight(ImageObserver imageObserver) {
        return baseImage.getHeight(imageObserver);
    }
}
