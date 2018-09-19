/*
 * Copyright (c) 2016 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

/*
 * Copyright (c) 2014, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.awt.image.MultiResolutionImage;
import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * This class supports mapping of arbitrary multi-resolution images in Java 9. Note that caching is a necessity in the
 * current AWT design. If a new deferred image is returned each time a resolution variant is asked for by the graphics
 * context, the result is endless repainting that never succeeds. With caching, the image contents will eventually be
 * ready. Based in part on MultiResolutionCachedImage.
 */
public class Aqua9MappedMultiResolutionImage extends Image implements MultiResolutionImage {

    private final Image baseImage;
    private final MyVariantMapper mapper;
    private int availableInfo;

    public Aqua9MappedMultiResolutionImage(MultiResolutionImage source, Function<Image,Image> mapper) {
        this.baseImage = (Image) source;
        this.mapper = new MyVariantMapper(source, mapper);
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

    @Override
    public List<Image> getResolutionVariants() {
        List<Image> result = new ArrayList<>();
        MultiResolutionImage source = (MultiResolutionImage) baseImage;
        for (Image sourceVariant : source.getResolutionVariants()) {
            int width = sourceVariant.getWidth(null);
            int height = sourceVariant.getHeight(null);
            if (width > 0 && height > 0) {
                Image mappedVariant = mapper.apply(width, height);
                result.add(mappedVariant);
            }
        }
        return result;
    }

    @Override
    public ImageProducer getSource() {
        return baseImage.getSource();
    }

    @Override
    public Graphics getGraphics() {
        throw new UnsupportedOperationException("getGraphics() not supported on multiresolution images");
    }

    @Override
    public Image getResolutionVariant(double destWidth, double destHeight) {
        int width = (int) Math.ceil(destWidth);
        int height = (int) Math.ceil(destHeight);
        if (width <= 0 || height <= 0) {
            throw new IllegalArgumentException("Invalid image size: " + width + "x" + height);
        }

        VImageCache cache = VImageCache.getInstance();
        ImageCacheKey key = new ImageCacheKey(this, width, height);
        Image resolutionVariant = cache.getImage(key);
        if (resolutionVariant == null) {
            resolutionVariant = mapper.apply(width, height);
            cache.setImage(key, resolutionVariant);
        }
        JavaSupport.preload(resolutionVariant, availableInfo);
        return resolutionVariant;
    }

    public Aqua9MappedMultiResolutionImage map(Function<Image, Image> mapper) {
        return new Aqua9MappedMultiResolutionImage(this, mapper);
    }

    @Override
    public int getWidth(ImageObserver observer) {
        updateInfo(observer, ImageObserver.WIDTH);
        return baseImage.getWidth(observer);
    }

    @Override
    public int getHeight(ImageObserver observer) {
        updateInfo(observer, ImageObserver.HEIGHT);
        return baseImage.getHeight(observer);
    }

    @Override
    public Object getProperty(String name, ImageObserver observer) {
        updateInfo(observer, ImageObserver.PROPERTIES);
        return Image.UndefinedProperty;
    }

    @Override
    public Image getScaledInstance(int width, int height, int hints) {
        return getResolutionVariant(width, height);
    }

    private void updateInfo(ImageObserver observer, int info) {
        availableInfo |= (observer == null) ? ImageObserver.ALLBITS : info;
    }

    private static class ImageCacheKey implements VImageCache.PixelsKey {

        private final int pixelCount;
        private final int hash;

        private final int w;
        private final int h;
        private final Image baseImage;

        ImageCacheKey(Image baseImage, int w, int h) {
            this.baseImage = baseImage;
            this.w = w;
            this.h = h;
            this.pixelCount = w * h;
            hash = hash();
        }

        @Override
        public int getPixelCount() {
            return pixelCount;
        }

        private int hash() {
            int hash = baseImage.hashCode();
            hash = 31 * hash + w;
            hash = 31 * hash + h;
            return hash;
        }

        @Override
        public int hashCode() {
            return hash;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj instanceof ImageCacheKey) {
                ImageCacheKey key = (ImageCacheKey) obj;
                return baseImage == key.baseImage && w == key.w && h == key.h;
            }
            return false;
        }
    }
}
