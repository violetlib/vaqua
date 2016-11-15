/*
 * Copyright (c) 2016 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.awt.image.MultiResolutionImage;
import java.util.List;
import java.util.ArrayList;
import java.util.function.Function;

/**
 * A multi-resolution image with a 1x and a 2x representation. This class is designed for Java 9.
 */

public class Aqua9MultiResolutionImage2 extends AquaMultiResolutionImage implements MultiResolutionImage {

    private final BufferedImage im1;
    private final BufferedImage im2;

    public Aqua9MultiResolutionImage2(BufferedImage im1, BufferedImage im2) {
        super(im1);
        this.im1 = im1;
        this.im2 = im2;
    }

    @Override
    public Image getResolutionVariant(double width, double height) {
        return width > baseImageWidth || height > baseImageHeight ? im2 : im1;
    }

    @Override
    public List<Image> getResolutionVariants() {
        List<Image> result = new ArrayList<>();
        result.add(im1);
        result.add(im2);
        return result;
    }

    @Override
    public AquaMultiResolutionImage map(Mapper mapper) {
        BufferedImage m1 = mapper.map(im1, 1);
        BufferedImage m2 = im2 != null ? mapper.map(im2, 2) : null;
        return new Aqua9MultiResolutionImage2(m1, m2);
    }

    @Override
    public AquaMultiResolutionImage map(Function<Image, Image> mapper) {
        BufferedImage m1 = Images.toBufferedImage(mapper.apply(im1));
        BufferedImage m2 = Images.toBufferedImage(mapper.apply(im2));
        return new Aqua9MultiResolutionImage2(m1, m2);
    }
 }
