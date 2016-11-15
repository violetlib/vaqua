/*
 * Changes copyright (c) 2016 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.color.ColorSpace;
import java.awt.image.*;
import java.util.function.Function;
import javax.swing.*;
import javax.swing.plaf.basic.BasicGraphicsUtils;

/**
 * Support for Java 9 and later
 */

public class Java9Support implements JavaSupport.JavaSupportImpl {
    @Override
    public Image getDockIconImage() {
        // TBD: requires native support at present
        return new BufferedImage(256, 256, BufferedImage.TYPE_INT_ARGB_PRE);
    }

    @Override
    public void drawString(JComponent c, Graphics2D g, String string, float x, float y) {
        BasicGraphicsUtils.drawString(c, g, string, x, y);
    }

    @Override
    public void drawStringUnderlineCharAt(JComponent c, Graphics2D g, String string, int underlinedIndex, float x, float y) {
        BasicGraphicsUtils.drawStringUnderlineCharAt(c, g, string, underlinedIndex, x, y);
    }

    @Override
    public String getClippedString(JComponent c, FontMetrics fm, String string, int availTextWidth) {
        return BasicGraphicsUtils.getClippedString(c, fm, string, availTextWidth);
    }

    @Override
    public float getStringWidth(JComponent c, FontMetrics fm, String string) {
        return BasicGraphicsUtils.getStringWidth(c, fm, string);
    }

    @Override
    public void installAATextInfo(UIDefaults table) {
        System.err.println("Antialiased text properties are not supported");
    }

    @Override
    public AquaMultiResolutionImage createMultiResolutionImage(BufferedImage im) {
        return new Aqua9MultiResolutionImage(im);
    }

    @Override
    public AquaMultiResolutionImage createMultiResolutionImage(BufferedImage im1, BufferedImage im2) {
        return new Aqua9MultiResolutionImage2(im1, im2);
    }

    @Override
    public Image applyFilter(Image image, ImageFilter filter) {
        return Aqua9MultiResolutionImage.apply(image, filter);
    }

    @Override
    public Image applyMapper(Image source, Function<Image, Image> mapper) {
        return Aqua9MultiResolutionImage.apply(source, mapper);
    }

    @Override
    public Image applyMapper(Image source, AquaMultiResolutionImage.Mapper mapper) {
        return Aqua9MultiResolutionImage.apply(source, mapper);
    }

    @Override
    public BufferedImage createImage(int width, int height, int[] data) {
        ColorModel colorModel = new
                DirectColorModel(
                             ColorSpace.getInstance(ColorSpace.CS_sRGB),
                             32,
                             0x00ff0000,// Red
                             0x0000ff00,// Green
                             0x000000ff,// Blue
                             0xff000000,// Alpha
                             true,       // Alpha Premultiplied
                             DataBuffer.TYPE_INT
                             );
        WritableRaster raster = colorModel.createCompatibleWritableRaster(width, height);
        raster.setPixels(0, 0, width, height, data);
        BufferedImage b = new BufferedImage(colorModel, raster, true, null);
        return b;
    }

    public void preload(Image image, int availableInfo) {

        // TBD: preloading is supported by a private method of ToolkitImage
        // Currently would need to use native code to call it
    }

    @Override
    public void lockRenderQueue() {
        // TBD
    }

    @Override
    public void unlockRenderQueue() {
        // TBD
    }
}
