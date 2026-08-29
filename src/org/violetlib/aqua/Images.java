/*
 * @(#)Images.java
 *
 * Copyright (c) 2005-2013 Werner Randelshofer, Switzerland.
 * Copyright (c) 2025 Alan Snyder.
 * You may not use, copy or modify this file, except in compliance with the
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.awt.image.*;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Properties;
import javax.swing.*;

import org.jetbrains.annotations.*;

/**
 * Image processing methods.
 *
 * @author  Werner Randelshofer, Karl von Randow
 * @version $Id$
 */
public class Images {

    /** Prevent instance creation. */
    private Images() {
    }
    private static GraphiteFilter graphiteFilter;

    private static GraphiteFilter getGraphiteFilter() {
        if (graphiteFilter == null) {
            graphiteFilter = new GraphiteFilter();
        }
        return graphiteFilter;
    }

    //    public static Image getImage(Class baseClass, String location) {
//        // As of Java 1.8, only getImage() supports multiresolution images
//        URL resource=baseClass.getResource(location);
//        if (resource==null) throw new IllegalArgumentException("no resource found for location:"+location);
//        return Toolkit.getDefaultToolkit().getImage(resource);
//    }
//
//    public static Image createImage(Class baseClass, String location) {
//        URL resource=baseClass.getResource(location);
//        if (resource==null) throw new IllegalArgumentException("no resource found for location:"+location);
//        return createImage(resource);
//    }
//
//    public static Image createImage(URL resource) {
//        Image image = Toolkit.getDefaultToolkit().createImage(resource);
//        if (OSXPreferences.getString(OSXPreferences.GLOBAL_PREFERENCES, "AppleAquaColorVariant", "1").equals("6")) {
//            if (canGraphite(resource)) {
//                image = toGraphite(image);
//            }
//        }
//        return image;
//    }
    private static volatile Properties canGraphite;

    private static boolean canGraphite(URL resource) {
        if (canGraphite == null) {
            synchronized (Images.class) {
                if (canGraphite == null) {
                    Properties p = new Properties();
                    InputStream in = Images.class.getResourceAsStream("graphiteable.properties");
                    try {
                        p.load(in);
                    } catch (IOException e) {
                        Utils.logError("Failed to load graphiteable.properties", e);
                    } finally {
                        try {
                            in.close();
                        } catch (IOException ex) {
                            Utils.logError("Failed to load graphiteable.properties", ex);
                        }
                    }
                    canGraphite = p;
                }
            }
        }
        String file = resource.getFile();
        int i = file.lastIndexOf(File.separatorChar);
        if (i != -1) {
            file = file.substring(i + 1);
        }
        return canGraphite.containsKey(file);
    }

    /**
     * This method returns a buffered image with the contents of an image.
     *
     * Code derived from the Java Developers Almanac 1.4
     * http://javaalmanac.com/egs/java.awt.image/Image2Buf.html?l=rel
     */
    private static Image toGraphite(Image image) {
        return AquaImageFactory.applyFilter(image, getGraphiteFilter());
    }

    /**
     * The graphite filter converts Mac OS X artwork from "Blue Appearance" to
     * "Graphite Appearance" by desaturing the colors.
     */
    public static class GraphiteFilter extends RGBImageFilter {

        private final static float saturationAdjust = 0.179f;

        public int filterRGB(int x, int y, int rgb) {
            int alpha = rgb & 0xff000000;
            int red = (rgb >>> 16) & 0xff;
            int green = (rgb >>> 8) & 0xff;
            int blue = rgb & 0xff;

            float weight = (1f - saturationAdjust) * 1f / 3f;
            float a = weight + saturationAdjust;
            float b = weight;
            float c = weight;

            int outputRed = (int) (a * red + c * green + b * blue);
            int outputGreen = (int) (b * red + a * green + c * blue);
            int outputBlue = (int) (c * red + b * green + a * blue);
            return alpha | (outputRed << 16) | (outputGreen << 8) | (outputBlue);
        }
    }

    public static BufferedImage toBufferedImage(RenderedImage rImg) {
        BufferedImage image;
        if (rImg instanceof BufferedImage) {
            image = (BufferedImage) rImg;
        } else {
            Raster r = rImg.getData();
            WritableRaster wr = WritableRaster.createWritableRaster(
              r.getSampleModel(), null);
            rImg.copyData(wr);
            image = new BufferedImage(
              rImg.getColorModel(),
              wr,
              rImg.getColorModel().isAlphaPremultiplied(),
              null);
        }
        return image;
    }

    /**
     * Convert an image to a buffered image.
     * @param image The image.
     * @return the equivalent buffered image.
     */
    public static @NotNull BufferedImage toBufferedImage(@NotNull Image image) {
        if (image instanceof BufferedImage) {
            return (BufferedImage) image;
        }

        ImageIcon ic = new ImageIcon(image);
        return toBufferedImage(ic);
    }

    /**
     * Convert an image icon to a buffered image.
     * @param ic The image icon.
     * @return the equivalent buffered image.
     */
    public static @NotNull BufferedImage toBufferedImage(@NotNull ImageIcon ic) {
        int width = ic.getIconWidth();
        int height = ic.getIconHeight();
        BufferedImage b = createBufferedImage(ic.getImage(), width, height);
        Graphics g = b.createGraphics();
        g.drawImage(ic.getImage(), 0, 0, null);
        g.dispose();
        return b;
    }

    /**
     * Create a buffered image with a format that is compatible with the screen.
     * @param im If not null, this image is used to determine if transparency is needed.
     * @param width The image width.
     * @param height The image height.
     */
    public static @NotNull BufferedImage createBufferedImage(@Nullable Image im, int width, int height) {
        // Determine if the image has transparent pixels.
        boolean hasAlpha = true;
        if (im != null) {
            try {
                hasAlpha = hasAlpha(im);
            } catch (IllegalAccessError ignore) {
            }
        }
        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        try {
            int transparency = hasAlpha ? Transparency.TRANSLUCENT : Transparency.OPAQUE;
            GraphicsDevice gs = ge.getDefaultScreenDevice();
            GraphicsConfiguration gc = gs.getDefaultConfiguration();
            return gc.createCompatibleImage(width, height, transparency);
        } catch (Exception e) {
            // Create a buffered image using the default color model
            int type = hasAlpha ? BufferedImage.TYPE_INT_ARGB_PRE : BufferedImage.TYPE_INT_RGB;
            return new BufferedImage(width, height, type);
        }
    }

    /**
     * This method returns true if the specified image contains transparent pixels.
     */
    public static boolean hasAlpha(Image image) {
        // Source: Java Developers Almanac 1.4 [http://javaalmanac.com/egs/java.awt.image/HasAlpha.html]
        // If buffered image, the color model is readily available
        if (image instanceof BufferedImage) {
            BufferedImage bimage = (BufferedImage) image;
            return bimage.getColorModel().hasAlpha();
        }

        // Use a pixel grabber to retrieve the image's color model;
        // grabbing a single pixel is usually sufficient
        PixelGrabber pg = new PixelGrabber(image, 0, 0, 1, 1, false);
        try {
            pg.grabPixels();
        } catch (InterruptedException ignore) {
        }

        // Get the image's color model
        // We must check for null here, because the pixel grabber
        // may not have been able to retrieve the color model.
        ColorModel cm = pg.getColorModel();
        return cm != null && cm.hasAlpha();
    }

    /**
     * Splits an image into count subimages.
     */
    public static BufferedImage[] split(Image image, int count, boolean isHorizontal) {
        BufferedImage src = Images.toBufferedImage(image);
        if (count == 1) {
            return new BufferedImage[]{src};
        }

        BufferedImage[] parts = new BufferedImage[count];
        for (int i = 0; i < count; i++) {
            if (isHorizontal) {
                parts[i] = src.getSubimage(
                  src.getWidth() / count * i, 0,
                  src.getWidth() / count, src.getHeight());
            } else {
                parts[i] = src.getSubimage(
                  0, src.getHeight() / count * i,
                  src.getWidth(), src.getHeight() / count);
            }
        }
        return parts;
    }
}
