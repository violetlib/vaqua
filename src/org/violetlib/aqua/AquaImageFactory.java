/*
 * Changes copyright (c) 2015-2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

/*
 * Copyright (c) 2011, 2014, Oracle and/or its affiliates. All rights reserved.
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
import java.awt.image.BufferedImage;
import java.awt.image.ImageFilter;
import java.awt.image.ImageObserver;
import java.awt.image.RGBImageFilter;
import java.io.File;
import java.net.URL;
import java.security.PrivilegedAction;
import java.util.Objects;
import javax.swing.*;
import javax.swing.plaf.IconUIResource;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.aqua.AquaUtils.RecyclableSingleton;
import org.violetlib.aqua.fc.OSXFile;
import org.violetlib.jnr.aqua.AquaUIPainter.Size;

public class AquaImageFactory {

    public static final Object DARKEN_FOR_SELECTION = new Object();
    public static final Object DARKEN_FOR_PRESSED = new Object();
    public static final Object LIGHTEN_FOR_DISABLED = new Object();
    public static final Object LIGHTEN_100 = new LightenOperator(100);
    public static final Object LIGHTEN_50 = new LightenOperator(50);
    public static final Object LIGHTEN_25 = new LightenOperator(25);
    public static final Object INVERT_FOR_DARK_MODE = new Object();

    public static boolean debugNativeRendering = false;
    private static final int kAlertIconSize = 64;

    // Template images are used with a variety of colors depending upon context.
    // They also need to be identified as such.
    // To avoid recomputation, we soft cache this information.

    private static final AquaImageCache imageCache = new AquaImageCache();

    static class AquaImageCache extends ProcessedImageCache {
        @Override
        protected boolean determineTemplateImage(@NotNull Image source) {
            return AquaImageFactory.determineTemplateImageStatus(source);
        }

        @Override
        protected @NotNull Image createImageFromTemplate(@NotNull Image source, @NotNull Color color) {
            return AquaImageFactory.createImageFromTemplate(source, color);
        }

        @Override
        protected @NotNull Image createProcessedImage(@NotNull Image source, @NotNull Object operator) {
            return AquaImageFactory.createProcessedImage(source, operator);
        }
    }

    private static ImageIcon regularPopupMenuCheckIcon;
    private static ImageIcon smallPopupMenuCheckIcon;
    private static ImageIcon miniPopupMenuCheckIcon;

    public static IconUIResource getComputerIcon() {
        return new IconUIResource(new AquaIcon.CachingScalingIcon(16, 16) {
            Image createImage() {
                return getNSIcon("NSComputer");
            }
        });
    }

    public static IconUIResource getConfirmImageIcon() {
        // public, because UIDefaults.ProxyLazyValue uses reflection to get this value
        return new IconUIResource(new AquaIcon.CachingScalingIcon(kAlertIconSize, kAlertIconSize) {
            Image createImage() {
                return getGenericJavaIcon();
            }
        });
    }

    public static ImageIconUIResource getCautionImageIcon() {
        // public, because UIDefaults.ProxyLazyValue uses reflection to get this value
        return getAppIconCompositedOn(AquaIcon.getCautionIconImage());
    }

    public static ImageIconUIResource getStopImageIcon() {
        // public, because UIDefaults.ProxyLazyValue uses reflection to get this value
        return getAppIconCompositedOn(AquaIcon.getStopIconImage());
    }

    public static ImageIconUIResource getLockImageIcon() {
        // public, because UIDefaults.ProxyLazyValue uses reflection to get this value
        Image lockIcon = Toolkit.getDefaultToolkit().getImage("NSImage://NSSecurity");
        return getAppIconCompositedOn(lockIcon);
    }

    static Image getGenericJavaIcon() {
        return java.security.AccessController.doPrivileged(new PrivilegedAction<Image>() {
            public Image run() {
                return JavaSupport.getDockIconImage();
            }
        });
    }

    protected static ImageIconUIResource getAppIconCompositedOn(Image background) {
        return new ImageIconUIResource(applyMapper(background, appIconCompositor));
    }

    public static Image applyMapper(Image source, AquaMultiResolutionImage.Mapper mapper) {
        return AquaMultiResolutionImage.apply(source, mapper);
    }

    private static final AppIconCompositor appIconCompositor = new AppIconCompositor();

    private static class AppIconCompositor implements AquaMultiResolutionImage.Mapper {
        @Override
        public BufferedImage map(Image source, int scaleFactor) {
            return getAppIconImageCompositedOn(source, scaleFactor);
        }
    }

    private static BufferedImage getAppIconImageCompositedOn(Image background, int scaleFactor) {
        int scaledAlertIconSize = background.getWidth(null) * scaleFactor;
        int kAlertSubIconSize = (int) (scaledAlertIconSize * 0.5);
        int kAlertSubIconInset = scaledAlertIconSize - kAlertSubIconSize;
        Icon smallAppIconScaled = new AquaIcon.CachingScalingIcon(
                kAlertSubIconSize, kAlertSubIconSize) {
            Image createImage() {
                return getGenericJavaIcon();
            }
        };

        BufferedImage image = new BufferedImage(scaledAlertIconSize,
                scaledAlertIconSize, BufferedImage.TYPE_INT_ARGB_PRE);
        Graphics g = image.getGraphics();
        g.drawImage(background, 0, 0,
                scaledAlertIconSize, scaledAlertIconSize, null);
        if (g instanceof Graphics2D) {
            // improves icon rendering quality in Quartz
            ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_RENDERING,
                    RenderingHints.VALUE_RENDER_QUALITY);
        }

        smallAppIconScaled.paintIcon(null, g, kAlertSubIconInset, kAlertSubIconInset);
        g.dispose();

        return image;
    }

    public static @Nullable ImageIcon loadResource(String resource) {
        URL u = AquaImageFactory.class.getResource(resource);
        return u != null ? new ImageIcon(Toolkit.getDefaultToolkit().createImage(u)) : null;
    }

    public static @Nullable Image getImage(@Nullable File file, int size) {
        if (file != null) {
            ImageFileIconCreator r = new ImageFileIconCreator(file, size, size);
            return r.getImage();
        }

        return null;
    }

    private static class ImageFileIconCreator {
        private String path;
        private int width;
        private int height;
        private Image result;

        public ImageFileIconCreator(File file, int width, int height) {
            this.path = file.getAbsolutePath();
            this.width = width;
            this.height = height;
        }

        public Image getImage() {
            if (result == null) {
                AquaNativeSupport.load();
                int[][] buffers = new int[2][];
                if (!nativeRenderImageFile(path, buffers, width, height)) {
                    if (debugNativeRendering) {
                        AquaUtils.logDebug("Failed to render image file " + path);
                    }
                    throw new UnsupportedOperationException();
                }

                if (debugNativeRendering) {
                    AquaUtils.logDebug("Rendered image file " + path);
                }
                result = AquaMultiResolutionImage.createImage(width, height, buffers[0], buffers[1]);
            }
            return result;
        }
    }

    public static ImageIconUIResource getTreeFolderIcon() {
        // public, because UIDefaults.ProxyLazyValue uses reflection to get this value
        return treeFolderIconResource.getInstance();
    }

    public static ImageIconUIResource getTreeOpenFolderIcon() {
        // public, because UIDefaults.ProxyLazyValue uses reflection to get this value
        return treeOpenFolderIconResource.getInstance();
    }

    public static ImageIconUIResource getTreeDocumentIcon() {
        // public, because UIDefaults.ProxyLazyValue uses reflection to get this value
        return treeDocumentIconResource.getInstance();
    }

    private static final TreeFolderIconUIResourceSingleton treeFolderIconResource = new TreeFolderIconUIResourceSingleton();

    private static class TreeFolderIconUIResourceSingleton extends RecyclableSingleton<ImageIconUIResource> {
        @Override
        protected ImageIconUIResource getInstance() {
            Image im = OSXFile.getDirectoryIconImage(20);
            return new ImageIconUIResource(im);
        }
    }

    private static final TreeOpenFolderIconUIResourceSingleton treeOpenFolderIconResource = new TreeOpenFolderIconUIResourceSingleton();

    private static class TreeOpenFolderIconUIResourceSingleton extends RecyclableSingleton<ImageIconUIResource> {
        @Override
        protected ImageIconUIResource getInstance() {
            return AquaIcon.getOpenFolderIcon();
        }
    }

    private static final TreeDocumentIconUIResourceSingleton treeDocumentIconResource = new TreeDocumentIconUIResourceSingleton();

    private static class TreeDocumentIconUIResourceSingleton extends RecyclableSingleton<ImageIconUIResource> {
        @Override
        protected ImageIconUIResource getInstance() {
            Image im = OSXFile.getFileIconImage(20);
            return new ImageIconUIResource(im);
        }
    }

    static class NamedImageSingleton extends RecyclableSingleton<Image> {
        protected final String namedImage;

        NamedImageSingleton(String namedImage) {
            this.namedImage = namedImage;
        }

        @Override
        protected Image getInstance() {
            return getNSIcon(namedImage);
        }
    }

    static class IconUIResourceSingleton extends RecyclableSingleton<ImageIconUIResource> {
        protected final NamedImageSingleton holder;

        public IconUIResourceSingleton(NamedImageSingleton holder) {
            this.holder = holder;
        }

        @Override
        protected ImageIconUIResource getInstance() {
            return new ImageIconUIResource(holder.get());
        }
    }

    protected static final NamedImageSingleton northArrow = new NamedImageSingleton("NSMenuScrollUp");
    protected static final IconUIResourceSingleton northArrowIcon = new IconUIResourceSingleton(northArrow);
    protected static final NamedImageSingleton southArrow = new NamedImageSingleton("NSMenuScrollDown");
    protected static final IconUIResourceSingleton southArrowIcon = new IconUIResourceSingleton(southArrow);
    protected static final NamedImageSingleton westArrow = new NamedImageSingleton("NSMenuSubmenuLeft");
    protected static final IconUIResourceSingleton westArrowIcon = new IconUIResourceSingleton(westArrow);
    protected static final NamedImageSingleton eastArrow = new NamedImageSingleton("NSMenuSubmenu");
    protected static final IconUIResourceSingleton eastArrowIcon = new IconUIResourceSingleton(eastArrow);

    public static @Nullable Image getArrowImageForDirection(int direction) {
        switch(direction) {
            case SwingConstants.NORTH: return northArrow.get();
            case SwingConstants.SOUTH: return southArrow.get();
            case SwingConstants.EAST: return eastArrow.get();
            case SwingConstants.WEST: return westArrow.get();
        }
        return null;
    }

    public static Icon getMenuUpArrowIcon() {
        return northArrowIcon.get();
    }

    public static Icon getMenuDownArrowIcon() {
        return southArrowIcon.get();
    }

    public static Icon getMenuArrowIcon() {
        return eastArrowIcon.get();
    }

    public static Icon getPopupMenuItemCheckIcon(Size sizeVariant) {
        if (sizeVariant == Size.SMALL) {
            return getSmallPopupMenuItemCheckIcon();
        } else if (sizeVariant == Size.MINI) {
            return getMiniPopupMenuItemCheckIcon();
        } else {
            return getRegularPopupMenuItemCheckIcon();
        }
    }

    private static Icon getRegularPopupMenuItemCheckIcon() {
        if (regularPopupMenuCheckIcon == null) {
            regularPopupMenuCheckIcon = getPopupMenuItemCheckIcon(10);
        }
        return regularPopupMenuCheckIcon;
    }

    private static Icon getSmallPopupMenuItemCheckIcon() {
        if (smallPopupMenuCheckIcon == null) {
            smallPopupMenuCheckIcon = getPopupMenuItemCheckIcon(8);
        }
        return smallPopupMenuCheckIcon;
    }

    private static Icon getMiniPopupMenuItemCheckIcon() {
        if (miniPopupMenuCheckIcon == null) {
            miniPopupMenuCheckIcon = getPopupMenuItemCheckIcon(6);
        }
        return miniPopupMenuCheckIcon;
    }

    private static ImageIcon getPopupMenuItemCheckIcon(int size) {
        Image im = getNSImage("NSMenuCheckmark", size, size);
        return new ImageIcon(im);
    }

    public static Icon getMenuItemCheckIcon() {
        return new ImageIcon(getNSIcon("NSMenuCheckmark"));
    }

    public static Icon getMenuItemDashIcon() {
        return new ImageIcon(getNSIcon("NSMenuMixedState"));
    }

    private static @Nullable Image getNSImage(@NotNull String imageName, int width, int height) {
        return getNativeImage(imageName, width, height);
    }

    private static Image getNSIcon(String imageName) {
        Image icon = Toolkit.getDefaultToolkit().getImage("NSImage://" + imageName);
        return icon;
    }

    public static class NineSliceMetrics {
        public final int wCut, eCut, nCut, sCut;
        public final int minW, minH;
        public final boolean showMiddle, stretchH, stretchV;

        public NineSliceMetrics(int minWidth, int minHeight, int westCut, int eastCut, int northCut, int southCut) {
            this(minWidth, minHeight, westCut, eastCut, northCut, southCut, true);
        }

        public NineSliceMetrics(int minWidth, int minHeight, int westCut, int eastCut, int northCut, int southCut, boolean showMiddle) {
            this(minWidth, minHeight, westCut, eastCut, northCut, southCut, showMiddle, true, true);
        }

        public NineSliceMetrics(int minWidth, int minHeight, int westCut, int eastCut, int northCut, int southCut, boolean showMiddle, boolean stretchHorizontally, boolean stretchVertically) {
            this.wCut = westCut; this.eCut = eastCut; this.nCut = northCut; this.sCut = southCut;
            this.minW = minWidth; this.minH = minHeight;
            this.showMiddle = showMiddle; this.stretchH = stretchHorizontally; this.stretchV = stretchVertically;
        }
    }

    /*
     * A "paintable" which holds nine images, which represent a sliced up initial
     * image that can be stretched from its middles.
     */
    public static class SlicedImageControl {
        protected final BufferedImage NW, N, NE;
        protected final BufferedImage W, C, E;
        protected final BufferedImage SW, S, SE;

        protected final NineSliceMetrics metrics;

        protected final int totalWidth, totalHeight;
        protected final int centerColWidth, centerRowHeight;

        public SlicedImageControl(Image img, int westCut, int eastCut, int northCut, int southCut) {
            this(img, westCut, eastCut, northCut, southCut, true);
        }

        public SlicedImageControl(Image img, int westCut, int eastCut, int northCut, int southCut, boolean useMiddle) {
            this(img, westCut, eastCut, northCut, southCut, useMiddle, true, true);
        }

        public SlicedImageControl(Image img, int westCut, int eastCut, int northCut, int southCut, boolean useMiddle, boolean stretchHorizontally, boolean stretchVertically) {
            this(img, new NineSliceMetrics(img.getWidth(null), img.getHeight(null), westCut, eastCut, northCut, southCut, useMiddle, stretchHorizontally, stretchVertically));
        }

        public SlicedImageControl(Image img, NineSliceMetrics metrics) {
            this.metrics = metrics;

            if (img.getWidth(null) != metrics.minW || img.getHeight(null) != metrics.minH) {
                throw new IllegalArgumentException("SlicedImageControl: template image and NineSliceMetrics don't agree on minimum dimensions");
            }

            totalWidth = metrics.minW;
            totalHeight = metrics.minH;
            centerColWidth = totalWidth - metrics.wCut - metrics.eCut;
            centerRowHeight = totalHeight - metrics.nCut - metrics.sCut;

            NW = createSlice(img, 0, 0, metrics.wCut, metrics.nCut);
            N = createSlice(img, metrics.wCut, 0, centerColWidth, metrics.nCut);
            NE = createSlice(img, totalWidth - metrics.eCut, 0, metrics.eCut, metrics.nCut);
            W = createSlice(img, 0, metrics.nCut, metrics.wCut, centerRowHeight);
            C = metrics.showMiddle ? createSlice(img, metrics.wCut, metrics.nCut, centerColWidth, centerRowHeight) : null;
            E = createSlice(img, totalWidth - metrics.eCut, metrics.nCut, metrics.eCut, centerRowHeight);
            SW = createSlice(img, 0, totalHeight - metrics.sCut, metrics.wCut, metrics.sCut);
            S = createSlice(img, metrics.wCut, totalHeight - metrics.sCut, centerColWidth, metrics.sCut);
            SE = createSlice(img, totalWidth - metrics.eCut, totalHeight - metrics.sCut, metrics.eCut, metrics.sCut);
        }

        static BufferedImage createSlice(Image img, int x, int y, int w, int h) {
            if (w == 0 || h == 0) return null;

            BufferedImage slice = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB_PRE);
            Graphics2D g2d = slice.createGraphics();
            g2d.drawImage(img, 0, 0, w, h, x, y, x + w, y + h, null);
            g2d.dispose();

            return slice;
        }

        public void paint(Graphics g, int x, int y, int w, int h) {
            g.translate(x, y);

            if (w < totalWidth || h < totalHeight) {
                paintCompressed(g, w, h);
            } else {
                paintStretchedMiddles(g, w, h);
            }

            g.translate(-x, -y);
        }

        void paintStretchedMiddles(Graphics g, int w, int h) {
            int baseX = metrics.stretchH ? 0 : ((w / 2) - (totalWidth / 2));
            int baseY = metrics.stretchV ? 0 : ((h / 2) - (totalHeight / 2));
            int adjustedWidth = metrics.stretchH ? w : totalWidth;
            int adjustedHeight = metrics.stretchV ? h : totalHeight;

            if (NW != null) g.drawImage(NW, baseX, baseY, null);
            if (N != null) g.drawImage(N, baseX + metrics.wCut, baseY, adjustedWidth - metrics.eCut - metrics.wCut, metrics.nCut, null);
            if (NE != null) g.drawImage(NE, baseX + adjustedWidth - metrics.eCut, baseY, null);
            if (W != null) g.drawImage(W, baseX, baseY + metrics.nCut, metrics.wCut, adjustedHeight - metrics.nCut - metrics.sCut, null);
            if (C != null) g.drawImage(C, baseX + metrics.wCut, baseY + metrics.nCut, adjustedWidth - metrics.eCut - metrics.wCut, adjustedHeight - metrics.nCut - metrics.sCut, null);
            if (E != null) g.drawImage(E, baseX + adjustedWidth - metrics.eCut, baseY + metrics.nCut, metrics.eCut, adjustedHeight - metrics.nCut - metrics.sCut, null);
            if (SW != null) g.drawImage(SW, baseX, baseY + adjustedHeight - metrics.sCut, null);
            if (S != null) g.drawImage(S, baseX + metrics.wCut, baseY + adjustedHeight - metrics.sCut, adjustedWidth - metrics.eCut - metrics.wCut, metrics.sCut, null);
            if (SE != null) g.drawImage(SE, baseX + adjustedWidth - metrics.eCut, baseY + adjustedHeight - metrics.sCut, null);

            /*
            if (NW != null) {g.setColor(Color.GREEN); g.fillRect(baseX, baseY, NW.getWidth(), NW.getHeight());}
            if (N != null) {g.setColor(Color.RED); g.fillRect(baseX + metrics.wCut, baseY, adjustedWidth - metrics.eCut - metrics.wCut, metrics.nCut);}
            if (NE != null) {g.setColor(Color.BLUE); g.fillRect(baseX + adjustedWidth - metrics.eCut, baseY, NE.getWidth(), NE.getHeight());}
            if (W != null) {g.setColor(Color.PINK); g.fillRect(baseX, baseY + metrics.nCut, metrics.wCut, adjustedHeight - metrics.nCut - metrics.sCut);}
            if (C != null) {g.setColor(Color.ORANGE); g.fillRect(baseX + metrics.wCut, baseY + metrics.nCut, adjustedWidth - metrics.eCut - metrics.wCut, adjustedHeight - metrics.nCut - metrics.sCut);}
            if (E != null) {g.setColor(Color.CYAN); g.fillRect(baseX + adjustedWidth - metrics.eCut, baseY + metrics.nCut, metrics.eCut, adjustedHeight - metrics.nCut - metrics.sCut);}
            if (SW != null) {g.setColor(Color.MAGENTA); g.fillRect(baseX, baseY + adjustedHeight - metrics.sCut, SW.getWidth(), SW.getHeight());}
            if (S != null) {g.setColor(Color.DARK_GRAY); g.fillRect(baseX + metrics.wCut, baseY + adjustedHeight - metrics.sCut, adjustedWidth - metrics.eCut - metrics.wCut, metrics.sCut);}
            if (SE != null) {g.setColor(Color.YELLOW); g.fillRect(baseX + adjustedWidth - metrics.eCut, baseY + adjustedHeight - metrics.sCut, SE.getWidth(), SE.getHeight());}
            */
        }

        void paintCompressed(Graphics g, int w, int h) {
            double heightRatio = h > totalHeight ? 1.0 : (double)h / (double)totalHeight;
            double widthRatio = w > totalWidth ? 1.0 : (double)w / (double)totalWidth;

            int northHeight = (int)(metrics.nCut * heightRatio);
            int southHeight = (int)(metrics.sCut * heightRatio);
            int centerHeight = h - northHeight - southHeight;

            int westWidth = (int)(metrics.wCut * widthRatio);
            int eastWidth = (int)(metrics.eCut * widthRatio);
            int centerWidth = w - westWidth - eastWidth;

            if (NW != null) g.drawImage(NW, 0, 0, westWidth, northHeight, null);
            if (N != null) g.drawImage(N, westWidth, 0, centerWidth, northHeight, null);
            if (NE != null) g.drawImage(NE, w - eastWidth, 0, eastWidth, northHeight, null);
            if (W != null) g.drawImage(W, 0, northHeight, westWidth, centerHeight, null);
            if (C != null) g.drawImage(C, westWidth, northHeight, centerWidth, centerHeight, null);
            if (E != null) g.drawImage(E, w - eastWidth, northHeight, eastWidth, centerHeight, null);
            if (SW != null) g.drawImage(SW, 0, h - southHeight, westWidth, southHeight, null);
            if (S != null) g.drawImage(S, westWidth, h - southHeight, centerWidth, southHeight, null);
            if (SE != null) g.drawImage(SE, w - eastWidth, h - southHeight, eastWidth, southHeight, null);
        }
    }

    /**
     * Obtain a native image with a specified logical size.
     */
    private static native @Nullable Image getNativeImage(String name, int width, int height);

    /**
     * Render an image file.
     *
     * @param path the path to the file.
     * @param buffers 1x and 2x rasters are stored here (2x is optional)
     * @param w The width of the image.
     * @param h The height of the image.
     * @return true if successful, false otherwise.
     */
    private static native boolean nativeRenderImageFile(String path, int[][] buffers, int w, int h);

    public static @NotNull Object getLightenOperator(int percent) {
        return new LightenOperator(percent);
    }

    private static class LightenOperator {
        int percent;

        public LightenOperator(int percent) {
            this.percent = percent;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            LightenOperator that = (LightenOperator) o;
            return percent == that.percent;
        }

        @Override
        public int hashCode() {
            return Objects.hash(percent);
        }
    }

    private static @NotNull Image createProcessedImage(@NotNull Image source, @NotNull Object operator) {
        if (operator == DARKEN_FOR_SELECTION) {
            return applyFilter(source, new GenerateSelectedDarkFilter());
        }
        if (operator == DARKEN_FOR_PRESSED) {
            return applyFilter(source, new GeneratePressedDarkFilter());
        }
        if (operator == LIGHTEN_FOR_DISABLED) {
            return applyFilter(source, new GenerateDisabledLightFilter());
        }
        if (operator instanceof LightenOperator) {
            int percent = ((LightenOperator) operator).percent;
            GrayFilter filter = new GrayFilter(true, percent);
            return AquaMultiResolutionImage.apply(source, filter);
        }
        if (operator == INVERT_FOR_DARK_MODE) {
            if (isTemplateImage(source)) {
                return source;
            }
            return applyFilter(source, new InvertImageForDarkModeFilter());
        }
        return source;
    }

    private static class GenerateSelectedDarkFilter extends IconImageFilter {
        @Override
        int getGreyFor(int gray) {
            return gray * 75 / 100;
        }
    }

    private static class GeneratePressedDarkFilter extends RGBImageFilter {
        public GeneratePressedDarkFilter() {
            canFilterIndexColorModel = true;
        }

        @Override
        public int filterRGB(int x, int y, int rgb) {
            int red = (rgb >> 16) & 0xff;
            int green = (rgb >> 8) & 0xff;
            int blue = rgb & 0xff;

            return (rgb & 0xff000000) | (transform(red) << 16) | (transform(green) << 8) | (transform(blue) << 0);
        }

        protected int transform(int c) {
            int result = (c * 40) / 100;
            if (result < 0) result = 0;
            if (result > 255) result = 255;
            return result;
        }
    }

    private static class GenerateDisabledLightFilter extends RGBImageFilter {
        public GenerateDisabledLightFilter() {
            canFilterIndexColorModel = true;
        }

        @Override
        public int filterRGB(int x, int y, int rgb) {
            int red = (rgb >> 16) & 0xff;
            int green = (rgb >> 8) & 0xff;
            int blue = rgb & 0xff;

            return (rgb & 0xff000000) | (transform(red) << 16) | (transform(green) << 8) | (transform(blue) << 0);
        }

        protected int transform(int c) {
            int result = 255 - ((255 - c) * 50) / 100;
            if (result < 0) result = 0;
            if (result > 255) result = 255;
            return result;
        }
    }

    private abstract static class IconImageFilter extends RGBImageFilter {
        IconImageFilter() {
            canFilterIndexColorModel = true;
        }

        @Override
        public final int filterRGB(int x, int y, int rgb) {
            int red = (rgb >> 16) & 0xff;
            int green = (rgb >> 8) & 0xff;
            int blue = rgb & 0xff;
            int gray = getGreyFor((int) ((0.30 * red + 0.59 * green + 0.11 * blue) / 3));

            return (rgb & 0xff000000) | (grayTransform(red, gray) << 16) | (grayTransform(green, gray) << 8) | (grayTransform(blue, gray) << 0);
        }

        private static int grayTransform(int color, int gray) {
            int result = color - gray;
            if (result < 0) result = 0;
            if (result > 255) result = 255;
            return result;
        }

        abstract int getGreyFor(int gray);
    }

    /**
     * Determine whether an image is a template image. A template image is an image that contains only clear or
     * (possibly translucent) black pixels.
     */
    public static boolean isTemplateImage(@NotNull Image image) {
        return imageCache.isTemplateImage(image);
    }

    /**
     * Determine whether an icon is a template image. A template image is an image that contains only clear or
     * (possibly translucent) black pixels.
     */
    public static boolean isTemplateIcon(@NotNull Icon icon) {
        return imageCache.isTemplateIcon(icon);
    }

    private static boolean determineTemplateImageStatus(@NotNull Image image) {
        // Run the template filter in a special way that allows us to observe its behavior.
        TemplateFilter filter = new TemplateFilter(Color.BLACK, true);
        // The following is to force a lazy mapped image to run the filter
        // Multiresolution toolkit images return themselves unless the requested size is greater than the nominal size
        new ImageIcon(image);
        int width = image.getWidth(null);
        int height = image.getHeight(null);
        Image source = JavaSupport.getResolutionVariant(image, width * 2, height * 2);
        Image im = applyFilter(source, filter);
        // force the image to be created
        new ImageIcon(im);
        return filter.isTemplate();
    }

    public static @NotNull Image generateTemplateImage(@NotNull Image image) {
        return applyFilter(image, new GenerateTemplateFilter());
    }

    /**
     * Return the processed version of the specified image.
     * @param image The source image.
     * @param operator The operation to be performed on the image.
     * @return the processed version of {@code image}.
     */
    public static @NotNull Image getProcessedImage(@NotNull Image image, @NotNull Object operator) {
        return imageCache.getProcessedImage(image, operator);
    }

    /**
     * Return the processed version of the specified icon image.
     * @param icon The source icon.
     * @param operator The operation to be performed on the icon image, or null to return the actual icon image.
     * @return the processed version of {@code icon}, or null if the icon is not valid.
     */
    public static @Nullable Image getProcessedImage(@NotNull Icon icon, @Nullable Object operator) {
        return imageCache.getProcessedImage(icon, operator);
    }

    /**
     * Create an image by replacing the visible pixels in a template image with the specified color. A template image
     * is an image that contains only clear or (possibly translucent) black pixels.
     * @param image The template image.
     * @param replacementColor The replacement color.
     * @return the new image, or null if the source image is not a template image.
     */
    private static Image createImageFromTemplate(Image image, Color replacementColor) {
        int width = image.getWidth(null);
        int height = image.getHeight(null);
        BufferedImage result = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB_PRE);
        Graphics2D g = result.createGraphics();
        g.setComposite(AlphaComposite.Src);
        g.drawImage(image, 0, 0, null);
        g.setComposite(AlphaComposite.SrcIn);
        g.setColor(replacementColor);
        g.fillRect(0, 0, width, height);
        g.dispose();
        return result;
    }

    public static Image applyFilter(Image image, ImageFilter filter) {
        return JavaSupport.applyFilter(image, filter);
    }

    // This filter is now used only for testing whether an image is a template image
    private static class TemplateFilter extends RGBImageFilter {
        private final int replacementAlpha;
        private final int replacementRGB;
        private final boolean isForTesting;
        private boolean isTemplate;

        public TemplateFilter(Color replacementColor, boolean isForTesting) {
            this.replacementRGB = replacementColor.getRGB() & 0xffffff;
            this.replacementAlpha = replacementColor.getAlpha();
            this.isForTesting = isForTesting;
            canFilterIndexColorModel = true;
            isTemplate = true;
        }

        @Override
        public Object clone() {
            if (isForTesting) {
                return this;  // special case for filter used once, we need to retain access to the isTemplate flag
            }
            return super.clone();
        }

        public boolean isTemplate() {
            return isTemplate;
        }

        @Override
        public int filterRGB(int x, int y, int rgb) {
            int alpha = rgb >> 24 & 0xff;
            int color = rgb & 0xffffff;
            if (alpha != 0 && color != 0) {
                isTemplate = false;
            }
            alpha = alpha * replacementAlpha / 255;
            return alpha << 24 | replacementRGB;
        }
    }

    private static class GenerateTemplateFilter extends RGBImageFilter {

        public GenerateTemplateFilter() {
            canFilterIndexColorModel = true;
        }

        @Override
        public int filterRGB(int x, int y, int rgb) {
            int alpha = rgb >> 24 & 0xff;
            int color = rgb & 0xffffff;
            if (color > 0) {
                color = color & 0xff;
                alpha = alpha * color / 255;
            }
            return alpha << 24;
        }
    }

    private static class InvertImageForDarkModeFilter extends RGBImageFilter {

        public InvertImageForDarkModeFilter() {
            canFilterIndexColorModel = true;
        }

        public int filterRGB(int x, int y, int rgb) {
            // Use NTSC conversion formula.
            int gray = (int)((0.30 * ((rgb >> 16) & 0xff) + 0.59 * ((rgb >> 8) & 0xff) + 0.11 * (rgb & 0xff)) / 3);
            gray = (int) ((255 - gray) * 0.7);
            if (gray < 0) gray = 0;
            if (gray > 255) gray = 255;
            return (rgb & 0xff000000) | (gray << 16) | (gray << 8) | (gray << 0);
        }
    }

    private static @NotNull Image waitForImage(@NotNull Image image) {
        boolean[] mutex = new boolean[] { false };
        ImageObserver observer = (Image img, int infoflags, int x, int y, int width, int height) -> {
            if ((width != -1 && height != -1 && (infoflags & ImageObserver.ALLBITS) != 0)
                    || (infoflags & (ImageObserver.ERROR | ImageObserver.ABORT | ImageObserver.FRAMEBITS)) != 0) {
                synchronized (mutex) {
                    mutex[0] = true;
                    mutex.notify();
                }
                return false;
            } else {
                return true;
            }
        };
        synchronized (mutex) {
            while (!mutex[0] && image.getWidth(observer) == -1) {
                try {
                    mutex.wait();
                } catch (InterruptedException e) {
                    break;
                }
            }
        }
        return image;
    }
}
