/*
 * Changes copyright (c) 2015-2016 Alan Snyder.
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
import java.awt.image.*;
import java.io.File;
import java.net.URL;
import java.security.PrivilegedAction;
import javax.swing.*;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.IconUIResource;
import javax.swing.plaf.UIResource;

import org.violetlib.aqua.AquaIcon.InvertableIcon;
import org.violetlib.aqua.AquaUtils.RecyclableSingleton;
import org.violetlib.aqua.fc.OSXFile;
import org.violetlib.jnr.aqua.AquaUIPainter.Size;

public class AquaImageFactory {

    public static boolean debugNativeRendering = false;
    private static final int kAlertIconSize = 64;

    private static InvertableImageIcon regularPopupMenuCheckIcon;
    private static InvertableImageIcon smallPopupMenuCheckIcon;
    private static InvertableImageIcon miniPopupMenuCheckIcon;

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

    public static IconUIResource getCautionImageIcon() {
        // public, because UIDefaults.ProxyLazyValue uses reflection to get this value
        return getAppIconCompositedOn(AquaIcon.getCautionIconImage());
    }

    public static IconUIResource getStopImageIcon() {
        // public, because UIDefaults.ProxyLazyValue uses reflection to get this value
        return getAppIconCompositedOn(AquaIcon.getStopIconImage());
    }

    public static IconUIResource getLockImageIcon() {
        // public, because UIDefaults.ProxyLazyValue uses reflection to get this value
        final Image lockIcon = Toolkit.getDefaultToolkit().getImage("NSImage://NSSecurity");
        return getAppIconCompositedOn(lockIcon);
    }

    static Image getGenericJavaIcon() {
        return java.security.AccessController.doPrivileged(new PrivilegedAction<Image>() {
            public Image run() {
                return com.apple.eawt.Application.getApplication().getDockIconImage();
            }
        });
    }

    protected static IconUIResource getAppIconCompositedOn(final Image background) {
        return new IconUIResource(new ImageIcon(applyMapper(background, appIconCompositor)));
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

    static BufferedImage getAppIconImageCompositedOn(final Image background, int scaleFactor) {
        final int scaledAlertIconSize = background.getWidth(null) * scaleFactor;
        final int kAlertSubIconSize = (int) (scaledAlertIconSize * 0.5);
        final int kAlertSubIconInset = scaledAlertIconSize - kAlertSubIconSize;
        final Icon smallAppIconScaled = new AquaIcon.CachingScalingIcon(
                kAlertSubIconSize, kAlertSubIconSize) {
            Image createImage() {
                return getGenericJavaIcon();
            }
        };

        final BufferedImage image = new BufferedImage(scaledAlertIconSize,
                scaledAlertIconSize, BufferedImage.TYPE_INT_ARGB_PRE);
        final Graphics g = image.getGraphics();
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

    public static Image loadResource(String resource) {
        URL u = AquaImageFactory.class.getResource(resource);
        return u != null ? Toolkit.getDefaultToolkit().createImage(u) : null;
    }

    public static Image getImage(File file, int size) {
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
                        System.err.println("Failed to render image file " + path);
                    }
                    throw new UnsupportedOperationException();
                }

                if (debugNativeRendering) {
                    System.err.println("Rendered image file " + path);
                }
                result = AquaMultiResolutionImage.createImage(width, height, buffers[0], buffers[1]);
            }
            return result;
        }
    }

    public static IconUIResource getTreeFolderIcon() {
        // public, because UIDefaults.ProxyLazyValue uses reflection to get this value
        return treeFolderIconResource.getInstance();
    }

    public static IconUIResource getTreeOpenFolderIcon() {
        // public, because UIDefaults.ProxyLazyValue uses reflection to get this value
        return treeOpenFolderIconResource.getInstance();
    }

    public static IconUIResource getTreeDocumentIcon() {
        // public, because UIDefaults.ProxyLazyValue uses reflection to get this value
        return treeDocumentIconResource.getInstance();
    }

    private static final TreeFolderIconUIResourceSingleton treeFolderIconResource = new TreeFolderIconUIResourceSingleton();

    private static class TreeFolderIconUIResourceSingleton extends RecyclableSingleton<IconUIResource> {
        @Override
        protected IconUIResource getInstance() {
            Image im = OSXFile.getDirectoryIconImage(20);
            return new IconUIResource(new ImageIcon(im));
        }
    }

    private static final TreeOpenFolderIconUIResourceSingleton treeOpenFolderIconResource = new TreeOpenFolderIconUIResourceSingleton();

    private static class TreeOpenFolderIconUIResourceSingleton extends RecyclableSingleton<IconUIResource> {
        @Override
        protected IconUIResource getInstance() {
            return AquaIcon.getOpenFolderIcon();
        }
    }

    private static final TreeDocumentIconUIResourceSingleton treeDocumentIconResource = new TreeDocumentIconUIResourceSingleton();

    private static class TreeDocumentIconUIResourceSingleton extends RecyclableSingleton<IconUIResource> {
        @Override
        protected IconUIResource getInstance() {
            Image im = OSXFile.getFileIconImage(20);
            return new IconUIResource(new ImageIcon(im));
        }
    }

    static class NamedImageSingleton extends RecyclableSingleton<Image> {
        final String namedImage;

        NamedImageSingleton(final String namedImage) {
            this.namedImage = namedImage;
        }

        @Override
        protected Image getInstance() {
            return getNSIcon(namedImage);
        }
    }

    static class IconUIResourceSingleton extends RecyclableSingleton<IconUIResource> {
        final NamedImageSingleton holder;

        public IconUIResourceSingleton(final NamedImageSingleton holder) {
            this.holder = holder;
        }

        @Override
        protected IconUIResource getInstance() {
            return new IconUIResource(new ImageIcon(holder.get()));
        }
    }

    @SuppressWarnings("serial") // Superclass is not serializable across versions
    static class InvertableImageIcon extends ImageIcon implements InvertableIcon, UIResource {
        Icon invertedImage;
        public InvertableImageIcon(final Image image) {
            super(image);
        }

        @Override
        public Icon getInvertedIcon() {
            if (invertedImage != null) return invertedImage;
            return invertedImage = new IconUIResource(new ImageIcon(generateLightenedImage(getImage(), 100)));
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

    static Image getArrowImageForDirection(final int direction) {
        switch(direction) {
            case SwingConstants.NORTH: return northArrow.get();
            case SwingConstants.SOUTH: return southArrow.get();
            case SwingConstants.EAST: return eastArrow.get();
            case SwingConstants.WEST: return westArrow.get();
        }
        return null;
    }

    static Icon getArrowIconForDirection(int direction) {
        switch(direction) {
            case SwingConstants.NORTH: return northArrowIcon.get();
            case SwingConstants.SOUTH: return southArrowIcon.get();
            case SwingConstants.EAST: return eastArrowIcon.get();
            case SwingConstants.WEST: return westArrowIcon.get();
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
        return new InvertableImageIcon(generateLightenedImage(eastArrow.get(), 25));
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

    private static InvertableImageIcon getPopupMenuItemCheckIcon(int size) {
        Image im = getNSImage("NSMenuCheckmark", size, size);
        return new InvertableImageIcon(im);
    }

    public static Icon getMenuItemCheckIcon() {
        return new InvertableImageIcon(generateLightenedImage(getNSIcon("NSMenuCheckmark"), 25));
    }

    public static Icon getMenuItemDashIcon() {
        return new InvertableImageIcon(generateLightenedImage(getNSIcon("NSMenuMixedState"), 25));
    }

    private static Image getNSImage(String imageName, int width, int height) {
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

        public NineSliceMetrics(final int minWidth, final int minHeight, final int westCut, final int eastCut, final int northCut, final int southCut) {
            this(minWidth, minHeight, westCut, eastCut, northCut, southCut, true);
        }

        public NineSliceMetrics(final int minWidth, final int minHeight, final int westCut, final int eastCut, final int northCut, final int southCut, final boolean showMiddle) {
            this(minWidth, minHeight, westCut, eastCut, northCut, southCut, showMiddle, true, true);
        }

        public NineSliceMetrics(final int minWidth, final int minHeight, final int westCut, final int eastCut, final int northCut, final int southCut, final boolean showMiddle, final boolean stretchHorizontally, final boolean stretchVertically) {
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
        final BufferedImage NW, N, NE;
        final BufferedImage W, C, E;
        final BufferedImage SW, S, SE;

        final NineSliceMetrics metrics;

        final int totalWidth, totalHeight;
        final int centerColWidth, centerRowHeight;

        public SlicedImageControl(final Image img, final int westCut, final int eastCut, final int northCut, final int southCut) {
            this(img, westCut, eastCut, northCut, southCut, true);
        }

        public SlicedImageControl(final Image img, final int westCut, final int eastCut, final int northCut, final int southCut, final boolean useMiddle) {
            this(img, westCut, eastCut, northCut, southCut, useMiddle, true, true);
        }

        public SlicedImageControl(final Image img, final int westCut, final int eastCut, final int northCut, final int southCut, final boolean useMiddle, final boolean stretchHorizontally, final boolean stretchVertically) {
            this(img, new NineSliceMetrics(img.getWidth(null), img.getHeight(null), westCut, eastCut, northCut, southCut, useMiddle, stretchHorizontally, stretchVertically));
        }

        public SlicedImageControl(final Image img, final NineSliceMetrics metrics) {
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

        static BufferedImage createSlice(final Image img, final int x, final int y, final int w, final int h) {
            if (w == 0 || h == 0) return null;

            final BufferedImage slice = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB_PRE);
            final Graphics2D g2d = slice.createGraphics();
            g2d.drawImage(img, 0, 0, w, h, x, y, x + w, y + h, null);
            g2d.dispose();

            return slice;
        }

        public void paint(final Graphics g, final int x, final int y, final int w, final int h) {
            g.translate(x, y);

            if (w < totalWidth || h < totalHeight) {
                paintCompressed(g, w, h);
            } else {
                paintStretchedMiddles(g, w, h);
            }

            g.translate(-x, -y);
        }

        void paintStretchedMiddles(final Graphics g, final int w, final int h) {
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

        void paintCompressed(final Graphics g, final int w, final int h) {
            final double heightRatio = h > totalHeight ? 1.0 : (double)h / (double)totalHeight;
            final double widthRatio = w > totalWidth ? 1.0 : (double)w / (double)totalWidth;

            final int northHeight = (int)(metrics.nCut * heightRatio);
            final int southHeight = (int)(metrics.sCut * heightRatio);
            final int centerHeight = h - northHeight - southHeight;

            final int westWidth = (int)(metrics.wCut * widthRatio);
            final int eastWidth = (int)(metrics.eCut * widthRatio);
            final int centerWidth = w - westWidth - eastWidth;

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

    // We cannot change the system colors
    private static Color windowBackgroundColor = new ColorUIResource(236, 236, 236); // new SystemColorProxy(SystemColor.window);
    private static Color desktopBackgroundColor = new ColorUIResource(65, 105, 170); //SystemColor.desktop
    private static Color textSelectionBackgroundColor = new ColorUIResource(179, 215, 255); // new SystemColorProxy(SystemColor.textHighlight);
    private static Color textSelectionForegroundColor = new ColorUIResource(0, 0, 0); // new SystemColorProxy(SystemColor.textHighlightText);
    private static Color selectionBackgroundColor = new ColorUIResource(0, 104, 217); // new SystemColorProxy(SystemColor.controlHighlight);
    private static Color selctionForegroundColor = new ColorUIResource(255, 255, 255); // new SystemColorProxy(SystemColor.controlLtHighlight);
    private static Color comboBoxSelectionBackgroundColor = new ColorUIResource(0, 104, 217);
    private static Color comboBoxSelectionForegroundColor = new ColorUIResource(217, 233, 250);
    private static Color focusRingColor = new Color(62, 156, 246, 128); // new SystemColorProxy(LWCToolkit.getAppleColor(LWCToolkit.KEYBOARD_FOCUS_COLOR));
    private static Color selectionInactiveBackgroundColor = new ColorUIResource(220, 220, 220);
    private static Color selectionInactiveForegroundColor = new ColorUIResource(0, 0, 0); // new SystemColorProxy(LWCToolkit.getAppleColor(LWCToolkit.INACTIVE_SELECTION_FOREGROUND_COLOR));

    public static Color getWindowBackgroundColorUIResource() {
        return windowBackgroundColor;
    }

    public static Color getDesktopBackgroundColorUIResource() {
        return desktopBackgroundColor;
    }

    public static Color getTextSelectionBackgroundColorUIResource() {
        return textSelectionBackgroundColor;
    }

    public static Color getTextSelectionForegroundColorUIResource() {
        return textSelectionForegroundColor;
    }

    public static Color getSelectionBackgroundColorUIResource() {
        return selectionBackgroundColor;
    }

    public static Color getSelectionForegroundColorUIResource() {
        return selctionForegroundColor;
    }

    public static Color getComboBoxSelectionBackgroundColorUIResource() {
        return comboBoxSelectionBackgroundColor;
    }

    public static Color getComboBoxSelectionForegroundColorUIResource() {
        return comboBoxSelectionForegroundColor;
    }

    public static Color getFocusRingColorUIResource() {
        return focusRingColor;
    }

    public static Color getSelectionInactiveBackgroundColorUIResource() {
        return selectionInactiveBackgroundColor;
    }

    public static Color getSelectionInactiveForegroundColorUIResource() {
        return selectionInactiveForegroundColor;
    }

    /**
     * Obtain a native image with a specified logical size.
     */
    private static native Image getNativeImage(String name, int width, int height);

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

    public static Image generateSelectedDarkImage(final Image image) {
        return applyFilter(image, new GenerateSelectedDarkFilter());
    }

    private static class GenerateSelectedDarkFilter extends IconImageFilter {
        @Override
        int getGreyFor(final int gray) {
            return gray * 75 / 100;
        }
    }

    public static Image generatePressedDarkImage(Image image) {
        return applyFilter(image, new GeneratePressedDarkFilter());
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

    public static Image generateDisabledLightImage(Image image) {
        return applyFilter(image, new GenerateDisabledLightFilter());
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

    public static Image generateLightenedImage(final Image image, final int percent) {
        final GrayFilter filter = new GrayFilter(true, percent);
        return AquaMultiResolutionImage.apply(image, filter);
    }

    private abstract static class IconImageFilter extends RGBImageFilter {
        IconImageFilter() {
            super();
            canFilterIndexColorModel = true;
        }

        @Override
        public final int filterRGB(final int x, final int y, final int rgb) {
            final int red = (rgb >> 16) & 0xff;
            final int green = (rgb >> 8) & 0xff;
            final int blue = rgb & 0xff;
            final int gray = getGreyFor((int) ((0.30 * red + 0.59 * green + 0.11 * blue) / 3));

            return (rgb & 0xff000000) | (grayTransform(red, gray) << 16) | (grayTransform(green, gray) << 8) | (grayTransform(blue, gray) << 0);
        }

        private static int grayTransform(final int color, final int gray) {
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
    public static boolean isTemplateImage(Image image) {
        // Run the template filter in a special way that allows us to observe its behavior.
        TemplateFilter filter = new TemplateFilter(Color.BLACK, true);
        Image im = applyFilter(image, filter);
        // force the image to be created
        new ImageIcon(im);
        return filter.isTemplate();
    }

    /**
     * Create an image by replacing the visible pixels in a template image with the specified color. A template image
     * is an image that contains only clear or (possibly translucent) black pixels.
     * @param image The template image.
     * @param replacementColor The replacement color.
     * @return the new image, or null if the source image is not a template image.
     */
    public static Image createImageFromTemplate(Image image, Color replacementColor) {
        return applyFilter(image, new TemplateFilter(replacementColor, false));
    }

    public static Image applyFilter(Image image, ImageFilter filter) {
        return Aqua8MultiResolutionImage.apply(image, filter);
    }

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
}
