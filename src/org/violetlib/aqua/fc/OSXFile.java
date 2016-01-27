/*
 * @(#)OSXFile.java
 *
 * Copyright (c) 2009-2013 Werner Randelshofer, Switzerland.
 * Copyright (c) 2014-2016 Alan Snyder.
 * You may not use, copy or modify this file, except in compliance with the
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.*;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import javax.swing.*;
import javax.swing.plaf.IconUIResource;

import org.violetlib.aqua.*;
import org.violetlib.aqua.AquaUtils.RecyclableSingleton;

/**
 * Provides access to Mac OS X file meta data and can resolve file aliases.
 *
 * @author Werner Randelshofer
 * @version $Id$
 */
public class OSXFile {

    public final static int FILE_TYPE_ALIAS = 2;
    public final static int FILE_TYPE_DIRECTORY = 1;
    public final static int FILE_TYPE_FILE = 0;
    public final static int FILE_TYPE_UNKNOWN = -1;

    /**
     * This array holds the colors used for drawing the gradients of a file
     * label.
     */
    private static volatile Color[][] labelColors;

    private static Icon computerIcon;
    private static String computerModel;
    private static boolean computerModelInitialized;
    private static Icon computerSidebarIcon;

    private static final int kLSItemInfoIsPlainFile        = 0x00000001; /* Not a directory, volume, or symlink*/
    private static final int kLSItemInfoIsPackage          = 0x00000002; /* Packaged directory*/
    private static final int kLSItemInfoIsApplication      = 0x00000004; /* Single-file or packaged application*/
    private static final int kLSItemInfoIsContainer        = 0x00000008; /* Directory (includes packages) or volume*/
    private static final int kLSItemInfoIsAliasFile        = 0x00000010; /* Alias file (includes sym links)*/
    private static final int kLSItemInfoIsSymlink          = 0x00000020; /* UNIX sym link*/
    private static final int kLSItemInfoIsInvisible        = 0x00000040; /* Invisible by any known mechanism*/
    private static final int kLSItemInfoIsNativeApp        = 0x00000080; /* Carbon or Cocoa native app*/
    private static final int kLSItemInfoIsClassicApp       = 0x00000100; /* CFM/68K Classic app*/
    private static final int kLSItemInfoAppPrefersNative   = 0x00000200; /* Carbon app that prefers to be launched natively*/
    private static final int kLSItemInfoAppPrefersClassic  = 0x00000400; /* Carbon app that prefers to be launched in Classic*/
    private static final int kLSItemInfoAppIsScriptable    = 0x00000800; /* App can be scripted*/
    private static final int kLSItemInfoIsVolume           = 0x00001000; /* Item is a volume*/
    private static final int kLSItemInfoExtensionIsHidden  = 0x00100000; /* Item has a hidden extension*/

    private static final RecyclableFileIcon aliasBadgeIcon;
    private static final RecyclableFileIcon genericComputerIcon;
    private static final RecyclableFileIcon directoryIcon;
    private static final RecyclableFileIcon fileIcon;
    private static final RecyclableFileIcon networkIcon;

    private static final RecyclableSidebarIcon airDropSidebarIcon;
    private static final RecyclableSidebarIcon allMyFilesSidebarIcon;
    private static final RecyclableSidebarIcon applicationsSidebarIcon;
    private static final RecyclableSidebarIcon desktopSidebarIcon;
    private static final RecyclableSidebarIcon documentsSidebarIcon;
    private static final RecyclableSidebarIcon downloadsSidebarIcon;
    private static final RecyclableSidebarIcon dropboxSidebarIcon;
    private static final RecyclableSidebarIcon genericFileSidebarIcon;
    private static final RecyclableSidebarIcon genericFolderSidebarIcon;
    private static final RecyclableSidebarIcon homeSidebarIcon;
    private static final RecyclableSidebarIcon moviesSidebarIcon;
    private static final RecyclableSidebarIcon musicSidebarIcon;
    private static final RecyclableSidebarIcon networkSidebarIcon;
    private static final RecyclableSidebarIcon picturesSidebarIcon;
    private static final RecyclableSidebarIcon smartFolderSidebarIcon;
    private static final RecyclableSidebarIcon timeMachineSidebarIcon;
    private static final RecyclableSidebarIcon utilitiesSidebarIcon;
    private static final RecyclableSidebarIcon genericVolumeSidebarIcon;

    private static final RecyclableSidebarIcon iMacSidebarIcon;
    private static final RecyclableSidebarIcon macMiniSidebarIcon;
    private static final RecyclableSidebarIcon macProSidebarIcon;
    private static final RecyclableSidebarIcon macProCylinderSidebarIcon;
    private static final RecyclableSidebarIcon pcSidebarIcon;
    private static final RecyclableSidebarIcon laptopSidebarIcon;

    private static class RecyclableFileIcon extends RecyclableSingleton<IconUIResource> {

        final File file;

        public RecyclableFileIcon(File file) {
            this.file = file;
        }

        public RecyclableFileIcon(String path) {
            this.file = new File(path);
        }

        protected IconUIResource getInstance() {
            Image im = AquaImageFactory.getImage(file, 16);
            return new IconUIResource(new ImageIcon(im));
        }

        public Image asImage(int size) {
            return AquaImageFactory.getImage(file, size);
        }
    }

    private static class RecyclableSidebarIcon extends RecyclableSingleton<Icon> {

        final File file;

        public RecyclableSidebarIcon(File file) {
            this.file = file;
        }

        public RecyclableSidebarIcon(String path) {
            this.file = new File(path);
        }

        @Override
        protected Icon getInstance() {
            Image im = AquaImageFactory.getImage(file, 18);
            return createSidebarIcon(im);
        }
    }

    private static Icon createSidebarIcon(Image im) {
        Color basicColor = UIManager.getColor("Tree.sideBar.foreground");
        Color selectedColor = UIManager.getColor("Tree.sideBar.selectionForeground");
        Image basic = createSidebarImage(im, basicColor);
        Image selected = createSidebarImage(im, selectedColor);
        return new ListStateIcon(new ImageIcon(basic), new ImageIcon(selected));
    }

    /**
     * Create a sidebar icon image for Yosemite. Light areas remain transparent. Dark areas are mapped to the specified
     * color.
     */
    protected static Image createSidebarImage(Image source, Color color) {
        if (source instanceof AquaMultiResolutionImage) {
            AquaMultiResolutionImage im = (AquaMultiResolutionImage) source;
            SidebarImageMapper mapper = new SidebarImageMapper(color);
            return im.map(mapper);
        }

        BufferedImage img = Images.toBufferedImage(source);
        return basicCreateSidebarImage(img, color);
    }

    private static class SidebarImageMapper implements AquaMultiResolutionImage.Mapper {
        private final Color color;

        public SidebarImageMapper(Color color) {
            this.color = color;
        }

        @Override
        public Image map(Image source, int scaleFactor) {
            BufferedImage img = Images.toBufferedImage(source);
            return basicCreateSidebarImage(img, color);
        }
    }

    /**
     * Create a sidebar icon image for Yosemite. Light areas remain transparent. Dark areas are mapped to the specified
     * color.
     */
    protected static Image basicCreateSidebarImage(BufferedImage img, Color color) {
        int width = img.getWidth();
        int height = img.getHeight();
        BufferedImage iconImg = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB_PRE);
        Graphics2D g = iconImg.createGraphics();
        g.setComposite(AlphaComposite.Src);
        g.drawImage(img, 0, 0, null);
        g.setComposite(AlphaComposite.SrcIn);
        g.setColor(color);
        g.fillRect(0, 0, width, height);
        g.dispose();
        return iconImg;
    }

    private static RecyclableFileIcon createIcon(String name) {
        String prefix = "/System/Library/CoreServices/CoreTypes.bundle/Contents/Resources/";
        return new RecyclableFileIcon(prefix + name + ".icns");
    }

    private static RecyclableSidebarIcon createSidebarIcon(String name) {
        String prefix = "/System/Library/CoreServices/CoreTypes.bundle/Contents/Resources/Sidebar";
        RecyclableSidebarIcon icon = new RecyclableSidebarIcon(prefix + name + ".icns");
        return icon;
    }

    static {
        aliasBadgeIcon = createIcon("AliasBadgeIcon");
        genericComputerIcon = createIcon("public.generic-pc");
        directoryIcon = createIcon("GenericFolderIcon");
        fileIcon = createIcon("GenericDocumentIcon");
        networkIcon = createIcon("GenericNetworkIcon");

        applicationsSidebarIcon = createSidebarIcon("ApplicationsFolder");
        desktopSidebarIcon = createSidebarIcon("DesktopFolder");
        documentsSidebarIcon = createSidebarIcon("DocumentsFolder");
        downloadsSidebarIcon = createSidebarIcon("DownloadsFolder");
        dropboxSidebarIcon = createSidebarIcon("DropBoxFolder");
        genericFileSidebarIcon = createSidebarIcon("GenericFile");
        genericFolderSidebarIcon = createSidebarIcon("GenericFolder");
        genericVolumeSidebarIcon = createSidebarIcon("InternalDisk");
        homeSidebarIcon = createSidebarIcon("HomeFolder");
        moviesSidebarIcon = createSidebarIcon("MoviesFolder");
        musicSidebarIcon = createSidebarIcon("MusicFolder");
        networkSidebarIcon = createSidebarIcon("Network");
        picturesSidebarIcon = createSidebarIcon("PicturesFolder");
        smartFolderSidebarIcon = createSidebarIcon("SmartFolder");
        timeMachineSidebarIcon = createSidebarIcon("TimeMachine");
        utilitiesSidebarIcon = createSidebarIcon("UtilitiesFolder");
        iMacSidebarIcon = createSidebarIcon("iMac");
        macMiniSidebarIcon = createSidebarIcon("MacMini");
        macProSidebarIcon = createSidebarIcon("MacPro");
        macProCylinderSidebarIcon = createSidebarIcon("MacProCylinder");
        pcSidebarIcon = createSidebarIcon("PC");
        laptopSidebarIcon = createSidebarIcon("Laptop");
        airDropSidebarIcon = createSidebarIcon("AirDrop");
        allMyFilesSidebarIcon = createSidebarIcon("AllMyFiles");
    }

    public static Icon getAliasBadgeIcon() {
        return aliasBadgeIcon.getInstance();
    }

    public static Icon getDirectoryIcon() {
        return directoryIcon.getInstance();
    }

    public static Image getDirectoryIconImage(int size) {
        return directoryIcon.asImage(size);
    }

    public static Icon getFileIcon() {
        return fileIcon.getInstance();
    }

    public static Image getFileIconImage(int size) {
        return fileIcon.asImage(size);
    }

    public static Icon getGenericComputerIcon() {
        return genericComputerIcon.getInstance();
    }

    public static Icon getNetworkIcon() {
        return networkIcon.getInstance();
    }

    public static Icon getAirDropSidebarIcon() {
        return airDropSidebarIcon.getInstance();
    }

    public static Icon getAllMyFilesSidebarIcon() {
        return allMyFilesSidebarIcon.getInstance();
    }

    public static Icon getApplicationsSidebarIcon() {
        return applicationsSidebarIcon.getInstance();
    }

    public static Icon getDesktopSidebarIcon() {
        return desktopSidebarIcon.getInstance();
    }

    public static Icon getDocumentsSidebarIcon() {
        return documentsSidebarIcon.getInstance();
    }

    public static Icon getDownloadsSidebarIcon() {
        return downloadsSidebarIcon.getInstance();
    }

    public static Icon getDropboxSidebarIcon() {
        return dropboxSidebarIcon.getInstance();
    }

    public static Icon getGenericFileSidebarIcon() {
        return genericFileSidebarIcon.getInstance();
    }

    public static Icon getGenericFolderSidebarIcon() {
        return genericFolderSidebarIcon.getInstance();
    }

    public static Icon getGenericVolumeSidebarIcon() {
        return genericVolumeSidebarIcon.getInstance();
    }

    public static Icon getHomeSidebarIcon() {
        return homeSidebarIcon.getInstance();
    }

    public static Icon getMoviesSidebarIcon() {
        return moviesSidebarIcon.getInstance();
    }

    public static Icon getMusicSidebarIcon() {
        return musicSidebarIcon.getInstance();
    }

    public static Icon getNetworkSidebarIcon() {
        return networkSidebarIcon.getInstance();
    }

    public static Icon getPicturesSidebarIcon() {
        return picturesSidebarIcon.getInstance();
    }

    public static Icon getSmartFolderSidebarIcon() {
        return smartFolderSidebarIcon.getInstance();
    }

    public static Icon getTimeMachineSidebarIcon() {
        return timeMachineSidebarIcon.getInstance();
    }

    public static Icon getUtilitiesSidebarIcon() {
        return utilitiesSidebarIcon.getInstance();
    }

    public static Icon getiMacSidebarIcon() {
        return iMacSidebarIcon.getInstance();
    }

    public static Icon getMacMiniSidebarIcon() {
        return macMiniSidebarIcon.getInstance();
    }

    public static Icon getMacProSidebarIcon() {
        return macProSidebarIcon.getInstance();
    }

    public static Icon getMacProCylinderSidebarIcon() {
        return macProCylinderSidebarIcon.getInstance();
    }

    public static Icon getLaptopSidebarIcon() {
        return laptopSidebarIcon.getInstance();
    }

    public static Icon getPCSidebarIcon() {
        return pcSidebarIcon.getInstance();
    }

    /**
     * Returns true if native code is available.
     * This method also loads the native code.
     */
    private static boolean isNativeCodeAvailable() {
        return AquaNativeSupport.load();
    }

    /** Prevent instance creation. */
    private OSXFile() {
    }

    /**
     * Converts the path name denoted by the file to an absolute path.
     * Relative paths are always resolved against the home directory of the
     * user and not against the current user.dir directory.
     * The returned file objects represents an absolute path containing no
     * '.' and '..' relative path components.
     * This method acts solely on the textual representation of the file and
     * therefore does does not necessarily canonicalize the path nor does it
     * resolve aliases.
     *
     * @param f The file which we must ensure contains an absolute path.
     */
    public static File getAbsoluteFile(File f) {
        if (!f.isAbsolute()) {
            f = new File(AquaUtils.getProperty("user.home") + File.separatorChar + f.getPath());

        } // Windows does not support relative path segments, so we quit here
        if (File.separatorChar == '\\') {
            return f;

        } // The following code assumes that absolute paths start with a
        // File.separatorChar.
        StringBuffer buf = new StringBuffer(f.getPath().length());

        int skip = 0;

        for (File i = f; i
                != null; i = i.getParentFile()) {
            String name = i.getName();
            if (name.equals(".")) {
                if (skip > 0) {
                    skip--;
                }
            } else if (name.equals("..")) {
                skip++;
            } else {
                if (skip > 0) {
                    skip--;
                } else {
                    buf.insert(0, name);
                    buf.insert(0, File.separatorChar);
                }
            }
        }

        return f.getPath().equals(buf.toString()) ? f : new File(buf.toString());
    }

    /**
     * Returns true if native support is available.
     */
    public static boolean isAvailable() {
        return isNativeCodeAvailable();
    }

    /**
     * Returns the file type: 0=file, 1=directory, 2=alias, -1=unknown.
     */
    public static int getFileType(File f) {
        if (isNativeCodeAvailable()) {
            int flags = nativeGetBasicItemInfoFlags(f.getAbsolutePath());

            if ((flags & kLSItemInfoIsAliasFile) != 0) {

                /*
                  Special case: the program wants to see volumes as directories.
                */

                if (f.getParent().equals("/Volumes")) {
                    return FILE_TYPE_DIRECTORY;
                }

                return FILE_TYPE_ALIAS;
            }

            if ((flags & kLSItemInfoIsContainer) != 0) {
                return FILE_TYPE_DIRECTORY;
            }

            if ((flags & kLSItemInfoIsPlainFile) != 0) {
                return FILE_TYPE_FILE;
            }

            return FILE_TYPE_UNKNOWN;
        } else {
            return (f.isDirectory()) ? FILE_TYPE_DIRECTORY : ((f.isFile()) ? FILE_TYPE_FILE : FILE_TYPE_UNKNOWN);
        }
    }

    /**
     * Resolve a path that may be absolute, relative, or start with ~user.
     */
    public static File resolvePath(String path, File currentDirectory) {
        if (path.startsWith("/")) {
            return new File(path);
        }

        if (path.startsWith("~")) {
            path = path.substring(1);
            if (path.startsWith("/")) {
                String home = System.getProperty("user.home");
                return new File(home + path);
            } else if (path.isEmpty()) {
                String home = System.getProperty("user.home");
                return new File(home);
            } else {
                return new File("/Users/" + path);
            }
        }

        if (currentDirectory == null) {
            String home = System.getProperty("user.home");
            currentDirectory = new File(home);
        }

        return new File(currentDirectory, path);
    }

    /**
     * Resolve a file by converting all valid aliases in the path.
     */

    public static File resolve(File f) {
        return resolveAlias(f, true);
    }

    /**
     * Resolve a file by converting all valid aliases in the path.
     *
     * @param f The file to be resolved.
     * @param noUI Set this to true, if the alias should be resolved without user interaction.
     * @return Returns the resolved File object.
     */
    public static File resolveAlias(File f, boolean noUI) {
        if (isNativeCodeAvailable()) {

            String path = nativeResolveAlias(f.getAbsolutePath(), noUI);

            if (path == null) {
                return null;
            }

            f = new File(path);

            /*
              Cocoa path resolution refuses to follow certain top level links. If the result is an alias, we may have
              one of those top level links. The JDK support for canonical files works better!
            */

            if (getFileType(f) != FILE_TYPE_ALIAS) {
                return f;
            }
        }

        try {
            return f.getCanonicalFile();
        } catch (IOException ex) {
            return f;
        }
    }

    /**
     * Resolves a serialized Alias to a File object.
     * @return A File or null, if the serialized Alias could not be
     * resolved.
     * @param noUI Set this to true, if the alias should
     * be resolved without user interaction.
     */
    public static File resolveAlias(byte[] serializedAlias, boolean noUI) {
        if (isNativeCodeAvailable()) {
            String path = nativeResolveAlias(serializedAlias, noUI);
            return (path == null) ? null : new File(path);
        } else {
            return null;
        }
    }

    /**
     * Returns the label of the specified file.
     * The label is a value in the interval from 0 through 7.
     * Returns -1 if the label could not be determined, e.g. if the file does
     * not exist.
     */
    public static int getLabel(File f) {
        if (isNativeCodeAvailable() && f != null) {
            return nativeGetLabel(f.getAbsolutePath());
        } else {
            return -1;
        }
    }

    /**
     * Returns the color of the specified label. Returns null, if the label
     * does not have a color.
     *
     * @param label value from 0 through 7
     * @param type 0=dark enabled,1=bright enabld,2=dark disabled,3=bright enabled
     */
    public static Color getLabelColor(int label, int type) {
        if (labelColors == null) {
            synchronized (OSXFile.class) {
                if (labelColors  == null) {
                    labelColors = new Color[][]{
                            // dark, bright, dark disabled, bright disabled
                            {null, null, null, null}, // no label
                            {new Color(0xb7b7b7), new Color(0xd8d8d8), new Color(0xe9e9e9), new Color(0xf3f3f3)}, // gray
                            {new Color(0xc1d95e), new Color(0xdeebac), new Color(0xecf5ce), new Color(0xf5fae6)}, // green
                            {new Color(0xcba3df), new Color(0xe7cdee), new Color(0xf0e2f6), new Color(0xf7f0fa)}, // purple
                            {new Color(0x6db5fd), new Color(0xb8dbfe), new Color(0xd1e8ff), new Color(0xe9f4ff)}, // blue
                            {new Color(0xf2dd60), new Color(0xfbf2ac), new Color(0xfcf6ce), new Color(0xfefce6)}, // yellow
                            {new Color(0xfb7a70), new Color(0xfcb4ad), new Color(0xffd6d3), new Color(0xffe8e6)}, // red
                            {new Color(0xf7b65b), new Color(0xfbd6a4), new Color(0xfee9cc), new Color(0xfff3e3)} // orange
                    };
                }
            }
        }

        return (label == -1) ? null : labelColors[label][type];
    }

    /**
     * Return the icon image for a file.
     * @param file The file.
     * @param size The desired icon size.
     * @param useQuickLook If true, an icon based on the Quick Look preview will be returned. If false, the Launch
     * Services icon for the file wil be returned. Note that obtaining a Quick Look preview image can take a long time.
     * @throws UnsupportedOperationException if an image cannot be obtained.
     */
    public static Image getIconImage(File file, int size, boolean useQuickLook) throws UnsupportedOperationException {
        if (isNativeCodeAvailable() && file != null) {
            FileIconCreator c = new FileIconCreator(file, size, useQuickLook, true);
            return c.getImage();
        }

        // TBD: return a default image
        throw new UnsupportedOperationException();
    }

    /**
     * Return the Quick Look thumbnail image for a file.
     * @param file The file.
     * @param size The desired image size.
     * @throws UnsupportedOperationException if an image cannot be obtained.
     */
    public static Image getThumbnailImage(File file, int size) throws UnsupportedOperationException {
        if (isNativeCodeAvailable() && file != null) {
            FileIconCreator c = new FileIconCreator(file, size, true, false);
            return c.getImage();
        }

        throw new UnsupportedOperationException();
    }

    /**
     * Render the system icon for a file.
     * This renderer should be invoked on a background thread.
     */
    private static class FileIconCreator {
        private final File file;
        private final int size;
        private final boolean useQuickLook;
        private final boolean useIconMode;
        private Image result;

        public FileIconCreator(File file, int size, boolean useQuickLook, boolean useIconMode) {
            this.file = file;
            this.size = size;
            this.useQuickLook = useQuickLook;
            this.useIconMode = useIconMode;
        }

        public Image getImage() {
            if (result == null) {
                String path = file.getAbsolutePath();
                int[][] buffers = new int[2][];
                if (!nativeRenderFileImage(path, useQuickLook, useIconMode, buffers, size, size)) {
                    if (AquaImageFactory.debugNativeRendering) {
                        System.err.println("Failed to render image for " + path);
                    }
                    throw new UnsupportedOperationException();
                }

                if (AquaImageFactory.debugNativeRendering) {
                    System.err.println("Rendered image for " + path);
                }
                result = AquaMultiResolutionImage.createImage(size, size, buffers[0], buffers[1]);
            }
            return result;
        }
    }

    /**
     * Returns the kind string of the specified file. The description is
     * localized in the current Locale of the Finder.
     *
     * @return The kind or null, if it couldn't be determined.
     */
    public static String getKindString(File file) {
        if (isNativeCodeAvailable() && file != null) {
            return nativeGetKindString(file.getAbsolutePath());
        } else {
            return null;
        }
    }

    /**
     * Indicate whether the specified file is a directory that is normally viewed by the user as an ordinary file,
     * i.e., something that can be opened in an application.
     */
    public static boolean isVirtualFile(File file) {
        if (isNativeCodeAvailable() && file != null) {
            int flags = nativeGetBasicItemInfoFlags(file.getAbsolutePath());
            return (flags & kLSItemInfoIsPackage) != 0;
        } else {
            return false;
        }
    }

    public static boolean isInvisible(File file) {
        if (file != null) {
            if (isNativeCodeAvailable()) {
                int flags = nativeGetBasicItemInfoFlags(file.getAbsolutePath());
                return (flags & kLSItemInfoIsInvisible) != 0;
            } else {
                return file.isHidden();
            }
        }
        return false;
    }

    public static boolean isTraversable(File file) {
        return isTraversable(file, false, false);
    }

    public static boolean isTraversable(File file, boolean isPackageTraversable, boolean isApplicationTraversable) {
        if (file == null) {
            return false;

        } else if (isNativeCodeAvailable()) {
            int flags = nativeGetBasicItemInfoFlags(file.getAbsolutePath());

            if ((flags & kLSItemInfoIsAliasFile) != 0) {
                file = resolve(file);
                flags = nativeGetBasicItemInfoFlags(file.getAbsolutePath());
            }

            if ((flags & (kLSItemInfoIsPlainFile | kLSItemInfoIsContainer)) == kLSItemInfoIsContainer) {
                // The file is a container (a volume or directory).
                boolean isPackage = (flags & kLSItemInfoIsPackage) != 0;
                boolean isApplication = (flags & kLSItemInfoIsApplication) != 0;
                // All applications are packages, so the application option takes priority for applications.
                if (isApplication) {
                    return isApplicationTraversable;
                }
                if (isPackage) {
                    return isPackageTraversable;
                }
                return basicIsTraversable(file);

            } else if ((flags & kLSItemInfoIsPlainFile) != 0) {
                return basicIsTraversable(file);

            } else if (isVolumes(file.getParent())) {

                    /*
                      Special case: the program wants to see volumes as directories.
                    */

                    return true;

            } else {
                return false;
            }
        } else {
            return basicIsTraversable(file);
        }
    }

    private static boolean isVolumes(String s) {
        return s != null && s.equals("/Volumes");
    }

    private static String[] nonTraversableDirectories = { ".Spotlight-V100", ".DocumentRevisions", ".Trashes" };

    private static boolean basicIsTraversable(File f) {
        String name = f.getName();

        if (f.isDirectory()) {
            for (String s : nonTraversableDirectories) {
                if (s.equals(name)) {
                    return false;
                }
            }

            return true;

        } else if (isSavedSearch(f)) {
           return true;
        }

        return false;
    }

    public static boolean isSavedSearch(File f) {
        return f.getName().endsWith(".savedSearch");
    }

    public static Icon getComputerIcon() {
        if (computerIcon == null) {

            Image im = Toolkit.getDefaultToolkit().getImage("NSImage://NSComputer");
            if (im != null) {
                int height = im.getHeight(null);
                if (height > 0) {
                    BufferedImage b = Images.toBufferedImage(im);
                    im = AquaMultiResolutionImage.createImage2x(b);
                    return computerIcon = new ImageIcon(im);
                }
            }

            String model = getComputerModel();
            if (model != null) {
                if (model.startsWith("MacBookAir")) {
                    computerIcon = UIManager.getIcon("FileView.macbookAirIcon");
                } else if (model.startsWith("MacBookPro")) {
                    computerIcon = UIManager.getIcon("FileView.macbookProIcon");
                } else if (model.startsWith("MacBook")) {
                    computerIcon = UIManager.getIcon("FileView.macbookIcon");
                } else if (model.startsWith("MacPro")) {
                    computerIcon = UIManager.getIcon("FileView.macproIcon");
                } else if (model.startsWith("Macmini")) {
                    computerIcon = UIManager.getIcon("FileView.macminiIcon");
                } else if (model.startsWith("iMac")) {
                    computerIcon = UIManager.getIcon("FileView.imacIcon");
                }
            }

            if (computerIcon == null) {
                computerIcon = UIManager.getIcon("FileView.imacIcon");
            }

            if (computerIcon == null) {
                computerIcon = UIManager.getIcon("FileView.computerIcon");
            }
        }
        return computerIcon;
    }

    public static Icon getSidebarComputerIcon() {
        if (computerSidebarIcon == null) {
            String model = getComputerModel();
            if (model != null) {
                if (model.startsWith("MacBook")) {
                    computerSidebarIcon = getLaptopSidebarIcon();
                } else if (model.startsWith("MacPro")) {
                    computerSidebarIcon = getMacProSidebarIcon();
                } else if (model.startsWith("Macmini")) {
                    computerSidebarIcon = getMacMiniSidebarIcon();
                }
            }

            if (computerSidebarIcon == null) {
                computerSidebarIcon = getiMacSidebarIcon();
            }
        }

        return computerSidebarIcon;
    }

    private static String getComputerModel() {
        if (!computerModelInitialized) {
            computerModelInitialized = true;
            String[] cmd = { "/usr/sbin/sysctl", "hw.model" };
            Charset cs = Charset.forName("UTF-8");
            String result = exec(cmd, cs);
            if (result != null) {
                int pos = result.indexOf(":");
                computerModel = result.substring(pos+1).trim();
            }
        }
        return computerModel;
    }

    /**
     * Returns the file type: 0=file, 1=directory, 2=alias, -1=unknown.
     */
    private static native int nativeGetFileType(String path);

    /**
     * Resolves an alias to a path String.
     * Returns the same path if the provided path is not an alias.
     *
     * @param aliasPath the path to the alias to be resolved.
     * @param noUI Set this to true, if the alias should
     * be resolved without user interaction.
     * @return Returns the resolved path. Returns null, if the resolution failed.
     */
    private static native String nativeResolveAlias(String aliasPath, boolean noUI);

    /**
     * Resolves a serialized Alias to a path String.
     * Returns null if the resolution failed.
     *
     * @param serializedAlias the alias to be resolved.
     * @param noUI Set this to true, if the alias should
     * be resolved without user interaction.
     * @return Returns the resolved path.
     */
    private static native String nativeResolveAlias(byte[] serializedAlias, boolean noUI);

    /**
     * Returns the label of the file specified by the given path.
     * The label is a value in the interval from 0 through 7.
     * Returns -1 if the label could not be determined, e.g. if the file does
     * not exist.
     *
     * @param path the path to the file.
     */
    private static native int nativeGetLabel(String path);

    /**
     * Returns the kind of the file specified by the given path.
     * The kind is localized in the current locale of the Finder.
     *
     * @param path the path to the file.
     */
    private static native String nativeGetKindString(String path);

    /**
     * Returns the basic item-information flags of the file specified by the given path.
     * <p>
     * Requests all item-information flags that are not application-specific:
     * that is, all except kLSItemInfoIsNativeApp through kLSItemInfoAppIsScriptable.
     * <p>
     * The item-information flags can have the following values:
     * <pre>
     *
     *  Item-Information Flags
     *
     *  typedef OptionBits LSItemInfoFlags;enum {
     *  kLSItemInfoIsPlainFile = 0x00000001,
     *  kLSItemInfoIsPackage = 0x00000002,
     *  kLSItemInfoIsApplication = 0x00000004,
     *  kLSItemInfoIsContainer = 0x00000008,
     *  kLSItemInfoIsAliasFile = 0x00000010,
     *  kLSItemInfoIsSymlink = 0x00000020,
     *  kLSItemInfoIsInvisible = 0x00000040,
     *
     *  kLSItemInfoIsNativeApp = 0x00000080,
     *  kLSItemInfoIsClassicApp = 0x00000100,
     *  kLSItemInfoAppPrefersNative = 0x00000200,
     *  kLSItemInfoAppPrefersClassic = 0x00000400,
     *  kLSItemInfoAppIsScriptable = 0x00000800,
     *
     *  kLSItemInfoIsVolume = 0x00001000,
     *  kLSItemInfoExtensionIsHidden = 0x00100000
     *  };
     * </pre>
     *
     * For more information see
     * http://developer.apple.com/documentation/Carbon/Reference/LaunchServicesReference/Reference/reference.html#//apple_ref/c/tdef/LSItemInfoFlags
     *
     * @param path the path to the file.
     */
    private static native int nativeGetBasicItemInfoFlags(String path);

    private static String computerName;
    private static boolean haveFetchedComputerName;

    /**
     * Returns the display name of the computer.
     */
    public static String getComputerName() {
        if (!haveFetchedComputerName) {
            haveFetchedComputerName = true;
            String[] cmd = { "/usr/sbin/scutil", "--get", "ComputerName" };
            Charset cs = Charset.forName("UTF-8");
            String s = exec(cmd, cs);
            computerName = s != null ? s.trim() : null;
        }
        return computerName;
    }

    /**
     * Returns the localized display name of the specified file.
     */
    public static String getDisplayName(File f) {
        if (isNativeCodeAvailable()) {
            return nativeGetDisplayName(f.getAbsolutePath());
        } else {
            return f.getName();
        }
    }

    /**
     * Return the time of last use of a file, as recorded by Launch Services. Called the "Date Last Opened" by Finder.
     */
    public static Date getLastUsedDate(File f) {
        if (isNativeCodeAvailable()) {
            long t = nativeGetLastUsedDate(f.getAbsolutePath());
            return t > 0 ? new Date(t) : null;
        } else {
            return null;
        }
    }

    /**
     * Perform a saved search.
     *
     * @param savedSearchFile The .savedSearch file that defines the query.
     * @return an array containing the files that satisfy the query or null if the query could not be performed.
     */
    public static File[] executedSavedSearch(File savedSearchFile) {
        String savedSearchPath = savedSearchFile.getAbsolutePath();
        String[] paths = nativeExecuteSavedSearch(savedSearchPath);
        if (paths != null) {
            int count = paths.length;
            File[] files = new File[count];
            for (int i = 0; i < count; i++) {
                files[i] = new File(paths[i]);
            }
            return files;
        }
        return null;
    }

    public static class SystemItemInfo {
        public String name;
        public String path;
        int sequenceNumber;
        boolean isVisible;
        int id; // volume ID for sidebar volumes
        Icon icon;

        @Override
        public String toString() {
            String s = sequenceNumber + ": " + name;
            if (path != null) {
                s += " " + path;
            } else {
                s += " (unresolved)";
            }
            s += " " + id;
            if (!isVisible) {
                s += " (hidden)";
            }
            return s;
        }
    }

    private static class SharedFileList {
        private String name;
        private int which;
        private int lastSeed = -1;
        private List<SystemItemInfo> lastResults;

        public SharedFileList(int which, String name) {
            this.which = which;
            this.name = name;
        }

        public List<SystemItemInfo> getResults() {
            int imageSize = 18;
            Object[] data = nativeGetSidebarFiles(which, 18, lastSeed);
            if (data == null) {
                return lastResults;
            }
            lastSeed = (Integer) data[0];
            // debug
            //System.err.println("Updating " + name + " #" + lastSeed);
            int sequence = 0;
            List<SystemItemInfo> result = new ArrayList<>();
            for (int i = 1; i < data.length; i += 6) {
                String name = (String) data[i];
                if (name != null) {
                    Integer id = (Integer) data[i+1];
                    Integer flags = (Integer) data[i+2];
                    String path = (String) data[i+3];
                    int[] iconData1x = (int[]) data[i+4];
                    int[] iconData2x = (int[]) data[i+5];
                    SystemItemInfo info = new SystemItemInfo();
                    info.name = name;
                    info.path = path;
                    info.sequenceNumber = sequence++;
                    info.isVisible = flags == 0;
                    info.id = id;
                    if (iconData1x != null) {
                        info.icon = createSidebarIcon(AquaMultiResolutionImage.createImage(imageSize, imageSize, iconData1x, iconData2x));
                    }
                    result.add(info);
                }
            }
            lastResults = result;
            return result;
        }
    }

    public static final int SIDEBAR_FAVORITES = 0;
    public static final int SIDEBAR_VOLUMES = 1;

    private static final SharedFileList[] sharedFileLists = new SharedFileList[] {
        new SharedFileList(0, "Sidebar Favorites"),
        new SharedFileList(1, "Sidebar Volumes")
    };

    /**
     * Return the list of sidebar favorites or volumes.
     * @param which This parameter selects favorites or volumes.
     * @return the requested list.
     */
    public static List<SystemItemInfo> getSidebarFiles(int which) {
        return sharedFileLists[which].getResults();
    }

    public static String exec(String[] cmd, Charset cs) {
        try {
            Process p = Runtime.getRuntime().exec(cmd);
            InputStream stdout = p.getInputStream();
            InputStreamReader standardOutputReader = new InputStreamReader(stdout, cs);
            StreamCollector standardOutputCollector = new StreamCollector(standardOutputReader);
            standardOutputCollector.start();
            int rc = p.waitFor();
            standardOutputCollector.join();
            return standardOutputCollector.getContents();
        } catch (IOException ex) {
        } catch (InterruptedException ex) {
        }
        return null;
    }

    /**
     * Collect the contents of a string using a worker thread. Notify when done.
     */
    private static class StreamCollector extends Thread {
        private BufferedReader br;
        private String result;

        public StreamCollector(Reader r) {
            br = new BufferedReader(r);
        }

        /**
         * Return the collected contents, or null if not ready yet.
         */
        public synchronized String getContents() {
            return result;
        }

        private synchronized void setContents(String s) {
            result = s;
            notifyAll();
        }

        public void run() {
            StringBuilder sb = new StringBuilder();
            char[] buffer = new char[1000];
            try {
                for (;;) {
                    int count = br.read(buffer, 0, buffer.length);
                    if (count < 0) break;
                    if (count > 0) {
                        sb.append(buffer, 0, count);
                    } else {
                        try {
                            Thread.sleep(200);
                        } catch (InterruptedException ex) {
                        }
                    }
                }
            } catch (IOException ex) {
            }
            String s = sb.toString();
            setContents(s);
        }
    }

    /**
     * Returns the name of the file or directory at a given path in a localized
     * form appropriate for presentation to the user.
     *
     * @param path The path.
     * @return the display name.
     */
    private static native String nativeGetDisplayName(String path);

    /**
     * Return the time of last use, as recorded by Launch Services. Called the "Date Last Opened" by Finder.
     */
    private static native long nativeGetLastUsedDate(String path);

    /**
     * Perform a saved search.
     *
     * @param path The .savedSearch file that defines the query.
     * @return an array containing the paths of the files that satisfy the query or null if the query could not be
     * performed.
     */
    private static native String[] nativeExecuteSavedSearch(String path);

    private static native Object[] nativeGetSidebarFiles(int which, int iconSize, int lastSeed);

    /**
     * Obtain the icon or QuickLook image for a file.
     *
     * @param path the path to the file.
     * @param isQuickLook True to get the QuickLook image, false to get the file icon.
     * @param useIconMode True to use Icon mode when using Quick Look, false otherwise.
     * @param buffers 1x and 2x rasters stored are here (2x is optional)
     * @param w The width of the image.
     * @param h The height of the image.
     * @return true if successful, false otherwise.
     */
    private static native boolean nativeRenderFileImage(String path, boolean isQuickLook, boolean useIconMode, int[][] buffers, int w, int h);
}
