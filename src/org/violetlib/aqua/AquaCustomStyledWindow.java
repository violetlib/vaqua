/*
 * Copyright (c) 2015-2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

// Full content view support belongs in AWT.

import java.awt.*;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.*;
import javax.swing.border.AbstractBorder;
import javax.swing.border.Border;
import javax.swing.plaf.UIResource;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import static org.violetlib.aqua.AquaUtils.*;

/**
 * This class supports custom window styles that use the NSView full content view option on a decorated window. In all
 * cases the content pane occupies the entire window and title bar paints over the content pane. The styles differ in
 * the treatment of the title bar, the tool bar (if any), and the window background.
 *
 * <ul>
 * <li>{@code STYLE_OVERLAY}</li> - A normal title bar is used. The content pane will be visible beneath the title bar
 * when the title bar is translucent. The default border for the content pane has a top inset that matches the height of
 * the title bar.
 *
 * <li>{@code STYLE_TRANSPARENT}</li> - A title bar with a transparent background is used. The content pane will be
 * visible beneath the title bar. The default border for the content pane has a top inset that matches the height of the
 * title bar.
 *
 * <li>{@code STYLE_HIDDEN}</li> - A title bar with a transparent background is used, and an attempt is made to ensure
 * that the title bar does not paint anything. (The application should avoid setting a title on the window.) This
 * option is used to create a window with rounded corners but no (apparent) title bar. It is up to the application to
 * implement window dragging. Optional top and bottom window margins are painted if defined. If either a top or bottom
 * margin is defined, the content pane is set to not-opaque to expose the margin backgrounds.
 *
 * <li>{@code STYLE_UNIFIED}</li> - This option requires a non-floatable JToolBar or toolbar panel as a child component
 * of the content pane positioned at the top of the content pane. It creates a unified title bar and tool bar by using a
 * transparent title bar, painting a textured window background that includes a gradient under the title bar and tool
 * bar, and by installing a default tool bar border with a top inset, so that the tool bar is positioned below the title
 * bar. The content pane and tool bar are set to not-opaque to expose the textured background. The window title is
 * cleared; the application should avoid setting a title on the window. A mouse listener is attached to the toolbar to
 * support dragging the window.
 *
 * <li>{@code STYLE_COMBINED}</li> - This option requires a non-floatable JToolBar or toolbar panel as a child
 * component of the content pane positioned at the top of the content pane. It creates a combined title bar and tool bar
 * by using a transparent title bar, painting a textured window background that includes a gradient under the title bar
 * and tool bar, and by installing a default tool bar border with a left inset, so that the tool bar is positioned to
 * the right of the title bar buttons. The content pane and tool bar are set to not-opaque to expose the textured
 * background. The window title is cleared; the application should avoid setting a title on the window. A mouse listener
 * is attached to the toolbar to support dragging the window.
 *
 * <li>{@code STYLE_TEXTURED_HIDDEN}</li> - This option requires a non-floatable JToolBar or toolbar panel as a child
 * component of the content pane positioned at the top of the content pane. It creates a textured window with a tool bar
 * instead of a title bar by using a transparent title bar, and by painting a textured window background that includes a
 * gradient under the tool bar. The content pane and tool bar are set to not-opaque to expose the textured background.
 * The window title is cleared; the application should avoid setting a title on the window. A mouse listener is attached
 * to the toolbar to support dragging the window.
 */
public class AquaCustomStyledWindow {

    public static class RequiredToolBarNotFoundException extends IllegalArgumentException {
        public RequiredToolBarNotFoundException() {
            super("Window content pane must contain a non-floatable JToolBar or a toolbar panel identified by "
                    + TOOLBAR_PANEL_PROPERTY + "=true");
        }
    }

    public static final int STYLE_OVERLAY = 0;          // AKA overlay title bar
    public static final int STYLE_TRANSPARENT = 1;      // AKA transparent title bar
    public static final int STYLE_HIDDEN = 2;           // AKA no title bar
    public static final int STYLE_UNIFIED = 3;          // AKA unified tool bar
    public static final int STYLE_TEXTURED_HIDDEN = 4;  // AKA textured tool bar
    public static final int STYLE_COMBINED = 5;         // AKA combined title and tool bar
    public static final int STYLE_UNDECORATED = 6;      // Internal use

    protected final int TITLE_BAR_HEIGHT = 22;
    protected final int TITLE_BAR_BUTTONS_WIDTH = 78;

    protected @Nullable Window w;
    protected @Nullable JRootPane rp;

    protected final int style;

    protected int declaredTopMarginHeight;
    protected int declaredBottomMarginHeight;
    protected int fixedTopMarginHeight;
    protected int fixedBottomMarginHeight;

    /** Computed top margin height, used when painting and handling mouse events. */
    protected int topMarginHeight;
    /** Computed bottom margin height, used when painting and handling mouse events. */
    protected int bottomMarginHeight;

    protected final int titleBarStyle;
    /**
     * If true, a textured background is painted.
     */
    protected final boolean isTextured;

    protected JComponent contentPane;   // the window content pane
    protected JComponent windowToolBar;   // the window tool bar, if we care about it -- depends on the style

    protected WindowPropertyChangeListener propertyChangeListener;
    protected WindowDraggingMouseListener windowDraggingMouseListener;
    protected WindowMarginDraggingMouseListener windowMarginDraggingMouseListener;
    protected HierarchyListener toolbarHierarchyListener;

    /**
     * Enable a custom window style.
     * @param w The window. If the undecorated window style is selected, the window must not be decorated.
     *          Otherwise, the window must be decorated. The window should be fully configured and populated. This
     *          class does not support subsequent replacement of the content pane or the tool bar.
     * @param style The window style.
     * @param declaredTopMarginHeight The declared top margin height, or -1 if not declared.
     * @param declaredBottomMarginHeight The declared bottom margin height, or -1 if not declared.
     * @throws IllegalArgumentException if the style is not valid, the window is not appropriately decorated, the content
     *  pane is not a JComponent, or a required JToolBar or toolbar panel is not found.
     */
    public AquaCustomStyledWindow(@NotNull Window w, int style, int declaredTopMarginHeight, int declaredBottomMarginHeight)
            throws IllegalArgumentException {

        boolean isDecorated = AquaUtils.isDecorated(w);
        if (style == STYLE_UNDECORATED) {
            if (isDecorated) {
                throw new IllegalArgumentException("Window is decorated");
            }
        } else {
            if (!isDecorated) {
                throw new IllegalArgumentException("Window is not decorated");
            }
        }

        this.w = w;
        JRootPane rootPane = AquaUtils.getRootPane(w);
        if (rootPane == null) {
            throw new IllegalArgumentException("Window lacks a root pane");
        }

        this.rp = rootPane;
        this.style = style;
        this.declaredTopMarginHeight = declaredTopMarginHeight;
        this.declaredBottomMarginHeight = declaredBottomMarginHeight;
        this.fixedTopMarginHeight = getFixedTopMarginHeight();
        this.fixedBottomMarginHeight = getFixedBottomMarginHeight();

        contentPane = getContentPane();

        if (contentPane == null) {
            throw new IllegalArgumentException("Window content pane is not a Swing component");
        }

        windowToolBar = getWindowToolbar();

        switch (style) {
            case STYLE_OVERLAY:
                titleBarStyle = TITLE_BAR_OVERLAY;
                isTextured = false;
                break;
            case STYLE_TRANSPARENT:
                titleBarStyle = TITLE_BAR_TRANSPARENT;
                isTextured = false;
                break;
            case STYLE_HIDDEN:
                titleBarStyle = TITLE_BAR_HIDDEN;
                isTextured = false;
                break;
            case STYLE_UNIFIED:
                titleBarStyle = TITLE_BAR_TRANSPARENT;
                isTextured = true;
                break;
            case STYLE_TEXTURED_HIDDEN:
                titleBarStyle = TITLE_BAR_HIDDEN;
                isTextured = true;
                break;
            case STYLE_COMBINED:
                titleBarStyle = TITLE_BAR_TRANSPARENT;
                isTextured = true;
                break;
            case STYLE_UNDECORATED:
                titleBarStyle = TITLE_BAR_NONE;
                isTextured = false;
                break;
            default:
                throw new IllegalArgumentException("Invalid style");
        }

        if (isTextured) {
            if (windowToolBar == null) {
                throw new RequiredToolBarNotFoundException();
            }
            setupToolbar(windowToolBar);
        }

        setupContentPane(contentPane);

        propertyChangeListener = new WindowPropertyChangeListener();
        rp.addPropertyChangeListener(AquaFocusHandler.FRAME_ACTIVE_PROPERTY, propertyChangeListener);

        if (style == STYLE_COMBINED) {
            AquaUtils.setWindowTitle(w, "");
        }

        AquaUtils.setTitleBarStyle(w, titleBarStyle);
    }

    /**
     * Create a replacement custom styled window without resetting the window properties.
     * @param style The window style.
     * @param top The declared top margin height, or -1 if not declared.
     * @param bottom The declared bottom margin height, or -1 if not declared.
     * @return the replacement custom styled window.
     * @throws IllegalArgumentException if the style is not valid, the window is not appropriately decorated, the content
     *  pane is not a JComponent, or a required JToolBar or toolbar panel is not found.
     */

    public @NotNull AquaCustomStyledWindow reconfigure(int style, int top, int bottom) {
        assert w != null;
        AquaCustomStyledWindow replacement = new AquaCustomStyledWindow(w, style, top, bottom);
        removeListeners();
        // do not dispose, as that alters the window
        return replacement;
    }

    public int getStyle() {
        return style;
    }

    public boolean isValid(int style, int declaredTopMarginHeight, int declaredBottomMarginHeight) {
        return style == this.style
                && declaredTopMarginHeight == this.declaredTopMarginHeight
                && declaredBottomMarginHeight == this.declaredBottomMarginHeight;
    }

    public boolean isTextured() {
        return isTextured;
    }

    public void dispose() {
        if (w != null) {
            removeListeners();
            propertyChangeListener = null;
            toolbarHierarchyListener = null;
            windowDraggingMouseListener = null;
            windowMarginDraggingMouseListener = null;
            AquaUtils.setTitleBarStyle(w, AquaUtils.TITLE_BAR_ORDINARY);
            if (windowToolBar != null) {
                resetBorder(windowToolBar);
            }
            if (contentPane != null) {
                resetBorder(contentPane);
            }
            w = null;
            rp = null;
            contentPane = null;
            windowToolBar = null;
        }
    }

    private void removeListeners() {
        if (rp != null && propertyChangeListener != null) {
            rp.removePropertyChangeListener(AquaFocusHandler.FRAME_ACTIVE_PROPERTY, propertyChangeListener);
        }
        if (windowToolBar != null && toolbarHierarchyListener != null) {
            windowToolBar.removeHierarchyListener(toolbarHierarchyListener);
        }
        if (windowDraggingMouseListener != null) {
            windowDraggingMouseListener.detach();
        }
        if (windowMarginDraggingMouseListener != null) {
            windowMarginDraggingMouseListener.detach();
        }
    }

    protected class WindowPropertyChangeListener implements PropertyChangeListener {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {
            rp.repaint();
        }
    }

    protected void setupContentPane(JComponent cp) {
        if (style == STYLE_OVERLAY || (style == STYLE_TRANSPARENT && declaredTopMarginHeight < 0)) {
            installContentPaneBorder(cp, TITLE_BAR_HEIGHT, 0, 0, 0);
        } else {
            installContentPaneBorder(cp, 0, 0, 0, 0);
        }

        // Don't allow the content pane to paint a background unless we are sure there is nothing
        // underneath the content pane that we want to be visible.
        if (fixedTopMarginHeight != 0 || fixedBottomMarginHeight != 0 || style == STYLE_OVERLAY) {
            assert rp != null;
            cp.setOpaque(false);
            rp.getLayeredPane().setOpaque(false);
            attachWindowMarginDraggingMouseListener(cp);
        }
    }

    protected void setupToolbar(JComponent tb) {
        tb.setOpaque(false);

        Container p = tb;
        while ((p = p.getParent()) != contentPane && p != null) {
            if (p instanceof JComponent) {
                ((JComponent) p).setOpaque(false);
            }
        }

        installToolbarBorder(tb);
        attachWindowDraggingMouseListener(tb);
        attachHierarchyListener(tb);
    }

    protected void installToolbarBorder(JComponent tb) {
        Border b = tb.getBorder();
        if (b == null || b instanceof UIResource) {
            boolean isTall = AquaToolBarUI.isTallFormatToolBar(tb);
            int left = 4;
            int top = 4;
            int bottom = isTall ? 0 : 4;
            if (style == STYLE_UNIFIED) {
                tb.setBorder(new CustomToolbarBorder(left, TITLE_BAR_HEIGHT, bottom));
            } else if (style == STYLE_COMBINED) {
                tb.setBorder(new CustomToolbarBorder(TITLE_BAR_BUTTONS_WIDTH, top, bottom));
            } else if (style == STYLE_TEXTURED_HIDDEN){
                tb.setBorder(new CustomToolbarBorder(left, top, bottom));
            }
        }
    }

    protected void installContentPaneBorder(JComponent c, int top, int left, int bottom, int right) {
        Border b = c.getBorder();
        if (b == null || b instanceof UIResource) {
            c.setBorder(new CustomContentPaneBorder(top, left, bottom, right));
        }
    }

    protected void resetBorder(JComponent c) {
        Border b = c.getBorder();
        if (b == null || b instanceof UIResource) {
            if (c instanceof JToolBar) {
                c.setBorder(AquaToolBarUI.getToolBarBorder((JToolBar) c));
            } else {
                c.setBorder(null);
            }
        }
    }

    protected class WindowMarginDraggingMouseListener extends WindowDraggingMouseListener {
        public WindowMarginDraggingMouseListener() {
            super(0);
        }

        @Override
        protected boolean isDragArea(Component c, Point p) {
            int y = p.y;
            if (topMarginHeight > 0) {
                if (y < topMarginHeight) return true;
            }
            if (bottomMarginHeight > 0) {
                if (y >= c.getHeight() - bottomMarginHeight) {
                    return true;
                }
            }
            return false;
        }
    }

    protected void attachWindowDraggingMouseListener(JComponent c) {
        if (c != null) {
            if (windowDraggingMouseListener == null) {
                int topExclude = titleBarStyle == TITLE_BAR_TRANSPARENT ? TITLE_BAR_HEIGHT : 0;
                windowDraggingMouseListener = new WindowDraggingMouseListener(topExclude);
                windowDraggingMouseListener.attach(c);
            }
        }
    }

    protected void attachWindowMarginDraggingMouseListener(JComponent c) {
        if (c != null) {
            if (windowMarginDraggingMouseListener == null) {
                windowMarginDraggingMouseListener = new WindowMarginDraggingMouseListener();
                windowMarginDraggingMouseListener.attach(c);
            }
        }
    }

    protected void attachHierarchyListener(JComponent tb) {
        if (toolbarHierarchyListener == null) {
            toolbarHierarchyListener = new ToolbarHierarchyListener();
        }
        tb.addHierarchyListener(toolbarHierarchyListener);
    }

    public void paintMarginBackgrounds(@NotNull Graphics g) {
        // Update the possibly dynamic margin heights
        topMarginHeight = calculateTopMarginHeight();
        bottomMarginHeight = calculateBottomMarginHeight();

        assert rp != null;

        if (topMarginHeight > 0) {
            paintMarginBackground(g, 0, topMarginHeight, true);
        }
        if (bottomMarginHeight > 0) {
            int y = rp.getHeight() - bottomMarginHeight;
            paintMarginBackground(g, y, bottomMarginHeight, false);
        }
    }

    protected void paintMarginBackground(@NotNull Graphics g, int y, int height, boolean isTop) {
        if (height > 0) {
            Graphics2D gg = (Graphics2D) g.create();
            try {
                boolean isActive = AquaFocusHandler.isActive(rp);
                boolean isSheet = AquaSheetSupport.isSheet(rp);

                Color c;

                if (isSheet) {
                    c = AquaColors.CLEAR;
                } else {
                    c = AquaUtils.getWindowMarginBackground(rp, isTop);
                }

                AquaUtils.fillRect(g, c, 0, y, rp.getWidth(), height);

                int dividerY;
                if (isTop) {
                    dividerY = y + height - 1;
                } else {
                    dividerY = y;
                }
                int rootPaneHeight = rp.getHeight();
                if (dividerY < rootPaneHeight) {
                    int width = rp.getWidth();
                    paintUnifiedDivider(gg, dividerY, width, isActive, isTop);
                }
            } finally {
                gg.dispose();
            }
        }
    }

    protected void paintUnifiedDivider(Graphics2D g, int y, int width, boolean isActive, boolean isTop) {
        assert rp != null;
        Color c = AquaUtils.getWindowMarginDividerColor(rp, isTop);
        AquaUtils.fillRect(g, c, 0, y, width, 1);
    }

    /**
     * Compute a top margin height based only on the style and the declared top margin height.
     * @return the top margin height, as described, or -1 if a top margin is not supported or must be computed
     * dynamically.
     */
    protected int getFixedTopMarginHeight() {
        switch (style) {
            case STYLE_OVERLAY:
                if (declaredTopMarginHeight >= 0) {
                    return declaredTopMarginHeight + TITLE_BAR_HEIGHT;
                } else {
                    return -1;
                }
            case STYLE_HIDDEN:
            case STYLE_TRANSPARENT:
            case STYLE_UNDECORATED:
                return declaredTopMarginHeight > 0 ? declaredTopMarginHeight : 0;
            default:
                return -1;
        }
    }

    /**
     * Compute a bottom margin height based only on the style and the declared bottom margin height.
     * @return the bottom margin height, as described, or -1 if a bottom margin is not supported or must be computed
     * dynamically.
     */
    protected int getFixedBottomMarginHeight() {
        switch (style) {
            case STYLE_OVERLAY:
            case STYLE_HIDDEN:
            case STYLE_TRANSPARENT:
            case STYLE_UNIFIED:
            case STYLE_TEXTURED_HIDDEN:
            case STYLE_COMBINED:
            case STYLE_UNDECORATED:
                return declaredBottomMarginHeight;
        }
        return -1;
    }

    protected int calculateTopMarginHeight() {
        if (fixedTopMarginHeight >= 0) {
            return fixedTopMarginHeight;
        }

        int toolbarHeight = windowToolBar != null ? windowToolBar.getHeight() : 0;
        switch (style) {
            case STYLE_UNIFIED:
            case STYLE_TEXTURED_HIDDEN:
                return toolbarHeight;
            case STYLE_COMBINED:
                return Math.max(toolbarHeight, TITLE_BAR_HEIGHT);
            default:
                return 0;
        }
    }

    protected int calculateBottomMarginHeight() {
        return fixedBottomMarginHeight;
    }

    protected JComponent getContentPane() {
        Container c = rp.getContentPane();
        return c instanceof JComponent ? (JComponent) c : null;
    }

    public @Nullable JComponent getWindowToolbar() {
        Container c = rp.getContentPane();
        return getWindowToolbar(c);
    }

    protected @Nullable JComponent getWindowToolbar(Container c) {
        int count = c.getComponentCount();

        for (int i = 0; i < count; i++) {
            Component m = c.getComponent(i);
            if (AquaUtils.isToolBar(m)) {
                return (JComponent) m;
            }
        }

        for (int i = 0; i < count; i++) {
            Component m = c.getComponent(i);
            if (m instanceof Container) {
                JComponent tb = getWindowToolbar((Container) m);
                if (tb != null) {
                    return tb;
                }
            }
        }

        return null;
    }

    protected class ToolbarHierarchyListener implements HierarchyListener {
        @Override
        public void hierarchyChanged(HierarchyEvent e) {
            if (e.getChangeFlags() == HierarchyEvent.PARENT_CHANGED) {
                JComponent tb = (JComponent) e.getComponent();
                if (SwingUtilities.getWindowAncestor(tb) != w) {
                    resetBorder(tb);
                    if (tb == windowToolBar) {
                        windowToolBar = null;
                    }
                    tb.removeHierarchyListener(toolbarHierarchyListener);
                    if (windowDraggingMouseListener != null) {
                        windowDraggingMouseListener.detach();
                        windowDraggingMouseListener = null;
                    }
                }
            }
        }
    }

    protected class CustomBorderBase
            extends AbstractBorder
            implements UIResource {
    }

    protected class CustomContentPaneBorder extends CustomBorderBase {
        private int top;
        private int left;
        private int bottom;
        private int right;

        public CustomContentPaneBorder(int top, int left, int bottom, int right) {
            this.top = top;
            this.left = left;
            this.bottom = bottom;
            this.right = right;
        }

        @Override
        public Insets getBorderInsets(Component c, Insets insets)
        {
            insets.top = top;
            insets.left = left;
            insets.bottom = bottom;
            insets.right = right;
            return insets;
        }
    }

    protected class CustomToolbarBorder extends CustomBorderBase {
        protected int extraTop;
        protected int extraLeft;
        protected int extraBottom;

        public CustomToolbarBorder(int extraLeft, int extraTop, int extraBottom) {
            this.extraLeft = extraLeft;
            this.extraTop = extraTop;
            this.extraBottom = extraBottom;
        }

        @Override
        public Insets getBorderInsets(Component c, Insets insets)
        {
            Insets margin = c instanceof JToolBar ? ((JToolBar) c).getMargin() : new Insets(0, 0, 0, 0);
            insets.left = margin.left + extraLeft;
            insets.top = margin.top + extraTop;
            insets.right = margin.right;
            insets.bottom = margin.bottom + extraBottom + 1;
            return insets;
        }
    }
}
