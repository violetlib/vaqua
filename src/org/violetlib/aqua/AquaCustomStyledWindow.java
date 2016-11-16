/*
 * Copyright (c) 2015-2016 Alan Snyder.
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
import java.awt.geom.Rectangle2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.*;
import javax.swing.border.AbstractBorder;
import javax.swing.border.Border;
import javax.swing.plaf.BorderUIResource;
import javax.swing.plaf.UIResource;

import static org.violetlib.aqua.AquaUtils.*;

/**
 * This class supports custom window styles that use the NSView full content view option on a decorated window. In all
 * cases the content pane occupies the entire window and title bar paints over the content pane.
 * The styles differ in the treatment of the title bar, the tool bar (if any), and the window background.
 *
 * <ul>
 * <li>{@code STYLE_NORMAL}</li> - A normal title bar is used. The content pane will be visible beneath the title bar
 * when the title bar is translucent. The default border for the content pane has a top inset that matches the
 * height of the title bar.
 *
 * <li>{@code STYLE_TRANSPARENT}</li> - A title bar with a transparent background is used. The content pane will be
 * visible beneath the title bar. The default border for the content pane has a top inset that matches the
 * height of the title bar.
 *
 * <li>{@code STYLE_HIDDEN}</li> - A title bar with a transparent background is used, and an attempt is made to ensure
 * that the title bar does not paint anything. (The application should avoid setting a title on the window.) This
 * option is used to create a window with rounded corners but no (apparent) title bar. It is up to the application to
 * implement window dragging. Optional top and bottom window margins are painted if defined. If either a top or bottom
 * margin is defined, the content pane is set to not-opaque to expose the margin backgrounds.
 *
 * <li>{@code STYLE_UNIFIED}</li> - This option requires a non-floatable JToolBar as a child component of the content
 * pane positioned at the top of the content pane. It creates a unified title bar and tool bar by using a transparent
 * title bar, painting a textured window background that includes a gradient under the title bar and tool bar, and by
 * installing a default tool bar border with a top inset, so that the tool bar is positioned below the title bar. The
 * content pane and tool bar are set to not-opaque to expose the textured background. The window title is cleared; the
 * application should avoid setting a title on the window. A mouse listener is attached to the toolbar to support
 * dragging the window.
 *
 * <li>{@code STYLE_COMBINED}</li> - This option requires a non-floatable JToolBar as a child component of the content
 * pane positioned at the top of the content pane. It creates a combined title bar and tool bar by using a transparent
 * title bar, painting a textured window background that includes a gradient under the title bar and tool bar, and by
 * installing a default tool bar border with a left inset, so that the tool bar is positioned to the right of the title
 * bar buttons. The content pane and tool bar are set to not-opaque to expose the textured background. The window title
 * is cleared; the application should avoid setting a title on the window. A mouse listener is attached to the toolbar
 * to support dragging the window.
 *
 * <li>{@code STYLE_TEXTURED_HIDDEN}</li> - This option requires a non-floatable JToolBar as a child component of the
 * content pane positioned at the top of the content pane. It creates a textured window with a tool bar instead of a
 * title bar by using a transparent title bar, and by painting a textured window background that includes a
 * gradient under the tool bar. The content pane and tool bar are set to not-opaque to expose the textured background.
 * The window title is cleared; the application should avoid setting a title on the window. A mouse listener is attached
 * to the toolbar to support dragging the window.
 */
public class AquaCustomStyledWindow {

    public static class RequiredToolBarNotFoundException extends IllegalArgumentException {
        public RequiredToolBarNotFoundException() {
            super("Window content pane must contain a non-floatable JToolBar");
        }
    }

    public static final int STYLE_NORMAL = 0;
    public static final int STYLE_TRANSPARENT = 1;
    public static final int STYLE_HIDDEN = 2;
    public static final int STYLE_UNIFIED = 3;
    public static final int STYLE_TEXTURED_HIDDEN = 4;
    public static final int STYLE_COMBINED = 5;

    protected final int TITLE_BAR_HEIGHT = 22;
    protected final int TITLE_BAR_BUTTONS_WIDTH = 78;

    protected Window w;
    protected JRootPane rp;

    protected final int style;
    protected final int titleBarStyle;
    /**
     * If true, a top margin background is painted (in the style of a unified title/tool bar).
     */
    protected final boolean isTopMarginPainted;
    /**
     * If true, a bottom margin background is painted.
     */
    protected final boolean isBottomMarginPainted;
    /**
     * If true, a textured background is painted.
     */
    protected final boolean isTextured;

    protected JComponent contentPane;   // the window content pane
    protected JToolBar windowToolBar;   // the window tool bar, if we care about it -- depends on the style

    protected WindowPropertyChangeListener propertyChangeListener;
    protected WindowDraggingMouseListener windowDraggingMouseListener;
    protected WindowMarginDraggingMouseListener windowMarginDraggingMouseListener;
    protected HierarchyListener toolbarHierarchyListener;

    /**
     * Enable a custom window style.
     * @param w The window. The window must be decorated. The window should be fully configured and populated. This
     *          class does not support subsequent replacement of the content pane or the tool bar.
     * @param style The window style.
     * @throws IllegalArgumentException if the style is not valid, the window is not decorated, the content pane is
     *  not a JComponent, or a required JToolBar is not found.
     */
    public AquaCustomStyledWindow(Window w, int style) throws IllegalArgumentException {

        if (w instanceof JFrame) {
            JFrame fr = (JFrame) w;
            if (fr.isUndecorated()) {
                throw new IllegalArgumentException("Window is not decorated");
            }
            this.w = w;
            rp = fr.getRootPane();
        } else if (w instanceof JDialog) {
            JDialog d = (JDialog) w;
            if (d.isUndecorated()) {
                throw new IllegalArgumentException("Window is not decorated");
            }
            this.w = w;
            rp = d.getRootPane();
        } else {
            throw new IllegalArgumentException("Window is not decorated");
        }

        this.style = style;

        switch (style) {
            case STYLE_NORMAL:
                titleBarStyle = TITLE_BAR_OVERLAY;
                isTopMarginPainted = false;
                isBottomMarginPainted = false;
                isTextured = false;
                break;
            case STYLE_TRANSPARENT:
                titleBarStyle = TITLE_BAR_TRANSPARENT;
                isTopMarginPainted = getTopMarginHeight() > 0;
                isBottomMarginPainted = getBottomMarginHeight() > 0;
                isTextured = false;
                break;
            case STYLE_HIDDEN:
                titleBarStyle = TITLE_BAR_HIDDEN;
                isTopMarginPainted = getTopMarginHeight() > 0;
                isBottomMarginPainted = getBottomMarginHeight() > 0;
                isTextured = false;
                break;
            case STYLE_UNIFIED:
                titleBarStyle = TITLE_BAR_TRANSPARENT;
                isTopMarginPainted = true;
                isBottomMarginPainted = getBottomMarginHeight() > 0;
                isTextured = true;
                break;
            case STYLE_TEXTURED_HIDDEN:
                titleBarStyle = TITLE_BAR_HIDDEN;
                isTopMarginPainted = true;
                isBottomMarginPainted = getBottomMarginHeight() > 0;
                isTextured = true;
                break;
            case STYLE_COMBINED:
                titleBarStyle = TITLE_BAR_TRANSPARENT;
                isTopMarginPainted = true;
                isBottomMarginPainted = getBottomMarginHeight() > 0;
                isTextured = true;
                break;
            default:
                throw new IllegalArgumentException("Invalid style");
        }

        contentPane = getContentPane();

        if (contentPane == null) {
            throw new IllegalArgumentException("Window content pane is not a Swing component");
        }

        if (isTextured) {
            windowToolBar = getWindowToolbar();
            if (windowToolBar == null) {
                throw new RequiredToolBarNotFoundException();
            }
            setupToolbar(windowToolBar);
        }

        setupContentPane(contentPane);

        if (isTopMarginPainted || isBottomMarginPainted) {
            // the background depends (or may depend) on the active state of the window
            propertyChangeListener = new WindowPropertyChangeListener();
            rp.addPropertyChangeListener(AquaFocusHandler.FRAME_ACTIVE_PROPERTY, propertyChangeListener);
        }

        if (style == STYLE_COMBINED) {
            AquaUtils.setWindowTitle(w, "");
        }

        AquaUtils.setTitleBarStyle(w, titleBarStyle);
    }

    public int getStyle() {
        return style;
    }

    /**
     * This method supports a workaround. Make sure that the bits we care about in the NSWindow style mask are set
     * properly.
     */
    public void refreshWindowStyleMask() {
        AquaUtils.setTitleBarStyle(w, titleBarStyle);
    }

    public void dispose() {
        if (w != null) {
            rp.removePropertyChangeListener(AquaFocusHandler.FRAME_ACTIVE_PROPERTY, propertyChangeListener);
            propertyChangeListener = null;
            rp = null;
            AquaUtils.setTitleBarStyle(w, AquaUtils.TITLE_BAR_ORDINARY);
            if (windowToolBar != null) {
                resetBorder(windowToolBar);
                if (toolbarHierarchyListener != null) {
                    windowToolBar.removeHierarchyListener(toolbarHierarchyListener);
                    toolbarHierarchyListener = null;
                }
                windowToolBar = null;
            }
            if (windowDraggingMouseListener != null) {
                windowDraggingMouseListener.detach();
                windowDraggingMouseListener = null;
            }
            if (windowMarginDraggingMouseListener != null) {
                windowMarginDraggingMouseListener.detach();
                windowMarginDraggingMouseListener = null;
            }
            resetBorder(contentPane);
            w = null;
            contentPane = null;
        }
    }

    protected class WindowPropertyChangeListener implements PropertyChangeListener {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {
            rp.repaint();
        }
    }

    protected void setupContentPane(JComponent cp) {
        if ((style == STYLE_NORMAL || style == STYLE_TRANSPARENT) && !isTopMarginPainted) {
            installBorder(cp, TITLE_BAR_HEIGHT, 0, 0, 0);
        } else {
            installBorder(cp, 0, 0, 0, 0);
        }

        if (isTopMarginPainted || isBottomMarginPainted) {
            cp.setOpaque(false);
            attachWindowMarginDraggingMouseListener(cp);
        }
    }

    protected void setupToolbar(JToolBar tb) {
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

    protected void installToolbarBorder(JToolBar tb) {
        Border b = tb.getBorder();
        if (b == null || b instanceof UIResource) {
            boolean isTall = AquaToolBarUI.isTallFormat(tb);
            int left = 4;
            int top = 4;
            int bottom = isTall ? 0 : 4;
            if (style == STYLE_UNIFIED) {
                tb.setBorder(new CustomToolbarBorder(left, TITLE_BAR_HEIGHT, bottom, true));
            } else if (style == STYLE_COMBINED) {
                tb.setBorder(new CustomToolbarBorder(TITLE_BAR_BUTTONS_WIDTH, top, bottom, true));
            } else if (style == STYLE_TEXTURED_HIDDEN){
                tb.setBorder(new CustomToolbarBorder(left, top, bottom, true));
            }
        }
    }

    protected void installBorder(JComponent c, int top, int left, int bottom, int right) {
        Border b = c.getBorder();
        if (b == null || b instanceof UIResource) {
            c.setBorder(new BorderUIResource.EmptyBorderUIResource(top, left, bottom, right));
        }
    }

    protected void resetBorder(JComponent c) {
        Border b = c.getBorder();
        if (b == null || b instanceof UIResource) {
            if (c instanceof JToolBar) {
                c.setBorder(AquaToolBarUI.getToolBarBorder());
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
            int topMarginHeight = getTopMarginHeight();
            if (topMarginHeight > 0) {
                if (y < topMarginHeight) return true;
            }
            int bottomMarginHeight = getBottomMarginHeight();
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

    protected void attachHierarchyListener(JToolBar tb) {
        if (toolbarHierarchyListener == null) {
            toolbarHierarchyListener = new ToolbarHierarchyListener();
        }
        tb.addHierarchyListener(toolbarHierarchyListener);
    }

    public void paintBackground(Graphics g) {
        if (isTopMarginPainted || isBottomMarginPainted) {
            if (!AquaVibrantSupport.isVibrant(rp)) {
                if (isTopMarginPainted) {
                    paintTopMarginBackground(g);
                }
                if (isBottomMarginPainted) {
                    paintBottomMarginBackground(g);
                }
            }
        }
    }

    protected void paintTopMarginBackground(Graphics g) {
        Graphics2D gg = (Graphics2D) g.create();
        try {
            int width = rp.getWidth();

            int y = getTopMarginHeight();
            int height = isTextured ? rp.getHeight() : y;
            boolean isActive = AquaFocusHandler.isActive(rp);
            if (isActive) {
                Color c1 = new Color(230, 230, 230);
                Color c2 = new Color(208, 208, 208);
                GradientPaint gp = new GradientPaint(0, 0, c1, 0, y, c2);
                gg.setPaint(gp);
            } else {
                gg.setColor(new Color(246, 246, 246));
            }
            gg.fillRect(0, 0, width, height);

            if (y > 0) {
                int scale = JavaSupport.getScaleFactor(gg);
                paintUnifiedDivider(gg, 0, width, isActive, true, scale);
                paintUnifiedDivider(gg, y-1, width, isActive, false, scale);
            }
        } finally {
            gg.dispose();
        }
    }

    protected void paintUnifiedDivider(Graphics2D g, int y, int width, boolean isActive, boolean isTop, int scale) {
        String name = "Window.unified" + (isTop ? "Top" : "Bottom") + "Divider" + (isActive ? "" : "Inactive") + "Color";

        if (scale > 1) {
            Color c1 = UIManager.getColor(name + "1");
            Color c2 = UIManager.getColor(name + "2");
            if (c1 != null && c2 != null) {
                g.setColor(c1);
                g.fill(new Rectangle2D.Float(0, y, width, 0.5f));
                g.setColor(c2);
                g.fill(new Rectangle2D.Float(0, y+0.5f, width, 0.5f));
                return;
            }
        }

        Color c = UIManager.getColor(name);
        if (c != null) {
            g.setColor(c);
            g.fillRect(0, y, width, 1);
        }
    }

    protected int getTopMarginHeight() {
        switch (style) {
            case STYLE_UNIFIED:
            case STYLE_TEXTURED_HIDDEN:
                return windowToolBar.getHeight();
            case STYLE_COMBINED:
                return Math.max(windowToolBar.getHeight(), TITLE_BAR_HEIGHT);
            case STYLE_HIDDEN:
            case STYLE_TRANSPARENT:
                if (rp != null) {
                    Integer n = AquaUtils.getIntegerProperty(rp, AquaRootPaneUI.AQUA_WINDOW_TOP_MARGIN_KEY);
                    return n != null ? n : -1;
                }
            default:
                return -1;
        }
    }

    protected int getBottomMarginHeight() {
        switch (style) {
            case STYLE_HIDDEN:
            case STYLE_TRANSPARENT:
            case STYLE_UNIFIED:
            case STYLE_TEXTURED_HIDDEN:
            case STYLE_COMBINED:
                if (rp != null) {
                    Integer n = AquaUtils.getIntegerProperty(rp, AquaRootPaneUI.AQUA_WINDOW_BOTTOM_MARGIN_KEY);
                    return n != null ? n : -1;
                }
        }
        return -1;
    }

    protected void paintBottomMarginBackground(Graphics g) {
        int bottomMarginHeight = getBottomMarginHeight();
        if (bottomMarginHeight > 0) {
            int y = rp.getHeight() - bottomMarginHeight;
            paintBottomMarginBackground(g, y, bottomMarginHeight);
        }
    }

    protected void paintBottomMarginBackground(Graphics g, int y, int height) {
        Graphics2D gg = (Graphics2D) g.create();
        try {
            boolean isActive = AquaFocusHandler.isActive(rp);
            if (isActive) {
                if (OSXSystemProperties.OSVersion < 1012) {
                    gg.setColor(new Color(234, 234, 234));
                } else {
                    Color c1 = new Color(228, 228, 228);
                    Color c2 = new Color(217, 217, 217);
                    GradientPaint gp = new GradientPaint(0, y, c1, 0, y + height, c2);
                    gg.setPaint(gp);
                }
            } else {
                gg.setColor(new Color(246, 246, 246));
            }
            gg.fillRect(0, y, rp.getWidth(), height);
        } finally {
            gg.dispose();
        }
    }

    protected JComponent getContentPane() {
        Container c = rp.getContentPane();
        return c instanceof JComponent ? (JComponent) c : null;
    }

    public JToolBar getWindowToolbar() {
        Container c = rp.getContentPane();
        return getWindowToolbar(c);
    }

    protected JToolBar getWindowToolbar(Container c) {
        int count = c.getComponentCount();

        for (int i = 0; i < count; i++) {
            Component m = c.getComponent(i);
            if (m instanceof JToolBar) {
                JToolBar tb = (JToolBar) m;
                if (!tb.isFloatable()) {
                    return tb;
                }
            }
        }

        for (int i = 0; i < count; i++) {
            Component m = c.getComponent(i);
            if (m instanceof Container) {
                JToolBar tb = getWindowToolbar((Container) m);
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
                JToolBar tb = (JToolBar) e.getComponent();
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

    public static class CustomToolbarBorder extends AbstractBorder implements UIResource {
        protected int extraTop;
        protected int extraLeft;
        protected int extraBottom;
        protected boolean includeBottomLine;
        protected boolean isExtraTopSuppressed;

        public CustomToolbarBorder(int extraLeft, int extraTop, int extraBottom, boolean includeBottomLine) {
            this.extraLeft = extraLeft;
            this.extraTop = extraTop;
            this.extraBottom = extraBottom;
            this.includeBottomLine = includeBottomLine;
        }

        @Override
        public Insets getBorderInsets(Component c, Insets insets)
        {
            JToolBar tb = (JToolBar) c;
            Insets margin = tb.getMargin();
            insets.left = margin.left + extraLeft;
            insets.top = margin.top + (isExtraTopSuppressed ? 0 : extraTop);
            insets.right = margin.right;
            insets.bottom = margin.bottom + extraBottom + (includeBottomLine ? 1 : 0);
            return insets;
        }

        public void setExtraTopSuppressed(boolean extraTopSuppressed) {
            isExtraTopSuppressed = extraTopSuppressed;
        }

        @Override
        public void paintBorder(Component c, Graphics g, int x, int y, int width, int height)
        {
//            if (includeBottomLine) {
//                g.setColor(new Color(186, 186, 186));  // 206 if not textured
//                g.fillRect(x, y + height - 1, width, 1);
//            }
        }
    }
}
