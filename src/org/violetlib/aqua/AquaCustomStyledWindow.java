/*
 * Copyright (c) 2015-2016 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

// Full content view support belongs in AWT.

// TBD: support bottom gradient for textured

import java.awt.*;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
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
 * implement window dragging.
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
 * <li>{@code STYLE_UNIFIED_HIDDEN}</li> - This option requires a non-floatable JToolBar as a child component of the
 * content pane positioned at the top of the content pane. It creates a textured window with a tool bar instead of a
 * title bar by using a transparent title bar, and by painting a textured window background that includes a
 * gradient under the tool bar. The content pane and tool bar are set to not-opaque to expose the textured background.
 * The window title is cleared; the application should avoid setting a title on the window. A mouse listener is attached
 * to the toolbar to support dragging the window.
 */
public class AquaCustomStyledWindow {

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
    protected final boolean isUnifiedStyle;

    protected JComponent contentPane;   // the window content pane
    protected JToolBar windowToolBar;   // the window tool bar, if we care about it -- depends on the style

    protected WindowPropertyChangeListener propertyChangeListener;
    protected WindowDraggingMouseListener windowDraggingMouseListener;
    protected HierarchyListener toolbarHierarchyListener;

    /**
     * Enable a custom window style.
     * @param w The window. The window must be decorated. The window should be fully configured and populated. This
     *          class does not support subsequent replacement of the content pane or the tool bar.
     * @param style The window style.
     * @throws IllegalArgumentException if the style is not valid, the window is not decorated, the content pane is
     *  not a JComponent
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

        switch (style) {
            case STYLE_NORMAL:
                titleBarStyle = TITLE_BAR_OVERLAY;
                isUnifiedStyle = false;
                break;
            case STYLE_TRANSPARENT:
                titleBarStyle = TITLE_BAR_TRANSPARENT;
                isUnifiedStyle = false;
                break;
            case STYLE_HIDDEN:
                titleBarStyle = TITLE_BAR_HIDDEN;
                isUnifiedStyle = false;
                break;
            case STYLE_UNIFIED:
                titleBarStyle = TITLE_BAR_TRANSPARENT;
                isUnifiedStyle = true;
                break;
            case STYLE_TEXTURED_HIDDEN:
                titleBarStyle = TITLE_BAR_HIDDEN;
                isUnifiedStyle = true;
                break;
            case STYLE_COMBINED:
                titleBarStyle = TITLE_BAR_TRANSPARENT;
                isUnifiedStyle = true;
                break;
            default:
                throw new IllegalArgumentException("Invalid style");
        }

        this.style = style;

        contentPane = getContentPane();

        if (contentPane == null) {
            throw new IllegalArgumentException("Window content pane is not a Swing component");
        }

        if (isUnifiedStyle) {
            windowToolBar = getWindowToolbar();
            if (windowToolBar == null) {
                throw new IllegalArgumentException("Window content pane must contain a non-floatable JToolBar");
            }
            setupToolbar(windowToolBar);
        }

        setupContentPane(contentPane);

        if (isUnifiedStyle) {
            // the background depends on the active state of the window
            propertyChangeListener = new WindowPropertyChangeListener();
            rp.addPropertyChangeListener(AquaFocusHandler.FRAME_ACTIVE_PROPERTY, propertyChangeListener);
        }

        if (style == STYLE_COMBINED) {
            AquaUtils.setWindowTitle(w, "");
        }

        AquaUtils.setTitleBarStyle(w, titleBarStyle);
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
                if (windowDraggingMouseListener != null) {
                    windowDraggingMouseListener.detach(windowToolBar);
                    windowDraggingMouseListener = null;
                }
                windowToolBar = null;
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
        if (style == STYLE_NORMAL || style == STYLE_TRANSPARENT) {
            installBorder(cp, TITLE_BAR_HEIGHT, 0, 0, 0);
        } else {
            installBorder(cp, 0, 0, 0, 0);
        }

        if (isUnifiedStyle) {
            cp.setOpaque(false);
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

    protected void attachWindowDraggingMouseListener(JComponent c) {
        if (c != null) {
            if (windowDraggingMouseListener == null) {
                windowDraggingMouseListener = new WindowDraggingMouseListener();
                windowDraggingMouseListener.attach(c);
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
        if (rp != null) {
            if (isUnifiedStyle) {
                paintUnifiedBackground(g);
            }
        }
    }

    protected void paintUnifiedBackground(Graphics g) {
        Graphics2D gg = (Graphics2D) g.create();
        try {
            int y = getTopGradientHeight();
            boolean isActive = AquaFocusHandler.isActive(rp);
            if (isActive) {
                Color c1 = new Color(230, 230, 230);
                Color c2 = new Color(210, 210, 210);
                GradientPaint gp = new GradientPaint(0, 0, c1, 0, y, c2);
                gg.setPaint(gp);
            } else {
                gg.setColor(new Color(246, 246, 246));
            }
            gg.fillRect(0, 0, rp.getWidth(), rp.getHeight());
        } finally {
            gg.dispose();
        }
    }

    protected int getTopGradientHeight() {
        int toolbarHeight = windowToolBar.getHeight();
        switch (style) {
            case STYLE_UNIFIED:
                return toolbarHeight + TITLE_BAR_HEIGHT;
            case STYLE_COMBINED:
                return Math.max(toolbarHeight, TITLE_BAR_HEIGHT);
            default:
                return toolbarHeight;
        }
   }

    protected JComponent getContentPane() {
        Container c = rp.getContentPane();
        return c instanceof JComponent ? (JComponent) c : null;
    }

    protected JToolBar getWindowToolbar() {
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
                        windowDraggingMouseListener.detach(tb);
                    }
                }
            }
        }
    }

    protected class CustomToolbarBorder extends AbstractBorder implements UIResource
    {
        protected int extraTop;
        protected int extraLeft;
        protected int extraBottom;
        protected boolean includeBottomLine;

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
            insets.top = margin.top + extraTop;
            insets.right = margin.right;
            insets.bottom = margin.bottom + extraBottom + (includeBottomLine ? 1 : 0);
            return insets;
        }

        @Override
        public void paintBorder(Component c, Graphics g, int x, int y, int width, int height)
        {
            if (includeBottomLine) {
                g.setColor(new Color(186, 186, 186));  // 206 if not textured
                g.fillRect(x, y + height - 1, width, 1);
            }
        }
    }
}
