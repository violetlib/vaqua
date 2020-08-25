/*
 * Changes Copyright (c) 2015-2016 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

/*
 * Copyright (c) 2011, 2012, Oracle and/or its affiliates. All rights reserved.
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
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.*;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicRootPaneUI;

// I have commented out the code that refers to AquaMenuBarUI, as that class at present cannot be cloned without losing
// support for the screen menu bar. The commented out code appears to handle the case where the application installs
// its own layered pane.

public class AquaRootPaneUI extends BasicRootPaneUI implements AncestorListener, WindowListener, ContainerListener {

    public final static String AQUA_WINDOW_STYLE_KEY = "Aqua.windowStyle";

    //final static int kDefaultButtonPaintDelayBetweenFrames = 50;
//    JButton fCurrentDefaultButton = null;
//    Timer fTimer = null;
    //static final boolean sUseScreenMenuBar = AquaMenuBarUI.getScreenMenuBarProperty();

    public static ComponentUI createUI(final JComponent c) {
        return new AquaRootPaneUI();
    }

    private static boolean forceActiveWindowDisplay;

    protected JRootPane rootPane;
    protected WindowHierarchyListener hierarchyListener;
    protected AncestorChangeListener ancestorChangeListener;
    protected PropertyChangeListener windowStylePropertyChangeListener;
    protected boolean isInitialized;
    protected AquaCustomStyledWindow customStyledWindow;
    protected int vibrantStyle = -1;

    /**
     * Set the debugging option to force all windows to display as active. Used to compare a Java window with a native
     * active window.
     */
    public static void setForceActiveWindowDisplay(boolean b) {
        if (b != forceActiveWindowDisplay) {
            forceActiveWindowDisplay = b;
            Window[] windows = Window.getWindows();
            for (Window w : windows) {
                if (w instanceof RootPaneContainer) {
                    RootPaneContainer rpc = (RootPaneContainer) w;
                    JRootPane rp = rpc.getRootPane();
                    AquaRootPaneUI ui = AquaUtils.getUI(rp, AquaRootPaneUI.class);
                    if (ui != null) {
                        boolean shouldBeActive = b || w.isActive();
                        boolean isActiveStyle = Boolean.TRUE.equals(rp.getClientProperty(AquaFocusHandler.FRAME_ACTIVE_PROPERTY));
                        if (shouldBeActive != isActiveStyle) {
                            rp.repaint();
                            updateComponentTreeUIActivation(rp, shouldBeActive);
                        }
                    }
                }
            }
        }
    }

    public void installUI(final JComponent c) {

        rootPane = (JRootPane) c;

        super.installUI(c);

        c.addAncestorListener(this);

        ancestorChangeListener = new AncestorChangeListener();

        if (c.isShowing() && c.isEnabled()) {
            updateDefaultButton(rootPane);
        }

        // for <rdar://problem/3689020> REGR: Realtime LAF updates no longer work
        //
        // because the JFrame parent has a LAF background set (why without a UI element I don't know!)
        // we have to set it from the root pane so when we are coming from metal we will set it to
        // the aqua color.
        // This is because the aqua color is a magical color that gets the background of the window,
        // so for most other LAFs the root pane changing is enough since it would be opaque, but for us
        // it is not since we are going to grab the one that was set on the JFrame. :(
        final Component parent = c.getParent();

        if (parent != null && parent instanceof JFrame) {
            final JFrame frameParent = (JFrame)parent;
            final Color bg = frameParent.getBackground();
            if (bg == null || bg instanceof UIResource) {
                frameParent.setBackground(UIManager.getColor("Panel.background"));
            }
        }

//        // for <rdar://problem/3750909> OutOfMemoryError swapping menus.
//        // Listen for layered pane/JMenuBar updates if the screen menu bar is active.
//        if (sUseScreenMenuBar) {
//            final JRootPane root = (JRootPane)c;
//            root.addContainerListener(this);
//            root.getLayeredPane().addContainerListener(this);
//        }
    }

    public void uninstallUI(final JComponent c) {
//        stopTimer();
        c.removeAncestorListener(this);

        if (customStyledWindow != null) {
            customStyledWindow.dispose();
            customStyledWindow = null;
        }

        removeVisualEffectView();

//        if (sUseScreenMenuBar) {
//            final JRootPane root = (JRootPane)c;
//            root.removeContainerListener(this);
//            root.getLayeredPane().removeContainerListener(this);
//        }

        isInitialized = false;

        super.uninstallUI(c);
    }

    @Override
    protected void installListeners(JRootPane root) {
        super.installListeners(root);
        hierarchyListener = new WindowHierarchyListener();
        root.addHierarchyListener(hierarchyListener);
    }

    @Override
    protected void uninstallListeners(JRootPane root) {
        root.removeHierarchyListener(hierarchyListener);
        hierarchyListener = null;
        root.removePropertyChangeListener("ancestor", ancestorChangeListener);
        ancestorChangeListener = null;
        super.uninstallListeners(root);
        if (windowStylePropertyChangeListener != null) {
            root.removePropertyChangeListener(windowStylePropertyChangeListener);
            windowStylePropertyChangeListener = null;
        }
    }

    /**
     * If the screen menu bar is active we need to listen to the layered pane of the root pane
     * because it holds the JMenuBar.  So, if a new layered pane was added, listen to it.
     * If a new JMenuBar was added, tell the menu bar UI, because it will need to update the menu bar.
     */
    public void componentAdded(final ContainerEvent e) {
        if (e.getContainer() instanceof JRootPane) {
            final JRootPane root = (JRootPane)e.getContainer();
            if (e.getChild() == root.getLayeredPane()) {
                final JLayeredPane layered = root.getLayeredPane();
                layered.addContainerListener(this);
            }
//        } else {
//            if (e.getChild() instanceof JMenuBar) {
//                final JMenuBar jmb = (JMenuBar)e.getChild();
//                final MenuBarUI mbui = jmb.getUI();
//
//                if (mbui instanceof AquaMenuBarUI) {
//                    final Window owningWindow = SwingUtilities.getWindowAncestor(jmb);
//
//                    // Could be a JDialog, and may have been added to a JRootPane not yet in a window.
//                    if (owningWindow != null && owningWindow instanceof JFrame) {
//                        ((AquaMenuBarUI)mbui).setScreenMenuBar((JFrame)owningWindow);
//                    }
//                }
//            }
        }
    }

    /**
     * Likewise, when the layered pane is removed from the root pane, stop listening to it.
     * If the JMenuBar is removed, tell the menu bar UI to clear the menu bar.
     */
    public void componentRemoved(final ContainerEvent e) {
        if (e.getContainer() instanceof JRootPane) {
            final JRootPane root = (JRootPane)e.getContainer();
            if (e.getChild() == root.getLayeredPane()) {
                final JLayeredPane layered = root.getLayeredPane();
                layered.removeContainerListener(this);
            }
//        } else {
//            if (e.getChild() instanceof JMenuBar) {
//                final JMenuBar jmb = (JMenuBar)e.getChild();
//                final MenuBarUI mbui = jmb.getUI();
//
//                if (mbui instanceof AquaMenuBarUI) {
//                    final Window owningWindow = SwingUtilities.getWindowAncestor(jmb);
//
//                    // Could be a JDialog, and may have been added to a JRootPane not yet in a window.
//                    if (owningWindow != null && owningWindow instanceof JFrame) {
//                        ((AquaMenuBarUI)mbui).clearScreenMenuBar((JFrame)owningWindow);
//                    }
//                }
//            }
        }
    }

    /**
     * Invoked when a property changes on the root pane. If the event
     * indicates the <code>defaultButton</code> has changed, this will
     * update the animation.
     * If the enabled state changed, it will start or stop the animation
     */
    public void propertyChange(final PropertyChangeEvent e) {
        super.propertyChange(e);

        final String prop = e.getPropertyName();
        if ("defaultButton".equals(prop) || "temporaryDefaultButton".equals(prop)) {
            // Change the animating button if this root is showing and enabled
            // otherwise do nothing - someone else may be active
            final JRootPane root = (JRootPane)e.getSource();

            if (root.isShowing() && root.isEnabled()) {
                updateDefaultButton(root);
            }
        } else if ("enabled".equals(prop) || AquaFocusHandler.FRAME_ACTIVE_PROPERTY.equals(prop)) {
            final JRootPane root = (JRootPane)e.getSource();
            if (root.isShowing()) {
                if (((Boolean)e.getNewValue()).booleanValue()) {
                    updateDefaultButton((JRootPane)e.getSource());
                } else {
//                    stopTimer();
                }
            }
        } else if (AquaVibrantSupport.BACKGROUND_STYLE_KEY.equals(prop)) {
            Object o = e.getNewValue();
            setupBackgroundStyle(o, true);
        }
    }

    /**
     * A property change listener for the {@code ancestor} property. The intent is for this listener to run after the
     * similar listener used by CPlatformWindow to configure itself from root pane client properties. The problem we
     * are trying to solve is that CPlatformWindow does not know about the full content view style bit and it clears
     * that bit every time it updates the NSWindow style mask.
     */
    protected class AncestorChangeListener implements PropertyChangeListener {
        @Override
        public void propertyChange(PropertyChangeEvent e) {
            if (e.getNewValue() != null) {
                refreshWindowStyleMask();
                if (windowStylePropertyChangeListener == null) {
                    windowStylePropertyChangeListener = new WindowStylePropertyChangeListener();
                    rootPane.addPropertyChangeListener(windowStylePropertyChangeListener);
                }
            }
        }
    }

    protected class WindowStylePropertyChangeListener implements PropertyChangeListener {
        @Override
        public void propertyChange(PropertyChangeEvent e) {
            String prop = e.getPropertyName();
            if (prop != null) {
                if (prop.equals("Window.closeable") || prop.equals("Window.minimizable")) {
                    // These properties can be changed after the window peer is created and they alter the window
                    // style mask.
                    refreshWindowStyleMask();
                }
            }
        }
    }

    protected void refreshWindowStyleMask() {
        if (customStyledWindow != null) {
            customStyledWindow.refreshWindowStyleMask();
        }
    }

//    synchronized void stopTimer() {
//        if (fTimer != null) {
//            fTimer.stop();
//            fTimer = null;
//        }
//    }

    synchronized void updateDefaultButton(final JRootPane root) {
        final JButton button = root.getDefaultButton();
        //System.err.println("in updateDefaultButton button = " + button);
//        fCurrentDefaultButton = button;
//        stopTimer();
//        if (button != null) {
//            fTimer = new Timer(kDefaultButtonPaintDelayBetweenFrames, new DefaultButtonPainter(root));
//            fTimer.start();
//        }
    }

//    class DefaultButtonPainter implements ActionListener {
//        JRootPane root;
//
//        public DefaultButtonPainter(final JRootPane root) {
//            this.root = root;
//        }
//
//        public void actionPerformed(final ActionEvent e) {
//            final JButton defaultButton = root.getDefaultButton();
//            if ((defaultButton != null) && defaultButton.isShowing()) {
//                if (defaultButton.isEnabled()) {
//                    defaultButton.repaint();
//                }
//            } else {
//                stopTimer();
//            }
//        }
//    }

    /**
     * This is sort of like viewDidMoveToWindow:.  When the root pane is put into a window
     * this method gets called for the notification.
     * We need to set up the listener relationship so we can pick up activation events.
     * And, if a JMenuBar was added before the root pane was added to the window, we now need
     * to notify the menu bar UI.
     */
    public void ancestorAdded(final AncestorEvent event) {
        // this is so we can handle window activated and deactivated events so
        // our swing controls can color/enable/disable/focus draw correctly
        final JComponent comp = event.getComponent();
        final Window owningWindow = SwingUtilities.getWindowAncestor(comp);
        JRootPane rp = comp instanceof JRootPane ? (JRootPane) comp : null;

        if (owningWindow != null) {
            // We get this message even when a dialog is opened and the owning window is a window
            // that could already be listened to. We should only be a listener once.
            // adding multiple listeners was the cause of <rdar://problem/3534047>
            // but the incorrect removal of them caused <rdar://problem/3617848>
            owningWindow.removeWindowListener(this);
            owningWindow.addWindowListener(this);
        }

        // The root pane has been added to the hierarchy.  If it's enabled update the default
        // button to start the throbbing.  Since the UI is a singleton make sure the root pane
        // we are checking has a default button before calling update otherwise we will stop
        // throbbing the current default button.
        if (rp != null && rp.isEnabled() && rp.getDefaultButton() != null) {
            updateDefaultButton(rp);
        }
    }

    /**
     * If the JRootPane was removed from the window we should clear the screen menu bar.
     * That's a non-trivial problem, because you need to know which window the JRootPane was in
     * before it was removed.  By the time ancestorRemoved was called, the JRootPane has already been removed
     */

    public void ancestorRemoved(final AncestorEvent event) { }
    public void ancestorMoved(final AncestorEvent event) { }

    public void windowActivated(final WindowEvent e) {
        updateWindowActivation(e, Boolean.TRUE);
    }

    public void windowDeactivated(final WindowEvent e) {
        updateWindowActivation(e, Boolean.FALSE);
    }

    public void windowOpened(final WindowEvent e) { }
    public void windowClosing(final WindowEvent e) { }

    public void windowClosed(final WindowEvent e) {
        // We know the window is closed so remove the listener.
        final Window w = e.getWindow();
        w.removeWindowListener(this);
    }

    public void windowIconified(final WindowEvent e) { }
    public void windowDeiconified(final WindowEvent e) { }
    public void windowStateChanged(final WindowEvent e) { }
    public void windowGainedFocus(final WindowEvent e) { }
    public void windowLostFocus(final WindowEvent e) { }

    private static void updateWindowActivation(WindowEvent e, Object active) {
        if (forceActiveWindowDisplay) {
            active = Boolean.TRUE;
        }
        Component c = (Component) e.getSource();
        c.repaint(); // much faster to make one repaint call rather than repainting individual components
        updateComponentTreeUIActivation(c, active);
    }

    private static void updateComponentTreeUIActivation(final Component c, Object active) {
        if (c instanceof javax.swing.JInternalFrame) {
            active = (((JInternalFrame)c).isSelected() ? Boolean.TRUE : Boolean.FALSE);
        }

        if (c instanceof javax.swing.JComponent) {
            ((javax.swing.JComponent)c).putClientProperty(AquaFocusHandler.FRAME_ACTIVE_PROPERTY, active);
        }

        Component[] children = null;

        if (c instanceof javax.swing.JMenu) {
            children = ((javax.swing.JMenu)c).getMenuComponents();
        } else if (c instanceof Container) {
            children = ((Container)c).getComponents();
        }

        if (children == null) return;

        for (final Component element : children) {
            updateComponentTreeUIActivation(element, active);
        }
    }

    @Override
    public final void update(final Graphics g, final JComponent c) {
        if (customStyledWindow != null) {
            customStyledWindow.paintBackground(g);
        } else if (c.isOpaque() || vibrantStyle >= 0) {
            AquaUtils.fillRect(g, c, AquaUtils.ERASE_IF_TEXTURED|AquaUtils.ERASE_IF_VIBRANT);
        }
        paint(g, c);
    }

    protected class WindowHierarchyListener implements HierarchyListener {
        @Override
        public void hierarchyChanged(HierarchyEvent e) {
            if (!isInitialized && e.getChangeFlags() == HierarchyEvent.DISPLAYABILITY_CHANGED) {
                // Add listener now because we want to be called after the window peer property change listener
                rootPane.addPropertyChangeListener("ancestor", ancestorChangeListener);
                configure();
            }
        }
    }

    /**
     * Configure or reconfigure the window based on root pane client properties.
     * This method has no effect if the root pane parent is not displayable.
     */
    public void configure() {
        if (rootPane.getParent() != null && rootPane.getParent().isDisplayable()) {
            isInitialized = true;
            updatePopupStyle(rootPane);
            installCustomWindowStyle();
            updateVisualEffectView();
        }
    }

    protected void updatePopupStyle(JRootPane rp) {
        Window w = SwingUtilities.getWindowAncestor(rp);
        if (w != null && w.getType() == Window.Type.POPUP) {
            Container cp = rp.getContentPane();
            if (cp != null) {
                int count = cp.getComponentCount();
                if (count == 1) {
                    Component c = cp.getComponent(0);
                    if (c instanceof JComponent) {
                        JComponent jc = (JComponent) c;
                        Object o = jc.getClientProperty(AquaVibrantSupport.POPUP_BACKGROUND_STYLE_KEY);
                        setupBackgroundStyle(o, false);
                        o = jc.getClientProperty(AquaVibrantSupport.POPUP_CORNER_RADIUS_KEY);
                        setupCornerRadius(o);
                    }
                }
            }
        }
    }

    protected void setupBackgroundStyle(Object o, boolean update) {
        int style = AquaVibrantSupport.parseVibrantStyle(o, false);
        if (style != vibrantStyle) {
            vibrantStyle = style;
            rootPane.setBackground(vibrantStyle >= 0 ? new Color(0, 0, 0, 0) : null);
            if (isInitialized && update) {
                updateVisualEffectView();
            }
        }
    }

    protected void setupCornerRadius(Object o) {
        float radius = 0;
        if (o != null) {
            if (o instanceof String) {
                String s = (String) o;
                if (s.equals("default")) {
                    radius = 6;
                }
            } else if (o instanceof Number) {
                radius = ((Number) o).floatValue();
            }
            if (radius < 0) {
                radius = 0;
            }
        }
        Window w = SwingUtilities.getWindowAncestor(rootPane);
        AquaUtils.setCornerRadius(w, radius);
    }

    protected void updateVisualEffectView() {
        Window w = SwingUtilities.getWindowAncestor(rootPane);
        if (w != null) {
            if (vibrantStyle >= 0) {
                try {
                    AquaVibrantSupport.addFullWindowVibrantView(w, vibrantStyle);
                } catch (IllegalArgumentException ex) {
                    System.err.println("Unable to install visual effect view: " + ex.getMessage());
                }
            } else {
                AquaVibrantSupport.removeFullWindowVibrantView(w);
                // TBD: cannot really undo this
            }
        }
    }

    protected void removeVisualEffectView() {
        Window w = SwingUtilities.getWindowAncestor(rootPane);
        if (w != null) {
            AquaVibrantSupport.removeFullWindowVibrantView(w);
        }
    }

    protected void installCustomWindowStyle() {
        if (customStyledWindow == null) {
            int style = getCustomWindowStyle();
            if (style >= 0) {
                Window w = SwingUtilities.getWindowAncestor(rootPane);
                if (w != null) {
                    try {
                        customStyledWindow = new AquaCustomStyledWindow(w, style);
                    } catch (IllegalArgumentException ex) {
                        AquaUtils.syslog("Unable to install custom window style: " + ex.getMessage());
                    }
                }
            }
        }
    }

    protected int getCustomWindowStyle() {
        Object o = rootPane.getClientProperty(AQUA_WINDOW_STYLE_KEY);
        if (o instanceof String) {
            if ("unifiedToolBar".equals(o)) {
                return AquaCustomStyledWindow.STYLE_UNIFIED;
            }
            if ("texturedToolBar".equals(o)) {
                return AquaCustomStyledWindow.STYLE_TEXTURED_HIDDEN;
            }
            if ("combinedToolBar".equals(o)) {
                return AquaCustomStyledWindow.STYLE_COMBINED;
            }
            if ("overlayTitleBar".equals(o)) {
                return AquaCustomStyledWindow.STYLE_NORMAL;
            }
            if ("transparentTitleBar".equals(o)) {
                return AquaCustomStyledWindow.STYLE_TRANSPARENT;
            }
            if ("noTitleBar".equals(o)) {
                return AquaCustomStyledWindow.STYLE_HIDDEN;
            }
        }

        return -1;
    }
}
