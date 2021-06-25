/*
 * Changes Copyright (c) 2015-2021 Alan Snyder.
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
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyVetoException;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.event.MouseInputAdapter;
import javax.swing.plaf.ActionMapUIResource;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.MenuBarUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicInternalFrameUI;

import org.jetbrains.annotations.NotNull;
import org.violetlib.aqua.AquaUtils.RecyclableSingleton;
import org.violetlib.jnr.NullPainter;
import org.violetlib.jnr.Painter;
import org.violetlib.jnr.aqua.AquaUIPainter;

/**
 * From AquaInternalFrameUI
 *
 * InternalFrame implementation for Aqua LAF
 *
 * We want to inherit most of the inner classes, but not the base class,
 * so be very careful about subclassing so you know you get what you want
 *
 */
public class AquaInternalFrameUI extends BasicInternalFrameUI implements SwingConstants, AquaComponentUI {
    protected static final String IS_PALETTE_PROPERTY = "JInternalFrame.isPalette";
    private static final String FRAME_TYPE = "JInternalFrame.frameType";
    private static final String NORMAL_FRAME = "normal";
    private static final String PALETTE_FRAME = "palette";
    private static final String OPTION_DIALOG = "optionDialog";

    // Instance variables
    PropertyChangeListener fPropertyListener;
    ComponentListener fComponentListener;
    private AWTEventListener fModifiedKeyListener;
    AquaInternalFrameBorder fAquaBorder;

    // for button tracking
    boolean fMouseOverPressedButton;
    int fWhichButtonPressed = -1;
    boolean fRollover = false;
    boolean fOption = false;
    boolean fDocumentEdited = false; // to indicate whether we should use the dirty document red dot.
    boolean fIsPallet;

    public int getWhichButtonPressed() {
        return fWhichButtonPressed;
    }

    public boolean getMouseOverPressedButton() {
        return fMouseOverPressedButton;
    }

    public boolean getRollover() {
        return fRollover;
    }

    public boolean getOption() {
        return fOption;
    }

    // ComponentUI Interface Implementation methods
    public static ComponentUI createUI(JComponent b) {
        return new AquaInternalFrameUI((JInternalFrame)b);
    }

    public AquaInternalFrameUI(JInternalFrame b) {
        super(b);
    }

    /// Inherit  (but be careful to check everything they call):
    public void installUI(JComponent c) {
//        super.installUI(c);  // Swing 1.1.1 has a bug in installUI - it doesn't check for null northPane
        frame = (JInternalFrame)c;
        frame.add(frame.getRootPane(), "Center");

        installDefaults();
        installListeners();
        installComponents();
        installKeyboardActions();

        configureFrameType();

        // We only have a southPane, for grow box room, created in setFrameType
        frame.setMinimumSize(new Dimension(fIsPallet ? 120 : 150, fIsPallet ? 39 : 65));
        LookAndFeel.installProperty(frame, "opaque", false);
    }

    private void configureFrameType()
    {
        String frameType = NORMAL_FRAME;

        Object paletteProp = frame.getClientProperty(IS_PALETTE_PROPERTY);
        if (paletteProp instanceof Boolean) {
            boolean isPalette = (Boolean) paletteProp;
            if (isPalette) {
                frameType = PALETTE_FRAME;
            }
        } else {
            Object frameTypeProp = frame.getClientProperty(FRAME_TYPE);
            if (frameTypeProp instanceof String) {
                frameType = (String) frameTypeProp;
            }
        }
        setFrameType(frameType);
    }

    public void setSouthPane(JComponent c) {
        if (southPane != null) {
            frame.remove(southPane);
            deinstallMouseHandlers(southPane);
        }
        if (c != null) {
            frame.add(c);
            installMouseHandlers(c);
        }
        southPane = c;
    }

    protected void installKeyboardActions() {
    } //$ Not Mac-ish - should we support?

    protected ResizeBox resizeBox;
    protected void installComponents() {
        JLayeredPane layeredPane = frame.getLayeredPane();
        if (resizeBox != null) {
            resizeBox.removeListeners();
            layeredPane.removeComponentListener(resizeBox);
            layeredPane.remove(resizeBox);
            resizeBox = null;
        }

        resizeBox = new ResizeBox(layeredPane);
        resizeBox.repositionResizeBox();

        layeredPane.add(resizeBox);
        layeredPane.setLayer(resizeBox, JLayeredPane.DRAG_LAYER);
        layeredPane.addComponentListener(resizeBox);

        resizeBox.addListeners();
        resizeBox.setVisible(frame.isResizable());
    }

    /// Inherit all the listeners - that's the main reason we subclass Basic!
    protected void installListeners() {
        fPropertyListener = new PropertyListener();
        frame.addPropertyChangeListener(fPropertyListener);
        fComponentListener = new MyComponentListener();
        frame.addComponentListener(fComponentListener);
        fModifiedKeyListener = new ModifierKeyListener();
        Toolkit.getDefaultToolkit().addAWTEventListener(fModifiedKeyListener, AWTEvent.KEY_EVENT_MASK);
        super.installListeners();
    }

    // uninstallDefaults
    // uninstallComponents
    protected void uninstallListeners() {
        super.uninstallListeners();
        frame.removeComponentListener(fComponentListener);
        frame.removePropertyChangeListener(fPropertyListener);
        Toolkit.getDefaultToolkit().removeAWTEventListener(fModifiedKeyListener);
    }

    protected void uninstallKeyboardActions() {
    }

    // Called when a DesktopIcon replaces an InternalFrame & vice versa
    //protected void replacePane(JComponent currentPane, JComponent newPane) {}
    protected void installMouseHandlers(JComponent c) {
        c.addMouseListener(borderListener);
        c.addMouseMotionListener(borderListener);
    }

    protected void deinstallMouseHandlers(JComponent c) {
        c.removeMouseListener(borderListener);
        c.removeMouseMotionListener(borderListener);
    }

    ActionMap createActionMap() {
        ActionMap map = new ActionMapUIResource();
        // add action for the system menu
        // Set the ActionMap's parent to the Auditory Feedback Action Map
        AquaLookAndFeel lf = (AquaLookAndFeel)UIManager.getLookAndFeel();
        ActionMap audioMap = lf.getAudioActionMap();
        map.setParent(audioMap);
        return map;
    }

    @Override
    public void appearanceChanged(@NotNull JComponent c, @NotNull AquaAppearance appearance) {
    }

    @Override
    public void activeStateChanged(@NotNull JComponent c, boolean isActive) {
    }

    public Dimension getPreferredSize(JComponent x) {
        Dimension preferredSize = super.getPreferredSize(x);
        Dimension minimumSize = frame.getMinimumSize();
        if (preferredSize.width < minimumSize.width) {
            preferredSize.width = minimumSize.width;
        }
        if (preferredSize.height < minimumSize.height) {
            preferredSize.height = minimumSize.height;
        }
        return preferredSize;
    }

    public void setNorthPane(JComponent c) {
        replacePane(northPane, c);
        northPane = c;
    }

    /**
     * Installs necessary mouse handlers on <code>newPane</code>
     * and adds it to the frame.
     * Reverse process for the <code>currentPane</code>.
     */
    protected void replacePane(JComponent currentPane, JComponent newPane) {
        if (currentPane != null) {
            deinstallMouseHandlers(currentPane);
            frame.remove(currentPane);
        }
        if (newPane != null) {
            frame.add(newPane);
            installMouseHandlers(newPane);
        }
    }

    // Our "Border" listener is shared by the AquaDesktopIcon
    protected MouseInputAdapter createBorderListener(JInternalFrame w) {
        return new AquaBorderListener();
    }

    /**
     * Mac-specific stuff begins here
     */
    void setFrameType(String frameType) {
        // Basic sets the background of the contentPane to null so it can inherit JInternalFrame.setBackground
        // but if *that's* null, we get the JDesktop, which makes ours look invisible!
        // So JInternalFrame has to have a background color
        // See Sun bugs 4268949 & 4320889
        Color bg = frame.getBackground();
        boolean replaceColor = (bg == null || bg instanceof UIResource);

        Font font = frame.getFont();
        boolean replaceFont = (font == null || font instanceof UIResource);

        boolean isPalette = false;
        if (frameType.equals(OPTION_DIALOG)) {
            fAquaBorder = new AquaInternalFrameBorder(frame, AquaUIPainter.TitleBarWidget.DOCUMENT_WINDOW);
            if (replaceColor) frame.setBackground(UIManager.getColor("InternalFrame.optionDialogBackground"));
            if (replaceFont) frame.setFont(UIManager.getFont("InternalFrame.optionDialogTitleFont"));
        } else if (frameType.equals(PALETTE_FRAME)) {
            fAquaBorder = new AquaInternalFrameBorder(frame, AquaUIPainter.TitleBarWidget.UTILITY_WINDOW);
            if (replaceColor) frame.setBackground(UIManager.getColor("InternalFrame.paletteBackground"));
            if (replaceFont) frame.setFont(UIManager.getFont("InternalFrame.paletteTitleFont"));
            isPalette = true;
        } else {
            fAquaBorder = new AquaInternalFrameBorder(frame, AquaUIPainter.TitleBarWidget.DOCUMENT_WINDOW);
            if (replaceColor) frame.setBackground(UIManager.getColor("InternalFrame.background"));
            if (replaceFont) frame.setFont(UIManager.getFont("InternalFrame.titleFont"));
        }
        // We don't get the borders from UIManager, in case someone changes them - this class will not work with
        // different borders.  If they want different ones, they have to make their own InternalFrameUI class

        frame.setBorder(new CompoundUIBorder(isPalette ? paletteWindowShadow.get() : documentWindowShadow.get(), fAquaBorder));
        fIsPallet = isPalette;
    }

    public boolean isDocumentEdited() {
        return fDocumentEdited;
    }

    public void setDocumentEdited(boolean flag) {
        fDocumentEdited = flag;
    }

    @Override
    public void update(Graphics g, JComponent c) {
        AppearanceManager.registerCurrentAppearance(c);
        paint(g, c);
        // The root pane must paint over the extra shadow
        JRootPane rp = frame.getRootPane();
        rp.setOpaque(true);
        Color bc = AquaUtils.getWindowBackground(rp);
        rp.setBackground(bc);
    }

    @Override
    public void paint(Graphics g, JComponent c) {
    }

    protected void doButtonAction(JInternalFrame frame, int whichButton) {
        switch (whichButton) {
            case AquaInternalFrameBorder.kCloseButton:
                frame.doDefaultCloseAction();
                break;

            case AquaInternalFrameBorder.kIconButton:
                if (frame.isIconifiable()) {
                    if (!frame.isIcon()) {
                        try {
                            frame.setIcon(true);
                        } catch(PropertyVetoException e1) {}
                    } else {
                        try {
                            frame.setIcon(false);
                        } catch(PropertyVetoException e1) {}
                    }
                }
                break;

            case AquaInternalFrameBorder.kGrowButton:
                if (frame.isMaximizable()) {
                    if (!frame.isMaximum()) {
                        try {
                            frame.setMaximum(true);
                        } catch(PropertyVetoException e5) {}
                    } else {
                        try {
                            frame.setMaximum(false);
                        } catch(PropertyVetoException e6) {}
                    }
                }
                break;

            default:
                AquaUtils.logError("AquaInternalFrameBorder should never get here!!!!");
                Thread.dumpStack();
                break;
        }
    }

/*
    // helpful debug drawing, shows component and content bounds
    public void paint(Graphics g, JComponent c) {
        super.paint(g, c);

        g.setColor(Color.green);
        g.drawRect(0, 0, frame.getWidth() - 1, frame.getHeight() - 1);

        Insets insets = frame.getInsets();
        g.setColor(Color.orange);
        g.drawRect(insets.left - 2, insets.top - 2, frame.getWidth() - insets.left - insets.right + 4, frame.getHeight() - insets.top - insets.bottom + 4);
    }
*/

    // Border Listener Class
    /**
     * Listens for border adjustments.
     */
    protected class AquaBorderListener extends MouseInputAdapter {
        // _x & _y are the mousePressed location in absolute coordinate system
        int _x, _y;
        // __x & __y are the mousePressed location in source view's coordinate system
        int __x, __y;
        Rectangle startingBounds;
        boolean fDraggingFrame;
        int resizeDir;

        protected final int RESIZE_NONE = 0;
        private boolean discardRelease = false;

        @Override
        public void mouseEntered(MouseEvent e) {
            updateRollover(e);
        }

        @Override
        public void mouseExited(MouseEvent e) {
            updateRollover(e);
        }

        public void mouseClicked(MouseEvent e) {
            if (didForwardEvent(e)) return;

            if (e.getClickCount() <= 1 || e.getSource() != getNorthPane()) return;

            if (frame.isIconifiable() && frame.isIcon()) {
                try {
                    frame.setIcon(false);
                } catch(PropertyVetoException e2) {}
            } else if (frame.isMaximizable()) {
                if (!frame.isMaximum()) try {
                    frame.setMaximum(true);
                } catch(PropertyVetoException e2) {}
                else try {
                    frame.setMaximum(false);
                } catch(PropertyVetoException e3) {}
            }
        }

        public void updateRollover(MouseEvent e) {
            fRollover = (isTitleBarDraggableArea(e) && fAquaBorder.getWithinRolloverArea(e.getX(), e.getY()));
            fOption = e.isAltDown();
            repaintButtons();
        }

        public void mouseReleased(MouseEvent e) {
            if (didForwardEvent(e)) return;

            fDraggingFrame = false;

            if (fWhichButtonPressed != -1) {
                int newButton = fAquaBorder.getWhichButtonHit(e.getX(), e.getY());

                int buttonPressed = fWhichButtonPressed;
                fWhichButtonPressed = -1;
                fMouseOverPressedButton = false;

                if (buttonPressed == newButton) {
                    fMouseOverPressedButton = false;
                    doButtonAction(frame, buttonPressed);
                    fRollover = false;
                    repaintButtons();
                }

                return;
            }

            if (discardRelease) {
                discardRelease = false;
                return;
            }
            if (resizeDir == RESIZE_NONE) getDesktopManager().endDraggingFrame(frame);
            else {
                Container c = frame.getTopLevelAncestor();
                if (c instanceof JFrame) {
                    ((JFrame)frame.getTopLevelAncestor()).getGlassPane().setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));

                    ((JFrame)frame.getTopLevelAncestor()).getGlassPane().setVisible(false);
                } else if (c instanceof JApplet) {
                    ((JApplet)c).getGlassPane().setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
                    ((JApplet)c).getGlassPane().setVisible(false);
                } else if (c instanceof JWindow) {
                    ((JWindow)c).getGlassPane().setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
                    ((JWindow)c).getGlassPane().setVisible(false);
                } else if (c instanceof JDialog) {
                    ((JDialog)c).getGlassPane().setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
                    ((JDialog)c).getGlassPane().setVisible(false);
                }
                getDesktopManager().endResizingFrame(frame);
            }
            _x = 0;
            _y = 0;
            __x = 0;
            __y = 0;
            startingBounds = null;
            resizeDir = RESIZE_NONE;
        }

        public void mousePressed(MouseEvent e) {
            if (didForwardEvent(e)) return;

            Point p = SwingUtilities.convertPoint((Component)e.getSource(), e.getX(), e.getY(), null);
            __x = e.getX();
            __y = e.getY();
            _x = p.x;
            _y = p.y;
            startingBounds = frame.getBounds();
            resizeDir = RESIZE_NONE;

            if (updatePressed(e)) { return; }

            if (!frame.isSelected()) {
                try {
                    frame.setSelected(true);
                } catch(PropertyVetoException e1) {}
            }

            if (isTitleBarDraggableArea(e)) {
                getDesktopManager().beginDraggingFrame(frame);
                fDraggingFrame = true;
                return;
            }

            if (e.getSource() == getNorthPane()) {
                getDesktopManager().beginDraggingFrame(frame);
                return;
            }

            if (!frame.isResizable()) { return; }

            if (e.getSource() == frame) {
                discardRelease = true;
                return;
            }
        }

        // returns true if we have handled the pressed
        public boolean updatePressed(MouseEvent e) {
            // get the component.
            fWhichButtonPressed = getButtonHit(e);
            fMouseOverPressedButton = true;
            repaintButtons();
            return (fWhichButtonPressed >= 0);
            // e.getX(), e.getY()
        }

        public int getButtonHit(MouseEvent e) {
            return fAquaBorder.getWhichButtonHit(e.getX(), e.getY());
        }

        public boolean isTitleBarDraggableArea(MouseEvent e) {
            if (e.getSource() != frame) return false;

            Point point = e.getPoint();
            Insets insets = frame.getInsets();

            if (point.y < insets.top - fAquaBorder.getTitleHeight()) return false;
            if (point.y > insets.top) return false;
            if (point.x < insets.left) return false;
            if (point.x > frame.getWidth() - insets.left - insets.right) return false;

            return true;
        }

        public void mouseDragged(MouseEvent e) {
// do not forward drags
//            if (didForwardEvent(e)) return;

            if (startingBounds == null) {
                // (STEVE) Yucky work around for bug ID 4106552
                return;
            }

            if (fWhichButtonPressed != -1) {
                // track the button we started on.
                int newButton = getButtonHit(e);
                fMouseOverPressedButton = (fWhichButtonPressed == newButton);
                repaintButtons();
                return;
            }

            Point p = SwingUtilities.convertPoint((Component)e.getSource(), e.getX(), e.getY(), null);
            int deltaX = _x - p.x;
            int deltaY = _y - p.y;
            int newX, newY;

            // Handle a MOVE
            if (!fDraggingFrame && e.getSource() != getNorthPane()) return;

            if (frame.isMaximum() || ((e.getModifiers() & InputEvent.BUTTON1_MASK) != InputEvent.BUTTON1_MASK)) {
                // don't allow moving of frames if maximized or left mouse
                // button was not used.
                return;
            }

            Dimension s = frame.getParent().getSize();
            int pWidth = s.width;
            int pHeight = s.height;

            Insets i = frame.getInsets();
            newX = startingBounds.x - deltaX;
            newY = startingBounds.y - deltaY;

            // Make sure we stay in-bounds
            if (newX + i.left <= -__x) newX = -__x - i.left;
            if (newY + i.top <= -__y) newY = -__y - i.top;
            if (newX + __x + i.right > pWidth) newX = pWidth - __x - i.right;
            if (newY + __y + i.bottom > pHeight) newY = pHeight - __y - i.bottom;

            getDesktopManager().dragFrame(frame, newX, newY);
            return;
        }

        public void mouseMoved(MouseEvent e) {
            if (didForwardEvent(e)) return;
            updateRollover(e);
        }

        // guards against accidental infinite recursion
        boolean isTryingToForwardEvent = false;
        boolean didForwardEvent(MouseEvent e) {
            if (isTryingToForwardEvent) return true; // we didn't actually...but we wound up back where we started.

            isTryingToForwardEvent = true;
            boolean didForwardEvent = didForwardEventInternal(e);
            isTryingToForwardEvent = false;

            return didForwardEvent;
        }

        boolean didForwardEventInternal(MouseEvent e) {
            if (fDraggingFrame) return false;

            Point originalPoint = e.getPoint();
            if (!isEventInWindowShadow(originalPoint)) return false;

            Container parent = frame.getParent();
            if (!(parent instanceof JDesktopPane)) return false;
            JDesktopPane pane = (JDesktopPane)parent;
            Point parentPoint = SwingUtilities.convertPoint(frame, originalPoint, parent);

        /*     // debug drawing
            Graphics g = parent.getGraphics();
            g.setColor(Color.red);
            g.drawLine(parentPoint.x, parentPoint.y, parentPoint.x, parentPoint.y);
        */

            Component hitComponent = findComponentToHitBehindMe(pane, parentPoint);
            if (hitComponent == null || hitComponent == frame) return false;

            Point hitComponentPoint = SwingUtilities.convertPoint(pane, parentPoint, hitComponent);
            hitComponent.dispatchEvent(new MouseEvent(hitComponent, e.getID(), e.getWhen(), e.getModifiers(), hitComponentPoint.x, hitComponentPoint.y, e.getClickCount(), e.isPopupTrigger(), e.getButton()));
            return true;
        }

        Component findComponentToHitBehindMe(JDesktopPane pane, Point parentPoint) {
            JInternalFrame[] allFrames = pane.getAllFrames();

            boolean foundSelf = false;
            for (JInternalFrame f : allFrames) {
                if (f == frame) { foundSelf = true; continue; }
                if (!foundSelf) continue;

                Rectangle bounds = f.getBounds();
                if (bounds.contains(parentPoint)) return f;
            }

            return pane;
        }

        boolean isEventInWindowShadow(Point point) {
            Rectangle bounds = frame.getBounds();
            Insets insets = frame.getInsets();
            insets.top -= fAquaBorder.getTitleHeight();

            if (point.x < insets.left) return true;
            if (point.x > bounds.width - insets.right) return true;
            if (point.y < insets.top) return true;
            if (point.y > bounds.height - insets.bottom) return true;

            return false;
        }
    }

    static void updateComponentTreeUIActivation(Component c, Object active) {
        if (c instanceof javax.swing.JComponent) {
            ((javax.swing.JComponent)c).putClientProperty(AquaFocusHandler.FRAME_ACTIVE_PROPERTY, active);
        }

        Component[] children = null;

        if (c instanceof javax.swing.JMenu) {
            children = ((javax.swing.JMenu)c).getMenuComponents();
        } else if (c instanceof Container) {
            children = ((Container)c).getComponents();
        }

        if (children != null) {
            for (Component element : children) {
                updateComponentTreeUIActivation(element, active);
            }
        }
    }

    class ModifierKeyListener implements AWTEventListener {
        @Override
        public void eventDispatched(AWTEvent e) {
            KeyEvent k = (KeyEvent) e;
            int keyCode = k.getKeyCode();
            if (keyCode == KeyEvent.VK_ALT || keyCode == KeyEvent.VK_ALT_GRAPH) {
                int id = e.getID();
                if (id == KeyEvent.KEY_PRESSED) {
                    fOption = true;
                } else if (id == KeyEvent.KEY_RELEASED) {
                    fOption = false;
                }
                repaintButtons();
            }
        }
    }

    class MyComponentListener extends ComponentAdapter {
        @Override
        public void componentResized(ComponentEvent e) {
            if (fAquaBorder != null) {
                fAquaBorder.invalidateLayout();
            }
        }
    }

    class PropertyListener implements PropertyChangeListener {
        public void propertyChange(PropertyChangeEvent e) {
            String name = e.getPropertyName();
            if (FRAME_TYPE.equals(name)) {
                configureFrameType();
            } else if (IS_PALETTE_PROPERTY.equals(name)) {
                configureFrameType();
            } else if ("windowModified".equals(name) || "Window.documentModified".equals(name)) {
                // repaint title bar
                setDocumentEdited((Boolean) e.getNewValue());
                frame.repaint(0, 0, frame.getWidth(), frame.getBorder().getBorderInsets(frame).top);
            } else if ("resizable".equals(name) || "state".equals(name) || "iconable".equals(name) || "maximizable".equals(name) || "closable".equals(name)) {
                if ("resizable".equals(name)) {
                    frame.revalidate();
                }
                frame.repaint();
            } else if ("title".equals(name)) {
                frame.repaint();
            } else if (JInternalFrame.IS_MAXIMUM_PROPERTY.equals(name)) {
                frame.repaint();
            } else if ("componentOrientation".equals(name)) {
                frame.revalidate();
                frame.repaint();
            } else if (JInternalFrame.IS_SELECTED_PROPERTY.equals(name)) {
                Component source = (Component)(e.getSource());
                updateComponentTreeUIActivation(source, frame.isSelected() ? Boolean.TRUE : Boolean.FALSE);
            } else if (JInternalFrame.MENU_BAR_PROPERTY.equals(name)) {
                menuBarInstalled();
            }
        }
    }

    private void repaintButtons() {
        if (fAquaBorder != null) {
            fAquaBorder.repaintButtonArea();
        }
    }

    private void menuBarInstalled() {
        // This method is called when a menu bar is installed in an internal frame.
        // Here we attempt to undo the nasty hack in JMenuBar that installs apple.laf.AquaMenuBarUI in
        // all menu bars when the screen menu bar is in use.

        JMenuBar installedMenuBar = frame.getJMenuBar();
        if (installedMenuBar.getUI().getClass().getName().endsWith("apple.laf.AquaMenuBarUI")) {
            // debug
            AquaUtils.logDebug("Fixing the UI for a menu bar installed on an internal frame");
            installedMenuBar.setUI((MenuBarUI) AquaMenuBarUI.createUI(installedMenuBar));
        }
    }

    static final InternalFrameShadow documentWindowShadow = new InternalFrameShadow() {
        Border getForegroundShadowBorder() {
            return new AquaUtils.SlicedShadowBorder(new AquaUtils.Painter() {
                public void paint(Graphics g, int x, int y, int w, int h) {
                    g.setColor(new Color(0, 0, 0, 196));
                    g.fillRoundRect(x, y, w, h, 16, 16);
                    g.fillRect(x, y + h - 16, w, 16);
                }
            }, new AquaUtils.Painter() {
                public void paint(Graphics g, int x, int y, int w, int h) {
                    g.setColor(new Color(0, 0, 0, 64));
                    g.drawLine(x + 2, y - 8, x + w - 2, y - 8);
                }
            },
            0, 7, 1.1f, 1.0f, 24, 51, 51, 25, 25, 25, 25);
        }

        Border getBackgroundShadowBorder() {
            return new AquaUtils.SlicedShadowBorder(new AquaUtils.Painter() {
                public void paint(Graphics g, int x, int y, int w, int h) {
                    g.setColor(new Color(0, 0, 0, 128));
                    g.fillRoundRect(x - 3, y - 8, w + 6, h, 16, 16);
                    g.fillRect(x - 3, y + h - 20, w + 6, 19);
                }
            }, new AquaUtils.Painter() {
                public void paint(Graphics g, int x, int y, int w, int h) {
                    g.setColor(new Color(0, 0, 0, 32));
                    g.drawLine(x, y - 11, x + w - 1, y - 11);
                }
            },
            0, 0, 3.0f, 1.0f, 10, 51, 51, 25, 25, 25, 25);
        }
    };

    static final InternalFrameShadow paletteWindowShadow = new InternalFrameShadow() {
        Border getForegroundShadowBorder() {
            return new AquaUtils.SlicedShadowBorder(new AquaUtils.Painter() {
                public void paint(Graphics g, int x, int y, int w, int h) {
                    g.setColor(new Color(0, 0, 0, 128));
                    g.fillRect(x, y + 3, w, h - 3);
                }
            }, null,
            0, 3, 1.0f, 1.0f, 10, 25, 25, 12, 12, 12, 12);
        }

        Border getBackgroundShadowBorder() {
            return getForegroundShadowBorder();
        }
    };

    @SuppressWarnings("serial") // Superclass is not serializable across versions
    static class CompoundUIBorder extends CompoundBorder implements UIResource {
        public CompoundUIBorder(Border inside, Border outside) { super(inside, outside); }
    }

    abstract static class InternalFrameShadow extends RecyclableSingleton<Border> {
        abstract Border getForegroundShadowBorder();
        abstract Border getBackgroundShadowBorder();

        protected Border getInstance() {
            Border fgShadow = getForegroundShadowBorder();
            Border bgShadow = getBackgroundShadowBorder();

            return new Border() {
                public Insets getBorderInsets(Component c) {
                    return fgShadow.getBorderInsets(c);
                }

                public boolean isBorderOpaque() {
                    return false;
                }

                public void paintBorder(Component c, Graphics g, int x, int y, int w, int h) {
                    if (((JInternalFrame)c).isSelected()) {
                        fgShadow.paintBorder(c, g, x, y, w, h);
                    } else {
                        bgShadow.paintBorder(c, g, x, y, w, h);
                    }
                }
            };
        }
    }

    static final RecyclableSingleton<Icon> RESIZE_ICON = new RecyclableSingleton<Icon>() {
        @Override
        protected Icon getInstance() {
            return new AquaIcon.ScalingNativeRenderedIcon(11, 11) {
                @Override
                public Painter getPainter(int width, int height) {

                    // No resize handles in Yosemite
//                    iconState.state.set(Widget.GROW_BOX_TEXTURED);
//                    iconState.state.set(WindowType.UTILITY);

                    return new NullPainter(null);
                }
            };
        }
    };

    @SuppressWarnings("serial") // Superclass is not serializable across versions
    class ResizeBox extends JLabel implements MouseListener, MouseMotionListener, MouseWheelListener, ComponentListener, PropertyChangeListener, UIResource {
        protected final JLayeredPane layeredPane;
        protected Dimension originalSize;
        protected Point originalLocation;

        public ResizeBox(JLayeredPane layeredPane) {
            super(RESIZE_ICON.get());
            setSize(11, 11);
            this.layeredPane = layeredPane;

            addMouseListener(this);
            addMouseMotionListener(this);
            addMouseWheelListener(this);
        }

        void addListeners() {
            frame.addPropertyChangeListener("resizable", this);
        }

        void removeListeners() {
            frame.removePropertyChangeListener("resizable", this);
        }

        void repositionResizeBox() {
            if (frame == null) { setSize(0, 0); } else { setSize(11, 11); }
            setLocation(layeredPane.getWidth() - 12, layeredPane.getHeight() - 12);
        }

        void resizeInternalFrame(Point pt) {
            if (originalLocation == null || frame == null) return;

            Container parent = frame.getParent();
            if (!(parent instanceof JDesktopPane)) return;

            Point newPoint = SwingUtilities.convertPoint(this, pt, frame);
            int deltaX = originalLocation.x - newPoint.x;
            int deltaY = originalLocation.y - newPoint.y;
            Dimension min = frame.getMinimumSize();
            Dimension max = frame.getMaximumSize();

            int newX = frame.getX();
            int newY = frame.getY();
            int newW = frame.getWidth();
            int newH = frame.getHeight();

            Rectangle parentBounds = parent.getBounds();

            if (originalSize.width - deltaX < min.width) {
                deltaX = originalSize.width - min.width;
            }  else if (originalSize.width - deltaX > max.width) {
                deltaX = -(max.width - originalSize.width);
            }

            if (newX + originalSize.width - deltaX > parentBounds.width) {
                deltaX = newX + originalSize.width - parentBounds.width;
            }

            if (originalSize.height - deltaY < min.height) {
                deltaY = originalSize.height - min.height;
            }  else if (originalSize.height - deltaY > max.height) {
                deltaY = -(max.height - originalSize.height);
            }

            if (newY + originalSize.height - deltaY > parentBounds.height) {
                deltaY = newY + originalSize.height - parentBounds.height;
            }

            newW = originalSize.width - deltaX;
            newH = originalSize.height - deltaY;

            getDesktopManager().resizeFrame(frame, newX, newY, newW, newH);
        }

        boolean testGrowboxPoint(int x, int y, int w, int h) {
            return (w - x) + (h - y) < 12;
        }

        void forwardEventToFrame(MouseEvent e) {
            Point pt = new Point();
            Component c = getComponentToForwardTo(e, pt);
            if (c == null) return;
            c.dispatchEvent(new MouseEvent(c, e.getID(), e.getWhen(), e.getModifiers(), pt.x, pt.y, e.getClickCount(), e.isPopupTrigger(), e.getButton()));
        }

        Component getComponentToForwardTo(MouseEvent e, Point dst) {
            if (frame == null) return null;
            Container contentPane = frame.getContentPane();
            if (contentPane == null) return null;
            Point pt = SwingUtilities.convertPoint(this, e.getPoint(), contentPane);
            Component c = SwingUtilities.getDeepestComponentAt(contentPane, pt.x, pt.y);
            if (c == null) return null;
            pt = SwingUtilities.convertPoint(contentPane, pt, c);
            if (dst != null) dst.setLocation(pt);
            return c;
        }

        public void mouseClicked(MouseEvent e) {
            forwardEventToFrame(e);
        }

        public void mouseEntered(MouseEvent e) { }

        public void mouseExited(MouseEvent e) { }

        public void mousePressed(MouseEvent e) {
            if (frame == null) return;

            if (frame.isResizable() && !frame.isMaximum() && testGrowboxPoint(e.getX(), e.getY(), getWidth(), getHeight())) {
                originalLocation = SwingUtilities.convertPoint(this, e.getPoint(), frame);
                originalSize = frame.getSize();
                getDesktopManager().beginResizingFrame(frame, SwingConstants.SOUTH_EAST);
                return;
            }

            forwardEventToFrame(e);
        }

        public void mouseReleased(MouseEvent e) {
            if (originalLocation != null) {
                resizeInternalFrame(e.getPoint());
                originalLocation = null;
                getDesktopManager().endResizingFrame(frame);
                return;
            }

            forwardEventToFrame(e);
        }

        public void mouseDragged(MouseEvent e) {
            resizeInternalFrame(e.getPoint());
            repositionResizeBox();
        }

        public void mouseMoved(MouseEvent e) { }

        public void mouseWheelMoved(MouseWheelEvent e) {
            Point pt = new Point();
            Component c = getComponentToForwardTo(e, pt);
            if (c == null) return;
            c.dispatchEvent(new MouseWheelEvent(c, e.getID(), e.getWhen(),
                    e.getModifiers(), pt.x, pt.y, e.getXOnScreen(), e.getYOnScreen(),
                    e.getClickCount(), e.isPopupTrigger(), e.getScrollType(),
                    e.getScrollAmount(), e.getWheelRotation(),
                    e.getPreciseWheelRotation()));
        }

        public void componentResized(ComponentEvent e) {
            repositionResizeBox();
        }

        public void componentShown(ComponentEvent e) {
            repositionResizeBox();
        }

        public void componentMoved(ComponentEvent e) {
            repositionResizeBox();
        }

        public void componentHidden(ComponentEvent e) { }

        public void propertyChange(PropertyChangeEvent evt) {
            if (!"resizable".equals(evt.getPropertyName())) return;
            setVisible(Boolean.TRUE.equals(evt.getNewValue()));
        }
    }
}
