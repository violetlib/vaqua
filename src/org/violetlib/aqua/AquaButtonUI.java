/*
 * Copyright (c) 2015-2026 Alan Snyder.
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
import java.awt.geom.RoundRectangle2D;
import java.beans.PropertyChangeEvent;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicButtonListener;
import javax.swing.plaf.basic.BasicButtonUI;
import javax.swing.plaf.basic.BasicHTML;
import javax.swing.text.View;

import org.jetbrains.annotations.*;
import org.violetlib.aqua.AquaUtils.RecyclableSingleton;
import org.violetlib.aqua.AquaUtils.RecyclableSingletonFromDefaultConstructor;
import org.violetlib.jnr.Insets2D;
import org.violetlib.jnr.aqua.AquaUIPainter.Size;
import org.violetlib.jnr.aqua.LayoutConfiguration;

public class AquaButtonUI extends BasicButtonUI
  implements AquaUtilControlSize.Sizeable, FocusRingOutlineProvider, ToolbarSensitiveUI, AquaComponentUI,
  SystemPropertyChangeManager.SystemPropertyChangeListener {

    // This UI is shared.
    // Button borders may also be shared.
    // All button configuration state must be in the button itself.

    // Important programming note:
    //
    // Do not call b.getDisabledIcon() or b.getDisabledSelectedIcon().
    // Use the static methods defined in AquaButtonSupport instead.

    public static final String BUTTON_TYPE = "JButton.buttonType";
    public static final String SEGMENTED_BUTTON_POSITION = "JButton.segmentPosition";
    public static final String SELECTED_STATE_KEY = "JButton.selectedState";
    public static final String ENABLE_TRANSLUCENT_COLORS_KEY = "JButton.enableTranslucentColors";

    public static final float OUTLINE_OFFSET = 0;
    public static final float OUTLINE_CORNER = 9;

    public static final String LAYOUT_CONFIGURATION_PROPERTY = "Aqua.Button.LayoutConfiguration";
    public static final String DEFAULT_FONT_PROPERTY = "Aqua.Button.DefaultFont";
    protected static final String COLOR_CHOOSER_OWNER_PROPERTY = "Aqua.Button.ColorChooserOwner";
    protected static final String SPECIAL_ICON_PROPERTY = "Aqua.Button.SpecialIcon";
    protected static final String CACHED_TOOLBAR_STATUS_PROPERTY = "Aqua.Button.IsToolbarButton";

    protected static final RecyclableSingleton<AquaButtonUI> buttonUI = new RecyclableSingletonFromDefaultConstructor<>(AquaButtonUI.class);

    private static boolean isConfiguring;

    public static ComponentUI createUI(JComponent c) {
        return buttonUI.get();
    }

    @Override
    public void installUI(JComponent c) {
        super.installUI(c);
        AquaButtonSupport.removeCachedIcons((AbstractButton) c);
    }

    @Override
    protected void installDefaults(@NotNull AbstractButton b) {
        // load shared instance defaults
        String pp = getPropertyPrefix();
        setButtonMarginIfNeeded(b, UIManager.getInsets(pp + "margin"));
        AquaUtils.installFont(b, pp + "font");
        LookAndFeel.installProperty(b, "opaque", false);
        b.putClientProperty(DEFAULT_FONT_PROPERTY, b.getFont());
        initializeToolbarStatus(b);
        configure(b);
        AquaUtils.configureFocusable(b);
    }

    @Override
    public void systemPropertyChanged(@NotNull JComponent c, @Nullable Object type) {
        if (OSXSystemProperties.USER_PREFERENCE_CHANGE_TYPE.equals(type)) {
            AquaUtils.configureFocusable(c);
        }
    }

    /**
     * Configure a button.
     * @param b The button component to be configured.
     */
    public void configure(@NotNull AbstractButton b) {

        if (isConfiguring) {
            // Avoid recursion caused by installing the default font as part of configuring the button
            return;
        }

        isConfiguring = true;
        try {

            // The configuration of a button is rather complex. It potentially affects the border, the font, the
            // foreground color, the minimum and preferred component sizes. The configuration potentially depends upon
            // client properties (for button type, segment position, and size variant), whether the button is contained
            // in a toolbar, whether the button is a member of a button group, whether the button has an icon or any
            // child components (e.g. images). Instead of trying to figure out exactly which parts of the configuration
            // depend upon which inputs, we do a complete reconfiguration whenever an input has potentially changed.

            // Unfortunately, there are no notifications when a button is added to or removed from a button group.
            // Button group membership potentially affects layout and painting, but only of JToggleButtons.

            // Note that the choice of border does not depend upon the size variant, but the border may use the size
            // variant as part of its configuration of the button. The border performs all of the configuration based on
            // a defined button type.

            b.putClientProperty(LAYOUT_CONFIGURATION_PROPERTY, null);

            boolean isToolbar = AquaButtonSupport.isToolbar(b);
            AquaButtonExtendedTypes.TypeSpecifier type = AquaButtonExtendedTypes.getTypeSpecifier(b, isToolbar);
            installBorder(b, type, isToolbar);

            AquaButtonBorder border = getAquaButtonBorder(b);
            LayoutConfiguration g = border != null ? border.determineLayoutConfiguration(b) : null;
            b.putClientProperty(LAYOUT_CONFIGURATION_PROPERTY, g);

            if (border != null) {
                if (border.isRolloverEnabled(b)) {
                    LookAndFeel.installProperty(b, "rolloverEnabled", true);
                }
                int iconTextGap = border.getIconTextGap(b);
                LookAndFeel.installProperty(b, "iconTextGap", iconTextGap);
            }

            // Perform configuration of the button based on the size variant, whether specified or implied.
            // This may change the button font, foreground color, and layout sizes.
            Object widget = g != null ? g.getWidget() : null;
            Size size = g != null ? g.getSize() : null;
            if (size == null) {
                size = AquaUtils.getSize(b, isToolbar, widget);
            }
            if (AquaUtilControlSize.isOKToInstallDefaultFont(b)) {
                Font df = getFontForButton(b, size);
                b.setFont(df);
            }

            AquaButtonSupport.updateTemplateIconStatus(b);

            if (!AquaButtonSupport.isColorWell(b)) {
                AquaButtonSupport.disconnectColorChooser(b);
            }

            b.setRequestFocusEnabled(false);

            b.revalidate();
            b.repaint();
        } finally {
            isConfiguring = false;
        }
    }

    /**
     * Install the appropriate border for a button.
     */
    protected void installBorder(AbstractButton b, AquaButtonExtendedTypes.TypeSpecifier type, boolean isToolbar) {
        Border customBorder = type != null ? type.getBorder() : null;
        if (customBorder != null) {
            b.setBorder(customBorder);
        } else {
            Border oldBorder = b.getBorder();
            if (oldBorder == null || oldBorder instanceof UIResource) {
                Border border = getDefaultBorder(b, isToolbar);
                if (border == null) {
                    border = new AquaPushButtonBorder();
                }
                b.setBorder(border);
            }
        }
    }

    /**
     * Return a default border for a button component that does not specify any client properties that we understand.
     * @param b The button component.
     * @return the border to use for the button component.
     */
    protected Border getDefaultBorder(@NotNull AbstractButton b, boolean isToolbar) {
        if (isToolbar) {
            if (b instanceof JToggleButton) {
                return AquaButtonBorder.getToolBarToggleButtonBorder();
            } else {
                return AquaButtonBorder.getToolBarPushButtonBorder();
            }
        } else {
            if (b instanceof JToggleButton) {
                return AquaButtonBorder.getToggleButtonBorder();
            } else {
                return AquaButtonBorder.getPushButtonBorder();
            }
        }
    }

    protected @Nullable AquaButtonBorder getAquaButtonBorder(@NotNull JComponent c) {
        return AquaBorderSupport.get(c, AquaButtonBorder.class);
    }

    @Override
    public @Nullable Shape getFocusRingOutline(@NotNull JComponent c) {
        Border border = c.getBorder();
        FocusRingOutlineProvider p = AquaBorderSupport.get(border, FocusRingOutlineProvider.class);
        if (p != null) {
            return p.getFocusRingOutline(c);
        }
        int width = c.getWidth();
        int height = c.getHeight();
        return new RoundRectangle2D.Double(OUTLINE_OFFSET, OUTLINE_OFFSET,
          width-2*OUTLINE_OFFSET, height-2*OUTLINE_OFFSET, OUTLINE_CORNER, OUTLINE_CORNER);
    }

    @Override
    public void applySizeFor(JComponent c, Size size, boolean isDefaultSize) {
        configure((AbstractButton) c);
    }

    /**
     * Identify the font to use for a button when the application has not installed a font.
     * The font may be chosen based on the button type and size.
     */
    protected @NotNull Font getFontForButton(@NotNull AbstractButton b, @NotNull Size size) {
        Font base = AquaButtonSupport.getDefaultFontPropertyValue(b);
        if (base == null) {
            // should not happen
            base = new Font("Default", Font.PLAIN, 12);
        }
        AquaButtonBorder border = getAquaButtonBorder(b);
        return border != null
          ? border.getCustomDefaultFont(b, size, base)
          : AquaUtilControlSize.getFontForSize(base, size);
    }

    protected Color getDefaultForegroundColor(AbstractButton b, @NotNull AquaAppearance appearance) {
        boolean isEnabled = b.getModel().isEnabled();
        Color existingColor = b.getForeground();
        if (existingColor == null || existingColor instanceof UIResource || !isEnabled) {
            // Most buttons do not display text differently when the window is inactive
            if (useSelectedForeground(b)) {
                return appearance.getColor("alternateSelectedControlText");
            } else if (useSelectedDisabledForeground(b)) {
                return appearance.getColor("selectedControlText_disabled");
            } else if (useDisabledForeground(b)) {
                return appearance.getColor("controlText_disabled");
            } else {
                return appearance.getColor("controlText");
            }
        }
        return existingColor;
    }

    private boolean useSelectedForeground(AbstractButton b) {
        if (b instanceof JToggleButton && !(b instanceof JCheckBox) && !(b instanceof JRadioButton)) {
            return b.getModel().isSelected() && b.isEnabled();
        }
        return false;
    }

    private boolean useSelectedDisabledForeground(AbstractButton b) {
        if (b instanceof JToggleButton && !(b instanceof JCheckBox) && !(b instanceof JRadioButton)) {
            return b.getModel().isSelected() && !b.isEnabled();
        }
        return false;
    }
    private boolean useDisabledForeground(AbstractButton b) {
        return !b.getModel().isEnabled();
    }

    private void initializeToolbarStatus(@NotNull AbstractButton b) {
        Boolean isToolbar = AquaUtils.isOnToolbar(b);
        b.putClientProperty(CACHED_TOOLBAR_STATUS_PROPERTY, isToolbar);
    }

    @Override
    public void toolbarStatusChanged(@NotNull JComponent c) {
        AbstractButton b = (AbstractButton) c;
        Boolean isToolbar = AquaUtils.isOnToolbar(b);
        Object oldStatus = b.getClientProperty(CACHED_TOOLBAR_STATUS_PROPERTY);
        if (!isToolbar.equals(oldStatus)) {
            b.putClientProperty(CACHED_TOOLBAR_STATUS_PROPERTY, isToolbar);
            configure(b);
        }
    }

    protected void setButtonMarginIfNeeded(AbstractButton b, Insets insets) {
        Insets margin = b.getMargin();
        if (margin == null || (margin instanceof UIResource)) {
            b.setMargin(insets);
        }
    }

    @Override
    protected void installListeners(AbstractButton b) {
        AquaButtonListener listener = createButtonListener(b);
        // put the listener in the button's client properties so that we can find it later
        b.putClientProperty(this, listener);
        b.addMouseListener(listener);
        b.addMouseMotionListener(listener);
        b.addFocusListener(listener);
        b.addPropertyChangeListener(listener);
        b.addChangeListener(listener);
        b.addActionListener(listener);
        b.addItemListener(listener);
        if (AquaButtonSupport.isToolbarSensitive(b)) {
            AquaUtils.installToolbarSensitivity(b);
        }
        AquaUtilControlSize.addSizePropertyListener(b);
        OSXSystemProperties.register(b);
        AppearanceManager.install(b);
    }

    @Override
    protected void installKeyboardActions(AbstractButton b) {
        BasicButtonListener listener = (BasicButtonListener)b.getClientProperty(this);
        if (listener != null) {
            listener.installKeyboardActions(b);
        }
    }

    @Override
    public void uninstallUI(JComponent c) {
        AquaButtonSupport.disconnectColorChooser((AbstractButton)c);
        uninstallKeyboardActions((AbstractButton)c);
        uninstallListeners((AbstractButton)c);
        uninstallDefaults((AbstractButton)c);
        AquaButtonSupport.removeCachedIcons((AbstractButton) c);
    }

    @Override
    protected void uninstallKeyboardActions(AbstractButton b) {
        BasicButtonListener listener = (BasicButtonListener)b.getClientProperty(this);
        if (listener != null) listener.uninstallKeyboardActions(b);
    }

    @Override
    protected void uninstallListeners(AbstractButton b) {
        AppearanceManager.uninstall(b);
        AquaButtonListener listener = (AquaButtonListener)b.getClientProperty(this);
        b.putClientProperty(this, null);
        if (listener != null) {
            b.removeItemListener(listener);
            b.removeMouseListener(listener);
            b.removeMouseMotionListener(listener);
            b.removeFocusListener(listener);
            b.removeChangeListener(listener);
            b.removePropertyChangeListener(listener);
            b.removeActionListener(listener);
        }
        AquaUtilControlSize.removeSizePropertyListener(b);
        OSXSystemProperties.unregister(b);
        AquaUtils.uninstallToolbarSensitivity(b);
    }

    @Override
    protected void uninstallDefaults(AbstractButton b) {
        LookAndFeel.uninstallBorder(b);
        AquaUtilControlSize.uninstallDefaultFont(b);
    }

    @Override
    protected @NotNull AquaButtonListener createButtonListener(AbstractButton b) {
        return new AquaButtonListener(b);
    }

    @Override
    public void update(Graphics g, JComponent c) {
        paint(g, c);
    }

    @Override
    public void paint(Graphics g, JComponent c) {
        AppearanceManager.withContext(g, c, this::paint);
    }

    protected void paint(@NotNull Graphics2D g, @NotNull JComponent c, @NotNull PaintingContext pc) {
        if (c.isOpaque()) {
            g.setColor(c.getBackground());
            g.fillRect(0, 0, c.getWidth(),c.getHeight());
        }
        paint(g, (AbstractButton) c, pc);

    }

    protected void paint(@NotNull Graphics2D g, @NotNull AbstractButton b, @NotNull PaintingContext pc) {
        Rectangle viewRect = new Rectangle(b.getWidth(), b.getHeight());
        AquaButtonBorder border = b.isBorderPainted() ? getAquaButtonBorder(b) : null;
        Icon icon = AquaButtonSupport.getIcon(b, pc);
        if (border != null) {
            border.paintButton(g, b, icon, viewRect, pc);
        } else {
            paintButtonDefault(g, b, icon, viewRect, pc);
        }
    }

    /**
     * Paint a button whose appearance is not defined by VAqua.
     */
    protected void paintButtonDefault(@NotNull Graphics2D g,
                                      @NotNull AbstractButton b,
                                      @Nullable Icon icon,
                                      @NotNull Rectangle viewRect,
                                      @NotNull PaintingContext pc) {
        ButtonModel model = b.getModel();
        boolean isColorWell = AquaButtonSupport.isColorWell(b);
        if (isColorWell) {
            // we are overdrawing here with translucent colors so we get
            // a darkening effect. How can we avoid it. Try clear rect?
            if (b.isOpaque()) {
                g.setColor(b.getBackground());
                g.fillRect(viewRect.x, viewRect.y, viewRect.width, viewRect.height);
            }
        }
        int width = b.getWidth();
        int height = b.getHeight();
        if (b.isOpaque()) {
            Insets i = b.getInsets();
            viewRect.x = i.left - 2;
            viewRect.y = i.top - 2;
            viewRect.width = width - (i.right + viewRect.x) + 4;
            viewRect.height = height - (i.bottom + viewRect.y) + 4;
            if (b.isContentAreaFilled() || model.isSelected()) {
                if (model.isSelected()) {
                    // Toggle buttons
                    g.setColor(b.getBackground().darker());
                } else {
                    g.setColor(b.getBackground());
                }
                g.fillRect(viewRect.x, viewRect.y, viewRect.width, viewRect.height);
            }
        }
        Color fc = getDefaultForegroundColor(b, pc.appearance);
        Insets2D insets = AquaUtils.asInsets2D(b.getInsets());
        AquaButtonSupport.paintIconAndText(g, pc, b, null, null, insets, icon, fc, fc, viewRect, null, false);
    }

    @Override
    protected final void paintText(Graphics g, AbstractButton b, Rectangle localTextRect, String text) {
        throw new UnsupportedOperationException("This method should not be used");
    }

    @Override
    protected final void paintText(Graphics g, JComponent c, Rectangle localTextRect, String text) {
        throw new UnsupportedOperationException("This method should not be used");
    }

    /**
     * Button states that can have different icons in Swing.
     */
    enum ButtonIconState {
        DISABLED,
        DISABLED_SELECTED,
        PRESSED,
        ROLLOVER,
        ROLLOVER_SELECTED,
        SELECTED,
        DEFAULT
    }

    @Override
    protected void paintButtonPressed(Graphics g, AbstractButton b) {
        paint(g, b);
    }

    @Override
    public Dimension getMinimumSize(JComponent c) {
        Dimension d = getPreferredSize(c);
        View v = (View)c.getClientProperty(BasicHTML.propertyKey);
        if (v != null) {
            d.width -= v.getPreferredSpan(View.X_AXIS) - v.getMinimumSpan(View.X_AXIS);
        }
        return d;
    }

    @Override
    public Dimension getPreferredSize(JComponent c) {
        AbstractButton b = (AbstractButton) c;
        AquaButtonBorder border = getAquaButtonBorder(c);
        if (border != null) {
            return border.getPreferredButtonSize(b);
        } else {
            Icon ic = b.getIcon();
            Dimension iconSize = ic != null ? new Dimension(ic.getIconWidth(), ic.getIconHeight()) : null;
            return AquaButtonSupport.getBasicPreferredButtonSize(b, iconSize, null, null);
        }
    }

    @Override
    public Dimension getMaximumSize(JComponent c) {
        Dimension d = getPreferredSize(c);
        View v = (View)c.getClientProperty(BasicHTML.propertyKey);
        if (v != null) {
            d.width += v.getMaximumSpan(View.X_AXIS) - v.getPreferredSpan(View.X_AXIS);
        }
        return d;
    }

    class AquaButtonListener extends BasicButtonListener implements ActionListener, ItemListener {
        protected AbstractButton b;

        public AquaButtonListener(AbstractButton b) {
            super(b);
            this.b = b;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            // If the button is a color well and no other action listeners are defined, bring up a color chooser.
            if (AquaButtonSupport.isColorWell(b)) {
                ActionListener[] listeners = b.getActionListeners();
                if (listeners.length == 1) {
                    AquaButtonSupport.toggleColorChooser(b);
                }
            }
        }

        @Override
        public void itemStateChanged(ItemEvent e) {
            AquaButtonSupport.toggleButtonStateChanged(b);
        }

        @Override
        public void focusGained(FocusEvent e) {
            ((Component)e.getSource()).repaint();
        }

        @Override
        public void focusLost(FocusEvent e) {
            // 10-06-03 VL: [Radar 3187049]
            // If focusLost arrives while the button has been left-clicked this would disarm the button,
            // causing actionPerformed not to fire on mouse release!
            //b.getModel().setArmed(false);
            b.getModel().setPressed(false);
            ((Component)e.getSource()).repaint();
        }

        @Override
        public void propertyChange(PropertyChangeEvent e) {
            super.propertyChange(e);

            String propertyName = e.getPropertyName();

            if ("icon".equals(propertyName) || "text".equals(propertyName)) {
                configure(b);
                return;
            }

            if (propertyName != null && !propertyName.contains(".") && propertyName.endsWith("Icon")) {
                AquaButtonSupport.updateTemplateIconStatus(b);
                return;
            }

            if (BUTTON_TYPE.equals(propertyName)) {
                configure(b);
                return;
            }

            if (SEGMENTED_BUTTON_POSITION.equals(propertyName)) {
                configure(b);
                return;
            }

            if (AbstractButton.VERTICAL_ALIGNMENT_CHANGED_PROPERTY.equals(propertyName)
              || AbstractButton.VERTICAL_TEXT_POSITION_CHANGED_PROPERTY.equals(propertyName)
              || "font".equals(propertyName)) {
                // A change to the preferred content height can change the selected button widget
                configure(b);
                return;
            }

            if ("componentOrientation".equals(propertyName)) {
                configure(b);
            }

            if ("ancestor".equals(propertyName)) {
                if (!b.isDisplayable()) {
                    AquaButtonSupport.disconnectColorChooser(b);
                }
            }
        }

        @Override
        public void mousePressed(MouseEvent e) {
            if (SwingUtilities.isLeftMouseButton(e) ) {
                AbstractButton b = (AbstractButton) e.getSource();
                if (b.contains(e.getX(), e.getY())) {
                    Object data = willHandleButtonPress(b);
                    super.mousePressed(e);
                    didHandleButtonPress(b, data);
                }
            }
        }
    }

    protected Object willHandleButtonPress(AbstractButton b) {
        return null;
    }

    protected void didHandleButtonPress(AbstractButton b, Object data) {
    }
}
