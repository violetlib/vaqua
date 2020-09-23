/*
 * Copyright (c) 2015-2020 Alan Snyder.
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

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.aqua.AquaUtils.RecyclableSingleton;
import org.violetlib.aqua.AquaUtils.RecyclableSingletonFromDefaultConstructor;
import org.violetlib.jnr.aqua.AquaUIPainter;
import org.violetlib.jnr.aqua.AquaUIPainter.Size;
import org.violetlib.jnr.aqua.ButtonLayoutConfiguration;
import org.violetlib.jnr.aqua.LayoutConfiguration;

public class AquaButtonUI extends BasicButtonUI
        implements AquaUtilControlSize.Sizeable, FocusRingOutlineProvider, ToolbarSensitiveUI, AquaComponentUI {

    // This UI is shared.
    // Button borders may also be shared.
    // All button configuration state must be in the button itself.

    // Important programming note:
    //
    // Do not call b.getDisabledIcon() or b.getDisabledSelectedIcon().
    // Use the static methods defined here instead.

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
        removeCachedIcons((AbstractButton) c);
    }

    protected void installDefaults(@NotNull AbstractButton b) {
        // load shared instance defaults
        String pp = getPropertyPrefix();
        setButtonMarginIfNeeded(b, UIManager.getInsets(pp + "margin"));
        AquaUtils.installFont(b, pp + "font");
        LookAndFeel.installProperty(b, "opaque", false);
        b.putClientProperty(DEFAULT_FONT_PROPERTY, b.getFont());
        initializeToolbarStatus(b);
        configure(b);
    }

    @Override
    public void appearanceChanged(@NotNull JComponent c, @NotNull AquaAppearance appearance) {
    }

    @Override
    public void activeStateChanged(@NotNull JComponent c, boolean isActive) {
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
            // in a tool bar, whether the button is a member of a button group, whether the button has an icon or any
            // child components (e.g. images). Instead of trying to figure out exactly which parts of the configuration
            // depend upon which inputs, we do a complete reconfiguration whenever an input has potentially changed.

            // Unfortunately, there are no notifications when a button is added to or removed from a button group.
            // Button group membership potentially affects layout and painting, but only of JToggleButtons.

            // Note that the choice of border does not depend upon the size variant, but the border may use the size
            // variant as part of its configuration of the button. The border performs all of the configuration based on
            // a defined button type.

            boolean isToolbar = isToolbar(b);
            AquaButtonExtendedTypes.TypeSpecifier type = AquaButtonExtendedTypes.getTypeSpecifier(b, isToolbar);
            installBorder(b, type, isToolbar);

            LayoutConfiguration g = null;
            Border border = b.getBorder();
            if (border instanceof AquaButtonBorder) {
                AquaButtonBorder bb = (AquaButtonBorder) border;
                g = bb.determineLayoutConfiguration(b);
                if (bb.isRolloverEnabled(b)) {
                    LookAndFeel.installProperty(b, "rolloverEnabled", true);
                }
                int iconTextGap = bb.getIconTextGap(b);
                LookAndFeel.installProperty(b, "iconTextGap", iconTextGap);
            }
            b.putClientProperty(LAYOUT_CONFIGURATION_PROPERTY, g);

            // Perform configuration of the button based on the size variant, whether specified or implied.
            // This may change the button font, foreground color, and layout sizes.
            Size size = AquaUtilControlSize.getUserSizeFrom(b);
            if (AquaUtilControlSize.isOKToInstallDefaultFont(b)) {
                Font df = getFontForButton(b, size);
                b.setFont(df);
            }

            updateTemplateIconStatus(b);

            if (!isColorWell(b)) {
                disconnectColorChooser(b);
            }

            b.setRequestFocusEnabled(false);

            b.revalidate();
            b.repaint();
        } finally {
            isConfiguring = false;
        }
    }

    /**
     * Invalidate the cached special icon if the template icon status has changed.
     */
    protected void updateTemplateIconStatus(AbstractButton b) {
        Object o = b.getClientProperty(SPECIAL_ICON_PROPERTY);
        if (o instanceof AquaButtonIcon) {
            AquaButtonIcon icon = (AquaButtonIcon) o;
            boolean isTemplate = determineTemplateIconStatus(b);
            if (icon.isTemplate != isTemplate) {
                // Force a new icon to be created with the new status
                removeCachedIcons(b);
            }
        }
    }

    /**
     * Determine whether or not the button is eligible for painting the icon as a template.
     * To be eligible, the button must define an icon that is a template image, and it must not have any other
     * application provided icon.
     */
    protected boolean determineTemplateIconStatus(AbstractButton b) {
        Icon standardIcon = b.getIcon();
        if (standardIcon instanceof ImageIcon) {
            ImageIcon im = (ImageIcon) standardIcon;
            Image image = im.getImage();

            return !isApplicationDefined(b.getPressedIcon())
                    && !isApplicationDefined(getDisabledIcon(b))
                    && !isApplicationDefined(b.getSelectedIcon())
                    && !isApplicationDefined(getDisabledSelectedIcon(b))
                    && !isApplicationDefined(b.getRolloverIcon())
                    && !isApplicationDefined(b.getRolloverSelectedIcon())
                    && AquaImageFactory.isTemplateImage(image);
        }
        return false;
    }

    protected boolean isApplicationDefined(Icon ic) {
        return ic != null && !(ic instanceof UIResource);
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

    @Override
    public @Nullable Shape getFocusRingOutline(@NotNull JComponent c) {
        Border border = c.getBorder();
        if (border instanceof FocusRingOutlineProvider) {
            FocusRingOutlineProvider bb = (FocusRingOutlineProvider) border;
            return bb.getFocusRingOutline(c);
        }

        int width = c.getWidth();
        int height = c.getHeight();
        return new RoundRectangle2D.Double(OUTLINE_OFFSET, OUTLINE_OFFSET, width-2*OUTLINE_OFFSET, height-2*OUTLINE_OFFSET, OUTLINE_CORNER, OUTLINE_CORNER);
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
        Font base = getDefaultFontPropertyValue(b);
        if (base == null) {
            // should not happen
            base = new Font("Default", Font.PLAIN, 12);
        }
        Border border = b.getBorder();
        if (border instanceof AquaButtonBorder) {
            AquaButtonBorder bb = (AquaButtonBorder) border;
            return bb.getCustomDefaultFont(b, size, base);
        } else {
            return AquaUtilControlSize.getFontForSize(base, size);
        }
    }

    public static @Nullable Font getDefaultFontPropertyValue(@NotNull AbstractButton b) {
        Object o = b.getClientProperty(DEFAULT_FONT_PROPERTY);
        if (o instanceof Font) {
            return (Font) o;
        }
        return null;
    }

    protected Color getDefaultForegroundColor(AbstractButton b) {
        boolean isEnabled = b.getModel().isEnabled();
        Color existingColor = b.getForeground();
        if (existingColor == null || existingColor instanceof UIResource || !isEnabled) {
            // Most buttons do not display text differently when the window is inactive
            AquaAppearance appearance = AppearanceManager.ensureAppearance(b);
            if (useSelectedForeground(b)) {
                return appearance.getColor("alternateSelectedControlText");
            } else {
                return appearance.getColor("controlText");
            }
        }
        return existingColor;
    }

    private boolean useSelectedForeground(AbstractButton b) {
        if (b instanceof JToggleButton && !(b instanceof JCheckBox) && !(b instanceof JRadioButton)) {
            return b.getModel().isSelected();
        }

        return false;
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

    protected void installListeners(AbstractButton b) {
        AquaButtonListener listener = createButtonListener(b);
        if (listener != null) {
            // put the listener in the button's client properties so that
            // we can get at it later
            b.putClientProperty(this, listener);

            b.addMouseListener(listener);
            b.addMouseMotionListener(listener);
            b.addFocusListener(listener);
            b.addPropertyChangeListener(listener);
            b.addChangeListener(listener);
            b.addActionListener(listener);
            b.addItemListener(listener);
        }
        if (isToolbarSensitive(b)) {
            AquaUtils.installToolbarSensitivity(b);
        }
        AquaUtilControlSize.addSizePropertyListener(b);
        AquaFullKeyboardFocusableHandler.addListener(b);
    }

    protected void installKeyboardActions(AbstractButton b) {
        BasicButtonListener listener = (BasicButtonListener)b.getClientProperty(this);
        if (listener != null) {
            listener.installKeyboardActions(b);
        }
    }

    public void uninstallUI(JComponent c) {
        disconnectColorChooser((AbstractButton)c);
        uninstallKeyboardActions((AbstractButton)c);
        uninstallListeners((AbstractButton)c);
        uninstallDefaults((AbstractButton)c);
        removeCachedIcons((AbstractButton) c);
    }

    protected void uninstallKeyboardActions(AbstractButton b) {
        BasicButtonListener listener = (BasicButtonListener)b.getClientProperty(this);
        if (listener != null) listener.uninstallKeyboardActions(b);
    }

    protected void uninstallListeners(AbstractButton b) {
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
        AquaFullKeyboardFocusableHandler.removeListener(b);
        AquaUtils.uninstallToolbarSensitivity(b);
    }

    protected void uninstallDefaults(AbstractButton b) {
        LookAndFeel.uninstallBorder(b);
        AquaUtilControlSize.uninstallDefaultFont(b);
    }

    protected void removeCachedIcons(AbstractButton b) {

        if (b.getSelectedIcon() instanceof UIResource) {
            b.setSelectedIcon(null);
        }

        if (getDisabledIcon(b) instanceof UIResource) {
            b.setDisabledIcon(null);
        }

        if (getDisabledSelectedIcon(b) instanceof UIResource) {
            b.setDisabledSelectedIcon(null);
        }

        if (b.getPressedIcon() instanceof UIResource) {
            b.setPressedIcon(null);
        }

        if (b.getRolloverIcon() instanceof UIResource) {
            b.setRolloverIcon(null);
        }

        if (b.getRolloverSelectedIcon() instanceof UIResource) {
            b.setRolloverSelectedIcon(null);
        }

        b.putClientProperty(SPECIAL_ICON_PROPERTY, null);
    }

    // Create Listeners
    protected AquaButtonListener createButtonListener(AbstractButton b) {
        return new AquaButtonListener(b);
    }

    // Paint Methods
    @Override
    public void update(Graphics g, JComponent c) {
        AppearanceManager.ensureAppearance(c);
        AquaAppearance appearance = AppearanceManager.registerCurrentAppearance(c);
        super.update(g, c);
        AppearanceManager.restoreCurrentAppearance(appearance);
    }

    public final void paint(Graphics g, JComponent c) {
        paint((Graphics2D) g, (AbstractButton) c);
    }

    protected void paint(@NotNull Graphics2D g, @NotNull AbstractButton b) {
        Rectangle viewRect = new Rectangle(b.getWidth(), b.getHeight());
        AquaButtonBorder aquaBorder = null;
        if (b.isBorderPainted()) {
            Border border = b.getBorder();
            if (border instanceof AquaButtonBorder) {
                aquaBorder = (AquaButtonBorder) border;
            }
        }
        Icon icon = getIcon(b);
        if (aquaBorder != null) {
            aquaBorder.paintButton(g, b, icon, viewRect);
        } else {
            paintButtonDefault(g, b, icon, viewRect);
        }
    }

    /**
     * Paint a button whose appearance is not defined by VAqua.
     */
    protected void paintButtonDefault(@NotNull Graphics2D g,
                                      @NotNull AbstractButton b,
                                      @Nullable Icon icon,
                                      @NotNull Rectangle viewRect) {
        ButtonModel model = b.getModel();
        boolean isColorWell = isColorWell(b);
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
        Color textColor = getDefaultForegroundColor(b);
        paintIconAndText(g, b, b.getInsets(), icon, textColor, viewRect, null);
    }

    public static void paintIconAndText(@NotNull Graphics2D g,
                                          @NotNull AbstractButton b,
                                          @NotNull Insets insets,
                                          @Nullable Icon icon,
                                          @NotNull Color textColor,
                                          @NotNull Rectangle viewRect,
                                          @Nullable Dimension iconSize) {
        Rectangle iconRect = new Rectangle();
        Rectangle textRect = new Rectangle();
        String text = AquaButtonUI.layoutAndGetText(g, b, insets, viewRect, iconRect, textRect, iconSize);
        if (icon != null) {
            paintIcon(g, b, icon, iconRect);
        }
        paintText(g, b, textRect, textColor, text);
    }

    public static @NotNull String layoutAndGetText(@Nullable Graphics2D g,
                                                   @NotNull AbstractButton b,
                                                   @NotNull Insets i,
                                                   @NotNull Rectangle viewRect,
                                                   @NotNull Rectangle iconRect,
                                                   @NotNull Rectangle textRect,
                                                   @Nullable Dimension iconSize) {
        // re-initialize the view rect to the selected insets
        viewRect.x = i.left;
        viewRect.y = i.top;
        viewRect.width = b.getWidth() - (i.right + viewRect.x);
        viewRect.height = b.getHeight() - (i.bottom + viewRect.y);

        // reset the text and icon rects
        textRect.x = textRect.y = textRect.width = textRect.height = 0;
        iconRect.x = iconRect.y = iconRect.width = iconRect.height = 0;

        // setup the font metrics
        // If no graphics context is provided, the request is to determine the icon location under the
        // assumption that the text does not matter.
        FontMetrics fm = null;
        if (g != null) {
            g.setFont(b.getFont());
            fm = g.getFontMetrics();
        }

        // layout the text and icon
        String originalText = b.getText();
        if (iconSize == null) {
            Icon icon = b.getIcon();
            if (icon != null) {
                iconSize = new Dimension(icon.getIconWidth(), icon.getIconHeight());
            }
        }
        String text = AquaUtils.layoutCompoundLabel(b, fm, originalText, iconSize, b.getVerticalAlignment(),
                b.getHorizontalAlignment(), b.getVerticalTextPosition(), b.getHorizontalTextPosition(),
                viewRect, iconRect, textRect, originalText == null ? 0 : b.getIconTextGap());
        return text;
    }

    /**
     * Paint the icon.
     */
    public static void paintIcon(Graphics2D g, @NotNull AbstractButton b, @NotNull Icon icon, Rectangle iconRect) {
        Graphics2D gg = null;
        if (icon.getIconWidth() != iconRect.width || icon.getIconHeight() != iconRect.height) {
            gg = (Graphics2D) g.create();
            g = gg;
            gg.translate(iconRect.x, iconRect.y);
            gg.scale(iconRect.getWidth() / icon.getIconWidth(), iconRect.getHeight() / icon.getIconHeight());
            gg.translate(-iconRect.x, -iconRect.y);
        }
        icon.paintIcon(b, g, iconRect.x, iconRect.y);
        if (gg != null) {
            gg.dispose();
        }
    }

    /**
     * Paint the text.
     */
    public static void paintText(@NotNull Graphics2D g,
                                 @NotNull AbstractButton b,
                                 @NotNull Rectangle textRect,
                                 @NotNull Color textColor,
                                 @NotNull String text) {

        if (!text.isEmpty()) {
            if (textRect.width == 0) {
                textRect.width = 50;
            }
            Object o = b.getClientProperty(BasicHTML.propertyKey);
            if (o instanceof View) {
                View v = (View) o;
                v.paint(g, textRect);
            } else {
                FontMetrics fm = g.getFontMetrics();
                int mnemonicIndex = AquaMnemonicHandler.isMnemonicHidden() ? -1 : b.getDisplayedMnemonicIndex();
                g.setColor(textColor);
                int x = textRect.x;
                int y = textRect.y + fm.getAscent();
                JavaSupport.drawStringUnderlineCharAt(b, g, text, mnemonicIndex, x, y);
            }
        }
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
     * Obtain the icon to use based on the button state.
     */
    protected @Nullable Icon getIcon(AbstractButton b) {
        ButtonIconState bs = getIconState(b);
        Icon definedIcon = getDefinedIcon(b, bs);
        if (definedIcon != null && bs != ButtonIconState.DEFAULT) {
            return definedIcon;
        }
        // The special icon creates state specific renderings based on the button default icon.
        Icon icon = getSpecialIcon(b);
        if (icon != null) {
            return icon;
        }
        return definedIcon;
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

    public static boolean isColorWell(@NotNull AbstractButton b) {
        AquaUIPainter.ButtonWidget widget = getButtonWidget(b);
        return widget == AquaUIPainter.ButtonWidget.BUTTON_COLOR_WELL;
    }

    private static @Nullable AquaUIPainter.ButtonWidget getButtonWidget(AbstractButton b) {
        Object o = b.getClientProperty(LAYOUT_CONFIGURATION_PROPERTY);
        if (o instanceof ButtonLayoutConfiguration) {
            ButtonLayoutConfiguration bg = (ButtonLayoutConfiguration) o;
            return bg.getButtonWidget();
        }
        return null;
    }

    /**
     * Return a value that can be used to select an appropriate icon based on the current state of the specified button.
     * @param b The button.
     * @return the button icon state.
     */
    public static @NotNull ButtonIconState getIconState(AbstractButton b) {
        ButtonModel model = b.getModel();
        if (!model.isEnabled()) {
            if (model.isSelected()) {
                return ButtonIconState.DISABLED_SELECTED;
            } else {
                return ButtonIconState.DISABLED;
            }
        } else if (model.isPressed() && model.isArmed()) {
            return ButtonIconState.PRESSED;
        } else if (b.isRolloverEnabled() && model.isRollover()) {
            if (model.isSelected()) {
                return ButtonIconState.ROLLOVER_SELECTED;
            } else {
                return ButtonIconState.ROLLOVER;
            }
        } else if (model.isSelected()) {
            return ButtonIconState.SELECTED;
        } else {
            return ButtonIconState.DEFAULT;
        }
    }

    /**
     * Obtain the explicitly defined icon for a button based on the specified button state.
     * If the button uses this class for its UI, then this method will not install any default icons on the button.
     * @param b The button.
     * @return the icon defined on that button for the specified button state (may return null).
     */
    public static @Nullable Icon getDefinedIcon(AbstractButton b, ButtonIconState bs) {
        switch (bs) {
            case DISABLED:              return getDisabledIcon(b);
            case DISABLED_SELECTED:     return getDisabledSelectedIcon(b);
            case PRESSED:               return getIconForPressedState(b);
            case ROLLOVER_SELECTED:     return getIconForRolloverSelectedState(b);
            case ROLLOVER:              return b.getRolloverIcon();
            case SELECTED:              return b.getSelectedIcon();
            default:                    return b.getIcon();
        }
    }

    private static @Nullable Icon getIconForPressedState(@NotNull AbstractButton b) {
        Icon icon = b.getPressedIcon();
        return icon != null ? icon : b.getSelectedIcon();
    }

    private static @Nullable Icon getIconForRolloverSelectedState(@NotNull AbstractButton b) {
        Icon icon = b.getRolloverSelectedIcon();
        return icon != null ? icon : b.getSelectedIcon();
    }

    /**
     * Obtain a special icon to use for a button. The special icon rendering may be context dependent.
     * This method should not be called unless the button has an icon.
     * @param b The button.
     * @return the special icon for the button, or null if no special icon is defined for the button.
     */
    protected @Nullable AquaButtonIcon getSpecialIcon(@NotNull AbstractButton b) {
        Object o = b.getClientProperty(SPECIAL_ICON_PROPERTY);
        if (o instanceof AquaButtonIcon) {
            return (AquaButtonIcon) o;
        }
        Border border = b.getBorder();
        if (border instanceof AquaButtonBorder) {
            AquaButtonBorder bb = (AquaButtonBorder) border;
            boolean isTemplate = determineTemplateIconStatus(b);
            AquaButtonIcon icon = bb.createIcon(b, isTemplate);
            b.putClientProperty(SPECIAL_ICON_PROPERTY, icon);
            return icon;
        }
        return null;
    }

    /**
     * This method is called by AbstractButton via the LAF to obtain an icon to use when a button is disabled.
     * @param b The button.
     * @param source The button icon.
     * @return the icon to use.
     */
    public @NotNull Icon createDisabledIcon(AbstractButton b, ImageIcon source) {
        AquaButtonIcon specialIcon = getSpecialIcon(b);
        if (specialIcon != null) {
            return specialIcon;
        }
        return createDefaultDisabledIcon(source);
    }

    /**
     * This method is called by AbstractButton via the LAF to obtain an icon to use when a button is disabled and
     * selected.
     * @param b The button.
     * @param source The button selected icon.
     * @return the icon to use.
     */
    public @NotNull Icon createDisabledSelectedIcon(AbstractButton b, ImageIcon source) {
        AquaButtonIcon specialIcon = getSpecialIcon(b);
        if (specialIcon != null) {
            return specialIcon;
        }
        return createDefaultDisabledIcon(source);
    }

    protected @NotNull ImageIcon createDefaultDisabledIcon(ImageIcon source) {
        return new ImageIconUIResource(AquaImageFactory.getProcessedImage(source.getImage(), AquaImageFactory.LIGHTEN_FOR_DISABLED));
    }

    protected void paintButtonPressed(Graphics g, AbstractButton b) {
        paint(g, b);
    }

    // Layout Methods
    public Dimension getMinimumSize(JComponent c) {
        AbstractButton b = (AbstractButton) c;
        ensureValidSegmentedControlModel(b);
        Border border = b.getBorder();
        Dimension d;
        if (border instanceof AquaButtonBorder) {
            AquaButtonBorder bb = (AquaButtonBorder) border;
            d = bb.getPreferredButtonSize(b);
        } else {
            d = getPreferredSize(b);
        }

        View v = (View)c.getClientProperty(BasicHTML.propertyKey);
        if (v != null) {
            d.width -= v.getPreferredSpan(View.X_AXIS) - v.getMinimumSpan(View.X_AXIS);
        }
        return d;
    }

    public Dimension getPreferredSize(JComponent c) {
        AbstractButton b = (AbstractButton) c;
        ensureValidSegmentedControlModel(b);
        Border border = b.getBorder();
        if (border instanceof AquaButtonBorder) {
            AquaButtonBorder bb = (AquaButtonBorder) border;
            return bb.getPreferredButtonSize(b);
        } else {
            return getPreferredButtonSize(b, b.getIconTextGap(), null);
        }
    }

    public Dimension getMaximumSize(JComponent c) {
        Dimension d = getPreferredSize(c);
        View v = (View)c.getClientProperty(BasicHTML.propertyKey);
        if (v != null) {
            d.width += v.getMaximumSpan(View.X_AXIS) - v.getPreferredSpan(View.X_AXIS);
        }
        return d;
    }

    /**
     * Return the segmented control model for a button, if any. If a current segmented control model is found, it
     * is checked to ensure that it is still valid. If not valid, it is discarded, and a new model will be created, if
     * possible. If an old model is discarded or a new model created, the button is reconfigured.
     *
     * @param b The button.
     *
     * @return the valid segmented control model for this button, or null if none.
     */
    public @Nullable SegmentedControlModel getSegmentedControlModel(@NotNull AbstractButton b) {
        if (isPotentialSegmentedControlMember(b)) {
            return SegmentedControlModel.getSegmentedControlModel(b);
        }
        return null;
    }

    /**
     * Ensure that the button has a valid segmented control model, if one is needed and can be created. A segmented
     * control model may be needed if the button layout and rendering are potentially sensitive to the segmented control
     * model. Update the cached information and reconfigure if needed.
     */
    protected void ensureValidSegmentedControlModel(@NotNull AbstractButton b) {
        SegmentedControlModel ignore = getSegmentedControlModel(b);
    }

    public boolean isPotentialSegmentedControlMember(@NotNull AbstractButton b) {
        return false;
    }

    private void toggleButtonStateChanged(@NotNull AbstractButton b) {
        if (isPotentialSegmentedControlMember(b) && b instanceof JToggleButton) {
            // When a segmented control button selection state changes, it may be necessary to
            // repaint the adjacent button on the left side.

            SegmentedControlModel m = getSegmentedControlModel(b);
            if (m != null) {
                JToggleButton tb = (JToggleButton) b;
                JToggleButton leftButton = m.getLeftAdjacentButton(tb);
                if (leftButton != null) {
                    leftButton.repaint();
                }
            }
        }
    }

    public @Nullable JToggleButton getRightAdjacentSegmentButton(@NotNull JToggleButton b) {
        SegmentedControlModel m = getSegmentedControlModel(b);
        return m != null ? m.getRightAdjacentButton(b) : null;
    }

    public @Nullable JToggleButton getLeftAdjacentSegmentButton(@NotNull JToggleButton b) {
        SegmentedControlModel m = getSegmentedControlModel(b);
        return m != null ? m.getLeftAdjacentButton(b) : null;
    }

    public boolean isToolbar(@NotNull AbstractButton b) {
        return Boolean.TRUE.equals(b.getClientProperty(CACHED_TOOLBAR_STATUS_PROPERTY));
    }

    private boolean isToolbarSensitive(@NotNull AbstractButton b) {
        // Checkboxes and radio buttons are not toolbar sensitive
        return b instanceof JButton
                || b instanceof JToggleButton && !(b instanceof JCheckBox) && !(b instanceof JRadioButton);
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
            if (isColorWell(b)) {
                ActionListener[] listeners = b.getActionListeners();
                if (listeners.length == 1) {
                    toggleColorChooser(b);
                }
            }
        }

        @Override
        public void itemStateChanged(ItemEvent e) {
            toggleButtonStateChanged(b);
        }

        public void focusGained(FocusEvent e) {
            ((Component)e.getSource()).repaint();
        }

        public void focusLost(FocusEvent e) {
            // 10-06-03 VL: [Radar 3187049]
            // If focusLost arrives while the button has been left-clicked this would disarm the button,
            // causing actionPerformed not to fire on mouse release!
            //b.getModel().setArmed(false);
            ((Component)e.getSource()).repaint();
        }

        public void propertyChange(PropertyChangeEvent e) {
            super.propertyChange(e);

            String propertyName = e.getPropertyName();

            // Repaint the button, since its border needs to handle the new state.
            if (AquaFocusHandler.FRAME_ACTIVE_PROPERTY.equals(propertyName)) {
                b.repaint();
                return;
            }

            if ("icon".equals(propertyName) || "text".equals(propertyName)) {
                configure(b);
                return;
            }

            if (propertyName != null && !propertyName.contains(".") && propertyName.endsWith("Icon")) {
                updateTemplateIconStatus(b);
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
                Border border = b.getBorder();
                if (border instanceof AquaSegmentedButtonBorder) {
                    configure(b);
                }
            }

            if ("ancestor".equals(propertyName)) {
                if (!b.isDisplayable()) {
                    disconnectColorChooser(b);
                }
            }
        }

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

    protected void disconnectColorChooser(AbstractButton b)
    {
        Object o = b.getClientProperty(COLOR_CHOOSER_OWNER_PROPERTY);
        if (o instanceof SharedColorChooserOwner) {
            SharedColorChooserOwner owner = (SharedColorChooserOwner) o;
            AquaSharedColorChooser.disconnect(owner);
            b.putClientProperty(COLOR_CHOOSER_OWNER_PROPERTY, null);
        }
    }

    protected void toggleColorChooser(AbstractButton b) {

        Object o = b.getClientProperty(COLOR_CHOOSER_OWNER_PROPERTY);
        if (o instanceof SharedColorChooserOwner) {
            SharedColorChooserOwner owner = (SharedColorChooserOwner) o;
            AquaSharedColorChooser.disconnect(owner);
            b.setSelected(false);
            b.putClientProperty(COLOR_CHOOSER_OWNER_PROPERTY, null);
            return;
        }

        SharedColorChooserOwner owner = new SharedColorChooserOwner() {
            @Override
            public void applyColor(Color c) {
                b.setBackground(c);
            }

            @Override
            public void disconnected() {
                b.setSelected(false);
                b.putClientProperty(COLOR_CHOOSER_OWNER_PROPERTY, null);
            }
        };

        Color c = b.getBackground();
        boolean enableTranslucentColors = Boolean.TRUE.equals(b.getClientProperty(ENABLE_TRANSLUCENT_COLORS_KEY));
        if (AquaSharedColorChooser.connect(owner, c, enableTranslucentColors)) {
            b.putClientProperty(COLOR_CHOOSER_OWNER_PROPERTY, owner);
            b.setSelected(true);
        }
    }

    /**
     * Return the disabled icon explicitly assigned to a button. This method is useful only for buttons that use this
     * class for their UI. It inhibits the automatic creation of a disabled icon by the UI when none is defined on the
     * button.
     *
     * @param b the button.
     *
     * @return the disabled icon explicitly assigned to the button.
     */
    public static Icon getDisabledIcon(AbstractButton b) {
        boolean oldValue = AquaLookAndFeel.suppressCreationOfDisabledButtonIcons;
        AquaLookAndFeel.suppressCreationOfDisabledButtonIcons = true;
        try {
            return b.getDisabledIcon();
        } finally {
            AquaLookAndFeel.suppressCreationOfDisabledButtonIcons = oldValue;
        }
    }

    /**
     * Return the disabled selected icon explicitly assigned to a button. This method is useful only for buttons that
     * use this class for their UI. It inhibits the automatic creation of a disabled selected icon by the UI when none
     * is defined on the button.
     *
     * @param b the button.
     *
     * @return the disabled selected icon explicitly assigned to the button.
     */
    public static Icon getDisabledSelectedIcon(AbstractButton b) {
        boolean oldValue = AquaLookAndFeel.suppressCreationOfDisabledButtonIcons;
        AquaLookAndFeel.suppressCreationOfDisabledButtonIcons = true;
        try {
            return b.getDisabledSelectedIcon();
        } finally {
            AquaLookAndFeel.suppressCreationOfDisabledButtonIcons = oldValue;
        }
    }

    public static @Nullable Dimension getPreferredButtonSize(@NotNull AbstractButton b,
                                                             int textIconGap,
                                                             @Nullable Dimension iconSize) {
        if (b.getComponentCount() > 0) {
            return null;
        }

        if (iconSize == null) {
            Icon icon = b.getIcon();
            if (icon != null) {
                iconSize = new Dimension(icon.getIconWidth(), icon.getIconHeight());
            }
        }

        String text = b.getText();
        Font font = b.getFont();
        FontMetrics fm = b.getFontMetrics(font);
        Rectangle iconR = new Rectangle();
        Rectangle textR = new Rectangle();
        Rectangle viewR = new Rectangle(Short.MAX_VALUE, Short.MAX_VALUE);

        AquaUtils.layoutCompoundLabel(
                b, fm, text, iconSize,
                b.getVerticalAlignment(), b.getHorizontalAlignment(),
                b.getVerticalTextPosition(), b.getHorizontalTextPosition(),
                viewR, iconR, textR, (text == null ? 0 : textIconGap)
        );

        // The preferred size of the button is the size of the text and icon rectangles plus the insets.
        Rectangle r = iconR.union(textR);
        Insets insets = b.getInsets();
        r.width += insets.left + insets.right;
        r.height += insets.top + insets.bottom;
        return r.getSize();
    }

    /**
     * Determine the preferred content size for a button. The preferred content size does not include the button insets
     * and must be based only on the button text, icon, and the specified parameters. This code does not handle toolbar
     * wells, and it does not need to. Only used for button styles with fixed heights.
     */
    public static Dimension getPreferredContentSize(AbstractButton b, Font font, int textIconGap) {
        Icon icon = b.getIcon();
        Dimension iconSize = icon != null ? new Dimension(icon.getIconWidth(), icon.getIconHeight()) : null;
        String text = b.getText();
        FontMetrics fm = b.getFontMetrics(font);

        Rectangle iconR = new Rectangle();
        Rectangle textR = new Rectangle();
        Rectangle viewR = new Rectangle(Short.MAX_VALUE, Short.MAX_VALUE);

        AquaUtils.layoutCompoundLabel(
                b, fm, text, iconSize,
                b.getVerticalAlignment(), b.getHorizontalAlignment(),
                b.getVerticalTextPosition(), b.getHorizontalTextPosition(),
                viewR, iconR, textR, (text == null ? 0 : textIconGap)
        );

        Rectangle r = iconR.union(textR);
        return r.getSize();
    }
}
