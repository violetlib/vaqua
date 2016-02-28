/*
 * Copyright (c) 2015-2016 Alan Snyder.
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
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeEvent;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;
import javax.swing.plaf.ButtonUI;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.RootPaneUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicButtonListener;
import javax.swing.plaf.basic.BasicButtonUI;
import javax.swing.plaf.basic.BasicHTML;
import javax.swing.text.View;

import org.violetlib.aqua.AquaUtils.RecyclableSingleton;
import org.violetlib.aqua.AquaUtils.RecyclableSingletonFromDefaultConstructor;
import org.violetlib.jnr.aqua.AquaUIPainter;
import org.violetlib.jnr.aqua.AquaUIPainter.Size;
import org.violetlib.jnr.aqua.ButtonLayoutConfiguration;
import org.violetlib.jnr.aqua.LayoutConfiguration;

public class AquaButtonUI extends BasicButtonUI implements AquaUtilControlSize.Sizeable, FocusRingOutlineProvider {

    // This UI is shared.
    // Button borders may also be shared.
    // All button configuration state must be in the button itself.

    public static final String BUTTON_TYPE = "JButton.buttonType";
    public static final String SEGMENTED_BUTTON_POSITION = "JButton.segmentPosition";
    public static final String SELECTED_STATE_KEY = "JButton.selectedState";

    public static final float OUTLINE_OFFSET = 0;
    public static final float OUTLINE_CORNER = 9;

    public static final String LAYOUT_CONFIGURATION_PROPERTY = "Aqua.Button.LayoutConfiguration";
    public static final String DEFAULT_FONT_PROPERTY = "Aqua.Button.DefaultFont";
    protected static final String COLOR_CHOOSER_OWNER_PROPERTY = "Aqua.Button.ColorChooserOwner";
    protected static final String UNSELECTED_ICON_PROPERTY = "Aqua.Button.UnselectedIcon";
    protected static final String TEMPLATE_ICON_PROPERTY = "Aqua.Button.IsTemplateIcon";

    protected static final RecyclableSingleton<AquaButtonUI> buttonUI = new RecyclableSingletonFromDefaultConstructor<AquaButtonUI>(AquaButtonUI.class);
    public static ComponentUI createUI(final JComponent c) {
        return buttonUI.get();
    }

    private AquaButtonExtendedTypes.ColorDefaults colorDefaults;

    @Override
    public void installUI(JComponent c) {
        super.installUI(c);
        removeCachedIcons((AbstractButton) c);
    }

    protected void installDefaults(final AbstractButton b) {
        // load shared instance defaults
        final String pp = getPropertyPrefix();

        if (colorDefaults == null) {
            colorDefaults = new AquaButtonExtendedTypes.ColorDefaults();
            colorDefaults.enabledTextColor = UIManager.getColor(pp + "foreground");
            colorDefaults.selectedTextColor = UIManager.getColor(pp + "selectedText");
            colorDefaults.disabledTextColor = UIManager.getColor(pp + "disabledText");
        }

        setButtonMarginIfNeeded(b, UIManager.getInsets(pp + "margin"));

        LookAndFeel.installColorsAndFont(b, pp + "background", pp + "foreground", pp + "font");
        LookAndFeel.installProperty(b, "opaque", UIManager.getBoolean(pp + "opaque"));

        b.putClientProperty(DEFAULT_FONT_PROPERTY, b.getFont());

        configure(b);
    }

    /**
     * Configure a button.
     * @param b The button component to be configured.
     */
    public void configure(AbstractButton b) {

        // The configuration of a button is rather complex. It potentially affects the border, the font, the foreground
        // color, the minimum and preferred component sizes. The configuration potentially depends upon client
        // properties (for button type, segment position, and size variant), whether the button is contained in a tool
        // bar, whether the button has an icon or any child components (e.g. images). Instead of trying to figure out
        // exactly which parts of the configuration depend upon which inputs, we do a complete reconfiguration whenever
        // an input has potentially changed.

        // Note that the choice of border does not depend upon the size variant, but the border may use the size variant
        // as part of its configuration of the button. The border performs all of the configuration based on a defined
        // button type.

        AquaButtonExtendedTypes.TypeSpecifier type = AquaButtonExtendedTypes.getTypeSpecifier(b);
        installBorder(b, type);

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
            Font df = getDefaultFont(b, size);
            df = getCustomDefaultFont(b, size, df);
            AquaUtilControlSize.installDefaultFont(b, df);
        }

        updateTemplateIconStatus(b);

        b.setRequestFocusEnabled(false);

        b.revalidate();
        b.repaint();
    }

    /**
     * Indicate whether a button is eligible for painting the icon as a template.
     */
    public static boolean isTemplateIconEnabled(AbstractButton b) {
        Object o = b.getClientProperty(TEMPLATE_ICON_PROPERTY);
        return Boolean.TRUE.equals(o);
    }

    /**
     * Update the client property that marks a button that is eligible for painting the icon as a template.
     */
    protected void updateTemplateIconStatus(AbstractButton b) {
        if (determineTemplateIconStatus(b)) {
            b.putClientProperty(TEMPLATE_ICON_PROPERTY, true);
        } else {
            b.putClientProperty(TEMPLATE_ICON_PROPERTY, null);
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

            // It is unfortunate that there is no way to ask for a disabled icon without risking creating one via the
            // LAF. We use a static variable to temporarily inhibit our LAF from creating an icon.

            boolean oldValue = AquaLookAndFeel.suppressCreationOfDisabledButtonIcons;
            AquaLookAndFeel.suppressCreationOfDisabledButtonIcons = true;

            try {
                return !isApplicationDefined(b.getPressedIcon())
                  && !isApplicationDefined(b.getDisabledIcon())
                  && !isApplicationDefined(b.getSelectedIcon())
                  && !isApplicationDefined(b.getDisabledSelectedIcon())
                  && !isApplicationDefined(b.getRolloverIcon())
                  && !isApplicationDefined(b.getRolloverSelectedIcon())
                  && AquaImageFactory.isTemplateImage(image);
            } finally {
                AquaLookAndFeel.suppressCreationOfDisabledButtonIcons = oldValue;
            }
        }
        return false;
    }

    protected boolean isApplicationDefined(Icon ic) {
        return ic != null && !(ic instanceof UIResource);
    }

    /**
     * Install the appropriate border for a button.
     */
    protected void installBorder(AbstractButton b, AquaButtonExtendedTypes.TypeSpecifier type) {
        Border customBorder = type != null ? type.getBorder() : null;
        if (customBorder != null) {
            b.setBorder(customBorder);
        } else {
            Border oldBorder = b.getBorder();
            if (oldBorder == null || oldBorder instanceof UIResource) {
                Border border = getDefaultBorder(b);
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
    protected Border getDefaultBorder(AbstractButton b) {
        if (isOnToolbar(b)) {
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
    public Shape getFocusRingOutline(JComponent c) {
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
     * Return the default font for a button independent of the button type and configuration.
     */
    public static Font getGenericDefaultFont(AbstractButton b) {
        Font f = (Font) b.getClientProperty(DEFAULT_FONT_PROPERTY);
        if (f != null) {
            return f;
        }
        return b.getFont();
    }

    protected Font getDefaultFont(AbstractButton b, Size size) {
        if (shouldUseIconFont(b)) {
            return size == Size.SMALL || size == Size.MINI
                ? UIManager.getFont("IconButton.smallFont")
                : UIManager.getFont("IconButton.font");
        } else {
            return (Font) b.getClientProperty(DEFAULT_FONT_PROPERTY);
        }
    }

    protected static boolean shouldUseIconFont(AbstractButton b) {
        return b.getIcon() != null || b.getComponentCount() > 0 || isOnToolbar(b);
    }

    protected Font getCustomDefaultFont(AbstractButton b, Size size, Font df) {
        Border border = b.getBorder();
        if (border instanceof AquaButtonBorder) {
            AquaButtonBorder bb = (AquaButtonBorder) border;
            return bb.getCustomDefaultFont(b, size, df);
        } else {
            return AquaUtilControlSize.getFontForSize(df, size);
        }
    }

    protected Color getForegroundColor(AbstractButton b) {
        boolean isEnabled = b.getModel().isEnabled();
        Color existingColor = b.getForeground();
        if (existingColor == null || existingColor instanceof UIResource || !isEnabled) {
            Border border = b.getBorder();
            if (border instanceof AquaButtonBorder) {
                AquaButtonBorder bb = (AquaButtonBorder) border;
                return bb.getTextColor(b, colorDefaults);
            }

            // Most buttons do not display text differently when the window is inactive
            boolean isSelected = b.isSelected();
            return colorDefaults.getTextColor(isEnabled, isSelected);
        }
        return existingColor;
    }

    public static boolean isOnToolbar(final AbstractButton b) {
        Component parent = b.getParent();
        while (parent != null) {
            if (parent instanceof JToolBar) return true;
            parent = parent.getParent();
        }
        return false;
    }

    protected void setButtonMarginIfNeeded(final AbstractButton b, final Insets insets) {
        final Insets margin = b.getMargin();
        if (margin == null || (margin instanceof UIResource)) {
            b.setMargin(insets);
        }
    }

    protected void installListeners(final AbstractButton b) {
        final AquaButtonListener listener = createButtonListener(b);
        if (listener != null) {
            // put the listener in the button's client properties so that
            // we can get at it later
            b.putClientProperty(this, listener);

            b.addMouseListener(listener);
            b.addMouseMotionListener(listener);
            b.addFocusListener(listener);
            b.addPropertyChangeListener(listener);
            b.addChangeListener(listener);
            b.addAncestorListener(listener);
            b.addActionListener(listener);
        }
        installHierListener(b);
        AquaUtilControlSize.addSizePropertyListener(b);
        AquaFullKeyboardFocusableHandler.addListener(b);
    }

    protected void installKeyboardActions(final AbstractButton b) {
        final BasicButtonListener listener = (BasicButtonListener)b.getClientProperty(this);
        if (listener != null) listener.installKeyboardActions(b);
    }

    // Uninstall PLAF
    public void uninstallUI(final JComponent c) {
        uninstallKeyboardActions((AbstractButton)c);
        uninstallListeners((AbstractButton)c);
        uninstallDefaults((AbstractButton)c);
        //BasicHTML.updateRenderer(c, "");
        removeCachedIcons((AbstractButton) c);
    }

    protected void uninstallKeyboardActions(final AbstractButton b) {
        final BasicButtonListener listener = (BasicButtonListener)b.getClientProperty(this);
        if (listener != null) listener.uninstallKeyboardActions(b);
    }

    protected void uninstallListeners(final AbstractButton b) {
        final AquaButtonListener listener = (AquaButtonListener)b.getClientProperty(this);
        b.putClientProperty(this, null);
        if (listener != null) {
            b.removeMouseListener(listener);
            b.removeMouseMotionListener(listener);
            b.removeFocusListener(listener);
            b.removeChangeListener(listener);
            b.removePropertyChangeListener(listener);
            b.removeAncestorListener(listener);
            b.removeActionListener(listener);
        }
        uninstallHierListener(b);
        AquaUtilControlSize.removeSizePropertyListener(b);
        AquaFullKeyboardFocusableHandler.removeListener(b);
    }

    protected void uninstallDefaults(final AbstractButton b) {
        LookAndFeel.uninstallBorder(b);
        AquaUtilControlSize.uninstallDefaultFont(b);
    }

    protected void removeCachedIcons(AbstractButton b) {

        // It is unfortunate that there is no way to ask for a disabled icon without risking creating one via the
        // LAF. We use a static variable to temporarily inhibit our LAF from creating an icon.

        boolean oldValue = AquaLookAndFeel.suppressCreationOfDisabledButtonIcons;
        AquaLookAndFeel.suppressCreationOfDisabledButtonIcons = true;

        try {
            if (b.getSelectedIcon() instanceof UIResource) {
                b.setSelectedIcon(null);
            }

            if (b.getDisabledIcon() instanceof UIResource) {
                b.setDisabledIcon(null);
            }

            if (b.getDisabledSelectedIcon() instanceof UIResource) {
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

            b.putClientProperty(UNSELECTED_ICON_PROPERTY, null);
        } finally {
            AquaLookAndFeel.suppressCreationOfDisabledButtonIcons = oldValue;
        }
    }

    // Create Listeners
    protected AquaButtonListener createButtonListener(final AbstractButton b) {
        return new AquaButtonListener(b);
    }

    // Paint Methods
    public void paint(Graphics g, final JComponent c) {
        final AbstractButton b = (AbstractButton)c;
        final ButtonModel model = b.getModel();

        final Insets i = c.getInsets();

        int width = b.getWidth();
        int height = b.getHeight();

        Rectangle viewRect = new Rectangle(width, height);
        Rectangle iconRect = new Rectangle();
        Rectangle textRect = new Rectangle();

        // we are overdrawing here with translucent colors so we get
        // a darkening effect. How can we avoid it. Try clear rect?
        if (b.isOpaque()) {
            g.setColor(c.getBackground());
            g.fillRect(viewRect.x, viewRect.y, viewRect.width, viewRect.height);
        }

        AquaButtonBorder aquaBorder = null;
        if (((AbstractButton)c).isBorderPainted()) {
            final Border border = c.getBorder();

            if (border instanceof AquaButtonBorder) {
                // only do this if borders are on!
                // this also takes care of focus painting.
                aquaBorder = (AquaButtonBorder)border;
                aquaBorder.paintBackground(c, g, viewRect.x, viewRect.y, viewRect.width, viewRect.height);
            }
        } else {
            if (b.isOpaque()) {
                viewRect.x = i.left - 2;
                viewRect.y = i.top - 2;
                viewRect.width = width - (i.right + viewRect.x) + 4;
                viewRect.height = height - (i.bottom + viewRect.y) + 4;
                if (b.isContentAreaFilled() || model.isSelected()) {
                    if (model.isSelected()) // Toggle buttons
                    g.setColor(c.getBackground().darker());
                    else g.setColor(c.getBackground());
                    g.fillRect(viewRect.x, viewRect.y, viewRect.width, viewRect.height);
                }
            }

//            // needs focus to be painted
//            // for now we don't know exactly what to do...we'll see!
//            if (b.isFocusPainted() && b.hasFocus()) {
//                // paint UI specific focus
//                paintFocus(g, b, viewRect, textRect, iconRect);
//            }
        }

        // Some button types do not allow any application provided content. For example, a help button.

        if (aquaBorder != null && !aquaBorder.allowsContent()) {
            return;
        }

        // performs icon and text rect calculations
        Icon sizingIcon = null;
        if (aquaBorder != null) {
            sizingIcon = aquaBorder.getSizingIcon(b);
        }
        final String text = layoutAndGetText(g, b, aquaBorder, i, viewRect, iconRect, textRect, sizingIcon);

        // Paint the Icon
        if (b.getIcon() != null) {
            paintIcon(g, b, iconRect);
        }

        if (textRect.width == 0) {
            textRect.width = 50;
        }

        if (text != null && !text.equals("")) {
            final View v = (View)c.getClientProperty(BasicHTML.propertyKey);
            if (v != null) {
                v.paint(g, textRect);
            } else {
                paintText(g, b, textRect, text);
            }
        }
    }

    protected String layoutAndGetText(Graphics g,
                                      AbstractButton b,
                                      AquaButtonBorder aquaBorder,
                                      Insets i,
                                      Rectangle viewRect,
                                      Rectangle iconRect,
                                      Rectangle textRect,
                                      Icon sizingIcon) {
        // re-initialize the view rect to the selected insets
        viewRect.x = i.left;
        viewRect.y = i.top;
        viewRect.width = b.getWidth() - (i.right + viewRect.x);
        viewRect.height = b.getHeight() - (i.bottom + viewRect.y);

        // reset the text and icon rects
        textRect.x = textRect.y = textRect.width = textRect.height = 0;
        iconRect.x = iconRect.y = iconRect.width = iconRect.height = 0;

        // setup the font
        g.setFont(b.getFont());
        final FontMetrics fm = g.getFontMetrics();

        // layout the text and icon
        final String originalText = b.getText();
        final Icon ic = sizingIcon != null ? sizingIcon : b.getIcon();
        final String text = AquaUtils.layoutCompoundLabel(b, fm, originalText, ic, b.getVerticalAlignment(), b.getHorizontalAlignment(), b.getVerticalTextPosition(), b.getHorizontalTextPosition(), viewRect, iconRect, textRect, originalText == null ? 0 : b.getIconTextGap());
        if (text == originalText || aquaBorder == null) return text; // everything fits

        // if the text didn't fit - check if the aqua border has alternate Insets that are more adhering
        final Insets alternateContentInsets = aquaBorder.getContentInsets(b, b.getWidth(), b.getHeight());
        if (alternateContentInsets != null) {
            // recursively call and don't pass AquaBorder
            return layoutAndGetText(g, b, null, alternateContentInsets, viewRect, iconRect, textRect, sizingIcon);
        }

        // there is no Aqua border, go with what we've got
        return text;
    }

    /**
     * Paint the appropriate icon based on the button state.
     * This method should not be called unless the button has an icon.
     */
    protected void paintIcon(Graphics g, AbstractButton b, Rectangle localIconRect) {
        Icon icon = getIcon(b);

        Graphics2D gg = null;

        if (icon.getIconWidth() != localIconRect.width || icon.getIconHeight() != localIconRect.height) {
            gg = (Graphics2D) g.create();
            g = gg;
            gg.translate(localIconRect.x, localIconRect.y);
            gg.scale(localIconRect.getWidth() / icon.getIconWidth(), localIconRect.getHeight() / icon.getIconHeight());
            gg.translate(-localIconRect.x, -localIconRect.y);
        }

        icon.paintIcon(b, g, localIconRect.x, localIconRect.y);

        if (gg != null) {
            gg.dispose();
        }
    }

    /**
     * Obtain the icon to use based on the button state.
     * This method should not be called unless the button has an icon.
     * @param b The button.
     * @return the icon to use.
     */
    protected Icon getIcon(AbstractButton b) {
        Icon icon = getSpecialIcon(b);
        return icon != null ? icon : b.getIcon();
    }

    /**
     * Obtain a special icon to use based on the button state.
     * This method should not be called unless the button has an icon.
     * @param b The button.
     * @return the icon to use, or null if no special icon is needed.
     */
    protected Icon getSpecialIcon(AbstractButton b) {
        final ButtonModel model = b.getModel();

        if (!model.isEnabled()) {
            if (model.isSelected()) {
                return b.getDisabledSelectedIcon();
            } else {
                return b.getDisabledIcon();
            }
        } else if (model.isPressed() && model.isArmed()) {
            return getPressedIcon(b);
        } else if (b.isRolloverEnabled() && model.isRollover()) {
            if (model.isSelected()) {
                return b.getRolloverSelectedIcon();
            } else {
                return b.getRolloverIcon();
            }
        } else if (model.isSelected()) {
            return getSelectedIcon(b);
        } else {
            return getUnselectedIcon(b);
        }
    }

    /**
     * Obtain a special icon to use when a button is selected.
     * @param b The button.
     * @return the icon to use, or null if no special icon is needed.
     */
    protected Icon getSelectedIcon(AbstractButton b) {
        Icon icon = b.getSelectedIcon();
        if (icon != null) {
            return icon;
        }

        icon = createSelectedIcon(b, b.getIcon());
        if (icon != null) {
            b.setSelectedIcon(icon);
        }

        return icon;
    }

    /**
     * Create a special icon to use when a button is selected, if appropriate.
     * @param b The button.
     * @param source The button icon.
     * @return the icon to use, or null if no special icon is needed.
     */
    protected Icon createSelectedIcon(AbstractButton b, Icon source) {
        Border border = b.getBorder();
        if (border instanceof AquaButtonBorder) {
            AquaButtonBorder bb = (AquaButtonBorder) border;
            return bb.createSelectedIcon(b, source);
        }
        return null;
    }

    /**
     * Obtain a special icon to use when a button is not selected.
     * @param b The button.
     * @return the icon to use, or null if no special icon is needed.
     */
    protected Icon getUnselectedIcon(AbstractButton b) {
        Object o = b.getClientProperty(UNSELECTED_ICON_PROPERTY);
        if (o instanceof Icon) {
            return (Icon) o;
        }

        Icon icon = createUnselectedIcon(b, b.getIcon());
        if (icon == null) {
            icon = b.getIcon();
        }
        b.putClientProperty(UNSELECTED_ICON_PROPERTY, icon);
        return icon;
    }

    /**
     * Create a special icon to use when a button is not selected, if appropriate.
     * @param b The button.
     * @param source The button icon.
     * @return the icon to use, or null if no special icon is needed.
     */
    protected Icon createUnselectedIcon(AbstractButton b, Icon source) {
        Border border = b.getBorder();
        if (border instanceof AquaButtonBorder) {
            AquaButtonBorder bb = (AquaButtonBorder) border;
            return bb.createUnselectedIcon(b, source);
        }
        return null;
    }

    /**
     * Obtain a special icon to use when a button is pressed.
     * @param b The button.
     * @return the icon to use, or null if no special icon is needed.
     */
    protected Icon getPressedIcon(AbstractButton b) {
        Icon icon = b.getPressedIcon();
        if (icon != null) {
            return icon;
        }

        icon = createPressedIcon(b, b.getIcon());
        if (icon != null) {
            b.setPressedIcon(icon);
        }

        return icon;
    }

    /**
     * Create a special icon to use when a button is pressed, if appropriate.
     * @param b The button.
     * @param source The button icon.
     * @return the icon to use, or null if no special icon is needed.
     */
    protected Icon createPressedIcon(AbstractButton b, Icon source) {
        Border border = b.getBorder();
        if (border instanceof AquaButtonBorder) {
            AquaButtonBorder bb = (AquaButtonBorder) border;
            return bb.createPressedIcon(b, source);
        }

        return createDefaultPressedIcon(source);
    }

    protected ImageIcon createDefaultPressedIcon(Icon source) {
        int width = source.getIconWidth();
        int height = source.getIconHeight();
        BufferedImage im = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB_PRE);
        Graphics2D g = im.createGraphics();
        source.paintIcon(null, g, 0, 0);
        g.setColor(new Color(0, 0, 0, 128));
        g.setComposite(AlphaComposite.SrcAtop);
        g.fillRect(0, 0, width, height);
        return new ImageIcon(im);
    }

    /**
     * This method is called by AbstractButton via the LAF to obtain an icon to use when a button is disabled.
     * @param b The button.
     * @param source The button icon.
     * @return the icon to use.
     */
    public Icon createDisabledIcon(AbstractButton b, ImageIcon source) {
        Border border = b.getBorder();
        if (border instanceof AquaButtonBorder) {
            AquaButtonBorder bb = (AquaButtonBorder) border;
            Icon icon = bb.createDisabledIcon(b, source);
            if (icon != null) {
                return icon;
            }
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
    public Icon createDisabledSelectedIcon(AbstractButton b, ImageIcon source) {
        Border border = b.getBorder();
        if (border instanceof AquaButtonBorder) {
            AquaButtonBorder bb = (AquaButtonBorder) border;
            Icon icon = bb.createDisabledSelectedIcon(b, source);
            if (icon != null) {
                return icon;
            }
        }
        return createDefaultDisabledIcon(source);
    }

    protected ImageIcon createDefaultDisabledIcon(ImageIcon source) {
        GrayFilter filter = new GrayFilter(true, 50);
        return new ImageIconUIResource(AquaImageFactory.applyFilter(source.getImage(), filter));
    }

    protected void paintText(final Graphics g, final AbstractButton b, final Rectangle localTextRect, final String text) {
        paintText(g, (JComponent) b, localTextRect, text);
    }

    /**
     * As of Java 2 platform v 1.4 this method should not be used or overriden.
     * Use the paintText method which takes the AbstractButton argument.
     */
    protected void paintText(final Graphics g, final JComponent c, final Rectangle localTextRect, final String text) {
        final Graphics2D g2d = g instanceof Graphics2D ? (Graphics2D)g : null;

        final AbstractButton b = (AbstractButton)c;
        final ButtonModel model = b.getModel();
        final FontMetrics fm = g.getFontMetrics();
        final int mnemonicIndex = AquaMnemonicHandler.isMnemonicHidden() ? -1 : b.getDisplayedMnemonicIndex();

        Color foreground = getForegroundColor(b);
        g.setColor(foreground);
        AquaUtils.drawStringUnderlineCharAt(c, g, text, mnemonicIndex, localTextRect.x, localTextRect.y + fm.getAscent());
    }

    protected void paintButtonPressed(final Graphics g, final AbstractButton b) {
        paint(g, b);
    }

    // Layout Methods
    public Dimension getMinimumSize(JComponent c) {
        AbstractButton b = (AbstractButton) c;
        Border border = b.getBorder();

        Dimension d;

        if (border instanceof AquaButtonBorder) {
            AquaButtonBorder bb = (AquaButtonBorder) border;
            d = bb.getMinimumButtonSize(b);
        } else {
            d = getPreferredSize(b);
        }

        final View v = (View)c.getClientProperty(BasicHTML.propertyKey);
        if (v != null) {
            d.width -= v.getPreferredSpan(View.X_AXIS) - v.getMinimumSpan(View.X_AXIS);
        }
        return d;
    }

    public Dimension getPreferredSize(final JComponent c) {
        final AbstractButton b = (AbstractButton) c;

        final Border border = b.getBorder();

        if (border instanceof AquaButtonBorder) {
            AquaButtonBorder bb = (AquaButtonBorder) border;
            return bb.getPreferredButtonSize(b);
        } else {
            return getPreferredButtonSize(b, b.getIconTextGap(), null);
        }
    }

    public Dimension getMaximumSize(final JComponent c) {
        final Dimension d = getPreferredSize(c);

        final View v = (View)c.getClientProperty(BasicHTML.propertyKey);
        if (v != null) {
            d.width += v.getMaximumSpan(View.X_AXIS) - v.getPreferredSpan(View.X_AXIS);
        }

        return d;
    }

    final static RecyclableSingleton<AquaHierarchyButtonListener> fHierListener = new RecyclableSingletonFromDefaultConstructor<AquaHierarchyButtonListener>(AquaHierarchyButtonListener.class);
    static AquaHierarchyButtonListener getAquaHierarchyButtonListener() {
        return fHierListener.get();
    }

    // We need to know when ordinary JButtons are put on JToolbars, but not JComboBoxButtons
    // JToggleButtons always have the same border

    private boolean shouldInstallHierListener(final AbstractButton b) {
        return  (b instanceof JButton || b instanceof JToggleButton && !(b instanceof AquaComboBoxButton) && !(b instanceof JCheckBox) && !(b instanceof JRadioButton));
    }

    protected void installHierListener(final AbstractButton b) {
        if (shouldInstallHierListener(b)) {
            // super put the listener in the button's client properties
            b.addHierarchyListener(getAquaHierarchyButtonListener());
        }
    }

    protected void uninstallHierListener(final AbstractButton b) {
        if (shouldInstallHierListener(b)) {
            b.removeHierarchyListener(getAquaHierarchyButtonListener());
        }
    }

    static class AquaHierarchyButtonListener implements HierarchyListener {
        // Everytime a hierarchy is change we need to check if the button if moved on or from
        // a toolbar. If that is the case, we need to re-set the border of the button.
        public void hierarchyChanged(final HierarchyEvent e) {
            if ((e.getChangeFlags() & HierarchyEvent.PARENT_CHANGED) == 0) return;

            final Object o = e.getSource();
            if (!(o instanceof AbstractButton)) return;

            final AbstractButton b = (AbstractButton)o;
            final ButtonUI ui = b.getUI();
            if (!(ui instanceof AquaButtonUI)) return;

            ((AquaButtonUI)ui).configure(b);
        }
    }

    class AquaButtonListener extends BasicButtonListener implements ActionListener, AncestorListener {
        protected final AbstractButton b;

        public AquaButtonListener(final AbstractButton b) {
            super(b);
            this.b = b;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            // If the button is a color well and no other action listeners are defined, bring up a
            // color chooser.
            Object o = b.getClientProperty(LAYOUT_CONFIGURATION_PROPERTY);
            if (o instanceof ButtonLayoutConfiguration) {
                ButtonLayoutConfiguration bg = (ButtonLayoutConfiguration) o;
                if (bg.getButtonWidget() == AquaUIPainter.ButtonWidget.BUTTON_COLOR_WELL) {
                    ActionListener[] listeners = b.getActionListeners();
                    if (listeners.length == 1) {
                        toggleColorChooser(b);
                    }
                }
            }
        }

        public void focusGained(final FocusEvent e) {
            ((Component)e.getSource()).repaint();
        }

        public void focusLost(final FocusEvent e) {
            // 10-06-03 VL: [Radar 3187049]
            // If focusLost arrives while the button has been left-clicked this would disarm the button,
            // causing actionPerformed not to fire on mouse release!
            //b.getModel().setArmed(false);
            ((Component)e.getSource()).repaint();
        }

        public void propertyChange(final PropertyChangeEvent e) {
            super.propertyChange(e);

            final String propertyName = e.getPropertyName();

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

            if (AbstractButton.VERTICAL_ALIGNMENT_CHANGED_PROPERTY.equals(propertyName)) {
                // A change to the preferred content height can change the selected button widget
                configure(b);
                return;
            }

            if ("componentOrientation".equals(propertyName)) {
                final Border border = b.getBorder();
                if (border instanceof AquaSegmentedButtonBorder) {
                    configure(b);
                }
            }
        }

        public void ancestorMoved(final AncestorEvent e) {}

        public void ancestorAdded(final AncestorEvent e) {
            updateDefaultButton();
        }

        public void ancestorRemoved(final AncestorEvent e) {
            updateDefaultButton();
        }

        protected void updateDefaultButton() {
            if (!(b instanceof JButton)) return;
            if (!((JButton)b).isDefaultButton()) return;

            final JRootPane rootPane = b.getRootPane();
            if (rootPane == null) return;

            final RootPaneUI ui = rootPane.getUI();
            if (!(ui instanceof AquaRootPaneUI)) return;
            ((AquaRootPaneUI)ui).updateDefaultButton(rootPane);
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

        if (AquaSharedColorChooser.connect(owner)) {
            b.putClientProperty(COLOR_CHOOSER_OWNER_PROPERTY, owner);
            b.setSelected(true);
        }
    }

    public static Dimension getPreferredButtonSize(AbstractButton b, int textIconGap, Icon substituteIcon) {
        if (b.getComponentCount() > 0) {
            return null;
        }

        Icon icon = substituteIcon != null ? substituteIcon : b.getIcon();
        String text = b.getText();

        Font font = b.getFont();
        FontMetrics fm = b.getFontMetrics(font);

        Rectangle iconR = new Rectangle();
        Rectangle textR = new Rectangle();
        Rectangle viewR = new Rectangle(Short.MAX_VALUE, Short.MAX_VALUE);

        SwingUtilities.layoutCompoundLabel(
            b, fm, text, icon,
            b.getVerticalAlignment(), b.getHorizontalAlignment(),
            b.getVerticalTextPosition(), b.getHorizontalTextPosition(),
            viewR, iconR, textR, (text == null ? 0 : textIconGap)
        );

        /* The preferred size of the button is the size of
         * the text and icon rectangles plus the buttons insets.
         */

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
        String text = b.getText();
        FontMetrics fm = b.getFontMetrics(font);

        Rectangle iconR = new Rectangle();
        Rectangle textR = new Rectangle();
        Rectangle viewR = new Rectangle(Short.MAX_VALUE, Short.MAX_VALUE);

        SwingUtilities.layoutCompoundLabel(
            b, fm, text, icon,
            b.getVerticalAlignment(), b.getHorizontalAlignment(),
            b.getVerticalTextPosition(), b.getHorizontalTextPosition(),
            viewR, iconR, textR, (text == null ? 0 : textIconGap)
        );

        Rectangle r = iconR.union(textR);
        return r.getSize();
    }
}
