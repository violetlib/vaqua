/*
 * @(#)FileRenderer.java
 *
 * Copyright (c) 2007-2013 Werner Randelshofer, Switzerland.
 * Copyright (c) 2018 Alan Snyder.
 * You may not use, copy or modify this file, except in compliance with the
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.awt.*;
import java.awt.geom.Ellipse2D;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.border.MatteBorder;
import javax.swing.plaf.BorderUIResource;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.aqua.*;
import org.violetlib.jnr.aqua.AquaUIPainter;

/**
 * The FileRenderer is used to render a file in the file chooser browser view.
 *
 * @author  Werner Randelshofer
 */
public class FileRenderer extends JLabel implements ListCellRenderer, GenericCellRenderer {

    private Icon selectedExpandingIcon;
    private Icon selectedExpandedIcon;
    private Icon focusedSelectedExpandingIcon;
    private Icon focusedSelectedExpandedIcon;
    private Icon expandingIcon;
    private Icon expandedIcon;
    private Icon emptyIcon;
    private Icon aliasBadgeIcon;
    private JFileChooser fileChooser;
    private int textIconGap;
    private int textArrowIconGap;
    private Icon icon;
    private String text;
    private Icon arrowIcon;
    private Color labelColor;
    private boolean isSelected;
    private boolean isActive;
    private boolean isGrayed;
    private boolean isAlias;
    private boolean isListView;
    private double labelRadius = 4.8;
    private Border border;

    public FileRenderer(JFileChooser fileChooser) {
        this.fileChooser = fileChooser;
        this.textIconGap = UIManager.getInt("FileChooser.browserCellTextIconGap");
        this.textArrowIconGap = UIManager.getInt("FileChooser.browserCellTextArrowIconGap");

        emptyIcon = new EmptyIcon(12, 12);  //TBD
        aliasBadgeIcon = UIManager.getIcon("FileView.aliasBadgeIcon");

        expandedIcon = UIManager.getIcon("Browser.expandedIcon");
        selectedExpandedIcon = UIManager.getIcon("Browser.selectedExpandedIcon");
        focusedSelectedExpandedIcon = UIManager.getIcon("Browser.focusedSelectedExpandedIcon");

        expandingIcon = UIManager.getIcon("Browser.expandingIcon");
        selectedExpandingIcon = UIManager.getIcon("Browser.selectedExpandingIcon");
        focusedSelectedExpandingIcon = UIManager.getIcon("Browser.focusedSelectedExpandingIcon");

        border = new EmptyBorder(0, 4, 2, 0);

        setOpaque(true);
    }

    // Overridden for performance reasons.
    @Override
    public void validate() {
    }

    @Override
    public void revalidate() {
    }

    @Override
    public void repaint(long tm, int x, int y, int width, int height) {
    }

    @Override
    public void repaint(Rectangle r) {
    }

    @Override
    protected void firePropertyChange(String propertyName, Object oldValue, Object newValue) {
    }

    @Override
    public void firePropertyChange(String propertyName, short oldValue, short newValue) {
    }

    @Override
    public void firePropertyChange(String propertyName, int oldValue, int newValue) {
    }

    @Override
    public void firePropertyChange(String propertyName, long oldValue, long newValue) {
    }

    @Override
    public void firePropertyChange(String propertyName, float oldValue, float newValue) {
    }

    @Override
    public void firePropertyChange(String propertyName, double oldValue, double newValue) {
    }

    @Override
    public void firePropertyChange(String propertyName, boolean oldValue, boolean newValue) {
    }

    public Component getListCellRendererComponent(@NotNull JList list,
                                                  @Nullable Object value,
                                                  int index,
                                                  boolean isSelected,
                                                  boolean cellHasFocus) {

        AquaListUI ui = AquaUtils.getUI(list, AquaListUI.class);
        AquaAppearance appearance = AppearanceManager.ensureAppearance(list);
        ContainerContextualColors colors = ui != null ? ui.getColors() : AquaColors.CONTAINER_COLORS;
        return getCellRendererComponent(list, appearance, colors, value, isSelected, cellHasFocus, false);
    }

    @Override
    public Component getCellRendererComponent(@NotNull JComponent container,
                                              @NotNull AquaAppearance appearance,
                                              @NotNull ContainerContextualColors colors,
                                              @Nullable Object value,
                                              boolean isSelected,
                                              boolean cellHasFocus) {
        return getCellRendererComponent(container, appearance, colors, value, isSelected, cellHasFocus, true);
    }

    protected Component getCellRendererComponent(@NotNull JComponent container,
                                                 @NotNull AquaAppearance appearance,
                                                 @NotNull ContainerContextualColors colors,
                                                 @Nullable Object value,
                                                 boolean isSelected,
                                                 boolean cellHasFocus,
                                                 boolean isListView) {

        this.isListView = isListView;

        if (!(value instanceof FileInfo)) {
            return this;
        }

        FileInfo info = (FileInfo) value;

        isGrayed = !info.isAcceptable() && !info.isTraversable();
        labelColor = null;
        {
            int tag = info.getFileLabel();
            String labelColorName = OSXFile.getTagColorName(tag);
            if (labelColorName != null) {
                EffectName effect = isGrayed ? EffectName.EFFECT_DISABLED : EffectName.EFFECT_NONE;
                labelColor = appearance.getColorForEffect(labelColorName, effect);
            }
        }

        this.isSelected = isSelected;
        this.isActive = container.isEnabled() && AquaFocusHandler.hasFocus(container);

        AquaUIPainter.State state = getState(container, isGrayed);
        AppearanceContext context = new AppearanceContext(appearance, state, isSelected, false);

        Color background = colors.getBackground(context);
        Color foreground = colors.getForeground(context);
        setBackground(AquaColors.getOrdinaryColor(background));
        setForeground(AquaColors.getOrdinaryColor(foreground));

        if (!isListView) {

            if (this.isSelected && labelColor == null) {
                if (cellHasFocus) {
                    arrowIcon = (info.isValidating()) ? focusedSelectedExpandingIcon : focusedSelectedExpandedIcon;
                } else {
                    arrowIcon = (info.isValidating()) ? selectedExpandingIcon : selectedExpandedIcon;
                }
            } else {
                arrowIcon = (info.isValidating()) ? expandingIcon : expandedIcon;
            }

            /*
              Special case: no arrow is displayed for a package even if the package is traversable (an option).
            */

            if (!info.isTraversable() || OSXFile.isVirtualFile(info.lazyGetResolvedFile())) {
                arrowIcon = (labelColor == null) ? null : emptyIcon;
            }
        } else {
            arrowIcon = null;
        }

        text = info.getUserName();
        icon = info.getIcon();

        isAlias = false;
        if (info instanceof FileSystemTreeModel.Node) {
            FileSystemTreeModel.Node n = (FileSystemTreeModel.Node) info;
            isAlias = n.isAlias();
        }

        setOpaque(!isListView);
        setEnabled(container.isEnabled());
        setFont(container.getFont());
        setBorder(isListView ? null : border);
        return this;
    }

    protected @NotNull AquaUIPainter.State getState(@NotNull JComponent c, boolean isGrayed) {
        return c.isEnabled() && !isGrayed ?
                AquaFocusHandler.hasFocus(c) ? AquaUIPainter.State.ACTIVE_DEFAULT : AquaUIPainter.State.ACTIVE
                : AquaUIPainter.State.DISABLED;
    }

    private class ConfigurableMatteBorder extends MatteBorder {
        private @NotNull Color configuredColor;

        public ConfigurableMatteBorder(int top, int left, int bottom, int right) {
            super(top, left, bottom, right, Color.BLACK);
            this.configuredColor = Color.BLACK;
        }

        public void configure(@NotNull Color c) {
            configuredColor = c;
        }

        @Override
        public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
            g.setColor(configuredColor);
            super.paintBorder(c, g, x, y, width, height);
        }
    }

    @Override
    protected void paintComponent(Graphics gr) {
        Object oldHints = AquaUtils.beginGraphics((Graphics2D) gr);
        Graphics2D g = (Graphics2D) gr;
        int width = getWidth();

        int height = getHeight();
        Insets insets = getInsets();

        resetRects();

        viewRect.setBounds(0, 0, width, height);
        viewRect.x += insets.left;
        viewRect.y += insets.top;
        viewRect.width -= insets.left + insets.right;
        viewRect.height -= insets.top + insets.bottom;

        Font textFont = getFont();
        g.setFont(textFont);
        FontMetrics textFM = g.getFontMetrics(textFont);
        if (isOpaque()) {
            g.setColor(getBackground());
            g.fillRect(0, 0, width, height);
        }

        String clippedText = layoutRenderer(
                textFM, text,
                icon, arrowIcon,
                viewRect, iconRect, textRect, arrowIconRect, labelRect,
                text == null ? 0 : textIconGap, textArrowIconGap);

        if (labelColor != null) {

            // Paint the label as a filled circle with an outline
            double r = labelRadius;
            Shape s = new Ellipse2D.Double(labelRect.x, labelRect.y, r * 2, r * 2);
            g.setPaint(labelColor);
            g.fill(s);
            g.setPaint(isSelected && isActive ? Color.WHITE : Color.LIGHT_GRAY);
            g.draw(s);
        }

        if (icon != null) {
            icon.paintIcon(this, g, iconRect.x, iconRect.y);
        }

        if (isAlias && aliasBadgeIcon != null) {
            aliasBadgeIcon.paintIcon(this, g, iconRect.x, iconRect.y);
        }

        if (clippedText != null && !clippedText.equals("")) {
            g.setColor(getForeground());
            g.drawString(clippedText, textRect.x, textRect.y + textFM.getAscent());
        }

        if (arrowIcon != null) {
            arrowIcon.paintIcon(this, g, arrowIconRect.x, arrowIconRect.y);
        }

        AquaUtils.endGraphics((Graphics2D) g, oldHints);
    }
    /**
     * The following variables are used for laying out the renderer.
     * This variables are static, because FileRenderer is always called
     * from the EventDispatcherThread, and because we do not use them in a
     * reentrant context, where a FileRenderer instance enters a method of
     * another FileRenderer instance.
     */
    private static final Rectangle zeroRect = new Rectangle(0, 0, 0, 0);
    private static Rectangle iconRect = new Rectangle();
    private static Rectangle textRect = new Rectangle();
    private static Rectangle arrowIconRect = new Rectangle();
    private static Rectangle viewRect = new Rectangle();
    private static Rectangle labelRect = new Rectangle();
    /** r is used in getPreferredSize and in paintComponent. It must not be
     * used in any method called by one of these.
     */
    private static Rectangle r = new Rectangle();

    private void resetRects() {
        iconRect.setBounds(zeroRect);
        textRect.setBounds(zeroRect);
        arrowIconRect.setBounds(zeroRect);
        labelRect.setBounds(zeroRect);
        viewRect.setBounds(0, 0, 32767, 32767);
        r.setBounds(zeroRect);
    }

    @Override
    public Dimension getPreferredSize() {
        Font textFont = getFont();
        FontMetrics textFM = getFontMetrics(textFont);

        resetRects();

        layoutRenderer(
                textFM, text,
                icon, arrowIcon,
                viewRect,
                iconRect,
                textRect,
                arrowIconRect,
                labelRect,
                text == null ? 0 : textIconGap, textArrowIconGap);

        r.setBounds(textRect);
        r = SwingUtilities.computeUnion(iconRect.x, iconRect.y, iconRect.width,
                iconRect.height, r);

        boolean isUseArrow = arrowIcon != null;
        if (isUseArrow) {
            r.width += arrowIconRect.width + textArrowIconGap;
        }

        if (labelColor != null) {
            r.width += labelRect.width + textArrowIconGap;
        }

        Insets insets = getInsets();
        if (insets != null) {
            r.width += insets.left + insets.right;
            r.height += insets.top + insets.bottom;
        }

        return r.getSize();
    }

    /**
     * Layouts the components of the renderer.
     */
    private String layoutRenderer(
            FontMetrics textFM, String text,
            Icon icon, Icon arrowIcon,
            Rectangle viewRect, Rectangle iconRect,
            Rectangle textRect,
            Rectangle arrowIconRect,
            Rectangle labelRect,
            int textIconGap, int textArrowIconGap) {

        boolean isUseArrow = arrowIcon != null;

        if (isUseArrow) {
            arrowIconRect.width = arrowIcon.getIconWidth();
            arrowIconRect.height = arrowIcon.getIconHeight();
            arrowIconRect.x = viewRect.x + viewRect.width - arrowIconRect.width;
            viewRect.width -= arrowIconRect.width + textArrowIconGap;
        }

        if (labelColor != null) {
            int d = (int) Math.ceil(2 * labelRadius);
            labelRect.width = d;
            labelRect.height = d;
            labelRect.x = viewRect.x + viewRect.width - labelRect.width;
            viewRect.width -= labelRect.width + textArrowIconGap;
        }

        text = AquaUtils.layoutCompoundLabel(
                this, textFM, text,
                icon, SwingConstants.CENTER, SwingConstants.LEFT,
                SwingConstants.CENTER, SwingConstants.RIGHT,
                viewRect, iconRect, textRect,
                textIconGap);

        if (isUseArrow) {
            viewRect.width += arrowIconRect.width + textArrowIconGap;
        }

        if (labelColor != null) {
            viewRect.width += labelRect.width + textArrowIconGap;
        }

        Rectangle jLabelRect = iconRect.union(textRect);

        if (isUseArrow) {
            arrowIconRect.y = (viewRect.y + jLabelRect.height / 2 - arrowIconRect.height / 2);
        }

        if (labelColor != null) {
            labelRect.y = (viewRect.y + jLabelRect.height / 2 - labelRect.height / 2);
        }

        if (!AquaUtils.isLeftToRight(this)) {
            int width = viewRect.width;
            iconRect.x = width - (iconRect.x + iconRect.width);
            textRect.x = width - (textRect.x + textRect.width);
            arrowIconRect.x = width - (arrowIconRect.x + arrowIconRect.width);
            labelRect.x = width - (labelRect.x + labelRect.width);
        }

        return text;
    }
}
