/*
 * Copyright (c) 2007-2013 Werner Randelshofer, Switzerland.
 * Copyright (c) 2018-2020 Alan Snyder.
 * You may not use, copy or modify this file, except in compliance with the
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.awt.*;
import java.awt.geom.Ellipse2D;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;

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

    private final @NotNull ImageIcon arrowIcon;
    private @Nullable Color arrowColor;
    private Icon aliasBadgeIcon;
    private int textIconGap;
    private int textArrowIconGap;
    private Icon icon;
    private String text;
    private @Nullable Color labelColor;
    private boolean isSelected;
    private boolean isFocused;
    private boolean isGrayed;
    private boolean isAlias;
    private boolean isListView;
    private double labelRadius = 4.8;
    private Border border;

    private static final Color LABEL_BORDER_SELECTED = Color.WHITE;

    public FileRenderer(@NotNull JFileChooser fileChooser) {
        this.textIconGap = UIManager.getInt("FileChooser.browserCellTextIconGap");
        this.textArrowIconGap = UIManager.getInt("FileChooser.browserCellTextArrowIconGap");
        aliasBadgeIcon = UIManager.getIcon("FileView.aliasBadgeIcon");
        arrowIcon = AquaIcon.getBrowserExpandArrowTemplate();
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
        this.isFocused = container.isEnabled() && AquaFocusHandler.hasFocus(container);

        AquaUIPainter.State state = getState(container, isGrayed);
        AppearanceContext context = new AppearanceContext(appearance, state, isSelected, false);

        Color background = colors.getBackground(context);
        Color foreground = colors.getForeground(context);
        setBackground(AquaAppearance.getOrdinaryColor(background));
        setForeground(AquaAppearance.getOrdinaryColor(foreground));

        arrowColor = getArrowColor(info, cellHasFocus, appearance);

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

    protected @Nullable Color getArrowColor(@NotNull FileInfo info, boolean isFocused, @NotNull AquaAppearance appearance) {
        if (isListView) {
            return null;
        }

        /*
          Special case: no arrow is displayed for a package even if the package is traversable (an option).
        */

        if (!info.isTraversable() || OSXFile.isVirtualFile(info.lazyGetResolvedFile())) {
            return null;
        }

        if (this.isSelected) {
            if (isFocused) {
                return appearance.getColor("selectedBrowserExpandArrow_focused");
            } else {
                return appearance.getColor("selectedBrowserExpandArrow");
            }
        } else {
            return appearance.getColor("browserExpandArrow");
        }
    }

    protected @Nullable Color getLabelBorderColor() {
        return isSelected && isFocused ? LABEL_BORDER_SELECTED : null;
    }

    protected @NotNull AquaUIPainter.State getState(@NotNull JComponent c, boolean isGrayed) {
        return c.isEnabled() && !isGrayed ?
                AquaFocusHandler.hasFocus(c) ? AquaUIPainter.State.ACTIVE_DEFAULT : AquaUIPainter.State.ACTIVE
                : AquaUIPainter.State.DISABLED;
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
                textFM,
                viewRect, iconRect, textRect, arrowIconRect, labelRect,
                text == null ? 0 : textIconGap, textArrowIconGap);

        if (labelColor != null) {
            // Paint the label as a filled circle with an outline
            double r = labelRadius;
            Shape s = new Ellipse2D.Double(labelRect.x, labelRect.y, r * 2, r * 2);
            g.setPaint(labelColor);
            g.fill(s);
            Color labelBorderColor = getLabelBorderColor();
            if (labelBorderColor != null) {
                g.setPaint(labelBorderColor);
                g.draw(s);
            }
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

        if (arrowColor != null) {
            Image arrow = AquaImageFactory.getProcessedImage(arrowIcon, arrowColor);
            if (arrow != null) {
                g.drawImage(arrow, arrowIconRect.x, arrowIconRect.y, null);
            }
        }

        AquaUtils.endGraphics(g, oldHints);
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
                textFM,
                viewRect,
                iconRect,
                textRect,
                arrowIconRect,
                labelRect,
                text == null ? 0 : textIconGap, textArrowIconGap);

        r.setBounds(textRect);
        r = SwingUtilities.computeUnion(iconRect.x, iconRect.y, iconRect.width, iconRect.height, r);

        if (arrowColor != null) {
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
     * Lay out the components of the renderer.
     */
    private String layoutRenderer(
            @NotNull FontMetrics textFM,
            @NotNull Rectangle viewRect,
            @NotNull Rectangle iconRect,
            @NotNull Rectangle textRect,
            @NotNull Rectangle arrowRect,
            @NotNull Rectangle labelRect,
            int textIconGap,
            int textArrowIconGap) {

        boolean isUseArrow = arrowColor != null;
        boolean isUseLabel = labelColor != null;

        int arrowWidth = arrowIcon.getIconWidth();
        int arrowHeight = arrowIcon.getIconHeight();
        int labelWidth = (int) Math.ceil(2 * labelRadius);
        int labelHeight = labelWidth;

        if (isUseArrow) {
            arrowRect.width = arrowWidth;
            arrowRect.height = arrowHeight;
            arrowRect.x = viewRect.x + viewRect.width - arrowWidth;
        }

        viewRect.width -= arrowWidth + textArrowIconGap;

        if (isUseLabel) {
            labelRect.width = labelWidth;
            labelRect.height = labelHeight;
            labelRect.x = viewRect.x + viewRect.width - labelWidth;
        }

        viewRect.width -= labelWidth + textArrowIconGap;

        text = AquaUtils.layoutCompoundLabel(
                this, textFM, text,
                icon, SwingConstants.CENTER, SwingConstants.LEFT,
                SwingConstants.CENTER, SwingConstants.RIGHT,
                viewRect, iconRect, textRect,
                textIconGap);

        viewRect.width += labelWidth + textArrowIconGap;
        viewRect.width += arrowWidth + textArrowIconGap;

        Rectangle jLabelRect = iconRect.union(textRect);

        if (isUseArrow) {
            arrowRect.y = (viewRect.y + jLabelRect.height / 2 - arrowHeight / 2);
        }

        if (isUseLabel) {
            labelRect.y = (viewRect.y + jLabelRect.height / 2 - labelHeight / 2);
        }

        if (!AquaUtils.isLeftToRight(this)) {
            int width = viewRect.width;
            iconRect.x = width - (iconRect.x + iconRect.width);
            textRect.x = width - (textRect.x + textRect.width);
            arrowRect.x = width - (arrowRect.x + arrowWidth);
            labelRect.x = width - (labelRect.x + labelWidth);
        }

        return text;
    }
}
