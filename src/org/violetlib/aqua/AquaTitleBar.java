/*
 * Copyright (c) 2015-2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.util.Arrays;
import javax.swing.*;

import org.jetbrains.annotations.NotNull;
import org.violetlib.jnr.Insetter;
import org.violetlib.jnr.LayoutInfo;
import org.violetlib.jnr.aqua.*;

import static org.violetlib.jnr.aqua.AquaUIPainter.TitleBarButtonWidget.*;

/**
 * A title bar for an internal frame.
 */
public class AquaTitleBar {
    private static final int sMaxIconWidth = 15;
    private static final int sMaxIconHeight = sMaxIconWidth;
    private static final int sAfterIconPad = 5;

    protected final AquaUIPainter titleBarPainter = AquaPainting.create();

    protected final JInternalFrame frame;
    protected final AquaUIPainter.TitleBarWidget widget;
    protected final TitleBarLayoutConfiguration layoutConfiguration;
    protected final int titleBarHeight;

    private int width;
    private TitleBarLayout titleBarLayout;  // recomputed as needed, do not access directly

    private class TitleBarLayout {
        int buttonAreaLeft;
        int buttonAreaRight;
        Rectangle titleBounds;
        Rectangle closeButtonBounds;
        Rectangle minimizeButtonBounds;
        Rectangle resizeButtonBounds;
    }

    public AquaTitleBar(JInternalFrame frame, AquaUIPainter.TitleBarWidget widget) {
        this.frame = frame;
        this.widget = widget;
        layoutConfiguration = new TitleBarLayoutConfiguration(this.widget);
        LayoutInfo layoutInfo = titleBarPainter.getLayoutInfo().getLayoutInfo(layoutConfiguration);
        titleBarHeight = (int) Math.ceil(layoutInfo.getFixedVisualHeight());
    }

    public void setWidth(int w) {
        if (w != width) {
            width = w;
            invalidateLayout();
        }
    }

    public void invalidateLayout() {
        titleBarLayout = null;
    }

    public int getTitleBarHeight() {
        return titleBarHeight;
    }

    public int getWhichButtonHit(int x, int y) {
        TitleBarLayout layout = getTitleBarLayout();
        if (layout.closeButtonBounds.contains(x, y)) {
            return AquaInternalFrameBorder.kCloseButton;
        }
        if (layout.minimizeButtonBounds.contains(x, y)) {
            return AquaInternalFrameBorder.kIconButton;
        }
        if (layout.resizeButtonBounds.contains(x, y)) {
            return AquaInternalFrameBorder.kGrowButton;
        }
        return -1;
    }

    public boolean getWithinRolloverArea(int x, int y) {
        if (y < 0 || y >= titleBarHeight) {
            return false;
        }
        TitleBarLayout layout = getTitleBarLayout();
        return x >= layout.buttonAreaLeft && x <= layout.buttonAreaRight;
    }

    protected TitleBarLayout getTitleBarLayout() {
        if (titleBarLayout == null) {
            titleBarLayout = createTitleBarLayout();
        }
        return titleBarLayout;
    }

    protected TitleBarLayout createTitleBarLayout() {
        TitleBarLayout result = new TitleBarLayout();
        result.buttonAreaLeft = Integer.MAX_VALUE;
        result.buttonAreaRight = 0;

        AquaUILayoutInfo uiLayoutInfo = titleBarPainter.getLayoutInfo();

        for (AquaUIPainter.TitleBarButtonWidget bw : Arrays.asList(CLOSE_BOX, MINIMIZE_BOX, RESIZE_BOX)) {
            Insetter s = uiLayoutInfo.getTitleBarButtonInsets(layoutConfiguration, bw);
            Rectangle bounds = s.apply(width, titleBarHeight);
            result.buttonAreaLeft = Math.min(result.buttonAreaLeft, bounds.x);
            result.buttonAreaRight = Math.max(result.buttonAreaRight, bounds.x + bounds.width);
            if (bw == CLOSE_BOX) {
                result.closeButtonBounds = bounds;
            } else if (bw == MINIMIZE_BOX) {
                result.minimizeButtonBounds = bounds;
            } else if (bw == RESIZE_BOX) {
                result.resizeButtonBounds = bounds;
            }
        }

        {
            Insetter s = titleBarPainter.getLayoutInfo().getTitleBarLabelInsets(layoutConfiguration);
            if (s != null) {
                result.titleBounds = s.apply(width, titleBarHeight);
            }
        }

        return result;
    }

    public void paint(@NotNull Graphics g) {
        AquaAppearance appearance = AppearanceManager.ensureAppearance(frame);
        boolean isSelected = frame.isSelected() || widget == AquaUIPainter.TitleBarWidget.UTILITY_WINDOW;
        EffectName effect = isSelected ? EffectName.EFFECT_NONE : EffectName.EFFECT_DISABLED;
        Color textColor = appearance.getColorForOptionalEffect("text", effect);
        assert textColor != null;
        // paint the background and buttons
        Configuration tg = getConfiguration();
        AquaUtils.configure(titleBarPainter, frame.getRootPane(), width, titleBarHeight);
        titleBarPainter.getPainter(tg).paint(g, 0, 0);
        // now the title and the icon
        paintTitleContents(g, textColor);
    }

    public Rectangle getButtonArea() {
        TitleBarLayout layout = getTitleBarLayout();
        return new Rectangle(layout.buttonAreaLeft, 0, layout.buttonAreaRight - layout.buttonAreaLeft, titleBarHeight);
    }

    protected TitleBarConfiguration getConfiguration() {
        AquaUIPainter.State state = frame.isSelected() ? AquaUIPainter.State.ACTIVE : AquaUIPainter.State.INACTIVE;
        AquaUIPainter.State closeButtonState = getButtonState(AquaInternalFrameBorder.kCloseButton, frame.isClosable());
        AquaUIPainter.State minimizeButtonState = getButtonState(AquaInternalFrameBorder.kIconButton, frame.isIconifiable());
        AquaUIPainter.State resizeButtonState = getButtonState(AquaInternalFrameBorder.kGrowButton, frame.isMaximizable());
        TitleBarConfiguration.ResizeAction resizeAction = getResizeAction(frame);
        boolean isDirty = isDirty();
        return new TitleBarConfiguration(widget, state, closeButtonState, minimizeButtonState, resizeButtonState, resizeAction, isDirty);
    }

    protected AquaUIPainter.State getButtonState(int buttonType, boolean isEnabled) {
        AquaInternalFrameUI ui = (AquaInternalFrameUI)frame.getUI();
        int buttonPressedIndex = ui.getWhichButtonPressed();
        boolean overButton = ui.getMouseOverPressedButton();
        boolean rollover = ui.getRollover();
        boolean frameSelected = frame.isSelected() || widget == AquaUIPainter.TitleBarWidget.UTILITY_WINDOW;
        boolean isActive = rollover || frameSelected;
        return getState(buttonPressedIndex == buttonType && overButton, rollover, isActive, isEnabled);
    }

    protected AquaUIPainter.State getState(boolean pressed, boolean rollover, boolean active, boolean enabled) {
        if (!enabled) return AquaUIPainter.State.DISABLED;
        if (!active) return AquaUIPainter.State.INACTIVE;
        if (pressed) return AquaUIPainter.State.PRESSED;
        if (rollover) return AquaUIPainter.State.ROLLOVER;
        return AquaUIPainter.State.ACTIVE;
    }

    protected void paintTitleContents(@NotNull Graphics g, @NotNull Color textColor) {
        TitleBarLayout layout = getTitleBarLayout();
        Rectangle titleBounds = layout.titleBounds;
        if (titleBounds == null) {
            return;
        }

        Font f = g.getFont();

        g.setFont(frame.getFont());

        // Center text vertically.
        FontMetrics fm = g.getFontMetrics();
        int baseline = titleBounds.y + (titleBounds.height + fm.getAscent() - fm.getLeading() - fm.getDescent()) / 2;

        int iconWidth = getIconWidth();
        if (iconWidth > 0) {
            iconWidth += sAfterIconPad;
        }
        int availTextWidth = titleBounds.width - iconWidth;

        String text = frame.getTitle();
        int totalTextWidth = 0;

        int startXPosition = titleBounds.x;
        boolean wasTextShortened = false;
        // shorten the string to fit in the
        if ((text != null) && !(text.equals(""))) {
            totalTextWidth = SwingUtilities.computeStringWidth(fm, text);
            String clipString = "\u2026";
            if (totalTextWidth > availTextWidth) {
                wasTextShortened = true;
                totalTextWidth = SwingUtilities.computeStringWidth(fm, clipString);
                int nChars;
                for (nChars = 0; nChars < text.length(); nChars++) {
                    int nextCharWidth = fm.charWidth(text.charAt(nChars));
                    if ((totalTextWidth + nextCharWidth) > availTextWidth) {
                        break;
                    }
                    totalTextWidth += nextCharWidth;
                }
                text = text.substring(0, nChars) + clipString;
            }

            if (!wasTextShortened) {
                // center it!
                startXPosition = (titleBounds.width - (totalTextWidth + iconWidth)) / 2;
                if (startXPosition < titleBounds.x) {
                    startXPosition = titleBounds.x;
                }
            }

            g.setColor(textColor);
            JavaSupport.drawString(frame, (Graphics2D) g, text, startXPosition + iconWidth, baseline);
            g.setFont(f);
        }

        int iconYPosition = titleBounds.y + (titleBounds.height - getIconHeight()) / 2;
        paintTitleIcon(g, startXPosition, iconYPosition);
    }

    protected void paintTitleIcon(Graphics g, int x, int y) {
        Icon icon = frame.getFrameIcon();
        if (icon == null) {
            icon = UIManager.getIcon("InternalFrame.icon");
        }
        if (icon == null) {
            return;
        }

        // Resize to 16x16 if necessary.
        if (icon instanceof ImageIcon && (icon.getIconWidth() > sMaxIconWidth || icon.getIconHeight() > sMaxIconHeight)) {
            Image img = ((ImageIcon)icon).getImage();
            ((ImageIcon)icon).setImage(img.getScaledInstance(sMaxIconWidth, sMaxIconHeight, Image.SCALE_SMOOTH));
        }

        icon.paintIcon(frame, g, x, y);
    }

    protected int getIconWidth() {
        int width = 0;

        Icon icon = frame.getFrameIcon();
        if (icon == null) {
            icon = UIManager.getIcon("InternalFrame.icon");
        }

        if (icon != null) {
            // Resize to 16x16 if necessary.
            width = Math.min(icon.getIconWidth(), sMaxIconWidth);
        }

        return width;
    }

    protected int getIconHeight() {
        int height = 0;

        Icon icon = frame.getFrameIcon();
        if (icon == null) {
            icon = UIManager.getIcon("InternalFrame.icon");
        }

        if (icon != null) {
            // Resize to 16x16 if necessary.
            height = Math.min(icon.getIconHeight(), sMaxIconHeight);
        }

        return height;
    }

    protected TitleBarConfiguration.ResizeAction getResizeAction(JInternalFrame frame) {
        AquaInternalFrameUI ui = (AquaInternalFrameUI) frame.getUI();
        if (frame.isMaximizable() && !ui.getOption()) {
            return frame.isMaximum() ? TitleBarConfiguration.ResizeAction.FULL_SCREEN_EXIT : TitleBarConfiguration.ResizeAction.FULL_SCREEN_ENTER;
        } else {
            return TitleBarConfiguration.ResizeAction.ZOOM_ENTER;
        }
    }

    // defaults to false
    protected boolean isDirty() {
        Object dirty = frame.getClientProperty("windowModified");
        return Boolean.TRUE.equals(dirty);
    }
}
