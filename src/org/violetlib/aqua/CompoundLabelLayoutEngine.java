/*
 * Copyright (c) 2025-2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;
import javax.swing.text.View;

import org.jetbrains.annotations.*;
import org.violetlib.jnr.Insets2D;
import org.violetlib.jnr.aqua.AquaUIPainter;
import org.violetlib.jnr.aqua.ButtonLayoutConfiguration;
import org.violetlib.jnr.aqua.LayoutConfiguration;

import static org.violetlib.aqua.OSXSystemProperties.macOS26;

/**
 * Allocate space for displaying an icon and a text label in a button-like control.
 * The icon and text are centered in the available space.
 */
public class CompoundLabelLayoutEngine {
    private final @Nullable LayoutConfiguration g;
    private @Nullable Dimension iconSize;
    private final @Nullable View view;
    private final @Nullable String text;
    private final int iconTextGap;
    private final @Nullable FontMetrics fm;
    private final @NotNull CompoundLabelAlignment alignment;
    private final @Nullable AquaUIPainter painter;
    private final @NotNull String clipString = "...";

    /**
     * Create a layout engine that arranges the label and icon of a button-line control, with an optional icon outline
     * @param g The button configuration.
     * @param iconSize The icon size, may be null if no icon is to be displayed.
     * @param v An optional source of rendered (HTML) text to display.
     * @param text Optional text to be displayed, if {@code v} is not supplied. If {@code v} and {@code text} are both
     *             null, no space will be allocated for the display of text. Otherwise, space is allocated for text
     *             even if {@code text} is empty.
     * @param fm The font metrics used to determine the space required for text. This parameter is required if
     *           {@code text} is not null and not empty.
     * @param alignment The requested alignment when space is allocated both for an icon and for text.
     * @param iconTextGap The gap between the spaces allocated for an icon and for text.
     * @param painter The native painter, used to determine the size of a split button outline.
     */
    public CompoundLabelLayoutEngine(@Nullable LayoutConfiguration g,
                                     @Nullable Dimension iconSize,
                                     @Nullable View v,
                                     @Nullable String text,
                                     @Nullable FontMetrics fm,  // required if text is not empty
                                     @NotNull CompoundLabelAlignment alignment,
                                     int iconTextGap,
                                     @Nullable AquaUIPainter painter)
    {
        if (iconSize != null && (iconSize.width == 0 || iconSize.height == 0)) {
            iconSize = null;
        }
        this.iconSize = iconSize;

        this.g = g;
        this.iconTextGap = text == null || iconSize == null ? 0 : iconTextGap;
        this.view = v;
        this.text = v != null ? null : text;
        this.fm = fm;
        this.alignment = alignment;
        this.painter = painter;
    }

    /**
     * Allocate space for displaying an icon and a text label in a button-like control.
     * @param controlWidth The width of the control, including content insets.
     * @param controlHeight The height of the control, including content insets.
     * @param insets The content insets.
     */
    public @NotNull ButtonLayoutInfo getLayoutInfo(int controlWidth, int controlHeight, @NotNull Insets2D insets) {
        int horizontalInsets = (int) Math.ceil((insets.getLeft() + insets.getRight()));
        int verticalInsets = (int) Math.ceil((insets.getTop() + insets.getBottom()));
        int top = (int) insets.getTop();
        int left = (int) insets.getLeft();
        int extraWidth = top < insets.getTop() ? 1 : 0;
        int extraHeight = left < insets.getLeft() ? 1 : 0;
        int contentWidth = Math.max(0, controlWidth - horizontalInsets + extraWidth);
        int contentHeight = Math.max(0, controlHeight - verticalInsets + extraHeight);
        return getLayoutInfo(contentWidth, contentHeight).offset(top, left);
    }

    /**
     * Allocate space for displaying an icon and a text label in a button-like control.
     * @param availableWidth The width of the available space for content. Can be very large to compute a preferred
     * size.
     * @param availableHeight The height of the available space for content. Can be very large to compute a preferred
     * size.
     */
    public @NotNull ButtonLayoutInfo getLayoutInfo(int availableWidth, int availableHeight) {
        boolean isPreferredSizeCalculation = availableWidth >= Short.MAX_VALUE || availableHeight >= Short.MAX_VALUE;
        Rectangle iconRect = null;
        Rectangle textRect = null;
        Rectangle outlineRect = null;
        String clippedText = null;
        Rectangle contentRect;

        // Determine the space required for text
        TextLayoutInfo textLayoutInfo;
        if (text != null) {
            if (fm == null) {
                throw new UnsupportedOperationException("Font metrics required");
            }
            if (text.isEmpty()) {
                // layout as if there is text (to match other buttons with text)
                textLayoutInfo = new TextLayoutInfo(1, fm.getHeight(), 0, false);
            } else {
                textLayoutInfo = AquaUtils.getTextLayoutInfo(view, text, fm);
            }
        } else {
            textLayoutInfo = null;
        }

        // Check to see if the icon will need to be reduced in size.
        if (iconSize != null) {
            int clipStringWidth = SwingUtilities.computeStringWidth(fm, clipString);
            float scalingFactor = getIconScaleFactor(availableWidth, availableHeight, iconSize, alignment,
              textLayoutInfo, iconTextGap, clipStringWidth);
            if (scalingFactor < 1) {
                int scaledWidth = (int) Math.floor(scalingFactor * iconSize.width);
                int scaledHeight = (int) Math.floor(scalingFactor * iconSize.height);
                iconSize = new Dimension(scaledWidth, scaledHeight);
            }
            iconRect = new Rectangle(0, 0, iconSize.width, iconSize.height);
        }

        // Assign space for text. Check to see if the text will need to be clipped.
        if (textLayoutInfo != null && textLayoutInfo.width > 0 && textLayoutInfo.height > 0) {
            textRect = new Rectangle(0, 0, textLayoutInfo.width, textLayoutInfo.height);

            int availTextWidth = availableWidth;
            if (!alignment.isVerticalArrangement() && iconSize != null) {
                availTextWidth = availableWidth - (iconSize.width + iconTextGap);
            }
            if (textLayoutInfo.width > availTextWidth) {
                String substitute = computeClippedText(availTextWidth);
                clippedText = substitute != null ? substitute : clipString;
                textRect.width = SwingUtilities.computeStringWidth(fm, clippedText);
            }
        }

        // Determine if a space is needed for a split toolbar item outline
        int iconWidth = iconSize != null ? iconSize.width : 0;
        int iconHeight = iconSize != null ? iconSize.height : 0;
        SplitButtonInfo outlineInfo = getSplitButtonInfo(iconWidth, iconHeight);
        Dimension outlineSize = null;
        Insets outlineInsets = null;

        if (outlineInfo != null) {
            outlineSize = new Dimension(outlineInfo.buttonSize.width, outlineInfo.buttonSize.height);
            outlineRect = new Rectangle(0, 0, outlineInfo.buttonSize.width, outlineInfo.buttonSize.height);
            outlineInsets = outlineInfo.contentInsets;
        }

        // Compute the overall content size.
        int contentWidth = 0;
        int contentHeight = 0;
        if (alignment.isVerticalArrangement()) {
            contentWidth = maxWidth(iconRect, textRect, outlineSize);
            contentHeight = sumHeight(iconRect, textRect, outlineSize, iconTextGap);
        } else {
            contentHeight = maxHeight(iconRect, textRect);
            contentWidth = sumWidth(iconRect, textRect, iconTextGap);
        }
        contentRect = new Rectangle(0, 0, contentWidth, contentHeight);

        // If calculating a preferred size, only the content dimensions are needed.
        if (!isPreferredSizeCalculation) {
            // This code supports four options:
            //   vertical arrangement, text on top or bottom (horizontally centered)
            //   horizontal arrangement, text on left or right (vertically centered)

            // Outline takes precedence. Icon is centered in outline.
            Rectangle effectiveIconRect = outlineRect != null ? outlineRect : iconRect;

            if (alignment.isVerticalArrangement()) {
                if (effectiveIconRect != null) {
                    effectiveIconRect.x = (availableWidth - effectiveIconRect.width) / 2;
                }
                if (textRect != null) {
                    textRect.x = (availableWidth - textRect.width) / 2;
                }
                int top = getTop(availableHeight - contentHeight);
                int bottom = getBottom(availableHeight - contentHeight);
                if (alignment.isTopText()) {
                    if (textRect != null) {
                        textRect.y = top;
                    }
                    if (effectiveIconRect != null) {
                        effectiveIconRect.y = availableHeight - effectiveIconRect.height - bottom;
                    }
                } else {
                    if (effectiveIconRect != null) {
                        effectiveIconRect.y = top;
                    }
                    if (textRect != null) {
                        textRect.y = availableHeight - textLayoutInfo.height - bottom;
                    }
                }

            } else {
                if (effectiveIconRect != null) {
                    effectiveIconRect.y = (availableHeight - effectiveIconRect.height) / 2;
                }
                if (textRect != null) {
                    textRect.y = (availableHeight - textRect.height) / 2;
                }
                int left = getLeft(availableWidth - contentWidth);
                int right = getRight(availableWidth - contentWidth);
                if (alignment.isLeftText()) {
                    if (textRect != null) {
                        textRect.x = left;
                    }
                    if (effectiveIconRect != null) {
                        effectiveIconRect.x = availableWidth - effectiveIconRect.width - right;
                    }
                } else {
                    if (effectiveIconRect != null) {
                        effectiveIconRect.x = left;
                    }
                    if (textRect != null) {
                        textRect.x = availableWidth - textRect.width - right;
                    }
                }
            }

            if (effectiveIconRect != iconRect && iconRect != null) {
                iconRect.x = effectiveIconRect.x + (effectiveIconRect.width - iconRect.width) / 2;
                iconRect.y = effectiveIconRect.y + (effectiveIconRect.height - iconRect.height) / 2;
            }
        }

        if (textRect != null && textLayoutInfo.leftSideBearing < 0) {
            textRect.width += textLayoutInfo.leftSideBearing;
            textRect.x -= textLayoutInfo.leftSideBearing;
        }

        ButtonLayoutInfo basic = new ButtonLayoutInfo(iconRect, textRect, contentRect, clippedText);
        if (outlineRect != null) {
            return new SplitToolbarItemLayoutInfo(basic, outlineRect, outlineInsets);
        }
        return basic;
    }

    private int getTop(int extraVertical) {
        return extraVertical / 2;
    }

    private int getBottom(int extraVertical) {
        return extraVertical / 2;
    }

    private int getLeft(int extraHorizontal) {
        return extraHorizontal / 2;
    }

    private int getRight(int extraHorizontal) {
        return extraHorizontal / 2;
    }

    private static int maxWidth(@Nullable Rectangle r1, @Nullable Rectangle r2, @Nullable Dimension d) {
        int width = 0;
        if (r1 != null && r1.width > width) {
            width = r1.width;
        }
        if (r2 != null && r2.width > width) {
            width = r2.width;
        }
        if (d != null && d.width > width) {
            width = d.width;
        }
        return width;
    }

    private static int sumHeight(@Nullable Rectangle iconRect, @Nullable Rectangle textRect, @Nullable Dimension outline, int iconTextGap) {
        int height = 0;
        if (outline != null) {
            // the outline is presumed to be larger than the icon
            height = outline.height;
        } else if (iconRect != null) {
            height = iconRect.height;
        }
        if (textRect != null) {
            if (height > 0) {
                height += iconTextGap;
            }
            height += textRect.height;
        }
        return height;
    }

    private static int maxHeight(@Nullable Rectangle r1, @Nullable Rectangle r2) {
        int height = 0;
        if (r1 != null && r1.width > height) {
            height = r1.height;
        }
        if (r2 != null && r2.width > height) {
            height = r2.height;
        }
        return height;
    }

    private static int sumWidth(@Nullable Rectangle iconRect, @Nullable Rectangle textRect, int iconTextGap) {
        int width = 0;
        if (iconRect != null) {
            width = iconRect.width;
        }
        if (textRect != null) {
            if (width > 0) {
                width += iconTextGap;
            }
            width += textRect.width;
        }
        return width;
    }

    private @Nullable String computeClippedText(int availableWidth) {
        if (view != null) {
            return null; // unable to clip rendered text
        }
        if (text == null || text.isEmpty()) {
            return null; // nothing to clip
        }
        if (fm == null) {
            return null; // unable to compute text width
        }

        int totalWidth = SwingUtilities.computeStringWidth(fm, clipString);
        int nChars;
        int len = text.length();
        for (nChars = 0; nChars < len; nChars++) {
            int charIndex = (nChars % 2 == 0) ? nChars / 2 : len - 1 - nChars / 2;
            totalWidth += fm.charWidth(text.charAt(charIndex));
            if (totalWidth > availableWidth) {
                break;
            }
        }
        return text.substring(0, nChars / 2) + clipString + text.substring(len - nChars / 2);
    }

    /**
     * Determine if the icon needs to be reduced in size to fit in the available space.
     * @param availableWidth The total available width.
     * @param availableHeight The total available height.
     * @param iconSize The icon size.
     * @param alignment The compound layout alignment parameters.
     * @param textLayoutInfo The text layout information, or null if there is no text.
     * @param iconTextGap The icon text gap, ignored if there is no text.
     * @param clipWidth The width of the text that will be displayed if the text does not fit.
     * @return the scaling factor (1 if no reduction is needed, otherwise a number less than 1).
     */
    private float getIconScaleFactor(int availableWidth,
                                     int availableHeight,
                                     @NotNull Dimension iconSize,
                                     @NotNull CompoundLabelAlignment alignment,
                                     @Nullable TextLayoutInfo textLayoutInfo,
                                     int iconTextGap,
                                     int clipWidth) {
        if (availableWidth < Short.MAX_VALUE && availableHeight < Short.MAX_VALUE && iconSize.width > 0 && iconSize.height > 0) {
            if (alignment.isVerticalArrangement()) {
                // In a vertical arrangement, the icon height is limited by the available height, the gap, and the text
                // height.
                if (textLayoutInfo != null) {
                    availableHeight -= (iconTextGap + textLayoutInfo.height);
                }
            } else {
                // In a horizonal arrangement, the icon width is limited by the available width, the gap, and the text
                // width. The text width will be limited by truncation, so only the clip string width matters.
                if (textLayoutInfo !=  null) {
                    availableWidth -= (iconTextGap + clipWidth);
                }
            }

            int revisedHeight = Math.min(iconSize.height, availableHeight);
            int revisedWidth = Math.min(iconSize.width, availableWidth);
            if (revisedHeight < iconSize.height || revisedWidth < iconSize.width) {
                // scaling is needed
                float vsf = ((float) revisedHeight) / iconSize.height;
                float hsf = ((float) revisedWidth) / iconSize.width;
                return Math.min(vsf, hsf);
            }
        }
        return 1;
    }

    /**
     * Information about a "virtual" button that encloses the icon in a toolbar item.
     */
    private static class SplitButtonInfo {
        public final @NotNull Dimension buttonSize;  // the layout size of the virtual button
        public final @NotNull Insets contentInsets;  // the content insets of the virtual button

        public SplitButtonInfo(@NotNull Dimension buttonSize, @NotNull Insets contentInsets) {
            this.buttonSize = buttonSize;
            this.contentInsets = contentInsets;
        }
    }

    private @Nullable SplitButtonInfo getSplitButtonInfo(int iconWidth, int iconHeight) {
        if (g instanceof ButtonLayoutConfiguration) {
            ButtonLayoutConfiguration bg = (ButtonLayoutConfiguration) g;
            if (bg.getWidget() != AquaUIPainter.ButtonWidget.BUTTON_TOOLBAR_ITEM) {
                return null;
            }

            int width = iconWidth;
            int height = iconHeight;

            int version = AquaPainting.getVersion();
            if (version < macOS26) {
                int minimumWidth = 40;
                if (width < minimumWidth) {
                    width = minimumWidth;
                }
                Insets s = new Insets(2, 2, 2, 2);
                width = width + s.left + s.right;
                height = height + s.top + s.bottom;
                return new SplitButtonInfo(new Dimension(width, height), s);
            } else {

                // Although the virtual buttons look like glass buttons, it is not clear that they have the same
                // layout parameters. To get the right look, special parameters appear to be necessary.

                int sideInsets = 14;
                height = 36;
                Insets s = new Insets(9, 14, 9, 14);
                width += s.left + s.right;
                return new SplitButtonInfo(new Dimension(width, height), s);

//                AquaUIPainter.Size sz = bg.getSize();
//                AquaUIPainter.ButtonWidget w = AquaUIPainter.ButtonWidget.BUTTON_GLASS;
//                ButtonLayoutConfiguration ig = new ButtonLayoutConfiguration(w, sz, bg.getLayoutDirection(), bg.getContent());
//                Insets s = new Insets(0, 0, 0, 0);
//
//                if (painter != null) {
//                    AquaUILayoutInfo uiLayoutInfo = painter.getLayoutInfo();
//                    LayoutInfo info = uiLayoutInfo.getLayoutInfo(ig);
//                    Insetter insetter = uiLayoutInfo.getButtonLabelInsets(ig);
//
//                    int minimumWidth = (int) Math.ceil(info.getMinimumVisualWidth());
//                    int fixedHeight = (int) Math.ceil(info.getFixedVisualHeight());
//                    // fixed height is expected
//                    int minimumHeight = (int) Math.ceil(info.getMinimumVisualHeight());
//
//                    Insetter ins = uiLayoutInfo.getContentInsets(bg);
//                    if (ins != null && ins.isInvertible()) {
//                        Dimension d = ins.expand(new Dimension(width, height));
//                        minimumWidth = Math.max(minimumWidth, d.width);
//                        minimumHeight = Math.max(minimumHeight, d.height);
//                    }
//
//                    if (fixedHeight > 0) {
//                        minimumHeight = fixedHeight;
//                    }
//
//                    if (width < minimumWidth) {
//                        width = minimumWidth;
//                    }
//                    if (height < minimumHeight) {
//                        height = minimumHeight;
//                    }
//                    if (insetter != null) {
//                        Dimension d = new Dimension(width, height);
//                        s = insetter.asInsets(d);
//                    }
//                }
//                return new SplitButtonInfo(new Dimension(width, height), s);
            }
        }
        return null;
    }
}
