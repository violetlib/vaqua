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

/**
 * Allocate space for displaying an icon and a text label in a button-like control.
 * The icon and text are centered in the available space.
 */
public class CompoundLabelLayoutEngine {
    private final @Nullable Dimension iconSize;
    private final @Nullable View view;
    private final @Nullable String text;
    private final int iconTextGap;
    private final @Nullable FontMetrics fm;
    private final @NotNull CompoundLabelAlignment alignment;
    private final @NotNull String clipString = "...";

    /**
     * Create a layout engine that arranges the label and icon of a button-like control, with an optional icon outline
     * @param iconSize The preferred icon size. This parameter may be null if no icon is to be displayed. If the
     *                 button is a split toolbar item, this parameter specifies the size of the virtual button.
     * @param v An optional source of rendered (HTML) text to display.
     * @param text Optional text to be displayed, if {@code v} is not supplied. If {@code v} and {@code text} are both
     *             null, no space will be allocated for the display of text. Otherwise, space is allocated for text
     *             even if {@code text} is empty.
     * @param fm The font metrics used to determine the space required for text. This parameter is required if
     *           {@code text} is not null and not empty.
     * @param alignment The requested alignment when space is allocated both for an icon and for text.
     * @param iconTextGap The gap between the spaces allocated for an icon and for text.
     */
    public CompoundLabelLayoutEngine(@Nullable Dimension iconSize,
                                     @Nullable View v,
                                     @Nullable String text,
                                     @Nullable FontMetrics fm,  // required if text is not empty
                                     @NotNull CompoundLabelAlignment alignment,
                                     int iconTextGap)
    {
        // No space needed for an icon
        if (iconSize != null && (iconSize.width == 0 || iconSize.height == 0)) {
            iconSize = null;
        }
        this.iconSize = iconSize;
        this.iconTextGap = text == null || iconSize == null ? 0 : iconTextGap;
        this.view = v;
        this.text = v != null ? null : text;
        this.fm = fm;
        this.alignment = alignment;
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
     * size or to compute an actual size where the width is not limited.
     */
    public @NotNull ButtonLayoutInfo getLayoutInfo(int availableWidth, int availableHeight) {
        boolean isPreferredSizeCalculation
          = AquaUtils.isUnlimitedSize(availableWidth) || AquaUtils.isUnlimitedSize(availableHeight);
        Rectangle iconRect = null;
        Rectangle textRect = null;
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

        // Check to see if the icon should be reduced in size.
        if (iconSize != null) {
            int clipStringWidth = SwingUtilities.computeStringWidth(fm, clipString);
            Dimension adjustedIconSize = getAdjustedIconSize(availableWidth, availableHeight, iconSize, alignment,
              textLayoutInfo, iconTextGap, clipStringWidth);
            iconRect = new Rectangle(0, 0, adjustedIconSize.width, adjustedIconSize.height);
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

        // Compute the overall content size.
        int contentWidth = 0;
        int contentHeight = 0;
        if (alignment.isVerticalArrangement()) {
            contentWidth = maxWidth(iconRect, textRect);
            contentHeight = sumHeight(iconRect, textRect, iconTextGap);
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

            if (alignment.isVerticalArrangement()) {
                if (iconRect != null) {
                    iconRect.x = (availableWidth - iconRect.width) / 2;
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
                    if (iconRect != null) {
                        iconRect.y = availableHeight - iconRect.height - bottom;
                    }
                } else {
                    if (iconRect != null) {
                        iconRect.y = top;
                    }
                    if (textRect != null) {
                        textRect.y = availableHeight - textLayoutInfo.height - bottom;
                    }
                }

            } else {
                if (iconRect != null) {
                    iconRect.y = (availableHeight - iconRect.height) / 2;
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
                    if (iconRect != null) {
                        iconRect.x = availableWidth - iconRect.width - right;
                    }
                } else {
                    if (iconRect != null) {
                        iconRect.x = left;
                    }
                    if (textRect != null) {
                        textRect.x = availableWidth - textRect.width - right;
                    }
                }
            }
        }

        if (textRect != null && textLayoutInfo.leftSideBearing < 0) {
            textRect.width += textLayoutInfo.leftSideBearing;
            textRect.x -= textLayoutInfo.leftSideBearing;
        }

        return new ButtonLayoutInfo(iconRect, textRect, contentRect, clippedText);
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

    private static int maxWidth(@Nullable Rectangle r1, @Nullable Rectangle r2) {
        int width = 0;
        if (r1 != null && r1.width > width) {
            width = r1.width;
        }
        if (r2 != null && r2.width > width) {
            width = r2.width;
        }
        return width;
    }

    private static int sumHeight(@Nullable Rectangle iconRect, @Nullable Rectangle textRect, int iconTextGap) {
        int height = 0;
        if (iconRect != null) {
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
        if (r1 != null && r1.height > height) {
            height = r1.height;
        }
        if (r2 != null && r2.height > height) {
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
     * Adjust the icon size as needed, if there is not enough space for the full size. The only supported adjustment
     * is to decrease the size preserving the aspect ratio.
     * @param availableWidth The total available width for the button.
     * @param availableHeight The total available height for the button.
     * @param iconSize The preferred icon size.
     * @param alignment The compound layout alignment parameters.
     * @param textLayoutInfo The text layout information, or null if there is no text.
     * @param iconTextGap The icon text gap, ignored if there is no text.
     * @param clipWidth The width of the text that will be displayed if the text does not fit.
     * @return the adjusted icon size.
     */

    private @NotNull Dimension getAdjustedIconSize(int availableWidth,
                                                   int availableHeight,
                                                   @NotNull Dimension iconSize,
                                                   @NotNull CompoundLabelAlignment alignment,
                                                   @Nullable TextLayoutInfo textLayoutInfo,
                                                   int iconTextGap,
                                                   int clipWidth) {
        if (iconSize.width == 0 || iconSize.height == 0) {
            return new Dimension(0, 0);
        }
        if (AquaUtils.isUnlimitedSize(availableWidth) && AquaUtils.isUnlimitedSize(availableHeight)) {
            return iconSize;
        }
        float scaleFactor = getIconScaleFactor(availableWidth, availableHeight, iconSize, alignment, textLayoutInfo, iconTextGap, clipWidth);
        int revisedWidth = Math.round(iconSize.width * scaleFactor);
        int revisedHeight = Math.round(iconSize.height * scaleFactor);
        return new Dimension(revisedWidth, revisedHeight);
    }

    /**
     * Determine if the icon needs to be reduced in size to fit in the available space.
     * @param availableWidth The total available width for the content.
     * @param availableHeight The total available height for the content.
     * @param iconSize The icon size (possibly adjusted to satisfy other constraints).
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
        if (!AquaUtils.isUnlimitedSize(availableWidth) && AquaUtils.isUnlimitedSize(availableHeight) && iconSize.width > 0 && iconSize.height > 0) {
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
}
