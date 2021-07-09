/*
 * Copyright (c) 2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;
import javax.swing.tree.DefaultTreeCellEditor;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreePath;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * A custom default tree cell editor that uses AquaTreeUI to paint the icon, with the goal of consistent template icon
 * painting by default renderers and editors.
 */
public class AquaTreeCellEditor
        extends DefaultTreeCellEditor {

    public AquaTreeCellEditor(@NotNull JTree tree, @Nullable DefaultTreeCellRenderer renderer) {
        super(tree, renderer);
    }

    @Override
    protected Container createContainer() {
        return new AquaEditorContainer();
    }

    private class AquaEditorContainer
            extends Container {
        @Override
        public void paint(Graphics g) {
            int width = getWidth();
            int height = getHeight();

            // Then the icon.
            if(editingIcon != null) {
                int yLoc = calculateIconY(editingIcon);
                if (getComponentOrientation().isLeftToRight()) {
                    paintIcon(g, editingIcon, 0, yLoc);
                } else {
                    paintIcon(g, editingIcon, width - editingIcon.getIconWidth(), yLoc);
                }
            }

            // Border selection color
            Color background = getBorderSelectionColor();
            if(background != null) {
                g.setColor(background);
                g.drawRect(0, 0, width - 1, height - 1);
            }
            super.paint(g);
            super.paint(g);
        }

        protected void paintIcon(@NotNull Graphics g, @NotNull Icon icon, int x, int y) {
            TreePath path = tree.getEditingPath();
            assert path != null;
            AquaTreeUI ui = AquaUtils.getUI(tree, AquaTreeUI.class);
            if (ui != null) {
                ui.paintEditorIcon(g, path, icon, x, y);
            } else {
                icon.paintIcon(this, g, x, y);
            }
        }

        public void doLayout() {
            if(editingComponent != null) {
                int width = getWidth();
                int height = getHeight();
                if (getComponentOrientation().isLeftToRight()) {
                    editingComponent.setBounds(
                            offset, 0, width - offset, height);
                } else {
                    editingComponent.setBounds(
                            0, 0, width - offset, height);
                }
            }
        }

        /**
         * Calculate the y location for the icon.
         */
        private int calculateIconY(Icon icon) {
            // To make sure the icon position matches that of the
            // renderer, use the same algorithm as JLabel
            // (SwingUtilities.layoutCompoundLabel).
            int iconHeight = icon.getIconHeight();
            int textHeight = editingComponent.getFontMetrics(
                    editingComponent.getFont()).getHeight();
            int textY = iconHeight / 2 - textHeight / 2;
            int totalY = Math.min(0, textY);
            int totalHeight = Math.max(iconHeight, textY + textHeight) -
                    totalY;
            return getHeight() / 2 - (totalY + (totalHeight / 2));
        }

        /**
         * Returns the preferred size for the <code>Container</code>.
         * This will be at least preferred size of the editor plus
         * <code>offset</code>.
         * @return a <code>Dimension</code> containing the preferred
         *   size for the <code>Container</code>; if
         *   <code>editingComponent</code> is <code>null</code> the
         *   <code>Dimension</code> returned is 0, 0
         */
        public Dimension getPreferredSize() {
            if(editingComponent != null) {
                Dimension         pSize = editingComponent.getPreferredSize();

                pSize.width += offset + 5;

                Dimension         rSize = (renderer != null) ?
                        renderer.getPreferredSize() : null;

                if(rSize != null)
                    pSize.height = Math.max(pSize.height, rSize.height);
                if(editingIcon != null)
                    pSize.height = Math.max(pSize.height,
                            editingIcon.getIconHeight());

                // Make sure width is at least 100.
                pSize.width = Math.max(pSize.width, 100);
                return pSize;
            }
            return new Dimension(0, 0);
        }
    }
}
