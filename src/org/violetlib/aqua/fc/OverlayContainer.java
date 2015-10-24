/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua.fc;

import java.awt.*;
import javax.swing.*;

public class OverlayContainer extends JComponent {

    public OverlayContainer() {
        setLayout(new MyLayoutManager());
        setOpaque(false);
    }

    @Override
    public boolean isOptimizedDrawingEnabled() {
        return false;
    }

    private class MyLayoutManager implements LayoutManager {
        @Override
        public void addLayoutComponent(String name, Component comp) {
        }

        @Override
        public void removeLayoutComponent(Component comp) {
        }

        @Override
        public Dimension preferredLayoutSize(Container parent) {
            return null;
        }

        @Override
        public Dimension minimumLayoutSize(Container parent) {
            return null;
        }

        @Override
        public void layoutContainer(Container parent) {
            int width = parent.getWidth();
            int height = parent.getHeight();
            int count = parent.getComponentCount();
            for (int i = 0; i < count; i++) {
                Component c = parent.getComponent(i);
                c.setBounds(0, 0, width, height);
            }
        }
    }
}
