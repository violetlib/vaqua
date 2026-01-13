/*
 * Copyright (c) 2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.util.function.Consumer;
import javax.swing.*;

import org.jetbrains.annotations.*;

/**
 * Support for component appearances.
 * <p>
 * This implementation supports the concept of a current appearance that can be used during painting. Ideally,
 * the current appearance would be established at the start of a painting operation and changed temporarily when
 * painting a component that specifies a different appearance.
 * <p>
 * Painting operations can start at components other than the root pane. When that happens, the appropriate
 * appearance may not be known at the time the first VAqua component is painted.
 */

public class AppearanceSupport
{
    public interface ComponentPainter
    {
        void paint(@NotNull Graphics2D g, @NotNull JComponent jc, @NotNull PaintingContext pc);
    }


    /**
     * The current appearance. If not null, a paint operation is in progress and the appearance is correct
     * for the component currently being painted.
     */
    private static @Nullable AquaAppearance currentAppearance;

    private static @Nullable PaintingContext currentContext;

    private static @Nullable JComponent currentComponent;




    /**
     * This method must be used to perform the paint operation for a VAqua component.
     */
    public static void withContext(@NotNull Graphics g, @NotNull JComponent jc, @NotNull ComponentPainter painter)
    {
        AquaAppearance a = getAppearanceForComponent(jc);
        AquaAppearance oldAppearance = currentAppearance;
        PaintingContext oldContext = currentContext;
        JComponent oldComponent = currentComponent;
        currentAppearance = a;
        currentContext = PaintingContext.of(a);
        currentComponent = jc;
        try {
            painter.paint((Graphics2D) g, jc, currentContext);
        } finally {
            currentAppearance = oldAppearance;
            currentContext = oldContext;
            currentComponent = oldComponent;
        }
    }

    public static void withContext(@NotNull JComponent jc, @NotNull Consumer<PaintingContext> r)
    {
        AquaAppearance a = getAppearanceForComponent(jc);
        AquaAppearance oldAppearance = currentAppearance;
        PaintingContext oldContext = currentContext;
        JComponent oldComponent = currentComponent;
        currentAppearance = a;
        currentContext = PaintingContext.of(a);
        currentComponent = jc;
        try {
            r.accept(currentContext);
        } finally {
            currentAppearance = oldAppearance;
            currentContext = oldContext;
            currentComponent = oldComponent;
        }
    }
    /**
     * Identify the appearance to use when painting the specified component and all of its descendants that do not
     * specify a different appearance.
     */
    private static @NotNull AquaAppearance getAppearanceForComponent(@NotNull JComponent jc)
    {
        AquaAppearance specifiedAppearance = getSpecifiedAppearance(jc);
        if (specifiedAppearance != null) {
            return specifiedAppearance;
        }

        if (currentAppearance != null) {
            return currentAppearance;
        }

        // No appearance has yet been defined for this painting operation. That implies that the ancestors of this
        // component that are part of the tree being painted are not VAqua components. However, the ancestors of that
        // tree may be VAqua components, and one of them might specify an appearance. If not, the application effective
        // appearance should be used.

        Component c = jc;
        for (;;) {
            Container parent = c.getParent();
            if (parent == null) {
                break;
            }
            AquaAppearance a = getSpecifiedAppearance(parent);
            if (a != null) {
                return a;
            }
            c = parent;
        }

        return AppearanceManager.getApplicationAppearance();
    }

    private static @Nullable AquaAppearance getSpecifiedAppearance(@NotNull Component c)
    {
        if (c instanceof JComponent) {
            JComponent jc = (JComponent) c;
            String appearanceName = AppearanceManager.getSpecifiedAppearanceName(jc);
            if (appearanceName == null) {
                return null;
            }
            return AquaAppearances.get(appearanceName);
        }
        return null;
    }
}
