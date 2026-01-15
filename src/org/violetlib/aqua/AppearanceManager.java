/*
 * Copyright (c) 2018-2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.basic.BasicSplitPaneDivider;

import org.jetbrains.annotations.*;

/**
 * This class supports the association of appearances with components.
 */

/**
 * This implementation supports the concept of a current appearance that can be used during painting. Ideally,
 * the current appearance would be established at the start of a painting operation and changed temporarily when
 * painting a component that specifies a different appearance.
 * <p>
 * Painting operations can start at components other than the root pane. When that happens, the appropriate
 * appearance may not be known at the time the first VAqua component is painted.
 */


public class AppearanceManager {

    // This implementation supports the concept of a current painting context that includes the appearance that should
    // be used by the current painting operation. The current painting context can be thought of as a thread local
    // variable, but as AWT is single threaded, it is actually a global variable. (In a more functional world, the
    // painting context would be a parameter of every painting method.)

    // Conceptually, a painting context is created for the root of a component tree when the tree is painted by the
    // repaint manager. The painting of a component tree is performed by recursive descent. Each component is called
    // three times: first to paint the component itself, second to paint the child components, and last to paint the
    // component border. As a global variable, the context is available to the child components; however, the context
    // may be superseded during the painting of a subtree whose root component has specified the use of a different
    // appearance. Effectively, there is a stack of painting contexts. In practice, the component replaces the context
    // at the start of its paint operation and restores the previous context when its paint operation completes.

    // In actuality, only VAqua components (components with a VAqua UI) know about the painting context, and the root of
    // the tree being painted may not be a VAqua component. A more accurate description of the process is that any VAqua
    // component that needs the painting context must create one if there is no current painting context when it is
    // painted. Unless the component is the root pane, it cannot assume that it should use the application effective
    // appearance. The ancestors of the component must be examined in case one of them specified a different appearance.

    // The actual implementation is complicated because the repaint manager is not configurable by a look and feel.
    // Creation of a painting context is actually performed by the component UI, which is called by paintComponent.
    // Restoration of the previous painting context is performed by a border installed on the component by the UI.
    // (Between paintComponent and paintBorder, the child components are painted.) This border is called a context
    // border, to distinguish it from borders that are present to occupy space and possibly to paint.

    // The installation of the context border can be tricky. Many VAqua components already have a border, specified by
    // VAqua and not reconfigurable. Other VAqua components allow the application to define a border. To support the
    // latter components, the context border is a wrapper that delegates to the real border (if any). There are
    // operations in VAqua that examine the component border. These operations are not interested in the context border.
    // They must use special methods to access the wrapped border.

    // This class has been converted to use a simple, no-cache implementation. A component UI that is potentially
    // dependent on the appearance and appearance settings is required to obtain an appearance and the system colors
    // that it needs each time it is painted. (This information is available from the painting context.) Doing so
    // ensures that the UI will always have the correct colors for its appearance and the current appearance settings.

    // The new implementation is much simpler than the previous caching implementation. Unlike the old implementation,
    // the new implementation easily handles changes to the system appearance and to appearance settings. The old
    // implementation required event listeners and complicated logic to ensure that cached appearance data was properly
    // updated after a change to the system appearance or to appearance settings. (It was not completely correct,
    // either.)

    // The new implementation supports a feature that otherwise would be impossible to fully support: the ability for a
    // component with no UI or a non-component such as a border to be appearance sensitive, even when used in the
    // context of an appearance that is not the application effective appearance. This information is available from the
    // current painting context.

    public static boolean isDebug = false;

    public static final String AQUA_APPEARANCE_NAME_KEY = "Aqua.appearanceName";  // the name of an explicitly chosen appearance

    private static boolean isResponderRegistered;
    /**
     * The current appearance. If not null, a paint operation is in progress and the appearance is correct for the
     * component currently being painted.
     */
    private static @Nullable AquaAppearance currentAppearance;
    /**
     * The current painting context. If not null, a paint operation is in progress and the painting context is correct
     * for the component currently being painted.
     */
    private static @Nullable PaintingContext currentContext;
    /**
     * The current component being painted, as derived from requests for a painting context.
     */
    private static @Nullable JComponent currentComponent;

    private static boolean isSpecifiedAppearanceFeatureEnabled;

    private static boolean ignoreBorderChange;
    private static boolean ignoreBorderPaintedChange;

    private static final @NotNull PropertyChangeListener myPropertyChangeListener = AppearanceManager::propertyChanged;


    /* package private */ static void initialize()
    {
//        if (!isResponderRegistered) {
//            isResponderRegistered = true;
//            SystemPropertyChangeManager.register(AppearanceManager::systemPropertyChanged);
//        }
    }

    /**
     * Install support for a component whose UI is potentially appearance-sensitive.
     */
    public static void install(@NotNull JComponent c)
    {
        c.addPropertyChangeListener(myPropertyChangeListener);
        ContextBorder.install(c);
    }

    /**
     * Uninstall support for a component whose UI is potentially appearance-sensitive.
     */
    public static void uninstall(@NotNull JComponent c)
    {
        c.removePropertyChangeListener(myPropertyChangeListener);
        ContextBorder cb = AquaBorderSupport.getContextBorder(c);
        if (cb != null) {
            cb.uninstall();
        }
    }

    private static void propertyChanged(@NotNull PropertyChangeEvent e)
    {
        String name = e.getPropertyName();
        if (name != null) {
            Object source = e.getSource();
            if (source instanceof JComponent) {
                JComponent jc = (JComponent) source;
                Object oldValue = e.getOldValue();
                Object newValue = e.getNewValue();
                if (name.equals("border")) {
                    if (!ignoreBorderChange && (oldValue == null || oldValue instanceof Border)
                      && (newValue == null || newValue instanceof Border)) {
                        borderChanged(jc, (Border) oldValue, (Border) newValue);
                    }

                } else if (name.equals(AQUA_APPEARANCE_NAME_KEY)) {
                    if ((oldValue == null || oldValue instanceof String) && (newValue == null || newValue instanceof String)) {
                        specifiedAppearanceChanged(jc, (String) oldValue, (String) newValue);
                    }

                } else if (name.equals(AbstractButton.BORDER_PAINTED_CHANGED_PROPERTY)) {
                    if (!ignoreBorderPaintedChange && oldValue instanceof Boolean && newValue instanceof Boolean) {
                        borderPaintedChanged(jc, (Boolean) oldValue, (Boolean) newValue);
                    }
                }
            }
        }
    }

    private static void borderChanged(@NotNull JComponent jc,
                                      @Nullable Border oldBorder,
                                      @Nullable Border newBorder)
    {
        // If the application defines a border to replace a context border, reinstall the context border with the
        // new application border as its delegate.

        if (oldBorder instanceof ContextBorder) {
            ContextBorder contextBorder = (ContextBorder) oldBorder;
            contextBorder.setDelegate(newBorder);
            installBorderQuietly(jc, contextBorder);
        }
    }

    public static void installBorderQuietly(@NotNull JComponent jc, @Nullable Border b)
    {
        boolean old = ignoreBorderChange;
        ignoreBorderChange = true;
        try {
            jc.setBorder(b);
        } finally {
            ignoreBorderChange = old;
        }
    }

    public static void installBorderQuietly(@NotNull BasicSplitPaneDivider d, @Nullable Border b)
    {
        // No notification is possible...
        boolean old = ignoreBorderChange;
        ignoreBorderChange = true;
        try {
            d.setBorder(b);
        } finally {
            ignoreBorderChange = old;
        }
    }

    private static void specifiedAppearanceChanged(@NotNull JComponent jc,
                                                   @Nullable String oldName,
                                                   @Nullable String newName)
    {
        // If the application specifies an appearance, then the specified appearance feature must be enabled.
        if (!isSpecifiedAppearanceFeatureEnabled && newName != null) {
            isSpecifiedAppearanceFeatureEnabled = true;
        }
    }

    private static void borderPaintedChanged(@NotNull JComponent jc, boolean oldValue, boolean newValue)
    {
        // If the component has a context border and the application specifies that a border should not be painted, save
        // that as the value to be restored and force the attribute to be true. The context border must be invoked.

        if (!newValue) {

            ContextBorder cb = AquaBorderSupport.getContextBorder(jc);
            if (cb != null) {
                cb.setOldIsBorderPainted(true);
                boolean old = ignoreBorderPaintedChange;
                ignoreBorderPaintedChange = true;
                try {
                    AquaBorderSupport.setBorderPainted(jc, true);
                } finally {
                    ignoreBorderPaintedChange = old;
                }
            }
        }
    }

    /**
     * Return the current application appearance.
     * @throws UnsupportedOperationException if the current application appearance is not available.
     */
    public static @NotNull AquaAppearance getApplicationAppearance()
      throws UnsupportedOperationException
    {
        String name = AquaUtils.nativeGetApplicationAppearanceName();
        if (name != null) {
            return AquaAppearances.get(name);
        }
        return AquaAppearances.getDefaultAppearance();
    }


    /**
     * Return the appearance that should be used by a component for a painting operation in progress. If the component
     * has a registered appearance, that appearance is returned. Otherwise, an appearance is determined based on the
     * existence of a specified appearance, an appearance obtained from an ancestor, or the application appearance. If
     * appropriate, the vibrant variant of the specified, inherited, or application appearance will be used. The
     * component is updated to register the appearance for future use, if it is not a default appearance. Ancestors may
     * also be updated.
     * @param c The component.
     * @return the appearance to use.
     */

    public static @NotNull AquaAppearance getAppearance(@NotNull Component c) {

        return findAppearanceForComponent(c);
    }

    /**
     * Identify the appearance that should be used by the specified component to paint in the current context. The
     * returned appearance is valid only for a single painting operation. This method does not cache any information.
     */
    public static @NotNull AquaAppearance findAppearanceForComponent(@NotNull Component c)
      throws UnsupportedOperationException
    {
        String name = getSpecifiedAppearanceName(c);
        if (name != null) {
            return AquaAppearances.get(name);
        }
        Container parent = c.getParent();
        if (parent == null) {
            return getApplicationAppearance();
        }
        return findAppearanceForComponent(parent);
    }

    /**
     * Return the appearance name that has been specified for use by a component.
     * @param c The component.
     * @return the specified appearance name, or null if none.
     */

    public static @Nullable String getSpecifiedAppearanceName(@NotNull Component c) {
        return c instanceof JComponent ? getSpecifiedAppearanceName((JComponent) c) : null;
    }

    /**
     * Return the appearance name that has been specified for use by a component.
     * @param jc The component.
     * @return the specified appearance name, or null if none.
     */

    public static @Nullable String getSpecifiedAppearanceName(@NotNull JComponent jc) {
        Object o = jc.getClientProperty(AQUA_APPEARANCE_NAME_KEY);
        if (o instanceof String) {
            return (String) o;
        }
        return null;
    }

    /**
     * Set the name of the appearance to be used by a component.
     * @param jc The component.
     * @param name the appearance name, or null to remove any specified appearance name.
     */

    public static void setSpecifiedAppearanceName(@NotNull JComponent jc, @Nullable String name) {
        jc.putClientProperty(AQUA_APPEARANCE_NAME_KEY, name);
    }

    /* package private */ static void handleActiveStatusChange(@NotNull JComponent jc, boolean isActive)
    {
        AquaComponentUI ui = AquaUtils.getUI(jc, AquaComponentUI.class);
        if (ui != null) {
            ui.activeStateChanged(jc, isActive);
            jc.repaint();
        }
    }

//    private static void systemPropertyChanged()
//    {
//        notifyAllWindows();
//    }
//
//    private static void notifyAllWindows()
//    {
//        Window[] windows = Window.getWindows();
//        for (Window w : windows) {
//            if (w instanceof RootPaneContainer) {
//                RootPaneContainer rpc = (RootPaneContainer) w;
//                JRootPane rp = rpc.getRootPane();
//                notifyTree(rp);
//            }
//        }
//    }
//
//    private static void notifyTree(@NotNull JRootPane rp)
//    {
//        rp.repaint();
//    }

    private static void debug(@NotNull Component c, @NotNull String s)
    {
        Window w = SwingUtilities.getWindowAncestor(c);
        String name = w != null ? w.getName() : "";
        if (name != null && !name.isEmpty()) {
            Utils.logDebug("[" + name + "] " + s);
        } else {
            Utils.logDebug(s);
        }
    }

    /**
     * This method for use by painters not associated with a component.
     */
    public static @NotNull PaintingContext getCurrentPaintingContext()
    {
        if (currentContext != null) {
            return currentContext;
        }
        AquaAppearance appearance = getApplicationAppearance();
        return PaintingContext.of(appearance);
    }


    /**
     * This method must be used to perform the paintComponent operation for a VAqua component.
     */
    public static void withContext(@NotNull Graphics g, @NotNull JComponent jc, @NotNull ComponentPainter painter)
    {
        AquaAppearance specifiedAppearance = getSpecifiedAppearance(jc);

        ContextBorder cb = AquaBorderSupport.getContextBorder(jc);
        if (specifiedAppearance != null && cb == null) {
            Utils.logError("Specified appearance is not supported on component " + jc.getClass());
        }

        AquaAppearance push = null;

        if (currentAppearance != null) {
            // If a current appearance is defined, then the only possible change is substitution of a different
            // specified appearance.
            if (specifiedAppearance != null && specifiedAppearance != currentAppearance) {
                push = specifiedAppearance;
            }
        } else {
            // If no current appearance is defined, the appropriate appearance must be identified.
            push = findAppearanceForComponent(jc);
        }

        if (push == null) {
            // Use the current context, which remains valid.
            assert currentContext != null;
            painter.paint((Graphics2D) g, jc, currentContext);
        } else {
            // Push a new context onto the "stack". If the component does not have a context border, then there is
            // no place to stack the old context, so just use it without pushing.
            PaintingContext pc = PaintingContext.of(push);
            if (cb != null) {
                cb.setOldPaintingContext(currentContext);
                currentContext = pc;
            } else {
                Utils.logError("Component " + jc.getClass() + " requested a painting context but does not have a context border");
            }
            painter.paint((Graphics2D) g, jc, pc);
        }
    }

    private static @Nullable AquaAppearance getSpecifiedAppearance(@NotNull Component c)
    {
        if (c instanceof JComponent) {
            JComponent jc = (JComponent) c;
            String appearanceName = getSpecifiedAppearanceName(jc);
            if (appearanceName == null) {
                return null;
            }
            return AquaAppearances.get(appearanceName);
        }
        return null;
    }

    public interface ComponentPainter
    {
        void paint(@NotNull Graphics2D g, @NotNull JComponent jc, @NotNull PaintingContext pc);
    }
}
