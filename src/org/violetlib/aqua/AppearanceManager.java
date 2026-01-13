/*
 * Copyright (c) 2018-2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;

import org.jetbrains.annotations.*;

/**
 * This class supports the association of appearances with components.
 */

public class AppearanceManager {

    // This class has been converted to use a simple, no-cache implementation. A component UI that is potentially
    // dependent on the appearance and appearance settings is required to obtain an appearance and the system colors
    // that it needs each time it is painted. Doing so ensures that the UI will always have the correct colors for its
    // appearance and the current appearance settings.

    // The new implementation is much simpler than the previous caching implementation. Unlike the old implementation,
    // the new implementation easily handles changes to the system appearance and to appearance settings. The old
    // implementation required event listeners and complicated logic to ensure that cached appearance data was properly
    // updated after a change to the system appearance or to appearance settings. (It was not completely correct,
    // either.)

    // The API retains some vestiges of the caching implementation, with the goal of facilitating the possible future
    // addition of caching, should that be advisable. Component UI classes continue to invoke methods that attach event
    // listeners (which do nothing). Component UI classes continue to support notification of appearance changes, even
    // though the notification method is called only when the UI calls registerCurrentAppearance.

    // The new implementation supports a feature that otherwise would be impossible to fully support: the ability for an
    // application to specify an appearance for a component with no UI (an action that is not immediately detectable by
    // a look and feel). Nevertheless, it is recommended that applications use the specified appearance feature only on
    // JRootPane and JPanel components.

    // The elimination of caching does not appear to have a noticeable performance penalty. Although it requires
    // additional traversal of the hierarchy to the root pane (just like getWindowAncestor), it also eliminates a lot
    // of event handling.

    public static boolean isDebug = false;

    public static final String AQUA_APPEARANCE_NAME_KEY = "Aqua.appearanceName";    // the name of an explicitly chosen appearance

    private static boolean isResponderRegistered;

    /* package private */ static void initialize()
    {
        if (!isResponderRegistered) {
            isResponderRegistered = true;
            SystemPropertyChangeManager.register(AppearanceManager::systemPropertyChanged);
        }
    }

    public static void installListeners(@NotNull JComponent c) {
    }

    public static void uninstallListeners(@NotNull JComponent c) {
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
     * Ensure that a component has the correct appearance for a painting operation in progress.
     * The appearance may be based on the existence of a specified appearance, the appearance obtained from an ancestor,
     * or the application appearance.
     * The component UI may be notified of an appearance change, even if the component appearance has not changed.
     * <p>
     * A component UI should call this method before performing a painting operation to ensure that the correct
     * appearance is used even if the component hierarchy has been modified since the UI last configured itself.
     * Updating the configuration may be essential if the component is being used as a cell renderer.
     *
     * @param c The component.
     * @return the appearance for the component.
     */

    public static @NotNull AquaAppearance registerCurrentAppearance(@NotNull JComponent c) {

        AquaAppearance appearance = findAppearanceForComponent(c);
        AquaComponentUI ui = AquaUtils.getUI(c, AquaComponentUI.class);
        if (ui != null) {
            ui.appearanceChanged(c, appearance);
        }
        return appearance;
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

    private static void systemPropertyChanged()
    {
        notifyAllWindows();
    }

    private static void notifyAllWindows()
    {
        Window[] windows = Window.getWindows();
        for (Window w : windows) {
            if (w instanceof RootPaneContainer) {
                RootPaneContainer rpc = (RootPaneContainer) w;
                JRootPane rp = rpc.getRootPane();
                notifyTree(rp);
            }
        }
    }

    private static void notifyTree(@NotNull JRootPane rp)
    {
        rp.repaint();
    }

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
}
