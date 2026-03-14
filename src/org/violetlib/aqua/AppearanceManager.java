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
import java.util.function.Consumer;
import javax.swing.*;

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

    // There are two kinds of painting contexts.
    //
    // A JAppearancePanel (defined by VAquaClient) allows an application to designate the appearance to be used when
    // painting the panel and its children. It installs a painting context at the beginning of a paint operation and
    // uninstalls it at the end of the paint operation. A JAppearancePanel is always repainted whenever any subcomponent
    // needs to be repainted.

    // When there is no active appearance panel context, the painting context is one that uses the appearance of the
    // window whose components are being painted.

    // In actuality, only VAqua components (components with a VAqua UI) know about the painting context, and the root of
    // the tree being painted may not be a VAqua component. A more accurate description of the process is that any
    // VAqua component that needs the painting context when none must install the window context. The window context
    // remains valid until the window appearance is changed.

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

    private static boolean isSpecifiedAppearanceFeatureEnabled;

    private static final @NotNull PropertyChangeListener myPropertyChangeListener = AppearanceManager::propertyChanged;

    private static boolean isEnabled;

    /* package private */ static void initialize()
    {
        isEnabled = true;
    }

    /* package private */ static void uninitialize()
    {
        isEnabled = false;
    }

    /**
     * Install support for a component whose UI is potentially appearance-sensitive.
     */
    public static void install(@NotNull JComponent c)
    {
        String specifiedAppearance = getSpecifiedAppearanceName(c);
        if (specifiedAppearance != null) {
            isSpecifiedAppearanceFeatureEnabled = true;
        }
        c.addPropertyChangeListener(myPropertyChangeListener);
    }

    /**
     * Uninstall support for a component whose UI is potentially appearance-sensitive.
     */
    public static void uninstall(@NotNull JComponent c)
    {
        c.removePropertyChangeListener(myPropertyChangeListener);
    }

    private static void propertyChanged(@NotNull PropertyChangeEvent e)
    {
        if (isEnabled) {
            String name = e.getPropertyName();
            if (name != null) {
                Object source = e.getSource();
                if (source instanceof JComponent) {
                    JComponent jc = (JComponent) source;
                    Object oldValue = e.getOldValue();
                    Object newValue = e.getNewValue();
                    if (name.equals(AQUA_APPEARANCE_NAME_KEY)) {
                        if ((oldValue == null || oldValue instanceof String) && (newValue == null || newValue instanceof String)) {
                            specifiedAppearanceChanged(jc, (String) oldValue, (String) newValue);
                        }
                    }
                }
            }
        }
    }

    private static void specifiedAppearanceChanged(@NotNull JComponent jc,
                                                   @Nullable String oldName,
                                                   @Nullable String newName)
    {
        if (isEnabled) {
            AquaAppearancePanelUI ui = AquaUtils.getUI(jc, AquaAppearancePanelUI.class);
            AquaRootPaneUI rui = AquaUtils.getUI(jc, AquaRootPaneUI.class);

            if (ui == null && rui == null) {
                Utils.logError("Custom component appearances are supported only for JRootPane and JAquaAppearancePanel (provided by VAquaClient)");
            } else {
                // If the application specifies an appearance, then the specified appearance feature must be enabled.
                if (!isSpecifiedAppearanceFeatureEnabled && newName != null) {
                    isSpecifiedAppearanceFeatureEnabled = true;
                }
                if (rui != null) {
                    rui.specifiedAppearanceChanged();
                }
                jc.repaint();
            }
        }
    }

    /**
     * Identify the appearance that should be used by the specified component to paint in the current context. The
     * returned appearance is valid only for a single painting operation.
     */
    public static @NotNull AquaAppearance findAppearanceForComponent(@NotNull Component c)
      throws UnsupportedOperationException
    {
        if (isEnabled) {
            String name = getSpecifiedAppearanceName(c);
            if (name != null) {
                return AquaAppearances.get(name);
            }
            Container parent = c.getParent();
            if (parent == null) {
                return AquaAppearances.getApplicationEffectiveAppearance();
            }
            return findAppearanceForComponent(parent);
        }
        return AquaAppearances.getDefaultAppearance();
    }

    public static @Nullable AquaAppearance getSpecifiedWindowAppearance(@NotNull JRootPane rp)
    {
        Object o = rp.getClientProperty(AQUA_APPEARANCE_NAME_KEY);
        if (o instanceof String) {
            String appearanceName = (String) o;
            AquaAppearance a = AquaAppearances.getOptional(appearanceName);
            if (a != null) {
                return a;
            }
        }
        return null;
    }

    /**
     * Return the appearance name that has been specified for use by a component.
     * @param c The component.
     * @return the specified appearance name, or null if none.
     */
    public static @Nullable String getSpecifiedAppearanceName(@NotNull Component c)
    {
        return c instanceof JComponent ? getSpecifiedAppearanceName((JComponent) c) : null;
    }

    /**
     * Return the appearance name that has been specified for use by a component.
     * @param jc The component.
     * @return the specified appearance name, or null if none.
     */
    public static @Nullable String getSpecifiedAppearanceName(@NotNull JComponent jc)
    {
        Object o = jc.getClientProperty(AQUA_APPEARANCE_NAME_KEY);
        if (o instanceof String) {
            AquaAppearancePanelUI ui = AquaUtils.getUI(jc, AquaAppearancePanelUI.class);
            if (ui != null) {
                return (String) o;
            }
        }
        return null;
    }

    /* package private */ static void handleActiveStatusChange(@NotNull JComponent jc, boolean isActive)
    {
        if (isEnabled) {
            ActiveSensitiveComponentUI ui = AquaUtils.getUI(jc, ActiveSensitiveComponentUI.class);
            if (ui != null) {
                ui.activeStateChanged(jc, isActive);
                jc.repaint();
            }
        }
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

    /**
     * This method for use by painters not associated with a VAqua component.
     */
    public static @NotNull PaintingContext getPaintingContext(@NotNull Component c)
    {
        PaintingContext pc = PaintingContext.get();
        if (pc != null) {
            return pc;
        }
        return getWindowPaintingContext(c);
    }

    public static @NotNull PaintingContext getWindowPaintingContext(@NotNull Component c)
      throws UnsupportedOperationException
    {
        if (isEnabled) {
            if (isSpecifiedAppearanceFeatureEnabled) {
                Window w = SwingUtilities.getWindowAncestor(c);
                if (w != null) {
                    JRootPane rp = AquaUtils.getRootPane(w);
                    if (rp != null) {
                        AquaRootPaneUI ui = AquaUtils.getUI(rp, AquaRootPaneUI.class);
                        if (ui != null) {
                            return ui.getPaintingContext();
                        }
                    }
                }
            }
            AquaAppearance appearance = AquaAppearances.getApplicationEffectiveAppearance();
            return PaintingContext.of(appearance);
        }
        return PaintingContext.of(AquaAppearances.getDefaultAppearance());
    }

    /* package private */ static void paintAppearancePanel(@NotNull JComponent owner,
                                                           @NotNull String appearanceName,
                                                           @NotNull Consumer<Color> r)
    {
        if (isEnabled) {
            PaintingContext pc = getPaintingContext(owner);
            if (!appearanceName.equals(pc.appearance.getName())) {
                AquaAppearance appearance = AquaAppearances.getOptional(appearanceName);
                if (appearance != null) {
                    Color bc = AquaUtils.getWindowBackground(owner.getRootPane(), appearance);
                    PaintingContext.push(owner, appearance);
                    try {
                        r.accept(bc);
                    } finally {
                        PaintingContext.pop(owner);
                    }
                    return;
                }
            }
        }
        r.accept(null);
    }

    /**
     * This method must be used to perform the paintComponent operation for a VAqua component.
     * It provides the appropriate painting context.
     */
    public static void withContext(@NotNull Graphics g, @NotNull JComponent jc, @NotNull ComponentPainter painter)
    {
        PaintingContext pc = PaintingContext.get();
        if (pc == null) {
            pc = getWindowPaintingContext(jc);
        }
        painter.paint((Graphics2D) g, jc, pc);
    }

    public interface ComponentPainter
    {
        void paint(@NotNull Graphics2D g, @NotNull JComponent jc, @NotNull PaintingContext pc);
    }
}
