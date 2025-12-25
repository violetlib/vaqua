/*
 * Copyright (c) 2023-2025 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.WeakHashMap;
import javax.swing.*;

import org.jetbrains.annotations.*;

/**
 * Manage components whose UIs want to be notified and responders that want to be invoked when certain system properties
 * change. This design avoids holding a strong reference to the component or responder.
 */
public class SystemPropertyChangeManager {

    public interface SystemPropertyChangeListener {
        void systemPropertyChanged(JComponent c, Object type);
    }

    private static final WeakHashMap<JComponent,JComponent> components = new WeakHashMap<>();
    private static final @NotNull WeakHashMap<Runnable,Runnable> responders = new WeakHashMap<>();

    /**
     * Register a component whose UI is to be notified. The component is weakly held. The component may be
     * removed if it is discovered that its UI is not a SystemPropertyChangeListener.
     */
    public static void register(@NotNull JComponent c)
    {
        components.put(c, null);
    }

    public static void unregister(@NotNull JComponent c)
    {
        components.remove(c);
    }

    /**
     * Register a responder to be notified. The responder is weakly held.
     */
    public static void register(@NotNull Runnable r)
    {
        responders.put(r, null);
    }

    public static void unregister(@NotNull Runnable r)
    {
        responders.remove(r);
    }

    public static void notifyChange(@Nullable Object type)
    {
        Collection<JComponent> cs = components.keySet();
        if (!cs.isEmpty()) {
            List<JComponent> componentList = new ArrayList<>(cs);
            for (JComponent jc : componentList) {
                SystemPropertyChangeListener l = AquaUtils.getUI(jc, SystemPropertyChangeListener.class);
                if (l != null) {
                    l.systemPropertyChanged(jc, type);
                } else {
                    components.remove(jc);
                }
            }
        }

        Collection<Runnable> rs = responders.keySet();
        if (!rs.isEmpty()) {
            List<Runnable> runnableList = new ArrayList<>(rs);
            for (Runnable r : runnableList) {
                r.run();
            }
        }
    }
}
