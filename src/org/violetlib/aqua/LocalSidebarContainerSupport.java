/*
 * Copyright (c) 2025 Alan Snyder.
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
 * Support for a container (list or tree) that can either directly use a sidebar container style or can cause a scroll
 * pane ancestor to use a sidebar container style.
 */
public class LocalSidebarContainerSupport
{
    private final @NotNull JComponent c;
    private final @NotNull AquaViewStyleContainerUI ui;
    private @Nullable SidebarContainerSupport directSupport;
    private @Nullable JScrollPane configuredScrollPaneAncestor;

    public LocalSidebarContainerSupport(@NotNull JComponent c, @NotNull AquaViewStyleContainerUI ui)
    {
        this.c = c;
        this.ui = ui;
    }

    public void detach()
    {
        updateConfiguredScrollPaneAncestor(null);
        if (directSupport != null) {
            directSupport.uninstall();
            directSupport = null;
        }
    }

    /**
     * Return a new graphics context to use when painting the container.
     *
     * @param o The original graphics context given to the UI.
     * @return a graphics context to use instead of {@code o} to paint the container.
     */
    public @NotNull Graphics2D setupContainerGraphics(@NotNull Graphics o, @NotNull AppearanceContext ac)
    {
        if (directSupport != null) {
            // There is no scroll pane, so the developer may need to write code to clip when painting children. For
            // normal uses of JList and JTree, this code is not needed either because there are no child components or
            // the child component (a cell editor) is indented and does not conflict with the clipping shape.

            // The code that would be needed:

//            @Override
//            protected void paintChildren(Graphics g)
//            {
//                Shape clip = (Shape) getClientProperty("Aqua.clip");
//                if (clip != null) {
//                    Graphics2D gg = (Graphics2D) g;
//                    gg.clip(clip);
//                }
//                super.paintChildren(g);
//            }

//            @Override
//            protected boolean isPaintingOrigin()
//            {
//                return getClientProperty("Aqua.clip") != null;
//            }

            Graphics2D gg = directSupport.setupContainerGraphics(o, ac);
            Shape clip = null;
            Insetter2D insetter = directSupport.getInsetter();
            if (insetter != null) {
                clip = insetter.getInteriorShape(c.getWidth(), c.getHeight());
            }
            c.putClientProperty("Aqua.clip", clip);
            return gg;
        } else {
            return (Graphics2D) o.create();
        }
    }

    // Should be called on a change to the style or a possibly relevant change to ancestry of the list
    public void update()
    {
        JScrollPane sp = ui.isSideBar() ? getPossibleScrollPaneSidebarContainer() : null;
        updateConfiguredScrollPaneAncestor(sp);
        updateLocalSideBarConfiguration();
    }

    private @Nullable JScrollPane getPossibleScrollPaneSidebarContainer()
    {
        JScrollPane sp = AquaUtils.getScrollPaneAncestor(c);
        if (sp != null) {
            AquaScrollPaneUI ui = AquaUtils.getUI(sp, AquaScrollPaneUI.class);
            if (ui != null) {
                return sp;
            }
        }
        return null;
    }

    private void updateConfiguredScrollPaneAncestor(@Nullable JScrollPane sp)
    {
        if (configuredScrollPaneAncestor != sp) {
            if (configuredScrollPaneAncestor != null) {
                reconfigureScrollPane(configuredScrollPaneAncestor, false);
            }
            configuredScrollPaneAncestor = sp;
            if (sp != null) {
                reconfigureScrollPane(configuredScrollPaneAncestor, true);
            }
        }
    }

    private void reconfigureScrollPane(@NotNull JScrollPane sp, boolean shouldConfigure)
    {
        AquaScrollPaneUI spui = AquaUtils.getUI(sp, AquaScrollPaneUI.class);
        assert spui != null;
        spui.configureAsSidebarContainer(shouldConfigure ? ui : null);
    }

    protected void updateLocalSideBarConfiguration()
    {
        updateDirectSupport();
        ui.configureSidebarStyle();
    }

    private void updateDirectSupport()
    {
        if (directSupport != null) {
            directSupport.uninstall();
            directSupport = null;
        }
        if (ui.isSideBar() && AquaPainting.useLiquidGlassSidebar() && configuredScrollPaneAncestor == null) {
            directSupport = new SidebarContainerSupport(c);
            if (!c.isOpaque()) {
                c.setOpaque(false);
            }
        }
        c.revalidate();
        c.repaint();
    }
}
