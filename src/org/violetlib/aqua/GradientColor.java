/*
 * Copyright (c) 2018-2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.plaf.ColorUIResource;

import org.jetbrains.annotations.NotNull;

/**
 *
 */
public class GradientColor extends ColorUIResource {
    private final @NotNull Color finish;
    private final boolean useMagicEraser;
    private final @NotNull Logger log;

    public GradientColor(@NotNull Color start, @NotNull Color finish, @NotNull Logger log) {
        super(start);
        this.finish = finish;
        this.useMagicEraser = false;
        this.log = log;
    }

    public GradientColor(@NotNull Color start, @NotNull Color finish, boolean useMagicEraser, @NotNull Logger log) {
        super(start);
        this.finish = finish;
        this.useMagicEraser = useMagicEraser;
        this.log = log;

        if (useMagicEraser && (start.getAlpha() == 255 && finish.getAlpha() == 255)) {
            log.log("Magic eraser not needed with opaque gradient");
        }
    }

    public @NotNull Color getStart() {
        return this;
    }

    public @NotNull Color getFinish() {
        return finish;
    }

    public boolean useMagicEraser() {
        return useMagicEraser;
    }
}
