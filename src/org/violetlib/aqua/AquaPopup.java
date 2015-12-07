/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;
import javax.swing.*;

/**
 * A heavy weight popup. Used only when unable to configure the standard popup factory.
 */
public class AquaPopup extends Popup {

    public AquaPopup(Component owner, Component contents, int x, int y) {
        super(owner, contents, x, y);
    }
}
