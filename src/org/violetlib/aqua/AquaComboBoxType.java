/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

/**
 * This enum distinguishes different types of controls that are represented in Swing using JComboBox.
 */
public enum AquaComboBoxType {
    EDITABLE_COMBO_BOX,     // the standard combo box, with an editable text field
    POP_UP_MENU_BUTTON,     // a button that brings up a pop up menu (custom for Aqua)
    PULL_DOWN_MENU_BUTTON   // a button that brings up a pull down menu (custom for Aqua)
}
