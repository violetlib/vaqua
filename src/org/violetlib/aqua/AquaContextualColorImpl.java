/*
 * Copyright (c) 2018-2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

package org.violetlib.aqua;

import java.awt.*;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.violetlib.jnr.aqua.AquaUIPainter;

import static org.violetlib.aqua.EffectName.*;
import static org.violetlib.jnr.aqua.AquaUIPainter.State.*;

/**
 * A contextual color defined using appearance color names.
 */

public class AquaContextualColorImpl implements ContextualColor {

    protected final @NotNull String colorName;

    protected @NotNull String basicName;
    protected @Nullable String rolloverName;
    protected @Nullable String pressedName;
    protected @Nullable String iconPressedName;
    protected @Nullable String inactiveName;
    protected @Nullable String disabledName;
    protected @Nullable String inactiveDisabledName;
    protected @Nullable String activeDefaultName;   // used for default buttons and focused container cells

    protected @Nullable String selectedName;
    protected @Nullable String pressedSelectedName;
    protected @Nullable String inactiveSelectedName;
    protected @Nullable String disabledSelectedName;
    protected @Nullable String inactiveDisabledSelectedName;
    protected @Nullable String activeDefaultSelectedName;

    // Most components use the same color when inactive or disabled.
    // In this case, either the disabled or inactive color name may be set.

    public AquaContextualColorImpl(@NotNull String colorName, @NotNull String basicName) {
        this.colorName = colorName;
        this.basicName = basicName;
    }

    @Override
    public @NotNull String getColorName() {
        return colorName;
    }

    public @NotNull String getBasicName() {
        return basicName;
    }

    public void setBasicName(@NotNull String basicName) {
        this.basicName = basicName;
    }

    public void setAllNames() {
        setAllNames(basicName);
    }

    public void setAllNames(@NotNull String name) {
        this.basicName = name;
        this.rolloverName = name + "_rollover";
        this.pressedName = name + "_pressed";
        this.inactiveName = name + "_inactive";
        this.disabledName = name + "_disabled";
        this.inactiveDisabledName = name + "_inactive_disabled";
        this.activeDefaultName = name + "_focused";

        String selectedName = AquaColors.createSelectedColorName(basicName);
        setAllSelectedNames(selectedName);
    }

    public void setAllSelectedNames(@NotNull String selectedName) {
        this.selectedName = selectedName;
        this.pressedSelectedName = selectedName + "_pressed";
        this.inactiveSelectedName = selectedName + "_inactive";
        this.disabledSelectedName = selectedName + "_disabled";
        this.inactiveDisabledSelectedName = selectedName + "_inactive_disabled";
        this.activeDefaultSelectedName = selectedName + "_focused";
    }

    public @Nullable String getRolloverName() {
        return rolloverName;
    }

    public void setRolloverName(@Nullable String rolloverName) {
        this.rolloverName = rolloverName;
    }

    public @Nullable String getIconPressedName() {
        return iconPressedName;
    }

    public void setIconPressedName(@Nullable String iconPressedName) {
        this.iconPressedName = iconPressedName;
    }

    public @Nullable String getPressedSelectedName() {
        return pressedSelectedName;
    }

    public void setPressedSelectedName(@Nullable String pressedSelectedName) {
        this.pressedSelectedName = pressedSelectedName;
    }

    public @Nullable String getPressedName() {
        return pressedName;
    }

    public void setPressedName(@Nullable String pressedName) {
        this.pressedName = pressedName;
    }

    public @Nullable String getInactiveDisabledSelectedName() {
        return inactiveDisabledSelectedName;
    }

    public void setInactiveDisabledSelectedName(@Nullable String inactiveDisabledSelectedName) {
        this.inactiveDisabledSelectedName = inactiveDisabledSelectedName;
    }

    public @Nullable String getInactiveDisabledName() {
        return inactiveDisabledName;
    }

    public void setInactiveDisabledName(@Nullable String inactiveDisabledName) {
        this.inactiveDisabledName = inactiveDisabledName;
    }

    public @Nullable String getDisabledSelectedName() {
        return disabledSelectedName;
    }

    public void setDisabledSelectedName(@Nullable String disabledSelectedName) {
        this.disabledSelectedName = disabledSelectedName;
    }

    public @Nullable String getDisabledName() {
        return disabledName;
    }

    public void setDisabledName(@Nullable String disabledName) {
        this.disabledName = disabledName;
    }

    public @Nullable String getInactiveSelectedName() {
        return inactiveSelectedName;
    }

    public void setInactiveSelectedName(@Nullable String inactiveSelectedName) {
        this.inactiveSelectedName = inactiveSelectedName;
    }

    public @Nullable String getInactiveName() {
        return inactiveName;
    }

    public void setInactiveName(@Nullable String inactiveName) {
        this.inactiveName = inactiveName;
    }

    public @Nullable String getSelectedName() {
        return selectedName;
    }

    public void setSelectedName(@Nullable String selectedName) {
        this.selectedName = selectedName;
    }

    public @Nullable String getActiveDefaultName() {
        return activeDefaultName;
    }

    public void setActiveDefaultName(@Nullable String activeDefaultName) {
        this.activeDefaultName = activeDefaultName;
    }

    public @Nullable String getActiveDefaultSelectedName() {
        return activeDefaultSelectedName;
    }

    public void setActiveDefaultSelectedName(@Nullable String activeDefaultSelectedName) {
        this.activeDefaultSelectedName = activeDefaultSelectedName;
    }

    @Override
    public @NotNull Color get(@NotNull AppearanceContext context) {

        AquaAppearance appearance = context.getAppearance();
        AquaUIPainter.State state = context.getState();
        boolean isSelected = context.isSelected();
        boolean isIcon = context.isIcon();

        if (AquaColors.isDebugging()) {
            String colorName = getColorName();
            String message = colorName + ": lookup color for " + appearance.getName() + " " + state;
            if (isSelected) {
                message = message + " SELECTED";
            }
            if (isIcon) {
                message = message + " ICON";
            }
            AquaUtils.logDebug(message);
        }

        if (state == ROLLOVER) {
            if (rolloverName != null) {
                Color c = appearance.getColor(rolloverName);
                if (c != null) {
                    return c;
                }
            }
            {
                Color c = appearance.getColorForEffect(basicName, EFFECT_ROLLOVER);
                if (c != null) {
                    return c;
                }
            }
        }

        if (state == PRESSED) {
            if (isIcon && iconPressedName != null) {
                Color c = appearance.getColor(iconPressedName);
                if (c != null) {
                    return c;
                }
            }
            if (isSelected && pressedSelectedName != null) {
                Color c = appearance.getColor(pressedSelectedName);
                if (c != null) {
                    return c;
                }
            }
            if (pressedName != null) {
                Color c = appearance.getColor(pressedName);
                if (c != null) {
                    return c;
                }
            }
            {
                Color c = appearance.getColorForEffect(basicName, EFFECT_PRESSED);
                if (c != null) {
                    return c;
                }
            }
        }

        if (state == DISABLED_INACTIVE) {
            if (isSelected && inactiveDisabledSelectedName != null) {
                Color c = appearance.getColor(inactiveDisabledSelectedName);
                if (c != null) {
                    return c;
                }
            }
            if (inactiveDisabledName != null) {
                Color c = appearance.getColor(inactiveDisabledName);
                if (c != null) {
                    return c;
                }
            }

            {
                String name = basicName + "_inactive_disabled";
                Color c = appearance.getColor(name);
                if (c != null) {
                    return c;
                }
            }

            // Most components look the same when inactive or disabled
            state = DISABLED;
        }

        if (state == DISABLED) {
            if (isSelected) {
                if (disabledSelectedName != null) {
                    Color c = appearance.getColor(disabledSelectedName);
                    if (c != null) {
                        return c;
                    }
                }
                if (selectedName != null) {
                    Color c = appearance.getColorForEffect(selectedName, EFFECT_DISABLED);
                    if (c != null) {
                        return c;
                    }
                }
            }

            if (disabledName != null) {
                Color c = appearance.getColor(disabledName);
                if (c != null) {
                    return c;
                }
            }
            {
                Color c = appearance.getColorForEffect(basicName, EFFECT_DISABLED);
                if (c != null) {
                    return c;
                }
            }

            // Most components look the same when inactive or disabled
            state = INACTIVE;
        }

        if (state == INACTIVE) {
            if (isSelected) {
                if (inactiveSelectedName != null) {
                    Color c = appearance.getColor(inactiveSelectedName);
                    if (c != null) {
                        return c;
                    }
                }
                if (selectedName != null) {
                    Color c = appearance.getColorForEffect(selectedName, EFFECT_DISABLED);
                    if (c != null) {
                        return c;
                    }
                }
            }

            if (inactiveName != null) {
                Color c = appearance.getColor(inactiveName);
                if (c != null) {
                    return c;
                }
            }
            {
                Color c = appearance.getColorForEffect(basicName, EFFECT_DISABLED);
                if (c != null) {
                    return c;
                }
            }
        }

        if (state == ACTIVE_DEFAULT) {
            if (isSelected && activeDefaultSelectedName != null) {
                Color c = appearance.getColor(activeDefaultSelectedName);
                if (c != null) {
                    return c;
                }
            }

            if (activeDefaultName != null) {
                Color c = appearance.getColor(activeDefaultName);
                 if (c != null) {
                     return c;
                 }
            }
        }

        if (isSelected && selectedName != null) {
            Color c = appearance.getColor(selectedName);
            if (c != null) {
                return c;
            }
        }

        {
            Color c = appearance.getColor(basicName);
            if (c != null) {
                return c;
            }
        }

        throw new UnsupportedOperationException("No color defined for: " + basicName);
    }
}
