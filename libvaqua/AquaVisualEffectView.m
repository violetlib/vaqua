/*
 * Copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

#import "AquaVisualEffectView.h"
#import "VAppearances.h"

#pragma weak VAppearances_updateAppearance=updateAppearanceMissing

static void updateAppearanceMissing(NSAppearance *appearance)
{
    NSLog(@"VAppearances_updateAppearance is undefined");
}

/*
 * A visual effect view that remembers the style specified by Java so that it can reconfigure itself when the
 * window appearance changes.
 *
 */

@implementation AquaVisualEffectView
@synthesize style = _style;

static NSAppearance *getVibrantAppearance(NSAppearance *context, UInt16 style)
{
    NSString *name = nil;

    switch (style) {

    case LIGHT_STYLE:
    case MEDIUM_LIGHT_STYLE:
        name = NSAppearanceNameVibrantLight;
        break;

    case DARK_STYLE:
    case ULTRA_DARK_STYLE:
        name = NSAppearanceNameVibrantDark;
        break;
    }

    if (name == nil) {
        NSAppearanceName contextName = context.name;
        if ([contextName containsString: @"Dark"]) {
            name = NSAppearanceNameVibrantDark;
        } else {
            name = NSAppearanceNameVibrantLight;
        }
    }

    // debug
    // NSLog(@"Vibrant appearance: %@", name);

    return [NSAppearance appearanceNamed: name];
}

static NSVisualEffectMaterial getVibrantMaterial(UInt16 style)
{
    BOOL isYosemite = floor(NSAppKitVersionNumber) <= NSAppKitVersionNumber10_10_Max;

    switch (style) {

    default:
    case LIGHT_STYLE:
        return NSVisualEffectMaterialLight;

    case SHEET_STYLE:
        if (@available(macOS 10.14, *)) {
            return NSVisualEffectMaterialSheet;
        }
        return NSVisualEffectMaterialLight;

    case DARK_STYLE:
        return NSVisualEffectMaterialDark;

    case SIDEBAR_STYLE:
        return isYosemite ? NSVisualEffectMaterialLight : NSVisualEffectMaterialSidebar;

    case TITLE_BAR_STYLE:
        return NSVisualEffectMaterialTitlebar;

    case MENU_STYLE:
        return NSVisualEffectMaterialMenu;

    case POPOVER_STYLE:
        return isYosemite ? NSVisualEffectMaterialLight : NSVisualEffectMaterialPopover;

    case MEDIUM_LIGHT_STYLE:
        return isYosemite ? NSVisualEffectMaterialLight : NSVisualEffectMaterialMediumLight;

    case ULTRA_DARK_STYLE:
        return isYosemite ? NSVisualEffectMaterialDark : NSVisualEffectMaterialUltraDark;

    case SELECTION_STYLE:
        if (@available(macOS 10.14, *)) {
            return NSVisualEffectMaterialSelection;
        }
        break;

    case HEADER_STYLE:
        if (@available(macOS 10.14, *)) {
            return NSVisualEffectMaterialHeaderView;
        }
        break;

    case WINDOW_BACKGROUND_STYLE:
        if (@available(macOS 10.14, *)) {
            return NSVisualEffectMaterialWindowBackground;
        }
        break;

    case HUD_WINDOW_STYLE:
        if (@available(macOS 10.14, *)) {
            return NSVisualEffectMaterialHUDWindow;
        }
        break;

    case FULL_SCREEN_MODAL_STYLE:
        if (@available(macOS 10.14, *)) {
            return NSVisualEffectMaterialFullScreenUI;
        }
        break;

    case TOOL_TIP_STYLE:
        if (@available(macOS 10.14, *)) {
            return NSVisualEffectMaterialToolTip;
        }
        break;

    case CONTENT_BACKGROUND_STYLE:
        if (@available(macOS 10.14, *)) {
            return NSVisualEffectMaterialContentBackground;
        }
        break;

    case UNDER_WINDOW_BACKGROUND_STYLE:
        if (@available(macOS 10.14, *)) {
            return NSVisualEffectMaterialUnderWindowBackground;
        }
        break;

    case UNDER_PAGE_BACKGROUND_STYLE:
        if (@available(macOS 10.14, *)) {
            return NSVisualEffectMaterialUnderPageBackground;
        }
        break;
    }

    return NSVisualEffectMaterialLight;
}

- (void) configureWithAppearance: (NSAppearance *) appearance
{
    self.appearance = getVibrantAppearance(appearance, self.style);
    self.material = getVibrantMaterial(self.style);
}

- (void) viewDidChangeEffectiveAppearance
{
    [super viewDidChangeEffectiveAppearance];
    [self setNeedsDisplay:YES];
    VAppearances_updateAppearance(self.effectiveAppearance);
}

@end
