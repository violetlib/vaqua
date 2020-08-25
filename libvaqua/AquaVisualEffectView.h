/*
 * Copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

#ifndef __VISUAL_EFFECT_VIEW__
#define __VISUAL_EFFECT_VIEW__

#import <Cocoa/Cocoa.h>

/*
 * A visual effect view that remembers the style specified by Java so that it can reconfigure itself when the
 * window appearance changes.
 *
 */

@interface AquaVisualEffectView : NSVisualEffectView
@property UInt16 style;
- (void) configureWithAppearance: (NSAppearance *) appearance;
@end

// Java constants for vibrant styles

#define LIGHT_STYLE 0
#define DARK_STYLE 1
#define SIDEBAR_STYLE 2
#define TITLE_BAR_STYLE 3
#define MENU_STYLE 4
#define POPOVER_STYLE 5
#define MEDIUM_LIGHT_STYLE 6
#define ULTRA_DARK_STYLE 7
#define SHEET_STYLE 8
#define SELECTION_STYLE 9
#define HEADER_STYLE 10
#define WINDOW_BACKGROUND_STYLE 11
#define HUD_WINDOW_STYLE 12
#define FULL_SCREEN_MODAL_STYLE 13
#define TOOL_TIP_STYLE 14
#define CONTENT_BACKGROUND_STYLE 15
#define UNDER_WINDOW_BACKGROUND_STYLE 16
#define UNDER_PAGE_BACKGROUND_STYLE 17

#endif /* __VISUAL_EFFECT_VIEW__ */
