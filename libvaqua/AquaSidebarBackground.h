/*
 * Copyright (c) 2015-2021 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

#import <Cocoa/Cocoa.h>
#import "AquaVisualEffectView.h"

@interface AquaSidebarBackground : AquaVisualEffectView
- (AquaSidebarBackground *) initWithFrame: (NSRect) frameRect style: (UInt16) style forceActive: (BOOL) forceActive;
- (void) updateSelectionViews: (int*) data;
@end
