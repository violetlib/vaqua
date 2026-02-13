/*
 * Copyright (c) 2015-2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

#import <Cocoa/Cocoa.h>
#import "AquaVisualEffectView.h"
#import <jni.h>

@interface AquaSidebarBackground : AquaVisualEffectView
- (AquaSidebarBackground *) initWithFrame: (NSRect) frameRect style: (UInt16) style forceActive: (BOOL) forceActive;
- (void) removeSelectionViews;
- (void) configureSelectionsWithOldLeftInset: (jint) oldLeftInset
                               oldRightInset: (jint) oldRightInset
                                   leftInset: (jint) leftInset
                                  rightInset: (jint) rightInset;
- (void) updateSelectionViews: (int*) data
                    leftInset: (jint) leftInset
                   rightInset: (jint) rightInset;
@end
