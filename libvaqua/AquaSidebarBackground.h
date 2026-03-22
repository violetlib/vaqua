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
- (AquaSidebarBackground *) initWithFrame: (NSRect) frameRect
                                    style: (UInt16) style
                             cornerRadius: (jint) cornerRadius
                              forceActive: (BOOL) forceActive;
- (void) removeSelectionViews;
- (void) configureSelectionsWithLeftInset: (jint) leftInset
                               rightInset: (jint) rightInset
                             cornerRadius: (jint) cornerRadius;
- (void) updateSelectionViews: (int*) data;
@end

@interface AquaSelectionView : AquaVisualEffectView
- (AquaSelectionView *) initWithFrame: (NSRect) frameRect
                           emphasized: (BOOL) emphasized
                          forceActive: (BOOL) forceActive
                             material: (NSVisualEffectMaterial) material;
- (void) reuseWithFrame: (NSRect) frameRect
           emphasized: (BOOL) emphasized
          forceActive: (BOOL) forceActive;
- (void) configureWithLeftInset: (jint) leftInset
                     rightInset: (jint) rightInset
                   cornerRadius: (jint) cornerRadius;
- (void) setFullFrame: (NSRect) frameRect;
@end
