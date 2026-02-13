/*
 * Copyright (c) 2015-2026 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

#import "AquaSidebarBackground.h"
#import "org_violetlib_aqua_AquaVibrantSupport.h"
#import <Availability.h>

BOOL DEBUG = NO;

// This background is used for sidebars and optionally for menus

@implementation AquaSidebarBackground {
    NSMutableArray<NSVisualEffectView*> *selectionViews;
    NSVisualEffectMaterial selectionMaterial;
    BOOL forceActive;
}

- (void) dealloc {

    if (selectionViews) {
        [selectionViews release];
        selectionViews = nil;
    }

    [super dealloc];
}

- (AquaSidebarBackground *) initWithFrame: (NSRect) frameRect style: (UInt16) style forceActive: (BOOL) shouldForceActive {
    self = [super initWithFrame: frameRect];

    if (self) {
        if (floor(NSAppKitVersionNumber) <= NSAppKitVersionNumber10_10_Max) {
            self.material = NSVisualEffectMaterialAppearanceBased;
        } else if (style == SIDEBAR_STYLE) {
            self.material = NSVisualEffectMaterialSidebar;
        } else {
            self.material = NSVisualEffectMaterialMenu;
        }
        self.style = style;
        selectionMaterial = NSVisualEffectMaterialSelection;
        forceActive = shouldForceActive;
        self.wantsLayer = YES;
        self.autoresizesSubviews = YES;
        self.clipsToBounds = YES;
        selectionViews = [[NSMutableArray alloc] init];
        self.blendingMode = NSVisualEffectBlendingModeBehindWindow;
        if (shouldForceActive) {
            self.state = NSVisualEffectStateActive;
        }
    }

    return self;
}

- (void) removeSelectionViews {
    [self updateSelectionViews: NULL leftInset: 0 rightInset: 0];
}

- (void) configureSelectionsWithOldLeftInset: (jint) oldLeftInset
                               oldRightInset: (jint) oldRightInset
                                   leftInset: (jint) leftInset
                                  rightInset: (jint) rightInset {
    int count = (int) selectionViews.count;
    float width = self.bounds.size.width;

    BOOL useInset = NO;
    if (@available(macOS 10.16, *)) {
        useInset = YES;
    }

    if (DEBUG) {
        NSLog(@"Configuring selection views %@ %f %f %ld %ld",
            [self description],
            self.frame.size.width,
            self.frame.size.height,
            count,
            useInset);
    }

    for (int index = 0; index < count; index++) {
        NSVisualEffectView *v = [selectionViews objectAtIndex:index];
        if (v) {
            if (DEBUG) {
                NSLog(@"Configuring visual effect view %@", [v description]);
            }
            NSRect f = v.frame;
            CGFloat x = f.origin.x - oldLeftInset;
            CGFloat y = f.origin.y;
            CGFloat w = f.size.width + oldLeftInset + oldRightInset;
            CGFloat h = f.size.height;
            NSRect frame = NSMakeRect(x, y, w, h);
            if (useInset) {
                frame = NSMakeRect(x + leftInset, y, w - (leftInset + rightInset), h);
            }
            v.frame = frame;
            if (useInset) {
                v.layer.cornerRadius = 4;
                v.layer.masksToBounds = YES;
            }
            [v.layer setNeedsDisplay];
            v.needsDisplay = YES;
        }
    }

    self.needsDisplay = YES;
}

- (void) updateSelectionViews: (int*) data
                    leftInset: (jint) leftInset
                   rightInset: (jint) rightInset {
    int *p = data;
    int count = p ? *p++ : 0;
    int index = 0;
    int currentCount = (int) selectionViews.count;
    float width = self.bounds.size.width;

    BOOL useInset = NO;
    if (@available(macOS 10.16, *)) {
        useInset = YES;
    }

    if (DEBUG) {
        NSLog(@"Updating selection views %@ %f %f %ld %ld",
            [self description],
            self.frame.size.width,
            self.frame.size.height,
            count,
            useInset);
    }

    for (index = 0; index < count; index++) {
        int y = *p++;
        int h = *p++;
        BOOL useEmphasized = h < 0;
        if (useEmphasized) {
            h = -h;
        }
        NSRect frame = NSMakeRect(0, y, width, h);
        if (useInset) {
            frame = NSMakeRect(leftInset, y, width - (leftInset + rightInset), h);
        }
        NSVisualEffectView *v = index < currentCount ? [selectionViews objectAtIndex:index] : nil;
        if (v) {
            if (DEBUG) {
                NSLog(@"Reusing visual effect view %@", [v description]);
            }
            v.frame = frame;
            v.emphasized = (self.style != SIDEBAR_STYLE) || useEmphasized;
            if (useInset) {
                v.layer.cornerRadius = 4;
                v.layer.masksToBounds = YES;
            }
            if (forceActive) {
                v.state = NSVisualEffectStateActive;
            }
            [v.layer setNeedsDisplay];
            v.needsDisplay = YES;
        } else {
            if (DEBUG) {
                NSLog(@"Creating visual effect view");
            }
            v = [[NSVisualEffectView alloc] initWithFrame: frame];
            v.wantsLayer = YES;
            v.autoresizingMask = NSViewWidthSizable;
            v.blendingMode = NSVisualEffectBlendingModeBehindWindow;
            v.emphasized = (self.style != SIDEBAR_STYLE) || useEmphasized;
            v.material = selectionMaterial;
            if (useInset) {
                v.layer.cornerRadius = 4;
                v.layer.masksToBounds = YES;
            }
            if (forceActive) {
                v.state = NSVisualEffectStateActive;
            }
            [v.layer setNeedsDisplay];
            v.needsDisplay = YES;
            [selectionViews addObject: v];
            [self addSubview: v];
        }
    }

    for (int i = index; i < currentCount; i++) {
        NSVisualEffectView *v = selectionViews.lastObject;
        [selectionViews removeLastObject];
        [v removeFromSuperview];
    }

    self.needsDisplay = YES;
}

- (NSVisualEffectView *) createVisualEffectViewWithFrame: (NSRect) frame {
    NSVisualEffectView *v = [[NSVisualEffectView alloc] initWithFrame: frame];
    v.blendingMode = NSVisualEffectBlendingModeBehindWindow;
    v.wantsLayer = YES;
    v.material = self.material;
    return v;
}

- (BOOL) isFlipped {
    return YES;
}

@end
