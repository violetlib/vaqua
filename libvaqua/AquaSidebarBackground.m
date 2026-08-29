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
#include "log.h"

BOOL DEBUG = NO;

// This background is used for sidebars and optionally for menus

@implementation AquaSidebarBackground {
    NSMutableArray<AquaSelectionView*> *selectionViews;
    NSVisualEffectMaterial selectionMaterial;
    BOOL forceActive;
    BOOL useInset;
    float selectionLeftInset;
    float selectionRightInset;
    float selectionCornerRadius;
}

- (void) dealloc {
    if (selectionViews) {
        [selectionViews release];
        selectionViews = nil;
    }

    [super dealloc];
}

- (AquaSidebarBackground *) initWithFrame: (NSRect) frameRect
                                    style: (UInt16) style
                             cornerRadius: (jint) cornerRadius
                              forceActive: (BOOL) shouldForceActive {
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
        useInset = NO;
        if (@available(macOS 10.16, *)) {
            useInset = YES;
        }
        if (cornerRadius > 0) {
            self.layer.cornerRadius = cornerRadius;
        }
    }

    return self;
}

- (void) setFrame: (NSRect) frameRect {
    if (DEBUG) {
        OSLog(@"Updating selection background bounds %@ %f %f %f %f",
            [self description],
            frameRect.origin.x,
            frameRect.origin.y,
            frameRect.size.width,
            frameRect.size.height);
    }
    [super setFrame:frameRect];
    float width = frameRect.size.width;

    int count = (int) selectionViews.count;
    for (int index = 0; index < count; index++) {
        AquaSelectionView *v = [selectionViews objectAtIndex:index];
        NSRect oldFrame = v.frame;
        [v setFullFrame: NSMakeRect(0, oldFrame.origin.y, width, oldFrame.size.height)];
    }
}

- (void) removeSelectionViews {
    [self updateSelectionViews: NULL];
}

- (void) configureSelectionViews {
    int count = (int) selectionViews.count;

    if (DEBUG) {
        OSLog(@"Configuring selection views %@ %d %f %f %f",
            [self description],
            count,
            selectionLeftInset,
            selectionRightInset,
            selectionCornerRadius);
    }

    for (int index = 0; index < count; index++) {
        AquaSelectionView *v = [selectionViews objectAtIndex:index];
        if (v) {
            [v configureWithLeftInset:selectionLeftInset
                           rightInset:selectionRightInset
                         cornerRadius:selectionCornerRadius];
        }
    }
    self.needsDisplay = YES;
}

- (void) configureSelectionsWithLeftInset: (jint) leftInset
                               rightInset: (jint) rightInset
                             cornerRadius: (jint) cornerRadius {
    selectionLeftInset = leftInset;
    selectionRightInset = rightInset;
    selectionCornerRadius = cornerRadius;

    if (DEBUG) {
        OSLog(@"Setting selection view configuration parameters %f %f %f",
            selectionLeftInset,
            selectionRightInset,
            selectionCornerRadius);
    }

    if (useInset) {
        [self configureSelectionViews];
    }
}

// Update the bounds of the selection views
- (void) updateSelectionViews: (int*) data {
    int *p = data;
    int count = p ? *p++ : 0;
    int index = 0;
    int currentCount = (int) selectionViews.count;
    float width = self.bounds.size.width;

    if (DEBUG) {
        OSLog(@"Updating selection views %@ %f %f count=%d",
            [self description],
            self.frame.size.width,
            self.frame.size.height,
            count);
    }

    for (index = 0; index < count; index++) {
        int y = *p++;
        int h = *p++;
        BOOL useEmphasized = h < 0;
        if (useEmphasized) {
            h = -h;
        }
        NSRect frame = NSMakeRect(0, y, width, h);
        BOOL emphasized = (self.style != SIDEBAR_STYLE) || useEmphasized;

        AquaSelectionView *v = index < currentCount ? [selectionViews objectAtIndex:index] : nil;
        if (v) {
            if (DEBUG) {
                OSLog(@"Reusing selection view %@ bounds = %d %d %f %d", [v description], 0, y, width, h);
            }
            [v reuseWithFrame:frame emphasized:emphasized forceActive:forceActive];
            if (useInset) {
                [v configureWithLeftInset:selectionLeftInset rightInset:selectionRightInset cornerRadius:selectionCornerRadius];
            }
        } else {
            if (DEBUG) {
                OSLog(@"Creating selection view bounds = %d %d %f %d", 0, y, width, h);
            }
            v = [[AquaSelectionView alloc] initWithFrame:frame
                                              emphasized:emphasized
                                             forceActive:forceActive
                                                material:selectionMaterial];
            [v configureWithLeftInset:selectionLeftInset
                           rightInset:selectionRightInset
                         cornerRadius:selectionCornerRadius];
            //[v.layer setNeedsDisplay];
            //v.needsDisplay = YES;
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

- (BOOL) isFlipped {
    return YES;
}

@end

@implementation AquaSelectionView {
    NSRect fullFrame;  // frame to which insets are applied
}

- (AquaSelectionView *) initWithFrame: (NSRect) frameRect
                           emphasized: (BOOL) emphasized
                          forceActive: (BOOL) forceActive
                             material: (NSVisualEffectMaterial) material
{
    self = [super initWithFrame: frameRect];
    if (self) {
        fullFrame = frameRect;
        self.emphasized = emphasized;
        if (forceActive) {
            self.state = NSVisualEffectStateActive;
        }
        self.wantsLayer = YES;
        self.autoresizingMask = NSViewWidthSizable;
        self.blendingMode = NSVisualEffectBlendingModeBehindWindow;
        self.material = material;
    }
    return self;
}

- (void) reuseWithFrame: (NSRect) frameRect
             emphasized: (BOOL) emphasized
            forceActive: (BOOL) forceActive
{
    self.frame = frameRect;
    fullFrame = frameRect;
    self.emphasized = emphasized;
    if (forceActive) {
        self.state = NSVisualEffectStateActive;
    } else {
        // TBD
    }
    [self.layer setNeedsDisplay];
    self.needsDisplay = YES;
}

- (void) configureWithLeftInset: (jint) leftInset
                     rightInset: (jint) rightInset
                   cornerRadius: (jint) cornerRadius
{
    if (DEBUG) {
        OSLog(@"Configuring selection view %@ %d %d %d", [self description], leftInset, rightInset, cornerRadius);
    }

    CGFloat x = fullFrame.origin.x + leftInset;
    CGFloat y = fullFrame.origin.y;
    CGFloat w = fullFrame.size.width - (leftInset + rightInset);
    CGFloat h = fullFrame.size.height;

    if (w < 0) {
        w = 0;
    }

    self.frame = NSMakeRect(x, y, w, h);
    self.layer.cornerRadius = cornerRadius;
    self.layer.masksToBounds = YES;
    [self.layer setNeedsDisplay];
    self.needsDisplay = YES;
}

- (void) setFullFrame: (NSRect) frameRect
{
    CGFloat leftInset = self.frame.origin.x - fullFrame.origin.x;
    CGFloat totalInset = fullFrame.size.width - self.frame.size.width;
    fullFrame = frameRect;
    self.frame = NSMakeRect(leftInset, frameRect.origin.y, frameRect.size.width - totalInset, frameRect.size.height);
    self.needsDisplay = YES;

    if (DEBUG) {
        OSLog(@"Updating selection view bounds %f %f %f %f",
            self.frame.origin.x,
            self.frame.origin.y,
            self.frame.size.width,
            self.frame.size.height);
    }
}

@end
