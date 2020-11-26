/*
 * Copyright (c) 2015-2020 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

#import "AquaSidebarBackground.h"
#import <Availability.h>

@implementation AquaSidebarBackground {
    NSMutableArray<NSVisualEffectView*> *selectionViews;
    NSVisualEffectMaterial selectionMaterial;
}

- (AquaSidebarBackground *) initWithFrame: (NSRect) frameRect {
    self = [super initWithFrame: frameRect];

    if (self) {
        if (floor(NSAppKitVersionNumber) <= NSAppKitVersionNumber10_10_Max) {
            self.material = NSVisualEffectMaterialAppearanceBased;
        } else {
            self.material = NSVisualEffectMaterialSidebar;
        }
        self.style = SIDEBAR_STYLE;
        selectionMaterial = NSVisualEffectMaterialSelection;

        self.wantsLayer = YES;
        self.autoresizesSubviews = YES;
        selectionViews = [[NSMutableArray alloc] init];
        self.blendingMode = NSVisualEffectBlendingModeBehindWindow;
    }

    return self;
}

- (void) updateSelectionViews: (int*) data {
    int *p = data;
    int count = p ? *p++ : 0;
    int index = 0;
    int currentCount = (int) selectionViews.count;
    float width = self.bounds.size.width;

//    NSLog(@"Updating selection views %@ %f %f %@ %f %f",
//        [self description],
//        self.frame.size.width,
//        self.frame.size.height,
//        [backgroundView description],
//        backgroundView.frame.size.width,
//        backgroundView.frame.size.height);

    BOOL useInset = NO;
    if (@available(macOS 10.16, *)) {
        useInset = YES;
    }

    for (index = 0; index < count; index++) {
        int y = *p++;
        int h = *p++;
        NSRect frame = NSMakeRect(0, y, width, h);
        if (useInset) {
            int inset = 10;
            frame = NSMakeRect(inset, y, width - 2*inset, h);
        }
        NSVisualEffectView *v = index < currentCount ? [selectionViews objectAtIndex:index] : nil;
        if (v) {
            v.frame = frame;
        } else {
            v = [[NSVisualEffectView alloc] initWithFrame: frame];
            v.wantsLayer = YES;
            v.autoresizingMask = NSViewWidthSizable;
            v.blendingMode = NSVisualEffectBlendingModeBehindWindow;
            v.material = selectionMaterial;
            if (useInset) {
                v.layer.cornerRadius = 5;
                v.layer.masksToBounds = YES;
            }
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
