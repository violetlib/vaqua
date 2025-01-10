/*
 * Copyright (c) 2015-2025 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

#import "AquaWrappedAWTView.h"
#import "AquaVisualEffectView.h"
#import "AquaNativeSupport.h"

void VAppearances_updateAppearance(NSAppearance *appearance);

#pragma weak VAppearances_updateAppearance=updateAppearanceMissing

static void updateAppearanceMissing(NSAppearance *appearance)
{
    NSLog(@"VAppearances_updateAppearance is undefined");
}

@implementation AquaWrappedAWTView {
    NSView *awtView;
    AquaVisualEffectView *fullWindowVisualEffectView;
    NSString *lastKnownEffectiveAppearanceName;
    BOOL isPopup;
}

- (instancetype) initWithAWTView: (NSView *) view {
    if (self = [super initWithFrame: view.frame]) {
        awtView = [view retain];
        self.autoresizesSubviews = YES;
        self.wantsLayer = YES;
        // do not use auto resizing, it generates an unexpected upcall
        // instead, see setFrameSize:
        // awtView.autoresizingMask = NSViewWidthSizable+NSViewHeightSizable;
        [self addSubview: awtView];
    }
    return self;
}

- (void) dealloc {

    if (awtView) {
        [awtView release];
        awtView = nil;
    }

    if (fullWindowVisualEffectView) {
        [fullWindowVisualEffectView release];
        fullWindowVisualEffectView = nil;
    }

    [super dealloc];
}

- (NSView *) awtView {
    return awtView;
}

- (void) setFrameSize: (NSSize) newSize {
    [super setFrameSize: newSize];
    [awtView setFrameSize: newSize];
}

- (int) configureAsPopup: (CGFloat) cornerRadius {
    CALayer *layer = self.layer;
    NSWindow *w = self.window;
    if (layer && w) {
        if (![w hasShadow]) {
            NSLog(@"Window %@ has no shadow", w);
        }
        isPopup = YES;
        layer.cornerRadius = cornerRadius;
        layer.masksToBounds = YES;
        layer.opaque = NO;
        [layer setNeedsDisplay];
        CGColorRef borderColor = [self popupBorderColor];
        if (borderColor) {
            layer.borderWidth = 1;
            layer.borderColor = borderColor;
        }
        [w invalidateShadow];
        w.opaque = NO;
        w.backgroundColor = [NSColor clearColor];
        return 0;
    } else {
        NSLog(@"Unable to set corner radius: missing layer or window");
        return -1;
    }
}

- (void) reconfigurePopup {
    if (isPopup) {
        CALayer *layer = self.layer;
        if (layer) {
            CGColorRef borderColor = [self popupBorderColor];
            if (borderColor) {
                layer.borderWidth = 1;
                layer.borderColor = borderColor;
                [layer setNeedsDisplay];
            }
        }
    }
}

- (CGColorRef) popupBorderColor {
    if (@available(macOS 10.14, *)) {
        BOOL isHighContrast = NSWorkspace.sharedWorkspace.accessibilityDisplayShouldIncreaseContrast;
        if (isHighContrast) {
            return CGColorCreateGenericRGB(1, 1, 1, .70);
        }
        NSAppearance *appearance = self.effectiveAppearance;
        NSString *appearanceName = appearance.name;
        if ([appearanceName containsString: @"Dark"]) {
            return CGColorCreateGenericRGB(0.85, 0.85, 0.85, .22);
        } else {
            return CGColorCreateGenericRGB(0.85, 0.85, 0.85, .06);
        }
    } else {
        return NULL;
    }
}

// Add a full sized visual effect view as a sibling of the AWT view, if there is not already one present.
// This view must always be behind all of its siblings.

- (AquaVisualEffectView *) addFullWindowVisualEffectView {
    if (fullWindowVisualEffectView != nil) {
        return fullWindowVisualEffectView;
    }
    fullWindowVisualEffectView = [[AquaVisualEffectView alloc] initWithFrame: self.frame];
    fullWindowVisualEffectView.blendingMode = NSVisualEffectBlendingModeBehindWindow;
    fullWindowVisualEffectView.autoresizingMask = NSViewWidthSizable|NSViewHeightSizable;
    NSArray *siblings = [self subviews];
    NSView *sibling = [siblings objectAtIndex:0];
    [self addSubview: fullWindowVisualEffectView positioned: NSWindowBelow relativeTo: sibling];
    return fullWindowVisualEffectView;
}

- (void) removeFullWindowVisualEffectView {
    if (fullWindowVisualEffectView != nil) {
        [fullWindowVisualEffectView removeFromSuperview];
        [fullWindowVisualEffectView release];
        fullWindowVisualEffectView = nil;
    }
}

- (void) viewDidChangeEffectiveAppearance {

    NSAppearance *appearance = self.effectiveAppearance;
    NSString *appearanceName = appearance.name;
    NSString *previousAppearanceName = lastKnownEffectiveAppearanceName;
    if (previousAppearanceName == nil || ![previousAppearanceName isEqualToString:appearanceName]) {
        lastKnownEffectiveAppearanceName = appearanceName;
        NSLog(@"New appearance for AWT Window: %@", appearanceName);
        deliverWindowChangedAppearance(self.window, appearance);
    }

    // The popup border may need to be reconfigured
    [self reconfigurePopup];

    // Visual effect views may need to be reconfigured

    NSArray<NSView *> *views = self.subviews;
    int count = views.count;
    for (int i = 0; i < count; i++) {
        NSView *view = views[i];
        if ([view isKindOfClass: [AquaVisualEffectView class]]) {
            AquaVisualEffectView *v = (AquaVisualEffectView *) view;
            [v configureWithAppearance: appearance];
        }
    }

    [super viewDidChangeEffectiveAppearance];
    [self setNeedsDisplay:YES];
    VAppearances_updateAppearance(appearance);
}

- (void) addSiblingView: (NSView *) view {
    [self addSubview: view positioned: NSWindowBelow relativeTo: awtView];
}

- (void) deliverJavaMouseEvent: (NSEvent *) event {
    [(id)awtView deliverJavaMouseEvent: event];
}

- (BOOL) mouseIsOver {
    return [(id)awtView mouseIsOver];
}

@end
