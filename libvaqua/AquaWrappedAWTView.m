/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

#import "AquaWrappedAWTView.h"

@implementation AquaWrappedAWTView {
    NSView *awtView;
    NSVisualEffectView *fullWindowVisualEffectView;
}

- (void) installAWTView: (NSView *) view {
    awtView = [view retain];
    [view removeFromSuperview];
    self.autoresizesSubviews = YES;
    self.wantsLayer = YES;
    awtView.autoresizingMask = NSViewWidthSizable+NSViewHeightSizable;
    [self addSubview: awtView];
}

- (NSView *) awtView {
    return awtView;
}

// Add a full sized visual effect view as a sibling of the AWT view, if there is not already one present.
// This view must always be behind all of its siblings.

- (NSVisualEffectView *) addFullWindowVisualEffectView {
    if (fullWindowVisualEffectView != nil) {
        return fullWindowVisualEffectView;
    }
    fullWindowVisualEffectView = [[NSVisualEffectView alloc] initWithFrame: self.frame];
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
        fullWindowVisualEffectView = nil;
    }
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
