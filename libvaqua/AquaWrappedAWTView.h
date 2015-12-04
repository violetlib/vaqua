/*
 * Copyright (c) 2015 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

#import <Cocoa/Cocoa.h>

@interface AquaWrappedAWTView : NSView
- (void) installAWTView: (NSView *) awtView;
- (NSView *) awtView;
- (NSVisualEffectView *) addFullWindowVisualEffectView;
- (void) addSiblingView: (NSView *) view;
- (void) removeFullWindowVisualEffectView;
- (void) deliverJavaMouseEvent: (NSEvent *) event;
- (BOOL) mouseIsOver;
@end
