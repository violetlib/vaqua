/*
 * Copyright (c) 2015-2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

#ifndef __AQUA_WRAPPED_AWT_VIEW__
#define __AQUA_WRAPPED_AWT_VIEW__

#import <Cocoa/Cocoa.h>
#import "AquaVisualEffectView.h"

@interface AquaWrappedAWTView : NSView
- (instancetype) initWithAWTView: (NSView *) awtView;
- (NSView *) awtView;
- (int) configureAsPopup: (CGFloat) cornerRadius;
- (AquaVisualEffectView *) addFullWindowVisualEffectView;
- (void) addSiblingView: (NSView *) view;
- (void) removeFullWindowVisualEffectView;
- (void) deliverJavaMouseEvent: (NSEvent *) event;
- (BOOL) mouseIsOver;
@end

#endif /* __AQUA_WRAPPED_AWT_VIEW__ */
