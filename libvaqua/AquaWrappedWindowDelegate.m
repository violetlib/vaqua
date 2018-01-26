/*
 * Copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

#import "AquaWrappedWindowDelegate.h"

static BOOL debugFlag = NO;

@implementation AquaWrappedWindowDelegate
{
  id delegate;
}

- (instancetype)initWithObject:(id)object
{
  delegate = object;
  return self;
}

- (BOOL)isKindOfClass:(Class)aClass
{
  // called to test for the need to wrap the delegate
  return aClass == [AquaWrappedWindowDelegate class];
}

- (BOOL)respondsToSelector:(SEL)aSelector
{
  return aSelector == @selector(windowDidResignMain:) || [delegate respondsToSelector:aSelector];
}

- (NSMethodSignature *)methodSignatureForSelector:(SEL)selector
{
  return [delegate methodSignatureForSelector:selector];
}

- (void)forwardInvocation:(NSInvocation *)invocation
{
  NSString *selectorString = NSStringFromSelector(invocation.selector);
  if (debugFlag) NSLog(@"%@: %@", selectorString, [[delegate nsWindow] title]);
  [invocation invokeWithTarget:delegate];
}

- (void) windowDidBecomeKey: (NSNotification *) notification
{
  if (debugFlag) NSLog(@"windowDidBecomeKey: %@", [[delegate nsWindow] title]);
  [delegate windowDidBecomeKey: notification];
}

- (void) windowDidResignKey: (NSNotification *) notification
{
  if (debugFlag) NSLog(@"windowDidResignKey: %@", [[delegate nsWindow] title]);
  // The implementation of windowDidResignKey is appropriate for an inactive window
  if (![[delegate nsWindow] isMainWindow]) {
    if (debugFlag) NSLog(@"  deactivating window: %@", [[delegate nsWindow] title]);
    [delegate windowDidResignKey: notification];
  } else {
    if (debugFlag) NSLog(@"  not deactivating window: %@", [[delegate nsWindow] title]);
  }
}

- (void) windowDidBecomeMain: (NSNotification *) notification
{
  if (debugFlag) NSLog(@"windowDidBecomeMain: %@", [[delegate nsWindow] title]);
  [delegate windowDidBecomeMain: notification];
}

- (void) windowDidResignMain: (NSNotification *) notification
{
  if (debugFlag) NSLog(@"windowDidResignMain: %@", [[delegate nsWindow] title]);
  // The implementation of windowDidResignKey is appropriate for an inactive window
  if (![[delegate nsWindow] isKeyWindow]) {
    if (debugFlag) NSLog(@"  deactivating window: %@", [[delegate nsWindow] title]);
    [delegate windowDidResignKey: notification];
  } else {
    if (debugFlag) NSLog(@"  not deactivating window: %@", [[delegate nsWindow] title]);
  }
}

- (BOOL) canBecomeKeyWindow
{
  BOOL result = [delegate canBecomeKeyWindow];
  if (debugFlag) NSLog(@"canBecomeKeyWindow: %@ %s", [[delegate nsWindow] title], result ? "true" : "false");
  return result;
}

- (BOOL) canBecomeMainWindow
{
  BOOL result = [delegate canBecomeMainWindow];
  if (debugFlag) NSLog(@"canBecomeMainWindow: %@ %s", [[delegate nsWindow] title], result ? "true" : "false");
  return result;
}

@end
