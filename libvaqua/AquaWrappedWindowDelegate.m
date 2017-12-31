/*
 * Copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

#import "AquaWrappedWindowDelegate.h"

@implementation AquaWrappedWindowDelegate
{
  id delegate;
}

- (instancetype)initWithObject:(id)object
{
  delegate = object;
  return self;
}

- (NSMethodSignature *)methodSignatureForSelector:(SEL)selector
{
  return [delegate methodSignatureForSelector:selector];
}

- (void)forwardInvocation:(NSInvocation *)invocation
{
  NSString *selectorString = NSStringFromSelector(invocation.selector);
  //NSLog(@"%@: %@", selectorString, [[delegate nsWindow] title]);
  [invocation invokeWithTarget:delegate];
}

- (void) windowDidBecomeKey: (NSNotification *) notification
{
  NSLog(@"windowDidBecomeKey: %@", [[delegate nsWindow] title]);
  [delegate windowDidBecomeKey: notification];
}

- (void) windowDidResignKey: (NSNotification *) notification
{
  NSLog(@"windowDidResignKey: %@", [[delegate nsWindow] title]);
  // The implementation of windowDidResignKey is appropriate for an inactive window
  if (![[delegate nsWindow] isMainWindow]) {
    [delegate windowDidResignKey: notification];
  }
}

- (void) windowDidBecomeMain: (NSNotification *) notification
{
  NSLog(@"windowDidBecomeMain: %@", [[delegate nsWindow] title]);
  [delegate windowDidBecomeMain: notification];
}

- (void) windowDidResignMain: (NSNotification *) notification
{
  NSLog(@"windowDidResignMain: %@", [[delegate nsWindow] title]);
  // The implementation of windowDidResignKey is appropriate for an inactive window
  if (![[delegate nsWindow] isKeyWindow]) {
    [delegate windowDidResignKey: notification];
  }
}




@end
