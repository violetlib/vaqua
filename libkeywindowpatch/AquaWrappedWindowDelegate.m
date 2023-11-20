/*
 * Copyright (c) 2018-2023 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

#import "KeyWindowPatch.h"
#import "AquaWrappedWindowDelegate.h"

@interface CMenuBar { }
@end

@interface AWTWindow { }
@end

@interface ApplicationDelegate { }
@end

@interface AWTToolkit { }
@end

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
    // NSLog(@"%@: %@", selectorString, [[delegate nsWindow] title]);
    [invocation invokeWithTarget:delegate];
}

- (void) windowDidBecomeKey: (NSNotification *) notification
{
#ifdef DEBUG_PATCH
  NSLog(@"windowDidBecomeKey: %@", [[delegate nsWindow] title]);
#endif

    if (![[delegate nsWindow] isMainWindow]) {
        [self activateWindowMenuBar];
    }
    [delegate windowDidBecomeKey: notification];
}

- (void) windowDidBecomeMain: (NSNotification *) notification
{
#ifdef DEBUG_PATCH
    NSLog(@"windowDidBecomeMain: %@", [[delegate nsWindow] title]);
#endif

    if (![[delegate nsWindow] isKeyWindow]) {
        [self activateWindowMenuBar];
    }
    [delegate windowDidBecomeMain: notification];
}

- (void) activateWindowMenuBar {
//AWT_ASSERT_APPKIT_THREAD;
    // Finds appropriate menu bar in our hierarchy
    AWTWindow *awtWindow = delegate;
    while ([awtWindow ownerWindow] != nil) {
        awtWindow = [awtWindow ownerWindow];
    }

    CMenuBar *menuBar = nil;
    BOOL isDisabled = NO;
    if ([[awtWindow nsWindow] isVisible]){
        menuBar = [awtWindow javaMenuBar];
        isDisabled = ![awtWindow isEnabled];
    }

    if (menuBar == nil) {
        menuBar = [[ApplicationDelegate sharedDelegate] defaultMenuBar];
        isDisabled = NO;
    }

    [CMenuBar activate:menuBar modallyDisabled:isDisabled];
}

- (void) windowDidResignKey: (NSNotification *) notification
{
#ifdef DEBUG_PATCH
    NSLog(@"windowDidResignKey: %@", [[delegate nsWindow] title]);
#endif

    [AWTToolkit eventCountPlusPlus];
    if (![[delegate nsWindow] isMainWindow]) {
        [self deactivateWindow];
    }
}

- (void) windowDidResignMain: (NSNotification *) notification
{
#ifdef DEBUG_PATCH
    NSLog(@"windowDidResignMain: %@", [[delegate nsWindow] title]);
#endif

    [AWTToolkit eventCountPlusPlus];
    if (![[delegate nsWindow] isKeyWindow]) {
      [self deactivateWindow];
    }
}

- (void) deactivateWindow {
//AWT_ASSERT_APPKIT_THREAD;
#ifdef DEBUG_PATCH
    NSLog(@"deactivating window: %@", [[self nsWindow] title]);
#endif

    [[delegate javaMenuBar] deactivate];

    // the new key window
    NSWindow *keyWindow = [NSApp keyWindow];
    AWTWindow *opposite = nil;
    if ([AWTWindow isAWTWindow: keyWindow]) {
        opposite = (AWTWindow *)[keyWindow delegate];
        [AWTWindow setLastKeyWindow: self];
    } else {
        [AWTWindow setLastKeyWindow: nil];
    }

    [delegate _deliverWindowFocusEvent:NO oppositeWindow: opposite];
    [delegate orderChildWindows:NO];
}

- (BOOL) canBecomeKeyWindow
{
    BOOL result = [delegate canBecomeKeyWindow];
    // NSLog(@"canBecomeKeyWindow: %@ %s", [[delegate nsWindow] title], result ? "true" : "false");
    return result;
}

- (BOOL) canBecomeMainWindow
{
    BOOL result = [delegate canBecomeMainWindow];
    // NSLog(@"canBecomeMainWindow: %@ %s", [[delegate nsWindow] title], result ? "true" : "false");
    return result;
}

// The following methods are defined to improve performance

- (void)sendEvent:(NSEvent *)event
{
    [delegate sendEvent:event];
}

- (NSObject *) javaPlatformWindow
{
    return [delegate javaPlatformWindow];
}

- (BOOL) isEnabled
{
    return [delegate isEnabled];
}

@end
