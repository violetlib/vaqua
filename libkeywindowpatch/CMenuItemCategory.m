/*
 * Copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

#import <Cocoa/Cocoa.h>
#import <JavaNativeFoundation/JavaNativeFoundation.h>
#include <Carbon/Carbon.h>
#import "KeyWindowPatch.h"
#import "CMenuItemCategory.h"

static unichar CHAR_UNDEFINED = 0xFFFF;

// The following functions and class are located in JDK dynamic libraries.

extern unichar NsCharToJavaChar(unichar nsChar, NSUInteger modifiers);
jint NsKeyModifiersToJavaModifiers(NSUInteger nsFlags, BOOL isExtMods);
jlong UTC(NSEvent *event);

@interface ThreadUtilities { }
+ (JNIEnv*)getJNIEnv;
@end

@implementation CMenuItem (CMenuItemCategory)

- (void)handleAction:(NSMenuItem *)sender
{
#ifdef DEBUG_PATCH
    NSLog(@"Patched CMenuItem called");
#endif

    //AWT_ASSERT_APPKIT_THREAD;
    JNIEnv *env = [ThreadUtilities getJNIEnv];
    JNF_COCOA_ENTER(env);

    // If we are called as a result of user pressing a shortcut, do nothing,
    // because AVTView has already sent corresponding key event to the Java
    // layer from performKeyEquivalent.
    // There is an exception from the rule above, though: if a window with
    // a menu gets minimized by user and there are no other windows to take
    // focus, the window's menu won't be removed from the global menu bar.
    // However, the Java layer won't handle invocation by a shortcut coming
    // from this "frameless" menu, because there are no active windows. This
    // means we have to handle it here.
    NSEvent *currEvent = [[NSApplication sharedApplication] currentEvent];
    if (fIsCheckbox) {
        static JNF_CLASS_CACHE(jc_CCheckboxMenuItem, "sun/lwawt/macosx/CCheckboxMenuItem");
        static JNF_MEMBER_CACHE(jm_ckHandleAction, jc_CCheckboxMenuItem, "handleAction", "(Z)V");

        // Send the opposite of what's currently checked -- the action
        // indicates what state we're going to.
        NSInteger state = [sender state];
        jboolean newState = (state == NSOnState ? JNI_FALSE : JNI_TRUE);
        JNFCallVoidMethod(env, fPeer, jm_ckHandleAction, newState);
    }
    else {
        if ([currEvent type] == NSKeyDown) {
            // Event available through sender variable hence NSApplication
            // not needed for checking the keyboard input sans the modifier keys
            // Also, the method used to fetch eventKey earlier would be locale dependent
            // With earlier implementation, if MenuKey: e EventKey: ा ; if input method
            // is not U.S. (Devanagari in this case)
            // With current implementation, EventKey = MenuKey = e irrespective of
            // input method
            NSString *eventKey = [sender keyEquivalent];
            // Apple uses characters from private Unicode range for some of the
            // keys, so we need to do the same translation here that we do
            // for the regular key down events
            if ([eventKey length] == 1) {
                unichar origChar = [eventKey characterAtIndex:0];
                unichar newChar =  NsCharToJavaChar(origChar, 0);
                if (newChar == CHAR_UNDEFINED) {
                    newChar = origChar;
                }
                eventKey = [NSString stringWithCharacters: &newChar length: 1];
            }

            // Here is the change. The action event can be ignored only if the window is an AWT window.
            // Otherwise, the action event is the only notification and must be processed.
            NSWindow *keyWindow = [NSApp keyWindow];
            if (keyWindow != nil && [[keyWindow delegate] respondsToSelector: @selector(sendEvent:)]) {
                return;
            }
		}

        static JNF_CLASS_CACHE(jc_CMenuItem, "sun/lwawt/macosx/CMenuItem");
        static JNF_MEMBER_CACHE(jm_handleAction, jc_CMenuItem, "handleAction", "(JI)V"); // AWT_THREADING Safe (event)

        NSUInteger modifiers = [currEvent modifierFlags];
        jint javaModifiers = NsKeyModifiersToJavaModifiers(modifiers, NO);

        JNFCallVoidMethod(env, fPeer, jm_handleAction, UTC(currEvent), javaModifiers); // AWT_THREADING Safe (event)
    }
    JNF_COCOA_EXIT(env);
}

@end

jlong UTC(NSEvent *event) {
    struct timeval tv;
    if (gettimeofday(&tv, NULL) == 0) {
        long long sec = (long long)tv.tv_sec;
        return (sec*1000) + (tv.tv_usec/1000);
    }
    return 0;
}
