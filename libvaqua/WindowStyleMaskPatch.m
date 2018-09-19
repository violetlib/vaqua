/*
 * @(#)WindowStyleMaskPatch.m
 *
 * Copyright (c) 2018 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

#import <Cocoa/Cocoa.h>

#import <objc/runtime.h>

/*
    The purpose of this patch is to preserve certain window style mask bits set by VAqua from being reset by AWT.
    Starting with JDK 11, AWT resets the style mask after showing a window, with no subsequent event generated.
*/

static char holderKey;

static NSWindowStyleMask MY_STYLE_BITS = NSWindowStyleMaskTitled | NSWindowStyleMaskFullSizeContentView;

//#define DEBUG_PATCH

@interface AWTWindow_Normal : NSWindow
@end

@interface AWTWindow_Panel : NSPanel
@end

@interface AWTWindow_Normal (WindowStyleMaskPatch)
- (void) setStyleMaskOverride: (NSWindowStyleMask) mask;
@end

@interface AWTWindow_Panel (WindowStyleMaskPatch)
- (void) setStyleMaskOverride: (NSWindowStyleMask) mask;
@end

@interface WindowStyleMaskPatchMaskHolder : NSObject
    @property NSWindowStyleMask specifiedMask;
@end

@implementation WindowStyleMaskPatchMaskHolder
    @synthesize specifiedMask;
@end

@implementation AWTWindow_Normal (WindowStyleMaskPatch)
- (void) setStyleMaskOverride: (NSWindowStyleMask) mask
{
#ifdef DEBUG_PATCH
    NSLog(@"Overriding window style mask %lx", (unsigned long) mask);
#endif
    WindowStyleMaskPatchMaskHolder *holder = objc_getAssociatedObject(self, &holderKey);
    if (holder == nil) {
        holder = [[WindowStyleMaskPatchMaskHolder alloc] init];
	    objc_setAssociatedObject(self, &holderKey, holder, OBJC_ASSOCIATION_RETAIN_NONATOMIC);
    }
    holder.specifiedMask = mask;

    [super setStyleMask: mask];
}

- (void) setStyleMask: (NSWindowStyleMask) mask
{
#ifdef DEBUG_PATCH
    NSLog(@"Setting window style mask %lx", (unsigned long) mask);
#endif
    WindowStyleMaskPatchMaskHolder *holder = objc_getAssociatedObject(self, &holderKey);
    if (holder) {
        NSWindowStyleMask revised = (mask & ~MY_STYLE_BITS) | (holder.specifiedMask & MY_STYLE_BITS);
        if (revised != mask) {
#ifdef DEBUG_PATCH
            NSLog(@"Fixing window style mask %lx -> %lx", (unsigned long) mask, (unsigned long) revised);
#endif
            mask = revised;
        }
    }

    [super setStyleMask: mask];
}
@end

@implementation AWTWindow_Panel (WindowStyleMaskPatch)
- (void) setStyleMaskOverride: (NSWindowStyleMask) mask
{
#ifdef DEBUG_PATCH
    NSLog(@"Overriding panel style mask %lx", (unsigned long) mask);
#endif
    WindowStyleMaskPatchMaskHolder *holder = objc_getAssociatedObject(self, &holderKey);
    if (holder == nil) {
        holder = [[WindowStyleMaskPatchMaskHolder alloc] init];
	    objc_setAssociatedObject(self, &holderKey, holder, OBJC_ASSOCIATION_RETAIN_NONATOMIC);
    }
    holder.specifiedMask = mask;

    [super setStyleMask: mask];
}

- (void) setStyleMask: (NSWindowStyleMask) mask
{
#ifdef DEBUG_PATCH
    NSLog(@"Setting panel style mask %lx", (unsigned long) mask);
#endif
    WindowStyleMaskPatchMaskHolder *holder = objc_getAssociatedObject(self, &holderKey);
    if (holder) {
        NSWindowStyleMask revised = (mask & ~MY_STYLE_BITS) | (holder.specifiedMask & MY_STYLE_BITS);
        if (revised != mask) {
#ifdef DEBUG_PATCH
            NSLog(@"Fixing panel style mask %lx -> %lx", (unsigned long) mask, (unsigned long) revised);
#endif
            mask = revised;
        }
    }

    [super setStyleMask: mask];
}
@end
