/*
 * @(#)AquaNativeSupport.m
 *
 * Copyright (c) 2004-2007 Werner Randelshofer, Switzerland.
 * Copyright (c) 2014-2024 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this software, except in
 * accordance with the license agreement you entered into with
 * Werner Randelshofer. For details see accompanying license terms.
 */

/**
 * Native code support for the VAqua look and feel.
 *
 * @version $Id$
 */

static int VERSION = 3;

#include <stdio.h>
#include <assert.h>
#include <math.h>

#import <Cocoa/Cocoa.h>
#import <CoreServices/CoreServices.h>
#import <CoreFoundation/CoreFoundation.h>
#import <CoreGraphics/CoreGraphics.h>
#import <Availability.h>

#import <QuickLookThumbnailing/QuickLookThumbnailing.h>
#import <Quartz/Quartz.h>

#include "jnix.h"

#include "org_violetlib_aqua_fc_OSXFile.h"
#include "org_violetlib_aqua_OSXSystemProperties.h"
#include "org_violetlib_aqua_AquaNativeSupport.h"
#include "org_violetlib_aqua_AquaIcon.h"
#include "org_violetlib_aqua_AquaImageFactory.h"
#include "org_violetlib_aqua_AquaNativeColorChooser.h"
#include "org_violetlib_aqua_AquaUtils.h"
#include "org_violetlib_aqua_AquaSheetSupport.h"
#include "org_violetlib_aqua_AquaVibrantSupport.h"

#import "AquaSidebarBackground.h"
#import "AquaWrappedAWTView.h"
#import "AquaVisualEffectView.h"
#import "JavaWindowAccess.h"

// Not sure if this works, but try to ensure that patched classes are loaded before the patch.

@interface CMenuBar
@end

@interface CMenuItem
@end

static JavaVM *vm;
static jint javaVersion;
static jobject synchronizeCallback;
static jobject windowChangedAppearanceCallback;

long getJavaVersion()
{
    return javaVersion;
}

NSString *createIndentation(int indent)
{
    return [@"                                   " substringToIndex: indent];
}

NSString *createColorDescription(NSColor *color)
{
    if (!color) {
        return @"";
    }
    color = [color colorUsingColorSpace: NSColorSpace.sRGBColorSpace];
    CGFloat red = color.redComponent;
    CGFloat green = color.greenComponent;
    CGFloat blue = color.blueComponent;
    CGFloat alpha = color.alphaComponent;
    if (alpha == 1) {
        return [NSString stringWithFormat: @"[%.2f %.2f %.2f]", red, green, blue];
    } else {
        return [NSString stringWithFormat: @"[%.2f %.2f %.2f %.2f]", red, green, blue, alpha];
    }
}

NSString *createCGColorDescription(CGColorRef color)
{
    if (!color) {
        return @"";
    }
    return createColorDescription([NSColor colorWithCGColor:color]);
}

NSString *createFrameDescription(NSRect frame)
{
    return [NSString stringWithFormat: @"[%.2f %.2f %.2f %.2f]",
        frame.origin.x, frame.origin.y, frame.size.width, frame.size.height];
}

NSString *createLayerDescription(CALayer *layer)
{
    if (layer) {
        NSString *description = [layer debugDescription];
        NSRect frame = layer.frame;
        NSString *od = layer.opaque ? @" Opaque" : @"";
        NSString *md = layer.masksToBounds ? @" Masks" : @"";
        NSString *rd = layer.cornerRadius > 0 ? [NSString stringWithFormat: @"Corner=%.2f", layer.cornerRadius] : @"";
        NSString *cd = createCGColorDescription(layer.backgroundColor);
        NSString *fd = createFrameDescription(layer.frame);
        return [NSString stringWithFormat: @" %@%@%@ %@ %@ %@", layer, od, md, rd, cd, fd];
    } else {
        return @"";
    }
}

NSString *createViewDescription(NSView *v)
{
    if (v) {
        NSString *description = [v description];
        if ([v isKindOfClass: [NSVisualEffectView class]]) {
            NSVisualEffectView *vv = (NSVisualEffectView*) v;
            description = [NSString stringWithFormat: @"%@ state=%ld", description, (long) vv.state];
        }
        return description;
    } else {
        return @"";
    }
}

void viewDebug(NSView *v, NSString *title, int indent)
{
    NSString *titleString = title ? [NSString stringWithFormat: @"%@: ", title] : @"";
    NSString *layerDescription = createLayerDescription(v.layer);
    NSString *od = v.opaque ? @" Opaque" : @"";
    NSString *viewDescription = createViewDescription(v);
    NSString *fd = createFrameDescription(v.frame);

    NSString *indentation = createIndentation(indent);

    NSLog(@"%@%@%@%@ %@",
        indentation,
        titleString, viewDescription, od, fd);
    NSLog(@"%@  Layer: %@", indentation, layerDescription);


//    if (v.layer) {
//        if (v.layer.superlayer) {
//            NSLog(@"%@superlayer: %@",
//                createIndentation(indent+2), createLayerDescription(v.layer.superlayer));
//        }
//        if (v.layer.sublayers) {
//            for (CALayer *sl in v.layer.sublayers) {
//                NSLog(@"%@sublayer: %@",
//                    createIndentation(indent+2), createLayerDescription(sl));
//            }
//        }
//    }

    for (NSView *sv in v.subviews) {
        viewDebug(sv, @"", indent+2);
    }
}

NSView *getTopView(NSWindow *w)
{
    NSView *view = w.contentView;
    while (view != nil) {
        NSView *parent = view.superview;
        if (parent == nil) {
            return view;
        }
        view = parent;
    }
    return nil;
}

void windowDebug(NSWindow *w)
{
    NSString *od = w.opaque ? @" Opaque" : @"";
    NSString *td = w.titlebarAppearsTransparent ? @" TransparentTitleBar" : @"";
    NSString *fd = createFrameDescription(w.frame);
    NSRect frame = w.frame;
    NSLog(@"Window: %@ %lx%@%@ %@", [w description], (unsigned long) w.styleMask, od, td, fd);
    NSLog(@"  Background: %@", createColorDescription(w.backgroundColor));

    NSAppearance *appearance = w.appearance;
    if (appearance) {
        NSLog(@"  Appearance: %@", [appearance name]);
    }
    appearance = w.effectiveAppearance;
    if (appearance) {
        NSLog(@"  Effective appearance: %@", [appearance name]);
    }

    NSView *v = getTopView(w);
    if (v != nil) {
        viewDebug(v, @"", 2);
    }
}

jboolean ensureVM(JNIEnv *env)
{
    if (!vm) {
        return (*env)->GetJavaVM(env, &vm) == 0;
    }
    return YES;
}

void runOnMainThread(void (^block)())
{
    APPKIT_EXEC(block);
}

void runFromNativeThread(void (^block)(JNIEnv *))
{
    assert(vm);

    jboolean attachedHere = NO;

    JNIEnv *env = NULL;
    int status = (*vm)->GetEnv(vm, (void **) &env, JNI_VERSION_1_6);
    if (status == JNI_EDETACHED) {
        status = (*vm)->AttachCurrentThread(vm, (void **) &env, 0);
        if (status == JNI_OK) {
            attachedHere = YES;
        }
    }

    if (status == JNI_OK) {
        block(env);
    } else {
        NSLog(@"Unable to attach thread %d", status);
    }

    if (attachedHere) {
        (*vm)->DetachCurrentThread(vm);
    }
}

void setupLayers(NSView *v)
{
    NSView *vv = v;
    while (vv) {
        vv.wantsLayer = YES;
        vv = vv.superview;
    }
}

// Ensure that our wrapper view is installed as the parent of the AWT view.

AquaWrappedAWTView *ensureWrapper(NSWindow *w)
{
    NSView *contentView = w.contentView;
    if ([contentView isKindOfClass: [AquaWrappedAWTView class]]) {
        return (AquaWrappedAWTView *) contentView;
    }

    [contentView retain];
    w.contentView = nil;
    AquaWrappedAWTView *wrapper = [[AquaWrappedAWTView alloc] initWithAWTView:contentView];
    w.contentView = wrapper;

    return wrapper;
}

AquaWrappedAWTView *getWrapper(NSWindow *w)
{
    NSView *contentView = w.contentView;
    if ([contentView isKindOfClass: [AquaWrappedAWTView class]]) {
        return (AquaWrappedAWTView *) contentView;
    }
    return nil;
}

NSView *getAWTView(NSWindow *w)
{
    NSView *contentView = w.contentView;
    if ([contentView isKindOfClass: [AquaWrappedAWTView class]]) {
        AquaWrappedAWTView *wrapper = (AquaWrappedAWTView *) contentView;
        return [wrapper awtView];
    }
    return contentView;
}

@interface MyDefaultResponder : NSObject
- (void)defaultsChanged:(NSNotification *)notification;
@end

@implementation MyDefaultResponder
- (void)defaultsChanged:(NSNotification *)notification {
    //NSLog(@"Notification received: %@", [notification name]);

    assert(vm);

    NSUserDefaults* defaults = [NSUserDefaults standardUserDefaults];
    [defaults synchronize];

    if (synchronizeCallback != NULL) {
        runFromNativeThread(^(JNIEnv *env) {
            jclass cl = (*env)->GetObjectClass(env, synchronizeCallback);
            jmethodID m = (*env)->GetMethodID(env, cl, "run", "()V");
            if (m != NULL) {
                (*env)->CallVoidMethod(env, synchronizeCallback, m);
            } else {
                NSLog(@"Unable to invoke callback -- run method not found");
            }
        });
    }
}
@end

/*
 * Class:     org_violetlib_aqua_OSXSystemProperties
 * Method:    nativeGetFullKeyboardAccessEnabled
 * Signature: ()Z
 */
JNIEXPORT jboolean JNICALL Java_org_violetlib_aqua_OSXSystemProperties_nativeGetFullKeyboardAccessEnabled
    (JNIEnv *env, jclass cl)
{
    jboolean result = NO;

    COCOA_ENTER();

    NSUserDefaults *userDefaults = [NSUserDefaults standardUserDefaults];
    NSInteger value = [userDefaults integerForKey: @"AppleKeyboardUIMode"];
    result = (value & 02) != 0;

    COCOA_EXIT();

    return result;
}

/*
 * Class:     org_violetlib_aqua_OSXSystemProperties
 * Method:    nativeGetShowAllFiles
 * Signature: ()Z
 */
JNIEXPORT jboolean JNICALL Java_org_violetlib_aqua_OSXSystemProperties_nativeGetShowAllFiles
    (JNIEnv *env, jclass cl)
{
    jboolean result = NO;

    COCOA_ENTER();

    NSUserDefaults *userDefaults = [NSUserDefaults standardUserDefaults];
    [userDefaults addSuiteNamed: @"com.apple.finder" ];
    result = [userDefaults boolForKey:@"AppleShowAllFiles"];

    //NSLog(@"Show all files: %d", result);

    COCOA_EXIT();

    return result;
}

/*
 * Class:     org_violetlib_aqua_OSXSystemProperties
 * Method:    nativeGetScrollToClick
 * Signature: ()Z
 */
JNIEXPORT jboolean JNICALL Java_org_violetlib_aqua_OSXSystemProperties_nativeGetScrollToClick
    (JNIEnv *env, jclass cl)
{
    jboolean result = NO;

    COCOA_ENTER();

    NSUserDefaults *userDefaults = [NSUserDefaults standardUserDefaults];
    result = [userDefaults boolForKey:@"AppleScrollerPagingBehavior"];

    COCOA_EXIT();

    return result;
}

/*
 * Class:     org_violetlib_aqua_OSXSystemProperties
 * Method:    nativeGetUseOverlayScrollBars
 * Signature: ()Z
 */
JNIEXPORT jboolean JNICALL Java_org_violetlib_aqua_OSXSystemProperties_nativeGetUseOverlayScrollBars
    (JNIEnv *env, jclass cl)
{
    jboolean result = NO;

    COCOA_ENTER();

    NSScrollerStyle style = [NSScroller preferredScrollerStyle];
    result = style == NSScrollerStyleOverlay;
    //NSLog(@"Use overlay scroll bars: %ld %d", (long) style, result);

    COCOA_EXIT();

    return result;
}

/*
 * Class:     org_violetlib_aqua_OSXSystemProperties
 * Method:    nativeGetReduceTransparency
 * Signature: ()Z
 */
JNIEXPORT jboolean JNICALL Java_org_violetlib_aqua_OSXSystemProperties_nativeGetReduceTransparency
    (JNIEnv *env, jclass cl)
{
    jboolean result = NO;

    COCOA_ENTER();

    NSUserDefaults *userDefaults = [NSUserDefaults standardUserDefaults];
    result = [userDefaults boolForKey:@"reduceTransparency"];

    COCOA_EXIT();

    return result;
}

/*
 * Class:     org_violetlib_aqua_OSXSystemProperties
 * Method:    enableCallback
 * Signature: (Ljava/lang/Runnable;)V
 */
JNIEXPORT void JNICALL Java_org_violetlib_aqua_OSXSystemProperties_enableCallback
    (JNIEnv *env, jclass cl, jobject jrunnable)
{
    COCOA_ENTER();

    synchronizeCallback = (*env)->NewGlobalRef(env, jrunnable);

    if (synchronizeCallback != NULL && ensureVM(env)) {
        NSString * const KeyboardUIModeDidChangeNotification = @"com.apple.KeyboardUIModeDidChange";
        NSString * const ReduceTransparencyStatusDidChangeNotification = @"AXInterfaceReduceTransparencyStatusDidChange";

        MyDefaultResponder *r = [[MyDefaultResponder alloc] init];
        [r retain];
        NSNotificationCenter *center = [NSNotificationCenter defaultCenter];
        NSDistributedNotificationCenter *dcenter = [NSDistributedNotificationCenter defaultCenter];
        [center addObserver:r
                    selector:@selector(defaultsChanged:)
                        name:NSPreferredScrollerStyleDidChangeNotification
                      object:nil];
        [dcenter addObserver:r
                    selector:@selector(defaultsChanged:)
                        name:KeyboardUIModeDidChangeNotification  // use nil to see all notifications
                      object:nil];
        [dcenter addObserver:r
                    selector:@selector(defaultsChanged:)
                        name:ReduceTransparencyStatusDidChangeNotification
                      object:nil];
        //NSLog(@"Observer registered");
    }

    COCOA_EXIT();
}

/*
 * Class:     org_violetlib_aqua_fc_OSXFile
 * Method:    getFileType
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_org_violetlib_aqua_fc_OSXFile_nativeGetFileType
    (JNIEnv *env, jclass instance, jstring pathJ)
{
    // Assert arguments
    if (pathJ == NULL) return false;

    jint result = -1;

    // Allocate a memory pool
    NSAutoreleasePool* pool = [NSAutoreleasePool new];

    // Convert Java String to NS String
    const jchar *pathC = (*env)->GetStringChars(env, pathJ, NULL);
    NSString *pathNS = [NSString stringWithCharacters:(UniChar *)pathC length:(*env)->GetStringLength(env, pathJ)];
    (*env)->ReleaseStringChars(env, pathJ, pathC);

    // Do the API calls
    NSFileManager *fileManagerNS = [NSFileManager defaultManager];
    NSDictionary* d = [fileManagerNS attributesOfItemAtPath:pathNS error:nil];
    if (d != nil) {
        NSString* fileType = [d fileType];
        if (fileType != nil) {
            if ([fileType isEqualToString:NSFileTypeRegular]) {
                result = 0;
            } else if ([fileType isEqualToString:NSFileTypeDirectory]) {
                result = 1;
            } else if ([fileType isEqualToString:NSFileTypeSymbolicLink]) {
                result = 2;
            }
        }
    }

    // Release memory pool
    [pool release];

    // Return the result
    return result;
}

/*
 * Class:     org_violetlib_aqua_fc_OSXFile
 * Method:    resolveAlias
 * Signature: (Ljava/lang/String;Z)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_org_violetlib_aqua_fc_OSXFile_nativeResolveAlias__Ljava_lang_String_2Z
    (JNIEnv *env, jclass instance, jstring aliasPathJ, jboolean noUI)
{
    // Assert arguments
    if (aliasPathJ == NULL) return false;

    jstring result = NULL;

    // Allocate a memory pool
    NSAutoreleasePool* pool = [NSAutoreleasePool new];

    // Convert Java String to NS String
    const jchar *pathC = (*env)->GetStringChars(env, aliasPathJ, NULL);
    NSString *pathNS = [NSString stringWithCharacters:(UniChar *)pathC length:(*env)->GetStringLength(env, aliasPathJ)];
    (*env)->ReleaseStringChars(env, aliasPathJ, pathC);

    // Do the API calls
    NSString *resultNS = [pathNS stringByResolvingSymlinksInPath];
    if (resultNS != nil) {
        // Convert NSString to jstring
        result = (*env)->NewStringUTF(env, [resultNS UTF8String]);
    }

    // Release memory pool
    [pool release];

    // Return the result
    return result;
}

/*
 * Class:     org_violetlib_aqua_fc_OSXFile
 * Method:    jniResolveAlias
 * Signature: ([BZ)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_org_violetlib_aqua_fc_OSXFile_nativeResolveAlias___3BZ
    (JNIEnv *env, jclass instance, jbyteArray serializedAlias, jboolean noUI)
{
    // Assert arguments
    if (serializedAlias == NULL) return false;

    CFDataRef dataRef;
    CFDataRef bookmarkDataRef;
    UInt8* serializedAliasBytes; // bytes of serializedAlias
    int length; // length of serializedAlias
    UInt8 resolvedPathC[2048];
    jstring result = NULL;

    length = (*env)->GetArrayLength(env, serializedAlias);
    serializedAliasBytes = (UInt8 *) (*env)->GetByteArrayElements(env, serializedAlias, NULL);
    if (serializedAliasBytes != NULL) {
        dataRef = CFDataCreate(NULL, serializedAliasBytes, length);
        if (dataRef != NULL) {
            bookmarkDataRef = CFURLCreateBookmarkDataFromAliasRecord(NULL, dataRef);
            if (bookmarkDataRef != NULL) {
                CFURLBookmarkResolutionOptions opt = (noUI) ? kCFBookmarkResolutionWithoutUIMask : 0;
                Boolean isStale;
                CFErrorRef error;
                CFURLRef u = CFURLCreateByResolvingBookmarkData(NULL, bookmarkDataRef, opt, NULL, NULL, &isStale, &error);
                if (u != NULL) {
                    Boolean success = CFURLGetFileSystemRepresentation(u, true, resolvedPathC, 2048);
                    if (success) {
                        result = (*env)->NewStringUTF(env, (const char *) resolvedPathC);
                    }
                    CFRelease(u);
                }
                CFRelease(bookmarkDataRef);
            }
            CFRelease(dataRef);
        }
        (*env)->ReleaseByteArrayElements(env, serializedAlias, (jbyte *) serializedAliasBytes, JNI_ABORT);
    }
    return result;
}

/*
 * Class:     org_violetlib_aqua_fc_OSXFile
 * Method:    getLabel
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_org_violetlib_aqua_fc_OSXFile_nativeGetLabel
    (JNIEnv *env, jclass instance, jstring pathJ)
{

    // Assert arguments
    if (pathJ == NULL) return -1;

    // Allocate a memory pool
    NSAutoreleasePool* pool = [NSAutoreleasePool new];

    jint result = -1;

    // Convert Java String to NS String
    const jchar *pathC = (*env)->GetStringChars(env, pathJ, NULL);
    NSString *pathNS = [NSString stringWithCharacters:(UniChar *)pathC length:(*env)->GetStringLength(env, pathJ)];
    (*env)->ReleaseStringChars(env, pathJ, pathC);

    // Do the API calls
    NSURL *u = [NSURL fileURLWithPath:pathNS];
    if (u != nil) {
        CFErrorRef error;
        CFNumberRef fileLabel;
        Boolean success = CFURLCopyResourcePropertyForKey((CFURLRef) u, kCFURLLabelNumberKey, &fileLabel, &error);
        if (success) {
            CFNumberGetValue(fileLabel, kCFNumberSInt32Type, &result);
        }
    }

    // Release memory pool
    [pool release];

    // Return the result
    return result;
}

/*
 * Class:     org_violetlib_aqua_fc_OSXFile
 * Method:    getKindString
 * Signature: (Ljava/lang/String;)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_org_violetlib_aqua_fc_OSXFile_nativeGetKindString
    (JNIEnv *env, jclass instance, jstring pathJ)
{
    // Assert arguments
    if (pathJ == NULL) return NULL;

    // Allocate a memory pool
    NSAutoreleasePool* pool = [NSAutoreleasePool new];

    jstring result = NULL;

    // Convert Java String to NS String
    const jchar *pathC = (*env)->GetStringChars(env, pathJ, NULL);
    NSString *pathNS = [NSString stringWithCharacters:(UniChar *)pathC length:(*env)->GetStringLength(env, pathJ)];
    (*env)->ReleaseStringChars(env, pathJ, pathC);

    // Do the API calls
    NSURL *u = [NSURL fileURLWithPath:pathNS];
    if (u != nil) {
        CFErrorRef error;
        CFStringRef kind;
        Boolean success = CFURLCopyResourcePropertyForKey((CFURLRef) u, kCFURLLocalizedTypeDescriptionKey, &kind, &error);
        if (success) {
            CFRange range;
            range.location = 0;
            // Note that CFStringGetLength returns the number of UTF-16 characters,
            // which is not necessarily the number of printed/composed characters
            range.length = CFStringGetLength(kind);
            UniChar charBuf[range.length];
            CFStringGetCharacters(kind, range, charBuf);
            result = (*env)->NewString(env, (jchar *)charBuf, (jsize)range.length);
        }
    }

    // Release memory pool
    [pool release];

    // Return the result
    return result;
}

// Render an image into a Java int array
// w and h is the desired image size (in points)
// scaleFactor is the scaleFactor of the display for which this rendering is intended
// return NULL if scaleFactor is greater than 1 and the image has only one representation

static jintArray renderImageIntoBufferForDisplay(JNIEnv *env, NSImage *image, jfloat w, jfloat h, jfloat scaleFactor)
{
    //NSLog(@"Calling renderImageIntoBufferForDisplay %f %f %f on thread %@", w, h, scaleFactor, NSThread.currentThread);

//     if (scaleFactor > 1 && [[image representations] count] < 2) {
//         return NULL;
//     }

    int rw = (int) ceil(w * scaleFactor);
    int rh = (int) ceil(h * scaleFactor);

    jboolean isCopy = JNI_FALSE;
    jintArray jdata = (*env)->NewIntArray(env, rw * rh);
    if (jdata != NULL) {
        void *data = (*env)->GetPrimitiveArrayCritical(env, jdata, &isCopy);
        if (data != nil) {
            CGColorSpaceRef colorspace = CGColorSpaceCreateDeviceRGB();
            CGContextRef cg = CGBitmapContextCreate(data, rw, rh, 8, rw * 4, colorspace, kCGImageAlphaPremultipliedFirst | kCGBitmapByteOrder32Host);
            CGColorSpaceRelease(colorspace);

            // The following method is deprecated in OS 10.10
            // NSGraphicsContext *ng = [NSGraphicsContext graphicsContextWithGraphicsPort:cg flipped:NO];

            NSGraphicsContext *ng = [NSGraphicsContext graphicsContextWithCGContext:cg flipped:NO];

            CGContextRelease(cg);

            //NSLog(@"Rendering image into %dx%d %fx: %@", w, h, scaleFactor, image);

            NSGraphicsContext *old = [[NSGraphicsContext currentContext] retain];
            [NSGraphicsContext setCurrentContext:ng];

            NSAffineTransform *tr = [NSAffineTransform transform];
            [tr scaleBy: scaleFactor];
            NSDictionary *hints = [NSDictionary dictionaryWithObject:tr forKey:NSImageHintCTM];
            NSRect frame = NSMakeRect(0, 0, w, h);
            NSImageRep *rep = [image bestRepresentationForRect:frame context:nil hints:hints];
            NSRect toRect = NSMakeRect(0, 0, rw, rh);

            //NSLog(@"Rendering image into %dx%d %fx using rep: %@", w, h, scaleFactor, rep);

            [rep drawInRect:toRect];

            [NSGraphicsContext setCurrentContext:old];
            [old release];
            (*env)->ReleasePrimitiveArrayCritical(env, jdata, data, 0);
            return jdata;
        }
    }

    return NULL;
}

// Render an image into a Java array
// rw and rh are the actual size of the raster

static jboolean renderImageIntoBuffers(JNIEnv *env, NSImage *image, jobjectArray joutput, jfloat w, jfloat h)
{
    //NSLog(@"Render image into buffers: %@", image);

    jboolean result = NO;

    jintArray buffer1 = renderImageIntoBufferForDisplay(env, image, w, h, 1);
    jintArray buffer2 = renderImageIntoBufferForDisplay(env, image, w, h, 2);

    if (buffer1) {
        (*env)->SetObjectArrayElement(env, joutput, 0, buffer1);
        (*env)->SetObjectArrayElement(env, joutput, 1, buffer2);
        result = YES;
    }

    return result;
}

typedef long (*QuickLookRequest)(CFAllocatorRef, CFURLRef, CGSize, CFDictionaryRef);

static NSImage *getFileImage(NSString *path, jboolean isQuickLook, jboolean isIconMode, jint w, jint h)
{
    //NSLog(@"getFileImage %d %@", isQuickLook, path);

    NSImage *result = nil;
    if (isQuickLook) {
        NSURL *fileURL = [NSURL fileURLWithPath:path];
        if (fileURL != nil) {
            // Load the QuickLook bundle
            NSURL *bundleURL = [NSURL fileURLWithPath:@"/System/Library/Frameworks/QuickLook.framework"];
            CFBundleRef cfBundle = CFBundleCreate(kCFAllocatorDefault, (CFURLRef)bundleURL);
            // If we didn't succeed, the framework does not exist.
            if (cfBundle) {
                NSDictionary *dict = [NSDictionary dictionaryWithObject:[NSNumber numberWithBool:isIconMode]
                                                                 forKey:@"IconMode"];
                // Get the thumbnail function pointer
                QuickLookRequest functionRef = CFBundleGetFunctionPointerForName(cfBundle,
                                                                                 CFSTR("QLThumbnailImageCreate"));
                if (functionRef) {
                    CGSize size = CGSizeMake(w, h);
                    CGImageRef ref = (CGImageRef) functionRef(kCFAllocatorDefault,
                                                 (CFURLRef)fileURL,
                                                 size,
                                                 (CFDictionaryRef)dict);

                    if (ref) {
                        result = [[[NSImage alloc] initWithCGImage:ref size:size] autorelease];
                        CFRelease(ref);
                    } else {
                        //NSLog(@"No quick look image found");
                    }
                }
            }
        }
    } else {
        result = [[NSWorkspace sharedWorkspace] iconForFile:path];
    }

    //NSLog(@"getFileImage result %@", result);

    return result;
}

/*
 * Class:     org_violetlib_aqua_fc_AquaFileIcons
 * Method:    nativeRenderFileImage
 * Signature: (Ljava/lang/String;ZZ[[III)Z
 */
JNIEXPORT jboolean JNICALL Java_org_violetlib_aqua_fc_AquaFileIcons_nativeRenderFileImage
    (JNIEnv *env, jclass cl, jstring jpath, jboolean isQuickLook, jboolean isIconMode, jobjectArray output, jint w, jint h)
{
    jboolean result = NO;

    COCOA_ENTER();

    NSString *path = TO_NSPATH(jpath);

    NSImage *image = getFileImage(path, isQuickLook, isIconMode, w, h);
    if (image != nil) {
        result = renderImageIntoBuffers(env, image, output, w, h);
    }

    COCOA_EXIT();

    return result;
}

static jobject thumbnailHandler;
static jclass thumbnailHandlerClass;
static jmethodID thumbnailHandlerMethodID;

/*
 * Class:     org_violetlib_aqua_fc_CatalinaFileIconServiceImpl
 * Method:    nativeInstallThumbnailHandler
 * Signature: (Lorg/violetlib/aqua/fc/CatalinaFileIconServiceImpl/MyHandler;)V
 */
JNIEXPORT void JNICALL Java_org_violetlib_aqua_fc_CatalinaFileIconServiceImpl_nativeInstallThumbnailHandler
  (JNIEnv *env, jclass cl, jobject jhandler)
{
    thumbnailHandler = (*env)->NewGlobalRef(env, jhandler);
    if (thumbnailHandler == NULL) {
        NSLog(@"Unable to create global reference to thumbnail handler");
    } else {
        jclass c = (*env)->GetObjectClass(env, thumbnailHandler);
        thumbnailHandlerClass = (*env)->NewGlobalRef(env, c);
        if (thumbnailHandlerClass == NULL) {
            NSLog(@"Unable to create global reference to thumbnail handler class");
        } else {
            thumbnailHandlerMethodID = (*env)->GetMethodID(env, thumbnailHandlerClass, "installImage", "(JII[IFI)V");
            if (thumbnailHandlerMethodID == NULL) {
                NSLog(@"Unable to find thumbnail handler method");
            }
        }
    }
}

/*
 * Class:     org_violetlib_aqua_fc_CatalinaFileIconServiceImpl
 * Method:    isAvailable
 * Signature: ()Z
 */
JNIEXPORT jboolean JNICALL Java_org_violetlib_aqua_fc_CatalinaFileIconServiceImpl_isAvailable
  (JNIEnv *env, jclass cl)
{
    jboolean result = 0;

    if (@available(macOS 10.15, *)) {
        result = 1;
    }

    return result;
}

/*
 * Class:     org_violetlib_aqua_fc_CatalinaFileIconServiceImpl
 * Method:    nativeInstallThumbnails
 * Signature: (Ljava/lang/String;IFJ)V
 */
JNIEXPORT void JNICALL Java_org_violetlib_aqua_fc_CatalinaFileIconServiceImpl_nativeInstallThumbnails
  (JNIEnv *env, jclass cl, jstring jpath, jint jsize, jfloat scale, jlong requestID)
{
    if (thumbnailHandlerMethodID == NULL) {
      return;
    }

    COCOA_ENTER();

    if (@available(macOS 10.15, *)) {
        NSString *path = TO_NSPATH(jpath);
        NSURL *u = [NSURL fileURLWithPath:path];
        if (u != nil) {
            CGSize size = CGSizeMake(jsize, jsize);
            QLThumbnailGenerator *generator = [QLThumbnailGenerator sharedGenerator];
            QLThumbnailGenerationRequest *request
              = [[QLThumbnailGenerationRequest alloc]
                            initWithFileAtURL:u
                                         size:size
                                        scale:scale
                          representationTypes:QLThumbnailGenerationRequestRepresentationTypeAll];

            if (request) {
                NSLog(@"Requesting thumbnails %@ %d %f", path, jsize, scale);
                [generator generateRepresentationsForRequest:request updateHandler:
                    ^(QLThumbnailRepresentation *thumbnail, QLThumbnailRepresentationType type, NSError *error) {
                        if (thumbnail != nil) {
                            NSImage *image = [thumbnail NSImage];
                            NSLog(@"  Thumbnail %ld delivered: %@ (%f x %f)", (long) type, path, image.size.width, image.size.height);
                            jint priority = 0;
                            switch (type) {
                              case QLThumbnailRepresentationTypeIcon: priority = 10; break;
                              case QLThumbnailRepresentationTypeLowQualityThumbnail: priority = 20; break;
                              case QLThumbnailRepresentationTypeThumbnail: priority = 30; break;
                            }
                            runFromNativeThread(^(JNIEnv *env) {
                                // TBD: the following assumes we get what we asked for, may not be true
                                int rasterWidth = (int) (image.size.width * scale);
                                int rasterHeight = (int) (image.size.height * scale);
                                jintArray data = renderImageIntoBufferForDisplay(env, image, image.size.width, image.size.height, scale);
                                if (data != NULL) {
                                    (*env)->CallVoidMethod(env, thumbnailHandler, thumbnailHandlerMethodID, requestID, rasterWidth, rasterHeight, data, scale, priority);
                                } else {
                                    NSLog(@"  Unable to get thumbnail: unable to render image contents");
                                }
                            });
                        } else if (error != nil) {
                            NSLog(@"  Unable to get thumbnail %ld for %@: %@", (long) type, path, error.localizedFailureReason);
                        }
                    }
                ];
            } else {
                NSLog(@"  Unable to create request for thumbnails %@ %d %f", path, jsize, scale);
            }
        }
    }

    COCOA_EXIT();
}

/*
 * Class:     org_violetlib_aqua_AquaImageFactory
 * Method:    nativeRenderImageFile
 * Signature: (Ljava/lang/String;[[III)Z
 */
JNIEXPORT jboolean JNICALL Java_org_violetlib_aqua_AquaImageFactory_nativeRenderImageFile
    (JNIEnv *env, jclass cl, jstring jpath, jobjectArray buffers, jint w, jint h)
{
    jboolean result = NO;

    COCOA_ENTER();

    NSString *path = TO_NSPATH(jpath);
    NSImage *image = [[NSImage alloc] initWithContentsOfFile:path];

    if (image != nil) {
        result = renderImageIntoBuffers(env, image, buffers, w, h);
    }

    COCOA_EXIT();

    return result;
}

/*
 * Class:     org_violetlib_aqua_AquaImageFactory
 * Method:    getNativeImage
 * Signature: (Ljava/lang/String;II)Ljava/awt/Image;
 */
JNIEXPORT jobject JNICALL Java_org_violetlib_aqua_AquaImageFactory_getNativeImage
    (JNIEnv *env, jclass cl, jstring jname, jint w, jint h)
{
    jobject result = NULL;

    static jclass jc_CImage;
    static jclass jc_Creator;
    static jmethodID jm_getCreator;
    static jmethodID jm_createImage;

    GET_CLASS_RETURN(jc_CImage, "sun/lwawt/macosx/CImage", NULL);
    GET_CLASS_RETURN(jc_Creator, "sun/lwawt/macosx/CImage$Creator", NULL);
    GET_STATIC_METHOD_RETURN(jm_getCreator, jc_CImage, "getCreator", "()Lsun/lwawt/macosx/CImage$Creator;", NULL);
    GET_METHOD_RETURN(jm_createImage, jc_Creator, "createImageFromName", "(Ljava/lang/String;II)Ljava/awt/Image;", NULL);

    COCOA_ENTER();

    jobject creator = (*env)->CallStaticObjectMethod(env, jc_CImage, jm_getCreator);
    if (creator != NULL) {
        result = (*env)->CallObjectMethod(env, creator, jm_createImage, jname, w, h);
    }
    CHECK_EXCEPTION();

    COCOA_EXIT();

    return result;
}

/*
 * Class:     org_violetlib_aqua_AquaIcon
 * Method:    nativeRenderIcon
 * Signature: (I[[II)Z
 */
JNIEXPORT jboolean JNICALL Java_org_violetlib_aqua_AquaIcon_nativeRenderIcon
    (JNIEnv *env, jclass cl, jint osType, jobjectArray buffers, jint size)
{
    jboolean result = NO;

    COCOA_ENTER();

    NSImage *image = [[NSWorkspace sharedWorkspace] iconForFileType: NSFileTypeForHFSTypeCode(osType)];

    if (image != nil) {
        result = renderImageIntoBuffers(env, image, buffers, size, size);
    }

    COCOA_EXIT();

    return result;
}

// Many deprecated functions but no replacement as of OS X 10.11
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#pragma GCC warning "Many deprecated functions used here"

/*
 * Class:     org_violetlib_aqua_fc_OSXFile
 * Method:    getBasicItemInfoFlags
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_org_violetlib_aqua_fc_OSXFile_nativeGetBasicItemInfoFlags
    (JNIEnv *env, jclass javaClass, jstring pathJ)
{
    // Assert arguments
    if (pathJ == NULL) return -1;

    // Allocate a memory pool
    NSAutoreleasePool* pool = [NSAutoreleasePool new];

    jint result = 0;

    // Convert Java String to NS String
    const jchar *pathC = (*env)->GetStringChars(env, pathJ, NULL);
    NSString *pathNS = [NSString stringWithCharacters:(UniChar *)pathC length:(*env)->GetStringLength(env, pathJ)];
    (*env)->ReleaseStringChars(env, pathJ, pathC);

    // Do the API calls
    NSURL *u = [NSURL fileURLWithPath:pathNS];
    if (u != nil) {
        OSStatus err;
        LSItemInfoRecord itemInfoRecord;
        err = LSCopyItemInfoForURL((CFURLRef) u, kLSRequestBasicFlagsOnly, &itemInfoRecord);
        if (err == 0) {
            result = itemInfoRecord.flags;
        }
    }

    // Release memory pool
    [pool release];

    // Return the result
    return result;
}

JNIEXPORT jstring JNICALL Java_org_violetlib_aqua_fc_OSXFile_nativeGetDisplayName
    (JNIEnv *env, jclass javaClass, jstring pathJ)
{
    // Assert arguments
    if (pathJ == NULL) return NULL;

    // Allocate a memory pool
    NSAutoreleasePool* pool = [NSAutoreleasePool new];

    // Convert Java String to NS String
    const jchar *pathC = (*env)->GetStringChars(env, pathJ, NULL);
    NSString *pathNS = [NSString stringWithCharacters:(UniChar *)pathC
        length:(*env)->GetStringLength(env, pathJ)];
    (*env)->ReleaseStringChars(env, pathJ, pathC);

    // Do the API calls
    NSFileManager *fileManagerNS = [NSFileManager defaultManager];
    NSString *displayNameNS = [fileManagerNS displayNameAtPath: pathNS];

    // Convert NSString to jstring
    jstring displayNameJ = (*env)->NewStringUTF(env, [displayNameNS UTF8String]);

    // Release memory pool
    [pool release];

    // Return the result
    return displayNameJ;
}

JNIEXPORT jstring JNICALL Java_org_violetlib_aqua_fc_OSXFile_nativeGetFileUTI
    (JNIEnv *env, jclass javaClass, jstring jpath)
{
    jstring result = NULL;

    COCOA_ENTER();

    if (jpath != NULL) {
        NSString *path = TO_NSPATH(jpath);
        NSURL *u = [NSURL fileURLWithPath:path];
        NSString *type = nil;
        if ([u getResourceValue:&type forKey:NSURLTypeIdentifierKey error:nil]) {
            result = (*env)->NewStringUTF(env, [type UTF8String]);
        }
    }

    COCOA_EXIT();

    return result;
}

/*
 * Class:     org_violetlib_aqua_fc_OSXFile
 * Method:    nativeGetLastUsedDate
 * Signature: (Ljava/lang/String;)Z
 */
JNIEXPORT jlong JNICALL Java_org_violetlib_aqua_fc_OSXFile_nativeGetLastUsedDate
    (JNIEnv *env, jclass javaClass, jstring pathJ)
{
    // Assert arguments
    if (pathJ == NULL) return 0;

    // Allocate a memory pool
    NSAutoreleasePool* pool = [NSAutoreleasePool new];

    // Convert Java String to NS String
    const jchar *pathC = (*env)->GetStringChars(env, pathJ, NULL);
    NSString *pathNS = [NSString stringWithCharacters:(UniChar *)pathC
        length:(*env)->GetStringLength(env, pathJ)];
    (*env)->ReleaseStringChars(env, pathJ, pathC);

    jlong result = 0;

    // Do the API calls
    NSURL *u = [NSURL fileURLWithPath:pathNS];
    if (u != nil) {
        MDItemRef item = MDItemCreateWithURL(NULL, CFBridgingRetain(u));
        if (item != NULL) {
            CFDateRef date = (CFDateRef) MDItemCopyAttribute(item, kMDItemLastUsedDate);
            if (date != NULL) {
                CFAbsoluteTime /* double */ at = CFDateGetAbsoluteTime(date);    /* seconds since Jan 1 2001 */
                long long jtime = (long long) at;
                jtime += (60 * 60 * 24) * (31 * 365 + 8);
                jtime *= 1000;
                result = (jlong) jtime;
            }
        }
    }

    // Release memory pool
    [pool release];

    // Return the result
    return result;
}

/*
 * Class:     org_violetlib_aqua_fc_OSXFile
 * Method:    nativeExecuteSavedSearch
 * Signature: (Ljava/lang/String)[Ljava/lang/String
 */
JNIEXPORT jobjectArray JNICALL Java_org_violetlib_aqua_fc_OSXFile_nativeExecuteSavedSearch
    (JNIEnv *env, jclass javaClass, jstring pathJ)
{
    // Assert arguments
    if (pathJ == NULL) return NULL;

    // Prepare result
    jobjectArray result = NULL;

    // Allocate a memory pool
    NSAutoreleasePool* pool = [NSAutoreleasePool new];

    // Convert Java String to NS String
    const jchar *pathC = (*env)->GetStringChars(env, pathJ, NULL);
    NSString *pathNS = [NSString stringWithCharacters:(UniChar *)pathC
                                               length:(*env)->GetStringLength(env, pathJ)];
    (*env)->ReleaseStringChars(env, pathJ, pathC);

    // Read the saved search file and execute the query synchronously
    NSData *data = [NSData dataWithContentsOfFile:pathNS];
    if (data != nil) {
        NSPropertyListReadOptions readOptions = NSPropertyListImmutable;
        NSError *error;
        NSDictionary *plist = (NSDictionary *)[NSPropertyListSerialization propertyListWithData:data options:readOptions format:NULL error:&error];
        if (plist != nil) {
            NSString *queryString = (NSString *) [plist objectForKey:@"RawQuery"];
            NSDictionary *searchCriteria = (NSDictionary *) [plist objectForKey:@"SearchCriteria"];
            if (queryString != nil && searchCriteria != nil) {
                NSArray *scopeDirectories = (NSArray *) [searchCriteria objectForKey:@"FXScopeArrayOfPaths"];
                if (scopeDirectories != nil) {
                    MDQueryRef query = MDQueryCreate(NULL, CFBridgingRetain(queryString), NULL, NULL);
                    if (query != NULL) {
                        OptionBits scopeOptions = 0;
                        MDQuerySetSearchScope(query, CFBridgingRetain(scopeDirectories), scopeOptions);
                        CFOptionFlags optionFlags = kMDQuerySynchronous;
                        Boolean b = MDQueryExecute(query, optionFlags);
                        if (b) {
                            CFIndex count = MDQueryGetResultCount(query);
                            jclass stringClass = (*env)->FindClass(env, "java/lang/String");
                            result = (*env)->NewObjectArray(env, count, stringClass, NULL);
                            for (CFIndex i = 0; i < count; i++) {
                                MDItemRef item = (MDItemRef) MDQueryGetResultAtIndex(query, i);
                                CFStringRef path = (CFStringRef) MDItemCopyAttribute(item, kMDItemPath);
                                NSString *pathNS = (NSString *) path;
                                jstring pathJ = (*env)->NewStringUTF(env, [pathNS UTF8String]);
                                (*env)->SetObjectArrayElement(env, result, i, pathJ);
                            }
                        }
                    }
                }
            }
        }
    }

    // Release memory pool
    [pool release];

    return result;
}

/*
 * Class:     org_violetlib_aqua_fc_OSXFile
 * Method:    nativeGetSidebarFiles
 * Signature: (I)[Ljava/lang/String;
 */
JNIEXPORT jobjectArray JNICALL Java_org_violetlib_aqua_fc_OSXFile_nativeGetSidebarFiles
    (JNIEnv *env, jclass cl, jint which, jint iconSize, jint lastSeed)
{
    CFStringRef listID = which > 0 ? kLSSharedFileListFavoriteVolumes : kLSSharedFileListFavoriteItems;

    LSSharedFileListRef list = LSSharedFileListCreate(NULL, listID, NULL);
    if (!list) {
        NSLog(@"Failed to create shared file list for %@", listID);
        return NULL;
    }

    UInt32 seed = LSSharedFileListGetSeedValue(list);
    if (seed == lastSeed) {
        CFRelease(list);
        return NULL;
    }

    CFArrayRef items = LSSharedFileListCopySnapshot(list, &seed);
    size_t count = CFArrayGetCount(items);

    jclass objectClass = (*env)->FindClass(env, "java/lang/Object");
    jclass integerClass = (*env)->FindClass(env, "java/lang/Integer");
    jmethodID newIntegerMethodID = (*env)->GetMethodID(env, integerClass, "<init>", "(I)V");

    jobjectArray result = (*env)->NewObjectArray(env, 1 + count * 6, objectClass, NULL);
    size_t j = 0;
    (*env)->SetObjectArrayElement(env, result, j++, (*env)->NewObject(env, integerClass, newIntegerMethodID, seed));

    if (which >= 2) {    // testing
        //NSLog(@"%ld elements for %@", count, list);
    }

    if (count > 0) {
        for (size_t i = 0; i < count; i++) {
            LSSharedFileListItemRef item = (LSSharedFileListItemRef) CFArrayGetValueAtIndex(items, i);
            if (!item) {
                continue;
            }

            // Collect six elements: display name, UID, hidden flag, resolved path, 1x rendering, 2x rendering

            jstring displayNameJ = NULL;

            CFStringRef displayName = LSSharedFileListItemCopyDisplayName(item);
            NSString *displayNameNS = (NSString *) displayName;
            if (displayNameNS) {
                displayNameJ = (*env)->NewStringUTF(env, [displayNameNS UTF8String]);
                CFRelease(displayName);
            }

            UInt32 itemId = LSSharedFileListItemGetID(item);
            jobject itemIdJ = (*env)->NewObject(env, integerClass, newIntegerMethodID, itemId);

            CFTypeRef hiddenProperty = LSSharedFileListItemCopyProperty(item, kLSSharedFileListItemHidden);
            jint hiddenFlag = hiddenProperty && hiddenProperty == kCFBooleanTrue;
            if (hiddenProperty) {
                CFRelease(hiddenProperty);
            }
            jobject flagsJ = (*env)->NewObject(env, integerClass, newIntegerMethodID, hiddenFlag);

            jstring pathJ = NULL;
            CFURLRef outURL = LSSharedFileListItemCopyResolvedURL(item, kLSSharedFileListNoUserInteraction|kLSSharedFileListDoNotMountVolumes, NULL);
            if (outURL) {
                CFStringRef itemPath = CFURLCopyFileSystemPath(outURL, kCFURLPOSIXPathStyle);
                if (itemPath) {
                    NSString *pathNS = (NSString *) itemPath;
                    pathJ = (*env)->NewStringUTF(env, [pathNS UTF8String]);
                    CFRelease(itemPath);
                }
                CFRelease(outURL);
            }

            jobject icon1J = NULL;
            jobject icon2J = NULL;

            if (iconSize > 0) {
                IconRef icon = LSSharedFileListItemCopyIconRef(item);
                if (icon) {
                    // Workaround for apparent bug in macOS 14
                    long long ptr = (long long) icon;
                    if (ptr > 0 && ptr < 10000) {
                        CFStringRef displayName = LSSharedFileListItemCopyDisplayName(item);
                        NSString *displayNameNS = (NSString *) displayName;
                        if (displayNameNS) {
                            NSLog(@"Bad pointer %llx returned by LSSharedFileListItemCopyIconRef for %@", ptr, displayNameNS);
                            CFRelease(displayName);
                        } else {
                            NSLog(@"Bad pointer %llx returned by LSSharedFileListItemCopyIconRef for item with no name", ptr);
                        }
                    } else {
                        NSImage *iconImage = [[NSImage alloc] initWithIconRef:icon];
                        icon1J = renderImageIntoBufferForDisplay(env, iconImage, iconSize, iconSize, 1);
                        icon2J = renderImageIntoBufferForDisplay(env, iconImage, iconSize, iconSize, 2);
                        [iconImage release];
                        CFRelease(icon);
                    }
                }
            }

            (*env)->SetObjectArrayElement(env, result, j++, displayNameJ);
            (*env)->SetObjectArrayElement(env, result, j++, itemIdJ);
            (*env)->SetObjectArrayElement(env, result, j++, flagsJ);
            (*env)->SetObjectArrayElement(env, result, j++, pathJ);
            (*env)->SetObjectArrayElement(env, result, j++, icon1J);
            (*env)->SetObjectArrayElement(env, result, j++, icon2J);
        }
    }

    CFRelease(items);
    CFRelease(list);
    return result;
}

#pragma GCC diagnostic pop

static NSColorPanel *colorPanel;
static jobject colorPanelCallback;
static jboolean colorPanelBeingConfigured;

@interface MyColorPanelDelegate : NSObject <NSWindowDelegate> {}
- (void) colorChanged: (id) sender;
@end

@implementation MyColorPanelDelegate

- (void) windowWillClose:(NSNotification *) ns
{
    assert(vm);

    runFromNativeThread(^(JNIEnv *env) {
        // Using dynamic lookup because we do not know which class loader was used
        jclass cl = (*env)->GetObjectClass(env, colorPanelCallback);
        jmethodID m = (*env)->GetMethodID(env, cl, "disconnected", "()V");
        if (m != NULL) {
            (*env)->CallVoidMethod(env, colorPanelCallback, m);
        } else {
            NSLog(@"Unable to invoke callback -- disconnected method not found");
        }
    });
}

- (void) colorChanged: (id) sender
{
    if (colorPanelBeingConfigured) {
        return;
    }

    NSColor *color = [colorPanel color];
    runFromNativeThread(^(JNIEnv *env) {
        static jclass jc_Color;
        static jmethodID jm_createColor;
        GET_CLASS(jc_Color, "java/awt/Color");
        GET_METHOD(jm_createColor, jc_Color, "<init>", "(FFFF)V");
        CGFloat r, g, b, a;
        [color getRed:&r green:&g blue:&b alpha:&a];
        jobject jColor = (*env)->CallStaticObjectMethod(env, jc_Color, jm_createColor, r, g, b, a);
        CHECK_EXCEPTION();
        // Using dynamic lookup because we do not know which class loader was used
        jclass cl = (*env)->GetObjectClass(env, colorPanelCallback);
        jmethodID m = (*env)->GetMethodID(env, cl, "applyColor", "(Ljava/awt/Color;)V");
        if (m != NULL) {
            (*env)->CallVoidMethod(env, colorPanelCallback, m, jColor);
        } else {
            NSLog(@"Unable to invoke callback -- applyColor method not found");
        }
    });
}

@end

static jboolean setupColorPanel()
{
    MyColorPanelDelegate *delegate = [[MyColorPanelDelegate alloc] init];
    colorPanel = [NSColorPanel sharedColorPanel];
    [colorPanel setDelegate: delegate];
    [colorPanel setAction: @selector(colorChanged:)];
    [colorPanel setTarget: delegate];
    [colorPanel setContinuous: YES];
    [colorPanel makeKeyAndOrderFront: nil];
    [colorPanel setReleasedWhenClosed: NO];
    return YES;
}

/*
 * Class:     org_violetlib_aqua_AquaNativeColorChooser
 * Method:    display
 * Signature: (Lorg/violetlib/aqua/AquaSharedColorChooser/Owner;)Z
 */
JNIEXPORT jboolean JNICALL Java_org_violetlib_aqua_AquaNativeColorChooser_create
    (JNIEnv *env, jclass cl, jobject ownerCallback)
{
    colorPanelCallback = (*env)->NewGlobalRef(env, ownerCallback);
    if (colorPanelCallback == NULL) {
        return NO;
    }

    __block jboolean result = NO;

    COCOA_ENTER();

    if (ensureVM(env)) {
        runOnMainThread(^(){
            result = setupColorPanel();
        });
    }

    COCOA_EXIT();

    return result;
}

/*
 * Class:     org_violetlib_aqua_AquaNativeColorChooser
 * Method:    show
 * Signature: (FFFFZ)V
 */
JNIEXPORT void JNICALL Java_org_violetlib_aqua_AquaNativeColorChooser_show
    (JNIEnv *env, jclass cl, jfloat red, jfloat green, jfloat blue, jfloat alpha, jboolean wantAlpha)
{
    if (colorPanel) {
        COCOA_ENTER();

        runOnMainThread(^(){
                colorPanelBeingConfigured = YES;
                        NSColor *color = [NSColor colorWithSRGBRed:(CGFloat)red
                                                 green:(CGFloat)green
                                                  blue:(CGFloat)blue
                                                 alpha:(CGFloat)alpha];
            colorPanel.showsAlpha = wantAlpha;
            colorPanel.color = color;
            [colorPanel makeKeyAndOrderFront: nil];
            colorPanelBeingConfigured = NO;
        });

        COCOA_EXIT();
    }
}

/*
 * Class:     org_violetlib_aqua_AquaNativeColorChooser
 * Method:    hide
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_org_violetlib_aqua_AquaNativeColorChooser_hide(JNIEnv *env, jclass cl)
{
    if (colorPanel) {
        COCOA_ENTER();

        runOnMainThread(^(){
            [colorPanel close];
        });

        COCOA_EXIT();
    }
}

static void internalDeliverWindowChangedAppearance(JNIEnv *env, NSWindow *window, NSAppearance *appearance)
{
    if (windowChangedAppearanceCallback == nil) {
        return;
    }

    NSLog(@"Deliver window change appearance called on %@ %@", window, appearance.name);

    jobject jWindow = getJavaWindow(env, window);
    if (jWindow) {
        // Using dynamic lookup because we do not know which class loader was used
        jclass cl = (*env)->GetObjectClass(env, windowChangedAppearanceCallback);
        jmethodID m = (*env)->GetMethodID(env, cl, "windowAppearanceChanged", "(Ljava/awt/Window;Ljava/lang/String;)V");
        if (m != NULL) {
            NSString *appearanceName = appearance.name;
            jobject jAppearanceName = (*env)->NewStringUTF(env, [appearanceName UTF8String]);
            (*env)->CallVoidMethod(env, windowChangedAppearanceCallback, m, jWindow, jAppearanceName);
        } else {
            NSLog(@"Unable to invoke callback -- windowAppearanceChanged method not found");
        }
    } else {
        NSLog(@"Unable to invoke callback -- Java window not found");
    }
}

void deliverWindowChangedAppearance(NSWindow *window, NSAppearance *appearance)
{
    if (windowChangedAppearanceCallback == nil) {
        NSLog(@"No callback for window changed appearance");
        return;
    }

    assert(vm);

    appearance = [appearance retain];

    runFromNativeThread(^(JNIEnv *env) {
        internalDeliverWindowChangedAppearance(env, window, appearance);
        [appearance release];
    });
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    registerWindowChangedAppearanceCallback
 * Signature: (Lorg/violetlib/aqua/AquaUtils/WindowChangedAppearanceCallback;)V
 */
JNIEXPORT void JNICALL Java_org_violetlib_aqua_AquaUtils_registerWindowChangedAppearanceCallback
  (JNIEnv *env, jclass cl, jobject callback)
{
    if (ensureVM(env)) {
        windowChangedAppearanceCallback = (*env)->NewGlobalRef(env, callback);
    }
}

static jclass jc_Window;
static jclass jc_LWWindowPeer;
static jclass jc_CPlatformWindow;
static jclass jc_CFRetainedResource;
static jclass jc_CViewEmbeddedFrame;
static jclass jc_Frame;
static jclass jc_Dialog;
static jclass jc_Color;
static jclass jc_Component;
static jclass jc_SwingUtilities2;
static jclass jc_HeavyWeightPopup;
static jclass jc_AquaMenuBarUI;
static jclass jc_LWCToolkit;
static jclass jc_JComponent;

static jobject getWindowPeer(JNIEnv *env, jobject w)
{
    static jfieldID jf_peer;

    GET_CLASS_RETURN(jc_Window, "java/awt/Window", NULL);
    GET_FIELD_RETURN(jf_peer, jc_Window, "peer", "Ljava/awt/peer/ComponentPeer;", NULL);
    jobject peer = (*env)->GetObjectField(env, w, jf_peer);
    CHECK_EXCEPTION();
    return peer;
}

static jobject getPlatformWindow(JNIEnv *env, jobject windowPeer)
{
    static jmethodID jm_getPlatformWindow;

    GET_CLASS_RETURN(jc_LWWindowPeer, "sun/lwawt/LWWindowPeer", NULL);
    GET_METHOD_RETURN(jm_getPlatformWindow, jc_LWWindowPeer, "getPlatformWindow", "()Lsun/lwawt/PlatformWindow;", NULL);
    return windowPeer != NULL ? (*env)->CallObjectMethod(env, windowPeer, jm_getPlatformWindow) : NULL;
}

static NSWindow *getNativeWindowFromPlatformWindow(JNIEnv *env, jobject platformWindow, jobject *readLockOutput)
{
    static jfieldID jf_ptr;
    static jfieldID jf_readLock;

    GET_CLASS_RETURN(jc_CPlatformWindow, "sun/lwawt/macosx/CPlatformWindow", nil);
    GET_CLASS_RETURN(jc_CFRetainedResource, "sun/lwawt/macosx/CFRetainedResource", nil);
    GET_FIELD_RETURN(jf_ptr, jc_CFRetainedResource, "ptr", "J", nil);
    GET_FIELD_RETURN(jf_readLock, jc_CFRetainedResource, "readLock", "Ljava/util/concurrent/locks/Lock;", nil);

    *readLockOutput = NULL;

    // Check for the normal case (CPlatformWindow)
    if ((*env)->IsInstanceOf(env, platformWindow, jc_CPlatformWindow)) {
        jlong ptr = (*env)->GetLongField(env, platformWindow, jf_ptr);
        if (ptr != 0) {
            *readLockOutput = (*env)->GetObjectField(env, platformWindow, jf_readLock);
        }
        CHECK_EXCEPTION();
        return (NSWindow *) ptr;
    }

    NSLog(@"Unsupported platform window");
    return NULL;
}

static NSWindow *getNativeWindow(JNIEnv *env, jobject w, jobject *readLockOutput)
{
    static jmethodID jm_getEmbedderHandle;

    GET_CLASS_RETURN(jc_CViewEmbeddedFrame, "sun/lwawt/macosx/CViewEmbeddedFrame", nil);
    GET_METHOD_RETURN(jm_getEmbedderHandle, jc_CViewEmbeddedFrame, "getEmbedderHandle", "()J", nil);

    *readLockOutput = NULL;

    // Check for an embedded frame (CViewEmbeddedFrame)
    if ((*env)->IsInstanceOf(env, w, jc_CViewEmbeddedFrame)) {
        NSView *v = (NSView *) (*env)->CallLongMethod(env, w, jm_getEmbedderHandle);
        NSLog(@"nativeGetNativeWindow: obtaining native window from embedded frame: %@", v);
        return v != nil ? v.window : NULL;
    }

    NSWindow *result = 0;
    jobject peer = getWindowPeer(env, w);
    if (peer != NULL) {
        jobject platformWindow = getPlatformWindow(env, peer);
        if (platformWindow != NULL) {
            result = getNativeWindowFromPlatformWindow(env, platformWindow, readLockOutput);
            if (result == NULL) {
                NSLog(@"nativeGetNativeWindow: No pointer");
            }
        } else {
            NSLog(@"nativeGetNativeWindow: No platform window");
        }
    } else {
        NSLog(@"nativeGetNativeWindow: No window peer");
    }
    return result;
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    nativeGetNativeWindow
 * Signature: (Ljava/awt/Window;[Ljava/lang/Object;)J
 */
JNIEXPORT jlong JNICALL Java_org_violetlib_aqua_AquaUtils_nativeGetNativeWindow
  (JNIEnv *env, jclass cl, jobject w, jobjectArray data)
{
    jlong result = 0;
    jobject readLock = NULL;

    COCOA_ENTER();

    result = (jlong) getNativeWindow(env, w, &readLock);

    (*env)->SetObjectArrayElement(env, data, 0, readLock);

    COCOA_EXIT();

    return result;
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    nativeSetTitledWindowStyle
 * Signature: (Ljava/awt/Window;ZLjava/awt/Insets;)V
 */
JNIEXPORT void JNICALL Java_org_violetlib_aqua_AquaUtils_nativeSetTitledWindowStyle
  (JNIEnv *env, jclass cl, jobject w, jboolean isDecorated, jobject insets)
{
    static jmethodID jm_setStyleBits;
    static jmethodID jm_updateInsets;
    static jfieldID jf_frameUndecorated;
    static jfieldID jf_dialogUndecorated;

    GET_CLASS(jc_CPlatformWindow, "sun/lwawt/macosx/CPlatformWindow");
    GET_METHOD(jm_setStyleBits, jc_CPlatformWindow, "setStyleBits", "(IZ)V");
    GET_CLASS(jc_LWWindowPeer, "sun/lwawt/LWWindowPeer");
    GET_METHOD(jm_updateInsets, jc_LWWindowPeer, "updateInsets", "(Ljava/awt/Insets;)Z");
    GET_CLASS(jc_Frame, "java/awt/Frame");
    GET_FIELD(jf_frameUndecorated, jc_Frame, "undecorated", "Z");
    GET_CLASS(jc_Dialog, "java/awt/Dialog");
    GET_FIELD(jf_dialogUndecorated, jc_Dialog, "undecorated", "Z");

    COCOA_ENTER();

    jobject peer = getWindowPeer(env, w);
    jobject platformWindow = getPlatformWindow(env, peer);
    if (platformWindow == NULL) {
        return;
    }
    int DECORATED = 1 << 1;
    (*env)->CallVoidMethod(env, platformWindow, jm_setStyleBits, DECORATED, isDecorated);
    CHECK_EXCEPTION();

    // Java eventually will be informed of the new window insets, but we need to update now so
    // that the initial painting of the root pane will be positioned correctly.

    (*env)->CallBooleanMethod(env, peer, jm_updateInsets, insets);
    CHECK_EXCEPTION();

    if ((*env)->IsInstanceOf(env, w, jc_Frame)) {
        (*env)->SetBooleanField(env, w, jf_frameUndecorated, !isDecorated);
    } else if ((*env)->IsInstanceOf(env, w, jc_Dialog)) {
        (*env)->SetBooleanField(env, w, jf_dialogUndecorated, !isDecorated);
    }
    CHECK_EXCEPTION();

    COCOA_EXIT();
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    nativeSetWindowTextured
 * Signature: (Ljava/awt/Window;Z)V
 */
JNIEXPORT void JNICALL Java_org_violetlib_aqua_AquaUtils_nativeSetWindowTextured
  (JNIEnv *env, jclass cl, jobject w, jboolean isTextured)
{
    static jmethodID jm_setTextured;
    static jmethodID jm_isTextured;
    static jmethodID jm_setOpaque;
    static jfieldID jf_isOpaque;

    GET_CLASS(jc_LWWindowPeer, "sun/lwawt/LWWindowPeer");
    GET_METHOD(jm_setTextured, jc_LWWindowPeer, "setTextured", "(Z)V");
    GET_METHOD(jm_isTextured, jc_LWWindowPeer, "isTextured", "()Z");
    GET_METHOD(jm_setOpaque, jc_LWWindowPeer, "setOpaque", "(Z)V");
    GET_FIELD(jf_isOpaque, jc_LWWindowPeer, "isOpaque", "Z");

    COCOA_ENTER();

    jobject peer = getWindowPeer(env, w);
    if (peer != nil) {
        jboolean currentTextured = (*env)->CallBooleanMethod(env, peer, jm_isTextured);
        CHECK_EXCEPTION();
        if (isTextured != currentTextured) {
            (*env)->CallVoidMethod(env, peer, jm_setTextured, isTextured);
            CHECK_EXCEPTION();
            // the setTextured method fails to update the surface, but setOpaque does
            jboolean isOpaque = (*env)->GetBooleanField(env, peer, jf_isOpaque);
            CHECK_EXCEPTION();
            (*env)->SetBooleanField(env, peer, jf_isOpaque, !isOpaque);
            CHECK_EXCEPTION();
            (*env)->CallVoidMethod(env, peer, jm_setOpaque, isOpaque);
            CHECK_EXCEPTION();
        }
    }

    COCOA_EXIT();
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    nativeSetWindowResizable
 * Signature: (Ljava/awt/Window;Z)V
 */
JNIEXPORT void JNICALL Java_org_violetlib_aqua_AquaUtils_nativeSetWindowResizable
  (JNIEnv *env, jclass cl, jobject w, jboolean isResizable)
{
    static jmethodID jm_setResizable;

    GET_CLASS(jc_LWWindowPeer, "sun/lwawt/LWWindowPeer");
    GET_METHOD(jm_setResizable, jc_LWWindowPeer, "setResizable", "(Z)V");

    COCOA_ENTER();

    jobject peer = getWindowPeer(env, w);
    if (peer != nil) {
        (*env)->CallVoidMethod(env, peer, jm_setResizable, isResizable);
        CHECK_EXCEPTION();
    }

    COCOA_EXIT();
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    nativeSetWindowBackground
 * Signature: (Ljava/awt/Window;Ljava/awt/Color;)V
 */
JNIEXPORT void JNICALL Java_org_violetlib_aqua_AquaUtils_nativeSetWindowBackground
  (JNIEnv *env, jclass cl, jobject w, jobject color)
{
    static jmethodID jm_setBackground;
    static jmethodID jm_setOpaque;
    static jmethodID jm_getAlpha;
    static jfieldID jf_background;

    GET_CLASS(jc_LWWindowPeer, "sun/lwawt/LWWindowPeer");
    GET_METHOD(jm_setBackground, jc_LWWindowPeer, "setBackground", "(Ljava/awt/Color;)V");
    GET_METHOD(jm_setOpaque, jc_LWWindowPeer, "setOpaque", "(Z)V");
    GET_CLASS(jc_Color, "java/awt/Color");
    GET_METHOD(jm_getAlpha, jc_Color, "getAlpha", "()I");
    GET_CLASS(jc_Component, "java/awt/Component");
    GET_FIELD(jf_background, jc_Component, "background", "Ljava/awt/Color;");

    COCOA_ENTER();

    jobject peer = getWindowPeer(env, w);
    if (peer != nil) {
        (*env)->CallVoidMethod(env, peer, jm_setBackground, color);
        CHECK_EXCEPTION();
        int alpha = (*env)->CallIntMethod(env, color, jm_getAlpha);
        CHECK_EXCEPTION();
        (*env)->CallVoidMethod(env, peer, jm_setOpaque, alpha == 255);
        CHECK_EXCEPTION();
    }

    (*env)->SetObjectField(env, w, jf_background, color);
    CHECK_EXCEPTION();

    COCOA_EXIT();
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    nativeSetWindowRepresentedFilename
 * Signature: (JLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_org_violetlib_aqua_AquaUtils_nativeSetWindowRepresentedFilename
  (JNIEnv *env, jclass cl, jlong wptr, jstring jFilename)
{
    jint result = -1;

    COCOA_ENTER();

    NSWindow *w = (NSWindow *) wptr;
    if (jFilename) {
        NSString *filename = TO_NSSTRING(jFilename);
        runOnMainThread(^() {
            w.representedFilename = filename;
        });
        result = 0;
    }

    COCOA_EXIT();
    return result;
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    nativeIsFullScreenWindow
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL Java_org_violetlib_aqua_AquaUtils_nativeIsFullScreenWindow
  (JNIEnv *env, jclass cl, jlong wptr)
{
    jboolean result = 0;

    COCOA_ENTER();

    NSWindow *w = (NSWindow *) wptr;
    NSUInteger mask = [w styleMask];
    if (mask & NSWindowStyleMaskFullScreen) {
        result = 1;
    }

    COCOA_EXIT();

    return result;
}

static const jint TITLEBAR_NONE = 0;
static const jint TITLEBAR_ORDINARY = 1;
static const jint TITLEBAR_TRANSPARENT = 2;
static const jint TITLEBAR_HIDDEN = 3;
static const jint TITLEBAR_OVERLAY = 4;

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    nativeSetTitleBarStyle
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL Java_org_violetlib_aqua_AquaUtils_nativeSetTitleBarStyle
    (JNIEnv *env, jclass cl, jlong wptr, jint style)
{
    // This method uses API introduced in Yosemite

    jint result = -1;

    COCOA_ENTER();

    NSWindow *w = (NSWindow *) wptr;
    if ([w respondsToSelector: @selector(setTitlebarAppearsTransparent:)]) {
        runOnMainThread(^() {

            NSProcessInfo *pi = [NSProcessInfo processInfo];
            NSOperatingSystemVersion osv = [pi operatingSystemVersion];
            BOOL isElCapitan = osv.majorVersion >= 10 && osv.minorVersion >= 11;

            // Because this method is used by component UIs, it updates the same set of properties regardless of the
            // style. It never does a partial update.

            // We need to make the window Movable if we want the user to be able to drag the window from the window
            // title, which we do when the title bar is transparent.

            // On Yosemite, if the window is not Movable, mouse events over the title bar never get to the Java window.
            // If we want some control over title bar mouse events (which we do when the title bar is hidden), we must
            // make the window Movable.

            NSUInteger originalStyleMask = w.styleMask;
            NSUInteger styleMask = originalStyleMask;
            BOOL isTextured = (styleMask & NSWindowStyleMaskTexturedBackground) != 0;
            BOOL isMovable = true;
            BOOL isMovableByBackground = isTextured;
            BOOL isTransparent = NO;
            BOOL isHidden = NO;
            BOOL isFixNeeded = NO;

            switch (style) {
                case TITLEBAR_NONE:
                    styleMask &= ~(NSWindowStyleMaskTitled | NSWindowStyleMaskFullSizeContentView);
                    break;
                case TITLEBAR_ORDINARY:
                default:
                    styleMask |= NSWindowStyleMaskTitled;
                    styleMask &= ~NSWindowStyleMaskFullSizeContentView;
                    break;
                case TITLEBAR_TRANSPARENT:
                    styleMask |= (NSWindowStyleMaskTitled | NSWindowStyleMaskFullSizeContentView);
                    isTransparent = YES;
                    isMovableByBackground = NO;
                    isFixNeeded = YES;
                    break;
                case TITLEBAR_HIDDEN:
                    styleMask |= (NSWindowStyleMaskTitled | NSWindowStyleMaskFullSizeContentView);
                    isTransparent = YES;
                    isHidden = YES;
                    isMovable = !isElCapitan;
                    isMovableByBackground = NO;
                    isFixNeeded = YES;
                    break;
                case TITLEBAR_OVERLAY:
                    styleMask |= (NSWindowStyleMaskTitled | NSWindowStyleMaskFullSizeContentView);
                    isFixNeeded = YES;
                    break;
                }

            [[w standardWindowButton:NSWindowCloseButton] setHidden:isHidden];
            [[w standardWindowButton:NSWindowMiniaturizeButton] setHidden:isHidden];
            [[w standardWindowButton:NSWindowZoomButton] setHidden:isHidden];

            [w setTitlebarAppearsTransparent: isTransparent];

            if (((originalStyleMask ^ styleMask) & NSWindowStyleMaskFullSizeContentView) != 0) {
                // The full size content view option has changed.
                // The content view must be resized first, otherwise the window will be resized to fit the existing
                // content view.
                NSRect frame = w.frame;
                NSRect screenContentRect = [NSWindow contentRectForFrameRect:frame styleMask:styleMask];
                NSRect contentFrame = NSMakeRect(screenContentRect.origin.x - frame.origin.x,
                    screenContentRect.origin.y - frame.origin.y,
                    screenContentRect.size.width,
                    screenContentRect.size.height);
                w.contentView.frame = contentFrame;
            }

            if ([w respondsToSelector: @selector(setStyleMaskOverride:)]) {
                [w setStyleMaskOverride: styleMask];
            } else {
                [w setStyleMask: styleMask];
            }

            [w setMovableByWindowBackground:isMovableByBackground];
            [w setMovable:isMovable];

            if (isFixNeeded) {
                // Workaround for a mysterious problem observed in some circumstances but not others.
                // The corner radius is not set, so painting happens outside the rounded corners.
                NSView *topView = getTopView(w);
                if (topView != nil) {
                    CALayer *layer = [topView layer];
                    if (layer != nil) {
                        CGFloat radius = [layer cornerRadius];
                        if (radius == 0) {
                            // debug
                            // NSLog(@"Fixing corner radius of %@", layer);
                            [layer setCornerRadius: 6];
                        }
                    } else {
                        NSLog(@"Unable to fix corner radius: no layer");
                    }
                } else {
                    NSLog(@"Unable to fix corner radius: did not find top view");
                }
            }

            if (((originalStyleMask ^ styleMask) & NSWindowStyleMaskFullSizeContentView) != 0) {
                // The full size content view option has changed.
                // We need to get Java to recompute the window insets.
                // This should do it...

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wnonnull"
                [((id)w.delegate) windowDidResize:nil];
#pragma GCC diagnostic pop
            }
        });
        result = 0;
    }

    COCOA_EXIT();

    return result;
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    nativeSetTitleBarProperties
 * Signature: (JZZZ)I
 */
JNIEXPORT jint JNICALL Java_org_violetlib_aqua_AquaUtils_nativeSetTitleBarProperties
  (JNIEnv *env, jclass cl, jlong wptr, jboolean hasTitleBar, jboolean isMovable, jboolean isHidden, jboolean isFixNeeded)
{
    __block jint result = -1;

    static jmethodID jm_setStyleBits;

    GET_CLASS_RETURN(jc_CPlatformWindow, "sun/lwawt/macosx/CPlatformWindow", -1);
    GET_METHOD_RETURN(jm_setStyleBits, jc_CPlatformWindow, "setStyleBits", "(IZ)V", -1);

    COCOA_ENTER();

    NSWindow *w = (NSWindow *) wptr;
    runOnMainThread(^() {

        // Update the titled window style bit using the UNDECORATED style bit of CPlatformWindow.

        BOOL isWindowTitled = (w.styleMask & NSWindowStyleMaskTitled) != 0;
        if (isWindowTitled != hasTitleBar) {
            jobject jPlatformWindow = getJavaPlatformWindow(env, w);
            if (jPlatformWindow) {
                int DECORATED = 1 << 1;
                (*env)->CallVoidMethod(env, jPlatformWindow, jm_setStyleBits, DECORATED, hasTitleBar);
                CHECK_EXCEPTION();
            }
        }

        [w setMovable:isMovable];

        [[w standardWindowButton:NSWindowCloseButton] setHidden:isHidden];
        [[w standardWindowButton:NSWindowMiniaturizeButton] setHidden:isHidden];
        [[w standardWindowButton:NSWindowZoomButton] setHidden:isHidden];

        if (isFixNeeded) {
            // Workaround for a mysterious problem observed in some circumstances but not others.
            // The corner radius is not set, so painting happens outside the rounded corners.
            NSView *topView = getTopView(w);
            if (topView != nil) {
                CALayer *layer = [topView layer];
                if (layer != nil) {
                    CGFloat radius = [layer cornerRadius];
                    if (radius == 0) {
                        // debug
                        // NSLog(@"Fixing corner radius of %@", layer);
                        [layer setCornerRadius: 6];
                    }
                } else {
                    NSLog(@"Unable to fix corner radius: no layer");
                }
            } else {
                NSLog(@"Unable to fix corner radius: did not find top view");
            }
        }

        result = 0;
    });

    COCOA_EXIT();

    return result;
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    nativeAddToolbarToWindow
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL Java_org_violetlib_aqua_AquaUtils_nativeAddToolbarToWindow
    (JNIEnv *env, jclass cl, jlong wptr)
{
    jint result = -1;

    COCOA_ENTER();

    NSWindow *w = (NSWindow *) wptr;
    runOnMainThread(^() {
        NSToolbar *tb = [[NSToolbar alloc] initWithIdentifier: @"Foo"];
        [tb setShowsBaselineSeparator: NO];
        [w setToolbar: tb];
    });
    result = 0;

    COCOA_EXIT();

    return result;
}

/*
 * Class:     org_violetlib_aqua_AquaSheetSupport
 * Method:    nativeDisplayAsSheet
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL Java_org_violetlib_aqua_AquaSheetSupport_nativeDisplayAsSheet
    (JNIEnv *env, jclass cl, jlong wptr, jlong owner_wptr)
{
    jint result = -1;

    COCOA_ENTER();

    NSWindow *w = (NSWindow *) wptr;
    NSWindow *no = (NSWindow *) owner_wptr;

    runOnMainThread(^() {
        [no beginSheet:w completionHandler:^(NSModalResponse r){NSLog(@"Modal sheet session terminated");}];
    });
    result = 0;

    COCOA_EXIT();

    return result;
}

/*
 * Class:     org_violetlib_aqua_AquaSheetSupport
 * Method:    nativeEndSheetSession
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL Java_org_violetlib_aqua_AquaSheetSupport_nativeEndSheetSession
    (JNIEnv *env, jclass cl, jlong wptr, jlong owner_wptr)
{
    jint result = -1;

    COCOA_ENTER();

    NSWindow *w = (NSWindow *) wptr;
    NSWindow *no = (NSWindow *) owner_wptr;

    runOnMainThread(^() {
        [no endSheet:w];
    });
    result = 0;

    COCOA_EXIT();

    return result;
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    nativeSetWindowVisibleField
 * Signature: (Ljava/awt/Window;Z)V
 */
JNIEXPORT void JNICALL Java_org_violetlib_aqua_AquaUtils_nativeSetWindowVisibleField
    (JNIEnv *env, jclass cl, jobject window, jboolean isVisible)
{
    static jfieldID jf_visible;

    GET_CLASS(jc_Window, "java/awt/Window");
    GET_FIELD(jf_visible, jc_Window, "visible", "Z");

    COCOA_ENTER();

    (*env)->SetBooleanField(env, window, jf_visible, isVisible);
    CHECK_EXCEPTION();

    COCOA_EXIT();
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    nativeSetWindowCornerRadius
 * Signature: (JF)I
 */
JNIEXPORT jint JNICALL Java_org_violetlib_aqua_AquaUtils_nativeSetWindowCornerRadius
    (JNIEnv *env, jclass cl, jlong wptr, jfloat radius)
{
    __block jint result = -1;

    COCOA_ENTER();

    NSWindow *w = (NSWindow *) wptr;
    runOnMainThread(^() {
        AquaWrappedAWTView *view = ensureWrapper(w);
        result = [view configureAsPopup:radius];
    });

    COCOA_EXIT();

    return result;
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    nativeUpdateWindowInsets
 * Signature: (Ljava/awt/Window;Ljava/awt/Insets)I
 */
JNIEXPORT jint JNICALL Java_org_violetlib_aqua_AquaUtils_nativeUpdateWindowInsets
    (JNIEnv *env, jclass cl, jobject w, jobject s)
{
    static jfieldID jf_updateInsets;

    GET_CLASS_RETURN(jc_LWWindowPeer, "sun/lwawt/LWWindowPeer", 1);
    GET_FIELD_RETURN(jf_updateInsets, jc_LWWindowPeer, "updateInsets", "(Ljava/awt/Insets;)Z", 1);

    jint result = 1;
    jobject peer = getWindowPeer(env, w);
    if (peer != nil) {
        if ((*env)->CallBooleanMethod(env, peer, jf_updateInsets, s)) {
            result = 0;
        }
    }
    return result;
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    nativeFixPopupWindow
 * Signature: (Ljava/awt/Window;Ljavax/swing/Popup;)V
 */
JNIEXPORT void JNICALL Java_org_violetlib_aqua_AquaUtils_nativeFixPopupWindow
    (JNIEnv *env, jclass cl, jobject jParent, jobject jPopup)
{
    static jclass jc_Popup;
    static jmethodID jm_pack;
    static jmethodID jm_getComponent;

    GET_CLASS(jc_Popup, "javax/swing/Popup");
    GET_METHOD(jm_pack, jc_Popup, "pack", "()V");
    GET_METHOD(jm_getComponent, jc_Popup, "getComponent", "()Ljava/awt/Component;");

    COCOA_ENTER();

    (*env)->CallVoidMethod(env, jPopup, jm_pack);  // ensure that component peer exists
    CHECK_EXCEPTION();
    jobject w = (*env)->CallObjectMethod(env, jPopup, jm_getComponent);
    if (w != NULL) {
        jobject readLock = NULL;
        NSWindow *nw = getNativeWindow(env, w, &readLock);
        NSWindow *nparent = getNativeWindow(env, jParent, &readLock);
        if (nw != nil) {
            APPKIT_EXEC(^() {
                nw.level = NSPopUpMenuWindowLevel;
                if (nparent != NULL) {
                    [nparent addChildWindow:nw ordered:NSWindowAbove];
                }
            });
        }
    }

    COCOA_EXIT();
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    nativeFixWindowWithEmbeddedOwner
 * Signature: (Ljava/awt/Window;Ljava/awt/Window;)V
 */
JNIEXPORT void JNICALL Java_org_violetlib_aqua_AquaUtils_nativeFixWindowWithEmbeddedOwner
    (JNIEnv *env, jclass cl, jobject jWindow, jobject jOwner, jint windowLevel)
{
    jobject readLock = NULL;
    __block NSWindow *window = getNativeWindow(env, jWindow, &readLock);
    if (window != nil) {
        __block NSWindow *oldOwnerWindow;

        COCOA_ENTER();
        APPKIT_EXEC(^() {
            oldOwnerWindow = window.parentWindow;
        });
        COCOA_EXIT();

       __block NSWindow *ownerWindow = getNativeWindow(env, jOwner, &readLock);
        if (ownerWindow != oldOwnerWindow) {

            // A special case for native file panels. VFileDialog replaces the NSAccessoryViewWindow owner with the
            // corresponding NSOpenPanel or NSSavePanel. That replacement should not be reverted here.

            if (oldOwnerWindow == nil || ![oldOwnerWindow isKindOfClass:[NSSavePanel class]]) {
                COCOA_ENTER();
                APPKIT_EXEC(^() {
                    NSLog(@"Updating native owner of %@ from %@ to %@", window, oldOwnerWindow, ownerWindow);
                    window.level = windowLevel;
                    if (oldOwnerWindow != nil) {
                        [oldOwnerWindow removeChildWindow:window];
                    }
                    if (ownerWindow != nil) {
                        [ownerWindow addChildWindow:window ordered:NSWindowAbove];
                    }
                });
                COCOA_EXIT();
            }
        }
    }
}

/*
 * Class:     org_violetlib_aqua_AquaVibrantSupport
 * Method:    setupVisualEffectWindow
 * Signature: (JIZ)I
 */
JNIEXPORT jint JNICALL Java_org_violetlib_aqua_AquaVibrantSupport_setupVisualEffectWindow
    (JNIEnv *env, jclass cl, jlong wptr, jint style, jboolean forceActive)
{
    jint result = -1;

    COCOA_ENTER();

    NSWindow *w = (NSWindow *) wptr;

    if (style == SHEET_STYLE) {
        forceActive = YES;
    }

    runOnMainThread(^() {
        // Insert a visual effect view as a sibling of the AWT view if there is not already one present.
        AquaWrappedAWTView *wrapper = ensureWrapper(w);
        AquaVisualEffectView *fxView = [wrapper addFullWindowVisualEffectView];
        fxView.style = style;
        [fxView configureWithAppearance: w.effectiveAppearance];
        fxView.state = forceActive ? NSVisualEffectStateActive : NSVisualEffectStateFollowsWindowActiveState;
        [fxView setNeedsDisplay: YES];
        setupLayers(fxView);
    });
    result = 0;

    COCOA_EXIT();

    return result;
}

/*
 * Class:     org_violetlib_aqua_AquaVibrantSupport
 * Method:    removeVisualEffectWindow
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL Java_org_violetlib_aqua_AquaVibrantSupport_removeVisualEffectWindow
    (JNIEnv *env, jclass cl, jlong wptr)
{
    jint result = -1;

    COCOA_ENTER();

    NSWindow *w = (NSWindow *) wptr;
    runOnMainThread(^() {
        AquaWrappedAWTView *wrapper = getWrapper(w);
        if (wrapper != nil) {
            [wrapper removeFullWindowVisualEffectView];
        }
    });
    result = 0;

    COCOA_EXIT();

    return result;
}

/*
 * Class:     org_violetlib_aqua_AquaVibrantSupport
 * Method:    createVisualEffectView
 * Signature: (JIZZ)J
 */
JNIEXPORT jlong JNICALL Java_org_violetlib_aqua_AquaVibrantSupport_nativeCreateVisualEffectView
    (JNIEnv *env, jclass cl, jlong wptr, jint style, jboolean supportSelections, jboolean forceActive)
{
    __block jlong result = 0;

    COCOA_ENTER();

    NSWindow *w = (NSWindow *) wptr;
    runOnMainThread(^() {
        // Insert a view as a sibling of the AWT view.
        AquaWrappedAWTView *wrapper = ensureWrapper(w);
        AquaVisualEffectView *view;
        if (supportSelections) {
            view = [[AquaSidebarBackground alloc] initWithFrame: NSMakeRect(0, 0, 0, 0) style:style forceActive:forceActive];
        } else {
            AquaVisualEffectView *fxView = [[AquaVisualEffectView alloc] initWithFrame: NSMakeRect(0, 0, 0, 0)];
            fxView.style = style;
            fxView.blendingMode = NSVisualEffectBlendingModeBehindWindow;
            if (forceActive) {
                fxView.state = NSVisualEffectStateActive;
            }
            view = fxView;
        }
        [view configureWithAppearance:w.effectiveAppearance];
        [wrapper addSiblingView: view];
        setupLayers(view);
        result = (jlong) view;
    });

    COCOA_EXIT();

    return result;
}

/*
 * Class:     org_violetlib_aqua_AquaVibrantSupport
 * Method:    setViewFrame
 * Signature: (JIIIII)I
 */
JNIEXPORT jint JNICALL Java_org_violetlib_aqua_AquaVibrantSupport_setViewFrame
    (JNIEnv *env, jclass cl, jlong ptr, jint x, jint y, jint w, jint h, jint yflipped)
{
    __block jint result = -1;

    COCOA_ENTER();

    NSView *view = (NSView *) ptr;
    runOnMainThread(^() {
        NSWindow *window = [view window];
        if (window != nil) {

//            NSLog(@"Setting visual effect view frame: %d %d %d %d %d", x, y, w, h, yflipped);
//            NSRect f = window.frame;
//            NSLog(@"  Window size: %f %f", f.size.width, f.size.height);

            [view setFrame: NSMakeRect(x, yflipped, w, h)];
            view.needsDisplay = YES;
            result = 0;
        } else {
            NSLog(@"AquaVibrantSupport_setViewFrame failed: no native window");
        }
    });

    COCOA_EXIT();

    return result;
}

/*
 * Class:     org_violetlib_aqua_AquaVibrantSupport
 * Method:    nativeUpdateSelectionBackgrounds
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL Java_org_violetlib_aqua_AquaVibrantSupport_nativeUpdateSelectionBackgrounds
    (JNIEnv *env, jclass cl, jlong ptr, jintArray jdata)
{
    __block jint result = -1;

    COCOA_ENTER();

    NSView *view = (NSView *) ptr;

    //windowDebug(view.window);   // debug

    if ([view isKindOfClass: [AquaSidebarBackground class]]) {
        AquaSidebarBackground *sbb = (AquaSidebarBackground*) view;
        if (jdata != NULL) {
            int *data = (*env)->GetIntArrayElements(env, jdata, NULL);
            if (data != NULL) {
                runOnMainThread(^() {
                    [sbb updateSelectionViews: data];
                    result = 0;
                });
                (*env)->ReleaseIntArrayElements(env, jdata, data, JNI_ABORT);
            }
        } else {
            runOnMainThread(^() {
                [sbb updateSelectionViews: NULL];
                result = 0;
            });
        }
    }

    COCOA_EXIT();

    return result;
}

/*
 * Class:     org_violetlib_aqua_AquaVibrantSupport
 * Method:    disposeVisualEffectView
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL Java_org_violetlib_aqua_AquaVibrantSupport_disposeVisualEffectView
    (JNIEnv *env, jclass cl, jlong ptr)
{
    __block jint result = -1;

    COCOA_ENTER();

    NSView *view = (NSView *) ptr;
    runOnMainThread(^() {
        NSWindow *window = [view window];
        if (window != nil) {
            AquaWrappedAWTView *wrapper = getWrapper(window);
            if (wrapper != nil && wrapper == [view superview]) {
                [view removeFromSuperview];
                result = 0;
            }
        } else {
            NSLog(@"AquaVibrantSupport_disposeVisualEffectView failed: no native window");
        }
    });

    COCOA_EXIT();

    return result;
}

@interface ViewportView : NSView
@end

@implementation ViewportView
@end

// Create a view frame from a Java rectangle
static NSRect createFrameInParentWithHeight(float parentHeight, float x, float y, float w, float h)
{
    return NSMakeRect(x, parentHeight - (y + h), w, h);
}

// Create a view frame from a Java rectangle
static NSRect createFrame(NSView *parent, float x, float y, float w, float h)
{
    float parentHeight = parent.frame.size.height;
    return createFrameInParentWithHeight(parentHeight, x, y, w, h);
}

static void internalHideNativeView(NSView *view)
{
    NSWindow *window = view.window;
    if (window != nil) {
        view.hidden = YES;
    }
}

static void removeNativeView(NSView *view)
{
    NSView *parent = view.superview;
    if (parent) {
        [view removeFromSuperview];
        if ([parent isKindOfClass:[ViewportView class]]) {
            [parent removeFromSuperview];
        }
    }
}

static void internalShowNativeView(NSView *view, NSWindow *window, jint x, jint y, jint w, jint h)
{
    NSView *parent = getAWTView(window);
    if (parent) {
        NSView *currentParent = view.superview;
        if (parent != currentParent) {
            removeNativeView(view);
            [parent addSubview: view];
        }

        view.hidden = NO;
        view.frame = createFrame(view.superview, x, y, w, h);
        view.needsDisplay = YES;
    }
}

static void internalShowNativeViewClipped(NSView *view, NSWindow *window,
        jint cx, jint cy, jint cw, jint ch, jint x, jint y, jint w, jint h)
{
    NSWindow *currentWindow = view.window;
    NSView *currentParent = view.superview;

    if (window != currentWindow || ![currentParent isKindOfClass:[ViewportView class]]) {
        removeNativeView(view);
    }

    NSView *awtView = getAWTView(window);
    if (awtView) {
        view.frame = createFrameInParentWithHeight(h, cx, cy, cw, ch);

        ViewportView *viewport;
        if (view.superview == nil) {
            viewport = [[ViewportView alloc] initWithFrame:createFrame(awtView, x, y, w, h)];
            viewport.autoresizesSubviews = NO;
            viewport.autoresizingMask = NSViewNotSizable;
            [viewport addSubview:view];
            [awtView addSubview:viewport];
        } else {
            viewport = (ViewportView *) view.superview;
            viewport.frame = createFrame(awtView, x, y, w, h);
        }

        view.hidden = NO;
        view.needsDisplay = YES;
    }
}

/*
 * Class:     org_violetlib_aqua_NativeOverlayView
 * Method:    hideNativeView
 * Signature: (J)V
 */
JNIEXPORT void JNICALL Java_org_violetlib_aqua_NativeOverlayView_hideNativeView
  (JNIEnv *env, jclass cl, jlong vptr)
{
    NSView *view = (NSView *) vptr;

    COCOA_ENTER();

    runOnMainThread(^() {internalHideNativeView(view);});

    COCOA_EXIT();
}

/*
 * Class:     org_violetlib_aqua_NativeOverlayView
 * Method:    showNativeView
 * Signature: (JJIIII)V
 */
JNIEXPORT void JNICALL Java_org_violetlib_aqua_NativeOverlayView_showNativeView
  (JNIEnv *env, jclass cl, jlong vptr, jlong wptr, jint x, jint y, jint w, jint h)
{
    NSView *view = (NSView *) vptr;
    NSWindow *window = (NSWindow *) wptr;

    COCOA_ENTER();

    runOnMainThread(^() {internalShowNativeView(view, window, x, y, w, h);});

    COCOA_EXIT();
}

/*
 * Class:     org_violetlib_aqua_NativeOverlayView
 * Method:    showNativeViewClipped
 * Signature: (JJIIIIIIII)V
 */
JNIEXPORT void JNICALL Java_org_violetlib_aqua_NativeOverlayView_showNativeViewClipped
  (JNIEnv *env, jclass cl, jlong vptr, jlong wptr, jint cx, jint cy, jint cw, jint ch, jint x, jint y, jint w, jint h)
{
    NSView *view = (NSView *) vptr;
    NSWindow *window = (NSWindow *) wptr;

    COCOA_ENTER();

    runOnMainThread(^() {internalShowNativeViewClipped(view, window, cx, cy, cw, ch, x, y, w, h);});

    COCOA_EXIT();
}

static NSView *internalCreatePreviewView()
{
    NSRect bounds = NSMakeRect(0, 0, 1, 1);
    QLPreviewView *preview = [[QLPreviewView alloc] initWithFrame:bounds style:QLPreviewViewStyleCompact];
    preview.shouldCloseWithWindow = NO;
    return preview;
}

static void internalConfigurePreview(NSView *v, NSURL *u)
{
    QLPreviewView *view = (QLPreviewView *) v;
    if (u) {
        BOOL oldHidden = view.hidden;
        view.hidden = YES;
        [view setPreviewItem:u];
        view.hidden = oldHidden;
    } else {
        view.hidden = YES;
    }
}

static void internalDisposePreviewView(NSView *view)
{
    [view release];
}

/*
 * Class:     org_violetlib_aqua_fc_FilePreviewView
 * Method:    nativeCreatePreviewView
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL Java_org_violetlib_aqua_fc_FilePreviewView_nativeCreatePreviewView
  (JNIEnv *env, jclass cl)
{
    __block jlong result = 0;

    COCOA_ENTER();

    runOnMainThread(^() {result = (jlong) internalCreatePreviewView();});

    COCOA_EXIT();

    return result;
}

/*
 * Class:     org_violetlib_aqua_fc_FilePreviewView
 * Method:    nativeConfigurePreview
 * Signature: (JLjava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_org_violetlib_aqua_fc_FilePreviewView_nativeConfigurePreview
  (JNIEnv *env, jclass cl, jlong vptr, jstring jpath)
{
    QLPreviewView *view = (QLPreviewView *) vptr;
    NSURL *u = nil;

    COCOA_ENTER();

    if (jpath != NULL) {
        NSString *path = TO_NSPATH(jpath);
        u = [NSURL fileURLWithPath:path];
    }

    runOnMainThread(^() {internalConfigurePreview(view, u);});

    COCOA_EXIT();
}

/*
 * Class:     org_violetlib_aqua_fc_FilePreviewView
 * Method:    nativeDisposePreviewView
 * Signature: (J)V
 */
JNIEXPORT void JNICALL Java_org_violetlib_aqua_fc_FilePreviewView_nativeDisposePreviewView
  (JNIEnv *env, jclass cl, jlong vptr)
{
    NSView *view = (NSView *) vptr;

    COCOA_ENTER();

    runOnMainThread(^() {internalDisposePreviewView(view);});

    COCOA_EXIT();
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    nativeSetAWTViewVisibility
 * Signature: (JZ)I
 */
JNIEXPORT jint JNICALL Java_org_violetlib_aqua_AquaUtils_nativeSetAWTViewVisibility
    (JNIEnv *env, jclass cl, jlong wptr, jboolean isVisible)
{
    COCOA_ENTER();

    NSWindow *w = (NSWindow *) wptr;
    runOnMainThread(^() {
        NSView *v = getAWTView(w);
        v.hidden = !isVisible;
    });

    COCOA_EXIT();

    return 0;
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    nativeSyncAWTView
 * Signature: (J)V
 */
JNIEXPORT jint JNICALL Java_org_violetlib_aqua_AquaUtils_nativeSyncAWTView
    (JNIEnv *env, jclass cl, jlong wptr)
{
    COCOA_ENTER();

    NSWindow *w = (NSWindow *) wptr;
    // Not waiting because of a possible deadlock observed creating a native file dialog with a Java accessory
    APPKIT_EXEC_LATER(^()
    {
         NSView *v = getAWTView(w);
         [v.layer displayIfNeeded];
    });

    COCOA_EXIT();
    return 0;
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    nativeGetLeftSideBearing
 * Signature: (Ljavax/swing/JComponent;Ljava/awt/FontMetrics;C)I
 */
JNIEXPORT jint JNICALL Java_org_violetlib_aqua_AquaUtils_nativeGetLeftSideBearing
  (JNIEnv *env, jclass cl, jobject comp, jobject fm, jchar firstChar)
{
    static jmethodID jm_getLeftSideBearing;

    GET_CLASS_RETURN(jc_SwingUtilities2, "sun/swing/SwingUtilities2", 0);
    GET_STATIC_METHOD_RETURN(jm_getLeftSideBearing, jc_SwingUtilities2, "getLeftSideBearing",
        "(Ljavax/swing/JComponent;Ljava/awt/FontMetrics;C)I", 0);

    jint result = 0;

    COCOA_ENTER();

    result = (*env)->CallStaticIntMethod(env, jc_SwingUtilities2, jm_getLeftSideBearing, comp, fm, firstChar);
    CHECK_EXCEPTION();

    COCOA_EXIT();

    return result;
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    nativeInstallAATextInfo
 * Signature: (Ljavax/swing/UIDefaults;)V
 */
JNIEXPORT void JNICALL Java_org_violetlib_aqua_AquaUtils_nativeInstallAATextInfo
  (JNIEnv *env, jclass cl, jobject table)
{
    // This implementation is valid in Java 9 (not in Java 8).
    // SwingUtilities2.putAATextInfo(true, table);

    static jmethodID jm_putAATextInfo;

    GET_CLASS(jc_SwingUtilities2, "sun/swing/SwingUtilities2");
    GET_STATIC_METHOD(jm_putAATextInfo, jc_SwingUtilities2, "putAATextInfo", "(ZLjava/util/Map;)V");

    COCOA_ENTER();

    (*env)->CallStaticVoidMethod(env, jc_SwingUtilities2, jm_putAATextInfo, JNI_TRUE, table);
    CHECK_EXCEPTION();

    COCOA_EXIT();
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    disablePopupCache
 * Signature: (Ljavax/swing/Popup;)V
 */
JNIEXPORT void JNICALL Java_org_violetlib_aqua_AquaUtils_disablePopupCache
    (JNIEnv *env, jclass cl, jobject popup)
{
    static jmethodID jm_setCacheEnabled;

    GET_CLASS(jc_HeavyWeightPopup, "javax/swing/PopupFactory$HeavyWeightPopup");
    GET_METHOD(jm_setCacheEnabled, jc_HeavyWeightPopup, "setCacheEnabled", "(Z)V");

    (*env)->CallVoidMethod(env, popup, jm_setCacheEnabled, JNI_FALSE);
    CHECK_EXCEPTION();
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    getScreenMenuBarProperty
 * Signature: ()Z
 */
JNIEXPORT jboolean JNICALL Java_org_violetlib_aqua_AquaUtils_getScreenMenuBarProperty
  (JNIEnv *env, jclass cl)
{
    static jmethodID jm_getScreenMenuBarProperty;
    static jmethodID jm_isSystemMenuBarSupported;

    GET_CLASS_RETURN(jc_AquaMenuBarUI, "com/apple/laf/AquaMenuBarUI", 0);
    GET_OPTIONAL_STATIC_METHOD(jm_getScreenMenuBarProperty, jc_AquaMenuBarUI, "getScreenMenuBarProperty", "()Z");
    GET_CLASS_RETURN(jc_LWCToolkit, "sun/lwawt/macosx/LWCToolkit", 0);
    GET_OPTIONAL_STATIC_METHOD(jm_isSystemMenuBarSupported, jc_LWCToolkit, "isSystemMenuBarSupported", "()Z");

    jboolean result = 0;

    if (jm_getScreenMenuBarProperty != NULL) {
        result = (*env)->CallStaticBooleanMethod(env, jc_AquaMenuBarUI, jm_getScreenMenuBarProperty);
    } else if (jm_isSystemMenuBarSupported != NULL) {
        result = (*env)->CallStaticBooleanMethod(env, jc_LWCToolkit, jm_isSystemMenuBarSupported);
    }

    CHECK_EXCEPTION();

    return result;
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    setScreenMenuBar
 * Signature: (Ljavax/swing/JFrame;Ljavax/swing/plaf/MenuBarUI;)V
 */
JNIEXPORT void JNICALL Java_org_violetlib_aqua_AquaUtils_setScreenMenuBar
  (JNIEnv *env, jclass cl, jobject frame, jobject menuBarUI)
{
    static jmethodID jm_setScreenMenuBar;

    GET_CLASS(jc_AquaMenuBarUI, "com/apple/laf/AquaMenuBarUI");
    GET_METHOD(jm_setScreenMenuBar, jc_AquaMenuBarUI, "setScreenMenuBar", "(Ljavax/swing/JFrame;)Z");

    (*env)->CallBooleanMethod(env, menuBarUI, jm_setScreenMenuBar, frame);
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    clearScreenMenuBar
 * Signature: (Ljavax/swing/JFrame;Ljavax/swing/plaf/MenuBarUI;)V
 */
JNIEXPORT void JNICALL Java_org_violetlib_aqua_AquaUtils_clearScreenMenuBar
  (JNIEnv *env, jclass cl, jobject frame, jobject menuBarUI)
{
    static jmethodID jm_clearScreenMenuBar;

    GET_CLASS(jc_AquaMenuBarUI, "com/apple/laf/AquaMenuBarUI");
    GET_METHOD(jm_clearScreenMenuBar, jc_AquaMenuBarUI, "clearScreenMenuBar", "(Ljavax/swing/JFrame;)V");

    (*env)->CallVoidMethod(env, menuBarUI, jm_clearScreenMenuBar, frame);
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    nativeHasOpaqueBeenExplicitlySet
 * Signature: (Ljavax/swing/JComponent;)Z
 */
JNIEXPORT jboolean JNICALL Java_org_violetlib_aqua_AquaUtils_nativeHasOpaqueBeenExplicitlySet
  (JNIEnv *env, jclass cl, jobject c)
{
    static jmethodID jm_getFlag;

    GET_CLASS_RETURN(jc_JComponent, "javax/swing/JComponent", NO);
    GET_METHOD_RETURN(jm_getFlag, jc_JComponent, "getFlag", "(I)Z", NO);

    jboolean result = (*env)->CallBooleanMethod(env, c, jm_getFlag, 24);    // 24 is JComponent.OPAQUE_SET
    CHECK_EXCEPTION();
    return result;
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    nativeSetWindowAppearance
 * Signature: (JLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_org_violetlib_aqua_AquaUtils_nativeSetWindowAppearance
  (JNIEnv *env, jclass cl, jlong wptr, jstring jAppearanceName)
{
    jint result = -1;

    COCOA_ENTER();

    NSWindow *w = (NSWindow *) wptr;
    NSAppearance *appearance = nil;

    if (jAppearanceName) {
        NSString *appearanceName = TO_NSSTRING(jAppearanceName);
        NSAppearance *app = [NSAppearance appearanceNamed: appearanceName];
        if ([appearanceName isEqualToString: app.name]) {
            // If the appearance name is not recognized, some other appearance is returned.
            appearance = app;
            result = 0;
        }
    }

    runOnMainThread(^() {
        w.appearance = appearance;
    });

    COCOA_EXIT();

    return result;
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    nativeGetWindowEffectiveAppearanceName
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_org_violetlib_aqua_AquaUtils_nativeGetWindowEffectiveAppearanceName
  (JNIEnv *env, jclass cl, jlong wptr)
{
    jstring result = nil;
    __block NSAppearanceName appearanceName = nil;

    COCOA_ENTER();

    NSWindow *w = (NSWindow *) wptr;
    runOnMainThread(^() {
        appearanceName = [w.effectiveAppearance name];
    });

    if (appearanceName) {
        result = (*env)->NewStringUTF(env, [appearanceName UTF8String]);
    }

    COCOA_EXIT();
    return result;
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    nativeGetApplicationAppearanceName
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_org_violetlib_aqua_AquaUtils_nativeGetApplicationAppearanceName
  (JNIEnv *env, jclass cl)
{
    __block jstring result = nil;

    COCOA_ENTER();

    if (@available(macOS 10.14, *)) {
        NSAppearanceName appearanceName = [NSApp.effectiveAppearance name];
        if (appearanceName) {
            result = (*env)->NewStringUTF(env, [appearanceName UTF8String]);
        }
    }

    COCOA_EXIT();
    return result;
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    nativeDebugWindow
 * Signature: (J)V
 */
JNIEXPORT int JNICALL Java_org_violetlib_aqua_AquaUtils_nativeDebugWindow
    (JNIEnv *env, jclass cl, jlong wptr)
{
    COCOA_ENTER();

    NSWindow *w = (NSWindow *) wptr;
    runOnMainThread(^() {
        windowDebug(w);
    });

    COCOA_EXIT();
    return 0;
}

/*
 * Class:     org_violetlib_aqua_AquaUtils
 * Method:    syslog
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_org_violetlib_aqua_AquaUtils_syslog
    (JNIEnv *env, jclass cl, jstring msg)
{
    jsize slen = (*env) -> GetStringLength(env, msg);
    const jchar *schars = (*env) -> GetStringChars(env, msg, NULL);
    CFStringRef s = CFStringCreateWithCharacters(NULL, schars, slen);
    NSLog(@"%@", s);
    CFRelease(s);
    (*env) -> ReleaseStringChars(env, msg, schars);
}

static void failure(JNIEnv *env, const char *msg)
{
    jclass cls = (*env)->FindClass(env, "java/lang/RuntimeException");
    /* if cls is NULL, an exception has already been thrown */
    if (cls != NULL) {
        (*env)->ThrowNew(env, cls, msg);
    }
    (*env)->DeleteLocalRef(env, cls);
}

/*
 * Class:     org_violetlib_aqua_AquaNativeSupport
 * Method:    setup
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_org_violetlib_aqua_AquaNativeSupport_setup
    (JNIEnv *env, jclass cl, jint jv)
{
    javaVersion = jv;
    if ((*env)->GetJavaVM(env, &vm) < 0) {
       failure(env, "Failed to get Java VM for native code");
    }
    JNU_SETUP(vm);
}

/*
 * Class:     org_violetlib_aqua_AquaNativeSupport
 * Method:    nativeGetNativeCodeVersion
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_org_violetlib_aqua_AquaNativeSupport_nativeGetNativeCodeVersion
    (JNIEnv *env, jclass javaClass)
{
    return VERSION;
}
