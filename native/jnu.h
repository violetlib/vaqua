// Based on code from OpenJDK, under the following license.

/*
 * Copyright (c) 2020, 2021, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

#ifndef __JNU_H
#define __JNU_H

#include "jni.h"

#import <Cocoa/Cocoa.h>

#ifndef _JNU_PREFIX
#define _JNU_PREFIX jnu
#endif

#define _JNU_PASTER(x,y) x ## _ ## y
#define _JNU_EVALUATOR(x,y) _JNU_PASTER(x,y)
#define JNU_SUPPORT(name) _JNU_EVALUATOR(_JNU_PREFIX, name)



extern void JNU_SUPPORT(appkitExec)(void (^block)());
extern void JNU_SUPPORT(appkitExecLater)(void (^block)());
extern void JNU_SUPPORT(appkitPerform)(id target, SEL selector, id arg);
extern JNIEnv *JNU_SUPPORT(getAppkitJNIEnvironment());




#define CHECK_NULL(x)                           \
    do {                                        \
        if ((x) == NULL) {                      \
            return;                             \
        }                                       \
    } while (0)                                 \

#define CHECK_NULL_RETURN(x, y)                 \
    do {                                        \
        if ((x) == NULL) {                      \
            return (y);                         \
        }                                       \
    } while (0)                                 \






/******** GET CLASS SUPPORT *********/

#define GET_CLASS(dst_var, cls) \
     if (dst_var == NULL) { \
         dst_var = (*env)->FindClass(env, cls); \
         if (dst_var != NULL) dst_var = (*env)->NewGlobalRef(env, dst_var); \
     } \
     CHECK_NULL(dst_var);

#define GET_OPTIONAL_CLASS(dst_var, cls) \
     if (dst_var == NULL) { \
         dst_var = (*env)->FindClass(env, cls); \
         if (dst_var != NULL) dst_var = (*env)->NewGlobalRef(env, dst_var); \
         (*env)->ExceptionClear(env); \
     }

#define DECLARE_CLASS(dst_var, cls) \
    static jclass dst_var = NULL; \
    GET_CLASS(dst_var, cls);

#define GET_CLASS_RETURN(dst_var, cls, ret) \
     if (dst_var == NULL) { \
         dst_var = (*env)->FindClass(env, cls); \
         if (dst_var != NULL) dst_var = (*env)->NewGlobalRef(env, dst_var); \
     } \
     CHECK_NULL_RETURN(dst_var, ret);

#define DECLARE_CLASS_RETURN(dst_var, cls, ret) \
    static jclass dst_var = NULL; \
    GET_CLASS_RETURN(dst_var, cls, ret);


/******** GET METHOD SUPPORT *********/

#define GET_METHOD(dst_var, cls, name, signature) \
     if (dst_var == NULL) { \
         dst_var = (*env)->GetMethodID(env, cls, name, signature); \
     } \
     CHECK_NULL(dst_var);

#define GET_OPTIONAL_METHOD(dst_var, cls, name, signature) \
     if (dst_var == NULL) { \
         dst_var = (*env)->GetMethodID(env, cls, name, signature); \
         (*env)->ExceptionClear(env); \
     }

#define DECLARE_METHOD(dst_var, cls, name, signature) \
     static jmethodID dst_var = NULL; \
     GET_METHOD(dst_var, cls, name, signature);

#define GET_METHOD_RETURN(dst_var, cls, name, signature, ret) \
     if (dst_var == NULL) { \
         dst_var = (*env)->GetMethodID(env, cls, name, signature); \
     } \
     CHECK_NULL_RETURN(dst_var, ret);

#define DECLARE_METHOD_RETURN(dst_var, cls, name, signature, ret) \
     static jmethodID dst_var = NULL; \
     GET_METHOD_RETURN(dst_var, cls, name, signature, ret);

#define GET_STATIC_METHOD(dst_var, cls, name, signature) \
     if (dst_var == NULL) { \
         dst_var = (*env)->GetStaticMethodID(env, cls, name, signature); \
     } \
     CHECK_NULL(dst_var);

#define GET_OPTIONAL_STATIC_METHOD(dst_var, cls, name, signature) \
     if (dst_var == NULL) { \
         dst_var = (*env)->GetStaticMethodID(env, cls, name, signature); \
         (*env)->ExceptionClear(env); \
     }

#define DECLARE_STATIC_METHOD(dst_var, cls, name, signature) \
     static jmethodID dst_var = NULL; \
     GET_STATIC_METHOD(dst_var, cls, name, signature);

#define GET_STATIC_METHOD_RETURN(dst_var, cls, name, signature, ret) \
     if (dst_var == NULL) { \
         dst_var = (*env)->GetStaticMethodID(env, cls, name, signature); \
     } \
     CHECK_NULL_RETURN(dst_var, ret);

#define DECLARE_STATIC_METHOD_RETURN(dst_var, cls, name, signature, ret) \
     static jmethodID dst_var = NULL; \
     GET_STATIC_METHOD_RETURN(dst_var, cls, name, signature, ret);


/******** GET FIELD SUPPORT *********/

#define GET_FIELD(dst_var, cls, name, signature) \
     if (dst_var == NULL) { \
         dst_var = (*env)->GetFieldID(env, cls, name, signature); \
     } \
     CHECK_NULL(dst_var);

#define GET_OPTIONAL_FIELD(dst_var, cls, name, signature) \
     if (dst_var == NULL) { \
         dst_var = (*env)->GetFieldID(env, cls, name, signature); \
         (*env)->ExceptionClear(env); \
     }

#define DECLARE_FIELD(dst_var, cls, name, signature) \
     static jfieldID dst_var = NULL; \
     GET_FIELD(dst_var, cls, name, signature);

#define GET_FIELD_RETURN(dst_var, cls, name, signature, ret) \
     if (dst_var == NULL) { \
         dst_var = (*env)->GetFieldID(env, cls, name, signature); \
     } \
     CHECK_NULL_RETURN(dst_var, ret);

#define DECLARE_FIELD_RETURN(dst_var, cls, name, signature, ret) \
     static jfieldID dst_var = NULL; \
     GET_FIELD_RETURN(dst_var, cls, name, signature, ret);

#define GET_STATIC_FIELD(dst_var, cls, name, signature) \
     if (dst_var == NULL) { \
         dst_var = (*env)->GetStaticFieldID(env, cls, name, signature); \
     } \
     CHECK_NULL(dst_var);

#define GET_OPTIONAL_STATIC_FIELD(dst_var, cls, name, signature) \
     if (dst_var == NULL) { \
         dst_var = (*env)->GetStaticFieldID(env, cls, name, signature); \
         (*env)->ExceptionClear(env); \
     }

#define GET_STATIC_FIELD_RETURN(dst_var, cls, name, signature, ret) \
     if (dst_var == NULL) { \
         dst_var = (*env)->GetStaticFieldID(env, cls, name, signature); \
     } \
     CHECK_NULL_RETURN(dst_var, ret);

#define DECLARE_STATIC_FIELD_RETURN(dst_var, cls, name, signature, ret) \
     static jfieldID dst_var = NULL; \
     GET_STATIC_FIELD_RETURN(dst_var, cls, name, signature, ret);

/********* EXCEPTION_HANDLING *********/

#define CHECK_EXCEPTION() \
    if ((*env)->ExceptionOccurred(env) != NULL) { \
        [NSException raise:NSGenericException format:@"Java Exception"]; \
    };

// Return if there is a pending exception.
// The exception is cleared.

#define EXCEPTION_EXIT() \
    if ((*env)->ExceptionCheck(env)) { \
        (*env)->ExceptionClear(env); \
        return; \
    }

// Return the specified value if there is a pending exception.
// The exception is cleared.

#define EXCEPTION_EXIT_RETURN(ret) \
    if ((*env)->ExceptionCheck(env)) { \
        (*env)->ExceptionClear(env); \
        return (ret); \
    }

/* Create a pool and initiate a try block to catch any exception */
#define COCOA_ENTER() \
 NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init]; \
 @try {

/* Don't allow NSExceptions to escape to Java.
 * If there is a Java exception that has been thrown that should escape.
 * And ensure we drain the auto-release pool.
 */
#define COCOA_EXIT() \
 } \
 @catch (NSException *e) { \
     NSLog(@"%@", [e callStackSymbols]); \
 } \
 @finally { \
    [pool drain]; \
 };

/******** AppKit main thread execution ********/

// Execute the block on the AppKit main thread, waiting until it completes.

#define APPKIT_EXEC(block) \
    if ([NSThread isMainThread]) { \
        block(); \
    } else { \
        JNU_SUPPORT(appkitExec)(block); \
    }

// Execute the block on the AppKit main thread. If invoked on the AppKit main thread
// the block is invoked directly. Otherwise, a request is posted to execute the block
// on the main thread (this thread does not wait).

#define APPKIT_EXEC_NOW_OR_LATER(block) \
    if ([NSThread isMainThread]) { \
        block(); \
    } else { \
        JNU_SUPPORT(appkitExecLater)(block); \
    }

// Post a request to execute the block on the main thread (this thread does not wait).

#define APPKIT_EXEC_LATER(block) \
    JNU_SUPPORT(appkitExecLater)(block);

#define APPKIT_PERFORM(target, selector, arg) \
    JNU_SUPPORT(appkitPerform)(target, selector, arg);

#define GET_APPKIT_JNI_ENVIRONMENT() \
    JNU_SUPPORT(getAppkitJNIEnvironment)();




/******** STRING CONVERSION SUPPORT *********/

extern NSString* JNU_SUPPORT(JavaStringToNSString)(JNIEnv *env, jstring jstr);

extern jstring JNU_SUPPORT(NSStringToJavaString)(JNIEnv* env, NSString *str);

extern NSString* JNU_SUPPORT(NormalizedPathNSStringFromJavaString)(JNIEnv *env, jstring pathStr);

extern jstring JNU_SUPPORT(NormalizedPathJavaStringFromNSString)(JNIEnv* env, NSString *str);

#define TO_JAVA_STRING(s) \
    JNU_SUPPORT(NSStringToJavaString)(env, s)

#define TO_NSSTRING(s) \
    JNU_SUPPORT(JavaStringToNSString)(env, s)

#define TO_JAVA_PATH(s) \
    JNU_SUPPORT(NormalizedPathJavaStringFromNSString)(env, s)

#define TO_NSPATH(s) \
    JNU_SUPPORT(NormalizedPathNSStringFromJavaString)(env, s)


#endif /* __JNU_H */
