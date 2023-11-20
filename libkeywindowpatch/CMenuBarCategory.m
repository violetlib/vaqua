/*
 * Copyright (c) 2018-2023 Alan Snyder.
 * All rights reserved.
 *
 * You may not use, copy or modify this file, except in compliance with the license agreement. For details see
 * accompanying license terms.
 */

#import <Cocoa/Cocoa.h>
#include <Carbon/Carbon.h>
#include "jnix.h"
#import "KeyWindowPatch.h"
#import "CMenuBarCategory.h"

@interface ApplicationDelegate { }
@end

static unichar CHAR_UNDEFINED = 0xFFFF;

static CMenuBar *sActiveMenuBar = nil;

__attribute__((visibility("default")))
NSString *CMenuBarDidReuseItemNotification =
    @"CMenuBarDidReuseItemNotification";

@implementation CMenuBar (CMenuBarCategory)

+ (BOOL) isActiveMenuBar:(CMenuBar *)inMenuBar {
    return (sActiveMenuBar == inMenuBar);
}

+ (void) activate:(CMenuBar *)menubar modallyDisabled:(BOOL)modallyDisabled {
    //AWT_ASSERT_APPKIT_THREAD;

    if (!menubar) {
        [CMenuBar clearMenuBarExcludingAppleMenu_OnAppKitThread:YES];
        return;
    }

#ifdef DEBUG_PATCH
    NSLog(@"activating menu bar: %@", menubar);
#endif

    @synchronized([CMenuBar class]) {
        sActiveMenuBar = menubar;
    }

    @synchronized(menubar) {
        menubar->fModallyDisabled = modallyDisabled;
    }

    NSUInteger i = 0, newMenuListSize = [menubar->fMenuList count];

    NSMenu *theMainMenu = [NSApp mainMenu];
    NSUInteger menuIndex, menuCount = [theMainMenu numberOfItems];

    NSUInteger cmenuIndex = 0, cmenuCount = newMenuListSize;
    NSMutableArray *removedMenuArray = [NSMutableArray array];

    for (menuIndex = 0; menuIndex < menuCount; menuIndex++) {
        NSMenuItem *currItem = [theMainMenu itemAtIndex:menuIndex];
        NSMenu *currMenu = [currItem submenu];

        if ([currMenu isJavaMenu]) {
            // Ready to replace, find next candidate
            CMenu *newMenu = nil;
            if (cmenuIndex < cmenuCount) {
                newMenu = (CMenu *)[menubar->fMenuList objectAtIndex:cmenuIndex];
                if (newMenu == menubar->fHelpMenu) {
                    cmenuIndex++;
                    if (cmenuIndex < cmenuCount) {
                        newMenu = (CMenu *)[menubar->fMenuList objectAtIndex:cmenuIndex];
                    }
                }
            }
            if (newMenu) {
                NSMenu *menuToAdd = [newMenu menu];
                if ([theMainMenu indexOfItemWithSubmenu:menuToAdd] == -1) {
                    [[NSNotificationCenter defaultCenter] postNotificationName:CMenuBarDidReuseItemNotification object:theMainMenu];

                    [currItem setSubmenu:menuToAdd];
                    [currItem setTitle:[menuToAdd title]];
                    cmenuIndex++;
                }

                BOOL newEnabledState = [newMenu isEnabled] && !menubar->fModallyDisabled;
                [currItem setEnabled:newEnabledState];
            } else {
                [removedMenuArray addObject:[NSNumber numberWithInteger:menuIndex]];
            }
        }
    }

    // Clean up extra items
    NSUInteger removedIndex, removedCount = [removedMenuArray count];
    for (removedIndex=removedCount; removedIndex > 0; removedIndex--) {
        NSUInteger index = [[removedMenuArray objectAtIndex:(removedIndex-1)] integerValue];
        NSMenuItem *currItem = [theMainMenu itemAtIndex:index];
        [currItem setSubmenu:nil];
        [theMainMenu removeItemAtIndex:index];
    }

    i = cmenuIndex;

    // Add all of the menus in the menu list.
    for (; i < newMenuListSize; i++) {
        CMenu *newMenu = (CMenu *)[menubar->fMenuList objectAtIndex:i];

        if (newMenu != menubar->fHelpMenu) {
            NSArray *args = [NSArray arrayWithObjects:newMenu, [NSNumber numberWithInt:-1], nil];
            [menubar nativeAddMenuAtIndex_OnAppKitThread:args];
        }
    }

    // Add the help menu last.
    if (menubar->fHelpMenu) {
        NSArray *args = [NSArray arrayWithObjects:menubar->fHelpMenu, [NSNumber numberWithInt:-1], nil];
        [menubar nativeAddMenuAtIndex_OnAppKitThread:args];
    } else {
        [CMenuBar addDefaultHelpMenu];
    }
}

-(void) deactivate {
    //AWT_ASSERT_APPKIT_THREAD;

    BOOL isDeactivated = NO;
    @synchronized([CMenuBar class]) {
        if (sActiveMenuBar == self) {
            sActiveMenuBar = nil;
            isDeactivated = YES;
        }
    }

    if (isDeactivated) {
#ifdef DEBUG_PATCH
        NSLog(@"deactivating menu bar: %@", self);
#endif

        @synchronized(self) {
            self->fModallyDisabled = NO;
        }

        // In theory, this might cause flickering if the window gaining focus
        // has its own menu. However, I couldn't reproduce it on practice, so
        // perhaps this is a non issue.
        CMenuBar* defaultMenu = [[ApplicationDelegate sharedDelegate] defaultMenuBar];
        if (defaultMenu != nil) {
            [CMenuBar activate:defaultMenu modallyDisabled:NO];
        }
    }
}

-(void) javaAddMenu: (CMenu *)theMenu {
    @synchronized(self) {
        [self->fMenuList addObject: theMenu];
    }

    if (self == sActiveMenuBar) {
        NSArray *args = [[NSArray alloc] initWithObjects:theMenu, [NSNumber numberWithInt:-1], nil];
        APPKIT_PERFORM(self, @selector(nativeAddMenuAtIndex_OnAppKitThread:), args);
        [args release];
    }
}

// This method is a special case for use by the screen menu bar.
// See ScreenMenuBar.java -- used to implement setVisible(boolean) by
// removing or adding the menu from the current menu bar's list.
-(void) javaAddMenu: (CMenu *)theMenu atIndex:(jint)index {
    @synchronized(self) {
        if (index == -1){
            [self->fMenuList addObject:theMenu];
        }else{
            [self->fMenuList insertObject:theMenu atIndex:index];
        }
    }

    if (self == sActiveMenuBar) {
        NSArray *args = [[NSArray alloc] initWithObjects:theMenu, [NSNumber numberWithInt:index], nil];
        APPKIT_PERFORM(self, @selector(nativeAddMenuAtIndex_OnAppKitThread:), args);
        [args release];
    }
}

- (void) javaDeleteMenu: (jint)index {
    if (self == sActiveMenuBar) {
        APPKIT_PERFORM(self, @selector(nativeDeleteMenu_OnAppKitThread:), [NSNumber numberWithInt:index]);
    }

    @synchronized(self) {
        CMenu *menuToRemove = [self->fMenuList objectAtIndex:index];

        if (menuToRemove == self->fHelpMenu) {
            [self->fHelpMenu release];
            self->fHelpMenu = nil;
        }

        [self->fMenuList removeObjectAtIndex:index];
    }
}

@end
