#ifndef __LOG_H
#define __LOG_H

#include <os/log.h>

#if OS_LOG_TARGET_HAS_10_12_FEATURES
#define OSLog(fmt, ...) \
    os_log_with_type(OS_LOG_DEFAULT, \
                     OS_LOG_TYPE_DEFAULT, \
                     "%{public}s", \
                     [[NSString stringWithFormat:fmt, ##__VA_ARGS__] UTF8String])
# else
#define OSLog(fmt, ...) NSLog(fmt, ##__VA_ARGS__)
# endif /* OS_LOG_TARGET_HAS_10_12_FEATURES */

#endif /* __LOG_H */
