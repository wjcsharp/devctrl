#ifndef __debug_h
#define __debug_h

#define TRACE_LEVEL_NONE        0   // Tracing is not on
#define TRACE_LEVEL_FATAL       1   // Abnormal exit or termination
#define TRACE_LEVEL_ERROR       2   // Severe errors that need logging
#define TRACE_LEVEL_WARNING     3   // Warnings such as allocation failure
#define TRACE_LEVEL_INFORMATION 4   // Includes non-error cases(e.g.,Entry-Exit)
#define TRACE_LEVEL_VERBOSE     5   // Detailed traces from intermediate steps

#define WPP_CHECK_FOR_NULL_STRING  //to prevent exceptions due to NULL strings

#define WPP_CONTROL_GUIDS \
	WPP_DEFINE_CONTROL_GUID(devctrlGuid,(11C89E0F, 1592, 4aaa, A638, 255777847216),  \
	WPP_DEFINE_BIT(DC_HIPS) \
	)

#define WPP_LEVEL_FLAGS_LOGGER(lvl,flags) WPP_LEVEL_LOGGER(flags)
#define WPP_LEVEL_FLAGS_ENABLED(lvl, flags) (WPP_LEVEL_ENABLED(flags) && WPP_CONTROL(WPP_BIT_ ## flags).Level  >= lvl)


#endif // __debug_h
