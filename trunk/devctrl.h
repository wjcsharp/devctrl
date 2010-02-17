#ifndef __devctrl_h
#define __devctrl_h

#include <ntddk.h>

//#include <wdmsec.h> // for IoCreateDeviceSecure
#include <initguid.h>
//#include <dontuse.h>
#include "debug.h"


// {05D4176B-81D5-4672-A543-D5539D29622B}  - generated using guidgen.exe
DEFINE_GUID (GUID_DEVCTRL_CONTROL_OBJECT,
        0x05D4176B, 0x81d5, 0x4672, 0xa5, 0x43, 0xd5, 0x53, 0x9d, 0x29, 0x62, 0x2b);


//
// GUID definition are required to be outside of header inclusion pragma to avoid
// error during precompiled headers.
//

extern PULONG InitSafeBootMode;
                 
#define DRIVERNAME "devctrl.sys upper: "
#define _ALLOC_TAG 'abos'

#define NTDEVICE_NAME_STRING      L"\\Device\\devctrl"
#define SYMBOLIC_NAME_STRING      L"\\DosDevices\\devctrl"


#ifndef FREE_POOL
#define FREE_POOL( _PoolPtr ) \
    if ( _PoolPtr ) \
    { \
        ExFreePool( _PoolPtr ); \
        _PoolPtr = NULL; \
    }
#endif // FREE_POOL

#ifndef CHECK_RETURN
#define CHECK_RETURN( _status, _ptr ) \
    if ( !NT_SUCCESS( _status )  ) \
    { \
        _ptr = NULL; \
    }
#endif // CHECK_RETURN


#define DEV_TYPE_OTHER							L"OtherTypes"
#define DEV_TYPE_USB_CLASS_RESERVED				L"USB\\CLASS_00"
#define DEV_TYPE_USB_CLASS_AUDIO				L"USB\\CLASS_01"
#define DEV_TYPE_USB_CLASS_COMMUNICATIONS		L"USB\\CLASS_02"
#define DEV_TYPE_USB_CLASS_HUMAN_INTERFACE		L"USB\\CLASS_03"
#define DEV_TYPE_USB_CLASS_MONITOR				L"USB\\CLASS_04"
#define DEV_TYPE_USB_CLASS_PHYSICAL_INTERFACE	L"USB\\CLASS_05"
#define DEV_TYPE_USB_CLASS_POWER				L"USB\\CLASS_06"
#define DEV_TYPE_USB_CLASS_PRINTER				L"USB\\CLASS_07"
#define DEV_TYPE_USB_CLASS_STORAGE				L"USB\\CLASS_08"
#define DEV_TYPE_USB_CLASS_HUB					L"USB\\CLASS_09"
#define DEV_TYPE_USB_CLASS_VENDOR_SPECIFIC		L"USB\\CLASS_FF"

//
// These are the states devctrl transition to upon
// receiving a specific PnP Irp. Refer to the PnP Device States
// diagram in DDK documentation for better understanding.
//

typedef enum _DEVICE_PNP_STATE {

    NotStarted = 0,         // Not started yet
    Started,                // Device has received the START_DEVICE IRP
    StopPending,            // Device has received the QUERY_STOP IRP
    Stopped,                // Device has received the STOP_DEVICE IRP
    RemovePending,          // Device has received the QUERY_REMOVE IRP
    SurpriseRemovePending,  // Device has received the SURPRISE_REMOVE IRP
    Deleted                 // Device has received the REMOVE_DEVICE IRP

} DEVICE_PNP_STATE;

#define INITIALIZE_PNP_STATE(_Data_)    \
        (_Data_)->DevicePnPState =  NotStarted;\
        (_Data_)->PreviousPnPState = NotStarted;

#define SET_NEW_PNP_STATE(_Data_, _state_) \
        (_Data_)->PreviousPnPState =  (_Data_)->DevicePnPState;\
        (_Data_)->DevicePnPState = (_state_);

#define RESTORE_PREVIOUS_PNP_STATE(_Data_)   \
        (_Data_)->DevicePnPState =   (_Data_)->PreviousPnPState;\

typedef enum _DEVICE_TYPE_SUBST {

    DEVICE_TYPE_INVALID = 0,         // Invalid Type;
    DEVICE_TYPE_FIDO,                // Device is a filter device.
    DEVICE_TYPE_CDO,                 // Device is a control device.

} DEVICE_TYPE_SUBST;

typedef struct _DEVICE_NAME
{
	UNICODE_STRING usGuid;
	UNICODE_STRING usDeviceType;

} DEVICE_NAME, *PDEVICE_NAME;

//
// A common header for the device extensions of the Filter and control
// device objects
//

typedef struct _COMMON_DEVICE_DATA
{

    DEVICE_TYPE_SUBST Type;

} COMMON_DEVICE_DATA, *PCOMMON_DEVICE_DATA;


typedef struct _DEVICE_EXTENSION
{
    COMMON_DEVICE_DATA m_CommonData;

    //
    // A back pointer to the device object.
    //

    PDEVICE_OBJECT  Self;

    //
    // The top of the stack before this filter was added.
    //

    PDEVICE_OBJECT  NextLowerDriver;

    //
    // current PnP state of the device
    //

    DEVICE_PNP_STATE  DevicePnPState;

    //
    // Remembers the previous pnp state
    //

    DEVICE_PNP_STATE    PreviousPnPState;

    //
    // Removelock to track IRPs so that device can be removed and
    // the driver can be unloaded safely.
    //
    IO_REMOVE_LOCK RemoveLock;

	DEVICE_NAME DevName;

} DEVICE_EXTENSION, *PDEVICE_EXTENSION;


extern "C"
DRIVER_INITIALIZE DriverEntry;

DRIVER_ADD_DEVICE FilterAddDevice;

DRIVER_DISPATCH FilterDispatchPnp;

DRIVER_DISPATCH FilterDispatchPower;

DRIVER_DISPATCH FilterPass;

DRIVER_UNLOAD FilterUnload;

IO_COMPLETION_ROUTINE FilterDeviceUsageNotificationCompletionRoutine;

NTSTATUS
FilterStartCompletionRoutine(
    __in PDEVICE_OBJECT   DeviceObject,
    __in PIRP             Irp,
    __in PVOID            Context
    );


PCHAR
PnPMinorFunctionString (
    UCHAR MinorFunction
);

typedef struct _CONTROL_DEVICE_EXTENSION {

    COMMON_DEVICE_DATA m_CommonData;

    ULONG   Deleted; // False if the deviceobject is valid, TRUE if it's deleted
	
	UNICODE_STRING usRegistryPath;

    PVOID   ControlData; // Store your control data here

} CONTROL_DEVICE_EXTENSION, *PCONTROL_DEVICE_EXTENSION;

NTSTATUS
FilterCreateControlObject(
	//__in PDEVICE_OBJECT    DeviceObject
	__in PUNICODE_STRING RegistryPath,
	__in PDRIVER_OBJECT  DriverObject
);

VOID
FilterDeleteControlObject(
    );

DRIVER_DISPATCH FilterDispatchIo;

typedef struct _GLOBAL_DATA {

	//  Control Device Object for this filter
	PDEVICE_OBJECT		m_CDO;

    FAST_MUTEX          m_ControlMutex;
    ULONG               m_InstanceCount;

    DECLSPEC_ALIGN( MEMORY_ALLOCATION_ALIGNMENT )
    ERESOURCE           m_DeviceListLock;

    LIST_ENTRY          m_DeviceList;
} GLOBAL_DATA, *PGLOBAL_DATA;

extern GLOBAL_DATA Globals;

BOOLEAN
AcquireResourceExclusive (
    __inout PERESOURCE Resource
    );

VOID
ReleaseResource (
    __inout PERESOURCE Resource
    );

#endif


