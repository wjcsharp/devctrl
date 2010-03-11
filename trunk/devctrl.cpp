#include "devctrl.h"

#pragma prefast(push)
#pragma prefast(disable: 6011)
#pragma prefast(disable: 6387)
#include "devctrl.tmh"
#pragma prefast(pop)

#include "devlist.h"

GLOBAL_DATA Globals;

PWCHAR DevStrings[] = {
    DEV_TYPE_USB_CLASS_RESERVED,
    DEV_TYPE_USB_CLASS_AUDIO,
    DEV_TYPE_USB_CLASS_COMMUNICATIONS,
    DEV_TYPE_USB_CLASS_HUMAN_INTERFACE,
    DEV_TYPE_USB_CLASS_MONITOR,
    DEV_TYPE_USB_CLASS_PHYSICAL_INTERFACE,
    DEV_TYPE_USB_CLASS_POWER,   
    DEV_TYPE_USB_CLASS_PRINTER,
    DEV_TYPE_USB_CLASS_STORAGE,
    DEV_TYPE_USB_CLASS_HUB,
    DEV_TYPE_USB_CLASS_VENDOR_SPECIFIC,
};

BOOLEAN
__drv_acquiresCriticalRegion
AcquireResourceExclusive (
    __inout __drv_acquiresResource( ExResourceType ) PERESOURCE pResource
    )
{
    BOOLEAN ret;
    ASSERT( ARGUMENT_PRESENT( pResource ) );
#pragma prefast(suppress: 28103)
    KeEnterCriticalRegion();
    ret = ExAcquireResourceExclusiveLite( pResource, TRUE );

    return ret;
}

VOID
__drv_releasesCriticalRegion
__drv_mustHoldCriticalRegion
ReleaseResource (
    __inout __drv_releasesResource( ExResourceType) PERESOURCE pResource
    )
{
    ASSERT( ARGUMENT_PRESENT( pResource ) );

    ExReleaseResourceLite( pResource );
    KeLeaveCriticalRegion();
}

extern "C"
NTSTATUS 
DriverEntry (
    __in PDRIVER_OBJECT  DriverObject,
    __in PUNICODE_STRING RegistryPath
    )
{
    NTSTATUS            status = STATUS_SUCCESS;
    PDRIVER_DISPATCH    *dispatch;
    UNICODE_STRING        parameters;

    WPP_INIT_TRACING( DriverObject, RegistryPath );
    RtlZeroMemory( &Globals, sizeof( Globals ) );

    ExInitializeResourceLite( &Globals.m_DeviceListLock );
    InitializeListHead( &Globals.m_DeviceList );

    for ( ULONG ulIndex = 0; ulIndex <= IRP_MJ_MAXIMUM_FUNCTION; ulIndex++ )
    {
#pragma prefast(suppress: 28169, "__drv_dispatchType ignored") 
        DriverObject->MajorFunction[ulIndex] = FilterPass;
    }

    DriverObject->MajorFunction[IRP_MJ_PNP]     = FilterDispatchPnp;
    DriverObject->MajorFunction[IRP_MJ_POWER]   = FilterDispatchPower;
    DriverObject->DriverExtension->AddDevice    = FilterAddDevice;
    DriverObject->DriverUnload                  = FilterUnload;

    // filtering points
    
    DriverObject->MajorFunction[IRP_MJ_CREATE]  =
    DriverObject->MajorFunction[IRP_MJ_CLOSE]   =
    DriverObject->MajorFunction[IRP_MJ_CLEANUP] =
    DriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL] =
    DriverObject->MajorFunction[IRP_MJ_INTERNAL_DEVICE_CONTROL] =
                                                        FilterDispatchIo;
    
    ExInitializeFastMutex( &Globals.m_ControlMutex );
    FilterCreateControlObject( RegistryPath, DriverObject );
    
    return status;
}

__checkReturn
NTSTATUS
GetClassGuidName (
    __in PDEVICE_OBJECT PhysicalDeviceObject,
    __deref_out_opt PWCHAR *pwcGuidName
    )
{
    NTSTATUS status = STATUS_UNSUCCESSFUL;
    ULONG retSize = 0;

    ASSERT( ARGUMENT_PRESENT( PhysicalDeviceObject ) );
    ASSERT( ARGUMENT_PRESENT( pwcGuidName ) );

    status = IoGetDeviceProperty (
        PhysicalDeviceObject, 
        DevicePropertyClassGuid, 
        0,
        *pwcGuidName,
        &retSize
        );

    if ( STATUS_BUFFER_TOO_SMALL != status )
    {
        return status;
    }

    *pwcGuidName = (PWCHAR) ExAllocatePoolWithTag (
        PagedPool,
        retSize,
        _ALLOC_TAG
        );
    
    if ( !*pwcGuidName )
    {
        return STATUS_INSUFFICIENT_RESOURCES;
    }

    status = IoGetDeviceProperty (
        PhysicalDeviceObject, 
        DevicePropertyClassGuid, 
        retSize,
        *pwcGuidName,
        &retSize
        );

    if ( !NT_SUCCESS( status ) )
    {
        FREE_POOL( pwcGuidName );
    }

    return status;
}

//! \todo - switch to UNICODE_STRING
int
TypeCompare (
    __in PWCHAR wcGuidType,
    __in PWCHAR wcDevType
    )
{
    ASSERT( ARGUMENT_PRESENT( wcGuidType ) );
    ASSERT( ARGUMENT_PRESENT( wcDevType ) );
    
    ULONG wcGuidTypeLen;
    ULONG wcDevTypeLen;

    wcGuidTypeLen = (ULONG)wcslen( wcGuidType );
    wcDevTypeLen = (ULONG)wcslen( wcDevType );

    if ( wcDevTypeLen > wcGuidTypeLen )
    {
        return 1;
    }

    return _wcsnicmp( wcGuidType, wcDevType, wcDevTypeLen );
}

PWCHAR
GetDevType (
    __in PWCHAR wcGuidType
    )
{
    ASSERT( ARGUMENT_PRESENT( wcGuidType ) );

    for ( int cou = 0; cou < RTL_NUMBER_OF( DevStrings ); cou++ )
    {
        if ( !TypeCompare( wcGuidType, DevStrings[cou] ) )
        {
            return DevStrings[ cou ];
        }
    }

    return DEV_TYPE_OTHER;
}

__checkReturn
NTSTATUS
IopSynchronousCall(
    __in PDEVICE_OBJECT DeviceObject,
    __in PIO_STACK_LOCATION TopStackLocation,
    __deref_out_opt PVOID *Information
    )
{
    PIRP irp;
    PIO_STACK_LOCATION irpSp;
    IO_STATUS_BLOCK statusBlock;
    KEVENT event;
    NTSTATUS status;
    PULONG_PTR returnInfo = (PULONG_PTR) Information;

    ASSERT( ARGUMENT_PRESENT( DeviceObject ) );
    ASSERT( ARGUMENT_PRESENT( TopStackLocation ) );

    KeInitializeEvent (
        &event,
        SynchronizationEvent,
        FALSE
        );

      irp = IoBuildSynchronousFsdRequest (
            IRP_MJ_PNP,
            DeviceObject,
            NULL,
            0,
            NULL,
            &event,
            &statusBlock
            );

      if ( !irp )
      {
            __debugbreak();
            return STATUS_INSUFFICIENT_RESOURCES;
      }

      irpSp = IoGetNextIrpStackLocation( irp );

      // Copy in the caller-supplied stack location contents
      *irpSp = *TopStackLocation;

      status = IoCallDriver( DeviceObject, irp );

    if ( STATUS_PENDING == status )
    {
        KeWaitForSingleObject (
            &event,
            Executive,
            KernelMode,
            FALSE,
            (PLARGE_INTEGER) NULL
            );

        status = statusBlock.Status;
    }

    if ( NT_SUCCESS( status ) )
    {
        *returnInfo = (ULONG_PTR) statusBlock.Information;
    }

    return status;
}

__checkReturn
NTSTATUS
IopQueryBus (
    __in PDEVICE_OBJECT DeviceObject,
    __in BUS_QUERY_ID_TYPE IdType,
    __deref_out_opt PVOID *UniqueId
    )
{
    IO_STACK_LOCATION irpSp;
    NTSTATUS status;

    ASSERT( ARGUMENT_PRESENT( DeviceObject ) );

    RtlZeroMemory( &irpSp, sizeof( IO_STACK_LOCATION ) );

    irpSp.MajorFunction = IRP_MJ_PNP;
    irpSp.MinorFunction = IRP_MN_QUERY_ID;

    irpSp.Parameters.QueryId.IdType = IdType;

    status = IopSynchronousCall( DeviceObject, &irpSp, UniqueId );

    return status;
}

__checkReturn
NTSTATUS
IopQueryDeviceText (
    __in PDEVICE_OBJECT DeviceObject,
    __deref_out_opt PVOID *Description
    )
{
    LCID PsDefaultSystemLocaleId = 0x00000409;
    IO_STACK_LOCATION irpSp;
    NTSTATUS status;

    RtlZeroMemory( &irpSp, sizeof( IO_STACK_LOCATION ) );

    irpSp.MajorFunction = IRP_MJ_PNP;
    irpSp.MinorFunction = IRP_MN_QUERY_DEVICE_TEXT;
    irpSp.Parameters.QueryDeviceText.DeviceTextType = DeviceTextDescription;
    irpSp.Parameters.QueryDeviceText.LocaleId = PsDefaultSystemLocaleId;
    status = IopSynchronousCall( DeviceObject, &irpSp, Description );

    return status;
}

__checkReturn
NTSTATUS
GetClassGuidType (
    __in PDEVICE_OBJECT PhysicalDeviceObject,
    __deref_out_opt PWCHAR *pwcGuidType
    )
{
    //! query device info function
    NTSTATUS status = STATUS_UNSUCCESSFUL;
    ULONG retSize = 0;
    PWCHAR buf = NULL;
        
    PWCHAR wcdevType = NULL;
    PWCHAR id = NULL;

    PWCHAR InstanceID = NULL,
        DeviceID = NULL,
        SerialNumber = NULL,
        HardwareIDs = NULL,
        CompatibleIDs = NULL,
        Description = NULL;

    __debugbreak();

    status = IopQueryBus(
        PhysicalDeviceObject,
        BusQueryInstanceID,
        (PVOID*) &InstanceID
        );
    
    CHECK_RETURN( status, InstanceID );

    status = IopQueryBus(
        PhysicalDeviceObject,
        BusQueryDeviceID,
        (PVOID*) &DeviceID
        );
   
    CHECK_RETURN( status, DeviceID );

    status = IopQueryBus(
        PhysicalDeviceObject,
        BusQueryDeviceSerialNumber,
        (PVOID*) &SerialNumber
        );

    CHECK_RETURN( status, SerialNumber );

    status = IopQueryBus(
        PhysicalDeviceObject,
        BusQueryHardwareIDs,
        (PVOID*) &HardwareIDs
        );
    
    CHECK_RETURN( status, HardwareIDs );

    status = IopQueryBus(
        PhysicalDeviceObject,
        BusQueryCompatibleIDs,
        (PVOID*) &CompatibleIDs
        );

    CHECK_RETURN( status, CompatibleIDs );

    status = IopQueryDeviceText (
        PhysicalDeviceObject,
        (PVOID*) &Description
        );

    CHECK_RETURN( status, Description );

    FREE_POOL( InstanceID );
    FREE_POOL( DeviceID );
    FREE_POOL( SerialNumber );
    FREE_POOL( HardwareIDs );
    FREE_POOL( CompatibleIDs );
    FREE_POOL( Description );

    status = IoGetDeviceProperty (
        PhysicalDeviceObject, 
        DevicePropertyCompatibleIDs, 
        0,
        buf,
        &retSize
        );

    if ( STATUS_BUFFER_TOO_SMALL != status )
    {
        return status;
    }

    buf = (PWCHAR) ExAllocatePoolWithTag( PagedPool, retSize, _ALLOC_TAG );
    if (!buf)
    {
        return STATUS_NO_MEMORY;
    }

    status = IoGetDeviceProperty (
        PhysicalDeviceObject, 
        DevicePropertyCompatibleIDs, 
        retSize,
        buf,
        &retSize
        );

    if ( !NT_SUCCESS( status ) )
    {
        FREE_POOL( buf );
        return status;
    }

    wcdevType = GetDevType( buf );
    FREE_POOL( buf );
    buf = NULL;
    
    retSize = (ULONG)( (wcslen(wcdevType)+1)*sizeof(WCHAR) );
    *pwcGuidType = (PWCHAR) ExAllocatePoolWithTag (
        PagedPool,
        retSize,
        _ALLOC_TAG
        );

    if ( !(*pwcGuidType) )
    {
        return STATUS_NO_MEMORY;
    }

    //! \todo remove zeromemory
    RtlZeroMemory( *pwcGuidType, retSize );
    RtlCopyMemory( *pwcGuidType, wcdevType, retSize );

    return STATUS_SUCCESS;
}

__checkReturn
NTSTATUS
QueryCapabilities (
    __in PDEVICE_OBJECT pDevice
    )
{
    PIRP Irp;
    PIO_STACK_LOCATION irpSp;
    IO_STATUS_BLOCK statusBlock;
    KEVENT event;
    NTSTATUS status;

    DEVICE_CAPABILITIES capabilities;
    RtlZeroMemory( &capabilities, sizeof( capabilities ) );

    KeInitializeEvent (
        &event,
        SynchronizationEvent,
        FALSE
        );

    __try
    {
        Irp = IoBuildSynchronousFsdRequest(
            IRP_MJ_PNP,
            pDevice,
            NULL,
            0,
            NULL,
            &event,
            &statusBlock
            );

        if ( !Irp )
        {
            status = STATUS_INSUFFICIENT_RESOURCES;
            __leave;
        }

        KeInitializeEvent (
            &event,
            SynchronizationEvent,
            FALSE
            );

        irpSp = IoGetNextIrpStackLocation( Irp );

        irpSp->MinorFunction = IRP_MN_QUERY_CAPABILITIES;
        irpSp->Parameters.DeviceCapabilities.Capabilities = &capabilities;

        status = IoCallDriver( pDevice, Irp );

        if ( STATUS_PENDING == status)
        {
            KeWaitForSingleObject (
                &event,
                Executive,
                KernelMode,
                FALSE,
                (PLARGE_INTEGER) NULL
                );

            status = statusBlock.Status;
        }

        if ( NT_SUCCESS( status ) )
        {
            if ( statusBlock.Information )
            {
                //! \todo save UiniqueID
            }
            else
            {
                status = STATUS_UNSUCCESSFUL;
            }
        }
    }
    __finally
    {
    }

    return status;
}

#pragma prefast(push)
#pragma prefast(disable: 28152, "DO_DEVICE_INITIALIZING flag")
NTSTATUS
FilterAddDevice (
    __in PDRIVER_OBJECT DriverObject,
    __in PDEVICE_OBJECT PhysicalDeviceObject
    )
#pragma prefast(pop)
/*++

Routine Description:

    The Plug & Play subsystem is handing us a brand new PDO, for which we
    (by means of INF registration) have been asked to provide a driver.

    We need to determine if we need to be in the driver stack for the device.
    Create a function device object to attach to the stack
    Initialize that device object
    Return status success.

    Remember: We can NOT actually send ANY non pnp IRPS to the given driver
    stack, UNTIL we have received an IRP_MN_START_DEVICE.

Arguments:

    DeviceObject - pointer to a device object.

    PhysicalDeviceObject -  pointer to a device object created by the
                            underlying bus driver.

Return Value:

    NT status code.

--*/
{
    NTSTATUS                status = STATUS_SUCCESS;
    PDEVICE_OBJECT          deviceObject = NULL;
    PDEVICE_EXTENSION       deviceExtension;
    ULONG                   deviceType = FILE_DEVICE_UNKNOWN;

    //
    // Create a filter device object.
    //

    if ( *InitSafeBootMode > 0 )
    {
        return STATUS_SUCCESS;
    }

    status = IoCreateDevice (
        DriverObject,
        sizeof ( DEVICE_EXTENSION ),
        NULL,  // No Name
        deviceType,
        FILE_DEVICE_SECURE_OPEN,
        FALSE,
        &deviceObject
        );


    if ( !NT_SUCCESS( status ) )
    {
        //
        // Returning failure here prevents the entire stack from functioning,
        // but most likely the rest of the stack will not be able to create
        // device objects either, so it is still OK.
        //
        return status;
    }

    deviceExtension = (PDEVICE_EXTENSION) deviceObject->DeviceExtension;

    deviceExtension->m_CommonData.Type = DEVICE_TYPE_FIDO;

    PWCHAR wcStr = NULL;
    BOOLEAN needLog = FALSE;

    RtlZeroMemory( &deviceExtension->DevName, sizeof( DEVICE_NAME ) );
    status = GetClassGuidName( PhysicalDeviceObject, &wcStr );
    if ( !NT_SUCCESS( status ) )
    {
        IoDeleteDevice( deviceObject );

        return STATUS_SUCCESS;
    }

    RtlInitUnicodeString( &deviceExtension->DevName.usGuid, wcStr );

    status = GetClassGuidType( PhysicalDeviceObject, &wcStr );
    if ( !NT_SUCCESS( status ) )
    {

        ULONG wcStrSize = (ULONG)( wcslen( DEV_TYPE_OTHER ) *
            sizeof( WCHAR ) + sizeof( WCHAR ) );

        wcStr = (PWCHAR) ExAllocatePoolWithTag (
            PagedPool,
            wcStrSize,
            _ALLOC_TAG
            );

        if ( !wcStr )
        {
            if ( deviceExtension->DevName.usGuid.Buffer )
            {
                FREE_POOL( deviceExtension->DevName.usGuid.Buffer );
            }

            IoDeleteDevice( deviceObject );

            return STATUS_SUCCESS;
        }

        //! \todo - remove zeromemory
        RtlZeroMemory( wcStr, wcStrSize );
        RtlCopyMemory( wcStr, DEV_TYPE_OTHER, wcStrSize );
    }

    RtlInitUnicodeString( &deviceExtension->DevName.usDeviceType, wcStr );

    status = QueryCapabilities( PhysicalDeviceObject );
    if ( !NT_SUCCESS ( status ) )
    {
        ASSERT( !NT_SUCCESS( status ) );
    }

    //если доступ к устройству разрешен и логировать не нужно то вообще не цепляемся к устройству  
    /*if ( IsAllowAccess( deviceExtension->DevName.usGuid, deviceExtension->DevName.usDeviceType, &needLog ) )
    {
        if( !needLog )
        {
            if ( deviceExtension->DevName.usGuid.Buffer )
                {
                FREE_POOL( deviceExtension->DevName.usGuid.Buffer );
                }

            if ( deviceExtension->DevName.usGuid.Buffer )
                {
                FREE_POOL( deviceExtension->DevName.usDeviceType.Buffer );
                }

            IoDeleteDevice(deviceObject);

            return STATUS_SUCCESS;
        }
    }*/

    status = InsertDeviceList( &deviceExtension->DevName );
    if ( !NT_SUCCESS( status ) )
    {
        if ( deviceExtension->DevName.usGuid.Buffer )
        {
            FREE_POOL( deviceExtension->DevName.usGuid.Buffer );
        }

        if ( deviceExtension->DevName.usGuid.Buffer )
        {
            FREE_POOL( deviceExtension->DevName.usDeviceType.Buffer );
        }

        IoDeleteDevice(deviceObject);

        //если не можем добавить элемент, то не фильтруем устройство, а система должна продолжать работать
        return STATUS_SUCCESS;
    }

    deviceExtension->NextLowerDriver = IoAttachDeviceToDeviceStack (
                                       deviceObject,
                                       PhysicalDeviceObject);
    //
    // Failure for attachment is an indication of a broken plug & play system.
    //

    if ( !deviceExtension->NextLowerDriver )
    {
        IoDeleteDevice( deviceObject );

        return STATUS_UNSUCCESSFUL;
    }

    deviceObject->Flags |= deviceExtension->NextLowerDriver->Flags &
                            (DO_BUFFERED_IO | DO_DIRECT_IO |
                            DO_POWER_PAGABLE );


    deviceObject->DeviceType = deviceExtension->NextLowerDriver->DeviceType;

    deviceObject->Characteristics =
                          deviceExtension->NextLowerDriver->Characteristics;

    deviceExtension->Self = deviceObject;

    //
    // Let us use remove lock to keep count of IRPs so that we don't 
    // deteach and delete our deviceobject until all pending I/Os in our
    // devstack are completed. Remlock is required to protect us from
    // various race conditions where our driver can get unloaded while we
    // are still running dispatch or completion code.
    //
    IoInitializeRemoveLock (
        &deviceExtension->RemoveLock,
        _ALLOC_TAG,
        1, // MaxLockedMinutes 
        100
        );

    INITIALIZE_PNP_STATE( deviceExtension );

    deviceObject->Flags &= ~DO_DEVICE_INITIALIZING;

    return STATUS_SUCCESS;
}

__checkReturn
NTSTATUS
FilterPass (
    __in PDEVICE_OBJECT DeviceObject,
    __in PIRP Irp
    )
{
    PDEVICE_EXTENSION   deviceExtension;
    NTSTATUS            status;

    deviceExtension = (PDEVICE_EXTENSION) DeviceObject->DeviceExtension;
    status = IoAcquireRemoveLock( &deviceExtension->RemoveLock, Irp );
    if ( !NT_SUCCESS( status ) )
    {
        Irp->IoStatus.Status = status;
        IoCompleteRequest( Irp, IO_NO_INCREMENT );

        return status;
    }

    IoSkipCurrentIrpStackLocation ( Irp );
    status = IoCallDriver( deviceExtension->NextLowerDriver, Irp );
    IoReleaseRemoveLock( &deviceExtension->RemoveLock, Irp );

    return status;
}

__checkReturn
NTSTATUS
FilterDispatchPnp (
    __in PDEVICE_OBJECT DeviceObject,
    __in PIRP Irp
    )
{
    PDEVICE_EXTENSION           deviceExtension;
    PIO_STACK_LOCATION         irpStack;
    NTSTATUS                            status;
    KEVENT                               event;

    deviceExtension = (PDEVICE_EXTENSION) DeviceObject->DeviceExtension;
    irpStack = IoGetCurrentIrpStackLocation( Irp );

    status = IoAcquireRemoveLock( &deviceExtension->RemoveLock, Irp );
    if ( !NT_SUCCESS( status ) )
    {
        Irp->IoStatus.Status = status;
        IoCompleteRequest( Irp, IO_NO_INCREMENT );

        return status;
    }

    switch ( irpStack->MinorFunction )
    {
    case IRP_MN_START_DEVICE:

        //
        // The device is starting.
        // We cannot touch the device (send it any non pnp irps) until a
        // start device has been passed down to the lower drivers.
        //
        KeInitializeEvent( &event, NotificationEvent, FALSE);
        IoCopyCurrentIrpStackLocationToNext( Irp );
        IoSetCompletionRoutine(Irp,
            FilterStartCompletionRoutine,
            &event,
            TRUE,
            TRUE,
            TRUE);

        status = IoCallDriver(deviceExtension->NextLowerDriver, Irp);

        //
        // Wait for lower drivers to be done with the Irp. Important thing to
        // note here is when you allocate memory for an event in the stack  
        // you must do a KernelMode wait instead of UserMode to prevent 
        // the stack from getting paged out.
        //
        if ( STATUS_PENDING == status )
        {
            KeWaitForSingleObject( &event, Executive, KernelMode, FALSE, NULL );
            status = Irp->IoStatus.Status;
        }

        /*if ( !IsAllowAccess( deviceExtension->DevName.usGuid, deviceExtension->DevName.usDeviceType, NULL ) )
        {
        status = STATUS_ACCESS_DENIED;
        }*/

        if ( NT_SUCCESS( status ) )
        {
            SET_NEW_PNP_STATE( deviceExtension, Started );
            // On the way up inherit FILE_REMOVABLE_MEDIA during Start.
            // This characteristic is available only after the driver 
            // stack is started!.
            if ( FlagOn (
                deviceExtension->NextLowerDriver->Characteristics,
                FILE_REMOVABLE_MEDIA
                ) )
            {
                SetFlag( DeviceObject->Characteristics, FILE_REMOVABLE_MEDIA );
            }
        }

        Irp->IoStatus.Status = status;
        IoCompleteRequest( Irp, IO_NO_INCREMENT );
        IoReleaseRemoveLock( &deviceExtension->RemoveLock, Irp );

        return status;

    case IRP_MN_REMOVE_DEVICE:
        // Wait for all outstanding requests to complete
        IoReleaseRemoveLockAndWait( &deviceExtension->RemoveLock, Irp );

        IoSkipCurrentIrpStackLocation( Irp );

        status = IoCallDriver( deviceExtension->NextLowerDriver, Irp );

        SET_NEW_PNP_STATE( deviceExtension, Deleted );

        RemItemFromDeviceList( &deviceExtension->DevName );
        FREE_POOL( deviceExtension->DevName.usDeviceType.Buffer );
        FREE_POOL( deviceExtension->DevName.usGuid.Buffer );

        IoDetachDevice( deviceExtension->NextLowerDriver );
        IoDeleteDevice( DeviceObject );

        return status;


    case IRP_MN_QUERY_STOP_DEVICE:
        SET_NEW_PNP_STATE( deviceExtension, StopPending );
        status = STATUS_SUCCESS;

        break;

    case IRP_MN_CANCEL_STOP_DEVICE:
        // Check to see whether you have received cancel-stop
        // without first receiving a query-stop. This could happen if someone
        // above us fails a query-stop and passes down the subsequent
        // cancel-stop.
        if ( StopPending == deviceExtension->DevicePnPState )
        {
            // We did receive a query-stop, so restore.
            RESTORE_PREVIOUS_PNP_STATE( deviceExtension );
        }
        status = STATUS_SUCCESS; // We must not fail this IRP.
        
        break;

    case IRP_MN_STOP_DEVICE:
        SET_NEW_PNP_STATE( deviceExtension, Stopped );
        status = STATUS_SUCCESS;

        break;

    case IRP_MN_QUERY_REMOVE_DEVICE:

        SET_NEW_PNP_STATE( deviceExtension, RemovePending );
        status = STATUS_SUCCESS;

        break;

    case IRP_MN_SURPRISE_REMOVAL:
        SET_NEW_PNP_STATE( deviceExtension, SurpriseRemovePending );
        status = STATUS_SUCCESS;

        break;

    case IRP_MN_CANCEL_REMOVE_DEVICE:
        // Check to see whether you have received cancel-remove
        // without first receiving a query-remove. This could happen if
        // someone above us fails a query-remove and passes down the
        // subsequent cancel-remove.
        if ( RemovePending == deviceExtension->DevicePnPState )
        {
            // We did receive a query-remove, so restore.
            RESTORE_PREVIOUS_PNP_STATE( deviceExtension );
        }

        status = STATUS_SUCCESS; // We must not fail this IRP.

        break;

    case IRP_MN_DEVICE_USAGE_NOTIFICATION:
        // On the way down, pagable might become set. Mimic the driver
        // above us. If no one is above us, just set pagable.
        if ( !DeviceObject->AttachedDevice
            ||
            FlagOn( DeviceObject->AttachedDevice->Flags, DO_POWER_PAGABLE )
            )
        {
            SetFlag( DeviceObject->Flags, DO_POWER_PAGABLE );
        }

        IoCopyCurrentIrpStackLocationToNext( Irp );

        IoSetCompletionRoutine(
            Irp,
            FilterDeviceUsageNotificationCompletionRoutine,
            NULL,
            TRUE,
            TRUE,
            TRUE
            );

        return IoCallDriver( deviceExtension->NextLowerDriver, Irp );

    default:
        // If you don't handle any IRP you must leave the
        // status as is.
        status = Irp->IoStatus.Status;

        break;
    }

    // Pass the IRP down and forget it.
    Irp->IoStatus.Status = status;
    IoSkipCurrentIrpStackLocation( Irp );
    status = IoCallDriver( deviceExtension->NextLowerDriver, Irp );
    IoReleaseRemoveLock( &deviceExtension->RemoveLock, Irp );

    return status;
}

__checkReturn
NTSTATUS
FilterStartCompletionRoutine (
    __in PDEVICE_OBJECT   DeviceObject,
    __in PIRP             Irp,
    __in PVOID            Context
    )
{
    PKEVENT event = (PKEVENT) Context;

    UNREFERENCED_PARAMETER( DeviceObject );

    // If the lower driver didn't return STATUS_PENDING, we don't need to 
    // set the event because we won't be waiting on it. 
    // This optimization avoids grabbing the dispatcher lock, and improves perf.
    if ( Irp->PendingReturned )
    {
        ASSERT( event );

        if ( event )
        {
            KeSetEvent( event, IO_NO_INCREMENT, FALSE );
        }
    }

    // The dispatch routine will have to call IoCompleteRequest
    return STATUS_MORE_PROCESSING_REQUIRED;
}

__checkReturn
NTSTATUS
FilterDeviceUsageNotificationCompletionRoutine (
    __in PDEVICE_OBJECT   DeviceObject,
    __in PIRP             Irp,
    __in PVOID            Context
    )
{
    PDEVICE_EXTENSION       deviceExtension;

    UNREFERENCED_PARAMETER( Context );

    deviceExtension = (PDEVICE_EXTENSION) DeviceObject->DeviceExtension;

    if ( Irp->PendingReturned )
    {
        IoMarkIrpPending( Irp );
    }

    // On the way up, pagable might become clear. Mimic the driver below us.
    if ( !FlagOn( deviceExtension->NextLowerDriver->Flags, DO_POWER_PAGABLE ) )
    {
        ClearFlag( DeviceObject->Flags, DO_POWER_PAGABLE );
    }

    IoReleaseRemoveLock( &deviceExtension->RemoveLock, Irp );

    return STATUS_CONTINUE_COMPLETION;
}

__checkReturn
NTSTATUS
FilterDispatchPower(
    __in PDEVICE_OBJECT    DeviceObject,
    __in PIRP              Irp
    )
{
    PDEVICE_EXTENSION   deviceExtension;
    NTSTATUS    status;

    deviceExtension = (PDEVICE_EXTENSION) DeviceObject->DeviceExtension;
    status = IoAcquireRemoveLock( &deviceExtension->RemoveLock, Irp );
    if ( !NT_SUCCESS( status ) )
    {
        // may be device is being removed.
        Irp->IoStatus.Status = status;
        PoStartNextPowerIrp( Irp );
        IoCompleteRequest( Irp, IO_NO_INCREMENT );

        return status;
    }

    PoStartNextPowerIrp( Irp );
    IoSkipCurrentIrpStackLocation( Irp );
    status = PoCallDriver(deviceExtension->NextLowerDriver, Irp );
    IoReleaseRemoveLock( &deviceExtension->RemoveLock, Irp );

    return status;
}

VOID
FilterUnload(
    __in PDRIVER_OBJECT DriverObject
    )
{
    ASSERT( !DriverObject->DeviceObject );

    // We should not be unloaded until all the devices we control
    // have been removed from our queue.
    FilterDeleteControlObject();

    ExDeleteResourceLite( &Globals.m_DeviceListLock );

    return;
}

__checkReturn
NTSTATUS
FilterCreateControlObject (
    __in PUNICODE_STRING RegistryPath,
    __in PDRIVER_OBJECT  DriverObject
)
{
    UNICODE_STRING      ntDeviceName;
    UNICODE_STRING      symbolicLinkName;
    PCONTROL_DEVICE_EXTENSION   deviceExtension;
    NTSTATUS status = STATUS_UNSUCCESSFUL;
    UNICODE_STRING  sddlString;    

    //
    // Using unsafe function so that the IRQL remains at PASSIVE_LEVEL.
    // IoCreateDeviceSecure & IoCreateSymbolicLink must be called at
    // PASSIVE_LEVEL.
    //

    ExAcquireFastMutexUnsafe( &Globals.m_ControlMutex );

    __try
    {
        //
        // If this is a first instance of the device, then create a controlobject
        // and register dispatch points to handle ioctls.
        //
        if ( 1 != ++Globals.m_InstanceCount )
        {
            __leave;
        }
        RtlInitUnicodeString( &ntDeviceName, NTDEVICE_NAME_STRING );
        RtlInitUnicodeString( &symbolicLinkName, SYMBOLIC_NAME_STRING );

        status = IoCreateDevice (
            DriverObject,
            sizeof( CONTROL_DEVICE_EXTENSION ),
            &ntDeviceName,
            FILE_DEVICE_UNKNOWN,
            0,
            FALSE,
            &Globals.m_CDO
            );

        if ( NT_SUCCESS( status ) )
        {
            SetFlag( Globals.m_CDO->Flags, DO_BUFFERED_IO );

            status = IoCreateSymbolicLink( &symbolicLinkName, &ntDeviceName );

            if ( !NT_SUCCESS( status ) )
            {
                IoDeleteDevice( Globals.m_CDO );
                
                __leave;
            }

            deviceExtension = (PCONTROL_DEVICE_EXTENSION) 
                Globals.m_CDO->DeviceExtension;
            deviceExtension->m_CommonData.Type = DEVICE_TYPE_CDO;
            deviceExtension->m_ControlData = NULL;
            deviceExtension->m_Deleted = FALSE;
            
            RtlZeroMemory (
                &deviceExtension->m_usRegistryPath,
                sizeof( UNICODE_STRING )
                );

            deviceExtension->m_usRegistryPath.Buffer = (PWCH) ExAllocatePoolWithTag (
                PagedPool,
                RegistryPath->Length,
                _ALLOC_TAG
                );

            if ( deviceExtension->m_usRegistryPath.Buffer )
            {
                deviceExtension->m_usRegistryPath.Length = 0;
                deviceExtension->m_usRegistryPath.MaximumLength =
                    RegistryPath->MaximumLength;

                RtlCopyUnicodeString (
                    &deviceExtension->m_usRegistryPath,
                    RegistryPath
                    );
            }
            ClearFlag( Globals.m_CDO->Flags, DO_DEVICE_INITIALIZING );
        }
    }
    __finally
    {
        ExReleaseFastMutexUnsafe( &Globals.m_ControlMutex );
    }

    return status;
}

VOID
FilterDeleteControlObject (
)
{
    UNICODE_STRING      symbolicLinkName;
    PCONTROL_DEVICE_EXTENSION   deviceExtension;

    ExAcquireFastMutexUnsafe( &Globals.m_ControlMutex );

    //
    // If this is the last instance of the device then delete the controlobject
    // and symbolic link to enable the pnp manager to unload the driver.
    //

    if ( !(--Globals.m_InstanceCount) && Globals.m_CDO )
    {
        RtlInitUnicodeString( &symbolicLinkName, SYMBOLIC_NAME_STRING);
        deviceExtension = (PCONTROL_DEVICE_EXTENSION) Globals.m_CDO->DeviceExtension;
        deviceExtension->m_Deleted = TRUE;

        FREE_POOL( deviceExtension->m_usRegistryPath.Buffer );

        IoDeleteSymbolicLink( &symbolicLinkName );
        IoDeleteDevice( Globals.m_CDO );
        Globals.m_CDO = NULL;
    }

    ExReleaseFastMutexUnsafe( &Globals.m_ControlMutex );
}

__checkReturn
BOOLEAN
IsGetMediaSerialNmberRequest (
    PIO_STACK_LOCATION IrpStack
    )
{
    if (IRP_MJ_DEVICE_CONTROL != IrpStack->MajorFunction )
    {
        return FALSE;
    }

    if ( IRP_MJ_INTERNAL_DEVICE_CONTROL != IrpStack->MajorFunction )
    {
        return FALSE;
    }

    if (IOCTL_STORAGE_GET_MEDIA_SERIAL_NUMBER != 
            IrpStack->Parameters.DeviceIoControl.IoControlCode )
    {
        return FALSE;
    }

    return TRUE;
}

__checkReturn
NTSTATUS
FilterDispatchIo (
    __in PDEVICE_OBJECT    DeviceObject,
    __in PIRP              Irp
    )
{
    PIO_STACK_LOCATION  irpStack;
    NTSTATUS            status;
    PCONTROL_DEVICE_EXTENSION   deviceExtension;
    PCOMMON_DEVICE_DATA commonData;
    
    PVOID               InputBuffer;
    PVOID               OutputBuffer;
    ULONG               InputBufferSize;
    ULONG               OutputBufferSize;
    PIO_STATUS_BLOCK    IoStatus;

   commonData = (PCOMMON_DEVICE_DATA) DeviceObject->DeviceExtension;

   IoStatus = &Irp->IoStatus;

   irpStack = IoGetCurrentIrpStackLocation( Irp );

   InputBuffer      = Irp->AssociatedIrp.SystemBuffer;
   InputBufferSize  = irpStack->Parameters.DeviceIoControl.InputBufferLength;
   OutputBuffer     = Irp->AssociatedIrp.SystemBuffer;
   OutputBufferSize = irpStack->Parameters.DeviceIoControl.OutputBufferLength;

    //
    // Please note that this is a common dispatch point for controlobject and
    // filter deviceobject attached to the pnp stack. 
    //
    if ( commonData->Type == DEVICE_TYPE_FIDO )
    {
        if ( IsGetMediaSerialNmberRequest( irpStack ) )
        {
            //! \todo
            /*InputBufferSize >= sizeof(KLFLTDEV_GET_SERIAL_ID))
            Irp->IoStatus.Status = status;
            IoCompleteRequest (Irp, IO_NO_INCREMENT);
            return status;*/
        }

        //
        // We will just  the request down as we are not interested in handling
        // requests that come on the PnP stack.
        //
        return FilterPass( DeviceObject, Irp );
    }
 
    ASSERT( commonData->Type == DEVICE_TYPE_CDO );

    deviceExtension = (PCONTROL_DEVICE_EXTENSION)DeviceObject->DeviceExtension;

    if ( !deviceExtension->m_Deleted )
    {
        status = STATUS_SUCCESS;
        Irp->IoStatus.Information = 0;
        irpStack = IoGetCurrentIrpStackLocation( Irp );

        switch (irpStack->MajorFunction)
        {
            case IRP_MJ_CREATE:
                break;

            case IRP_MJ_CLOSE:
                break;

            case IRP_MJ_CLEANUP:
                break;
#if DBG
             case  IRP_MJ_DEVICE_CONTROL:
#endif
             case  IRP_MJ_INTERNAL_DEVICE_CONTROL:
                //switch (irpStack->Parameters.DeviceIoControl.IoControlCode)
                break;

            default:
                break;
        }
    }
    else
    {
        ASSERTMSG( FALSE, "Requests being sent to a dead device\n" );
        status = STATUS_DEVICE_REMOVED;
    }

    Irp->IoStatus.Status = status;
    IoCompleteRequest( Irp, IO_NO_INCREMENT );

    return status;
}
