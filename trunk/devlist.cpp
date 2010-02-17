#include "devctrl.h"
#include "devlist.h"

NTSTATUS
InsertDeviceListEx(
	__in PLIST_ENTRY pListHead,
	__in PDEVICE_NAME pDevName
)
{
	PDEVICE_LIST_ENTRY pdev_list_entry;
	NTSTATUS status = STATUS_UNSUCCESSFUL;

	__try
	{
		pdev_list_entry = (PDEVICE_LIST_ENTRY) ExAllocatePoolWithTag (
            PagedPool,
            sizeof (DEVICE_LIST_ENTRY),
            _ALLOC_TAG
            );

        if (!pdev_list_entry)
		{
			status = STATUS_NO_MEMORY;
			__leave;
		}

		memset( pdev_list_entry, 0, sizeof (DEVICE_LIST_ENTRY) );

		if (!pDevName )
		{
			status = STATUS_INVALID_PARAMETER_4;
			__leave;
		}

		
		pdev_list_entry->pDevName = pDevName;
		
		InsertHeadList( 
			pListHead, 
			&(pdev_list_entry->ListEntry) 
			);

		
		status = STATUS_SUCCESS;
	}
	__finally
	{
		if (!NT_SUCCESS( status ) )
		{
			if (pdev_list_entry)
			{
				ExFreePool( pdev_list_entry );
			}
		}
	}

	return status;
}


NTSTATUS
InsertDeviceList( 
	__in PDEVICE_NAME pDevName
)
{
	NTSTATUS status;
	
    AcquireResourceExclusive( &Globals.m_DeviceListLock );

	status=InsertDeviceListEx(
		&Globals.m_DeviceList,
		pDevName
		);

	ReleaseResource( &Globals.m_DeviceListLock );
	return status;
}

NTSTATUS
RemAllDeviceListEx(
	__in PLIST_ENTRY pDevRulHead 
)
{
	PLIST_ENTRY Flink;
	PDEVICE_LIST_ENTRY pdev_list_entry;

	if ( !pDevRulHead ) return STATUS_INVALID_PARAMETER;

	if ( IsListEmpty( pDevRulHead ) )
		return	STATUS_SUCCESS;


	Flink=pDevRulHead->Flink;
	while ( Flink!=pDevRulHead )
	{
		pdev_list_entry=CONTAINING_RECORD( Flink, DEVICE_LIST_ENTRY, ListEntry );

		Flink=Flink->Flink;
		RemoveEntryList( Flink->Blink );


		if ( pdev_list_entry )
			ExFreePool( pdev_list_entry );
	}

	return STATUS_SUCCESS;
}


NTSTATUS
RemAllDeviceList()
{
	NTSTATUS status;
	AcquireResourceExclusive( &Globals.m_DeviceListLock );
	status = RemAllDeviceListEx( &Globals.m_DeviceList );
	ReleaseResource( &Globals.m_DeviceListLock );
	return status;
}

NTSTATUS
RemItemFromDeviceListEx(
	__in PLIST_ENTRY pDevRulHead, 
	__in PDEVICE_NAME pDevName
)
{
	PLIST_ENTRY Flink;
	PDEVICE_LIST_ENTRY pdev_rul_entry;

	if (!pDevRulHead) return STATUS_INVALID_PARAMETER;

	if ( IsListEmpty( pDevRulHead ) )
		return	STATUS_SUCCESS;


	Flink=pDevRulHead->Flink;
	while ( Flink != pDevRulHead )
	{

		pdev_rul_entry=CONTAINING_RECORD( Flink, DEVICE_LIST_ENTRY, ListEntry );

		if (pdev_rul_entry->pDevName == pDevName)
		{
			Flink = Flink->Flink;
			RemoveEntryList(Flink->Blink);

			if ( pdev_rul_entry )
				ExFreePool (pdev_rul_entry);
		}
		else
			Flink=Flink->Flink;


	}

	return STATUS_SUCCESS;
}

NTSTATUS
RemItemFromDeviceList(
	__in PDEVICE_NAME pDevName
)
{
	NTSTATUS status;
	AcquireResourceExclusive( &Globals.m_DeviceListLock );
	status = RemItemFromDeviceListEx( &Globals.m_DeviceList, pDevName );
	ReleaseResource( &Globals.m_DeviceListLock );
	return status;
}


BOOLEAN
IsAttachedDeviceEx (
	__in PLIST_ENTRY pDevRulHead, 
	__in PUNICODE_STRING pusGuid,
	__in PUNICODE_STRING pusDeviceType
)
{
	PLIST_ENTRY Flink;
	PDEVICE_LIST_ENTRY pdev_rul_entry;

	if (!pDevRulHead) return FALSE;

	if ( IsListEmpty( pDevRulHead ) )
		return	FALSE;


	Flink=pDevRulHead->Flink;
	while ( Flink != pDevRulHead )
	{

		pdev_rul_entry=CONTAINING_RECORD( Flink, DEVICE_LIST_ENTRY, ListEntry );

		if ( RtlCompareUnicodeString( &pdev_rul_entry->pDevName->usGuid,pusGuid, TRUE ) == 0 &&
			 RtlCompareUnicodeString( &pdev_rul_entry->pDevName->usDeviceType,pusDeviceType, TRUE ) == 0
			)
			return TRUE;
		
		 Flink=Flink->Flink;

	}

	return FALSE;
}


BOOLEAN
IsAttachedDevice(
	__in PUNICODE_STRING pusGuid,
	__in PUNICODE_STRING pusDeviceType
)
{
	BOOLEAN ret;
	AcquireResourceExclusive( &Globals.m_DeviceListLock );
	ret = IsAttachedDeviceEx (
		&Globals.m_DeviceList, 
		pusGuid,
		pusDeviceType
		);
	ReleaseResource( &Globals.m_DeviceListLock );
	return ret;
}