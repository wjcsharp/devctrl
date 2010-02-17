#ifndef __DEVLIST_H
#define __DEVLIST_H

//элемент списка шаблона
typedef struct 
{
	LIST_ENTRY		ListEntry;
	PDEVICE_NAME	pDevName;
} DEVICE_LIST_ENTRY, *PDEVICE_LIST_ENTRY;


NTSTATUS
InsertDeviceList( 
	__in PDEVICE_NAME pDevName
);

BOOLEAN
IsAttachedDevice(
	__in PUNICODE_STRING pusGuid,
	__in PUNICODE_STRING pusDeviceType
);

NTSTATUS
RemItemFromDeviceList(
	__in PDEVICE_NAME pDevName
);

#endif