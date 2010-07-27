#ifndef __devctrlex_h
#define __devctrlex_h

// {E636CD65-4021-4d20-97EC-5AE23189566B}
DEFINE_GUID( GET_MEDIA_SERIAL_NUMBER_GUID, 
    0xe636cd65,
    0x4021, 0x4d20, 0x97, 0xec, 0x5a, 0xe2, 0x31, 0x89, 0x56, 0x6b );

#define _DEVCTRL_DEVICEINFO_VER1    0x1

typedef struct _DEVCTRL_DEVICEINFO
{
    ULONG       Version;
    ULONG       BusId;                          /// \todo
    ULONG       IsRemovable;                    /// \todo
    ULONG       IsIdUnique;
    ULONG       IdOffset;
    ULONG       IdLenght;
    ULONG       ClassIdOffset;
    ULONG       ClassIdLength;
    UCHAR       Data[1];
} DEVCTRL_DEVICEINFO, *PDEVCTRL_DEVICEINFO;
#endif // __devctrlex_h