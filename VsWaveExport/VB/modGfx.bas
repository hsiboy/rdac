Attribute VB_Name = "modGfx"
Option Explicit

Private Declare Function GetObject Lib "gdi32.dll" Alias "GetObjectA" ( _
        ByVal hObject As Long, ByVal nCount As Long, lpObject As Any) As Long

Public Declare Function DeleteObject Lib "gdi32.dll" (ByVal hObject As Long) As Long

Private Declare Function CallWindowProc Lib "user32.dll" Alias "CallWindowProcA" _
        (ByVal lpPrevWndFunc As Long, ByVal hwnd As Long, ByVal msg As Long, _
        ByVal wParam As Long, ByVal lParam As Long) As Long
     
Private Declare Sub CopyMem Lib "kernel32.dll" Alias "RtlMoveMemory" _
        (ByVal hpvDest As Long, ByVal hpvSource As Long, ByVal cbCopy As Long)

Private Declare Function CreateDIBSection Lib "gdi32.dll" ( _
        ByVal hdc As Long, pBitmapInfo As BITMAPINFO, ByVal un As Long, _
        ByVal lplpVoid As Long, ByVal handle As Long, ByVal dw As Long) As Long

Private Declare Function GetDIBits Lib "gdi32.dll" (ByVal aHDC As Long, ByVal hBitmap As Long, _
        ByVal nStartScan As Long, ByVal nNumScans As Long, lpBits As Long, _
        lpBI As BITMAPINFO, ByVal wUsage As Long) As Long

Private Declare Function GetDC Lib "user32.dll" (ByVal hwnd As Long) As Long

Private Declare Function ReleaseDC Lib "user32.dll" (ByVal hwnd As Long, _
        ByVal hdc As Long) As Long

Private Declare Function OleCreatePictureIndirect Lib "olepro32.dll" _
        (PicDesc As PICTDESC, RefIID As GUID, _
        ByVal fPictureOwnsHandle As Long, IPic As IPicture) As Long

Private Type BITMAP
    bmType As Long
    bmWidth As Long
    bmHeight As Long
    bmWidthBytes As Long
    bmPlanes As Integer
    bmBitsPixel As Integer
    bmBits As Long
End Type

Public Type BITMAPINFOHEADER
    biSize As Long
    biWidth As Long
    biHeight As Long
    biPlanes As Integer
    biBitCount As Integer
    biCompression As Long
    biSizeImage As Long
    biXPelsPerMeter As Long
    biYPelsPerMeter As Long
    biClrUsed As Long
    biClrImportant As Long
End Type

Public Type RGBQUAD
  rgbBlue As Byte
  rgbGreen As Byte
  rgbRed As Byte
  rgbReserved As Byte
End Type

Public Type BITMAPINFO
    bmiHeader As BITMAPINFOHEADER
    bmiColors As RGBQUAD
End Type

Private Const BI_RGB = 0&
Private Const BI_RLE4 = 2&
Private Const BI_RLE8 = 1&
Private Const DIB_RGB_COLORS = 0

Private Type GUID
    Data1 As Long
    Data2 As Integer
    Data3 As Integer
    Data4(7) As Byte
End Type

Private Type PICTDESC
    cbSizeofStruct As Long
    picType As Long
    hImage As Long
    xExt As Long
    yExt As Long
End Type




Public Function GetBitmapData(SomePicture As StdPicture, Bpp As Byte, hDib As Long, _
        BmpInfoStruct As BITMAPINFO, Optional cByteWidth As Long, _
        Optional ForceCopy As Boolean = True) As Long
    'Get a Pointer to the RGB Data of a StdPicture by D.Aue
    'If the original Data is available in the required Color depth (Bpp Parameter),
    'and ForceCopy=false, the Pointer to the original Picture Data is returned,
    'otherwise a DIB is created, the Data is copied/transformed into the DIB and
    'the Handle is returned in hDip. In that case the calling Function is responsible
    'to delete the DIB Object
   
    'IN: SomePicture
    'IN: Bpp: required Color depth (16,24 oder 32)
    'OUT: hDib: Handle to a created DIB (if one was created)
    'OUT: BmpInfoStruct of the created DIB
    'OUT: cByteWidth number of bytes per scanline of the created DIB (includes padbytes)
    'IN: ForceCopy
    'RETURNS: Pointer to the RGB Data
    '
    'If ForceCopy=false and the original Data is available in the correct Color depth,
    '0 is returned in hDib
    Dim udtBmpStruct As BITMAP
    Dim lpData As Long
    Dim hDesktopDC As Long
    Dim Result As Long

    If Not (Bpp = 16 Or Bpp = 24 Or Bpp = 32) Then
        Err.Raise 513, , "ungültige Farbtiefe!"
    End If

    'get Infos
    If GetObject(SomePicture, Len(udtBmpStruct), udtBmpStruct) = 0 Then
        Err.Raise 513, , "GetBitmapData: GetObject Kann auf Bilddaten nicht zugreifen !"
        Exit Function 'We could'nt access the Data at all
    ElseIf (udtBmpStruct.bmWidthBytes And 3) <> 0 Then
        'Win2k oddity: PadBytes are calculated wrong !
        udtBmpStruct.bmWidthBytes = udtBmpStruct.bmWidthBytes + 2
    End If
  
    cByteWidth = udtBmpStruct.bmWidthBytes
  
    'init BitmapInfoHeader
    With BmpInfoStruct.bmiHeader
        .biSize = Len(BmpInfoStruct.bmiHeader)
        .biWidth = udtBmpStruct.bmWidth
        .biHeight = udtBmpStruct.bmHeight
        .biPlanes = 1
        .biBitCount = Bpp
        .biCompression = BI_RGB
        .biSizeImage = ((.biWidth * Bpp / 8 + 3) And &HFFFFFFFC) * .biHeight
    End With
  
  
    If udtBmpStruct.bmBits = 0 Or udtBmpStruct.bmBitsPixel <> Bpp Or ForceCopy = True Then
        'Create DIB:
        hDib = CreateDIBSection(0, BmpInfoStruct, DIB_RGB_COLORS, VarPtr(lpData), 0, 0)
        If hDib = 0 Then
           Err.Raise 513, , "GetBitmapData: Konnte DIB nicht erstellen !"
           Exit Function
        End If
        
        'Is the Format of the Source ok, or do we need some Translation ?
        If udtBmpStruct.bmBits <> 0 And udtBmpStruct.bmBitsPixel = Bpp Then
            'We can access the original Data and the Format fits.
            CopyMem lpData, udtBmpStruct.bmBits, BmpInfoStruct.bmiHeader.biSizeImage
        Else
            'Translation takes a bit longer.
            hDesktopDC = GetDC(0)
            Result = GetDIBits(hDesktopDC, SomePicture.handle, 0, udtBmpStruct.bmHeight, ByVal lpData, BmpInfoStruct, DIB_RGB_COLORS)
            If Result = 0 Then
               Err.Raise 513, , "GetBitmapData: GetDIBits kann auf Bilddaten nicht zugreifen !"
            ElseIf ReleaseDC(0, hDesktopDC) = 0 Then
               Err.Raise 513, , "GetBitmapData: ReleaseDC ist fehlgeschlagen !"
            End If
        End If
        GetBitmapData = lpData
    Else
        GetBitmapData = udtBmpStruct.bmBits
        hDib = 0
    End If
 
End Function


Public Function DibToPicture(HandleDIB) As StdPicture
   'Converts a DIB into a StdPicture.
   'Note that no DeleteObject is required for the inputed hDib after conersion.
   Dim vIDispatch As GUID
   Dim vPic As PICTDESC
   
   With vIDispatch
       .Data1 = &H20400
       .Data4(0) = &HC0
       .Data4(7) = &H46
   End With
   With vPic
       .cbSizeofStruct = Len(vPic)
       .picType = vbPicTypeBitmap
       .hImage = HandleDIB
   End With
   Call OleCreatePictureIndirect(vPic, vIDispatch, 1, DibToPicture) ' Convert image to OLE picture
End Function

