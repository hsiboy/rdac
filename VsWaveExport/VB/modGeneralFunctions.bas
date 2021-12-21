Attribute VB_Name = "modGeneralFunctions"
Option Explicit

Public Declare Sub RtlMoveMemory Lib "Kernel32" (ByRef Destination As Any, _
        ByRef Source As Any, ByVal Length As Long)

Private Declare Function GetDiskFreeSpaceEx Lib "Kernel32" _
        Alias "GetDiskFreeSpaceExA" (ByVal Path As String, lpFreeBytesAvailable As LARGE_INTEGER, _
        lpTotalNumberOfBytes As LARGE_INTEGER, lpTotalNumberOfFreeBytes As LARGE_INTEGER) As Long

Private Declare Function FormatMessage Lib "Kernel32" Alias _
        "FormatMessageA" (ByVal dwFlags As Long, ByRef lpSource As Any, _
        ByVal dwMessageId As Long, ByVal dwLanguageId As Long, ByVal lpBuffer As String, _
        ByVal nSize As Long, ByRef Arguments As Long) As Long

'8<-- Für ListDir
Private Declare Function FindFirstFile Lib "Kernel32" _
        Alias "FindFirstFileA" (ByVal lpFileName As String, _
        lpFindFileData As WIN32_FIND_DATA) As Long
        
Private Declare Function FindNextFile Lib "Kernel32" _
        Alias "FindNextFileA" (ByVal hFindFile As Long, _
        lpFindFileData As WIN32_FIND_DATA) As Long
        
Private Declare Function FindClose Lib "Kernel32" (ByVal _
        hFindFile As Long) As Long

Private Type FILETIME
  dwLowDateTime As Long
  dwHighDateTime As Long
End Type

Private Type WIN32_FIND_DATA
  dwFileAttributes As Long
  ftCreationTime As FILETIME
  ftLastAccessTime As FILETIME
  ftLastWriteTime As FILETIME
  nFileSizeHigh As Long
  nFileSizeLow As Long
  dwReserved0 As Long
  dwReserved1 As Long
  cFileName As String * 259 'MAX_PATH
  cAlternate As String * 14
End Type
'Für ListDir -->8

Private Declare Function SHFileOperation Lib "shell32.dll" _
        Alias "SHFileOperationA" (lpFileOp As SHFILEOPSTRUCT) As Long

Private Const FORMAT_MESSAGE_FROM_SYSTEM As Long = &H1000
Private Const FORMAT_MESSAGE_IGNORE_INSERTS As Long = &H200

Private Const MAX_PATH = 259
Private Const INVALID_HANDLE_VALUE = -1&

Private Const FO_MOVE = &H1
Private Const FO_COPY = &H2
Private Const FO_DELETE = &H3
Private Const FO_RENAME = &H4

Const FOF_SILENT = &H4
Const FOF_NOCONFIRMATION = &H10

Private Type SHFILEOPSTRUCT
  hwnd As Long
  wFunc As Long
  pFrom As String
  pTo As String
  fFlags As Integer
  fAnyOperationsAborted As Boolean
  hNameMappings As Long
  lpszProgressTitle As String
End Type

Public Type LARGE_INTEGER
    Lowpart As Long
    Highpart As Long
End Type


Public Function CreateHexEditorOutput(AnArray() As Byte, Optional Offset As Long = 0, _
    Optional BytesToRead As Long = 1024) As String
    'helpful for debugging
    Dim x As Long
    Dim Temp As Variant
    Dim Result As String
   
    Dim sHexNumbers As String
    Dim sAsciiChars As String
    
    If UBound(AnArray) - Offset < BytesToRead Then
        BytesToRead = UBound(AnArray) - Offset
    ElseIf BytesToRead = 0 Then
        BytesToRead = UBound(AnArray) - Offset
    End If
   
    sHexNumbers = Right("00000000" & Hex(Offset), 8) & ": "
    For x = Offset To Offset + BytesToRead
        Temp = Hex(AnArray(x))
        Temp = String(2 - Len(Temp), "0") & Temp
        sHexNumbers = sHexNumbers & Temp & " "
        Temp = AnArray(x)
            
        If Temp < 32 Then   'statt undarstellbaren Zeichen "." nehmen
            sAsciiChars = sAsciiChars & "."
        ElseIf Temp > 126 Then
            sAsciiChars = sAsciiChars & "."
        Else
            sAsciiChars = sAsciiChars & Chr(Temp)
        End If


        If (x + 1) Mod 16 = 0 Then
            Result = Result & sHexNumbers & sAsciiChars & vbNewLine
            sHexNumbers = Right("00000000" & Hex(x), 8) & ": "
            sAsciiChars = ""
        End If
    Next

    CreateHexEditorOutput = Result
End Function

Public Function MakeLong(LsByte1 As Byte, Byte2 As Byte, Byte3 As Byte, MsByte4 As Byte) As Long
    MakeLong = LsByte1 + Byte2 * &H100& + Byte3 * &H10000

    MakeLong = MakeLong + ((MsByte4 And &H7F) * &H1000000)
    If MsByte4 > &H7F Then
        MakeLong = MakeLong Or &H80000000
    End If
End Function

Public Function GetString(btArray() As Byte, Offset As Long, Length As Long) As String
    Dim bt() As Byte
    ReDim bt(Length - 1)
    RtlMoveMemory bt(0), btArray(Offset), Length
    GetString = StrConv(CStr(bt), vbUnicode)
End Function

Public Function GetInteger(btArray() As Byte, Offset As Long) As Integer
    RtlMoveMemory ByVal VarPtr(GetInteger) + 1, btArray(Offset), 1
    RtlMoveMemory ByVal VarPtr(GetInteger), btArray(Offset + 1), 1
End Function



Public Sub PutLong(ByVal Value As Long, btArray() As Byte, ByVal Offset As Long)
    btArray(Offset + 0) = (Value And &H7F000000) \ &H1000000
    If Value < 0 Then
        btArray(0) = btArray(0) Or &H80&
    End If
    btArray(Offset + 1) = (Value And &HFF0000) \ &H10000
    btArray(Offset + 2) = (Value And &HFF00&) \ &H100
    btArray(Offset + 3) = Value And &HFF&
End Sub

Public Sub PutInteger(ByVal Value As Integer, btArray() As Byte, ByVal Offset As Long)
    btArray(Offset + 0) = (Value And &H7F00) \ &H100
    If Value < 0 Then
        btArray(0) = btArray(0) Or &H80&
    End If
    btArray(Offset + 1) = Value And &HFF&
End Sub

Public Sub PutString(ByVal Value As String, btArray() As Byte, ByVal Offset As Long, _
    Optional ByVal Length As Long)
    Dim sPad As String
    Dim btString() As Byte
    
    If Length = 0 Then
        sPad = ""
        Length = Len(Value)
    Else
        If Len(Value) < Length Then
            sPad = String(Length - Len(Value), Chr(0))
        Else
            sPad = ""
        End If
    End If
    
    btString = StrConv(Value & sPad, vbFromUnicode)
    RtlMoveMemory btArray(Offset), btString(0), Length
End Sub

Public Function Double2LargeInt(ByVal DoubleIn As Double) As LARGE_INTEGER
    'Translate a Double into a C++ LARGE_INTEGER
    With Double2LargeInt
        .Highpart = Int(DoubleIn / 4294967296#)
        DoubleIn = DoubleIn - .Highpart * 4294967296#
        If DoubleIn > 2 ^ 31 Then
            .Lowpart = (DoubleIn - 2 ^ 32) Or &H80000000
        Else
            .Lowpart = DoubleIn
        End If
    End With
End Function

Public Function LargeInt2Double(LargeIn As LARGE_INTEGER) As Double
    Dim fReturn As Double
    'reverse
    With LargeIn
        fReturn = .Highpart * 4294967296#
        If .Lowpart < 0 Then
            .Lowpart = .Lowpart And Not &H80000000
            fReturn = fReturn + 2147483648#
        End If
        fReturn = fReturn + .Lowpart
    End With
    LargeInt2Double = fReturn
End Function


Public Function IsArrayDimensioned(AnArray As Variant) As Boolean
    On Error Resume Next
    IsArrayDimensioned = UBound(AnArray) > -1
    If Err.Number > 0 Then
        IsArrayDimensioned = False
    End If
    On Error GoTo 0
End Function

Public Function DirExists(ByVal DirectoryName As String) As Boolean
    On Error Resume Next
    DirExists = CBool(GetAttr(DirectoryName) And vbDirectory)
    On Error GoTo 0
End Function

Public Function FileExists(ByVal FileName As String) As Boolean
    On Error Resume Next
    FileExists = Not CBool(GetAttr(FileName) And (vbDirectory Or vbVolume))
    On Error GoTo 0
End Function


Public Sub Out(ByVal Text As String)
#If Not IsStub = 1 Then '8<---------------------------------- This Part is only compiled for VSWaveExport.exe
    frmDebug.Out Text
#End If '8<----------------------------------------------------------------------------------------------
End Sub


Public Function CreateValidFileName(ByVal Value As String) As String
    Dim Char As Variant
    For Each Char In Split("\ / : * ? "" < > | '", " ")
        Value = Replace(Value, Char, "")    'unerlaubte zeichen weg
    Next
    CreateValidFileName = Value
End Function

Public Function Seconds2TimeString(ByVal Value As Double) As String
     Seconds2TimeString = _
            Right("0" & Value \ 60 \ 60, 2) & ":" & _
            Right("0" & Value \ 60, 2) & ":" & _
            Right("0" & Value Mod 60, 2)
End Function

Public Function GetFreeDiskSpace(ByVal Path As String) As Double
    Dim udtAvailable As LARGE_INTEGER
    Dim udtSize As LARGE_INTEGER
    Dim udtFree As LARGE_INTEGER
    
    If Path Like "?:\*" Then
        Path = Left(Path, 3)
    End If
    If GetDiskFreeSpaceEx(Path, udtAvailable, udtSize, udtFree) <> 0 Then
        GetFreeDiskSpace = LargeInt2Double(udtAvailable)
    End If
End Function

Public Function ExistsInCol(AnyCol As Collection, Key As String) As Boolean
    On Error Resume Next
    ExistsInCol = IsObject(AnyCol(Key)) 'dummy statement um aufs Item zuzugreifen
    If Err.Number > 0 Then
        ExistsInCol = False
    Else
        ExistsInCol = True
    End If
    On Error GoTo 0
End Function


Public Function FormatError(ErrCode As Long) As String
    Dim sTemp As String
    Dim lLen As Long
    
    lLen = 256
    sTemp = String(lLen, 0)
    lLen = FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM Or _
                      FORMAT_MESSAGE_IGNORE_INSERTS, ByVal 0, ErrCode, 0, sTemp, lLen, ByVal 0)
    If lLen = 0 Then Stop
    FormatError = Left(sTemp, lLen - 2)
End Function


Public Function EnsureBackSlash(Path As String) As String
    If Right(Path, 1) = "\" Then
        EnsureBackSlash = Path
    Else
        EnsureBackSlash = Path & "\"
    End If
End Function

Public Function BiteLeft(Source As String, BiteChar As String) As String
    'schneidet alle zeichen von links ab die bitechar entsprechen
    'durch like können auch strings wie "[!\]" (=alles was nicht = "\" ist)
    'verwendet werden
    Dim x As Long
    x = 1
    While Mid$(Source, x, 1) Like BiteChar
        x = x + 1
    Wend
    BiteLeft = Mid(Source, x)
End Function

Public Function BiteRight(Source As String, BiteChar As String) As String
    'Wie BiteLeft nur von rechts
    Dim x As Long
    x = Len(Source)
    If x > 0 Then
        Do While Mid$(Source, x, 1) Like BiteChar
            x = x - 1
            If x = 0 Then Exit Do
        Loop
    End If
    If x > 0 Then
        BiteRight = Left(Source, x)
    End If
End Function

Public Function ListDir(ByVal Path As String, ByVal Mask As String, _
        Optional OnlyFiles As Boolean = True) As String()
    Dim udtFound() As WIN32_FIND_DATA
    Dim pFound As Long
    Dim hFind As Long
    Dim sReturn() As String
    
    
    ReDim udtFound(100)
    Path = EnsureBackSlash(Path)
    hFind = FindFirstFile(Path & Mask, udtFound(0))
    If hFind <> -1 Then 'INVALID_HANDLE_VALUE
        Do
            If OnlyFiles Then
                If Not (udtFound(pFound).dwFileAttributes And vbDirectory) = vbDirectory Then
                    pFound = pFound + 1
                    If pFound > UBound(udtFound) Then
                        ReDim Preserve udtFound(UBound(udtFound) + 100)
                    End If
                End If
            Else
                pFound = pFound + 1
                If pFound > UBound(udtFound) Then
                    ReDim Preserve udtFound(UBound(udtFound) + 100)
                End If
            End If
        Loop While FindNextFile(hFind, udtFound(pFound)) <> 0
        FindClose hFind
        
        If pFound > 0 Then
            ReDim sReturn(pFound - 1)
            For pFound = 0 To UBound(sReturn)
                sReturn(pFound) = Path & Left(udtFound(pFound).cFileName, InStr(udtFound(pFound).cFileName, Chr(0)) - 1)
            Next
        End If
    End If
    
    ListDir = sReturn
End Function

Public Function DeleteDir(ByVal Path As String) As Boolean
    Dim udtShellOp As SHFILEOPSTRUCT
    
    With udtShellOp
        .hwnd = 0
        .wFunc = FO_DELETE
        .pFrom = Path & Chr$(0)
        .pTo = Chr$(0)
        .fFlags = FOF_NOCONFIRMATION 'Or FOF_SILENT ' or FOF_NO_Ui

    End With

    DeleteDir = (SHFileOperation(udtShellOp) = 0)
End Function

Public Function CreatePath(ByVal Path As String) As Boolean
    Dim x As Long
    
    On Error Resume Next
    
    If Right(Path, 1) <> "\" Then Path = Path & "\"
    x = 1
    Do
        If Not DirExists(Left(Path, InStr(x, Path, "\"))) Then
            MkDir Left(Path, InStr(x, Path, "\") - 1)
            If Err.Number > 0 Then
                Err.Clear
                Exit Function
            End If
        End If
        x = InStr(x, Path, "\") + 1
    Loop While InStr(x, Path, "\") > 0
    
    On Error GoTo 0
    CreatePath = True
End Function

Public Function EnsureSlash(Path As String) As String
    If Right(Path, 1) <> "\" Then
        EnsureSlash = Path & "\"
    Else
        EnsureSlash = Path
    End If
End Function

