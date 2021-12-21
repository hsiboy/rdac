VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "clsVSProject"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public Name As String
Public FolderNumber As String
Public Folder As clsDirectoryEntry
Public FormatCode As Byte
Public SamplerateCode As Byte

Private oTracks(1 To 24, 1 To 16) As clsTrack
Private bProjectLoaded As Boolean

Private sEventListName As String
Private sExtension As String

Private bBypassDirectDiskAccess As Boolean
Private sSongFilePath As String   'only used if BypassDirectDiskAccess=true

Private udtProjectHeader As SFX_PROJECT_HEADER

Property Get UsedTracks() As Collection
    Dim lTrack As Long, lVTrack As Long
    Set UsedTracks = New Collection
    For lTrack = 1 To 24
        For lVTrack = 1 To 16
            If oTracks(lTrack, lVTrack).Events.Count > 0 Then
                UsedTracks.Add oTracks(lTrack, lVTrack)
            End If
        Next
    Next
End Property

Property Get SfxProjectHeader() As Byte()
    Dim bt() As Byte
    ReDim bt(LenB(udtProjectHeader) - 1)
    RtlMoveMemory bt(0), udtProjectHeader, LenB(udtProjectHeader)
    SfxProjectHeader = bt
End Property

Property Get BypassDirectDiskAccess() As Boolean
    BypassDirectDiskAccess = bBypassDirectDiskAccess
End Property

Public Property Let Extension(ByVal Value As String)
    sExtension = Value
End Property

Public Property Get Extension() As String
    Extension = sExtension
End Property

Property Get FolderName() As String
   FolderName = "SONG" & Right("000" & FolderNumber, 4) & "." & sExtension
End Property

Public Property Let PackedFile(ByVal FullName As String)
    'inits project properties out of a packed '*.exe' file, bypasses direct disk access
    Dim iFNr As Integer
    Dim btData() As Byte
    Dim btProject() As Byte
    Dim pRead As Long
    Dim oTrack As clsTrack
    Dim oPhrase As clsEvent
    Dim lClusters() As Long
    Dim x As Long
    Dim udtTrackHeader As SFX_TRACK_HEADER, udtEventHeader As SFX_EVENT_HEADER
    
    Dim lTrack As Long, lVTrack As Long
    
    bBypassDirectDiskAccess = True
    sSongFilePath = FullName
    Name = Mid(FullName, InStrRev(FullName, "\") + 1)
    
    iFNr = FreeFile
    Open FullName For Binary As iFNr
    Get iFNr, SFX_BINARY_HEAD_SIZE + 1, udtProjectHeader
    If udtProjectHeader.BlockLen > 2 ^ 20 Or udtProjectHeader.BlockLen < 32 Then
        MsgBox "Invalid File!", vbCritical, "Error:"
        Close iFNr
        Exit Property
    End If
    ReDim btProject(udtProjectHeader.BlockLen)
    Get iFNr, SFX_BINARY_HEAD_SIZE + 1, btProject
    Close iFNr
    
    sExtension = GetString(udtProjectHeader.Extension, 0, 3)
    FormatCode = 255
    UnSerialize btProject, LenB(udtProjectHeader)
    If FormatCode > 8 Then
        MsgBox "Invalid File!", vbCritical, "Error:"
        Exit Property
    End If
    
    'set up folder and Partition with required values
    Set Folder = New clsDirectoryEntry
    Set Folder.MotherPartition = New clsPartition
    With Folder.MotherPartition
        .BytesPerSector = 512
        .SectorsPerCluster = 64
        .PhysicalDrive = "Project:" & FullName
        .ClearFat
    End With
    
    pRead = udtProjectHeader.OffsetFirstTrack
    InitTracks

    While GetInteger(btProject, pRead) <> 0
        RtlMoveMemory udtTrackHeader, btProject(pRead), LenB(udtTrackHeader)
        pRead = pRead + LenB(udtTrackHeader)

        lTrack = udtTrackHeader.TrackNr
        lVTrack = udtTrackHeader.VTrackNr
        Set oTrack = oTracks(lTrack, lVTrack)
        oTrack.Name = GetString(udtTrackHeader.FileName, 0, 32)
        oTrack.Name = Trim(BiteRight(oTrack.Name, Chr(0)))
        'Stop 'debugpoint for each track
        While GetInteger(btProject, pRead) <> 0
            RtlMoveMemory udtEventHeader, btProject(pRead), LenB(udtEventHeader)
            pRead = pRead + LenB(udtEventHeader)
            ReDim lClusters(udtEventHeader.NumEntries - 1)
            RtlMoveMemory lClusters(0), btProject(pRead), udtEventHeader.NumEntries * 4
            pRead = pRead + udtEventHeader.NumEntries * 4
            
            Set oPhrase = CreateEvent(udtEventHeader.PhraseData(), 0, 2)
            Set oPhrase.ClusterChain = New Collection
            For x = 1 To udtEventHeader.NumEntries
                oPhrase.ClusterChain.Add lClusters(x - 1)
            Next
            oTrack.Events.Add oPhrase
            
        Wend
        pRead = pRead + 4   '0000 from the previous phrase
    Wend
    
    bProjectLoaded = True
End Property

Public Property Let SongFile(ByVal FullName As String)
    'inits project properties out of a 'SONG' file, bypasses direct disk access
    Dim iFNr As Integer
    Dim btSongFile() As Byte
    
    bBypassDirectDiskAccess = True
    sSongFilePath = Left(FullName, InStrRev(FullName, "\"))
    sExtension = Right(FullName, 3)
    
    iFNr = FreeFile
    Open FullName For Binary As iFNr
    ReDim btSongFile(LOF(iFNr) - 1)
    Get iFNr, , btSongFile
    Close iFNr
    
    UnSerialize btSongFile, 0
    
    'sEventListName has same Postfix as SONG
    If sExtension = "VR8" Then
        sEventListName = "EVENTLS0." & sExtension   'speciality: 880VX uses this name...
    Else
        sEventListName = "EVENTLST." & sExtension
    End If
    
    'set up folder and Partition with required values
    Set Folder = New clsDirectoryEntry
    Set Folder.MotherPartition = New clsPartition
    With Folder.MotherPartition
        .BytesPerSector = 512
        .SectorsPerCluster = 64
        .PhysicalDrive = "Project:" & FullName
        .ClearFat
    End With
End Property



Property Get Serialize() As Byte()
    Dim btData() As Byte
    Dim cbUnknown As Long
    
    Select Case sExtension
    Case "VR1", "VS1" '2480/2400
        ReDim btData(44 - 1)
        cbUnknown = 8
    Case "VR6", "VR5"   '1680/1880
        ReDim btData(38 - 1)
        cbUnknown = 4
    Case "VR9", "VR8", "VR7" '880/880EX/880VX/890
        ReDim btData(20 - 1)
        cbUnknown = 4
    Case Else
        MsgBox "Unsupported VS model!", vbExclamation, "Error:"
        Exit Property
    End Select
    PutInteger FolderNumber, btData(), cbUnknown
    PutString Name, btData, cbUnknown + 2, 12
    btData(cbUnknown + 14) = SamplerateCode
    btData(cbUnknown + 15) = FormatCode
    Serialize = btData
End Property

Public Sub UnSerialize(SongData() As Byte, ByVal pRead As Long)
    Select Case sExtension
    Case "VR1", "VS1" '2480/2400
        pRead = pRead + 8   'unknown content
    Case "VR6", "VR5" '1680/1880
        pRead = pRead + 4   'unknown content
    Case "VR9", "VR8", "VR7" '880/880VX/880EX/890
        pRead = pRead + 4   'unknown content
    Case Else
        MsgBox "Unsupported VS model!", vbExclamation, "Error:"
        Exit Sub
    End Select
    FolderNumber = GetInteger(SongData(), pRead)
    Name = GetString(SongData, pRead + 2, 12)
    Name = Trim(BiteRight(Name, Chr(0)))
    SamplerateCode = SongData(pRead + 14)
    FormatCode = SongData(pRead + 15)
End Sub

Private Sub InitTracks()
    'init the Tracks Array with new Track Objects:
    Dim lTrack As Long, lVTrack As Long
    For lTrack = 1 To 24
        For lVTrack = 1 To 16
            Set oTracks(lTrack, lVTrack) = New clsTrack
            oTracks(lTrack, lVTrack).TrackNr = lTrack
            oTracks(lTrack, lVTrack).VTrackNr = lVTrack
        Next
    Next
End Sub

Public Sub LoadProject()
    Dim oEventlist As clsDirectoryEntry
    Dim btEventlist() As Byte
    Dim iFNr As Integer
    Dim bFailed As Boolean
    
    bProjectLoaded = False
        
    InitTracks
    
    'Try to access the EVENTLST file:
    On Error Resume Next
    If bBypassDirectDiskAccess Then
        If FileExists(sSongFilePath & sEventListName) Then
            iFNr = FreeFile
            Open sSongFilePath & sEventListName For Binary As iFNr
            ReDim btEventlist(LOF(iFNr) - 1)
            Get iFNr, , btEventlist
            Close 1
        Else
            Err.Raise 513, , "No Eventlist file found!"
        End If
    Else
        Folder.ReadSubEntries
        Set oEventlist = Folder.SubEntryByName("EVENTLST.*")
        btEventlist = oEventlist.FileData
        sEventListName = oEventlist.ShortFilename
        sExtension = Right(sEventListName, 3)
    End If
    If Err.Number > 0 Then
        bFailed = True
    End If
    On Error GoTo 0
    
    'if we've got the Eventlist data, then parse it:
    If Not bFailed Then
        On Error Resume Next
        bProjectLoaded = ParseEventList(btEventlist)
        If Err.Number > 0 Then
            MsgBox "Error Parsing Eventlist! Could not load Project.", vbCritical, "Error:"
        End If
        On Error GoTo 0
    End If
End Sub




Private Function ParseEventList(btData() As Byte) As Boolean
#If Not IsStub = 1 Then '8<---------------------------------- This Part is only compiled for VSWaveExport.exe
    Dim pRead As Long
    Dim lTrack As Long, lVTrack As Long
    Dim oTrack As clsTrack
    Dim cEvents As Long
    Dim x As Long
    Dim colEditList As Collection
    Dim cbEvent As Long
    Dim cTracks As Long, cVTracks As Long
    Dim cbEventCountOffset As Long
    Dim oEvent As clsEvent
    Dim cEditEntries As Long
    Dim bTrackNamesPresent As Boolean
    Dim bHasAnormalities As Boolean
    Dim bIsGoodEvent As Boolean

    Select Case sExtension
    Case "VR1"  '2480
        cEditEntries = MakeLong(btData(21), btData(20), 0, 0) 'Number of Entries in the Edit List
        pRead = 22      'start reading at offset 22
        cbEvent = 128   'size of one eventlist entry
        cTracks = 24: cVTracks = 16 'track count
        cbEventCountOffset = 20
        bTrackNamesPresent = True
    Case "VS1"  '2400
        cEditEntries = MakeLong(btData(21), btData(20), 0, 0) 'Number of Entries in the Edit List
        pRead = 22
        cbEvent = 128
        cTracks = 24: cVTracks = 16
        cbEventCountOffset = 20
        bTrackNamesPresent = True
    Case "VR6"  '1680
        cEditEntries = MakeLong(btData(17), btData(16), 0, 0) 'Number of Entries in the Undo List
        pRead = 18
        cbEvent = 64
        cTracks = 16: cVTracks = 16
        cbEventCountOffset = 16
        bTrackNamesPresent = True
    Case "VR5"  '1880
        cEditEntries = MakeLong(btData(17), btData(16), 0, 0) 'Number of Entries in the Undo List
        pRead = 18
        cbEvent = 64
        cTracks = 18: cVTracks = 16
        cbEventCountOffset = 16
        bTrackNamesPresent = True
    Case "VR9"  '880EX
        cEditEntries = MakeLong(btData(1), btData(0), 0, 0) 'Number of Entries in the Undo List
        pRead = 2
        cbEvent = 48
        cTracks = 16: cVTracks = 8
        cbEventCountOffset = 0
        bTrackNamesPresent = False
    Case "VR8"  '880VX
        cEditEntries = MakeLong(btData(1), btData(0), 0, 0) 'Number of Entries in the Undo List
        pRead = 2
        cbEvent = 48
        cTracks = 16: cVTracks = 8
        cbEventCountOffset = 0
        bTrackNamesPresent = False
    Case "VR7"  '890
        cEditEntries = MakeLong(btData(1), btData(0), 0, 0) 'Number of Entries in the Undo List
        pRead = 2
        cbEvent = 48
        cTracks = 16: cVTracks = 8
        cbEventCountOffset = 0
        bTrackNamesPresent = False
    Case Else
        MsgBox "Unsupported VS model!", vbExclamation, "Error:"
        Exit Function
    End Select
  
    'build Edit List (at the start of the file):
    Set colEditList = New Collection
    While cEditEntries > 0
        colEditList.Add CreateEvent(btData, pRead, IIf(bBypassDirectDiskAccess, 1, 0))
        pRead = pRead + cbEvent
        cEditEntries = cEditEntries - 1
    Wend


'    MsgBox "Begin Track Events @" & pRead
    
    'now go through the track events:
    For lTrack = 1 To cTracks
       For lVTrack = 1 To cVTracks
            Set oTrack = oTracks(lTrack, lVTrack)
            
            If pRead + 16 < UBound(btData) Then
                If bTrackNamesPresent Then
                    oTrack.Name = GetString(btData, pRead, 16)
                End If
                cEvents = MakeLong(btData(pRead + cbEventCountOffset + 1), btData(pRead + cbEventCountOffset), 0, 0)
                pRead = pRead + cbEventCountOffset + 2
                
                For x = 1 To cEvents
                    bIsGoodEvent = False
                    Set oEvent = CreateEvent(btData, pRead, IIf(bBypassDirectDiskAccess, 1, 0))
                    If oEvent.Valid = 1 Then
                        bIsGoodEvent = True
                    ElseIf oEvent.EditReference <= 0 Then
                        'some 880 speciality? is this maybe the EditReference?
'                        Out "Event at T" & lTrack & "-" & lVTrack & " " & Seconds2TimeString(oEvent.StartSecond) & _
                         " [@" & pRead & "] shows valid " & oEvent.Valid
                        bIsGoodEvent = True
                    ElseIf oEvent.EditReference <= colEditList.Count Then
                        'if the event is marked as not valid,
                        'then it looks like the Event has to be taken from the editlist...
                        If oEvent.FirstCluster = colEditList(oEvent.EditReference).FirstCluster Then
                            bIsGoodEvent = True 'sometimes Editlist does not provide add. info
                        Else
                            oEvent.FirstCluster = colEditList(oEvent.EditReference).FirstCluster
                            'use only if it is not =0:
                            If oEvent.FirstCluster <> 0 Then
                                Out "Event at T" & lTrack & "-" & lVTrack & " " & Seconds2TimeString(oEvent.StartSecond) & _
                                 " [@" & pRead & "] substituted by Edit List Entry."
                                bIsGoodEvent = True
                            Else
                                Out "Ghost Event at T" & lTrack & "-" & lVTrack & " " & Seconds2TimeString(oEvent.StartSecond) & _
                                 " [@" & pRead & "] ignored."
                            End If
                            bHasAnormalities = True
                        End If
                    Else
                        Out "Undefined Evententry " & oEvent.EditReference & " in Trackevents at T" & _
                            lTrack & "-" & lVTrack & " " & Seconds2TimeString(oEvent.StartSecond) & _
                             " [@" & pRead & "] ignored."
                        bHasAnormalities = True
                    End If
                    
                    If bIsGoodEvent Then
                        If oTrack.Events.Count > 0 Then
                            If oTrack.Events(oTrack.Events.Count).EndSample > oEvent.StartSample Then
                                Out "Overlapping Events at T" & lTrack & "-" & lVTrack & " " & Seconds2TimeString(oEvent.StartSecond) & _
                                 " [@" & pRead & "] detected!"
                            End If
                        End If
                        
                        oTrack.Events.Add oEvent
                    End If
                    
                    pRead = pRead + cbEvent
                Next
            End If
        Next
    Next
    ParseEventList = True
#End If '8<----------------------------------------------------------------------------------------------
End Function




Private Function CreateEvent(btData() As Byte, ByVal pRead As Long, Optional ByVal FileType As Long) As clsEvent
    Dim oEvent As New clsEvent
    'FileType = 0: HD/Virdis Access, FileType = 1: CD-R File, FileType = 2: SFX File
    With oEvent
        Set .MotherPartition = Folder.MotherPartition
        .BytesPerRdacFrame = BytesPerRdacFrame
        .SamplesPerRdacFrame = SamplesPerRdacFrame
        .FormatCode = FormatCode
        .SamplesPerSecond = Samplerate
        .Extension = sExtension
        .UnSerialize btData(), pRead
        Select Case FileType
        Case 0
        Case 1
            oEvent.TakeFile = oEvent.PredictedTakeFileName
        Case 2
            oEvent.TakeFile = sSongFilePath
        End Select
    End With
    Set CreateEvent = oEvent
End Function

Public Property Get Loaded() As Boolean
    Loaded = bProjectLoaded
End Property

Public Property Get Tracks() As clsTrack()
    Tracks = oTracks
End Property

Public Property Get Format() As String
    Select Case FormatCode
    Case 0
        Format = "MT1"
    Case 1
        Format = "MT2"
    Case 2
        Format = "LIV"
    Case 3
        Format = "M16"
    Case 4
        Format = "CDR"
    Case 5
        Format = "MTP"
    Case 6
        Format = "LV2"
    Case 8
        Format = "M24"
    Case Else
        Format = "UKN"
    End Select
End Property

Public Property Get Samplerate() As Long
    Select Case (SamplerateCode And &HF&)
    Case 0
        Samplerate = 48000
    Case 1
        Samplerate = 44100
    Case 2
        Samplerate = 32000
    Case 3
        Samplerate = 96000
    Case 4
        Samplerate = 88200
    Case Else
        Samplerate = 64000
    End Select
End Property

Public Property Get BitDepth() As Long
    Select Case FormatCode
    Case 0, 1, 3
        BitDepth = 16
    Case 5, 8
        BitDepth = 24
    Case Else
        BitDepth = -1
    End Select
End Property

Public Property Get BytesPerRdacFrame() As Long
    Select Case FormatCode
    Case 0  'MT1
        BytesPerRdacFrame = 16
    Case 1  'MT2
        BytesPerRdacFrame = 12
    Case 3  'M16
        BytesPerRdacFrame = 32
    Case 4  'CDR
        BytesPerRdacFrame = 64
    Case 5  'MTP
        BytesPerRdacFrame = 16
    Case 8  'M24
        BytesPerRdacFrame = 48
    Case Else
        BytesPerRdacFrame = -1
    End Select
End Property

Public Property Get SamplesPerRdacFrame() As Long
    Select Case FormatCode
    Case 0      'MT1
        SamplesPerRdacFrame = 16
    Case 1      'MT2
        SamplesPerRdacFrame = 16
    Case 3      'M16
        SamplesPerRdacFrame = 16
    Case 4      'CDR
        SamplesPerRdacFrame = 32
    Case 5      'MTP
        SamplesPerRdacFrame = 16
    Case 8      'M24
        SamplesPerRdacFrame = 16
    Case Else
        SamplesPerRdacFrame = -1
    End Select
End Property

