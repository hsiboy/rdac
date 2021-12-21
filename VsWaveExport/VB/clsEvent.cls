VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "clsEvent"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
'clsEvent
Option Explicit

'the following items are used for conversion:
' FirstCluster, cTrimedFrames, StartFrame and EndFrame

Public StartFrame As Long
Public EndFrame As Long
Public cTrimedFrames As Long    'Trim in
Public cUnusedFramesInTrimInCluster As Long
Public cUsedFramesInTrimOutCluster As Long
Public FirstCluster As Long
Public LastCluster As Long          'may be -1 for 880EX
Public ClusterCount As Long         'may be -1 for 880EX
Public TrimInCluster As Long        'may be -1 for 880EX,1680,1880
Public TrimOutCluster As Long       'may be -1 for 880EX,1680,1880
Public FadeInTicks As Long
Public FadeOutTicks As Long
Public FadeInType As Byte
Public FadeOutType As Byte
Public PhraseLevel As Long
Public EditReference As Long
Public Valid As Long

Public SamplesPerSecond As Long
Public FormatCode As Byte
Public BytesPerRdacFrame As Long
Public SamplesPerRdacFrame As Long

Public MotherPartition As clsPartition
Public ClusterChain As Collection

Public Extension As String

Private bBypassDirectDiskAccess As Boolean
Private sFilePath As String

Property Get Serialize() As Byte()
    Dim btTarget() As Byte
    Select Case Extension
    Case "VR1", "VS1"   '2480/2400
        ReDim btTarget(128 - 1)
        PutLong StartFrame, btTarget, 8
        PutLong EndFrame, btTarget, 12
        PutLong cTrimedFrames, btTarget, 16
        PutLong cUnusedFramesInTrimInCluster, btTarget, 20
        PutLong cUsedFramesInTrimOutCluster, btTarget, 24
        PutLong FirstCluster, btTarget, 28
        PutLong LastCluster, btTarget, 32
        PutLong ClusterCount, btTarget, 36
        PutLong TrimInCluster, btTarget, 40
        PutLong TrimOutCluster, btTarget, 44
        PutLong FadeInTicks, btTarget, 48
        PutLong FadeOutTicks, btTarget, 53
        btTarget(56) = FadeInType
        btTarget(57) = FadeOutType
        PutInteger PhraseLevel, btTarget, 62
        PutInteger EditReference, btTarget, 108
        PutInteger Valid, btTarget, 100
    Case "VR5", "VR6"   '1680,1880
        ReDim btTarget(64 - 1)
        PutLong StartFrame, btTarget, 0
        PutLong EndFrame, btTarget, 4
        PutLong cTrimedFrames, btTarget, 8
        PutInteger cUnusedFramesInTrimInCluster, btTarget, 12
        PutInteger cUsedFramesInTrimOutCluster, btTarget, 14
        PutInteger FirstCluster, btTarget, 20
        PutInteger LastCluster, btTarget, 22
        PutInteger ClusterCount, btTarget, 24
        PutInteger EditReference, btTarget, 38
        PutInteger Valid, btTarget, 32
    Case "VR9", "VR8", "VR7" '880EX/880VX/890
        ReDim btTarget(48 - 1)
        PutLong StartFrame, btTarget, 0
        PutLong EndFrame, btTarget, 4
        PutLong cTrimedFrames, btTarget, 8
        PutInteger cUnusedFramesInTrimInCluster, btTarget, 12
        PutInteger cUsedFramesInTrimOutCluster, btTarget, 14
        PutInteger FirstCluster, btTarget, 20
        PutInteger Valid, btTarget, 46
    Case Else
        MsgBox "Unsupported VS model!", vbExclamation, "Error:"
        Exit Function
    End Select
    Serialize = btTarget
End Property

Public Sub UnSerialize(btData() As Byte, ByVal pRead As Long)
    Select Case Extension
    Case "VR1", "VS1"   '2480/2400
        If Not pRead + 127 > UBound(btData) Then
            StartFrame = MakeLong(btData(pRead + 11), btData(pRead + 10), btData(pRead + 9), btData(pRead + 8))
            EndFrame = MakeLong(btData(pRead + 15), btData(pRead + 14), btData(pRead + 13), btData(pRead + 12))
            cTrimedFrames = MakeLong(btData(pRead + 19), btData(pRead + 18), btData(pRead + 17), btData(pRead + 16))
            cUnusedFramesInTrimInCluster = MakeLong(btData(pRead + 23), btData(pRead + 22), btData(pRead + 21), btData(pRead + 20))
            cUsedFramesInTrimOutCluster = MakeLong(btData(pRead + 27), btData(pRead + 26), btData(pRead + 25), btData(pRead + 24))
            FirstCluster = MakeLong(btData(pRead + 31), btData(pRead + 30), btData(pRead + 29), btData(pRead + 28))
            LastCluster = MakeLong(btData(pRead + 35), btData(pRead + 34), btData(pRead + 33), btData(pRead + 32))
            ClusterCount = MakeLong(btData(pRead + 39), btData(pRead + 38), btData(pRead + 37), btData(pRead + 36))
            TrimInCluster = MakeLong(btData(pRead + 43), btData(pRead + 42), btData(pRead + 41), btData(pRead + 40))
            TrimOutCluster = MakeLong(btData(pRead + 47), btData(pRead + 46), btData(pRead + 45), btData(pRead + 44))
            FadeInTicks = MakeLong(btData(pRead + 51), btData(pRead + 50), btData(pRead + 49), btData(pRead + 48))
            FadeOutTicks = MakeLong(btData(pRead + 55), btData(pRead + 54), btData(pRead + 53), btData(pRead + 52))
            FadeInType = btData(pRead + 56)
            FadeOutType = btData(pRead + 57)
            PhraseLevel = MakeLong(btData(pRead + 63), btData(pRead + 62), 0, 0)
            EditReference = MakeLong(btData(pRead + 109), btData(pRead + 108), 0, 0) + 1
            Valid = MakeLong(btData(pRead + 101), btData(pRead + 100), 0, 0)
        Else
            Out "Parsing Error: less Data then expected for Event! Event dropped."
        End If
    Case "VR5", "VR6"   '1680,1880
        If Not pRead + 63 > UBound(btData) Then
            StartFrame = MakeLong(btData(pRead + 3), btData(pRead + 2), btData(pRead + 1), btData(pRead + 0))
            EndFrame = MakeLong(btData(pRead + 7), btData(pRead + 6), btData(pRead + 5), btData(pRead + 4))
            cTrimedFrames = MakeLong(btData(pRead + 11), btData(pRead + 10), btData(pRead + 9), btData(pRead + 8))
            cUnusedFramesInTrimInCluster = MakeLong(btData(pRead + 13), btData(pRead + 12), 0, 0)
            cUsedFramesInTrimOutCluster = MakeLong(btData(pRead + 15), btData(pRead + 14), 0, 0)
            FirstCluster = MakeLong(btData(pRead + 21), btData(pRead + 20), 0, 0)
            LastCluster = MakeLong(btData(pRead + 23), btData(pRead + 22), 0, 0)
            ClusterCount = MakeLong(btData(pRead + 25), btData(pRead + 24), 0, 0)
            TrimInCluster = -1
            TrimOutCluster = -1
            FadeInTicks = 0
            FadeOutTicks = 0
            FadeInType = 0
            FadeOutType = 0
            PhraseLevel = &H4000
            EditReference = MakeLong(btData(pRead + 39), btData(pRead + 38), 0, 0)
            Valid = MakeLong(btData(pRead + 33), btData(pRead + 32), 0, 0)
        Else
            Out "Parsing Error: less Data then expected for Event! Event dropped."
        End If
    
    Case "VR9", "VR8", "VR7" '880EX/880VX/890
        If Not pRead + 47 > UBound(btData) Then
            StartFrame = MakeLong(btData(pRead + 3), btData(pRead + 2), btData(pRead + 1), btData(pRead + 0))
            EndFrame = MakeLong(btData(pRead + 7), btData(pRead + 6), btData(pRead + 5), btData(pRead + 4))
            cTrimedFrames = MakeLong(btData(pRead + 11), btData(pRead + 10), btData(pRead + 9), btData(pRead + 8))
            cUnusedFramesInTrimInCluster = MakeLong(btData(pRead + 13), btData(pRead + 12), 0, 0)
            cUsedFramesInTrimOutCluster = MakeLong(btData(pRead + 15), btData(pRead + 14), 0, 0)
            FirstCluster = MakeLong(btData(pRead + 21), btData(pRead + 20), 0, 0)
            LastCluster = -1
            ClusterCount = -1
            TrimInCluster = -1
            TrimOutCluster = -1
            FadeInTicks = 0
            FadeOutTicks = 0
            FadeInType = 0
            FadeOutType = 0
            PhraseLevel = &H4000
            EditReference = -1 'MakeLong(btdata(pRead + 39), btdata(pRead + 38), 0, 0)
            Valid = MakeLong(btData(pRead + 47), btData(pRead + 46), 0, 0)
        Else
            Out "Parsing Error: less Data then expected for Event! Event dropped."
        End If
    
    Case Else
        MsgBox "Unsupported VS model!", vbExclamation, "Error:"
        Exit Sub
    End Select
End Sub


Property Get FileName() As String
    FileName = sFilePath
End Property

Property Get StartSample() As Long
    StartSample = StartFrame * SamplesPerRdacFrame
End Property

Property Get StartSecond() As Double
    StartSecond = StartSample / SamplesPerSecond
End Property

Property Get EndSample() As Long
    EndSample = EndFrame * SamplesPerRdacFrame
End Property

Property Get EndSecond() As Double
    EndSecond = EndSample / SamplesPerSecond
End Property

Property Let TakeFile(ByVal FullName As String)
    'inits an event out of a 'TAKE' or 'SFX' file, bypasses direct disk access
    sFilePath = FullName
    bBypassDirectDiskAccess = True
End Property

Property Get PredictedTakeFileName() As String
    Select Case Extension
    Case "VR1", "VS1"   '2480/2400
        PredictedTakeFileName = Mid(Left(Me.MotherPartition.PhysicalDrive, _
            InStrRev(Me.MotherPartition.PhysicalDrive, "\")), 9) & "TK" & _
            Right("000000" & Hex(FirstCluster), 6) & "." & Extension
    Case "VR9", "VR8", "VR7", "VR5", "VR6" '880EX/880VX/890/1680/1880
        PredictedTakeFileName = Mid(Left(Me.MotherPartition.PhysicalDrive, _
            InStrRev(Me.MotherPartition.PhysicalDrive, "\")), 9) & "TAKE" & _
            Right("0000" & Hex(FirstCluster), 4) & "." & Extension
    Case Else
        MsgBox "Unsupported VS model!", vbExclamation, "Error:"
        Exit Property
    End Select
End Property


Public Sub MapToFat()
    'map a file wich is accessed with bypassed direct disk access (e.g. CD-R) to a FAT table held
    'by the Motherpartition object. This creates unique references (to clusters) through all take files,
    'used for packing those files into the SFX archive.
    Dim cbFileSize As Long
    Dim x As Long
    If bBypassDirectDiskAccess Then
        If FileExists(sFilePath) Then
            If MotherPartition.GetFirstClusterOfFile(Mid(sFilePath, InStrRev(sFilePath, "\") + 1)) = 0 Then
                'new file, reserve clusters for that file in the Bypass FAT
                MotherPartition.Files.Add MotherPartition.pWrite, Mid(sFilePath, InStrRev(sFilePath, "\") + 1)
                FirstCluster = MotherPartition.pWrite
                cbFileSize = FileLen(sFilePath)
                While x < cbFileSize
                    x = x + MotherPartition.BytesPerCluster
                    If x > cbFileSize Then
                        MotherPartition.ReserveCluster &HFFFFFF7 'end signal
                    Else
                        MotherPartition.ReserveCluster MotherPartition.pWrite + 1
                    End If
                Wend
            Else
                FirstCluster = MotherPartition.GetFirstClusterOfFile(Mid(sFilePath, InStrRev(sFilePath, "\") + 1))
            End If
        Else
            Out "Error accessing File " & Mid(sFilePath, InStrRev(sFilePath, "\") + 1) & "!"
            FirstCluster = &HFFFFFF7
        End If
    End If
End Sub

Public Sub BuildClusterChain()
    'sets up a collection containing all cluster numbers from trim in till trim out point
    Dim lCurrCluster As Long
    Dim cFramesTillStart As Long
    Dim cFramesTillEnd As Long
    Dim cFramesPerCluster As Long
    Dim lFat() As Long
    
    Set ClusterChain = New Collection

    lFat = MotherPartition.Fat
    cFramesTillStart = cTrimedFrames
    cFramesTillEnd = EndFrame - StartFrame
    cFramesPerCluster = Int(MotherPartition.BytesPerCluster / BytesPerRdacFrame) And Not 1
    lCurrCluster = FirstCluster
    Do While Not lCurrCluster >= &HFFFFFF7 'end signal
        If lCurrCluster > UBound(lFat) Then
            MsgBox "Invalid ClusterNumber " & lCurrCluster & " in Take!", vbCritical, "Error"
            Exit Do
        ElseIf cFramesTillStart > cFramesPerCluster Then
            'have not reached Phrase start yet
            lCurrCluster = lFat(lCurrCluster) And &HFFFFFFF
            cFramesTillStart = cFramesTillStart - cFramesPerCluster
        ElseIf cFramesTillEnd + cFramesTillStart < cFramesPerCluster Then
            'reached Phrase end!
            ClusterChain.Add lCurrCluster
            lCurrCluster = &HFFFFFF7 'Fin
        Else
            ClusterChain.Add lCurrCluster
            lCurrCluster = lFat(lCurrCluster) And &HFFFFFFF
            cFramesTillEnd = cFramesTillEnd - (cFramesPerCluster - cFramesTillStart)
            cFramesTillStart = 0
        End If
    Loop
    If ClusterChain.Count > 0 Then
        TrimInCluster = ClusterChain(1)
        TrimOutCluster = ClusterChain(ClusterChain.Count)
    End If
End Sub


Public Property Get ClusterData(ClusterIndex As Long) As Byte()
    'Reads and returns the rdac data stored in the xth cluster of the take, starting with 1 for
    'the trim in cluster. Takes care of trim in/out point as well as padding.
    Dim btData() As Byte
    Dim cbUnusedStart As Long, cbUnusedEnd As Long
    Dim cbUsedInCluster As Long
    
    'Read whole cluster:
    If bBypassDirectDiskAccess Then
        If UCase(Right(sFilePath, 4)) = ".EXE" Then
            'SFX File
            btData = ReadDriveOrFile(sFilePath, _
                (ClusterChain(ClusterIndex)) * CLng(MotherPartition.SectorsPerCluster), _
                MotherPartition.BytesPerCluster)
        Else
            'ripped TK FileT
            btData = ReadDriveOrFile(sFilePath, _
                (ClusterChain(ClusterIndex) - FirstCluster) * CLng(MotherPartition.SectorsPerCluster), _
                MotherPartition.BytesPerCluster)
        End If
        If Not IsArrayDimensioned(btData) Then
            Out "Error accessing File " & Mid(sFilePath, InStrRev(sFilePath, "\") + 1) & "!"
            ReDim btData(MotherPartition.BytesPerCluster - 1)   'return emtpy data (audio=flat line)
        End If
    Else
        btData = ReadDriveOrFile(MotherPartition.PhysicalDrive, _
            MotherPartition.ClusterStartSector + (ClusterChain(ClusterIndex) - 2) * CLng(MotherPartition.SectorsPerCluster), _
            MotherPartition.BytesPerCluster)
        If Not IsArrayDimensioned(btData) Then
            Out "Error accessing Cluster " & ClusterChain(ClusterIndex) & "!"
            ReDim btData(MotherPartition.BytesPerCluster - 1)   'return emtpy data (audio=flat line)
        ElseIf MotherPartition.IsSwappedIDE Then
            SwapBytes btData(0), UBound(btData) + 1
        End If
    End If
    
    'padding:
    'rule of thumb: only an even number of frames may be placed in one cluster
    cbUsedInCluster = ((MotherPartition.BytesPerCluster \ Me.BytesPerRdacFrame) And Not 1) * Me.BytesPerRdacFrame
  
    If ClusterIndex = 1 Then
        'Trim In
        cbUnusedStart = cUnusedFramesInTrimInCluster * BytesPerRdacFrame
        RtlMoveMemory btData(0), btData(cbUnusedStart), UBound(btData) + 1 - cbUnusedStart
        cbUnusedEnd = cbUnusedStart
    End If
    If ClusterIndex = ClusterChain.Count Then
        'Trim Out
        cbUnusedEnd = cbUnusedEnd + cbUsedInCluster - cUsedFramesInTrimOutCluster * BytesPerRdacFrame
    End If
    
    If cbUsedInCluster - 1 - cbUnusedEnd >= 0 Then
        ReDim Preserve btData(cbUsedInCluster - 1 - cbUnusedEnd)
    End If
    ClusterData = btData
End Property

Property Get RawClusterData(ByVal ClusterIndex As Long) As Byte()
    'Reads and returns the rdac data stored in the xth cluster of the take, starting with
    '1 for the trim in cluster ignores trim in/out and padding. (used for SFX archive)
    Dim btData() As Byte
    
    'Read whole cluster:
    If bBypassDirectDiskAccess Then
        btData = ReadDriveOrFile(sFilePath, _
            (ClusterChain(ClusterIndex) - FirstCluster) * CLng(MotherPartition.SectorsPerCluster), _
            MotherPartition.BytesPerCluster)
        If Not IsArrayDimensioned(btData) Then
            Out "Error accessing File " & Mid(sFilePath, InStrRev(sFilePath, "\") + 1) & "!"
            ReDim btData(MotherPartition.BytesPerCluster - 1)
        End If
    Else
        btData = ReadDriveOrFile(MotherPartition.PhysicalDrive, _
            MotherPartition.ClusterStartSector + (ClusterChain(ClusterIndex) - 2) * CLng(MotherPartition.SectorsPerCluster), _
            MotherPartition.BytesPerCluster)
        If Not IsArrayDimensioned(btData) Then
            Out "Error accessing Cluster " & ClusterChain(ClusterIndex) & "!"
            ReDim btData(MotherPartition.BytesPerCluster - 1)
        ElseIf MotherPartition.IsSwappedIDE Then
            SwapBytes btData(0), UBound(btData) + 1
        End If
    End If
    RawClusterData = btData
End Property


