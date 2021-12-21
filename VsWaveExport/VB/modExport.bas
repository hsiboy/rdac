Attribute VB_Name = "modExport"
Option Explicit

Public Declare Function Decode Lib "RDAC.dll" (ByVal d0 As Long, _
        ByRef inRdac As Any, ByRef outWave As Any, ByVal cSamples As Long, _
        ByVal FormatCode As Long) As Long

Public Declare Function ShrinkTo24 Lib "RDAC.dll" ( _
        ByRef in32 As Long, ByRef out24 As Byte, ByVal cSamples As Long) As Long

Public Declare Function ShrinkTo16 Lib "RDAC.dll" ( _
        ByRef in32 As Long, ByRef out16 As Byte, ByVal cSamples As Long) As Long

Public Declare Function SwapBytes Lib "RDAC.dll" ( _
        ByRef Data As Byte, ByVal cBytes As Long) As Long


Public Const SFX_EXE_SIZE As Long = 4 * 32768
Public Const SFX_DLL_SIZE As Long = 3 * 32768

Public Const SFX_BINARY_HEAD_SIZE As Long = SFX_EXE_SIZE + SFX_DLL_SIZE


Private Type Wave_Header
    dwRiffID As Long
    dwFileSize As Long
    dwWaveFormatTag As Long
    dwFmtID As Long
    dwFmtSize As Long
    wFormatTag As Integer
    wChannels As Integer
    dwSamplesPerSec As Long
    dwAvgBytesPerSec As Long
    wBlockAlign As Integer
    wBitsPerSample As Integer
    dwDataID As Long
    dwDataSize As Long
End Type

Public Type SFX_PROJECT_HEADER
    Extension(3) As Byte
    OffsetFirstTrack As Long
    IgnorePhraseParameters As Boolean
    IncludeProjectName As Boolean
    BlockLen As Long
    StartSample As Long
    EndSample As Long
End Type
    
Public Type SFX_TRACK_HEADER
    FileName(31) As Byte
    TrackNr As Long
    VTrackNr As Long
End Type

Public Type SFX_EVENT_HEADER
    NumEntries As Long
    PhraseData(127) As Byte  'max event size
End Type
'es folgt ein longarray mit allen clustern zu dem event


Public CancelExportFlag As Boolean

Private iFNr As Integer
Private lBitDepth As Long
Private lSampleRate As Long
Private lStartSample As Long
Private lEndSample As Long

Private lFormatCode As Long
Private fSamplesPerRawByte As Double 'factor for size calculation

Private btProject() As Byte
Private pWriteProject As Long
Private lPackFat() As Long
Private pPackFreeCluster As Long

Private cClustersTotal As Long
Private cClustersToGo As Long
Private cSecondsSinceStart As Double
Private cSecondsEstimated As Double
Private cLastSeconds As Double

Private bIgnorePhraseParameters As Boolean

Public Enum ExportMode
    NormalMode
    SelfExtracting
    DataRecovery
End Enum

#If Not IsStub = 1 Then '8<---------------------------------- This Part is only compiled for VSWaveExport.exe

Public Sub PackTracks(Project As clsVSProject, Tracks As Collection, _
        ByVal StartSample As Long, ByVal EndSample As Long, ByVal DestPath As String, _
        ByVal IncludeProjectName As Boolean, ByVal IgnorePhraseParameters As Boolean)
        'exports all Tracks from StartSample till EndSample into one Self extracting file)
    Dim oTrack As clsTrack
    Dim oPhrase As clsEvent
    Dim sFileName As String
    Dim cEvents As Long
    Dim cbProjectHeader As Long
    Dim cClusters As Long
    Dim btData() As Byte
    Dim udtProjectHeader As SFX_PROJECT_HEADER, udtTrackHeader As SFX_TRACK_HEADER, udtEventHeader As SFX_EVENT_HEADER
    Dim btBinary() As Byte
    Dim iFnrExe As Integer

    
    lSampleRate = Project.Samplerate
    lFormatCode = Project.FormatCode
    lStartSample = StartSample
    lEndSample = EndSample
    lBitDepth = Project.BitDepth
    bIgnorePhraseParameters = IgnorePhraseParameters
    fSamplesPerRawByte = Project.SamplesPerRdacFrame / Project.BytesPerRdacFrame
    CancelExportFlag = False
    sFileName = DestPath & CreateValidFileName(Project.Name & ".exe")
    
    ReDim lPackFat(349567)
    
    'prepare progress window
    frmProgress.prgTracks.Max = Tracks.Count
    frmProgress.prgTracks.Value = 0
    frmProgress.Show
    frmMain.Enabled = False
    
    'estimate work by counting the clusters:
    cClustersTotal = 0
    frmProgress.lblCurrent = "Estimating Work..."
    frmProgress.lblTime.Visible = False
    DoEvents
    If Project.BypassDirectDiskAccess Then
        Project.Folder.MotherPartition.ClearFat
    End If
    For Each oTrack In Tracks
        frmProgress.prgTracks.Inc
        cClusters = 0
        For Each oPhrase In oTrack.Events
            oPhrase.MapToFat
            oPhrase.BuildClusterChain
            cClusters = cClusters + oPhrase.ClusterChain.Count
            cEvents = cEvents + 1
        Next
        cClustersTotal = cClustersTotal + cClusters
        oTrack.Tag = cClusters
    Next
    cClustersToGo = cClustersTotal
    frmProgress.lblTime.Visible = True
    frmProgress.StartTime = Now
    frmProgress.prgTracks.Value = 0
    
    'open output file, overwrite existing
    On Error Resume Next
    If FileExists(sFileName) Then
        Kill sFileName
    End If
    iFNr = FreeFile
    Open sFileName For Binary As iFNr
    If Not Err.Number = 0 Then
        MsgBox "Unable to open file " & sFileName & "!", vbCritical, "Error:"
        Err.Clear
        CancelExportFlag = True
        Exit Sub
    End If
    On Error GoTo 0
    
    'create Project Header:
    btData = Project.Serialize
    cbProjectHeader = LenB(udtProjectHeader) + UBound(btData) + 1 + _
        Tracks.Count * (LenB(udtTrackHeader) + 4) + cEvents * (LenB(udtEventHeader) + 4) + cClustersTotal * 4
    cbProjectHeader = (((cbProjectHeader \ 32768) + 1)) * 32768  'immer ganze cluster
    ReDim btProject(cbProjectHeader - 1)
    With udtProjectHeader
        .BlockLen = cbProjectHeader
        .OffsetFirstTrack = LenB(udtProjectHeader) + UBound(btData) + 1
        .IgnorePhraseParameters = bIgnorePhraseParameters
        .IncludeProjectName = IncludeProjectName
        PutString Project.Extension, .Extension, 0, 3
        .StartSample = StartSample
        .EndSample = EndSample
    End With
    RtlMoveMemory btProject(0), udtProjectHeader, LenB(udtProjectHeader)
    pWriteProject = LenB(udtProjectHeader)  'track daten nach Project header
    
    RtlMoveMemory btProject(pWriteProject), btData(0), UBound(btData) + 1
    pWriteProject = pWriteProject + UBound(btData) + 1   'put serialized project
    
    pPackFreeCluster = (SFX_BINARY_HEAD_SIZE + cbProjectHeader) / 32768 'erster Datencluster
    
    Put iFNr, SFX_BINARY_HEAD_SIZE + 1, btProject     'put empty header
    
    On Error GoTo ErrHandler
    For Each oTrack In Tracks
        frmProgress.Caption = "Packing Track " & frmProgress.prgTracks.Value + 1 & " of " & Tracks.Count
        frmProgress.lblCurrent.Caption = "Now working on: " & "V.T " & oTrack.TrackNr & _
            "-" & oTrack.VTrackNr & ": " & oTrack.Name
        
        PackTrack oTrack
        frmProgress.prgTracks.Inc
                
        UpdateTimeAndEstimate
        DoEvents
        If CancelExportFlag Then Exit For

        If CancelExportFlag Then Exit For
    Next
ExitSub:
    frmProgress.Hide
    frmMain.Enabled = True

    If Not CancelExportFlag Then
        RtlMoveMemory btProject(pWriteProject), CLng(0), 4  '0000
        pWriteProject = pWriteProject + 4 'put final project header
        Put iFNr, SFX_BINARY_HEAD_SIZE + 1, btProject
        
        'put exe stub:
        iFnrExe = FreeFile
        If FileExists(EnsureBackSlash(App.Path) & "ExeStub.exe") Then
            Open EnsureBackSlash(App.Path) & "ExeStub.exe" For Binary As iFnrExe
            If LOF(iFnrExe) > SFX_EXE_SIZE Then
                MsgBox "ExeStub.exe exceeds max allowed size!" & vbCrLf & _
                    "Process aborted.", vbCritical, "Error:"
                CancelExportFlag = True
            Else
                ReDim btBinary(SFX_EXE_SIZE - 1)
                Get iFnrExe, , btBinary
                Close iFnrExe
                Put iFNr, 1, btBinary
            End If
            
            If FileExists(EnsureBackSlash(App.Path) & "RDAC.dll") And CancelExportFlag = False Then
                'put dll:
                iFnrExe = FreeFile
                Open EnsureBackSlash(App.Path) & "RDAC.dll" For Binary As iFnrExe
                If LOF(iFnrExe) > SFX_DLL_SIZE Then
                    MsgBox "RDAC.dll exceeds max allowed size!" & vbCrLf & _
                    "Process aborted.", vbCritical, "Error:"
                    CancelExportFlag = True
                Else
                    ReDim btBinary(SFX_DLL_SIZE - 1)
                    Get iFnrExe, , btBinary
                    Close iFnrExe
                    Put iFNr, SFX_EXE_SIZE + 1, btBinary
                End If
            Else
                MsgBox "File " & EnsureBackSlash(App.Path) & "RDAC.dll is missing!" & vbCrLf & _
                    "Process aborted.", vbCritical, "Error:"
                CancelExportFlag = True
            End If
        Else
            MsgBox "File " & EnsureBackSlash(App.Path) & "ExeStub.exe is missing!" & vbCrLf & _
                "Process aborted.", vbCritical, "Error:"
            CancelExportFlag = True
        End If
        
        Close iFNr
        If CancelExportFlag Then
            On Error Resume Next
            Kill sFileName
            On Error GoTo 0
        End If
    
    Else
        Close iFNr
        On Error Resume Next
        Kill sFileName
        On Error GoTo 0
    End If

ErrHandler:
    Select Case Err.Number
    Case 0
    Case 61
        'Target drive is full!
        MsgBox Err.Description, vbCritical, "Error:"
        Close iFNr
        CancelExportFlag = True
        Err.Clear
        Resume ExitSub
    Case Else
        Err.Raise Err.Number, , Err.Description
    End Select

End Sub

Public Sub PackTrack(Track As clsTrack)
    Dim oEvent As clsEvent
    Dim cCurrentTime As Long
    Dim btData() As Byte
    Dim cbSilent As Long
    Dim vCluster As Variant
    Dim x As Long
    
    Dim udtTrackHeader As SFX_TRACK_HEADER
    Dim udtEventHeader As SFX_EVENT_HEADER
    Dim colNewClusterChain As Collection
    
    PutString Track.Name, udtTrackHeader.FileName, 0, 32
    udtTrackHeader.TrackNr = Track.TrackNr
    udtTrackHeader.VTrackNr = Track.VTrackNr
    
    RtlMoveMemory btProject(pWriteProject), udtTrackHeader, LenB(udtTrackHeader)
    pWriteProject = pWriteProject + LenB(udtTrackHeader)
    
    cCurrentTime = lStartSample
    
    frmProgress.prgTrack.Max = Track.Tag    'Tag=total count of clusters for this Track
    frmProgress.prgTrack.Value = 0
    
    'every raw data cluster is written into the destination file, a collection colNewClusterChain is set
    'up to hold every *new* clusternumber i.e. an offset into the sfx file
    For Each oEvent In Track.Events
        Set colNewClusterChain = New Collection
        For x = 1 To oEvent.ClusterChain.Count
            vCluster = CLng(oEvent.ClusterChain(x))
            If lPackFat(vCluster) = 0 Then
                'add cluster
                btData = oEvent.RawClusterData(x)
                Put iFNr, , btData
                lPackFat(vCluster) = pPackFreeCluster
                pPackFreeCluster = pPackFreeCluster + 1
            End If
            'add ref to cluster
            colNewClusterChain.Add lPackFat(vCluster)
            
            'screen updating and uia
            cClustersToGo = cClustersToGo - 1
            If cClustersToGo < 0 Then cClustersToGo = 0
            frmProgress.prgTrack.Inc
            UpdateTimeAndEstimate
            DoEvents
            If CancelExportFlag Then Exit Sub
        
        Next
        'write event header:
'Stop 'debugpoint for each track
        btData = oEvent.Serialize
        With udtEventHeader
            .NumEntries = colNewClusterChain.Count
            RtlMoveMemory .PhraseData(0), btData(0), UBound(btData) + 1
        End With
        If pWriteProject + LenB(udtEventHeader) > UBound(btProject) + 1 Then
            If MsgBox("Fatal Project Header resize occured!" & vbCrLf & "Continue?", _
                vbCritical Or vbYesNo, "Error:") = vbYes Then
                ReDim Preserve btProject(UBound(btProject) * 1.1) '10% bigger
            Else
                CancelExportFlag = True
                Exit Sub
            End If
        End If
        RtlMoveMemory btProject(pWriteProject), udtEventHeader, LenB(udtEventHeader)
        pWriteProject = pWriteProject + LenB(udtEventHeader)
        
        'now the *new* clusterchain is stored into the event header
        For Each vCluster In colNewClusterChain
            RtlMoveMemory btProject(pWriteProject), CLng(vCluster), 4
            pWriteProject = pWriteProject + 4
        Next
    Next
    RtlMoveMemory btProject(pWriteProject), CLng(0), 4  '0000
    pWriteProject = pWriteProject + 4
    
End Sub

#End If '8<----------------------------------------------------------------------------------------------

Public Sub ExportTracks(Project As clsVSProject, Tracks As Collection, _
        ByVal StartSample As Long, ByVal EndSample As Long, ByVal DestPath As String, _
        ByVal IncludeProjectName As Boolean, ByVal IgnorePhraseParameters As Boolean, _
        Optional WorkMode As ExportMode = 0)
    'exports all Tracks from StartSample till EndSample as Wave Files
    Dim oTrack As clsTrack
    Dim oPhrase As clsEvent
    
    lSampleRate = Project.Samplerate
    lFormatCode = Project.FormatCode
    lStartSample = StartSample
    lEndSample = EndSample
    lBitDepth = Project.BitDepth
    bIgnorePhraseParameters = IgnorePhraseParameters
    fSamplesPerRawByte = Project.SamplesPerRdacFrame / Project.BytesPerRdacFrame
    CancelExportFlag = False
    
    'prepare progress window
    frmProgress.prgTracks.Max = Tracks.Count
    frmProgress.prgTracks.Value = 0
    frmProgress.prgTrack.Max = Int((EndSample - StartSample) / 100000)
    frmProgress.prgTrack.Value = 0
    frmProgress.Show
#If Not IsStub = 1 Then '8<---------------------------------- This Part is only compiled for VSWaveExport.exe
    frmMain.Enabled = False
#End If '8<----------------------------------------------------------------------------------------------
    
    'estimate work by counting the clusters:
    cClustersTotal = 0
    frmProgress.lblCurrent = "Estimating Work..."
    frmProgress.lblTime.Visible = False
    DoEvents
    
    Select Case WorkMode
    Case NormalMode
        If Project.BypassDirectDiskAccess Then
            Project.Folder.MotherPartition.ClearFat
        End If
        For Each oTrack In Tracks
            frmProgress.prgTracks.Inc
            For Each oPhrase In oTrack.Events
                oPhrase.MapToFat
                oPhrase.BuildClusterChain
                cClustersTotal = cClustersTotal + oPhrase.ClusterChain.Count
            Next
        Next
    
    Case SelfExtracting
        For Each oTrack In Tracks
            frmProgress.prgTracks.Inc
            For Each oPhrase In oTrack.Events
                If oPhrase.ClusterChain Is Nothing Then
                    oPhrase.BuildClusterChain
                End If
                cClustersTotal = cClustersTotal + oPhrase.ClusterChain.Count
            Next
        Next
    
    Case DataRecovery
        cClustersTotal = Tracks(1).Events(1).ClusterChain.Count
    End Select
    
    cClustersToGo = cClustersTotal
    frmProgress.lblTime.Visible = True
    frmProgress.StartTime = Now
    frmProgress.prgTracks.Value = 0
    
    On Error GoTo ErrHandler
    For Each oTrack In Tracks
        frmProgress.Caption = "Exporting Track " & frmProgress.prgTracks.Value + 1 & " of " & Tracks.Count
        frmProgress.lblCurrent.Caption = "Now working on: " & "V.T " & oTrack.TrackNr & _
            "-" & oTrack.VTrackNr & ": " & oTrack.Name
        frmProgress.prgTrack.Value = 0
        
        WriteTrack oTrack, DestPath & CreateValidFileName( _
            IIf(IncludeProjectName, Project.Name & "_", "") & _
            Format(oTrack.TrackNr, "00") & "-" & Format(oTrack.VTrackNr, "00") & "_" & _
            Trim(oTrack.Name) & ".wav")
        frmProgress.prgTracks.Inc
        
        If CancelExportFlag Then Exit For
    Next
ExitSub:
    frmProgress.Hide
#If Not IsStub = 1 Then '8<---------------------------------- This Part is only compiled for VSWaveExport.exe
    frmMain.Enabled = True
#End If '8<----------------------------------------------------------------------------------------------

ErrHandler:
    Select Case Err.Number
    Case 0
    Case 61
        'Target drive is full!
        MsgBox Err.Description, vbCritical, "Error:"
        Close iFNr
        CancelExportFlag = True
        Err.Clear
        Resume ExitSub
    Case Else
        Err.Raise Err.Number, , Err.Description
    End Select
End Sub

Public Sub WriteTrack(Track As clsTrack, FileName As String)
    Dim udtFileHeader As Wave_Header
    Dim oEvent As clsEvent
    Const C_SILENT_BUFFER As Long = 1000000
    Dim cCurrentTime As Long
    Dim btSilence() As Byte
    Dim cbSilent As Long
    
    'open output file, overwrite existing
    On Error Resume Next
    If FileExists(FileName) Then
        Kill FileName
    End If
    iFNr = FreeFile
    Open FileName For Binary As iFNr
    If Not Err.Number = 0 Then
        MsgBox "Unable to open file " & FileName & "!", vbCritical, "Error:"
        Err.Clear
        CancelExportFlag = True
        Exit Sub
    End If
    On Error GoTo 0
    
    Put iFNr, , udtFileHeader       'put an empty header for now
    
    cbSilent = (C_SILENT_BUFFER * lBitDepth / 8)
    ReDim btSilence(cbSilent - 1)
    
    cCurrentTime = lStartSample
    For Each oEvent In Track.Events
        
        'only process event if its within the export range:
        If oEvent.EndSample > lStartSample And oEvent.StartSample <= lEndSample Then
        
            'write silence if phrase.start is after curent pos
            Do While cCurrentTime < oEvent.StartSample
                If (oEvent.StartSample - cCurrentTime) < C_SILENT_BUFFER Then
                    ReDim btSilence(((oEvent.StartSample - cCurrentTime) * lBitDepth / 8) - 1)
                ElseIf UBound(btSilence) <> cbSilent - 1 Then
                    ReDim btSilence(cbSilent - 1)
                End If
                Put iFNr, , btSilence
                cCurrentTime = cCurrentTime + (UBound(btSilence) + 1) / (lBitDepth / 8)
                
                frmProgress.prgTrack.Value = Int((cCurrentTime - lStartSample) / 100000)
                UpdateTimeAndEstimate
                DoEvents
                If CancelExportFlag Then Exit For
            Loop
        
            WritePhrase oEvent
            cCurrentTime = oEvent.EndSample
        End If
        If oEvent.StartSample > lEndSample Then
            Exit For    'finished with this track
        End If
        If CancelExportFlag Then Exit For
    Next 'event
    
    If Not CancelExportFlag Then
        With udtFileHeader
            .dwRiffID = &H46464952          'RIFF
            .dwFileSize = LOF(iFNr) - 8
            .dwWaveFormatTag = &H45564157   'WAVE
            .dwFmtID = &H20746D66           'fmt
            .dwFmtSize = 16
            .wFormatTag = 1                 'PCM
            .wChannels = 1
            .dwSamplesPerSec = lSampleRate
            .dwAvgBytesPerSec = lSampleRate * lBitDepth / 8
            .wBlockAlign = lBitDepth / 8
            .wBitsPerSample = lBitDepth
            .dwDataID = &H61746164          'data
            .dwDataSize = LOF(iFNr) - Len(udtFileHeader)
        End With
        Put iFNr, 1, udtFileHeader       'put the real header now
        Close iFNr
    Else
        Close iFNr
        On Error Resume Next
        Kill FileName
        On Error GoTo 0
    End If
End Sub



Private Sub WritePhrase(Phrase As clsEvent)
    'read compressed/uncompressed raw data, decode/unpad, and write
    'as wave into the File held in iFNr
    Const EXP_FACTOR As Double = 4
    Dim EXP_RESCALE As Double
        
    Dim cSamplesinPhrase As Long
    Dim cSamplesTillFadeInEnd As Long
    Dim cSamplesTillFadeOutStart As Long
    Dim fPhraseLevel As Double
    
    Dim cSamplesToCut As Long
    
    Dim lCurrentPos As Long 'relative to phrase start
    Dim btData() As Byte    'raw data read
    Dim lWaveData() As Long '32 bit wave data
    Dim d0 As Long          'required for rdac decompression
    Dim lRet As Long
    Dim x As Long, Y As Long
    Dim lPrg As Long

    
    cSamplesinPhrase = (Phrase.EndSample - Phrase.StartSample)
    
    'set up fadeout and level variables:
    If Not bIgnorePhraseParameters Then
        EXP_RESCALE = Exp(EXP_FACTOR)
        cSamplesTillFadeInEnd = Phrase.FadeInTicks * lSampleRate / 1000
        cSamplesTillFadeOutStart = cSamplesinPhrase - Phrase.FadeOutTicks * (lSampleRate / 1000)
        If cSamplesTillFadeOutStart < 0 Then
            cSamplesTillFadeOutStart = 0
            cSamplesTillFadeInEnd = 0
        ElseIf cSamplesTillFadeInEnd + (cSamplesinPhrase - cSamplesTillFadeOutStart) > cSamplesinPhrase Then
            cSamplesTillFadeInEnd = cSamplesinPhrase - cSamplesTillFadeOutStart
        End If
        fPhraseLevel = Phrase.PhraseLevel / &H4000 'todo: is this correct?
    Else
        cSamplesTillFadeOutStart = 0
        cSamplesTillFadeInEnd = cSamplesinPhrase
        fPhraseLevel = 1
    End If

    'perform conversion:
    ReDim lWaveData(0)
    For x = 1 To Phrase.ClusterChain.Count
        
        'read rdac data and decode it:
        btData = Phrase.ClusterData(x)
        If (UBound(btData) + 1) * fSamplesPerRawByte <> UBound(lWaveData) Then
            ReDim lWaveData((UBound(btData) + 1) * fSamplesPerRawByte - 1)
        End If
        lRet = Decode(d0, btData(0), lWaveData(0), UBound(lWaveData) + 1, lFormatCode)
        d0 = lWaveData(UBound(lWaveData))   'storing d0 is important for the rdac algo!
        
        'apply phrase parameters if required:
        If Not bIgnorePhraseParameters Then
            'Fade In -------------------
            If lCurrentPos < cSamplesTillFadeInEnd Then
                If Phrase.FadeInType = 0 Then
                    'linear fade
                    For Y = 0 To UBound(lWaveData) 'rescale samples
                        If Y + lCurrentPos = cSamplesTillFadeInEnd Then Exit For
                        lWaveData(Y) = lWaveData(Y) * ((lCurrentPos + Y) / cSamplesTillFadeInEnd)
                    Next
                Else
                    'exponential fade
                    For Y = 0 To UBound(lWaveData) 'rescale samples
                        If Y + lCurrentPos = cSamplesTillFadeInEnd Then Exit For
                        lWaveData(Y) = lWaveData(Y) * (Exp((lCurrentPos + Y) / cSamplesTillFadeInEnd * EXP_FACTOR) - 1) / EXP_RESCALE
                    Next
                End If
            End If
        
            'Fade out -------------------
            If lCurrentPos + UBound(lWaveData) > cSamplesTillFadeOutStart Then
                If Phrase.FadeOutType = 0 Then
                    'linear fade
                    For Y = cSamplesTillFadeOutStart - lCurrentPos To UBound(lWaveData) 'rescale samples
                        If Y >= 0 Then
                            If (cSamplesinPhrase - cSamplesTillFadeOutStart) > 0 Then
                                lWaveData(Y) = lWaveData(Y) * ((cSamplesinPhrase - lCurrentPos - Y) / _
                                    (cSamplesinPhrase - cSamplesTillFadeOutStart))
                            Else
                                lWaveData(Y) = 0
                            End If
                        End If
                    Next
                Else
                    'exponential fade
                    For Y = cSamplesTillFadeOutStart - lCurrentPos To UBound(lWaveData) 'rescale samples
                        If Y >= 0 Then
                            If (cSamplesinPhrase - cSamplesTillFadeOutStart) > 0 Then
                                lWaveData(Y) = lWaveData(Y) * (Exp((cSamplesinPhrase - lCurrentPos - Y) / _
                                    (cSamplesinPhrase - cSamplesTillFadeOutStart) * EXP_FACTOR) - 1) / EXP_RESCALE
                            Else
                                lWaveData(Y) = 0
                            End If
                        End If
                    Next
                End If
            End If
        
            'Phrase Level -------------------
            If fPhraseLevel <> 1 Then
                'rescale samples
                For Y = 0 To UBound(lWaveData)
                    lWaveData(Y) = lWaveData(Y) * fPhraseLevel
                Next
            End If
        End If
        
        'cut the read data into the selection window (lStartSample/lEndSample)
        cSamplesToCut = 0
        If Phrase.StartSample + lCurrentPos + UBound(lWaveData) + 1 < lStartSample Or _
                Phrase.StartSample + lCurrentPos > lEndSample Then
            'the whole cluster is not in the selection, don't save
        Else
            If Phrase.StartSample + lCurrentPos >= lStartSample And _
                Phrase.StartSample + lCurrentPos + UBound(lWaveData) + 1 <= lEndSample Then
                'the whole cluster is in the selection, save all
            ElseIf Phrase.StartSample + lCurrentPos <= lStartSample Then
                'samples at the start have to be cut
                cSamplesToCut = lStartSample - Phrase.StartSample - lCurrentPos
                If cSamplesToCut > 0 Then
                    RtlMoveMemory lWaveData(0), lWaveData(cSamplesToCut), _
                        (UBound(lWaveData) + 1 - cSamplesToCut) * 4
                    ReDim Preserve lWaveData(UBound(lWaveData) - cSamplesToCut)
                End If
            Else
                'samples at the end have to be cut
                cSamplesToCut = Phrase.StartSample + lCurrentPos + UBound(lWaveData) + 1 - lEndSample - 1
                ReDim Preserve lWaveData(UBound(lWaveData) - cSamplesToCut)
            End If
            btData = ShrinkToBitDepth(lWaveData, lBitDepth)
            Put iFNr, , btData
        End If
        
        lCurrentPos = lCurrentPos + UBound(lWaveData) + 1 + cSamplesToCut
        
        cClustersToGo = cClustersToGo - 1
        If cClustersToGo < 0 Then cClustersToGo = 0
        
        'screen updating and uia
        frmProgress.prgTrack.Value = Int((Phrase.StartSample + lCurrentPos - lStartSample) / 100000)
        
        UpdateTimeAndEstimate
        DoEvents
        If CancelExportFlag Then Exit Sub
    Next 'cluster
End Sub

Private Function UpdateTimeAndEstimate()
    cSecondsSinceStart = DateDiff("s", frmProgress.StartTime, Now)
    If cClustersTotal <> cClustersToGo Then
        cSecondsEstimated = DateDiff("s", frmProgress.StartTime, Now) / (cClustersTotal - cClustersToGo) * cClustersTotal
    Else
        cSecondsEstimated = 0
    End If
    If cSecondsSinceStart <> cLastSeconds Then
        cLastSeconds = cSecondsSinceStart
        frmProgress.lblTime.Caption = Seconds2TimeString(cSecondsSinceStart) & _
            " (" & Seconds2TimeString(cSecondsEstimated) & " estimated)"
    End If
End Function


Private Function ShrinkToBitDepth(Data() As Long, BitDepth As Long)
    Dim lReadIndex As Long
    Dim btOut() As Byte
    Select Case BitDepth
    Case 16
        ReDim btOut((UBound(Data) + 1) * 2 - 1)
        ShrinkTo16 Data(0), btOut(0), UBound(Data) + 1
    Case 24
        ReDim btOut((UBound(Data) + 1) * 3 - 1)
        ShrinkTo24 Data(0), btOut(0), UBound(Data) + 1
    Case Else
        Err.Raise 513, , "Unsuported Bit Depth!"
    End Select
    ShrinkToBitDepth = btOut
End Function


Private Sub DebugPut(ByVal iFNr As Long)
    'used to clearly mark a position in the 24bit out wave
    Dim btData(5) As Byte
    btData(0) = 127: btData(1) = 127: btData(2) = 127
    btData(3) = 128: btData(4) = 128: btData(5) = 128
    Put iFNr, , btData
End Sub
