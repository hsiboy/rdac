VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form frmAnalysis 
   Caption         =   "Partition Analysis"
   ClientHeight    =   9630
   ClientLeft      =   165
   ClientTop       =   450
   ClientWidth     =   16680
   Icon            =   "frmAnalysis.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   642
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   1112
   StartUpPosition =   3  'Windows-Standard
   Begin VB.Frame Frame2 
      Caption         =   "upon click in the FAT graphic:"
      Height          =   615
      Left            =   12180
      TabIndex        =   23
      Top             =   6840
      Width           =   4455
      Begin VB.OptionButton optClusterplay 
         Caption         =   "Play cluster"
         Height          =   255
         Left            =   2460
         TabIndex        =   4
         Top             =   240
         Width           =   1215
      End
      Begin VB.OptionButton optFileselect 
         Caption         =   "Select corresponding File"
         Height          =   255
         Left            =   120
         TabIndex        =   3
         Top             =   240
         Value           =   -1  'True
         Width           =   2175
      End
   End
   Begin VB.Frame Frame1 
      Caption         =   "Playback Parameters:"
      Height          =   2055
      Left            =   12180
      TabIndex        =   16
      Top             =   7500
      Width           =   4455
      Begin VB.CommandButton cmdSave 
         Caption         =   "Save as..."
         Height          =   315
         Left            =   3240
         TabIndex        =   11
         Top             =   960
         Width           =   1095
      End
      Begin VB.TextBox txtClusterFrom 
         BackColor       =   &H8000000B&
         Enabled         =   0   'False
         Height          =   315
         Left            =   1200
         TabIndex        =   5
         Text            =   "1"
         Top             =   240
         Width           =   855
      End
      Begin VB.TextBox txtClusterOffset 
         BackColor       =   &H8000000B&
         Enabled         =   0   'False
         Height          =   315
         Left            =   1200
         TabIndex        =   6
         Text            =   "1"
         Top             =   600
         Width           =   855
      End
      Begin VB.TextBox txtClusterCount 
         BackColor       =   &H8000000B&
         Enabled         =   0   'False
         Height          =   315
         Left            =   1200
         TabIndex        =   7
         Text            =   "1"
         Top             =   960
         Width           =   855
      End
      Begin VB.ComboBox cbxMode 
         BackColor       =   &H8000000B&
         Enabled         =   0   'False
         Height          =   315
         ItemData        =   "frmAnalysis.frx":058A
         Left            =   3240
         List            =   "frmAnalysis.frx":059D
         Style           =   2  'Dropdown-Liste
         TabIndex        =   8
         Top             =   240
         Width           =   1095
      End
      Begin VB.ComboBox cbxSampleRate 
         BackColor       =   &H8000000B&
         Enabled         =   0   'False
         Height          =   315
         ItemData        =   "frmAnalysis.frx":05BA
         Left            =   3240
         List            =   "frmAnalysis.frx":05D0
         Style           =   2  'Dropdown-Liste
         TabIndex        =   9
         Top             =   600
         Width           =   1095
      End
      Begin VB.ComboBox cbxDevice 
         Height          =   315
         Left            =   1500
         Style           =   2  'Dropdown-Liste
         TabIndex        =   12
         Top             =   1380
         Width           =   2835
      End
      Begin VB.CommandButton cmdPlay 
         Caption         =   "Play"
         Height          =   315
         Left            =   2100
         TabIndex        =   10
         Top             =   960
         Width           =   1095
      End
      Begin VB.Label Label1 
         Alignment       =   1  'Rechts
         Caption         =   "from cluster:"
         Height          =   195
         Left            =   120
         TabIndex        =   22
         Top             =   300
         Width           =   915
      End
      Begin VB.Label Label2 
         Alignment       =   1  'Rechts
         Caption         =   "offset:"
         Height          =   195
         Left            =   120
         TabIndex        =   21
         Top             =   660
         Width           =   915
      End
      Begin VB.Label Label3 
         Alignment       =   1  'Rechts
         Caption         =   "count:"
         Height          =   195
         Left            =   120
         TabIndex        =   20
         Top             =   1020
         Width           =   915
      End
      Begin VB.Label Label4 
         Alignment       =   1  'Rechts
         Caption         =   "Mode:"
         Height          =   195
         Left            =   2220
         TabIndex        =   19
         Top             =   300
         Width           =   915
      End
      Begin VB.Label Label5 
         Alignment       =   1  'Rechts
         Caption         =   "Samplerate:"
         Height          =   195
         Left            =   2220
         TabIndex        =   18
         Top             =   660
         Width           =   915
      End
      Begin VB.Label Label6 
         Alignment       =   1  'Rechts
         Caption         =   "Output Device:"
         Height          =   195
         Left            =   240
         TabIndex        =   17
         Top             =   1440
         Width           =   1155
      End
   End
   Begin MSComctlLib.TreeView trvFiles 
      Height          =   2595
      Left            =   12540
      TabIndex        =   2
      Top             =   3000
      Width           =   4095
      _ExtentX        =   7223
      _ExtentY        =   4577
      _Version        =   393217
      HideSelection   =   0   'False
      LabelEdit       =   1
      Style           =   7
      Appearance      =   1
   End
   Begin VB.VScrollBar scrFatPic 
      Height          =   5295
      LargeChange     =   30
      Left            =   12180
      Max             =   100
      TabIndex        =   0
      Top             =   60
      Width           =   255
   End
   Begin VB.PictureBox picFat 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
      Height          =   9495
      Left            =   60
      ScaleHeight     =   629
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   800
      TabIndex        =   13
      TabStop         =   0   'False
      Top             =   60
      Width           =   12060
   End
   Begin VB.TextBox txtOut 
      Height          =   2595
      Left            =   12540
      Locked          =   -1  'True
      MultiLine       =   -1  'True
      TabIndex        =   1
      Text            =   "frmAnalysis.frx":05FE
      Top             =   60
      Width           =   4095
   End
   Begin VB.Label lblFiles 
      Caption         =   "Files:"
      Height          =   195
      Left            =   12540
      TabIndex        =   15
      Top             =   2760
      Width           =   4035
   End
   Begin VB.Label lblInfo 
      Height          =   1035
      Left            =   12540
      TabIndex        =   14
      Top             =   5700
      Width           =   4035
   End
End
Attribute VB_Name = "frmAnalysis"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Declare Sub DrawAllClusters Lib "RDAC.dll" ( _
            Fat1 As Any, Fat2 As Any, FatToFile As Any, ByVal cFatEntries As Long, _
            ByVal lpBitmapData As Long)
            
Private Declare Sub HighlightClusters Lib "RDAC.dll" ( _
            Fat As Any, FatToFile As Any, ByVal cFatEntries As Long, _
            ByVal lpBitmapData As Long, CurHighlight As Any, NewHighlight As Any)
            

Private oPart As clsPartition
Private cClusters As Long

Private lFat() As Long
Private lFat2File() As Long
Private colFiles As Collection

Private lCurHighlight() As Long

Private oSelectedFile As clsDirectoryEntry
Private oSelectedProject As clsVSProject

Private udtFATBitmap As BITMAPINFO
Private hFatDib As Long
Private pFatDibData As Long
Private cFatByteWidth As Long

Public Sub Analyze(Partition As clsPartition)
    Dim lFat2() As Long
    Dim sDeviceNames() As String
    Dim x As Long
    Dim oFolder As clsDirectoryEntry
    
    Dim lCol As Long
    
    'prepare form:
    Me.Show False
    Me.txtOut = ""
    Me.Caption = "Partition Analysis ongoing, please wait..."
    lblFiles.Caption = "Files:"
    lblInfo.Caption = ""
    
    sDeviceNames = GetOutDeviceNames
    cbxDevice.Clear
    For x = 0 To UBound(sDeviceNames)
        cbxDevice.AddItem sDeviceNames(x)
        cbxDevice.ItemData(x) = x - 1
    Next
    cbxDevice.ListIndex = 0
    cbxMode.ListIndex = 3
    cbxSampleRate.ListIndex = 1
    
    
    Set oPart = New clsPartition
    With oPart
        .LBAStart = Partition.LBAStart
        .PhysicalDrive = Partition.PhysicalDrive
        .Size = Partition.Size
        .FSType = Partition.FSType
    End With
    
    oPart.ReadPartition
    lFat = oPart.Fat
    lFat2 = oPart.ReadSecondFAT
    cClusters = UBound(lFat) + 1
    
    Out "Partition on: " & oPart.PhysicalDrive
    Out "LBAStart: " & oPart.LBAStart
    Out "Filsystem: " & oPart.FatType
    Out "Size: " & oPart.Size * CDbl(oPart.BytesPerSector) & " Bytes"
    Out cClusters & " clusters, " & oPart.SectorsPerCluster & " sectors per cluster, " & _
        oPart.BytesPerSector & " bytes per sector"

    Out "Building filelist now...", True
    
    Set colFiles = New Collection
    ReDim lFat2File(UBound(lFat))
    trvFiles.Nodes.Clear
    trvFiles.Nodes.Add(, , oPart.Root.FullName, oPart.Root.ShortFilename).Expanded = True
    
    On Error Resume Next
    ReadDirectory oPart.Root
    
    If Err.Number <> 0 Then
        'try 1680/880 swapped byte mode:
        Err.Clear
        oPart.IsSwappedIDE = True
        
        'root entries have already been read successfuly, as they are in normal byte order
        For Each oFolder In oPart.Root.SubEntries
            If oFolder.IsFolder Then
                ReadDirectory oPart.Root
            End If
        Next
        
        If Err.Number <> 0 Then
            MsgBox "Failed to read directory entries!" & vbCrLf & Err.Number & ": " & _
                Err.Description, vbCritical, "Error:"
            Err.Clear
            Me.Hide
            Exit Sub
        Else
            Out "disc identified as 1680 IDE...", True
        End If
    End If
    On Error GoTo 0
    If Not Me.Visible Then Exit Sub 'window closed by user
    lblFiles.Caption = colFiles.Count & " Files:"
    Out "done"
    
    
    Out "Analyzing FAT now...", True
    picFat.Height = ((UBound(lFat) + 1) \ 400) * 2 + 6
    picFat.Cls
    pFatDibData = GetBitmapData(picFat.Image, 32, hFatDib, udtFATBitmap, cFatByteWidth, False)
    picFat.Picture = DibToPicture(hFatDib)
    DrawAllClusters lFat(0), lFat2(0), lFat2File(0), (UBound(lFat) + 1), pFatDibData
    ReDim lCurHighlight(0)
    
    For x = 0 To UBound(lFat)
        If lFat(x) <> lFat2(x) Then
            Out "FAT discrepancy cluster " & x & "!!"
        End If
    Next
    Out "done"

    Me.Caption = "Partition Analysis"
End Sub


Private Sub ReadDirectory(Dir As clsDirectoryEntry)
    'recursivly collect all files and output abnormalities
    Dim oEntry As clsDirectoryEntry
    Dim vCluster As Variant
    Dim nd As Node
    Dim colSubFolders As New Collection
    Dim colClustersCorected As Collection
    
    Dir.ReadSubEntries
    For Each oEntry In Dir.SubEntries
        If Not oEntry.Deleted Then
            If oEntry.IsFolder Then
                If Not oEntry.ShortFilename = ".." And Not oEntry.ShortFilename = "..." Then
                    colSubFolders.Add oEntry
                    Set nd = trvFiles.Nodes.Add(oEntry.Path, tvwChild, oEntry.FullName, oEntry.ShortFilename)
                    Set nd.Tag = oEntry
                End If
            Else
                colFiles.Add oEntry
                Set nd = trvFiles.Nodes.Add(oEntry.Path, tvwChild, oEntry.FullName, oEntry.ShortFilename)
                Set nd.Tag = oEntry
                
                oEntry.BuildClusterChain
                
                If oEntry.Size > 0 And oEntry.ClusterChain.Count * _
                        CDbl(oPart.BytesPerCluster) < oEntry.Size Then
                    Out "Stored size of file " & oEntry.ShortFilename & " exceeds available data!!"
                End If
                
                If oEntry.ClusterChain.Count > 131072 Then
                    'etwaige endlosschleife auflösen:
                    Set colClustersCorected = New Collection
                    On Error Resume Next
                    For Each vCluster In oEntry.ClusterChain
                        colClustersCorected.Add vCluster, "Key" & CStr(vCluster)
                        If Err.Number <> 0 Then
                            'Cluster existiert bereits in der Liste
                            Exit For
                        End If
                    Next
                    On Error GoTo 0
                    If colClustersCorected.Count <> oEntry.ClusterChain.Count Then
                        Out "FAT Error: Cluster chain for " & oEntry.ShortFilename & " contains infinite loop at cluster " & CStr(colClustersCorected(colClustersCorected.Count)) & "!!"
                        oEntry.SetCorrectedClusterChain colClustersCorected
                    Else
                        Out "File " & oEntry.ShortFilename & " exceeds maximum allowed filesize!!"
                    End If
                End If
                    
                For Each vCluster In oEntry.ClusterChain
                    If lFat2File(vCluster) = 0 Then
                        lFat2File(vCluster) = colFiles.Count
                    ElseIf vCluster <> 0 Then
                        Out "Shared cluster @" & vCluster & " in file " & oEntry.FullName & "!!"
                    End If
                Next
            End If
        End If
        If Me.Visible Then
            If colFiles.Count Mod 10 = 0 Then
                lblFiles.Caption = colFiles.Count & " Files:"
                DoEvents
            End If
        End If
        If Not Me.Visible Then Exit Sub
    Next

    'recursivly go through subentries
    For Each oEntry In colSubFolders
        ReadDirectory oEntry
    Next
End Sub



Private Sub cmdSave_Click()
    Dim colClusters As Collection
    Dim x As Long
    Dim vCluster As Variant
    Dim btData() As Byte
    Dim oProject As clsVSProject
    Dim colTracks As Collection
    Dim sFileName As String
    Dim iFNr As Integer

    If Not IsValidUserInput Then
        MsgBox "Invalid Cluster Parameters!", vbCritical, "Error:"
        Exit Sub
    End If

    frmSaveClusters.bSave = False
    If frmSaveClusters.cbxFormat = "" Then frmSaveClusters.cbxFormat = "Wave"
    If frmSaveClusters.txtExportPath = "" Then frmSaveClusters.txtExportPath = frmMain.txtExportPath
    frmSaveClusters.Show vbModal
    
    If frmSaveClusters.bSave Then
        CancelExportFlag = False
        If frmSaveClusters.cbxFormat = "Wave" Then
            If optFileselect Then
                '*** Exporting TK File to Wave File
                If Not oSelectedProject Is Nothing Then
                    If oSelectedProject.Samplerate <> 0 And _
                        (oSelectedFile.ShortFilename Like "TK*" Or oSelectedFile.ShortFilename Like "TAKE*") Then
                        Set oProject = oSelectedProject
                        Set colTracks = New Collection
                        colTracks.Add New clsTrack
                        With colTracks(1)
                            .TrackNr = 0
                            .VTrackNr = 0
                            .Name = oSelectedFile.ClusterChain(1)
                            Set .Events = New Collection
                            .Events.Add New clsEvent
                        End With
                        
                        Set colClusters = New Collection
                        For x = 1 To oSelectedFile.ClusterChain.Count
                            colClusters.Add oSelectedFile.ClusterChain(x)
                        Next
                    Else
                        MsgBox "Not a playable file!", vbExclamation, "Error:"
                        CancelExportFlag = True
                    End If
                Else
                    MsgBox "Select a file first!", vbExclamation, "Error:"
                    CancelExportFlag = True
                End If
            
            Else
                '*** Exporting Clusters to Wave File
                'set up a project and Tracks Collection for passing to the Export function
                Set oProject = New clsVSProject
                With oProject
                    Select Case Val(cbxSampleRate)
                    Case 48000
                        .SamplerateCode = 0
                    Case 44100
                        .SamplerateCode = 1
                    Case 32000
                        .SamplerateCode = 2
                    Case 96000
                        .SamplerateCode = 3
                    Case 88200
                        .SamplerateCode = 4
                    Case 64000
                        .SamplerateCode = 5
                    End Select
                    
                    Select Case cbxMode
                    Case "MT1"
                        .FormatCode = 0
                    Case "MT2"
                        .FormatCode = 1
                    Case "LIV"
                        .FormatCode = 2
                    Case "M16"
                        .FormatCode = 3
                    Case "CDR"
                        .FormatCode = 4
                    Case "MTP"
                        .FormatCode = 5
                    Case "LV2"
                        .FormatCode = 6
                    Case "M24"
                        .FormatCode = 8
                    End Select
                    .Name = "LAZARUS"
                End With
                Set colTracks = New Collection
                colTracks.Add New clsTrack
                With colTracks(1)
                    .TrackNr = 0
                    .VTrackNr = 0
                    .Name = txtClusterFrom
                    Set .Events = New Collection
                    .Events.Add New clsEvent
                End With
                
                Set colClusters = New Collection
                For x = 0 To Val(txtClusterCount) - 1
                    colClusters.Add Val(txtClusterFrom) + x * Val(txtClusterOffset)
                Next
            End If
            
            If Not CancelExportFlag Then
                With colTracks(1).Events(1)
                    .StartFrame = 0
                    .EndFrame = colClusters.Count * _
                        ((oPart.BytesPerCluster \ oProject.BytesPerRdacFrame) And Not 1)
                    .SamplesPerSecond = oProject.Samplerate
                    .FormatCode = oProject.FormatCode
                    .BytesPerRdacFrame = oProject.BytesPerRdacFrame
                    .SamplesPerRdacFrame = oProject.SamplesPerRdacFrame
                    Set .MotherPartition = oPart
                    Set .ClusterChain = colClusters
                    .cUsedFramesInTrimOutCluster = (oPart.BytesPerCluster \ oProject.BytesPerRdacFrame) And Not 1
                End With
                
                'check if required diskspace is available:
                If (colTracks(1).Events(1).EndSample - colTracks(1).Events(1).StartSample) * _
                    (oProject.BitDepth / 8) > GetFreeDiskSpace(frmSaveClusters.txtExportPath) Then
                    If MsgBox("The required disk space to store the wave may exceed the available space." & vbCrLf & _
                        "Continue anyway?", vbQuestion Or vbYesNo, "Warning:") = vbNo Then
                        CancelExportFlag = True
                    End If
                End If
                If Not CancelExportFlag Then
                    ExportTracks oProject, colTracks, 0, colTracks(1).Events(1).EndSample, EnsureSlash(frmSaveClusters.txtExportPath), True, True, DataRecovery
                    If Not CancelExportFlag Then
                        MsgBox "Track written to " & frmSaveClusters.txtExportPath, vbInformation, "Done!"
                    End If
                End If
            End If
        Else
            If optFileselect Then
                '*** storing TK File to new RDAC File
                If Not oSelectedProject Is Nothing Then
                    If oSelectedProject.Samplerate <> 0 And _
                        (oSelectedFile.ShortFilename Like "TK*" Or oSelectedFile.ShortFilename Like "TAKE*") Then
                        Set colClusters = New Collection
                        For x = 1 To oSelectedFile.ClusterChain.Count
                            colClusters.Add oSelectedFile.ClusterChain(x)
                        Next
                    Else
                        MsgBox "Not a playable file!", vbExclamation, "Error:"
                        CancelExportFlag = True
                    End If
                Else
                    MsgBox "Select a file first!", vbExclamation, "Error:"
                    CancelExportFlag = True
                End If
            Else
                '*** storing Clusters to new RDAC File
                Set colClusters = New Collection
                For x = 0 To Val(txtClusterCount) - 1
                    colClusters.Add Val(txtClusterFrom) + x * Val(txtClusterOffset)
                Next
            End If
            
            If Not CancelExportFlag Then
                sFileName = EnsureSlash(frmSaveClusters.txtExportPath) & "LAZARUS_TK" & Right("000000" & Hex(colClusters(1)), 6) & ".RDAC"
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
            
                For Each vCluster In colClusters
                    btData = ReadDriveOrFile(oPart.PhysicalDrive, _
                        (oPart.ClusterStartSector + (vCluster - 2) * oPart.SectorsPerCluster), _
                        oPart.BytesPerCluster)
                    If Not IsArrayDimensioned(btData) Then
                        Out "Error accessing Cluster " & vCluster & "!"
                        ReDim btData(oPart.BytesPerCluster - 1)
                    End If
                    Put iFNr, , btData
                Next
                Close iFNr
                MsgBox "File " & sFileName & " written!", vbInformation, "Completed"
            End If
        End If
    End If
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    Set colFiles = Nothing
    Set oSelectedFile = Nothing
    Set oSelectedProject = Nothing
    If Not oPart Is Nothing Then
        oPart.CleanUp
        Set oPart = Nothing
    End If
End Sub

Private Sub Form_Resize()
'    On Error Resume Next
'    With Me.txtOut
'        .Width = Me.ScaleWidth - .Left * 2
'        .Height = Me.ScaleHeight - .Top * 2 - 500
'    End With
'    On Error GoTo 0
End Sub

Private Sub Out(Text As String, Optional NoLineBreak As Boolean)
    Me.txtOut.SelLength = 0
    If Len(Me.txtOut) > 20000 Then
        Me.txtOut = ""
    End If
    Me.txtOut.SelStart = Len(Me.txtOut)
    Me.txtOut.SelText = Text & IIf(NoLineBreak, "", vbCrLf)
    Me.Refresh
End Sub

Private Sub cmdPlay_Click()
'    Dim colClusters As Collection
    Dim x As Long
    Dim lNewClusters() As Long

    If cmdPlay.Caption = "Play" Then
        cmdPlay.Caption = "Stop"
        If optFileselect Then
            If Not oSelectedProject Is Nothing Then
                If oSelectedProject.Samplerate <> 0 And _
                    (oSelectedFile.ShortFilename Like "TK*" Or oSelectedFile.ShortFilename Like "TAKE*") Then
                    PlayClusters cbxDevice.ItemData(cbxDevice.ListIndex), oSelectedProject.Samplerate, _
                        oSelectedProject.Format, oPart, oSelectedFile.ClusterChain
                Else
                    MsgBox "Not a playable file!", vbExclamation, "Error:"
                End If
            Else
                MsgBox "Select a file first!", vbExclamation, "Error:"
            End If
        Else
            If Not IsValidUserInput Then
                MsgBox "Invalid Cluster Parameters!", vbCritical, "Error:"
            Else
                ReDim lNewClusters(Val(txtClusterCount))
                lNewClusters(0) = UBound(lNewClusters)
                For x = 1 To UBound(lNewClusters)
                    lNewClusters(x) = Val(txtClusterFrom) + (x - 1) * Val(txtClusterOffset)
                Next
                HighlightClusters lFat(0), lFat2File(0), UBound(lFat) + 1, pFatDibData, lCurHighlight(0), lNewClusters(0)
                picFat.Refresh
                lCurHighlight = lNewClusters
                
                PlayClusters cbxDevice.ItemData(cbxDevice.ListIndex), Val(cbxSampleRate), cbxMode, _
                    oPart, Array2Col(lNewClusters)
            End If
        End If
        cmdPlay.Caption = "Play"
    Else
        cmdPlay.Caption = "Play"
        StopClusterPlayback
    End If
End Sub

Private Function Array2Col(AnArray() As Long) As Collection
    Dim x As Long
    Set Array2Col = New Collection
    For x = 1 To UBound(AnArray)
        Array2Col.Add AnArray(x)
    Next
End Function

Public Sub PlayCallback(Position As Long)
    Static lPrevCluster As Long
    Dim lCol As Long
    Dim vCluster As Variant
    Dim x As Long
    Dim cRows As Long
    
    If Not lPrevCluster = 0 Then
        For x = 1 To UBound(lCurHighlight)
            If lCurHighlight(x) = lPrevCluster Then
                lCol = RGB(255, 0, 255)  'selected cluster
                Exit For
            End If
        Next
        If lCol <> RGB(0, 0, 0) Then
            'selected->done
        ElseIf lFat(lPrevCluster) = 0 Then
            lCol = RGB(0, 0, 0)     'not in use
        ElseIf lFat(lPrevCluster) >= &HFFFFF7 Then
            lCol = RGB(255, 255, 0) 'eof or bad cluster
        ElseIf lFat2File(lPrevCluster) <> 0 Then
            lCol = RGB(0, 255, 0)   'used cluster
        Else
            lCol = RGB(0, 100, 100) 'unempty, but unreferenced cluster
        End If
        cRows = (UBound(lFat) + 1) / 400
        RtlMoveMemory ByVal pFatDibData + ((cRows - 1 - lPrevCluster \ 400) * 1600 + (lPrevCluster Mod 400) * 2) * 4, lCol, 4
        RtlMoveMemory ByVal pFatDibData + ((cRows - 1 - lPrevCluster \ 400) * 1600 + (lPrevCluster Mod 400) * 2 + 1) * 4, lCol, 4
        RtlMoveMemory ByVal pFatDibData + ((cRows - 1 - lPrevCluster \ 400) * 1600 + 800 + (lPrevCluster Mod 400) * 2) * 4, lCol, 4
        RtlMoveMemory ByVal pFatDibData + ((cRows - 1 - lPrevCluster \ 400) * 1600 + 800 + (lPrevCluster Mod 400) * 2 + 1) * 4, lCol, 4
    End If
    
    lPrevCluster = Position
    If Position > 0 Then
        lCol = RGB(70, 180, 255)
        cRows = (UBound(lFat) + 1) / 400
        RtlMoveMemory ByVal pFatDibData + ((cRows - 1 - Position \ 400) * 1600 + (Position Mod 400) * 2) * 4, lCol, 4
        RtlMoveMemory ByVal pFatDibData + ((cRows - 1 - Position \ 400) * 1600 + (Position Mod 400) * 2 + 1) * 4, lCol, 4
        RtlMoveMemory ByVal pFatDibData + ((cRows - 1 - Position \ 400) * 1600 + 800 + (Position Mod 400) * 2) * 4, lCol, 4
        RtlMoveMemory ByVal pFatDibData + ((cRows - 1 - Position \ 400) * 1600 + 800 + (Position Mod 400) * 2 + 1) * 4, lCol, 4
    End If
    picFat.Refresh
End Sub

Private Sub optClusterplay_Click()
    Dim vCluster As Variant
    Dim lCol As Long
    
    txtClusterFrom.Enabled = True
    txtClusterFrom.BackColor = &H80000005
    txtClusterOffset.Enabled = True
    txtClusterOffset.BackColor = &H80000005
    txtClusterCount.Enabled = True
    txtClusterCount.BackColor = &H80000005
    cbxMode.Enabled = True
    cbxMode.BackColor = &H80000005
    cbxSampleRate.Enabled = True
    cbxSampleRate.BackColor = &H80000005
    
    HighLightClusterEntry
End Sub

Private Sub optFileselect_Click()
    txtClusterFrom.Enabled = False
    txtClusterFrom.BackColor = &H8000000B
    txtClusterOffset.Enabled = False
    txtClusterOffset.BackColor = &H8000000B
    txtClusterCount.Enabled = False
    txtClusterCount.BackColor = &H8000000B
    cbxMode.Enabled = False
    cbxMode.BackColor = &H8000000B
    cbxSampleRate.Enabled = False
    cbxSampleRate.BackColor = &H8000000B
    
    ReDim lNewClusters(0) As Long
    HighlightClusters lFat(0), lFat2File(0), UBound(lFat) + 1, pFatDibData, lCurHighlight(0), lNewClusters(0)
    lCurHighlight = lNewClusters
    picFat.Refresh
End Sub

Private Sub picFat_MouseDown(Button As Integer, Shift As Integer, x As Single, Y As Single)
    Dim vCluster As Variant
    Dim lCol As Long
    Dim oFile As clsDirectoryEntry
    Dim colClusters As Collection
    Dim c As Long
    
    If optFileselect Then
        vCluster = CLng((Y \ 2) * 400 + x \ 2)
        If lFat2File(vCluster) > 0 Then
            Set oFile = colFiles(lFat2File(vCluster))
            trvFiles.Nodes(oFile.FullName).EnsureVisible
            trvFiles.Nodes(oFile.FullName).Selected = True
            trvFiles_NodeClick trvFiles.Nodes(oFile.FullName)
        End If
    Else
        vCluster = CLng((Y \ 2) * 400 + x \ 2)
        txtClusterFrom.Text = vCluster
        cmdPlay_Click
    End If
End Sub

Private Sub trvFiles_NodeClick(ByVal Node As MSComctlLib.Node)
    Dim vCluster As Variant
    Dim lCol As Long
    Dim oSongFile As clsDirectoryEntry
    Dim btSongFile() As Byte
    Dim x As Long
    
    'unhighlight previously selected file:
    If Not oSelectedFile Is Nothing Then
        ReDim lNewClusters(0) As Long
        HighlightClusters lFat(0), lFat2File(0), UBound(lFat) + 1, pFatDibData, lCurHighlight(0), lNewClusters(0)
        lCurHighlight = lNewClusters
    End If
    
    'Show Info about selected Entry:
    If VarType(Node.Tag) = vbObject Then
        If Not Node.Tag.IsFolder Then
            Set oSelectedFile = Node.Tag
    
            ReDim lNewClusters(oSelectedFile.ClusterChain.Count) As Long
            lNewClusters(0) = oSelectedFile.ClusterChain.Count
            For x = 1 To oSelectedFile.ClusterChain.Count
                lNewClusters(x) = oSelectedFile.ClusterChain(x)
            Next
            HighlightClusters lFat(0), lFat2File(0), UBound(lFat) + 1, pFatDibData, lCurHighlight(0), lNewClusters(0)
            lCurHighlight = lNewClusters
            lblInfo.Caption = "File " & oSelectedFile.FullName & ", size: " & oSelectedFile.Size & _
                    " bytes, first cluster: " & oSelectedFile.ClusterChain(1) & ", " & _
                    oSelectedFile.ClusterChain.Count & " clusters total"
            'try get some info about the related project:
            On Error Resume Next
            Set oSongFile = oSelectedFile.Parent.SubEntryByName("SONG.*")
            On Error GoTo 0
            Set oSelectedProject = New clsVSProject
            If Not oSongFile Is Nothing Then
                btSongFile = oSongFile.FileData
                With oSelectedProject
                    .Extension = Right(oSongFile.ShortFilename, 3) 'defines machine (880EX/1680/2480/...)
                    .UnSerialize btSongFile, 0
                    lblInfo.Caption = lblInfo.Caption & ", belongs to Project: " & .Name & _
                        ", Mode: " & .Format & ", Samplerate: " & .Samplerate
                End With
            End If
        End If
    End If
    picFat.Refresh
End Sub


Private Sub scrFatPic_Change()
    If picFat.Height > frmAnalysis.ScaleHeight Then
        Me.picFat.Top = 4 - (picFat.Height - frmAnalysis.ScaleHeight + 20) * (scrFatPic.Value / 100)
    End If
End Sub

Private Sub scrFatPic_Scroll()
    scrFatPic_Change
End Sub

Private Function IsValidUserInput()
    Dim bValid As Boolean
    
    bValid = True
    If Not IsInteger(txtClusterCount) Then
        bValid = False
    ElseIf Not IsInteger(txtClusterFrom) Then
        bValid = False
    ElseIf Not IsInteger(txtClusterOffset) Then
        bValid = False
    ElseIf Not Val(txtClusterFrom) > 0 And Val(txtClusterFrom) <= cClusters Then
        bValid = False
    ElseIf Not Val(txtClusterOffset) > 0 And Val(txtClusterCount) > 0 Then
        bValid = False
    ElseIf Not Val(txtClusterFrom) + Val(txtClusterCount) * Val(txtClusterOffset) < cClusters Then
        bValid = False
    End If
    IsValidUserInput = bValid
End Function

Private Function IsInteger(Value As String) As Boolean
    If Value = "" Then
        Exit Function
    ElseIf Value Like String(Len(Value), "#") Then
        IsInteger = True
    End If
End Function

Private Sub txtClusterCount_LostFocus()
    HighLightClusterEntry
End Sub

Private Sub txtClusterFrom_LostFocus()
    HighLightClusterEntry
End Sub

Private Sub txtClusterOffset_LostFocus()
    HighLightClusterEntry
End Sub

Private Sub HighLightClusterEntry()
    Dim lNewClusters() As Long
    Dim x As Long
    
    If IsValidUserInput Then
        ReDim lNewClusters(Val(txtClusterCount))
        lNewClusters(0) = UBound(lNewClusters)
        For x = 1 To UBound(lNewClusters)
            lNewClusters(x) = Val(txtClusterFrom) + (x - 1) * Val(txtClusterOffset)
        Next
        HighlightClusters lFat(0), lFat2File(0), UBound(lFat) + 1, pFatDibData, lCurHighlight(0), lNewClusters(0)
        lCurHighlight = lNewClusters
        picFat.Refresh
    End If
End Sub
