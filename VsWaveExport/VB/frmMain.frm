VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form frmMain 
   Caption         =   "VS Wave Export"
   ClientHeight    =   6225
   ClientLeft      =   165
   ClientTop       =   855
   ClientWidth     =   9885
   Icon            =   "frmMain.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form2"
   MaxButton       =   0   'False
   ScaleHeight     =   415
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   659
   StartUpPosition =   3  'Windows-Standard
   Begin VB.CommandButton cmdPack 
      Caption         =   "Pack"
      Height          =   495
      Left            =   4200
      TabIndex        =   6
      ToolTipText     =   "Pack selected tracks and region into selfextracting executable file"
      Top             =   4500
      Width           =   975
   End
   Begin VB.Frame fraTrackInfo 
      Appearance      =   0  '2D
      BackColor       =   &H80000005&
      BorderStyle     =   0  'Kein
      ForeColor       =   &H80000008&
      Height          =   315
      Left            =   3600
      TabIndex        =   28
      Top             =   0
      Visible         =   0   'False
      Width           =   1155
      Begin VB.Label lblTrackInfo 
         Appearance      =   0  '2D
         AutoSize        =   -1  'True
         BackColor       =   &H80000005&
         BorderStyle     =   1  'Fest Einfach
         Caption         =   "TrackInfo"
         ForeColor       =   &H80000008&
         Height          =   225
         Left            =   0
         TabIndex        =   29
         Top             =   0
         Width           =   720
      End
   End
   Begin VB.CheckBox chkIgnorePhraseParameters 
      Caption         =   "Ignore phrase parameters"
      Height          =   315
      Left            =   120
      TabIndex        =   4
      ToolTipText     =   "Ignore FadeIn/Out and Phrase Level parameters used by VS-2480"
      Top             =   4680
      Width           =   2955
   End
   Begin VB.PictureBox picTimeline 
      Appearance      =   0  '2D
      AutoRedraw      =   -1  'True
      BackColor       =   &H00000000&
      ForeColor       =   &H80000008&
      Height          =   960
      Left            =   120
      ScaleHeight     =   62
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   471
      TabIndex        =   22
      TabStop         =   0   'False
      Top             =   5160
      Width           =   7095
      Begin VB.PictureBox picTimelineMask 
         Appearance      =   0  '2D
         AutoRedraw      =   -1  'True
         AutoSize        =   -1  'True
         BackColor       =   &H80000005&
         ForeColor       =   &H80000008&
         Height          =   855
         Left            =   4080
         ScaleHeight     =   55
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   135
         TabIndex        =   25
         Top             =   60
         Visible         =   0   'False
         Width           =   2055
         Begin VB.PictureBox picTimelineSelected 
            Appearance      =   0  '2D
            AutoRedraw      =   -1  'True
            AutoSize        =   -1  'True
            BackColor       =   &H00000000&
            ForeColor       =   &H80000008&
            Height          =   435
            Left            =   960
            ScaleHeight     =   27
            ScaleMode       =   3  'Pixel
            ScaleWidth      =   31
            TabIndex        =   27
            Top             =   60
            Visible         =   0   'False
            Width           =   495
         End
         Begin VB.PictureBox picTimelineUnselected 
            Appearance      =   0  '2D
            AutoRedraw      =   -1  'True
            AutoSize        =   -1  'True
            BackColor       =   &H00000000&
            ForeColor       =   &H80000008&
            Height          =   435
            Left            =   60
            ScaleHeight     =   27
            ScaleMode       =   3  'Pixel
            ScaleWidth      =   31
            TabIndex        =   26
            Top             =   60
            Visible         =   0   'False
            Width           =   495
         End
      End
   End
   Begin VB.CheckBox chkProjectName 
      Caption         =   "Include project name in file names"
      Height          =   315
      Left            =   120
      TabIndex        =   3
      ToolTipText     =   "Creates files named [ProjectName]_[Track]-[VTrack]_[TrackName].wav"
      Top             =   4440
      Width           =   2955
   End
   Begin VB.Frame Frame1 
      Caption         =   "Export Start/Stop"
      Height          =   1635
      Left            =   7320
      TabIndex        =   18
      Top             =   4500
      Width           =   2475
      Begin VB.Frame Frame4 
         BorderStyle     =   0  'Kein
         Caption         =   "Frame2"
         Height          =   675
         Left            =   120
         TabIndex        =   20
         Top             =   900
         Width           =   1995
         Begin VB.TextBox txtEnd 
            BeginProperty Font 
               Name            =   "Lucida Console"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   285
            Left            =   900
            TabIndex        =   13
            Text            =   "00:00:00"
            Top             =   300
            Width           =   1095
         End
         Begin VB.OptionButton optEndType 
            Caption         =   "end at:"
            Height          =   255
            Index           =   1
            Left            =   0
            TabIndex        =   12
            Top             =   300
            Width           =   1215
         End
         Begin VB.OptionButton optEndType 
            Caption         =   "end after last event"
            Height          =   315
            Index           =   0
            Left            =   0
            TabIndex        =   11
            Top             =   0
            Value           =   -1  'True
            Width           =   2355
         End
      End
      Begin VB.Frame Frame2 
         BorderStyle     =   0  'Kein
         Caption         =   "Frame2"
         Height          =   615
         Left            =   120
         TabIndex        =   19
         Top             =   240
         Width           =   2055
         Begin VB.TextBox txtStart 
            BeginProperty Font 
               Name            =   "Lucida Console"
               Size            =   8.25
               Charset         =   0
               Weight          =   400
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   285
            Left            =   900
            TabIndex        =   10
            Text            =   "00:00:00"
            Top             =   300
            Width           =   1095
         End
         Begin VB.OptionButton optStartType 
            Caption         =   "start at:"
            Height          =   255
            Index           =   1
            Left            =   0
            TabIndex        =   9
            Top             =   300
            Width           =   915
         End
         Begin VB.OptionButton optStartType 
            Caption         =   "start at first event"
            Height          =   435
            Index           =   0
            Left            =   0
            TabIndex        =   8
            Top             =   -60
            Value           =   -1  'True
            Width           =   2115
         End
      End
   End
   Begin VB.CommandButton cmdBrowse 
      Height          =   315
      Left            =   1020
      Picture         =   "frmMain.frx":058A
      Style           =   1  'Grafisch
      TabIndex        =   1
      Top             =   4080
      Width           =   315
   End
   Begin VB.TextBox txtExportPath 
      Height          =   315
      Left            =   1380
      TabIndex        =   2
      Text            =   "C:\"
      Top             =   4080
      Width           =   5775
   End
   Begin VB.CommandButton cmdExport 
      Caption         =   "Export"
      Height          =   495
      Left            =   3180
      TabIndex        =   5
      ToolTipText     =   "Save selected tracks and region as wave files"
      Top             =   4500
      Width           =   975
   End
   Begin VB.CommandButton cmdSelectAll 
      Caption         =   "All"
      Height          =   315
      Left            =   7320
      TabIndex        =   7
      ToolTipText     =   "Select all Tracks"
      Top             =   4020
      Width           =   675
   End
   Begin VB.PictureBox picTracks 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   3690
      Left            =   7320
      Picture         =   "frmMain.frx":0B14
      ScaleHeight     =   242
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   162
      TabIndex        =   14
      TabStop         =   0   'False
      Top             =   300
      Width           =   2490
   End
   Begin MSComctlLib.TreeView trvDrives 
      Height          =   3675
      Left            =   120
      TabIndex        =   0
      Top             =   300
      Width           =   7035
      _ExtentX        =   12409
      _ExtentY        =   6482
      _Version        =   393217
      HideSelection   =   0   'False
      Indentation     =   706
      LabelEdit       =   1
      Style           =   7
      Appearance      =   1
      OLEDropMode     =   1
   End
   Begin VB.Line Line4 
      BorderColor     =   &H8000000E&
      X1              =   0
      X2              =   670
      Y1              =   1
      Y2              =   1
   End
   Begin VB.Line Line3 
      BorderColor     =   &H80000010&
      X1              =   0
      X2              =   670
      Y1              =   0
      Y2              =   0
   End
   Begin VB.Label lblMax 
      Alignment       =   1  'Rechts
      Caption         =   "00:00:00"
      BeginProperty Font 
         Name            =   "Lucida Console"
         Size            =   6
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   135
      Left            =   6420
      TabIndex        =   24
      Top             =   5040
      Width           =   735
   End
   Begin VB.Label lblZero 
      Caption         =   "00:00:00"
      BeginProperty Font 
         Name            =   "Lucida Console"
         Size            =   6
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   135
      Left            =   180
      TabIndex        =   23
      Top             =   5040
      Width           =   735
   End
   Begin VB.Line Line2 
      X1              =   480
      X2              =   480
      Y1              =   344
      Y2              =   335
   End
   Begin VB.Line Line1 
      X1              =   8
      X2              =   8
      Y1              =   344
      Y2              =   335
   End
   Begin VB.Label lblFreeSpace 
      Alignment       =   1  'Rechts
      Caption         =   "100,0 GB free on Drive"
      Height          =   255
      Left            =   5220
      TabIndex        =   21
      Top             =   4500
      Width           =   1875
   End
   Begin VB.Label Label4 
      Caption         =   "Export to:"
      Height          =   255
      Left            =   120
      TabIndex        =   17
      Top             =   4140
      Width           =   915
   End
   Begin VB.Label Label3 
      Caption         =   "Select Tracks:"
      Height          =   255
      Left            =   7320
      TabIndex        =   16
      Top             =   60
      Width           =   1995
   End
   Begin VB.Label Label2 
      Caption         =   "Roland drives and files:"
      Height          =   255
      Left            =   120
      TabIndex        =   15
      Top             =   60
      Width           =   1995
   End
   Begin VB.Menu mnuExtras 
      Caption         =   "Extras"
      Begin VB.Menu mnuRipper 
         Caption         =   "Rip from CD-R"
         Shortcut        =   ^R
      End
      Begin VB.Menu mnuShuffle 
         Caption         =   "Shuffle Partitions"
         Shortcut        =   ^S
      End
      Begin VB.Menu mnuAnalysis 
         Caption         =   "Partition Analysis"
         Shortcut        =   ^A
      End
   End
   Begin VB.Menu mnuAbout 
      Caption         =   "About"
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'******************************************************************************************
'*
'*                              This is VS Wave Export Version 1.23a
'*                              created by Daniel Aue (aka Danielo)
'*                                    released on 16.August 2007
'*                              not Licensed under any terms at all
'*
'******************************************************************************************
'* This VB Project includes:
'* .) direct Disk access and FAT32/16/12 interpretation for reading all 12
'*    partitions of a VS Hard Disk
'* .) code to parse the SongList and Eventlist Files on the VS HD
'* .) reconstitute the track out of the entries in the eventlist
'* .) passing the rdac data to an external dll for decoding to wave
'* .) storing the wave data as file
'* .) creating a selfextracting file, by adding the dll code and rdac data
'*    to an exe template (ExeStub.exe)
'* .) Partition Analysis: Fat and directory consistence verification and cluster access
'*    that makes it possible to recover recorded but unsaved audio data (see recover.doc)
'*
'* The RDAC decoding dll consists of two parts, the decodeMTP based on
'* work by Randy Gordon and decodeMT2 written by me.
'*
'* Randy is also the one we all have to thank for deciphering RDAC.
'* everything started on VS-Planet:
'* http://www.vsplanet.com/ubb/ultimatebb.php?ubb=get_topic;f=1;t=022118
'* Without his work, this would not have been possible.
'*
'* Please note that this is an Alpha Release, and is expected to contain several bugs.
'* Furthermore, although it was carefully tested, i cannot guarantee that
'* data loss or damage to the VS harddisk or any other part of you equipment
'* is impossible. If you use this software you do it at your own risk and
'* responsibility, especially if you modify this source code.

'HISTORY:
' V1.23a: added untested support for 880VX and 890 assuming the file format is identical
'         to 880/880EX. Released 16.Aug.2007
' V1.22a: bugfixed: Partition Analysis Wave Recovery, SFX Mode, added check for cluster
'         loops causing potential deadlock, Samplerate code in the Songfile now only uses
'         lower 4 Bits. Released 04.July 2007
' V1.21a: fixed a bug in CD-R ripped file mode. Released 16.May 2007
' V1.2a:  added: Partition Analysis, Partition Shuffler, Self Extracting executeable,
'         Bears CD ripper. Released on 12.May 2007
' V1.15b: fixed a bug in CD-R ripped file mode released 30.Dec.2006
' V1.14b: added drag and drop support for CD-R ripped Files, tested filebased 1680,
'         added untested 880EX
' V1.13b: added and tested VS1880 support, 1680 still untested,
'         added, but not tested(!) FAT12 support, release including source 16.12.2006
' V1.12b: added, but not tested(!) 1680 family support
' V1.11b: removed a bug in export start/stop time, and added Track display
' V1.1:   Bugfixed
' V1.0:   initial release including source 12.11.2006
'******************************************************************************************

Option Explicit

Const PRG_VERSION As String = "1.23 Alpha"

Private lTrack As Long, lVTrack As Long
Private oTracks() As clsTrack
Private bSelect As Boolean

Private oCurrentProject As clsVSProject

'Export from/to:
Private lStartSample As Long
Private lEndSample As Long
Private lEndSampleMax As Long   'the very last sample of the currently selected project

Private colUsedTracks As New Collection
Private colSelectedTracks As New Collection

Private bDontChange As Boolean  'used to prevent updating when setting start/stop values

Private fTimelineScale As Double

Private bIsExpDragAction As Boolean
Private bIsExpStartDragged As Boolean


'Private Sub cmdDiag_Click()
'    Dim oEventlist As clsDirectoryEntry
'    Dim btEventlist() As Byte
'    Dim iFNr As Integer
'
'    If MsgBox("Save the Eventlist for diagnosis to C:\EVENTLST." & oCurrentProject.Extension & "?", vbYesNo Or vbQuestion, "Save Eventlist:") = vbYes Then
'        Set oEventlist = oCurrentProject.Folder.SubEntryByName("EVENTLST.*")
'        btEventlist = oEventlist.FileData
'        iFNr = FreeFile
'        If FileExists("C:\" & oEventlist.ShortFilename) Then
'            Kill "C:\" & oEventlist.ShortFilename
'        End If
'        Open "C:\" & oEventlist.ShortFilename For Binary As iFNr
'        Put iFNr, , btEventlist
'        Close iFNr
'    End If
'End Sub
'
'Private Sub cmdDiag_Click()
'    Dim btData() As Byte
'    Dim iFNr As Integer
'    Dim oPart As clsPartition
'    Dim oSonglist As clsDirectoryEntry
'    Dim lSonglistCluster As Long
'
'    Dim sDriveName As String
'
'    If Me.trvDrives.SelectedItem Is Nothing Then
'        MsgBox "Select a Drive from the list first!", vbExclamation, "Error:"
'    ElseIf Not (Me.trvDrives.SelectedItem.Text Like "PHYSICALDRIVE*" Or _
'             Me.trvDrives.SelectedItem.Text Like "SCSI*") Then
'        MsgBox "Select a Drive from the list first!", vbExclamation, "Error:"
'    ElseIf Not Me.trvDrives.SelectedItem.Children > 4 Then
'        MsgBox "This Drive has only " & Me.trvDrives.SelectedItem.Children & " Partitions!" & vbCrLf & _
'            "A minimum of 5 partitions is required.", vbExclamation, "Error:"
'    Else
'        sDriveName = Me.trvDrives.SelectedItem.Text
'    End If
'
'    If MsgBox("Save a Partition Image of " & sDriveName & " for diagnosis to C:\Part.dat?", vbYesNo Or vbQuestion, "Save:") = vbYes Then
'        btData = ReadDriveOrFile(sDriveName, 0, 2 ^ 22)
'        iFNr = FreeFile
'        If FileExists("C:\Part.dat") Then
'            Kill "C:\Part.dat"
'        End If
'        Open "C:\Part.dat" For Binary As iFNr
'        Put iFNr, , btData
'
'        Set oPart = New clsPartition
'        oPart.InitPartition sDriveName, btData, 446 + 16 * 0 '1st Primary Partition
'        oPart.ReadPartition
'        Set oSonglist = oPart.Root.SubEntryByName("SONGLIST.*")
'        oSonglist.BuildClusterChain
'        lSonglistCluster = oSonglist.ClusterChain(1)
'
'        btData = ReadDriveOrFile(sDriveName, _
'            oPart.ClusterStartSector + (lSonglistCluster - 1 - 2) * oPart.SectorsPerCluster, _
'            oPart.BytesPerCluster * 3)
'        Put iFNr, , btData
'        Close iFNr
'        MsgBox "Done!", vbInformation, "completed"
'    End If
'End Sub




Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
    Dim x As Long
    Dim nd As Node
    Dim sFile As String

    Select Case KeyCode
    Case vbKeyF5
        'refresh for VirDis Files
        For x = trvDrives.Nodes.Count To 1 Step -1
            With trvDrives.Nodes(x)
                If .Text Like "VirDis:*" Then
                    sFile = .Text
                    trvDrives.Nodes.Remove .Index
                    ExamineDrive sFile
                End If
            End With
        Next
    End Select
End Sub

Private Sub Form_Load()
    ScanDrives
    txtExportPath = GetSetting("VSWaveExport", "Settings", "ExportPath", "C:\")
    chkProjectName = GetSetting("VSWaveExport", "Settings", "ProjectName", vbChecked)
    chkIgnorePhraseParameters = GetSetting("VSWaveExport", "Settings", "IgnorePhraseParameters", vbUnchecked)

    frmRipperDriver.RipperDestPath = GetSetting("VSWaveExport", "Settings", "RipDestPath", txtExportPath.Text)
    frmRipperDriver.RipperSourcePath = GetSetting("VSWaveExport", "Settings", "RipSourceDrive", "D")
    frmRipperDriver.chkOverwrite = GetSetting("VSWaveExport", "Settings", "RipOverwrite", vbChecked)
    
    lblFreeSpace.Caption = Format(GetFreeDiskSpace(txtExportPath) / 10 ^ 9, "0.0") & " GB free on Drive"
End Sub


Public Sub ScanDrives()
    'Trys to access PHYSICALDRIVE1 to 10 and reads the first 512 Bytes of the 1st primary
    'Partition to determine if the drive is Roland VS formated.
    Dim x As Long
    
    trvDrives.Nodes.Clear
    For x = 0 To 10
        ExamineDrive "PHYSICALDRIVE" & x
    Next
    For x = 0 To 7
        ExamineDrive "SCSI" & x & ":"
    Next
    
'    If trvDrives.Nodes.Count = 0 Then
'        MsgBox "No Roland Device found!", vbCritical, "Failed:"
'    End If
End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
    fraTrackInfo.Visible = False
End Sub

Private Sub Form_Paint()
    If frmProgress.Visible Then
        frmProgress.ZOrder 0
    End If
End Sub


Private Sub mnuAbout_Click()
    MsgBox "VS Wave Export Version " & PRG_VERSION & vbCrLf & "by Daniel Aue 2007" & vbCrLf & vbCrLf & _
        "The CD-R ripper was written by Don Speirs", vbInformation
End Sub

Private Sub mnuAnalysis_Click()
    If Not Me.trvDrives.SelectedItem Is Nothing Then
        If Not Me.trvDrives.SelectedItem.Text Like "Partition*" Then
            MsgBox "Select a Partition from the list first!", vbExclamation, "Error:"
        Else
            frmAnalysis.Analyze Me.trvDrives.SelectedItem.Tag
        End If
    Else
        MsgBox "Select a Partition from the list first!", vbExclamation, "Error:"
    End If
End Sub


Private Sub mnuRipper_Click()
    frmRipperDriver.StartDialog
End Sub

Private Sub mnuShuffle_Click()
    Dim nd As Node
    Dim liTemp As ListItem
    Dim oPart As clsPartition
    Dim x As Long
    
    
    If Not Me.trvDrives.SelectedItem Is Nothing Then
        If Me.trvDrives.SelectedItem.Text Like "PHYSICALDRIVE*" Or _
             Me.trvDrives.SelectedItem.Text Like "SCSI*" Then
            
            If Me.trvDrives.SelectedItem.Children > 4 Then
                frmShuffler.StartDialog Me.trvDrives.SelectedItem.Text
            Else
                MsgBox "This Drive has only " & Me.trvDrives.SelectedItem.Children & " Partitions!" & vbCrLf & _
                    "A minimum of 5 partitions is required.", vbExclamation, "Error:"
            End If
        Else
            MsgBox "Select a Drive from the list first!", vbExclamation, "Error:"
        End If
    Else
        MsgBox "Select a Drive from the list first!", vbExclamation, "Error:"
    End If
End Sub

Private Sub optEndType_Click(Index As Integer)
    If Index = 0 Then
        CalcAutoExportTime
        RefreshTimeControls
    End If
End Sub

Private Sub optStartType_Click(Index As Integer)
    If Index = 0 Then
        CalcAutoExportTime
        RefreshTimeControls
    End If
End Sub

Private Sub picTimeline_MouseUp(Button As Integer, Shift As Integer, x As Single, Y As Single)
    bIsExpDragAction = False
End Sub

Private Sub picTracks_MouseDown(Button As Integer, Shift As Integer, x As Single, Y As Single)
    Dim oTrack As clsTrack
    If Button = vbLeftButton Then
        If lTrack + lVTrack > 0 And Not oCurrentProject Is Nothing Then
            Set oTrack = oTracks(lTrack, lVTrack)
            If oTrack.Events.Count > 0 Then
                If Not oTrack.Selected And oTrack.Events.Count > 0 Then
                    bSelect = True
                    PaintSelection lTrack, lVTrack, True
                    oTrack.Selected = True
                    colSelectedTracks.Add oTrack, CStr(oTrack.TrackNr & "|" & oTrack.VTrackNr)
                    RedrawSelectedTracks True, oTrack
                    CalcAutoExportTime
                    RefreshTimeControls
                Else
                    bSelect = False
                    PaintSelection lTrack, lVTrack, False
                    oTrack.Selected = False
                    colSelectedTracks.Remove CStr(oTrack.TrackNr & "|" & oTrack.VTrackNr)
                    RedrawSelectedTracks False, oTrack
                    CalcAutoExportTime
                    RefreshTimeControls
                End If
            End If
        Else
            bSelect = True
        End If
    End If
End Sub

Private Sub picTracks_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
    If Y >= 2 Then
        lTrack = ((Y - 2) \ 10) + 1
    End If
    If x >= 2 Then
        lVTrack = ((x - 2) \ 10) + 1
    End If
    If (Y - 2) Mod 10 >= 8 Then lTrack = 0
    If (x - 2) Mod 10 >= 8 Then lVTrack = 0
    If lTrack = 0 Or lVTrack = 0 Or lTrack > 24 Or lVTrack > 16 Then
        lTrack = 0: lVTrack = 0
    End If
    
    If lTrack + lVTrack > 0 And Not oCurrentProject Is Nothing Then
        If Button = vbLeftButton Then
            If oTracks(lTrack, lVTrack).Events.Count > 0 Then
                If Not bSelect = oTracks(lTrack, lVTrack).Selected Then
                    If bSelect Then
                        colSelectedTracks.Add oTracks(lTrack, lVTrack), CStr(lTrack & "|" & lVTrack)
                    Else
                        colSelectedTracks.Remove CStr(lTrack & "|" & lVTrack)
                    End If
                    oTracks(lTrack, lVTrack).Selected = Not oTracks(lTrack, lVTrack).Selected
                    PaintSelection lTrack, lVTrack, bSelect
                    RedrawSelectedTracks bSelect, oTracks(lTrack, lVTrack)
                    CalcAutoExportTime
                    RefreshTimeControls
                End If
            End If
        End If
        
        If IsArrayDimensioned(oTracks) Then
            With oTracks(lTrack, lVTrack)
                SetTrackInfo "V.T " & .TrackNr & "-" & .VTrackNr & ": " & Trim(.Name)
            End With
            fraTrackInfo.Visible = True
            fraTrackInfo.Top = picTracks.Top + Y + 10
            If picTracks.Left + x + 10 + fraTrackInfo.Width > frmMain.ScaleX(frmMain.Width, vbTwips, vbPixels) Then
                fraTrackInfo.Left = frmMain.ScaleX(frmMain.Width, vbTwips, vbPixels) - fraTrackInfo.Width - 10
            Else
                fraTrackInfo.Left = picTracks.Left + x + 10
            End If
        End If
    Else
        fraTrackInfo.Visible = False
    End If
End Sub

Private Sub picTimeline_MouseDown(Button As Integer, Shift As Integer, x As Single, Y As Single)
    If Button = vbLeftButton Then
        If optStartType(0).Value = True And optEndType(0).Value = True Then
            'full automatic do nothing
            bIsExpDragAction = False
        ElseIf optStartType(1).Value = True And optEndType(1).Value = True Then
            'full manual, fix start, drag end
            lStartSample = x / fTimelineScale
            If lStartSample < 0 Then lStartSample = 0
            If lStartSample > lEndSampleMax Then lStartSample = lEndSampleMax
            lEndSample = lStartSample
            bIsExpDragAction = True
            bIsExpStartDragged = False
            RefreshTimeControls
        ElseIf optStartType(0).Value = True Then
            'automtic start, drag end
            bIsExpDragAction = True
            bIsExpStartDragged = False
            lEndSample = x / fTimelineScale
            If lEndSample < lStartSample Then lEndSample = lStartSample
            If lEndSample > lEndSampleMax Then lEndSample = lEndSampleMax
            RefreshTimeControls
        Else
            'automatic end, drag start
            bIsExpDragAction = True
            bIsExpStartDragged = True
            lStartSample = x / fTimelineScale
            If lStartSample > lEndSample Then lStartSample = lEndSample
            If lStartSample < 0 Then lStartSample = 0
            RefreshTimeControls
        End If
    End If
End Sub

Private Sub picTimeline_MouseMove(Button As Integer, Shift As Integer, x As Single, Y As Single)
    If Not oCurrentProject Is Nothing Then
        If Button = vbLeftButton Then
            If bIsExpDragAction Then
                If bIsExpStartDragged Then
                    lStartSample = x / fTimelineScale
                    If lStartSample > lEndSample Then lStartSample = lEndSample
                    If lStartSample < 0 Then lStartSample = 0
                Else
                    lEndSample = x / fTimelineScale
                    If lEndSample < lStartSample Then lEndSample = lStartSample
                    If lEndSample > lEndSampleMax Then lEndSample = lEndSampleMax
                End If
                RefreshTimeControls
            End If
        End If
        If optStartType(0) = True And optEndType(0) = True Then
            'full automatic->show track names
            If Int(Y / 2) + 1 <= colUsedTracks.Count Then
                With colUsedTracks(Int(Y / 2) + 1)
                    SetTrackInfo "V.T " & .TrackNr & "-" & .VTrackNr & ": " & Trim(.Name)
                End With
                fraTrackInfo.Visible = True
                fraTrackInfo.Left = picTimeline.Left + x + 10
                fraTrackInfo.Top = picTimeline.Top + Y + 2
            Else
                fraTrackInfo.Visible = False
            End If
        Else
            fraTrackInfo.Visible = False
        End If
    End If
End Sub


Private Sub CalcAutoExportTime()
    'recalcultes values for Exporttimes which are set to automatic
    Dim oTrack As clsTrack
    Dim oEvent As clsEvent
    Dim fFactor As Double
    Dim lMin As Long, lMax As Long
    
    If optStartType(0).Value = False And optEndType(0).Value = False Then
        'no automatic times required
        Exit Sub
    ElseIf oCurrentProject Is Nothing Then
        If optStartType(0).Value = True Then
            lStartSample = 0
        End If
        If optEndType(0).Value = True Then
            lEndSample = 0
        End If
        Exit Sub
    End If
    
    'calc start/end time of selected tracks
    lMin = 2 ^ 31 - 1
    lMax = 0
    For Each oTrack In colSelectedTracks
        If oTrack.Events(1).StartFrame < lMin Then
            lMin = oTrack.Events(1).StartFrame
        End If
        If oTrack.Events(oTrack.Events.Count).EndFrame > lMax Then
            lMax = oTrack.Events(oTrack.Events.Count).EndFrame
        End If
    Next
    If lMin = 2 ^ 31 - 1 Then lMin = 0
    
    If optStartType(0).Value = True Then
        lStartSample = lMin * oCurrentProject.SamplesPerRdacFrame
    End If
    
    If optEndType(0).Value = True Then
        lEndSample = lMax * oCurrentProject.SamplesPerRdacFrame + 1
    End If
End Sub


Private Function CalcStartEndFromTextbox() As Boolean
    'interprets text written in the start end time entry boxes and
    'changes ExportTimes which are set to manual
    Dim sParts() As String
    Dim vPart As Variant
    Dim lMin As Long, lMax As Long
    Dim lSampleRate As Long
    
    If oCurrentProject Is Nothing Then
        lSampleRate = 44100
    Else
        lSampleRate = oCurrentProject.Samplerate
    End If
    
    sParts = Split(txtStart.Text, ":")
    If UBound(sParts) <> 2 Then Exit Function
    For Each vPart In sParts
        If Not vPart Like String(Len(vPart), "#") Or Len(vPart) > 2 Then Exit Function
    Next
    lMin = (CLng(sParts(0)) * 60 * 60 * lSampleRate + CLng(sParts(1)) * 60 * lSampleRate + _
                CLng(sParts(2)) * lSampleRate)
    
    
    sParts = Split(txtEnd.Text, ":")
    If UBound(sParts) <> 2 Then Exit Function
    For Each vPart In sParts
        If Not vPart Like String(Len(vPart), "#") Or Len(vPart) > 2 Then Exit Function
    Next
    lMax = (CLng(sParts(0)) * 60 * 60 * lSampleRate + _
                    CLng(sParts(1)) * 60 * lSampleRate + CLng(sParts(2)) * lSampleRate)
    
    
    If lMin > lMax Then lMin = lMax
    If lMax > lEndSampleMax Then lMax = lEndSampleMax
    
    If optStartType(0) = False Then
        lStartSample = lMin
    End If
    
    If optEndType(0) = False Then
        lEndSample = lMax
    End If
    CalcStartEndFromTextbox = True
End Function


Private Sub RefreshTimeControls()
    'redraws Timeline and export start/stop markers & texts
    Dim lSampleRate As Long
    Dim oTrack As clsTrack
    Dim oEvent As clsEvent
    Dim x As Long
    Dim lTr As Long, lVTr As Long
    
    If oCurrentProject Is Nothing Then
        lSampleRate = 44100
    Else
        lSampleRate = oCurrentProject.Samplerate
    End If
    
    bDontChange = True
    txtStart.Text = Seconds2TimeString(lStartSample / lSampleRate)
    txtEnd.Text = Seconds2TimeString(lEndSample / lSampleRate)
    bDontChange = False
    
    'draw selection:
    picTimeline.Cls
    picTimeline.Line (lStartSample * fTimelineScale, 0)- _
            (lEndSample * fTimelineScale, picTimeline.ScaleHeight), RGB(70, 70, 70), BF
    'imprint unselected tracks:
    picTimeline.PaintPicture picTimelineMask.Image, 0, 0, , , 0, 0, , , vbSrcAnd
    picTimeline.PaintPicture picTimelineUnselected.Image, 0, 0, , , , , , , vbSrcPaint
    
    'draw highlit phrases:
    If Int((lEndSample - lStartSample) * fTimelineScale) > 0 Then
        picTimeline.PaintPicture picTimelineSelected.Image, _
                lStartSample * fTimelineScale, 0, _
                (lEndSample - lStartSample) * fTimelineScale, picTimeline.ScaleHeight, _
                lStartSample * fTimelineScale, 0, _
                (lEndSample - lStartSample) * fTimelineScale, picTimeline.ScaleHeight, _
                vbSrcPaint
    End If
    
    'draw from to markers:
    picTimeline.Line (lStartSample * fTimelineScale, 0)- _
            (lStartSample * fTimelineScale, picTimeline.ScaleHeight), _
            RGB(255, 0, 0)
    picTimeline.Line (lEndSample * fTimelineScale, 0)- _
            (lEndSample * fTimelineScale, picTimeline.ScaleHeight), _
            RGB(255, 0, 0)

End Sub


Private Sub cmdSelectAll_Click()
    Dim lTrack As Long, lVTrack As Long
    
    If Not oCurrentProject Is Nothing Then
        If colSelectedTracks.Count < colUsedTracks.Count Then
            Set colSelectedTracks = New Collection
            For lTrack = 1 To 24
                For lVTrack = 1 To 16
                    If oTracks(lTrack, lVTrack).Events.Count > 0 Then
                        oTracks(lTrack, lVTrack).Selected = True
                        PaintSelection lTrack, lVTrack, True
                        colSelectedTracks.Add oTracks(lTrack, lVTrack), CStr(lTrack & "|" & lVTrack)
                    End If
                Next
            Next
            RedrawSelectedTracks
        Else
            Set colSelectedTracks = New Collection
            picTimelineSelected.Cls
            For lTrack = 1 To 24
                For lVTrack = 1 To 16
                    If oTracks(lTrack, lVTrack).Events.Count > 0 Then
                        oTracks(lTrack, lVTrack).Selected = False
                        PaintSelection lTrack, lVTrack, False
                    End If
                Next
            Next
        End If
        CalcAutoExportTime
        RefreshTimeControls
    End If
End Sub

Private Sub RedrawSelectedTracks(Optional Add As Boolean, Optional Track As clsTrack)
    'draws all currently selected tracks into the backbuffer picTimelineSelected
    Dim oTrack As clsTrack
    Dim oEvent As clsEvent
    Dim x As Long
    
    If Track Is Nothing Then
        'draw all Selected ones
        picTimelineSelected.Cls
        For Each oTrack In colSelectedTracks
            For Each oEvent In oTrack.Events
                With oEvent
                    picTimelineSelected.Line _
                        (.StartSample * fTimelineScale, x * 2)- _
                        (.EndSample * fTimelineScale, x * 2 + 1), _
                        oTrack.Color, BF
                End With
            Next
            x = x + 1
        Next
        
    ElseIf Add Then
        x = Track.Index
        For Each oEvent In Track.Events
            With oEvent
                picTimelineSelected.Line _
                    (.StartSample * fTimelineScale, x * 2)- _
                    (.EndSample * fTimelineScale, x * 2 + 1), _
                    Track.Color, BF
            End With
        Next
    Else
        x = Track.Index
        For Each oEvent In Track.Events
            With oEvent
                picTimelineSelected.Line _
                    (.StartSample * fTimelineScale, x * 2)- _
                    (.EndSample * fTimelineScale, x * 2 + 1), _
                    0, BF
            End With
        Next
    End If
End Sub

Private Sub cmdBrowse_Click()
    Dim sPath As String
    sPath = BrowseForFolder(Me.hwnd, Me.txtExportPath, "Export Waves to:")
    If sPath <> "" Then
        txtExportPath = sPath
        lblFreeSpace.Caption = Format(GetFreeDiskSpace(txtExportPath) / 10 ^ 9, "0.0") & " GB free on Drive"
    End If
End Sub


Private Sub cmdExport_Click()
    If IsExportSelectionValid Then
        CancelExportFlag = False
        'check if required diskspace is available:
        If CDbl(colSelectedTracks.Count) * (lEndSample - lStartSample) * _
            (oCurrentProject.BitDepth / 8) > GetFreeDiskSpace(txtExportPath) Then
            If MsgBox("The required disk space to store these waves may exceed the available space." & vbCrLf & _
                "Continue anyway?", vbQuestion Or vbYesNo, "Warning:") = vbNo Then
                CancelExportFlag = True
            End If
        ElseIf (lEndSample - lStartSample) * (oCurrentProject.BitDepth / 8) > 2 ^ 31 Then
            MsgBox "The selected export area may result in wave files bigger than 2 Gigabyte." & vbCrLf & _
                "The Wave fileformat does not support such files.", vbExclamation, "Error:"
            CancelExportFlag = True
        End If
        If Not CancelExportFlag Then
            ExportTracks oCurrentProject, colSelectedTracks, lStartSample, _
                lEndSample, EnsureSlash(txtExportPath), chkProjectName = vbChecked, chkIgnorePhraseParameters
            If Not CancelExportFlag Then
                MsgBox colSelectedTracks.Count & " Tracks written to " & Me.txtExportPath, vbInformation, "Done!"
            End If
            lblFreeSpace.Caption = Format(GetFreeDiskSpace(txtExportPath) / 10 ^ 9, "0.0") & " GB free on Drive"
        End If
    End If
End Sub

Private Sub cmdPack_Click()
    If IsExportSelectionValid Then
        CancelExportFlag = False
        If (lEndSample - lStartSample) * (oCurrentProject.BitDepth / 8) > 2 ^ 31 Then
            MsgBox "The selected export area may result in wave files bigger than 2 Gigabyte." & vbCrLf & _
                "The Wave file format does not support such files.", vbExclamation, "Error:"
            CancelExportFlag = True
        End If
        If Not CancelExportFlag Then
            PackTracks oCurrentProject, colSelectedTracks, lStartSample, _
                lEndSample, EnsureSlash(txtExportPath), (chkProjectName = vbChecked), chkIgnorePhraseParameters
            If Not CancelExportFlag Then
                MsgBox colSelectedTracks.Count & " Tracks packed to " & EnsureSlash(Me.txtExportPath) & CreateValidFileName(oCurrentProject.Name & ".exe"), vbInformation, "Done!"
            End If
            lblFreeSpace.Caption = Format(GetFreeDiskSpace(txtExportPath) / 10 ^ 9, "0.0") & " GB free on Drive"
        End If
    End If
End Sub


Private Function IsExportSelectionValid() As Boolean
    If Not oCurrentProject Is Nothing Then
        
        Select Case oCurrentProject.Format
        
        Case "CDR", "LIV", "LV2"
            MsgBox "Only the following VS formats are supported: MTP, MT2, M24 and M16.", vbExclamation, "Unsupported format."
        
        Case "MTP", "M24", "M16", "MT2", "MT1"
            
            If colSelectedTracks.Count = 0 Then
                MsgBox "No Tracks selected!", vbExclamation, "Error:"
            ElseIf Not DirExists(EnsureSlash(txtExportPath)) Then
                If MsgBox("The selected Export Path does not exist!" & vbCrLf & "Shall it be created?", vbExclamation Or vbYesNo, "Error:") = vbYes Then
                    If CreatePath(EnsureSlash(txtExportPath)) Then
                        IsExportSelectionValid = True
                    Else
                        MsgBox "Failed to create path!", vbExclamation, "Error:"
                    End If
                End If
            ElseIf Not CalcStartEndFromTextbox Then
                MsgBox "The entered values for Start/End time are invalid!", vbExclamation, "Error:"
            ElseIf lStartSample > lEndSample Then
                MsgBox "The entered values for Start/End time are invalid!", vbExclamation, "Error:"
            Else
                IsExportSelectionValid = True
            End If
        Case Else
            MsgBox "Unknown Format.", vbExclamation, "Unknown"
        End Select
    Else
        MsgBox "No Project selected!", vbExclamation, "Error:"
    End If
End Function

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    Dim frm As Form
    Dim nd As Node
    
    On Error Resume Next
    
    SaveSetting "VSWaveExport", "Settings", "ExportPath", txtExportPath
    SaveSetting "VSWaveExport", "Settings", "ProjectName", chkProjectName
    SaveSetting "VSWaveExport", "Settings", "IgnorePhraseParameters", chkIgnorePhraseParameters

    SaveSetting "VSWaveExport", "Settings", "RipDestPath", frmRipperDriver.RipperDestPath
    SaveSetting "VSWaveExport", "Settings", "RipSourceDrive", frmRipperDriver.RipperSourcePath
    SaveSetting "VSWaveExport", "Settings", "RipOverwrite", frmRipperDriver.chkOverwrite
    
    For Each frm In Forms
        Unload frm
    Next
    For Each nd In trvDrives.Nodes
        If nd.Text Like "Partition*" Then
            nd.Tag.CleanUp
        End If
    Next
    
    On Error GoTo 0
End Sub


Private Function PaintSelection(Track As Long, VTrack As Long, Paint As Boolean)
    Dim x As Long, Y As Long, col As Long
    Y = 2 + (Track - 1) * 10 + 1
    x = 2 + (VTrack - 1) * 10 + 1
    
    picTracks.DrawWidth = 2
    If Paint Then
        col = RGB(255, 0, 0)
    Else
        col = RGB(192, 192, 192)
    End If
    picTracks.Line (x, Y)-(x + 6, Y + 6), col, B
End Function


Private Sub trvDrives_Expand(ByVal Node As MSComctlLib.Node)
    Dim oSonglist As clsDirectoryEntry
    Dim oProject As clsVSProject
    Dim ndProject As Node
    Dim btSongList() As Byte
    Dim colSongs As Collection
    Dim oPart As clsPartition

    If Node.Text Like "Partition*" Then
        If Node.Child = "Dummy" Then
            trvDrives.Nodes.Remove Node.Index + 1
            Me.MousePointer = vbHourglass
            DoEvents
            
            On Error Resume Next
            Set oPart = Node.Tag
            oPart.ReadPartition
            If Err.Number > 0 Then
                MsgBox Err.Description, vbCritical, "Error:"
            End If
            Me.MousePointer = vbNormal
            Set oSonglist = oPart.Root.SubEntryByName("SONGLIST.*")
            On Error GoTo 0

            If oSonglist Is Nothing Then
                MsgBox "No SONGLIST File found on the Partition!", vbCritical, "Error:"
            Else
                'Load Songlist
                btSongList = oSonglist.FileData
                'Check for reversed Byteorder known to be used by 1680/880 IDE drives:
                If btSongList(0) > 0 And btSongList(1) = 0 And _
                        (oSonglist.ShortFilename Like "SONGLIST.VR[5-9]") Then
                    oPart.IsSwappedIDE = True
                    SwapBytes btSongList(0), UBound(btSongList) + 1
                End If
                    
                Set colSongs = ParseSonglist(btSongList, oSonglist.ShortFilename)
                If Not colSongs Is Nothing Then
                    Set oPart.Tag = colSongs
                    For Each oProject In colSongs
                        With oProject
                            Set .Folder = oPart.Root.SubEntryByName(.FolderName)
                            Set ndProject = trvDrives.Nodes.Add(Node, tvwChild, , _
                                "Project: " & .Name & ", " & .Format & ", " & .Samplerate & "Hz, Folder: " & .FolderName)
                            Set ndProject.Tag = oProject
                        End With
                    Next
                Else
                    MsgBox "Unsuported Songlist File!", vbCritical, "Error:"
                End If
            End If
        End If
    End If
End Sub


Private Function ParseSonglist(btData() As Byte, FileName As String) As Collection
    Dim pNextEntry As Long
    Dim sTemp As String
    Dim oProject As clsVSProject
    Dim cEntries As Long
    
    Set ParseSonglist = New Collection
    cEntries = MakeLong(btData(1), btData(0), 0, 0)
    
    Select Case UCase(FileName)
    Case "SONGLIST.VR1", "SONGLIST.VS1"
        '2480/2400
        pNextEntry = 4  'unknown
        While cEntries > 0
            Set oProject = New clsVSProject
            With oProject
                .Extension = Right(UCase(FileName), 3)
                .UnSerialize btData, pNextEntry
            End With
            ParseSonglist.Add oProject
            pNextEntry = pNextEntry + 44
            cEntries = cEntries - 1
        Wend
    Case "SONGLIST.VR6", "SONGLIST.VR5"
        '1680/1880
        pNextEntry = 2  'unknown
        While cEntries > 0
            Set oProject = New clsVSProject
            With oProject
                .Extension = Right(UCase(FileName), 3)
                .UnSerialize btData, pNextEntry
            End With
            ParseSonglist.Add oProject
            pNextEntry = pNextEntry + 38
            cEntries = cEntries - 1
        Wend
    Case Else
        Set ParseSonglist = Nothing
    End Select
End Function


Private Sub trvDrives_NodeClick(ByVal Node As MSComctlLib.Node)
    If Node.Text Like "Project: *" Then
        If Not Node.Tag.Loaded Then
            Node.Tag.LoadProject
        End If
        Set Me.CurrentProject = Node.Tag
    Else
        Set Me.CurrentProject = Nothing
    End If
End Sub


Property Set CurrentProject(Project As clsVSProject)
    Dim x As Long, Y As Long, col As Long
    Dim lTrack As Long, lVTrack As Long
    Dim oTrack As clsTrack, oEvent As clsEvent
    
    'init for new project
    picTracks.Cls
    picTimelineUnselected.Cls
    picTimelineSelected.Cls
    picTimelineMask.Cls
    lStartSample = 0
    lEndSample = 0
    Set colUsedTracks = New Collection
    Set colSelectedTracks = New Collection
    
    Set oCurrentProject = Project
    If oCurrentProject Is Nothing Then
        Erase oTracks
    ElseIf Not oCurrentProject.Loaded Then
        Erase oTracks
        MsgBox "Unable to load project!", vbExclamation, "Error:"
    Else
        oTracks = oCurrentProject.Tracks
        'set up colUsedTracks and draw Used Markers:
        col = RGB(0, 255, 0)
        picTracks.DrawWidth = 2
        For lTrack = 1 To 24
            For lVTrack = 1 To 16
                oTracks(lTrack, lVTrack).Selected = False
                If oTracks(lTrack, lVTrack).Events.Count > 0 Then
                    'draw 'used' marker:
                    Y = 2 + (lTrack - 1) * 10 + 3
                    x = 2 + (lVTrack - 1) * 10 + 3
                    picTracks.Line (x, Y)-(x + 2, Y + 2), col, BF
                    colUsedTracks.Add oTracks(lTrack, lVTrack)
                End If
            Next
        Next
        If oCurrentProject.Format = "CDR" Then
            'draw stereo track separator line if CDR mode
            picTracks.DrawWidth = 1
            For lTrack = 3 To 23 Step 2
                picTracks.Line (0, (lTrack - 1) * 10)-(picTracks.ScaleWidth, (lTrack - 1) * 10), RGB(120, 120, 120)
                picTracks.Line (0, (lTrack - 1) * 10 + 1)-(picTracks.ScaleWidth, (lTrack - 1) * 10 + 1), RGB(250, 250, 250)
            Next
        End If
        
        'determine max through all tracks:
        lEndSampleMax = 0
        For Each oTrack In colUsedTracks
            If oTrack.Events(oTrack.Events.Count).EndFrame > lEndSampleMax Then
                lEndSampleMax = oTrack.Events(oTrack.Events.Count).EndFrame
            End If
        Next
        lEndSampleMax = lEndSampleMax * oCurrentProject.SamplesPerRdacFrame
        lblMax.Caption = Seconds2TimeString(lEndSampleMax / oCurrentProject.Samplerate)
        fTimelineScale = 1
        
        If lEndSampleMax > 0 Then
            fTimelineScale = (picTimeline.ScaleWidth - 1) / lEndSampleMax
            'rescale picture box and form if required:
            If colUsedTracks.Count > 36 Then
                picTimeline.Height = colUsedTracks.Count * 2 + 2
            Else
                picTimeline.Height = 36 * 2 + 2
            End If
            frmMain.Height = frmMain.ScaleY(picTimeline.Top + picTimeline.Height + 40, vbPixels, vbTwips)
            'adjust back buffers:
            picTimelineUnselected.Height = picTimeline.Height: picTimelineUnselected.Width = picTimeline.Width
            picTimelineMask.Height = picTimeline.Height: picTimelineMask.Width = picTimeline.Width
            picTimelineSelected.Height = picTimeline.Height: picTimelineSelected.Width = picTimeline.Width
            
            'draw the unselected tracks into backbuffer and create Mask:
            x = 0
            For Each oTrack In colUsedTracks
                oTrack.Color = Choose((x Mod 3) + 1, RGB(255, 0, 0), RGB(0, 230, 0), RGB(50, 50, 255))
                oTrack.Index = x
                For Each oEvent In oTrack.Events
                    picTimelineUnselected.Line (oEvent.StartSample * fTimelineScale, x * 2)- _
                        (oEvent.EndSample * fTimelineScale, x * 2 + 1), _
                    Choose((x Mod 3) + 1, RGB(100, 0, 0), RGB(0, 100, 0), RGB(0, 0, 100)), BF
                    
                    picTimelineMask.Line (oEvent.StartSample * fTimelineScale, x * 2)- _
                        (oEvent.EndSample * fTimelineScale, x * 2 + 1), _
                    RGB(0, 0, 0), BF
                Next
                x = x + 1
            Next
        End If
        
    End If
    CalcAutoExportTime
    RefreshTimeControls
End Property


Property Get CurrentProject() As clsVSProject
    Set CurrentProject = oCurrentProject
End Property


Private Sub trvDrives_OLEDragDrop(Data As MSComctlLib.DataObject, Effect As Long, Button As Integer, Shift As Integer, x As Single, Y As Single)
    Dim v As Variant
    Dim oProject As clsVSProject
    Dim ndProject As Node
    
    If Data.GetFormat(vbCFFiles) Then
        For Each v In Data.Files
            If CStr(v) Like "?:\" Then
                ExamineDrive Left(CStr(v), 2)
            ElseIf Right(CStr(v), 8) Like "SONG.*" Then
                Set oProject = New clsVSProject
                oProject.SongFile = CStr(v)
                With oProject
                    Set ndProject = trvDrives.Nodes.Add(, , , _
                        "Project: " & .Name & ", " & .Format & ", " & .Samplerate & "Hz, Folder: " & .FolderName)
                    Set ndProject.Tag = oProject
                End With
            ElseIf UCase(Right(CStr(v), 4)) = ".EXE" Then
                Set oProject = New clsVSProject
                oProject.PackedFile = CStr(v)
                With oProject
                    Set ndProject = trvDrives.Nodes.Add(, , , _
                        "Project: " & .Name & ", " & .Format & ", " & .Samplerate & "Hz, Folder: " & .FolderName)
                    Set ndProject.Tag = oProject
                End With
            ElseIf FileExists(CStr(v)) Then
                ExamineDrive "VirDis:" & v
            End If
        Next
    End If
End Sub

Private Sub txtEnd_Change()
    If Not bDontChange Then
        optEndType(1).Value = True
    End If
End Sub

Private Sub txtEnd_Validate(Cancel As Boolean)
    CalcStartEndFromTextbox
    RefreshTimeControls
End Sub

Private Sub txtStart_Validate(Cancel As Boolean)
    CalcStartEndFromTextbox
    RefreshTimeControls
End Sub

Private Sub txtStart_KeyPress(KeyAscii As Integer)
    If KeyAscii = 13 Then
        KeyAscii = 0
        CalcStartEndFromTextbox
        RefreshTimeControls
    End If
End Sub

Private Sub txtEnd_KeyPress(KeyAscii As Integer)
    If KeyAscii = 13 Then
        KeyAscii = 0
        CalcStartEndFromTextbox
        RefreshTimeControls
    End If
End Sub


Private Sub txtStart_Change()
    If Not bDontChange Then
        optStartType(1).Value = True
    End If
End Sub

Private Sub SetTrackInfo(ByVal Text As String)
    lblTrackInfo.Caption = " " & Text & " "
    fraTrackInfo.Width = frmMain.ScaleX(lblTrackInfo.Width, vbTwips, vbPixels)
    fraTrackInfo.Height = frmMain.ScaleY(lblTrackInfo.Height, vbTwips, vbPixels)
End Sub

