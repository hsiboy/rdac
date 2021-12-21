VERSION 5.00
Begin VB.Form frmProgress 
   BorderStyle     =   1  'Fest Einfach
   Caption         =   "Progress:"
   ClientHeight    =   1695
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   4620
   Icon            =   "frmProgress.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1695
   ScaleWidth      =   4620
   StartUpPosition =   3  'Windows-Standard
   Begin VB.PictureBox picPrgTrack 
      ForeColor       =   &H8000000D&
      Height          =   195
      Left            =   120
      ScaleHeight     =   135
      ScaleWidth      =   4335
      TabIndex        =   4
      TabStop         =   0   'False
      Top             =   780
      Width           =   4395
   End
   Begin VB.PictureBox picPrgTracks 
      ForeColor       =   &H8000000D&
      Height          =   195
      Left            =   120
      ScaleHeight     =   135
      ScaleWidth      =   4335
      TabIndex        =   3
      TabStop         =   0   'False
      Top             =   180
      Width           =   4395
   End
   Begin VB.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Caption         =   "Cancel"
      Height          =   375
      Left            =   3240
      TabIndex        =   0
      Top             =   1200
      Width           =   1215
   End
   Begin VB.Label lblTime 
      Caption         =   "00:00:00 (00:00:00 estimated)"
      Height          =   255
      Left            =   180
      TabIndex        =   2
      Top             =   1200
      Width           =   2355
   End
   Begin VB.Label lblCurrent 
      Alignment       =   2  'Zentriert
      Caption         =   "Label1"
      Height          =   255
      Left            =   180
      TabIndex        =   1
      Top             =   480
      Width           =   4275
   End
End
Attribute VB_Name = "frmProgress"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Public StartTime As Date

Public prgTrack As clsProgressbar
Public prgTracks As clsProgressbar


Private Sub cmdCancel_Click()
    frmProgress.Hide
    If MsgBox("Are you sure you want to cancel the ongoing Operation?", vbYesNo, "Sure?") = vbYes Then
        CancelExportFlag = True
    End If
    frmProgress.Show
End Sub


Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    If UnloadMode = 0 Then
        Cancel = True
    End If
End Sub

#If IsStub = 1 Then '8<---------------------------------- This Part is only compiled for ExeStub.exe

Private Sub Form_Load()
    Dim oProject As New clsVSProject
    Dim udtProjectHeader As SFX_PROJECT_HEADER
    Dim bt() As Byte
    Dim iFNr As Integer
    Dim sAppName As String
    
    Set prgTrack = New clsProgressbar
    Set prgTrack.Client = picPrgTrack
    Set prgTracks = New clsProgressbar
    Set prgTracks.Client = picPrgTracks
    
    sAppName = EnsureBackSlash(App.Path) & App.EXEName & ".exe"
'    sAppName = "C:\kill\Test.exe"   'todo
    oProject.PackedFile = sAppName
    If oProject.Loaded Then
        bt = oProject.SfxProjectHeader
        RtlMoveMemory udtProjectHeader, bt(0), LenB(udtProjectHeader)
        
        
        'unpack the decode dll
        If FileExists(EnsureBackSlash(App.Path) & "RDAC.dll") Then
            Kill EnsureBackSlash(App.Path) & "RDAC.dll"
        End If
        ReDim bt(SFX_DLL_SIZE - 1)
        iFNr = FreeFile
        Open sAppName For Binary As iFNr
        Get iFNr, SFX_EXE_SIZE + 1, bt
        Close iFNr
        Open EnsureBackSlash(App.Path) & "RDAC.dll" For Binary As iFNr
        Put iFNr, , bt
        Close iFNr
        
        With udtProjectHeader
            ExportTracks oProject, oProject.UsedTracks, .StartSample, .EndSample, EnsureBackSlash(App.Path), _
            .IncludeProjectName, .IgnorePhraseParameters, SelfExtracting
        End With
    End If
    Unload Me
End Sub

#Else '8<---------------------------------- This Part is only compiled for VSWaveExport.exe

Private Sub Form_Load()
    Set prgTrack = New clsProgressbar
    Set prgTrack.Client = picPrgTrack
    Set prgTracks = New clsProgressbar
    Set prgTracks.Client = picPrgTracks
End Sub

#End If '8<----------------------------------------------------------------------------------------------


