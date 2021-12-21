VERSION 5.00
Begin VB.Form frmSaveClusters 
   BorderStyle     =   1  'Fest Einfach
   Caption         =   "Save Clusters"
   ClientHeight    =   1575
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   4680
   Icon            =   "frmSaveClusters.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1575
   ScaleWidth      =   4680
   StartUpPosition =   3  'Windows-Standard
   Begin VB.TextBox txtExportPath 
      Height          =   315
      Left            =   1680
      TabIndex        =   1
      Top             =   60
      Width           =   2835
   End
   Begin VB.CommandButton cmdBrowse 
      Height          =   315
      Left            =   1320
      Picture         =   "frmSaveClusters.frx":058A
      Style           =   1  'Grafisch
      TabIndex        =   0
      Top             =   60
      Width           =   315
   End
   Begin VB.CommandButton cmdCancel 
      Caption         =   "Cancel"
      Height          =   435
      Index           =   1
      Left            =   840
      TabIndex        =   3
      Top             =   960
      Width           =   1215
   End
   Begin VB.CommandButton cmdSave 
      Caption         =   "Save"
      Height          =   435
      Index           =   0
      Left            =   2460
      TabIndex        =   4
      Top             =   960
      Width           =   1215
   End
   Begin VB.ComboBox cbxFormat 
      Height          =   315
      ItemData        =   "frmSaveClusters.frx":0B14
      Left            =   1320
      List            =   "frmSaveClusters.frx":0B1E
      Style           =   2  'Dropdown-Liste
      TabIndex        =   2
      Top             =   480
      Width           =   1635
   End
   Begin VB.Label Label2 
      Alignment       =   1  'Rechts
      Caption         =   "Format:"
      Height          =   255
      Left            =   120
      TabIndex        =   6
      Top             =   540
      Width           =   1035
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Rechts
      Caption         =   "Save to Folder:"
      Height          =   255
      Left            =   120
      TabIndex        =   5
      Top             =   120
      Width           =   1155
   End
End
Attribute VB_Name = "frmSaveClusters"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Public bSave As Boolean

Private Sub cmdBrowse_Click()
    Dim sPath As String
    sPath = BrowseForFolder(Me.hwnd, Me.txtExportPath, "Save recovered Files to:")
    If sPath <> "" Then
        txtExportPath = sPath
    End If
End Sub

Private Sub cmdCancel_Click(Index As Integer)
    bSave = False
    Me.Hide
End Sub

Private Sub cmdSave_Click(Index As Integer)
    bSave = True
    Me.Hide
End Sub
