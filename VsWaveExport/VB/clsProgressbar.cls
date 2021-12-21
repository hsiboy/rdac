VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "clsProgressbar"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public picClient As PictureBox

Private lMin As Long
Private lMax As Long
Private lValue As Long

Property Set Client(PicBox As PictureBox)
    Set picClient = PicBox
    picClient.AutoRedraw = True
End Property

Property Get Min() As Long
    Min = lMin
End Property

Property Let Min(Value As Long)
    lMin = Value
    Redraw
End Property

Property Get Max() As Long
    Max = lMax
End Property

Property Let Max(Value As Long)
    lMax = Value
    Redraw
End Property

Property Get Value() As Long
    Value = lValue
End Property

Property Let Value(Value As Long)
    If Value < lMin Then
        lValue = lMin
    ElseIf Value > lMax Then
        lValue = lMax
    Else
        lValue = Value
    End If
    Redraw
End Property

Public Function Inc()
    If lValue < lMax Then
        lValue = lValue + 1
        Redraw
    End If
End Function

Private Sub Redraw()
    Static lOldValue As Long
    
    If lValue = lOldValue Then
        Exit Sub
    ElseIf lValue < lOldValue Then
        picClient.Cls
    End If
    If lValue > 0 Then
        picClient.Line (0, 0)-(picClient.Width / (lMax - lMin) * lValue, picClient.Height), , BF
    End If
    lOldValue = lValue
End Sub

