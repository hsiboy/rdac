Attribute VB_Name = "modFolderBrowser"
Option Explicit

'Konstanten
Private Const BIF_STATUSTEXT = &H4&
Private Const BIF_RETURNONLYFSDIRS = 1
Private Const BIF_DONTGOBELOWDOMAIN = 2
Private Const MAX_PATH = 260

Private Const WM_USER = &H400
Private Const BFFM_INITIALIZED = 1
Private Const BFFM_SELCHANGED = 2
Private Const BFFM_SETSTATUSTEXT = (WM_USER + 100)
Private Const BFFM_SETSELECTION = (WM_USER + 102)

'Die Struktur für den browser
Private Type BrowseInfo
hWndOwner As Long
pIDLRoot As Long
pszDisplayName As Long
lpszTitle As Long
ulFlags As Long
lpfnCallback As Long
lParam As Long
iImage As Long
End Type

'API Funktionen
Private Declare Function SHBrowseForFolder Lib "shell32" (lpbi As BrowseInfo) As Long
Private Declare Function SHGetPathFromIDList Lib "shell32" (ByVal pidList As Long, ByVal lpBuffer As String) As Long
Private Declare Sub CoTaskMemFree Lib "ole32.dll" (ByVal hMem As Long)
Private Declare Function lstrcat Lib "kernel32" Alias "lstrcatA" (ByVal lpString1 As String, ByVal lpString2 As String) As Long
Private Declare Function SendMessage Lib "user32" Alias "SendMessageA" (ByVal hWnd As Long, ByVal wMsg As Long, ByVal wParam As Long, ByVal lParam As String) As Long

'Speichert Startdverzeichnis für Callbackfunktion
Private m_StartDir As String

' ************************************************************
' Das ist die Hauptfunktion, welche von außen aufgerufen wird
' ************************************************************
Public Function BrowseForFolder( _
    ByVal nOwner_Hwnd As Long, _
    ByVal sStartDir As String, _
    ByVal sTitle As String) As String
    
    Dim BI As BrowseInfo
    Dim lpIDList As Long
    Dim sBuffer As String
    
    ' Speicher das Startverzeichnis für Callbackfunktion
    m_StartDir = sStartDir
    
    ' Struktur für Callbackfuntkion initialisieren
    With BI
        ' Besitzer des Formulars übergeben
        .hWndOwner = nOwner_Hwnd
        ' Flags setzen, Bedeutung siehe MSDN
        .ulFlags = BIF_RETURNONLYFSDIRS Or BIF_DONTGOBELOWDOMAIN Or BIF_STATUSTEXT
        .lpszTitle = lstrcat(sTitle, "")
        ' Adresse derRückruffunktion übergeben
        .lpfnCallback = cAddressOf(AddressOf cBFF_CallBack)
    End With
    
    ' SHBrowseForFolder erzeugt den Browser
    lpIDList = SHBrowseForFolder(BI)
    ' Auswahlergebnis auswerten
    If (lpIDList) Then
        sBuffer = Space$(MAX_PATH)
        SHGetPathFromIDList lpIDList, sBuffer
        sBuffer = Left(sBuffer, InStr(sBuffer, vbNullChar) - 1)
        
        CoTaskMemFree lpIDList
        
        BrowseForFolder = sBuffer
    Else
        BrowseForFolder = ""
    End If

End Function

' ************************************************************
'Das ist die Callbackfunkion, welche das Startverzeichnis festlegt
' ************************************************************

Private Function cBFF_CallBack(ByVal hWnd As Long, ByVal uMsg As Long, ByVal lp As Long, ByVal pData As Long) As Long
   Dim lpIDList As Long
   Dim sBuffer As String
    
   If uMsg = BFFM_INITIALIZED Then
      ' Startverzeichnis wird über API-Funktion gesetzt
      Call SendMessage(hWnd, BFFM_SETSELECTION, 1, m_StartDir)
    
   ElseIf uMsg = BFFM_SELCHANGED Then
    
      ' Statustext setzen (zeigt an, was Benutzer ausgewählt hat
      sBuffer = Space$(MAX_PATH)
      If Abs(SHGetPathFromIDList(lp, sBuffer)) Then
          Call SendMessage(hWnd, BFFM_SETSTATUSTEXT, 0, sBuffer)
      End If
      
   End If
   
   cBFF_CallBack = 0

End Function

' ************************************************************
' Wird verwendet, um die Adresse der Callbackfunktion zuzuweisen
' ************************************************************
Private Function cAddressOf(ByVal lValue As Long) As Long
    cAddressOf = lValue
End Function



