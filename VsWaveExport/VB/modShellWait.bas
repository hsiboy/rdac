Attribute VB_Name = "modShellWait"
'Dieser Source stammt von http://www.activevb.de
'und kann frei verwendet werden. Für eventuelle Schäden
'wird nicht gehaftet.

'Um Fehler oder Fragen zu klären, nutzen Sie bitte unser Forum.
'Ansonsten viel Spaß und Erfolg mit diesem Source!

'------------- Anfang Projektdatei Project1.vbp -------------
'--------- Anfang Formular "Form1" alias Form1.frm  ---------
' Steuerelement: Schaltfläche "Command1"
' Steuerelement: Beschriftungsfeld "Label1"

Option Explicit

'Diverse APIs deklarieren
Private Declare Function CreateProcess Lib "Kernel32" Alias _
                                            "CreateProcessA" ( _
    ByVal lpAppName As Long, _
    ByVal lpCmdLine As String, _
    ByVal lpProcAttr As Long, _
    ByVal lpThreadAttr As Long, _
    ByVal lpInheritedHandle As Long, _
    ByVal lpCreationFlags As Long, _
    ByVal lpEnv As Long, _
    ByVal lpCurDir As Long, _
    lpStartupInfo As STARTUPINFO, _
    lpProcessInfo As PROCESS_INFORMATION _
    ) As Long
     
Private Declare Function WaitForSingleObject Lib "Kernel32" ( _
    ByVal hHandle As Long, _
    ByVal dwMilliseconds As Long _
    ) As Long
    
Private Declare Function CloseHandle Lib "Kernel32" ( _
    ByVal hObject As Long _
    ) As Long
    
'Einige Konstanten benennen
Private Const NORMAL_PRIORITY_CLASS  As Long = &H20&
Private Const INFINITE As Long = -1&
Private Const WAIT_TIMEOUT As Long = 258&

'Einige Datentypen erstellen
Private Type STARTUPINFO
    cb As Long
    lpReserved As String
    lpDesktop As String
    lpTitle As String
    dwX As Long
    dwY As Long
    dwXSize As Long
    dwYSize As Long
    dwXCountChars As Long
    dwYCountChars As Long
    dwFillAttribute As Long
    dwFlags As Long
    wShowWindow As Integer
    cbReserved2 As Integer
    lpReserved2 As Integer
    hStdInput As Long
    hStdOutput As Long
    hStdError As Long
End Type

Private Type PROCESS_INFORMATION
    hProcess As Long
    hThread As Long
    dwProcessID As Long
    dwThreadID As Long
End Type

Public Function ShellWait(cmdline As String, Optional ByVal _
                        bShowApp As Boolean = False) As Boolean
    
    'Diese Funktion führt einen Befehl (in CmdLine) aus.
    'Dabei wird das sich öffnende Fenster unsichtbar gemacht.
    'Diese Funktion wird erst beendet, wenn der Befehl
    'vollständig abgearbeitet ist.
    
    'Speicher reservieren
    Dim uProc As PROCESS_INFORMATION
    Dim uStart As STARTUPINFO
    Dim lRetVal As Long
    
    'Die Datentypen initialisieren
    uStart.cb = Len(uStart)
    uStart.wShowWindow = Abs(bShowApp)
    uStart.dwFlags = 1
    
    'Fenster erzeugen
    lRetVal = CreateProcess(0&, cmdline, 0&, 0&, 1&, _
        NORMAL_PRIORITY_CLASS, 0&, 0&, uStart, uProc)
    
    If lRetVal = 0 Then
        Call MsgBox("Starten der Anwendung ist fehlgeschlagen!", _
            vbExclamation + vbOKOnly, App.Title)
        
        ShellWait = False
        Exit Function
    End If
    
    'Warten, bis Fenster beendet wurde
    'Dabei das eigene Fenster aktualisieren
    Do While WaitForSingleObject(uProc.hProcess, 100) = WAIT_TIMEOUT
        DoEvents
    Loop
    
    'Wenn man solange warten will, bis die Anwendung beendet
    'wird und nicht darauf achtet, dass die wartende Anwendung
    'dabei absolut zum Stillstand kommt.
    'lRetVal = WaitForSingleObject(uProc.hProcess, INFINITE)
    
    'Fenster schließen
    lRetVal = CloseHandle(uProc.hProcess)
    
    'Rückgabewert setzen
    ShellWait = (lRetVal <> 0)
End Function
