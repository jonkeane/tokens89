VERSION 5.00
Begin VB.UserControl Tokens89OCX 
   CanGetFocus     =   0   'False
   ClientHeight    =   210
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   2370
   Enabled         =   0   'False
   InvisibleAtRuntime=   -1  'True
   ScaleHeight     =   210
   ScaleWidth      =   2370
   Begin VB.Label Label1 
      Caption         =   "Tokens 89 OCX control (invisible)"
      Height          =   252
      Left            =   0
      TabIndex        =   0
      Top             =   0
      UseMnemonic     =   0   'False
      Width           =   2412
   End
End
Attribute VB_Name = "Tokens89OCX"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
' Tokens 89 OCX - tokenizer/detokenizer for the TI-89/92+/V200
' Copyright (C) 2000-2003 Kevin Kofler <kevin.kofler@chello.at>
' Copyright (C) 2013-2015 Peter Engels <tiedit@arcor.de>
'
' This library is free software; you can redistribute it and/or
' modify it under the terms of the GNU Lesser General Public
' License as published by the Free Software Foundation; either
' version 2.1 of the License, or (at your option) any later version.
'
' This library is distributed in the hope that it will be useful,
' but WITHOUT ANY WARRANTY; without even the implied warranty of
' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
' Lesser General Public License for more details.
'
' You should have received a copy of the GNU Lesser General Public
' License along with this library; if not, write to the Free Software
' Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

Public Enum FTypes
  ft89t = 0
  ft89p89f = 1
  ft89e89l89m89s = 2
End Enum

Private Type TIFile
  Signature As String * 8
  Fixed1(1 To 2) As Byte
  Folder As String * 8
  Description As String * &H28
  Fixed2(1 To 6) As Byte
  FileName As String * 8
  FileType As Byte
  FixedFT(1 To 3) As Byte
  FileSize As Long
  Fixed3(1 To 6) As Byte
  TISize(1 To 2) As Byte
End Type

Public Function OpenTI$(ByVal FN$)
Attribute OpenTI.VB_Description = "Takes the file name of a variable in TI-GraphLink binary format. Returns the data in readable form."
  OpenTI = DeToken(TIVar(FN))
End Function

Public Function GetTIFolder$(ByVal FN$)
Attribute GetTIFolder.VB_Description = "Takes the file name of a variable in TI-GraphLink binary format. Returns its on-calc folder name."
  Dim S8 As String * 8
  Open FN For Binary As #1
  Get #1, 11, S8
  Close #1
  If InStr(S8, Chr(0)) <> 0 Then
    GetTIFolder = Left(S8, InStr(S8, Chr(0)) - 1)
  Else
    GetTIFolder = S8
  End If
End Function

Public Function GetTIFile$(ByVal FN$)
Attribute GetTIFile.VB_Description = "Takes the file name of a variable in TI-GraphLink binary format. Returns its on-calc file name."
  Dim S8 As String * 8
  Open FN For Binary As #1
  Get #1, 65, S8
  Close #1
  If InStr(S8, Chr(0)) <> 0 Then
    GetTIFile = Left(S8, InStr(S8, Chr(0)) - 1)
  Else
    GetTIFile = S8
  End If
End Function

Public Function TIVar$(ByVal FN$)
Attribute TIVar.VB_Description = "Takes the file name of a variable in TI-GraphLink binary format. Returns the raw data contained in the variable as a string."
  Dim Size1 As Byte, Size2 As Byte, Size&
  Open FN For Binary As #1
  Get #1, &H57, Size1
  Get #1, &H58, Size2
  Size = 256& * Size1 + Size2
  TIVar = String(Size, " ")
  Get #1, &H59, TIVar
  Close #1
End Function

Public Function DeToken$(ByVal S$)
Attribute DeToken.VB_Description = "Takes the possibly tokenized raw data of a variable as returned by TIVar and returns the data in a readable form."
  Dim T As Byte, i&, n&, r&, c&, p&, S2$, S3$, list$(), context$, polar As Byte, unit$, Temp$, nBase As Byte
  DeToken = ""
  context = ""
  p = Len(S)
  While p >= 1
    T = Asc(Mid(S, p, 1))
    Select Case T
      Case 0 'variable
        S2 = ""
        For i = p - 1 To p - 17 Step -1
          If Mid(S, i, 1) = Chr(0) Then Exit For
          S2 = Mid(S, i, 1) & S2
        Next
        DeToken = DeToken & S2
        p = i - 1
      Case 1 To &HA 'q-z
        DeToken = DeToken & Chr(T + 112) '+Asc("q")-1
        p = p - 1
      Case &HB To &H1B 'a-q
        DeToken = DeToken & Chr(T + 86) '+Asc("a")-&HB
        p = p - 1
      Case &H1C 'System variable
        Select Case Asc(Mid(S, p - 1, 1))
        Case 1 'x_bar_
          DeToken = DeToken & Chr(154)
        Case 2 'y_bar_
          DeToken = DeToken & Chr(155)
        Case 3 '_SIGMA_x
          DeToken = DeToken & Chr(142) & "x"
        Case 4 '_SIGMA_x²
          DeToken = DeToken & Chr(142) & "x²"
        Case 5 '_SIGMA_y
          DeToken = DeToken & Chr(142) & "y"
        Case 6 '_SIGMA_y²
          DeToken = DeToken & Chr(142) & "y²"
        Case 7 '_SIGMA_xy
          DeToken = DeToken & Chr(142) & "xy"
        Case 8 'Sx
          DeToken = DeToken & "Sx"
        Case 9 'Sy
          DeToken = DeToken & "Sy"
        Case &HA '_sigma_x
          DeToken = DeToken & Chr(143) & "x"
        Case &HB '_sigma_y
          DeToken = DeToken & Chr(143) & "y"
        Case &HC 'nStat
          DeToken = DeToken & "nStat"
        Case &HD 'minX
          DeToken = DeToken & "minX"
        Case &HE 'minY
          DeToken = DeToken & "minY"
        Case &HF 'q1
          DeToken = DeToken & "q1"
        Case &H10 'medStat
          DeToken = DeToken & "medStat"
        Case &H11 'q3
          DeToken = DeToken & "q3"
        Case &H12 'maxX
          DeToken = DeToken & "maxX"
        Case &H13 'maxY
          DeToken = DeToken & "maxY"
        Case &H14 'corr
          DeToken = DeToken & "corr"
        Case &H15 'R²
          DeToken = DeToken & "R²"
        Case &H16 'medx1
          DeToken = DeToken & "medx1"
        Case &H17 'medx2
          DeToken = DeToken & "medx2"
        Case &H18 'medx3
          DeToken = DeToken & "medx3"
        Case &H19 'medy1
          DeToken = DeToken & "medy1"
        Case &H1A 'medy2
          DeToken = DeToken & "medy2"
        Case &H1B 'medy3
          DeToken = DeToken & "medy3"
        Case &H1C 'xc
          DeToken = DeToken & "xc"
        Case &H1D 'yc
          DeToken = DeToken & "yc"
        Case &H1E 'zc
          DeToken = DeToken & "zc"
        Case &H1F 'tc
          DeToken = DeToken & "tc"
        Case &H20 'rc
          DeToken = DeToken & "rc"
        Case &H21 '_theta_c
          DeToken = DeToken & Chr(136) & "c"
        Case &H22 'nc
          DeToken = DeToken & "nc"
        Case &H23 'xfact
          DeToken = DeToken & "xfact"
        Case &H24 'yfact
          DeToken = DeToken & "yfact"
        Case &H25 'zfact
          DeToken = DeToken & "zfact"
        Case &H26 'xmin
          DeToken = DeToken & "xmin"
        Case &H27 'xmax
          DeToken = DeToken & "xmax"
        Case &H28 'xscl
          DeToken = DeToken & "xscl"
        Case &H29 'ymin
          DeToken = DeToken & "ymin"
        Case &H2A 'ymax
          DeToken = DeToken & "ymax"
        Case &H2B 'yscl
          DeToken = DeToken & "yscl"
        Case &H2C '_DELTA_x
          DeToken = DeToken & Chr(132) & "x"
        Case &H2D '_DELTA_y
          DeToken = DeToken & Chr(132) & "y"
        Case &H2E 'xres
          DeToken = DeToken & "xres"
        Case &H2F 'xgrid
          DeToken = DeToken & "xgrid"
        Case &H30 'ygrid
          DeToken = DeToken & "ygrid"
        Case &H31 'zmin
          DeToken = DeToken & "zmin"
        Case &H32 'zmax
          DeToken = DeToken & "zmax"
        Case &H33 'zscl
          DeToken = DeToken & "zscl"
        Case &H34 'eye_theta_
          DeToken = DeToken & "eye" & Chr(136)
        Case &H35 'eye_phi_
          DeToken = DeToken & "eye" & Chr(145)
        Case &H36 '_theta_min
          DeToken = DeToken & Chr(136) & "min"
        Case &H37 '_theta_max
          DeToken = DeToken & Chr(136) & "max"
        Case &H38 '_theta_step
          DeToken = DeToken & Chr(136) & "step"
        Case &H39 'tmin
          DeToken = DeToken & "tmin"
        Case &H3A 'tmax
          DeToken = DeToken & "tmax"
        Case &H3B 'tstep
          DeToken = DeToken & "tstep"
        Case &H3C 'nmin
          DeToken = DeToken & "nmin"
        Case &H3D 'nmax
          DeToken = DeToken & "nmax"
        Case &H3E 'plotStrt
          DeToken = DeToken & "plotStrt"
        Case &H3F 'plotStep
          DeToken = DeToken & "plotStep"
        Case &H40 'zxmin
          DeToken = DeToken & "zxmin"
        Case &H41 'zxmax
          DeToken = DeToken & "zxmax"
        Case &H42 'zxscl
          DeToken = DeToken & "zxscl"
        Case &H43 'zymin
          DeToken = DeToken & "zymin"
        Case &H44 'zymax
          DeToken = DeToken & "zymax"
        Case &H45 'zyscl
          DeToken = DeToken & "zyscl"
        Case &H46 'zxres
          DeToken = DeToken & "zxres"
        Case &H47 'z_theta_min
          DeToken = DeToken & "z" & Chr(136) & "min"
        Case &H48 'z_theta_max
          DeToken = DeToken & "z" & Chr(136) & "max"
        Case &H49 'z_theta_step
          DeToken = DeToken & "z" & Chr(136) & "step"
        Case &H4A 'ztmin
          DeToken = DeToken & "ztmin"
        Case &H4B 'ztmax
          DeToken = DeToken & "ztmax"
        Case &H4C 'ztstep
          DeToken = DeToken & "ztstep"
        Case &H4D 'zxgrid
          DeToken = DeToken & "zxgrid"
        Case &H4E 'zygrid
          DeToken = DeToken & "zygrid"
        Case &H4F 'zzmin
          DeToken = DeToken & "zzmin"
        Case &H50 'zzmax
          DeToken = DeToken & "zzmax"
        Case &H51 'zzscl
          DeToken = DeToken & "zzscl"
        Case &H52 'zeye_theta_
          DeToken = DeToken & "zeye" & Chr(136)
        Case &H53 'zeye_phi_
          DeToken = DeToken & "zeye" & Chr(145)
        Case &H54 'znmin
          DeToken = DeToken & "znmin"
        Case &H55 'znmax
          DeToken = DeToken & "znmax"
        Case &H56 'zpltstep
          DeToken = DeToken & "zpltstep"
        Case &H57 'zpltstrt
          DeToken = DeToken & "zpltstrt"
        Case &H58 'seed1
          DeToken = DeToken & "seed1"
        Case &H59 'seed2
          DeToken = DeToken & "seed2"
        Case &H5A 'ok
          DeToken = DeToken & "ok"
        Case &H5B 'errornum
          DeToken = DeToken & "errornum"
        Case &H5C 'sysMath
          DeToken = DeToken & "sysMath"
        Case &H5D 'sysData
          DeToken = DeToken & "sysData"
          '&H5E is invalid!
        Case &H5F 'regCoef
          DeToken = DeToken & "regCoef"
        Case &H60 'tblInput
          DeToken = DeToken & "tblInput"
        Case &H61 'tblStart
          DeToken = DeToken & "tblStart"
        Case &H62 '_DELTA_tbl
          DeToken = DeToken & Chr(132) & "tbl"
          '&H63 is invalid!
        Case &H64 'eye_psi_
          DeToken = DeToken & "eye" & Chr(146)
        Case &H65 'tplot
          DeToken = DeToken & "tplot"
        Case &H66 'diftol
          DeToken = DeToken & "diftol"
        Case &H67 'zeye_psi_
          DeToken = DeToken & "zeye" & Chr(146)
        Case &H68 't0
          DeToken = DeToken & "t0"
        Case &H69 'dtime
          DeToken = DeToken & "dtime"
        Case &H6A 'ncurves
          DeToken = DeToken & "ncurves"
        Case &H6B 'fldres
          DeToken = DeToken & "fldres"
        Case &H6C 'Estep
          DeToken = DeToken & "Estep"
        Case &H6D 'zt0de
          DeToken = DeToken & "zt0de"
        Case &H6E 'ztmaxde
          DeToken = DeToken & "ztmaxde"
        Case &H6F 'ztstepde
          DeToken = DeToken & "ztstepde"
        Case &H70 'ztplotde
          DeToken = DeToken & "ztplotde"
        Case &H71 'ncontour
          DeToken = DeToken & "ncontour"
        Case Else
          DeToken = DeToken & "(INVALID SYSTEM VARIABLE: &h" & Hex(Asc(Mid(S, p - 1, 1))) & ")"
      End Select
      p = p - 2
      Case &H1D 'arbitrary real
        DeToken = DeToken & "@" & Asc(Mid(S, p - 1, 1))
        p = p - 2
      Case &H1E 'arbitrary integer
        DeToken = DeToken & "@n" & Asc(Mid(S, p - 1, 1))
        p = p - 2
      Case &H1F 'type: positive integer
        S2 = "0"
        n = Asc(Mid(S, p - 1, 1))
        For i = 1 To n
          S2 = StrAdd(S2, StrMult256P(Asc(Mid(S, p - i - 1, 1)), n - i))
        Next
        p = p - n - 2
        If nBase = 1 Then
          S2 = bStr(S2)
        ElseIf nBase = 2 Then
          S2 = hStr(S2)
        End If
        DeToken = DeToken & S2
        nBase = 0
      Case &H20 'type: negative integer
        If Len(context) > 0 And InStr("#12²", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        S2 = "0"
        n = Asc(Mid(S, p - 1, 1))
        For i = 1 To n
          S2 = StrAdd(S2, StrMult256P(Asc(Mid(S, p - i - 1, 1)), n - i))
        Next
        p = p - n - 2
        DeToken = DeToken & Chr(173) & S2
      Case &H21 'type: positive fraction
        If Len(context) > 0 And InStr("#12²345", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        S2 = "0"
        n = Asc(Mid(S, p - 1, 1))
        For i = 1 To n
          S2 = StrAdd(S2, StrMult256P(Asc(Mid(S, p - i - 1, 1)), n - i))
        Next
        p = p - n - 1
        DeToken = DeToken & S2
        S2 = "0"
        n = Asc(Mid(S, p - 1, 1))
        For i = 1 To n
          S2 = StrAdd(S2, StrMult256P(Asc(Mid(S, p - i - 1, 1)), n - i))
        Next
        p = p - n - 2
        DeToken = DeToken & "/" & S2
      Case &H22 'type: negative fraction
        If Len(context) > 0 And InStr("#12²345", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        S2 = "0"
        n = Asc(Mid(S, p - 1, 1))
        For i = 1 To n
          S2 = StrAdd(S2, StrMult256P(Asc(Mid(S, p - i - 1, 1)), n - i))
        Next
        p = p - n - 1
        DeToken = DeToken & Chr(173) & S2
        S2 = "0"
        n = Asc(Mid(S, p - 1, 1))
        For i = 1 To n
          S2 = StrAdd(S2, StrMult256P(Asc(Mid(S, p - i - 1, 1)), n - i))
        Next
        p = p - n - 2
        DeToken = DeToken & "/" & S2
      Case &H23 'type: float
        Select Case Mid(S, p - 9, 9)
        Case String(9, 0), Chr(&H40) & String(8, 0), Chr(&H80) & String(8, 0) '0
          DeToken = DeToken & "0."
        Case Chr(&H7F) & Chr(&HFF) & Chr(&HAA) & String(7, 0), Chr(&H7F) & Chr(&HFF) & Chr(&HAA) & Chr(0) & Chr(&HCC) & String(4, 0) 'undef
          DeToken = DeToken & "undef"
        Case Chr(&H7F) & Chr(&HFF) & Chr(&HAA) & Chr(0) & Chr(&HBB) & String(4, 0) '_infinite_
          DeToken = DeToken & Chr(190)
        Case Chr(&HFF) & Chr(&HFF) & Chr(&HAA) & Chr(0) & Chr(&HBB) & String(4, 0) '- _infinite_
          DeToken = DeToken & Chr(173) & Chr(190)
        Case Else
          If Asc(Mid(S, p - 9, 1)) >= 128 Then
            S3 = Chr(173)
          Else
            S3 = ""
          End If
          S2 = ""
          T = Asc(Mid(S, p - 7, 1))
          S2 = S2 & (T \ 16) & "." & (T Mod 16)
          For i = p - 6 To p - 1
            T = Asc(Mid(S, i, 1))
            If T < 16 Then S2 = S2 & "0"
            S2 = S2 & Hex(T)
          Next
          i = ((CInt(Asc(Mid(S, p - 9, 1))) Mod 128 - 64) * 256 + CInt(Asc(Mid(S, p - 8, 1))))
          Select Case i
          Case 0 To 13
            S2 = S3 & Left(S2, 1) & Mid(S2, 3, i) & "." & Mid(S2, i + 3)
            While Right(S2, 1) = "0"
              S2 = Left(S2, Len(S2) - 1)
            Wend
          Case -1
            S2 = S3 & "." & Left(S2, 1) & Mid(S2, 3)
            While Right(S2, 1) = "0"
              S2 = Left(S2, Len(S2) - 1)
            Wend
          Case -2
            S2 = S3 & ".0" & Left(S2, 1) & Mid(S2, 3)
            While Right(S2, 1) = "0"
              S2 = Left(S2, Len(S2) - 1)
            Wend
          Case -3
            S2 = S3 & ".00" & Left(S2, 1) & Mid(S2, 3)
            While Right(S2, 1) = "0"
              S2 = Left(S2, Len(S2) - 1)
            Wend
          Case Else
            S2 = S3 & S2
            While Right(S2, 1) = "0"
              S2 = Left(S2, Len(S2) - 1)
            Wend
            If i < 0 Then
              S2 = S2 & Chr(149) & Chr(173) & (-i)
            Else
              S2 = S2 & Chr(149) & i
            End If
        End Select
        DeToken = DeToken & S2
      End Select
      p = p - 10
      Case &H24 '_pi_
        DeToken = DeToken & Chr(140)
        p = p - 1
      Case &H25 '_e_
        DeToken = DeToken & Chr(150)
        p = p - 1
      Case &H26 '_i_
        DeToken = DeToken & Chr(151)
        p = p - 1
      Case &H27 '- _infinite_
        DeToken = DeToken & Chr(173) & Chr(190)
        p = p - 1
      Case &H28 '_infinite_
        DeToken = DeToken & Chr(190)
        p = p - 1
      Case &H29, &H2A '(+/-) _infinite_, undef
        DeToken = DeToken & "undef"
        p = p - 1
      Case &H2B 'false
        DeToken = DeToken & "false"
        p = p - 1
      Case &H2C 'true
        DeToken = DeToken & "true"
        p = p - 1
      Case &H2D 'type: string
        S2 = ""
        For i = p - 2 To 1 Step -1
          If Mid(S, i, 1) = Chr(0) Then Exit For
          S2 = Mid(S, i, 1) & S2
        Next
        DeToken = DeToken & """" & Replace(S2, """", """""") & """"
        p = i - 1
      Case &H2E 'nothing
        p = p - 1
      Case &H2F 'cosh_^-1_
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "cosh" & Chr(180)
        context = "()" & context
        p = p - 1
      Case &H30 'sinh_^-1_
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "sinh" & Chr(180)
        context = "()" & context
        p = p - 1
      Case &H31 'tanh_^-1_
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "tanh" & Chr(180)
        context = "()" & context
        p = p - 1
      Case &H32 'sech_^-1_ (AMS 2.08)
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "sech" & Chr(180)
        context = "()" & context
        p = p - 1
      Case &H33 'csch_^-1_ (AMS 2.08)
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "csch" & Chr(180)
        context = "()" & context
        p = p - 1
      Case &H34 'coth_^-1_ (AMS 2.08)
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "coth" & Chr(180)
        context = "()" & context
        p = p - 1
      Case &H35 'cosh
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "cosh"
        context = "()" & context
        p = p - 1
      Case &H36 'sinh
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "sinh"
        context = "()" & context
        p = p - 1
      Case &H37 'tanh
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "tanh"
        context = "()" & context
        p = p - 1
      Case &H38 'sech (AMS 2.08)
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "sech"
        context = "()" & context
        p = p - 1
      Case &H39 'csch (AMS 2.08)
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "csch"
        context = "()" & context
        p = p - 1
      Case &H3A 'coth (AMS 2.08)
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "coth"
        context = "()" & context
        p = p - 1
      Case &H3B 'cos_^-1_
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "cos" & Chr(180)
        context = "()" & context
        p = p - 1
      Case &H3C 'sin_^-1_
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "sin" & Chr(180)
        context = "()" & context
        p = p - 1
      Case &H3D 'tan_^-1_
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "tan" & Chr(180)
        context = "()" & context
        p = p - 1
      Case &H3E 'sec_^-1_ (AMS 2.08)
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "sec" & Chr(180)
        context = "()" & context
        p = p - 1
      Case &H3F 'csc_^-1_ (AMS 2.08)
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "csc" & Chr(180)
        context = "()" & context
        p = p - 1
      Case &H40 'cot_^-1_ (AMS 2.08)
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "cot" & Chr(180)
        context = "()" & context
        p = p - 1
        '&H41-&H43 are internal!
      Case &H44 'cos
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "cos"
        context = "()" & context
        p = p - 1
      Case &H45 'sin
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "sin"
        context = "()" & context
        p = p - 1
      Case &H46 'tan
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "tan"
        context = "()" & context
        p = p - 1
      Case &H47 'sec
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "sec"
        context = "()" & context
        p = p - 1
      Case &H48 'csc
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "csc"
        context = "()" & context
        p = p - 1
      Case &H49 'cot
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "cot"
        context = "()" & context
        p = p - 1
      Case &H4B 'abs
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "abs"
        context = "()" & context
        p = p - 1
      Case &H4C 'angle
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "angle"
        context = "()" & context
        p = p - 1
      Case &H4D 'ceiling
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "ceiling"
        context = "()" & context
        p = p - 1
      Case &H4E 'floor
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "floor"
        context = "()" & context
        p = p - 1
      Case &H4F 'int
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "int"
        context = "()" & context
        p = p - 1
      Case &H50 'sign
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "sign"
        context = "()" & context
        p = p - 1
      Case &H51 '_sqrt_
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & Chr(168)
        context = "()" & context
        p = p - 1
      Case &H52 '_e_^
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & Chr(150) & "^"
        context = "()" & context
        p = p - 1
      Case &H53 'ln
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "ln"
        context = "()" & context
        p = p - 1
      Case &H54 'log
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "log"
        context = "()" & context
        p = p - 1
      Case &H55 'fPart
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "fPart"
        context = "()" & context
        p = p - 1
      Case &H56 'iPart
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "iPart"
        context = "()" & context
        p = p - 1
      Case &H57 'conj
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "conj"
        context = "()" & context
        p = p - 1
      Case &H58 'imag
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "imag"
        context = "()" & context
        p = p - 1
      Case &H59 'real
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "real"
        context = "()" & context
        p = p - 1
      Case &H5A 'approx
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "approx"
        context = "()" & context
        p = p - 1
      Case &H5B 'tExpand
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "tExpand"
        context = "()" & context
        p = p - 1
      Case &H5C 'tCollect
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "tCollect"
        context = "()" & context
        p = p - 1
      Case &H5D 'getDenom
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "getDenom"
        context = "()" & context
        p = p - 1
      Case &H5E 'getNum
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "getNum"
        context = "()" & context
        p = p - 1
        '&H5F is invalid!
      Case &H60 'cumSum
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "cumSum"
        context = "()" & context
        p = p - 1
      Case &H61 'det
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "det"
        context = "()" & context
        p = p - 1
      Case &H62 'colNorm
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "colNorm"
        context = "()" & context
        p = p - 1
      Case &H63 'rowNorm
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "rowNorm"
        context = "()" & context
        p = p - 1
      Case &H64 'norm
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "norm"
        context = "()" & context
        p = p - 1
      Case &H65 'mean
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "mean"
        context = "()" & context
        p = p - 1
      Case &H66 'median
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "median"
        context = "()" & context
        p = p - 1
      Case &H67 'product
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "product"
        context = "()" & context
        p = p - 1
      Case &H68 'stdDev
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "stdDev"
        context = "()" & context
        p = p - 1
      Case &H69 'sum
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "sum"
        context = "()" & context
        p = p - 1
      Case &H6A 'variance
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "variance"
        context = "()" & context
        p = p - 1
      Case &H6B 'unitV
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "unitV"
        context = "()" & context
        p = p - 1
      Case &H6C 'dim
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "dim"
        context = "()" & context
        p = p - 1
      Case &H6D 'mat_>_list
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "mat" & Chr(18) & "list"
        context = "()" & context
        p = p - 1
      Case &H6E 'newList
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "newList"
        context = "()" & context
        p = p - 1
      Case &H6F 'rref
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "rref"
        context = "()" & context
        p = p - 1
      Case &H70 'ref
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "ref"
        context = "()" & context
        p = p - 1
      Case &H71 'identity
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "identity"
        context = "()" & context
        p = p - 1
      Case &H72 'diag
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "diag"
        context = "()" & context
        p = p - 1
      Case &H73 'colDim
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "colDim"
        context = "()" & context
        p = p - 1
      Case &H74 'rowDim
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "rowDim"
        context = "()" & context
        p = p - 1
      Case &H75 '_transpose_
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        context = "i1" & Chr(153) & context
        p = p - 1
      Case &H76 '!
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        context = "i1!" & context
        p = p - 1
      Case &H77 '%
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        context = "i1%" & context
        p = p - 1
      Case &H78 '_radians_
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        context = "i1" & Chr(152) & context
        p = p - 1
      Case &H79 'not
        If Len(context) > 0 And InStr("#12²345V6I7S", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "not "
        context = "i8" & context
        p = p - 1
      Case &H7A '_negate_
        If Len(context) > 0 And InStr("#12²", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        context = Chr(173) & "3" & context
        p = p - 1
      Case &H7B 'type: polar vector
        polar = 1
        p = p - 1
        context = " " & context
        If Right(DeToken, 1) = " " Then DeToken = Left(DeToken, Len(DeToken) - 1)
      Case &H7C 'type: cylindric vector
        polar = 1
        p = p - 1
        context = " " & context
        If Right(DeToken, 1) = " " Then DeToken = Left(DeToken, Len(DeToken) - 1)
      Case &H7D 'type: sphere vector
        polar = 3
        p = p - 1
        context = " " & context
        If Right(DeToken, 1) = " " Then DeToken = Left(DeToken, Len(DeToken) - 1)
        '&H7E-&H7F are internal!
      Case &H80 '_->_
        If Len(context) > 0 And InStr("#12²345V6I7S89XaAbBc", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        Temp = DeToken & Chr(0) & Temp
        DeToken = ""
        context = "icUC" & Chr(22) & context
        p = p - 1
      Case &H81 '|
        If Len(context) > 0 And InStr("#12²345V6I7S89XaAb", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        context = "iB|b" & context
        p = p - 1
      Case &H82 'xor
        If Len(context) > 0 And InStr("#12²345V6I7S89Xa", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        context = "iAxa" & context
        p = p - 1
      Case &H83 'or
        If Len(context) > 0 And InStr("#12²345V6I7S89Xa", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        context = "iAoa" & context
        p = p - 1
      Case &H84 'and
        If Len(context) > 0 And InStr("#12²345V6I7S89", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        context = "iXd9" & context
        p = p - 1
      Case &H85 '<
        If Len(context) > 0 And InStr("#12²345V6I7", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        context = "iS<7" & context
        p = p - 1
      Case &H86 '<=
        If Len(context) > 0 And InStr("#12²345V6I7", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        context = "iS" & Chr(156) & "7" & context
        p = p - 1
      Case &H87 '=
        If Len(context) > 0 And InStr("#12²345V6I7", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        context = "iS=7" & context
        p = p - 1
      Case &H88 '>=
        If Len(context) > 0 And InStr("#12²345V6I7", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        context = "iS" & Chr(158) & "7" & context
        p = p - 1
      Case &H89 '>
        If Len(context) > 0 And InStr("#12²345V6I7", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        context = "iS>7" & context
        p = p - 1
      Case &H8A '/=
        If Len(context) > 0 And InStr("#12²345V6I7", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        context = "iS" & Chr(157) & "7" & context
        p = p - 1
      Case &H8B '+
        If Len(context) > 0 And InStr("#12²345V6", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        Temp = DeToken & Chr(0) & Temp
        DeToken = ""
        context = "i6UI+" & context
        p = p - 1
      Case &H8C '.+
        If Len(context) > 0 And InStr("#12²345V6", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        Temp = DeToken & Chr(0) & Temp
        DeToken = ""
        context = "i6UI.+" & context
        p = p - 1
      Case &H8D '-
        If Len(context) > 0 And InStr("#12²345V6", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        Temp = DeToken & Chr(0) & Temp
        DeToken = ""
        context = "i6UI-" & context
        p = p - 1
      Case &H8E '.-
        If Len(context) > 0 And InStr("#12²345V6", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        Temp = DeToken & Chr(0) & Temp
        DeToken = ""
        context = "i6UI.-" & context
        p = p - 1
      Case &H8F '*
        If Len(context) > 0 And InStr("#12²345", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        Temp = DeToken & Chr(0) & Temp
        DeToken = ""
        context = "i5UV*" & context
        p = p - 1
      Case &H90 '.*
        If Len(context) > 0 And InStr("#12²345", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        Temp = DeToken & Chr(0) & Temp
        DeToken = ""
        context = "i5UV.*" & context
        p = p - 1
      Case &H91 '/
        If Len(context) > 0 And InStr("#12²345", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        Temp = DeToken & Chr(0) & Temp
        DeToken = ""
        context = "i5UV/" & context
        p = p - 1
      Case &H92 './
        If Len(context) > 0 And InStr("#12²345", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        Temp = DeToken & Chr(0) & Temp
        DeToken = ""
        context = "i5UV./" & context
        p = p - 1
      Case &H93 '^
        If Len(context) > 0 And InStr("#12", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        context = "i2^²" & context
        p = p - 1
      Case &H94 '.^
        If Len(context) > 0 And InStr("#12", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        context = "i2.^²" & context
        p = p - 1
        '&H95 is internal!
      Case &H96 'solve
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "solve"
        context = "(,)" & context
        p = p - 1
      Case &H97 'cSolve
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "cSolve"
        context = "(,)" & context
        p = p - 1
      Case &H98 'nSolve
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "nSolve"
        context = "(,)" & context
        p = p - 1
      Case &H99 'zeros
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "zeros"
        context = "(,)" & context
        p = p - 1
      Case &H9A 'cZeros
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "cZeros"
        context = "(,)" & context
        p = p - 1
      Case &H9B 'fMin
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "fMin"
        context = "(,)" & context
        p = p - 1
      Case &H9C 'fMax
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "fMax"
        context = "(,)" & context
        p = p - 1
        '&H9D is internal!
      Case &H9E 'polyEval
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "polyEval"
        context = "(,)" & context
        p = p - 1
      Case &H9F 'randPoly
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "randPoly"
        context = "(,)" & context
        p = p - 1
      Case &HA0 'crossP
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "crossP"
        context = "(,)" & context
        p = p - 1
      Case &HA1 'dotP
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "dotP"
        context = "(,)" & context
        p = p - 1
      Case &HA2 'gcd
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "gcd"
        context = "(,)" & context
        p = p - 1
      Case &HA3 'lcm
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "lcm"
        context = "(,)" & context
        p = p - 1
      Case &HA4 'mod
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "mod"
        context = "(,)" & context
        p = p - 1
      Case &HA5 'intDiv
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "intDiv"
        context = "(,)" & context
        p = p - 1
      Case &HA6 'remain
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "remain"
        context = "(,)" & context
        p = p - 1
      Case &HA7 'nCr
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "nCr"
        context = "(,)" & context
        p = p - 1
      Case &HA8 'nPr
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "nPr"
        context = "(,)" & context
        p = p - 1
      Case &HA9 'P_>_Rx
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "P" & Chr(18) & "Rx"
        context = "(,)" & context
        p = p - 1
      Case &HAA 'P_>_Ry
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "P" & Chr(18) & "Ry"
        context = "(,)" & context
        p = p - 1
      Case &HAB 'R_>_P_theta_
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "R" & Chr(18) & "P" & Chr(136)
        context = "(,)" & context
        p = p - 1
      Case &HAC 'R_>_Pr
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "R" & Chr(18) & "Pr"
        context = "(,)" & context
        p = p - 1
      Case &HAD 'augment
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "augment"
        context = "(,)" & context
        p = p - 1
      Case &HAE 'newMat
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "newMat"
        context = "(,)" & context
        p = p - 1
      Case &HAF 'randMat
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "randMat"
        context = "(,)" & context
        p = p - 1
      Case &HB0 'simult
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "simult"
        context = "(,)" & context
        p = p - 1
      Case &HB1 'part
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "part"
        context = "(e)" & context
        p = p - 1
      Case &HB2 'exp_>list
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "exp" & Chr(18) & "list"
        context = "(,)" & context
        p = p - 1
      Case &HB3 'randNorm
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "randNorm"
        context = "(,)" & context
        p = p - 1
      Case &HB4 'mRow
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "mRow"
        context = "(e)" & context
        p = p - 1
      Case &HB5 'rowAdd
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "rowAdd"
        context = "(e)" & context
        p = p - 1
      Case &HB6 'rowSwap
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "rowSwap"
        context = "(e)" & context
        p = p - 1
      Case &HB7 'arcLen
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "arcLen"
        context = "(e)" & context
        p = p - 1
      Case &HB8 'nInt
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "nInt"
        context = "(e)" & context
        p = p - 1
      Case &HB9 '_PI_
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & Chr(139)
        context = "(e)" & context
        p = p - 1
      Case &HBA '_SIGMA_
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & Chr(142)
        context = "(e)" & context
        p = p - 1
      Case &HBB 'mRowAdd
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "nInt"
        context = "(e)" & context
        p = p - 1
      Case &HBC 'ans
        'This is included, even if it is internal.
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "ans"
        context = "()" & context
        p = p - 1
      Case &HBD 'entry
        'This is included, even if it is internal.
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "entry"
        context = "()" & context
        p = p - 1
      Case &HBE 'exact
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "exact"
        context = "(e)" & context
        p = p - 1
      Case &HBF 'xlog
        If Len(context) > 0 And InStr("#12²345", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "ln"
        context = "(l)" & context
        p = p - 1
      Case &HC0 'comDenom
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "comDenom"
        context = "(e)" & context
        p = p - 1
      Case &HC1 'expand
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "expand"
        context = "(e)" & context
        p = p - 1
      Case &HC2 'factor
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "factor"
        context = "(e)" & context
        p = p - 1
      Case &HC3 'cFactor
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "cFactor"
        context = "(e)" & context
        p = p - 1
      Case &HC4 '_integrate_
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & Chr(189)
        context = "(e)" & context
        p = p - 1
      Case &HC5 '_differentiate_
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & Chr(188)
        context = "(e)" & context
        p = p - 1
      Case &HC6 'avgRC
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "avgRC"
        context = "(e)" & context
        p = p - 1
      Case &HC7 'nDeriv
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "nDeriv"
        context = "(e)" & context
        p = p - 1
      Case &HC8 'taylor
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "taylor"
        context = "(e)" & context
        p = p - 1
      Case &HC9 'limit
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "limit"
        context = "(e)" & context
        p = p - 1
      Case &HCA 'propFrac
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "propFrac"
        context = "(e)" & context
        p = p - 1
      Case &HCB 'when
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "when"
        context = "(e)" & context
        p = p - 1
      Case &HCC 'round
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "round"
        context = "(e)" & context
        p = p - 1
      Case &HCD 'DMS
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        'context = "i1°1'1""e" & context
        context = "i°'""" & context
        p = p - 1
      Case &HCE 'left
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "left"
        context = "(e)" & context
        p = p - 1
      Case &HCF 'right
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "right"
        context = "(e)" & context
        p = p - 1
      Case &HD0 'mid
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "mid"
        context = "(e)" & context
        p = p - 1
      Case &HD1 'shift
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "shift"
        context = "(e)" & context
        p = p - 1
      Case &HD2 'seq
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "seq"
        context = "(e)" & context
        p = p - 1
      Case &HD3 'list_>_mat
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "list" & Chr(18) & "mat"
        context = "(e)" & context
        p = p - 1
      Case &HD4 'subMat
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "subMat"
        context = "(e)" & context
        p = p - 1
      Case &HD5 '[] (subscript)
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        context = "i[e]" & context
        p = p - 1
      Case &HD6 'rand
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "rand"
        context = "(e)" & context
        p = p - 1
      Case &HD7 'min
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "min"
        context = "(e)" & context
        p = p - 1
      Case &HD8 'max
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "max"
        context = "(e)" & context
        p = p - 1
      Case &HD9 'type: list / matrix
        If Right(DeToken, 1) = "{" Then
          DeToken = Left(DeToken, Len(DeToken) - 1) & "["
          context = "[vE]]" & Mid(context, 3)
        ElseIf Left(context, 3) <> "[eE" Then
          context = "{e}" & context
        End If
        p = p - 1
      Case &HDA '() (function call)
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        context = "i(e)" & context
        p = p - 1
      Case &HDB 'type: internal matrix
        DeToken = DeToken & "(CANNOT READ THE DATA TYPE ""INTERNAL MATRIX""!)"
        Exit Function
      Case &HDC 'type: program / function
        If DeToken <> "" Then
          DeToken = DeToken & "(WARNING: DATA TYPE ""PROGRAM / FUNCTION"" DECLARED IN THE MIDDLE OF A PROGRAM!)"
        End If
        If Asc(Mid(S, p - 1, 1)) And 8 Then
          'Program / function not tokenized
          DeToken = DeToken & Left(S, p - 10)
          Exit Function
        Else
          p = p - 4
          context = "(e)" & Chr(13) & context
        End If
      Case &HDD 'type: data
        DeToken = DeToken & "(CANNOT READ THE DATA TYPE ""DATA""!)"
        Exit Function
      Case &HDE 'type: GDB
        DeToken = DeToken & "(CANNOT READ THE DATA TYPE ""GDB""!)"
        Exit Function
      Case &HDF 'type: picture
        DeToken = DeToken & "(CANNOT READ THE DATA TYPE ""PICTURE""!)"
        Exit Function
      Case &HE0 'type: text
        If DeToken <> "" Then
          DeToken = DeToken & "(WARNING: DATA TYPE ""TEXT"" DECLARED IN THE MIDDLE OF A PROGRAM!)"
        End If
        DeToken = DeToken & Mid(S, 3, p - 4)
        Exit Function
      Case &HE1 'type: figure
        DeToken = DeToken & "(CANNOT READ THE DATA TYPE ""FIGURE""!)"
        Exit Function
      Case &HE2 'type: macro
        DeToken = DeToken & "(CANNOT READ THE DATA TYPE ""MACRO""!)"
        Exit Function
      Case &HE3 'more functions
        Select Case Asc(Mid(S, p - 1, 1))
        Case &H1 '#
          DeToken = DeToken & "#"
          context = "i#" & context
        Case &H2 'getKey
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "getKey"
          context = "(e)" & context
        Case &H3 'getFold
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "getFold"
          context = "(e)" & context
        Case &H4 'switch
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "switch"
          context = "(e)" & context
        Case &H5 '_>_
          Temp = DeToken & Chr(0) & Temp
          DeToken = ""
          context = "iU" & Chr(18) & context
        Case &H6 'ord
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "ord"
          context = "()" & context
        Case &H7 'expr
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "expr"
          context = "()" & context
        Case &H8 'char
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "char"
          context = "()" & context
        Case &H9 'string
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "string"
          context = "()" & context
        Case &HA 'getType
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "getType"
          context = "()" & context
        Case &HB 'getMode
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "getMode"
          context = "()" & context
        Case &HC 'setFold
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "setFold"
          context = "()" & context
        Case &HD 'ptTest
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "ptTest"
          context = "(,)" & context
        Case &HE 'pxlTest
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "pxlTest"
          context = "(,)" & context
        Case &HF 'setGraph
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "setGraph"
          context = "(,)" & context
        Case &H10 'setTable
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "setTable"
          context = "(,)" & context
        Case &H11 'setMode
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "setMode"
          context = "(e)" & context
        Case &H12 'format
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "format"
          context = "(e)" & context
        Case &H13 'inString
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "inString"
          context = "(e)" & context
        Case &H14 '&
          If Len(context) > 0 And InStr("#12²3", Left(context, 1)) Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          Temp = DeToken & Chr(0) & Temp
          DeToken = ""
          context = "i4U4&" & context
        Case &H15 '_>_DD
          unit = "DD" & Chr(0) & unit
          context = "i" & Chr(18) & context
        Case &H16 '_>_DMS
          unit = "DMS" & Chr(0) & unit
          context = "i" & Chr(18) & context
        Case &H17 '_>_Rect
          unit = "Rect" & Chr(0) & unit
          context = "i" & Chr(18) & context
        Case &H18 '_>_Polar
          unit = "Polar" & Chr(0) & unit
          context = "i" & Chr(18) & context
        Case &H19 '_>_Cylind
          unit = "Cylind" & Chr(0) & unit
          context = "i" & Chr(18) & context
        Case &H1A '_>_Sphere
          unit = "Sphere" & Chr(0) & unit
          context = "i" & Chr(18) & context
        Case &H1B '(
          DeToken = DeToken & "("
        Case &H1C ')
          DeToken = DeToken & ")"
        Case &H1D '[
          DeToken = DeToken & "["
        Case &H1E ']
          DeToken = DeToken & "]"
        Case &H1F '{
          DeToken = DeToken & "{"
        Case &H20 '}
          DeToken = DeToken & "}"
        Case &H21 ',
          DeToken = DeToken & ","
        Case &H22 ';
          DeToken = DeToken & ";"
        Case &H23 '_angle_
          DeToken = DeToken & Chr(159)
        Case &H24 ''
          DeToken = DeToken & "'"
        Case &H25 '"
          DeToken = DeToken & """"
        Case &H26 '(_angle_)
          Temp = DeToken & Chr(0) & Temp
          DeToken = ""
          context = "(U" & Chr(159) & context
        Case &H27 'tmpCnv
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "tmpCnv"
          context = "(,)" & context
        Case &H28 '_DELTA_tmpCnv
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & Chr(132) & "tmpCnv"
          context = "(,)" & context
        Case &H29 'getUnits
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "getUnits"
          context = "(e)" & context
        Case &H2A 'setUnits
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "setUnits"
          context = "()" & context
        Case &H2B '0b
          DeToken = DeToken & "0b"
          context = "i" & context
          nBase = 1
        Case &H2C '0h
          DeToken = DeToken & "0h"
          context = "i" & context
          nBase = 2
        Case &H2D '_>_Bin
          unit = "Bin" & Chr(0) & unit
          context = "i" & Chr(18) & context
        Case &H2E '_>_Dec
          unit = "Dec" & Chr(0) & unit
          context = "i" & Chr(18) & context
        Case &H2F '_>_Hex
          unit = "Hex" & Chr(0) & unit
          context = "i" & Chr(18) & context
        Case &H30 'det
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "det"
          context = "(,)" & context
        Case &H31 'ref
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "ref"
          context = "(,)" & context
        Case &H32 'rref
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "rref"
          context = "(,)" & context
        Case &H33 'simult
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "simult"
          context = "(,,)" & context
        Case &H34 'getConfg
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "getConfg"
          context = "(e)" & context
        Case &H35 'augment
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "augment"
          context = "(;)" & context
          'AMS 2 functions:
        Case &H36 'mean
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "mean"
          context = "(,)" & context
        Case &H37 'product
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "product"
          context = "(e)" & context
        Case &H38 'stdDev
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "stdDev"
          context = "(,)" & context
        Case &H39 'sum
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "sum"
          context = "(e)" & context
        Case &H3A 'variance
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "variance"
          context = "(,)" & context
        Case &H3B '_DELTA_list
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & Chr(132) & "list"
          context = "()" & context
          'AMS 2.07 functions:
          '&H3C to $H45 are invalid!
        Case &H46 'isClkOn
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "isClkOn"
          context = "(e)" & context
        Case &H47 'getDate
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "getDate"
          context = "(e)" & context
        Case &H48 'getTime
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "getTime"
          context = "(e)" & context
        Case &H49 'getTmZn
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "getTmZn"
          context = "(e)" & context
        Case &H4A 'setDate
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "setDate"
          context = "(e)" & context
        Case &H4B 'setTime
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "setTime"
          context = "(e)" & context
        Case &H4C 'setTmZn
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "setTmZn"
          context = "()" & context
        Case &H4D 'dayOfWk
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "dayOfWk"
          context = "(e)" & context
        Case &H4E 'startTmr
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "startTmr"
          context = "(e)" & context
        Case &H4F 'checkTmr
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "checkTmr"
          context = "()" & context
        Case &H50 'timeCnv
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "timeCnv"
          context = "()" & context
        Case &H51 'getDtFmt
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "getDtFmt"
          context = "(e)" & context
        Case &H52 'getTmFmt
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "getTmFmt"
          context = "(e)" & context
        Case &H53 'getDtStr
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "getDtStr"
          context = "(e)" & context
        Case &H54 'getTmStr
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "getTmStr"
          context = "(e)" & context
        Case &H55 'setDtFmt
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "setDtFmt"
          context = "()" & context
        Case &H56 'setTmFmt
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "setTmFmt"
          context = "()" & context
        Case &H57 'root
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "root"
          context = "(,)" & context
        Case &H59 'impDif
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "impDif"
          context = "(e)" & context
        Case &H5B 'isVar
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "isVar"
          context = "()" & context
        Case &H5C 'isLocked
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "isLocked"
          context = "()" & context
        Case &H5D 'isArchiv
          If Left(context, 1) = "#" Then
            DeToken = DeToken & "("
            context = ")" & context
          End If
          DeToken = DeToken & "isArchiv"
          context = "()" & context
        Case &H5F '_>_Grad
          unit = "Grad" & Chr(0) & unit
          context = "i" & Chr(18) & context
        Case &H60 '_>_Rad
          unit = "Rad" & Chr(0) & unit
          context = "i" & Chr(18) & context
          'END OF AMS 2.07 FUNCTIONS
          'END OF AMS2 FUNCTIONS
        Case Else
          DeToken = DeToken & "(INVALID FUNCTION TOKEN: &h" & Hex(Asc(Mid(S, p - 1, 1))) & ")"
      End Select
      p = p - 2
      Case &HE4 'instructions
        Select Case Asc(Mid(S, p - 1, 1))
        Case &H1 'ClrDraw
          DeToken = DeToken & "ClrDraw"
        Case &H2 'ClrGraph
          DeToken = DeToken & "ClrGraph"
        Case &H3 'ClrHome
          DeToken = DeToken & "ClrHome"
        Case &H4 'ClrIO
          DeToken = DeToken & "ClrIO"
        Case &H5 'ClrTable
          DeToken = DeToken & "ClrTable"
        Case &H6 'Custom
          DeToken = DeToken & "Custom"
        Case &H7 'Cycle
          DeToken = DeToken & "Cycle"
          p = p - 2 'displacement
        Case &H8 'Dialog
          DeToken = DeToken & "Dialog"
        Case &H9 'DispG
          DeToken = DeToken & "DispG"
        Case &HA 'DispTbl
          DeToken = DeToken & "DispTbl"
        Case &HB 'Else (If)
          DeToken = DeToken & "Else"
        Case &HC 'EndCustm
          DeToken = DeToken & "EndCustm"
        Case &HD 'EndDlog
          DeToken = DeToken & "EndDlog"
        Case &HE 'EndFor
          DeToken = DeToken & "EndFor"
          p = p - 2 'displacement
        Case &HF 'EndFunc
          DeToken = DeToken & "EndFunc"
        Case &H10 'EndIf
          DeToken = DeToken & "EndIf"
        Case &H11 'EndLoop
          DeToken = DeToken & "EndLoop"
          p = p - 2 'displacement
        Case &H12 'EndPrgm
          DeToken = DeToken & "EndPrgm"
        Case &H13 'EndTBar
          DeToken = DeToken & "EndTBar"
        Case &H14 'EndTry
          DeToken = DeToken & "EndTry"
        Case &H15 'EndWhile
          DeToken = DeToken & "EndWhile"
          p = p - 2 'displacement
        Case &H16 'Exit
          DeToken = DeToken & "Exit"
          p = p - 2 'displacement
        Case &H17 'Func
          DeToken = DeToken & "Func"
        Case &H18 'Loop
          DeToken = DeToken & "Loop"
        Case &H19 'Prgm
          DeToken = DeToken & "Prgm"
        Case &H1A 'ShowStat
          DeToken = DeToken & "ShowStat"
        Case &H1B 'Stop
          DeToken = DeToken & "Stop"
        Case &H1C 'Then
          DeToken = DeToken & "Then"
        Case &H1D 'Toolbar
          DeToken = DeToken & "Toolbar"
        Case &H1E 'Trace
          DeToken = DeToken & "Trace"
        Case &H1F 'Try
          DeToken = DeToken & "Try"
        Case &H20 'ZoomBox
          DeToken = DeToken & "ZoomBox"
        Case &H21 'ZoomData
          DeToken = DeToken & "ZoomData"
        Case &H22 'ZoomDec
          DeToken = DeToken & "ZoomDec"
        Case &H23 'ZoomFit
          DeToken = DeToken & "ZoomFit"
        Case &H24 'ZoomIn
          DeToken = DeToken & "ZoomIn"
        Case &H25 'ZoomInt
          DeToken = DeToken & "ZoomInt"
        Case &H26 'ZoomOut
          DeToken = DeToken & "ZoomOut"
        Case &H27 'ZoomPrev
          DeToken = DeToken & "ZoomPrev"
        Case &H28 'ZoomRcl
          DeToken = DeToken & "ZoomRcl"
        Case &H29 'ZoomSqr
          DeToken = DeToken & "ZoomSqr"
        Case &H2A 'ZoomStd
          DeToken = DeToken & "ZoomStd"
        Case &H2B 'ZoomSto
          DeToken = DeToken & "ZoomSto"
        Case &H2C 'ZoomTrig
          DeToken = DeToken & "ZoomTrig"
        Case &H2D 'DrawFunc
          DeToken = DeToken & "DrawFunc "
        Case &H2E 'DrawInv
          DeToken = DeToken & "DrawInv "
        Case &H2F 'Goto
          DeToken = DeToken & "Goto "
        Case &H30 'Lbl
          DeToken = DeToken & "Lbl "
        Case &H31 'Get
          DeToken = DeToken & "Get "
        Case &H32 'Send
          DeToken = DeToken & "Send "
        Case &H33 'GetCalc
          DeToken = DeToken & "GetCalc "
        Case &H34 'SendCalc
          DeToken = DeToken & "SendCalc "
        Case &H35 'NewFold
          DeToken = DeToken & "NewFold "
        Case &H36 'PrintObj
          DeToken = DeToken & "PrintObj "
        Case &H37 'RclGDB
          DeToken = DeToken & "RclGDB "
        Case &H38 'StoGDB
          DeToken = DeToken & "StoGDB "
        Case &H39 'ElseIf
          DeToken = DeToken & "ElseIf"
          context = " T" & context
        Case &H3A 'If
          DeToken = DeToken & "If "
        Case &H3B 'If Then
          DeToken = DeToken & "If"
          context = " T" & context
        Case &H3C 'RandSeed
          DeToken = DeToken & "RandSeed "
        Case &H3D 'While
          DeToken = DeToken & "While "
        Case &H3E 'LineTan
          DeToken = DeToken & "LineTan"
          context = " ," & context
        Case &H3F 'CopyVar
          DeToken = DeToken & "CopyVar"
          context = " ," & context
        Case &H40 'Rename
          DeToken = DeToken & "Rename"
          context = " ," & context
        Case &H41 'Style
          DeToken = DeToken & "Style"
          context = " ," & context
        Case &H42 'Fill
          DeToken = DeToken & "Fill"
          context = " ," & context
        Case &H43 'Request
          DeToken = DeToken & "Request"
          context = " ," & context
        Case &H44 'PopUp
          DeToken = DeToken & "PopUp"
          context = " ," & context
        Case &H45 'PtChg
          DeToken = DeToken & "PtChg"
          context = " ," & context
        Case &H46 'PtOff
          DeToken = DeToken & "PtOff"
          context = " ," & context
        Case &H47 'PtOn
          DeToken = DeToken & "PtOn"
          context = " ," & context
        Case &H48 'PxlChg
          DeToken = DeToken & "PxlChg"
          context = " ," & context
        Case &H49 'PxlOff
          DeToken = DeToken & "PxlOff"
          context = " ," & context
        Case &H4A 'PxlOn
          DeToken = DeToken & "PxlOn"
          context = " ," & context
        Case &H4B 'MoveVar
          DeToken = DeToken & "MoveVar"
          context = " ,," & context
        Case &H4C 'DropDown
          DeToken = DeToken & "DropDown"
          context = " ,," & context
        Case &H4D 'Output
          DeToken = DeToken & "Output"
          context = " ,," & context
        Case &H4E 'PtText
          DeToken = DeToken & "PtText"
          context = " ,," & context
        Case &H4F 'PxlText
          DeToken = DeToken & "PxlText"
          context = " ,," & context
        Case &H50 'DrawSlp
          DeToken = DeToken & "DrawSlp"
          context = " ,," & context
        Case &H51 'Pause
          DeToken = DeToken & "Pause"
          context = " e" & context
        Case &H52 'Return
          DeToken = DeToken & "Return"
          context = " e" & context
        Case &H53 'Input
          DeToken = DeToken & "Input"
          context = " e" & context
        Case &H54 'PlotsOff
          DeToken = DeToken & "PlotsOff"
          context = " e" & context
        Case &H55 'PlotsOn
          DeToken = DeToken & "PlotsOn"
          context = " e" & context
        Case &H56 'Title
          DeToken = DeToken & "Title"
          context = " e" & context
        Case &H57 'Item
          DeToken = DeToken & "Item"
          context = " e" & context
        Case &H58 'InputStr
          DeToken = DeToken & "InputStr"
          context = " e" & context
        Case &H59 'LineHorz
          DeToken = DeToken & "LineHorz"
          context = " e" & context
        Case &H5A 'LineVert
          DeToken = DeToken & "LineVert"
          context = " e" & context
        Case &H5B 'PxlHorz
          DeToken = DeToken & "PxlHorz"
          context = " e" & context
        Case &H5C 'PxlVert
          DeToken = DeToken & "PxlVert"
          context = " e" & context
        Case &H5D 'AndPic
          DeToken = DeToken & "AndPic"
          context = " e" & context
        Case &H5E 'RclPic
          DeToken = DeToken & "RclPic"
          context = " e" & context
        Case &H5F 'RplcPic
          DeToken = DeToken & "RplcPic"
          context = " e" & context
        Case &H60 'XorPic
          DeToken = DeToken & "XorPic"
          context = " e" & context
        Case &H61 'DrawPol
          DeToken = DeToken & "DrawPol"
          context = " e" & context
        Case &H62 'Text
          DeToken = DeToken & "Text"
          context = " e" & context
        Case &H63 'OneVar
          DeToken = DeToken & "OneVar"
          context = " e" & context
        Case &H64 'StoPic
          DeToken = DeToken & "StoPic"
          context = " e" & context
        Case &H65 'Graph
          DeToken = DeToken & "Graph"
          context = " e" & context
        Case &H66 'Table
          DeToken = DeToken & "Table"
          context = " e" & context
        Case &H67 'NewPic
          DeToken = DeToken & "NewPic"
          context = " e" & context
        Case &H68 'DrawParm
          DeToken = DeToken & "DrawParm"
          context = " e" & context
        Case &H69 'CyclePic
          DeToken = DeToken & "CyclePic"
          context = " e" & context
        Case &H6A 'CubicReg
          DeToken = DeToken & "CubicReg"
          context = " e" & context
        Case &H6B 'ExpReg
          DeToken = DeToken & "ExpReg"
          context = " e" & context
        Case &H6C 'LinReg
          DeToken = DeToken & "LinReg"
          context = " e" & context
        Case &H6D 'LnReg
          DeToken = DeToken & "LnReg"
          context = " e" & context
        Case &H6E 'MedMed
          DeToken = DeToken & "MedMed"
          context = " e" & context
        Case &H6F 'PowerReg
          DeToken = DeToken & "PowerReg"
          context = " e" & context
        Case &H70 'QuadReg
          DeToken = DeToken & "QuadReg"
          context = " e" & context
        Case &H71 'QuartReg
          DeToken = DeToken & "QuartReg"
          context = " e" & context
        Case &H72 'TwoVar
          DeToken = DeToken & "TwoVar"
          context = " e" & context
        Case &H73 'Shade
          DeToken = DeToken & "Shade"
          context = " e" & context
        Case &H74 'For
          DeToken = DeToken & "For"
          context = " e" & context
        Case &H75 'Circle
          DeToken = DeToken & "Circle"
          context = " e" & context
        Case &H76 'PxlCrcl
          DeToken = DeToken & "PxlCrcl"
          context = " e" & context
        Case &H77 'NewPlot
          DeToken = DeToken & "NewPlot"
          context = " e" & context
        Case &H78 'Line
          DeToken = DeToken & "Line"
          context = " e" & context
        Case &H79 'PxlLine
          DeToken = DeToken & "PxlLine"
          context = " e" & context
        Case &H7A 'Disp
          DeToken = DeToken & "Disp"
          context = " e" & context
        Case &H7B 'FnOff
          DeToken = DeToken & "FnOff"
          context = " e" & context
        Case &H7C 'FnOn
          DeToken = DeToken & "FnOn"
          context = " e" & context
        Case &H7D 'Local
          DeToken = DeToken & "Local"
          context = " e" & context
        Case &H7E 'DelFold
          DeToken = DeToken & "DelFold"
          context = " e" & context
        Case &H7F 'DelVar
          DeToken = DeToken & "DelVar"
          context = " e" & context
        Case &H80 'Lock
          DeToken = DeToken & "Lock"
          context = " e" & context
        Case &H81 'Prompt
          DeToken = DeToken & "Prompt"
          context = " e" & context
        Case &H82 'SortA
          DeToken = DeToken & "SortA"
          context = " e" & context
        Case &H83 'SortD
          DeToken = DeToken & "SortD"
          context = " e" & context
        Case &H84 'UnLock
          DeToken = DeToken & "UnLock"
          context = " e" & context
        Case &H85 'NewData
          DeToken = DeToken & "NewData"
          context = " e" & context
        Case &H86 'Define
          DeToken = DeToken & "Define"
          context = " =" & context
        Case &H87 'Else (Try)
          DeToken = DeToken & "Else"
        Case &H88 'ClrErr
          DeToken = DeToken & "ClrErr"
        Case &H89 'PassErr
          DeToken = DeToken & "PassErr"
        Case &H8A 'DispHome
          DeToken = DeToken & "DispHome"
        Case &H8B 'Exec
          DeToken = DeToken & "Exec"
          context = " e" & context
        Case &H8C 'Archive
          DeToken = DeToken & "Archive"
          context = " e" & context
        Case &H8D 'Unarchiv
          DeToken = DeToken & "Unarchiv"
          context = " e" & context
        Case &H8E 'LU
          DeToken = DeToken & "LU"
          context = " e" & context
        Case &H8F 'QR
          DeToken = DeToken & "QR"
          context = " e" & context
        Case &H90 'BldData
          DeToken = DeToken & "BldData "
        Case &H91 'DrwCtour
          DeToken = DeToken & "DrwCtour "
        Case &H92 'NewProb
          DeToken = DeToken & "NewProb"
        Case &H93 'SinReg
          DeToken = DeToken & "SinReg"
          context = " e" & context
        Case &H94 'Logistic
          DeToken = DeToken & "Logistic"
          context = " e" & context
        Case &H95 'CustmOn
          DeToken = DeToken & "CustmOn"
        Case &H96 'CustmOff
          DeToken = DeToken & "CustmOff"
        Case &H97 'SendChat
          DeToken = DeToken & "SendChat "
          '&H98 is invalid!
        Case &H99 'Request (AMS 2.07)
          DeToken = DeToken & "Request"
          context = " e" & context
        Case &H9A 'ClockOn (AMS 2.07)
          DeToken = DeToken & "ClockOn"
        Case &H9B 'ClockOff (AMS 2.07)
          DeToken = DeToken & "ClockOff"
        Case Else
          DeToken = DeToken & "(INVALID INSTRUCTION TOKEN: &h" & Hex(Asc(Mid(S, p - 1, 1))) & ")"
      End Select
      p = p - 2
      Case &HE5 'END_TAG
        If Left(context, 1) = "e" Or Left(context, 1) = "v" Then
          If Right(DeToken, 1) = " " Or Right(DeToken, 1) = "," Then DeToken = Left(DeToken, Len(DeToken) - 1)
          context = Mid(context, 2)
        ElseIf Left(context, 4) = "[eE]" Then
          If Right(DeToken, 1) = " " Or Right(DeToken, 1) = "," Then DeToken = Left(DeToken, Len(DeToken) - 1)
          context = Mid(context, 5)
        ElseIf Left(context, 1) = """" Then
          context = Mid(context, 2)
        ElseIf Left(context, 2) = "'""" Then
          context = Mid(context, 3)
        'Else
         ' DeToken = DeToken & "(UNEXPECTED END_TAG)"
        End If
        p = p - 1
      Case &HE6 '_(C)_
        S2 = ""
        For i = p - 3 To 1 Step -1
          If Mid(S, i, 1) = Chr(0) Then Exit For
          S2 = Mid(S, i, 1) & S2
        Next
        p = p - 1
        DeToken = DeToken & String(Asc(Mid(S, p, 1)), " ") & Chr(169) & S2
        p = i - 1
      Case &HE7 ':
        p = p - 1
        DeToken = DeToken & ":" & String(Asc(Mid(S, p, 1)), " ")
        p = p - 1
      Case &HE8 '_ENTER_
        p = p - 1
        DeToken = DeToken & Chr(13) & String(Asc(Mid(S, p, 1)), " ")
        p = p - 1
      Case &HE9 'end of estack
        p = p - 1
      Case &HEA '_+/-_ (unary)
        If Len(context) > 0 And InStr("#12²", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        context = Chr(177) & "3" & context
        p = p - 1
      Case &HEB '_+/-_ (binary)
        If Len(context) > 0 And InStr("#12²345V6", Left(context, 1)) Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        Temp = DeToken & Chr(0) & Temp
        DeToken = ""
        context = "i6UI" & Chr(177) & context
        p = p - 1
        '&HEC is internal!
      Case &HED 'eigVc
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "eigVc"
        context = "()" & context
        p = p - 1
      Case &HEE 'eigVl
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "eigVl"
        context = "()" & context
        p = p - 1
      Case &HEF ''
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        context = "i1p" & context
        p = p - 1
      Case &HF0 'convert local
        context = "i" & context
        p = p - 1
      Case &HF1 'deSolve
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "deSolve"
        context = "(e)" & context
        p = p - 1
      Case &HF2 ''() (function' call)
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        context = "ip(e)" & context
        p = p - 2
      Case &HF3 'type: asm
        DeToken = DeToken & "(CANNOT READ THE DATA TYPE ""ASM""!)"
        Exit Function
      Case &HF4 'isPrime
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "isPrime"
        context = "()" & context
        p = p - 1
        '&HF5-&HF7 are internal!
      Case &HF8 'type: other
        DeToken = DeToken & "(CANNOT READ THE DATA TYPE ""UNKNOWN""!)"
        Exit Function
      Case &HF9 'rotate
        If Left(context, 1) = "#" Then
          DeToken = DeToken & "("
          context = ")" & context
        End If
        DeToken = DeToken & "rotate"
        context = "(e)" & context
        p = p - 1
      Case Else
        DeToken = DeToken & "(INVALID TOKEN: &h" & Hex(T) & ")"
        p = p - 1
    End Select
    While Len(context) > 0 And InStr("p.12²345V6I7S89XaAbBcC)#""!%]}+-*/&" & Chr(18) & Chr(22) & Chr(152) & Chr(153) & Chr(159), Left(context, 1))
      Select Case Left(context, 1)
        Case "."
          DeToken = DeToken & " ."
          context = Mid(context, 2)
          DeToken = DeToken & Left(context, 1) & " " & Left(unit, InStr(unit, Chr(0)) - 1)
          unit = Mid(unit, InStr(unit, Chr(0)) + 1)
        Case ")", """", "!", "%", "]", "}", Chr(152), Chr(153)
          DeToken = DeToken & Left(context, 1)
        Case "p"
          DeToken = DeToken & "'"
        Case Chr(18)
          DeToken = DeToken & " " & Left(context, 1) & Left(unit, InStr(unit, Chr(0)) - 1)
          unit = Mid(unit, InStr(unit, Chr(0)) + 1)
        Case "+", "-", "*", "/", "&", Chr(22)
          DeToken = DeToken & Left(context, 1) & Left(unit, InStr(unit, Chr(0)) - 1)
          unit = Mid(unit, InStr(unit, Chr(0)) + 1)
        Case Chr(159)
          DeToken = DeToken & Chr(159) & Left(unit, InStr(unit, Chr(0)) - 1) & ")"
          unit = Mid(unit, InStr(unit, Chr(0)) + 1)
      End Select
      context = Mid(context, 2)
    Wend
    If Len(context) > 0 Then
      Select Case Left(context, 1)
        Case "e"
           DeToken = DeToken & ","
        Case "v"
          DeToken = DeToken & ","
          If polar And 1 Then
            DeToken = DeToken & Chr(159)
          End If
          polar = polar \ 2
        Case "E"
          DeToken = DeToken & "]"
          context = "[e" & context
        Case "i"
          context = Mid(context, 2)
        Case "d"
          DeToken = DeToken & " and "
          context = Mid(context, 2)
        Case "o"
          DeToken = DeToken & " or "
          context = Mid(context, 2)
        Case "x"
          DeToken = DeToken & " xor "
          context = Mid(context, 2)
        Case "l"
          DeToken = DeToken & ")/ln("
          context = Mid(context, 2)
        Case "U"
          unit = DeToken & Chr(0) & unit
          DeToken = Left(Temp, InStr(Temp, Chr(0)) - 1)
          Temp = Mid(Temp, InStr(Temp, Chr(0)) + 1)
          context = Mid(context, 2)
        Case "T"
          DeToken = DeToken & " Then "
          context = Mid(context, 2)
        Case Else
          DeToken = DeToken & Left(context, 1)
          context = Mid(context, 2)
      End Select
    End If
  Wend
End Function

Public Function OpenTIGLAscii$(ByVal FN$)
Attribute OpenTIGLAscii.VB_Description = "Takes the file name of a program or function in TI-GraphLink 7-bit-ASCII interchange format. Returns the data in readable form. Converts the 7-bit escape sequences to their equivalent 8-bit characters. The data returned uses CR-LF line endings."
  Dim S$, E&
  Open FN For Input As #1
  S = Input(LOF(1), #1)
  If InStr(S, "\START92\" & vbCrLf) = 0 Then GoTo Invalid
  E = InStr(S, vbCrLf & "\STOP92\" & vbCrLf)
  If E = 0 Then GoTo Invalid
  Seek #1, InStr(S, "\START92\" & vbCrLf)
  Line Input #1, S
  If S <> "\START92\" Then GoTo Invalid
  Line Input #1, S
  If Left(S, 9) <> "\COMMENT=" Then GoTo Invalid
  Line Input #1, S
  If Left(S, 6) <> "\NAME=" Then GoTo Invalid
  Line Input #1, S
  If Left(S, 6) <> "\FILE=" Then GoTo Invalid
  S = Input(E - Seek(1), #1)
  Close #1
  
  S = Replace(S, "\alpha\", Chr(128))
  S = Replace(S, "\beta\", Chr(129))
  S = Replace(S, "\Gamma\", Chr(130))
  S = Replace(S, "\gamma\", Chr(131))
  S = Replace(S, "\Delta\", Chr(132))
  S = Replace(S, "\delta\", Chr(133))
  S = Replace(S, "\epsilon\", Chr(134))
  S = Replace(S, "\zeta\", Chr(135))
  S = Replace(S, "\theta\", Chr(136))
  S = Replace(S, "\lambda\", Chr(137))
  S = Replace(S, "\xi\", Chr(138))
  S = Replace(S, "\Pi\", Chr(139))
  S = Replace(S, "\pi\", Chr(140))
  S = Replace(S, "\mu\", Chr(181))
  S = Replace(S, "\rho\", Chr(141))
  S = Replace(S, "\Sigma\", Chr(142))
  S = Replace(S, "\sigma\", Chr(143))
  S = Replace(S, "\tau\", Chr(144))
  S = Replace(S, "\phi\", Chr(145))
  S = Replace(S, "\psi\", Chr(146))
  S = Replace(S, "\Omega\", Chr(147))
  S = Replace(S, "\omega\", Chr(148))
  S = Replace(S, "\->\", Chr(22))
  S = Replace(S, "\option\", Chr(127))
  S = Replace(S, "\union\", Chr(28))
  S = Replace(S, "\intersect\", Chr(29))
  S = Replace(S, "\subset\", Chr(30))
  S = Replace(S, "\element\", Chr(31))
  S = Replace(S, "\ee\", Chr(149))
  S = Replace(S, "\e\", Chr(150))
  S = Replace(S, "\i\", Chr(151))
  S = Replace(S, "\r\", Chr(152))
  S = Replace(S, "\t\", Chr(153))
  S = Replace(S, "\xmean\", Chr(154))
  S = Replace(S, "\ymean\", Chr(155))
  S = Replace(S, "\<=\", Chr(156))
  S = Replace(S, "\!=\", Chr(157))
  S = Replace(S, "\>=\", Chr(158))
  S = Replace(S, "\/_\", Chr(159))
  S = Replace(S, "\diff\", Chr(188))
  S = Replace(S, "\integral\", Chr(189))
  S = Replace(S, "\infinity\", Chr(190))
  S = Replace(S, "\root\", Chr(168))
  S = Replace(S, "\(C)\", Chr(169))
  S = Replace(S, "\(-)\", Chr(173))
  S = Replace(S, "\o\", "°")
  S = Replace(S, "\lock\", Chr(14))
  S = Replace(S, "\check\", Chr(15))
  S = Replace(S, "\block\", Chr(16))
  S = Replace(S, "\from\", Chr(17))
  S = Replace(S, "\to\", Chr(18))
  S = Replace(S, "\up\", Chr(19))
  S = Replace(S, "\down\", Chr(20))
  S = Replace(S, "\leftarrow\", Chr(21))
  S = Replace(S, "\uparrow\", Chr(23))
  S = Replace(S, "\downarrow\", Chr(24))
  S = Replace(S, "\left\", Chr(25))
  S = Replace(S, "\right\", Chr(26))
  S = Replace(S, "\shift\", Chr(27))
  S = Replace(S, "\...\", Chr(160))
  S = Replace(S, "\cent\", "¢")
  S = Replace(S, "\pound\", "£")
  S = Replace(S, "\starbust\", "¤")
  S = Replace(S, "\yen\", "¥")
  S = Replace(S, "\split\", "¦")
  S = Replace(S, "\section\", "§")
  S = Replace(S, "\a_\", "ª")
  S = Replace(S, "\<<\", "«")
  S = Replace(S, "\lnot\", "~")
  S = Replace(S, "\(R)\", "®")
  S = Replace(S, "\^-\", Chr(175))
  S = Replace(S, "\^+\", "±")
  S = Replace(S, "\^2\", "²")
  S = Replace(S, "\^3\", "³")
  S = Replace(S, "\^-1\", Chr(180))
  S = Replace(S, "\para\", "¶")
  S = Replace(S, "\.\", "·")
  S = Replace(S, "\^x\", Chr(184))
  S = Replace(S, "\^1\", "¹")
  S = Replace(S, "\o_\", "º")
  S = Replace(S, "\>>\", "»")
  S = Replace(S, "\ud!\", "¡")
  S = Replace(S, "\ud?\", "¿")
  S = Replace(S, "\A`\", "À")
  S = Replace(S, "\A'\", "Á")
  S = Replace(S, "\A^\", "Â")
  S = Replace(S, "\A~\", "Ã")
  S = Replace(S, "\A..\", "Ä")
  S = Replace(S, "\Ao\", "Å")
  S = Replace(S, "\AE\", "Æ")
  S = Replace(S, "\C,\", "Ç")
  S = Replace(S, "\E`\", "È")
  S = Replace(S, "\E'\", "É")
  S = Replace(S, "\E^\", "Ê")
  S = Replace(S, "\E..\", "Ë")
  S = Replace(S, "\I`\", "Ì")
  S = Replace(S, "\I'\", "Í")
  S = Replace(S, "\I^\", "Î")
  S = Replace(S, "\I..\", "Ï")
  S = Replace(S, "\-D\", "Ð")
  S = Replace(S, "\N~\", "Ñ")
  S = Replace(S, "\O`\", "Ò")
  S = Replace(S, "\O'\", "Ó")
  S = Replace(S, "\O^\", "Ô")
  S = Replace(S, "\O~\", "Õ")
  S = Replace(S, "\O..\", "Ö")
  S = Replace(S, "\x\", "×")
  S = Replace(S, "\O/\", "Ø")
  S = Replace(S, "\U`\", "Ù")
  S = Replace(S, "\U'\", "Ú")
  S = Replace(S, "\U^\", "Û")
  S = Replace(S, "\U..\", "Ü")
  S = Replace(S, "\Y'\", "Ý")
  S = Replace(S, "\I>\", "Þ")
  S = Replace(S, "\ss\", "ß")
  S = Replace(S, "\a`\", "à")
  S = Replace(S, "\a'\", "á")
  S = Replace(S, "\a^\", "â")
  S = Replace(S, "\a~\", "ã")
  S = Replace(S, "\a..\", "ä")
  S = Replace(S, "\ao\", "å")
  S = Replace(S, "\ae\", "æ")
  S = Replace(S, "\c,\", "ç")
  S = Replace(S, "\e`\", "è")
  S = Replace(S, "\e'\", "é")
  S = Replace(S, "\e^\", "ê")
  S = Replace(S, "\e..\", "ë")
  S = Replace(S, "\i`\", "ì")
  S = Replace(S, "\i'\", "í")
  S = Replace(S, "\i^\", "î")
  S = Replace(S, "\i..\", "ï")
  S = Replace(S, "\-d\", "ð")
  S = Replace(S, "\n~\", "ñ")
  S = Replace(S, "\o`\", "ò")
  S = Replace(S, "\o'\", "ó")
  S = Replace(S, "\o^\", "ô")
  S = Replace(S, "\o~\", "õ")
  S = Replace(S, "\o..\", "ö")
  S = Replace(S, "\/\", "÷")
  S = Replace(S, "\o/\", "ø")
  S = Replace(S, "\u`\", "ù")
  S = Replace(S, "\u'\", "ú")
  S = Replace(S, "\u^\", "û")
  S = Replace(S, "\u..\", "ü")
  S = Replace(S, "\y'\", "ý")
  S = Replace(S, "\i>\", "þ")
  S = Replace(S, "\y..\", "ÿ")
  
  OpenTIGLAscii = S
  Close #1
  Exit Function
  
Invalid:
  OpenTIGLAscii = "(ERROR: Invalid ASCII program / function / text file!)"
  Close #1
End Function

Public Function GetTIGLAsciiName$(ByVal FN$)
Attribute GetTIGLAsciiName.VB_Description = "Takes the file name of a variable in TI-GraphLink 7-bit-ASCII interchange format. Returns its on-calc file name."
  Open FN For Binary As #1
  S = Input(LOF(1), #1)
  If InStr(S, "\START92\" & vbCrLf) <> 0 Then
    Seek #1, InStr(S, "\START92\" & vbCrLf)
    Line Input #1, S 'START92
    Line Input #1, S 'COMMENT
    Line Input #1, S 'NAME=
    If Left(S, 6) = "\NAME=" Then
      GetTIGLAsciiName = Mid(S, 7)
    Else
      GetTIGLAsciiName = "noname"
    End If
  End If
  Close #1
End Function

Public Sub SaveTIGLAscii(ByVal FN$, ByVal TIName$, ByVal S$)
Attribute SaveTIGLAscii.VB_Description = "Takes a file name FN, an on-calc file name TIName and a string of a program or function in readable form. Saves the program or function to the TI-GraphLink 7-bit-ASCII interchange format. The expected line ending is CR-LF."
  Dim L2$, Ext$
  L2 = Mid(S, InStr(S, Chr(13)) + 1)
  If InStr(L2, Chr(13)) <> 0 Then
    L2 = Left(L2, InStr(L2, Chr(13)) - 1)
  End If
  If Left(L2, 1) = Chr(10) Then L2 = Mid(L2, 2)
  'If L2 = "Prgm" Then
  '  Ext = ".89P"
  'Else
  '  Ext = ".89F"
  'End If
  
  S = Replace(S, Chr(128), "\alpha\")
  S = Replace(S, Chr(129), "\beta\")
  S = Replace(S, Chr(130), "\Gamma\")
  S = Replace(S, Chr(131), "\gamma\")
  S = Replace(S, Chr(132), "\Delta\")
  S = Replace(S, Chr(133), "\delta\")
  S = Replace(S, Chr(134), "\epsilon\")
  S = Replace(S, Chr(135), "\zeta\")
  S = Replace(S, Chr(136), "\theta\")
  S = Replace(S, Chr(137), "\lambda\")
  S = Replace(S, Chr(138), "\xi\")
  S = Replace(S, Chr(139), "\Pi\")
  S = Replace(S, Chr(140), "\pi\")
  S = Replace(S, Chr(181), "\mu\")
  S = Replace(S, Chr(141), "\rho\")
  S = Replace(S, Chr(142), "\Sigma\")
  S = Replace(S, Chr(143), "\sigma\")
  S = Replace(S, Chr(144), "\tau\")
  S = Replace(S, Chr(145), "\phi\")
  S = Replace(S, Chr(146), "\psi\")
  S = Replace(S, Chr(147), "\Omega\")
  S = Replace(S, Chr(148), "\omega\")
  S = Replace(S, Chr(22), "\->\")
  S = Replace(S, Chr(127), "\option\")
  S = Replace(S, Chr(28), "\union\")
  S = Replace(S, Chr(29), "\intersect\")
  S = Replace(S, Chr(30), "\subset\")
  S = Replace(S, Chr(31), "\element\")
  S = Replace(S, Chr(149), "\ee\")
  S = Replace(S, Chr(150), "\e\")
  S = Replace(S, Chr(151), "\i\")
  S = Replace(S, Chr(152), "\r\")
  S = Replace(S, Chr(153), "\t\")
  S = Replace(S, Chr(154), "\xmean\")
  S = Replace(S, Chr(155), "\ymean\")
  S = Replace(S, Chr(156), "\<=\")
  S = Replace(S, Chr(157), "\!=\")
  S = Replace(S, Chr(158), "\>=\")
  S = Replace(S, Chr(159), "\/_\")
  S = Replace(S, Chr(188), "\diff\")
  S = Replace(S, Chr(189), "\integral\")
  S = Replace(S, Chr(190), "\infinity\")
  S = Replace(S, Chr(168), "\root\")
  S = Replace(S, Chr(169), "\(C)\")
  S = Replace(S, Chr(173), "\(-)\")
  S = Replace(S, "°", "\o\")
  S = Replace(S, Chr(14), "\lock\")
  S = Replace(S, Chr(15), "\check\")
  S = Replace(S, Chr(16), "\block\")
  S = Replace(S, Chr(17), "\from\")
  S = Replace(S, Chr(18), "\to\")
  S = Replace(S, Chr(19), "\up\")
  S = Replace(S, Chr(20), "\down\")
  S = Replace(S, Chr(21), "\leftarrow\")
  S = Replace(S, Chr(23), "\uparrow\")
  S = Replace(S, Chr(24), "\downarrow\")
  S = Replace(S, Chr(25), "\left\")
  S = Replace(S, Chr(26), "\right\")
  S = Replace(S, Chr(27), "\shift\")
  S = Replace(S, Chr(160), "\...\")
  S = Replace(S, "¢", "\cent\")
  S = Replace(S, "£", "\pound\")
  S = Replace(S, "¤", "\starbust\")
  S = Replace(S, "¥", "\yen\")
  S = Replace(S, "¦", "\split\")
  S = Replace(S, "§", "\section\")
  S = Replace(S, "ª", "\a_\")
  S = Replace(S, "«", "\<<\")
  S = Replace(S, "~", "\lnot\")
  S = Replace(S, "®", "\(R)\")
  S = Replace(S, Chr(175), "\^-\")
  S = Replace(S, "±", "\^+\")
  S = Replace(S, "²", "\^2\")
  S = Replace(S, "³", "\^3\")
  S = Replace(S, Chr(180), "\^-1\")
  S = Replace(S, "¶", "\para\")
  S = Replace(S, "·", "\.\")
  S = Replace(S, Chr(184), "\^x\")
  S = Replace(S, "¹", "\^1\")
  S = Replace(S, "º", "\o_\")
  S = Replace(S, "»", "\>>\")
  S = Replace(S, "¡", "\ud!\")
  S = Replace(S, "¿", "\ud?\")
  S = Replace(S, "À", "\A`\")
  S = Replace(S, "Á", "\A'\")
  S = Replace(S, "Â", "\A^\")
  S = Replace(S, "Ã", "\A~\")
  S = Replace(S, "Ä", "\A..\")
  S = Replace(S, "Å", "\Ao\")
  S = Replace(S, "Æ", "\AE\")
  S = Replace(S, "Ç", "\C,\")
  S = Replace(S, "È", "\E`\")
  S = Replace(S, "É", "\E'\")
  S = Replace(S, "Ê", "\E^\")
  S = Replace(S, "Ë", "\E..\")
  S = Replace(S, "Ì", "\I`\")
  S = Replace(S, "Í", "\I'\")
  S = Replace(S, "Î", "\I^\")
  S = Replace(S, "Ï", "\I..\")
  S = Replace(S, "Ð", "\-D\")
  S = Replace(S, "Ñ", "\N~\")
  S = Replace(S, "Ò", "\O`\")
  S = Replace(S, "Ó", "\O'\")
  S = Replace(S, "Ô", "\O^\")
  S = Replace(S, "Õ", "\O~\")
  S = Replace(S, "Ö", "\O..\")
  S = Replace(S, "×", "\x\")
  S = Replace(S, "Ø", "\O/\")
  S = Replace(S, "Ù", "\U`\")
  S = Replace(S, "Ú", "\U'\")
  S = Replace(S, "Û", "\U^\")
  S = Replace(S, "Ü", "\U..\")
  S = Replace(S, "Ý", "\Y'\")
  S = Replace(S, "Þ", "\I>\")
  S = Replace(S, "ß", "\ss\")
  S = Replace(S, "à", "\a`\")
  S = Replace(S, "á", "\a'\")
  S = Replace(S, "â", "\a^\")
  S = Replace(S, "ã", "\a~\")
  S = Replace(S, "ä", "\a..\")
  S = Replace(S, "å", "\ao\")
  S = Replace(S, "æ", "\ae\")
  S = Replace(S, "ç", "\c,\")
  S = Replace(S, "è", "\e`\")
  S = Replace(S, "é", "\e'\")
  S = Replace(S, "ê", "\e^\")
  S = Replace(S, "ë", "\e..\")
  S = Replace(S, "ì", "\i`\")
  S = Replace(S, "í", "\i'\")
  S = Replace(S, "î", "\i^\")
  S = Replace(S, "ï", "\i..\")
  S = Replace(S, "ð", "\-d\")
  S = Replace(S, "ñ", "\n~\")
  S = Replace(S, "ò", "\o`\")
  S = Replace(S, "ó", "\o'\")
  S = Replace(S, "ô", "\o^\")
  S = Replace(S, "õ", "\o~\")
  S = Replace(S, "ö", "\o..\")
  S = Replace(S, "÷", "\/\")
  S = Replace(S, "ø", "\o/\")
  S = Replace(S, "ù", "\u`\")
  S = Replace(S, "ú", "\u'\")
  S = Replace(S, "û", "\u^\")
  S = Replace(S, "ü", "\u..\")
  S = Replace(S, "ý", "\y'\")
  S = Replace(S, "þ", "\i>\")
  S = Replace(S, "ÿ", "\y..\")
  
  Open FN For Output As #1
  Print #1, "\START92\"
  Print #1, "\COMMENT=saved with " & App.Title & " v." & App.Major & "." & Format(App.Minor, "00") & "." & Format(App.Revision, "0000")
  Print #1, "\NAME=" & TIName
  Print #1, "\FILE=" & UCase(TIName) ' & Ext
  Print #1, S
  Print #1, "\STOP92\"
  Close #1
End Sub

Public Sub SaveTI(ByVal FN$, ByVal TIFolder$, ByVal TIName$, ByVal S$, ByVal FType As FTypes, Optional Tokenize As Boolean = True)
Attribute SaveTI.VB_Description = "Takes a file name FN, an on-calc folder name TIFolder, an on-calc file name TIName, a string of readable data S, a file type FType (as defined in the FTypes enumeration above), and an optional parameter Tokenize. Saves the data to FN in AMS format."
  WriteTIVar FN, TIFolder, TIName, Token(S, FType, Tokenize)
End Sub

Public Function Token$(ByVal S$, ByVal FType As FTypes, Optional Tokenize As Boolean = True)
Attribute Token.VB_Description = "Takes a string of readable data S, a file type FType (as defined in the FTypes enumeration above), and an optional parameter Tokenize. Returns a string in the format expected by WriteTIVar. Tokenize may be ignored based on the file type."
  Dim L2$, L$, TL$, c$, ias As Boolean, Item$, stack$, nb&, nb2&, useless As Boolean, i&, j&, dms As Boolean, vn As Boolean, nr As Boolean, brex As Boolean, Temp$, iarg&, narg&, locvars$, loccmd As Boolean, PV&, AddrStack$, ExitStack$, CycleStack$, funcdef As Boolean, poplocalateol As Boolean, commentindent&
  Select Case FType
    Case ft89t
      Token = Chr(0) & Chr(1) & S & Chr(0) & Chr(&HE0)
      Exit Function
    Case ft89p89f
      If Not Tokenize Then
        L2 = Mid(S, InStr(S, Chr(13)) + 1)
        If InStr(L2, Chr(13)) <> 0 Then
          L2 = Left(L2, InStr(L2, Chr(13)) - 1)
        End If
        If L2 = "Prgm" Then
          Token = S & Chr(0) & Chr(0) & Chr(0) & Chr(&H19) & Chr(&HE4) & Chr(&HE5) & Chr(0) & Chr(0) & Chr(8) & Chr(&HDC)
          Exit Function
        ElseIf L2 = "Func" Then
          Token = S & Chr(0) & Chr(0) & Chr(0) & Chr(&H17) & Chr(&HE4) & Chr(&HE5) & Chr(0) & Chr(0) & Chr(8) & Chr(&HDC)
          Exit Function
        Else
          Tokenize = True
        End If
      End If
      If Tokenize Then
        S = "__args" & S & Chr(0)
      End If
      'Case ft89e89l89m89s
  End Select
  ' Open "c:\toklog.txt" For Output As 1
  ' Print #1, S
  locvars = Chr(0)
  'convert ° ' " to 85 format ° ° °
  'must be done now to be able to use "in a string" detection
  ias = False
  For i = 1 To Len(S)
    Select Case Mid(S, i, 1)
      Case "°"
        If Mid(S, i - 1, 1) <> "_" Then
          vn = True
        Else
          vn = False
          If i > 2 Then
            For j = 2 To 17
              Select Case Mid(S, i - j, 1)
              Case "a" To "z", "A" To "Z", Chr(128), Chr(129), Chr(130), Chr(131), Chr(132), Chr(133), Chr(134), Chr(135), Chr(136), Chr(137), Chr(138), Chr(139), Chr(141), Chr(142), Chr(143), Chr(144), Chr(145), Chr(146), Chr(147), Chr(148), "À" To "Ö", "Ø" To "ö", "ø" To "ÿ", "_", Chr(154), Chr(155), Chr(178), "\"
                vn = True
                Exit For
              Case "0" To "9"
                'avoid Case Else
              Case Else
                Exit For
            End Select
            If i - j = 1 Then Exit For
          Next
        End If
      End If
      If Not ias And vn Then
        dms = True
        vn = False
        nr = False
        brex = False
        nb = 0
      End If
      Case "'"
        If dms And nb = 0 Then
          Mid(S, i, 1) = "°"
          vn = False
          nr = False
          brex = False
        End If
      Case """"
        If dms And nb = 0 Then
          Mid(S, i, 1) = "°"
          dms = False
        Else
          ias = Not ias
        End If
      Case "0" To "9"
        If dms And nb = 0 Then
          If brex Then
            dms = False
          ElseIf Not vn Then
            nr = True
          End If
        End If
      Case "."
        If dms And nb = 0 Then
          If vn Or brex Then
            dms = False
          Else
            nr = True
          End If
        End If
      Case "a" To "z", "A" To "Z", Chr(128), Chr(129), Chr(130), Chr(131), Chr(132), Chr(133), Chr(134), Chr(135), Chr(136), Chr(137), Chr(138), Chr(139), Chr(141), Chr(142), Chr(143), Chr(144), Chr(145), Chr(146), Chr(147), Chr(148), "À" To "Ö", "Ø" To "ö", "ø" To "ÿ", "_", Chr(154), Chr(155), Chr(178), "\"
        If dms And nb = 0 Then
          If nr Or brex Then
            dms = False
          Else
            vn = True
          End If
        End If
      Case "("
        If dms Then
          If nr Then
            dms = False
          Else
            nb = nb + 1
          End If
        End If
      Case ")"
        If dms Then
          nb = nb - 1
          brex = True
        End If
      Case "©"
        If Not ias Then
          i = InStr(i + 1, S, Chr(13)) + 1
          dms = False
        End If
      Case Else
        dms = False
    End Select
  Next
'  j = 0
'  ias = False
'  For i = 1 To Len(S)
'    Select Case Mid(S, i, 1)
'      Case """"
'        ias = Not ias
'      Case "(", "{", "["
'        If Not ias Then
'          j = j + 1
'        End If
'      Case ")", "}", "]"
'        If Not ias Then
'          j = j - 1
'        End If
'    End Select
'  Next
'  If j <> 0 Then
'    j = 1
'    iarg = 0
'    j = j / iarg
'  End If
  While S <> ""
    'Get the next line and remove it from S
    L = ""
    ias = False
    Do While S <> ""
      c = Left(S, 1)
      Select Case c
        Case "©"
          If Not ias Then
            While Right(L, 1) = " " And commentindent < 255
              commentindent = commentindent + 1
              L = Left(L, Len(L) - 1)
            Wend
            L = RTrim(L) 'remove excess spaces
            If L = "" Then
              Token = Chr(&HE8) & Chr(0) & Mid(S, 2, InStr(S, Chr(13)) - 2) & Chr(0) & Chr(commentindent) & Chr(&HE6) & Token
              S = Mid(S, InStr(S, Chr(13)))
              commentindent = 0
              i = 0
              While Mid(S, 2, 1) = " " And i < 255
                i = i + 1
                S = Left(S, 1) & Mid(S, 3)
              Wend
              Token = Chr(i) & Token
            Else
              stack = Left(S, InStr(S, Chr(13)) - 1) & Chr(0)
              S = Mid(S, InStr(S, Chr(13)))
              Exit Do
            End If
            c = ""
          End If
        Case ":", Chr(13), Chr(0)
          If Not ias Then Exit Do
        Case """"
          ias = Not ias
      End Select
      L = L & c
      S = Mid(S, 2)
    Loop
    If L <> "" Then
      TL = ""
      stack = L & Chr(0) & stack
      While stack <> ""
        If Left(stack, 3) = "___" Then
          If UCase(Left(stack, 5)) = "___E5" Then loccmd = False
          TL = Chr(Val("&H" & Mid(stack, 4, 2))) & TL
          stack = Mid(stack, 7)
        ElseIf Left(stack, 1) = "©" Then
          TL = Chr(&HE8) & Chr(0) & Mid(stack, 2, InStr(stack, Chr(0)) - 2) & Chr(0) & Chr(commentindent) & Chr(&HE6) & TL
          i = 0
          While Mid(S, 2, 1) = " " And i < 255
            i = i + 1
            S = Left(S, 1) & Mid(S, 3)
          Wend
          TL = Chr(i) & TL
          commentindent = 0
          stack = Mid(stack, InStr(stack, Chr(0)) + 1)
        Else
          Item = Trim(Left(stack, InStr(stack, Chr(0)) - 1))
          stack = Mid(stack, InStr(stack, Chr(0)) + 1)
          If Item = "" Then 'nothing
            TL = Chr(&H2E) & TL
          Else
            'remove useless parentheses
            Do While Left(Item, 1) = "(" And Right(Item, 1) = ")"
              ias = False
              useless = True
              nb = 0
              For i = 1 To Len(Item) - 1
                Select Case Mid(Item, i, 1)
                  Case """"
                    ias = Not ias
                  Case "("
                    If Not ias Then nb = nb + 1
                  Case ")"
                    If Not ias Then nb = nb - 1
                    If nb = 0 Then
                      useless = False
                      Exit For
                    End If
                End Select
              Next
              If useless Then Item = Trim(Mid(Item, 2, Len(Item) - 2)) Else Exit Do
            Loop
            'convert matrices to lists
            '[[][]] format
            PV = 0
            If Left(Item, 2) = "[[" And Right(Item, 2) = "]]" Then
              ias = False
              useless = True
              nb = 0
              j = Len(Item) - 1
              i = 1
              Do While i <= j
                Select Case Mid(Item, i, 1)
                  Case """"
                    ias = Not ias
                  Case "["
                    If Not ias Then nb = nb + 1
                  Case "]"
                    If Not ias Then nb = nb - 1
                    If nb = 0 Then
                      useless = False
                      Exit Do
                    End If
                  Case ","
                    While Mid(Item, i + 1, 1) = " "
                      Item = Left(Item, i) & Mid(Item, i + 2)
                      j = j - 1
                    Wend
                    If Not ias And nb = 2 Then
                      If Mid(Item, i + 1, 1) = Chr(159) Then
                        PV = PV + 1
                      Else
                        PV = PV + 3
                      End If
                    End If
                End Select
                i = i + 1
              Loop
              If useless Then
                i = 1
                While i <= Len(Item)
                  Select Case Mid(Item, i, 1)
                    Case """"
                      ias = Not ias
                    Case "]"
                      If Not ias Then
                        If Mid(Item, i, 2) = "][" Then
                          Item = Left(Item, i - 1) & "},{" & Mid(Item, i + 2)
                        End If
                      End If
                  End Select
                  i = i + 1
                Wend
                Select Case PV
                  Case 1 'Polar
                    TL = Chr(&H7B) & TL
                  Case 2 'Sphere
                    TL = Chr(&H7D) & TL
                  Case 4 'Cylind
                    TL = Chr(&H7C) & TL
                End Select
                Item = "{{" & Mid(Item, 3, Len(Item) - 4) & "}}"
              End If
            End If
            '[,;,] format
            PV = 0
            If Left(Item, 1) = "[" And Right(Item, 1) = "]" Then
              ias = False
              useless = True
              nb = 0
              For i = 1 To Len(Item) - 1
                Select Case Mid(Item, i, 1)
                  Case """"
                    ias = Not ias
                  Case "["
                    If Not ias Then nb = nb + 1
                  Case "]"
                    If Not ias Then nb = nb - 1
                    If nb = 0 Then
                      useless = False
                      Exit For
                    End If
                  Case ","
                    If Not ias And nb = 1 Then
                      If Mid(Item, i + 1, 1) = Chr(159) Then
                        PV = PV + 1
                      Else
                        PV = PV + 3
                      End If
                    End If
                End Select
              Next
              If useless Then
                i = 1
                While i <= Len(Item)
                  Select Case Mid(Item, i, 1)
                    Case """"
                      ias = Not ias
                    Case ";"
                      If Not ias Then Item = Left(Item, i - 1) & "},{" & Mid(Item, i + 1)
                  End Select
                  i = i + 1
                Wend
                Select Case PV
                  Case 1 'Polar
                    TL = Chr(&H7B) & TL
                  Case 2 'Sphere
                    TL = Chr(&H7D) & TL
                  Case 4 'Cylind
                    TL = Chr(&H7C) & TL
                End Select
                Item = "{{" & Mid(Item, 2, Len(Item) - 2) & "}}"
              End If
            End If
            'detect lists
            Do While Left(Item, 1) = "{" And Right(Item, 1) = "}"
              ias = False
              useless = True
              nb = 0
              For i = 1 To Len(Item) - 1
                Select Case Mid(Item, i, 1)
                  Case """"
                    ias = Not ias
                  Case "{"
                    If Not ias Then nb = nb + 1
                  Case "}"
                    If Not ias Then nb = nb - 1
                    If nb = 0 Then
                      useless = False
                      Exit For
                    End If
                End Select
              Next
              If useless Then
                If Item = "{}" Then
                  TL = Chr(&HE5) & Chr(&HD9) & TL
                  Item = ""
                Else
                  stack = Mid(Item, 2, Len(Item) - 2) & Chr(0) & "___E5" & Chr(0) & stack
                  TL = Chr(&HD9) & TL
                  Item = ""
                End If
              Else
                Exit Do
              End If
            Loop
            
            'TRICK TO BREAK 64K LIMIT:
            'CALL SUBPROCEDURE PASSING ALL ARGUMENTS AND
            'LOCAL VARIABLES "BYREF" TO TRICK IT INTO
            'BEING ABLE TO USE THEM IN THE SAME WAY AS
            'IF IT WAS 1 PROCEDURE
            
            Token_2 S, FType, Tokenize, L2, L, TL, c, ias, Item, stack, nb, nb2, useless, i, j, dms, vn, nr, brex, Temp, iarg, narg, locvars, loccmd, PV, AddrStack, ExitStack, CycleStack, Token, funcdef, poplocalateol
            
            'CODE CONTINUES HERE
            
            'remove useless spaces
            'convert " " to "*" when applicable
            'convert " (" to "(" when applicable
            'convert "(" or " (" to "*(" when applicable
            'convert "[" or " [" to "*[" when applicable
            'convert "{" or " {" to "*{" when applicable
            'convert "."+operator to " ."+operator when applicable
            'convert number+variable to number+"*"+variable when applicable
            i = 1
            ias = False
            nb = 0
            While i < Len(Item)
              Select Case Mid(Item, i, 1)
                Case """"
                  ias = Not ias
                  i = i + 1
                Case " "
                  If ias Then
                    i = i + 1
                  Else
                    Select Case Mid(Item, i + 1, 1)
                    Case " ", ",", ";", Chr(22), "|", Chr(18), "=", ">", "<", Chr(156), Chr(157), Chr(158), Chr(159), "+", "-", "±", "*", "/", "^", "°", "!", "%", "&", Chr(152), Chr(153), "'", ")", "]", "}"
                      Item = Left(Item, i - 1) & Mid(Item, i + 1)
                    Case "("
                      vn = False
                      'indirection -> function call
                      If Mid(Item, i - 1, 1) = """" Then
                        For j = i - 2 To 2 Step -1
                          If Mid(Item, j, 1) = """" Then
                            If Mid(Item, j - 1, 1) = "#" Then
                              vn = True
                              Exit For
                            ElseIf Mid(Item, j - 1, 1) <> """" Then
                              Exit For
                            End If
                          End If
                        Next
                      ElseIf Mid(Item, i - 1, 1) = ")" Then
                        nb2 = -1
                        For j = i - 2 To 2 Step -1
                          Select Case Mid(Item, j, 1)
                          Case """"
                            ias = Not ias
                          Case "("
                            If Not ias Then
                              nb2 = nb2 + 1
                              If nb2 = 0 Then
                                If Mid(Item, j - 1, 1) = "#" Then vn = True
                                Exit For
                              End If
                            End If
                          Case ")"
                            If Not ias Then nb2 = nb2 - 1
                        End Select
                      Next
                      ias = False
                    End If
                    For j = 1 To 17
                      Select Case Mid(Item, i - j, 1)
                        Case "a" To "z", "A" To "Z", Chr(128), Chr(129), Chr(130), Chr(131), Chr(132), Chr(133), Chr(134), Chr(135), Chr(136), Chr(137), Chr(138), Chr(139), Chr(141), Chr(142), Chr(143), Chr(144), Chr(145), Chr(146), Chr(147), Chr(148), "À" To "Ö", "Ø" To "ö", "ø" To "ÿ", "_", Chr(154), Chr(155), Chr(178), "\"
                          vn = True
                          Exit For
                        Case "0" To "9"
                          If i - j = 1 Then Exit For
                        Case Else
                          Exit For
                      End Select
                    Next
                    'system variable -> *
                    If vn Then
                      For j = 1 To 17
                        Select Case Mid(Item, i - j, 1)
                          Case "0" To "9", "a" To "z", "A" To "Z", Chr(128), Chr(129), Chr(130), Chr(131), Chr(132), Chr(133), Chr(134), Chr(135), Chr(136), Chr(137), Chr(138), Chr(139), Chr(141), Chr(142), Chr(143), Chr(144), Chr(145), Chr(146), Chr(147), Chr(148), "À" To "Ö", "Ø" To "ö", "ø" To "ÿ", "_", Chr(154), Chr(155), Chr(178), "\"
                            If i - j = 1 Then
                              j = j + 1
                              Exit For
                            End If
                          Case Else
                            Exit For
                        End Select
                      Next
                      Select Case LCase89(Mid(Item, i - j + 1, j - 1))
  Case "true", "false", "undef", Chr(154), Chr(155), Chr(142) & "x", Chr(142) & "x²", Chr(142) & "y", Chr(142) & "y²", Chr(142) & "xy", "sx", "sy", Chr(143) & "x", Chr(143) & "y", "nstat", "minx", "miny", "q1", "medstat", "q3", "maxx", "maxy", "corr", "r²", "medx1", "medx2", "medx3", "medy1", "medy2", "medy3", "xc", "yc", "zc", "tc", "rc", Chr(136) & "c", "nc", "xfact", "yfact", "zfact", "xmin", "xmax", "xscl", "ymin", "ymax", "yscl", Chr(132) & "x", Chr(132) & "y", "xres", "xgrid", "ygrid", "zmin", "zmax", "zscl", "eye" & Chr(136), "eye" & Chr(145), Chr(136) & "min", Chr(136) & "max", Chr(136) & "step", "tmin", "tmax", "tstep", "nmin", "nmax", "plotstrt", "plotstep", "zxmin", "zxmax", "zxscl", "zymin", "zymax", "zyscl", "zxres", "z" & Chr(136) & "min", "z" & Chr(136) & "max", "z" & Chr(136) & "step", "ztmin", "ztmax", "ztstep", "zxgrid", "zygrid", "zzmin", "zzmax", "zzscl", "zeye" & Chr(136), "zeye" & Chr(145), "znmin", "znmax", "zpltstep", "zpltstrt", "seed1", "seed2", "ok", "errornum", "sysmath", _
       "sysdata", "tblinput", "tblstart", Chr(132) & "tbl", "fldpic", "eye" & Chr(146), "tplot", "diftol", "zeye" & Chr(146), "t0", "dtime", "ncurves", "fldres", "estep", "zt0de", "ztmaxde", "ztstepde", "ztplotde", "ncontour"
                          vn = False
                      End Select
                    End If
                    If vn Then
                      Item = Left(Item, i - 1) & Mid(Item, i + 1)
                    Else
                      'operator (or _sqrt_, _integrate_, _differentiate_) -> no space
                      Select Case Mid(Item, i - 1, 1)
                        Case Chr(173), "#", ",", ";", Chr(22), Chr(18), "|", "=", ">", "<", Chr(156), Chr(157), Chr(158), Chr(159), "+", "-", "±", "*", "/", "^", "&", "(", "[", "{", Chr(168), Chr(189), Chr(188)
                          Item = Left(Item, i - 1) & Mid(Item, i + 1)
                        Case Else
                          Mid(Item, i, 1) = "*"
                          i = i + 1
                      End Select
                    End If
                    Case "["
                      If Mid(Item, i + 2, 1) = "[" Then
                        'Mid(Item, i, 1) = "*" (2 .* [)
                        i = i + 1
                      Else
                        vn = False
                        'indirection -> array subscript
                        If Mid(Item, i - 1, 1) = """" Then
                          For j = i - 2 To 2 Step -1
                            If Mid(Item, j, 1) = """" Then
                              If Mid(Item, j - 1, 1) = "#" Then
                                vn = True
                                Exit For
                              ElseIf Mid(Item, j - 1, 1) <> """" Then
                                Exit For
                              End If
                            End If
                          Next
                        ElseIf Mid(Item, i - 1, 1) = ")" Then
                          nb2 = -1
                          For j = i - 2 To 2 Step -1
                            Select Case Mid(Item, j, 1)
                            Case """"
                              ias = Not ias
                            Case "("
                              If Not ias Then
                                nb2 = nb2 + 1
                                If nb2 = 0 Then
                                  If Mid(Item, j - 1, 1) = "#" Then vn = True
                                  Exit For
                                End If
                              End If
                            Case ")"
                              If Not ias Then nb2 = nb2 - 1
                          End Select
                        Next
                        ias = False
                      End If
                      Select Case Mid(Item, i - 1, 1)
                        Case ")", "]", "}"
                          vn = True
                      End Select
                      For j = 1 To 17
                        Select Case Mid(Item, i - j, 1)
                          Case "a" To "z", "A" To "Z", Chr(128), Chr(129), Chr(130), Chr(131), Chr(132), Chr(133), Chr(134), Chr(135), Chr(136), Chr(137), Chr(138), Chr(139), Chr(141), Chr(142), Chr(143), Chr(144), Chr(145), Chr(146), Chr(147), Chr(148), "À" To "Ö", "Ø" To "ö", "ø" To "ÿ", "_", Chr(154), Chr(155), Chr(178), "\"
                            vn = True
                            Exit For
                          Case "0" To "9"
                            If i - j = 1 Then Exit For
                          Case Else
                            Exit For
                        End Select
                      Next
                      'system variable -> *
                      If vn Then
                        For j = 1 To 17
                          Select Case Mid(Item, i - j, 1)
                            Case "0" To "9", "a" To "z", "A" To "Z", Chr(128), Chr(129), Chr(130), Chr(131), Chr(132), Chr(133), Chr(134), Chr(135), Chr(136), Chr(137), Chr(138), Chr(139), Chr(141), Chr(142), Chr(143), Chr(144), Chr(145), Chr(146), Chr(147), Chr(148), "À" To "Ö", "Ø" To "ö", "ø" To "ÿ", "_", Chr(154), Chr(155), Chr(178), "\"
                              If i - j = 1 Then
                                j = j + 1
                                Exit For
                              End If
                            Case Else
                              Exit For
                          End Select
                        Next
                        Select Case LCase89(Mid(Item, i - j + 1, j - 1))
  Case "true", "false", "undef", Chr(154), Chr(155), Chr(142) & "x", Chr(142) & "x²", Chr(142) & "y", Chr(142) & "y²", Chr(142) & "xy", "sx", "sy", Chr(143) & "x", Chr(143) & "y", "nstat", "minx", "miny", "q1", "medstat", "q3", "maxx", "maxy", "corr", "r²", "medx1", "medx2", "medx3", "medy1", "medy2", "medy3", "xc", "yc", "zc", "tc", "rc", Chr(136) & "c", "nc", "xfact", "yfact", "zfact", "xmin", "xmax", "xscl", "ymin", "ymax", "yscl", Chr(132) & "x", Chr(132) & "y", "xres", "xgrid", "ygrid", "zmin", "zmax", "zscl", "eye" & Chr(136), "eye" & Chr(145), Chr(136) & "min", Chr(136) & "max", Chr(136) & "step", "tmin", "tmax", "tstep", "nmin", "nmax", "plotstrt", "plotstep", "zxmin", "zxmax", "zxscl", "zymin", "zymax", "zyscl", "zxres", "z" & Chr(136) & "min", "z" & Chr(136) & "max", "z" & Chr(136) & "step", "ztmin", "ztmax", "ztstep", "zxgrid", "zygrid", "zzmin", "zzmax", "zzscl", "zeye" & Chr(136), "zeye" & Chr(145), "znmin", "znmax", "zpltstep", "zpltstrt", "seed1", "seed2", "ok", "errornum", "sysmath", _
       "sysdata", "tblinput", "tblstart", Chr(132) & "tbl", "fldpic", "eye" & Chr(146), "tplot", "diftol", "zeye" & Chr(146), "t0", "dtime", "ncurves", "fldres", "estep", "zt0de", "ztmaxde", "ztstepde", "ztplotde", "ncontour"
                            vn = False
                        End Select
                      End If
                      If vn Then
                        Item = Left(Item, i - 1) & Mid(Item, i + 1)
                      Else
                        'operator -> no space
                        Select Case Mid(Item, i - 1, 1)
                          Case Chr(173), "#", ",", ";", Chr(22), Chr(18), "|", "=", ">", "<", Chr(156), Chr(157), Chr(158), Chr(159), "+", "-", "±", "*", "/", "^", "&", "(", "[", "{"
                            Item = Left(Item, i - 1) & Mid(Item, i + 1)
                          Case Else
                            Mid(Item, i, 1) = "*"
                            i = i + 1
                        End Select
                      End If
                    End If
                    Case "."
                      Select Case Mid(Item, i + 2, 1)
                      Case "+", "-", "*", "/", "^"
                        i = i + 1
                      Case Else
                        Select Case Mid(Item, i - 1, 1)
                        Case Chr(173), "#", ",", ";", Chr(22), Chr(18), "|", "=", ">", "<", Chr(156), Chr(157), Chr(158), Chr(159), "+", "-", "±", "*", "/", "^", "&"
                          Item = Left(Item, i - 1) & Mid(Item, i + 1)
                        Case Else
                          Mid(Item, i, 1) = "*"
                          i = i + 1
                      End Select
                    End Select
                    Case Else
                      'operator -> no space
                      Select Case Mid(Item, i - 1, 1)
                      Case Chr(173), "#", ",", ";", Chr(22), Chr(18), "|", "=", ">", "<", Chr(156), Chr(157), Chr(158), Chr(159), "+", "-", "±", "*", "/", "^", "&", "(", "[", "{"
                        Item = Left(Item, i - 1) & Mid(Item, i + 1)
                      Case Else
                        Mid(Item, i, 1) = "*"
                        i = i + 1
                    End Select
                  End Select
                End If
                Case "("
                  If ias Then
                    i = i + 1
                  Else
                    vn = False
                    If i = 1 Then
                      i = 2
                    Else
                      'indirection -> function call
                      If Mid(Item, i - 1, 1) = """" Then
                        For j = i - 2 To 2 Step -1
                          If Mid(Item, j, 1) = """" Then
                            If Mid(Item, j - 1, 1) = "#" Then
                              vn = True
                              Exit For
                            ElseIf Mid(Item, j - 1, 1) <> """" Then
                              Exit For
                            End If
                          End If
                        Next
                      ElseIf Mid(Item, i - 1, 1) = ")" Then
                        nb2 = -1
                        For j = i - 2 To 2 Step -1
                          Select Case Mid(Item, j, 1)
                          Case """"
                            ias = Not ias
                          Case "("
                            If Not ias Then
                              nb2 = nb2 + 1
                              If nb2 = 0 Then
                                If Mid(Item, j - 1, 1) = "#" Then vn = True
                                Exit For
                              End If
                            End If
                          Case ")"
                            If Not ias Then nb2 = nb2 - 1
                        End Select
                      Next
                      ias = False
                    End If
                    For j = 1 To 17
                      Select Case Mid(Item, i - j, 1)
                        Case "a" To "z", "A" To "Z", Chr(180), Chr(128), Chr(129), Chr(130), Chr(131), Chr(132), Chr(133), Chr(134), Chr(135), Chr(136), Chr(137), Chr(138), Chr(139), Chr(141), Chr(142), Chr(143), Chr(144), Chr(145), Chr(146), Chr(147), Chr(148), "À" To "Ö", "Ø" To "ö", "ø" To "ÿ", "_", Chr(154), Chr(155), Chr(178), "\"
                          vn = True
                          Exit For
                        Case "0" To "9"
                          If i - j = 1 Then Exit For
                        Case Else
                          Exit For
                      End Select
                    Next
                    'system variable -> *
                    If vn Then
                      For j = 1 To 17
                        Select Case Mid(Item, i - j, 1)
                          Case "0" To "9", "a" To "z", "A" To "Z", Chr(180), Chr(128), Chr(129), Chr(130), Chr(131), Chr(132), Chr(133), Chr(134), Chr(135), Chr(136), Chr(137), Chr(138), Chr(139), Chr(141), Chr(142), Chr(143), Chr(144), Chr(145), Chr(146), Chr(147), Chr(148), "À" To "Ö", "Ø" To "ö", "ø" To "ÿ", "_", Chr(154), Chr(155), Chr(178), "\"
                            If i - j = 1 Then
                              j = j + 1
                              Exit For
                            End If
                          Case Else
                            Exit For
                        End Select
                      Next
                      Select Case LCase89(Mid(Item, i - j + 1, j - 1))
  Case "true", "false", "undef", Chr(154), Chr(155), Chr(142) & "x", Chr(142) & "x²", Chr(142) & "y", Chr(142) & "y²", Chr(142) & "xy", "sx", "sy", Chr(143) & "x", Chr(143) & "y", "nstat", "minx", "miny", "q1", "medstat", "q3", "maxx", "maxy", "corr", "r²", "medx1", "medx2", "medx3", "medy1", "medy2", "medy3", "xc", "yc", "zc", "tc", "rc", Chr(136) & "c", "nc", "xfact", "yfact", "zfact", "xmin", "xmax", "xscl", "ymin", "ymax", "yscl", Chr(132) & "x", Chr(132) & "y", "xres", "xgrid", "ygrid", "zmin", "zmax", "zscl", "eye" & Chr(136), "eye" & Chr(145), Chr(136) & "min", Chr(136) & "max", Chr(136) & "step", "tmin", "tmax", "tstep", "nmin", "nmax", "plotstrt", "plotstep", "zxmin", "zxmax", "zxscl", "zymin", "zymax", "zyscl", "zxres", "z" & Chr(136) & "min", "z" & Chr(136) & "max", "z" & Chr(136) & "step", "ztmin", "ztmax", "ztstep", "zxgrid", "zygrid", "zzmin", "zzmax", "zzscl", "zeye" & Chr(136), "zeye" & Chr(145), "znmin", "znmax", "zpltstep", "zpltstrt", "seed1", "seed2", "ok", "errornum", "sysmath", _
       "sysdata", "tblinput", "tblstart", Chr(132) & "tbl", "fldpic", "eye" & Chr(146), "tplot", "diftol", "zeye" & Chr(146), "t0", "dtime", "ncurves", "fldres", "estep", "zt0de", "ztmaxde", "ztstepde", "ztplotde", "ncontour"
                          vn = False
                      End Select
                    End If
                    If vn Then
                      i = i + 1
                    Else
                      'operator (or _sqrt_, _integrate_, _differentiate_) -> no space
                      Select Case Mid(Item, i - 1, 1)
                        Case Chr(173), "'", "#", ",", ";", Chr(22), Chr(18), "|", "=", ">", "<", Chr(156), Chr(157), Chr(158), Chr(159), "+", "-", "±", "*", "/", "^", "(", "[", "{", Chr(168), Chr(189), Chr(188)
                          i = i + 1
                        Case Else
                          Item = Left(Item, i - 1) & "*" & Mid(Item, i)
                          i = i + 2
                      End Select
                    End If
                  End If
                End If
                Case "["
                  If ias Then
                    i = i + 1
                  Else
                    If Mid(Item, i + 1, 1) = "[" Then
                      If i = 1 Then
                        i = 3
                      Else
                        'operator -> no space
                        Select Case Mid(Item, i - 1, 1)
                        Case " ", Chr(173), "#", ",", ";", Chr(22), Chr(18), "|", "=", ">", "<", Chr(156), Chr(157), Chr(158), Chr(159), "+", "-", "±", "*", "/", "^", "(", "[", "{"
                          i = i + 2
                        Case Else
                          Item = Left(Item, i - 1) & "*" & Mid(Item, i)
                          i = i + 3
                      End Select
                    End If
                  Else
                    vn = False
                    If i = 1 Then
                      i = 2
                    Else
                      'indirection -> array subscript
                      If Mid(Item, i - 1, 1) = """" Then
                        For j = i - 2 To 2 Step -1
                          If Mid(Item, j, 1) = """" Then
                            If Mid(Item, j - 1, 1) = "#" Then
                              vn = True
                              Exit For
                            ElseIf Mid(Item, j - 1, 1) <> """" Then
                              Exit For
                            End If
                          End If
                        Next
                      ElseIf Mid(Item, i - 1, 1) = ")" Then
                        nb2 = -1
                        For j = i - 2 To 2 Step -1
                          Select Case Mid(Item, j, 1)
                            Case """"
                              ias = Not ias
                            Case "("
                              If Not ias Then
                                nb2 = nb2 + 1
                                If nb2 = 0 Then
                                  If Mid(Item, j - 1, 1) = "#" Then vn = True
                                  Exit For
                                End If
                              End If
                            Case ")"
                              If Not ias Then nb2 = nb2 - 1
                          End Select
                        Next
                        ias = False
                      End If
                      Select Case Mid(Item, i - 1, 1)
                        Case ")", "]", "}"
                          vn = True
                      End Select
                      For j = 1 To 17
                        Select Case Mid(Item, i - j, 1)
                          Case "a" To "z", "A" To "Z", Chr(128), Chr(129), Chr(130), Chr(131), Chr(132), Chr(133), Chr(134), Chr(135), Chr(136), Chr(137), Chr(138), Chr(139), Chr(141), Chr(142), Chr(143), Chr(144), Chr(145), Chr(146), Chr(147), Chr(148), "À" To "Ö", "Ø" To "ö", "ø" To "ÿ", "_", Chr(154), Chr(155), Chr(178), "\"
                            vn = True
                            Exit For
                          Case "0" To "9"
                            If i - j = 1 Then Exit For
                          Case Else
                            Exit For
                        End Select
                      Next
                      'system variable -> *
                      If vn Then
                        For j = 1 To 17
                          Select Case Mid(Item, i - j, 1)
                            Case "0" To "9", "a" To "z", "A" To "Z", Chr(128), Chr(129), Chr(130), Chr(131), Chr(132), Chr(133), Chr(134), Chr(135), Chr(136), Chr(137), Chr(138), Chr(139), Chr(141), Chr(142), Chr(143), Chr(144), Chr(145), Chr(146), Chr(147), Chr(148), "À" To "Ö", "Ø" To "ö", "ø" To "ÿ", "_", Chr(154), Chr(155), Chr(178), "\"
                              If i - j = 1 Then
                                j = j + 1
                                Exit For
                              End If
                            Case Else
                              Exit For
                          End Select
                        Next
                        Select Case LCase89(Mid(Item, i - j + 1, j - 1))
  Case "true", "false", "undef", Chr(154), Chr(155), Chr(142) & "x", Chr(142) & "x²", Chr(142) & "y", Chr(142) & "y²", Chr(142) & "xy", "sx", "sy", Chr(143) & "x", Chr(143) & "y", "nstat", "minx", "miny", "q1", "medstat", "q3", "maxx", "maxy", "corr", "r²", "medx1", "medx2", "medx3", "medy1", "medy2", "medy3", "xc", "yc", "zc", "tc", "rc", Chr(136) & "c", "nc", "xfact", "yfact", "zfact", "xmin", "xmax", "xscl", "ymin", "ymax", "yscl", Chr(132) & "x", Chr(132) & "y", "xres", "xgrid", "ygrid", "zmin", "zmax", "zscl", "eye" & Chr(136), "eye" & Chr(145), Chr(136) & "min", Chr(136) & "max", Chr(136) & "step", "tmin", "tmax", "tstep", "nmin", "nmax", "plotstrt", "plotstep", "zxmin", "zxmax", "zxscl", "zymin", "zymax", "zyscl", "zxres", "z" & Chr(136) & "min", "z" & Chr(136) & "max", "z" & Chr(136) & "step", "ztmin", "ztmax", "ztstep", "zxgrid", "zygrid", "zzmin", "zzmax", "zzscl", "zeye" & Chr(136), "zeye" & Chr(145), "znmin", "znmax", "zpltstep", "zpltstrt", "seed1", "seed2", "ok", "errornum", "sysmath", _
       "sysdata", "tblinput", "tblstart", Chr(132) & "tbl", "fldpic", "eye" & Chr(146), "tplot", "diftol", "zeye" & Chr(146), "t0", "dtime", "ncurves", "fldres", "estep", "zt0de", "ztmaxde", "ztstepde", "ztplotde", "ncontour"
                            vn = False
                        End Select
                      End If
                      If vn Then
                        i = i + 1
                      Else
                        'operator -> no space
                        Select Case Mid(Item, i - 1, 1)
                          Case Chr(173), "#", ",", ";", Chr(22), Chr(18), "|", "=", ">", "<", Chr(156), Chr(157), Chr(158), Chr(159), "+", "-", "±", "*", "/", "^", "(", "[", "{"
                            i = i + 1
                          Case Else
                            Item = Left(Item, i - 1) & "*" & Mid(Item, i)
                            i = i + 2
                        End Select
                      End If
                    End If
                  End If
                End If
                Case "{"
                  If ias Then
                    i = i + 1
                  Else
                    If i = 1 Then
                      i = 2
                    Else
                      'operator -> no space
                      Select Case Mid(Item, i - 1, 1)
                      Case Chr(173), "#", ",", ";", Chr(22), Chr(18), "|", "=", ">", "<", Chr(156), Chr(157), Chr(158), Chr(159), "+", "-", "±", "*", "/", "^", "(", "[", "{"
                        i = i + 1
                      Case Else
                        Item = Left(Item, i - 1) & "*" & Mid(Item, i)
                        i = i + 2
                    End Select
                  End If
                End If
                Case Chr(168), Chr(189), Chr(188)
                  '_sqrt_, _integrate_, _differentiate_
                  If ias Then
                    i = i + 1
                  Else
                    If i = 1 Then
                      i = 2
                    Else
                      'operator -> no space
                      Select Case Mid(Item, i - 1, 1)
                      Case Chr(173), "#", ",", ";", Chr(22), Chr(18), "|", "=", ">", "<", Chr(156), Chr(157), Chr(158), Chr(159), "+", "-", "±", "*", "/", "^", "(", "[", "{"
                        i = i + 1
                      Case Else
                        Item = Left(Item, i - 1) & "*" & Mid(Item, i)
                        i = i + 2
                    End Select
                  End If
                  Item = Left(Item, i - 1) & LTrim(Mid(Item, i))
                End If
                Case Chr(150), Chr(151), Chr(140)
                  '_e_, _i_, _pi_
                  If ias Then
                    i = i + 1
                  Else
                    If i = 1 Then
                      i = 2
                    Else
                      'operator -> no space
                      Select Case Mid(Item, i - 1, 1)
                      Case Chr(173), "#", ",", ";", Chr(22), Chr(18), "|", "=", ">", "<", Chr(156), Chr(157), Chr(158), Chr(159), "+", "-", "±", "*", "/", "^", "(", "[", "{"
                        i = i + 1
                      Case Else
                        Item = Left(Item, i - 1) & "*" & Mid(Item, i)
                        i = i + 2
                    End Select
                  End If
                  If i < Len(Item) Then
                    Item = Left(Item, i) & " " & Mid(Item, i + 1)
                    i = i + 1
                  End If
                End If
                Case "."
                  If ias Then
                    i = i + 1
                  Else
                    If i = 1 Then
                      i = 2
                    Else
                      Select Case Mid(Item, i - 1, 1)
                      Case "0" To "9", "."
                        nr = True
                      Case Else
                        nr = False
                    End Select
                    For j = 1 To 17
                      Select Case Mid(Item, i - j, 1)
                        Case "a" To "z", "A" To "Z", Chr(128), Chr(129), Chr(130), Chr(131), Chr(132), Chr(133), Chr(134), Chr(135), Chr(136), Chr(137), Chr(138), Chr(139), Chr(141), Chr(142), Chr(143), Chr(144), Chr(145), Chr(146), Chr(147), Chr(148), "À" To "Ö", "Ø" To "ö", "ø" To "ÿ", "_", Chr(154), Chr(155), Chr(178), "\"
                          nr = False
                          Exit For
                        Case "0" To "9"
                          If i - j = 1 Then Exit For
                        Case Else
                          Exit For
                      End Select
                    Next
                    If nr Then
                      i = i + 1
                    Else
                      Select Case Mid(Item, i + 1, 1)
                        Case "+", "-", "*", "/", "^"
                          i = i + 1
                        Case Else
                          Select Case Mid(Item, i - 1, 1)
                          Case "(", "{", "[", Chr(173), "#", ",", ";", Chr(22), Chr(18), "|", "=", ">", "<", Chr(156), Chr(157), Chr(158), Chr(159), "+", "-", "±", "*", "/", "^"
                            i = i + 1
                          Case Else
                            Item = Left(Item, i - 1) & "*" & Mid(Item, i)
                            i = i + 2
                        End Select
                      End Select
                    End If
                  End If
                End If
                Case "a" To "z", "A" To "Z", Chr(128), Chr(129), Chr(130), Chr(131), Chr(132), Chr(133), Chr(134), Chr(135), Chr(136), Chr(137), Chr(138), Chr(139), Chr(141), Chr(142), Chr(143), Chr(144), Chr(145), Chr(146), Chr(147), Chr(148), "À" To "Ö", "Ø" To "ö", "ø" To "ÿ", "_", Chr(154), Chr(155), Chr(178), "\"
                  If ias Then
                    i = i + 1
                  Else
                    If i = 1 Then
                      i = 2
                    Else
                      Select Case Mid(Item, i - 1, 1)
                      Case "0" To "9", "."
                      If InStr("bhBH", Mid(Item, i, 1)) And Mid(Item, i - 1, 1) = "0" Then
                          nr = False
                        Else
                          nr = True
                        End If
                      Case Else
                        nr = False
                    End Select
                    For j = 1 To 17
                      Select Case Mid(Item, i - j, 1)
                        Case "a" To "z", "A" To "Z", Chr(128), Chr(129), Chr(130), Chr(131), Chr(132), Chr(133), Chr(134), Chr(135), Chr(136), Chr(137), Chr(138), Chr(139), Chr(141), Chr(142), Chr(143), Chr(144), Chr(145), Chr(146), Chr(147), Chr(148), "À" To "Ö", "Ø" To "ö", "ø" To "ÿ", "_", Chr(154), Chr(155), Chr(178), "\"
                          nr = False
                          Exit For
                        Case "0" To "9"
                          If i - j = 1 Then Exit For
                        Case Else
                          Exit For
                      End Select
                    Next
                    If nr Then
                      Item = Left(Item, i - 1) & "*" & Mid(Item, i)
                      i = i + 2
                    Else
                      i = i + 1
                    End If
                  End If
                End If
                Case Else
                  i = i + 1
              End Select
            Wend
            'fix boolean operators:
            'convert "not*" to "not "
            'convert "not(" to "not ("
            'convert "*and*" to " and "
            'convert "*and(" to " and ("
            'convert "*or*" to " or "
            'convert "*or(" to " or ("
            'convert "*xor*" to " xor "
            'convert "*xor(" to " xor ("
            i = 1
            ias = False
            nb = 0
            While i < Len(Item)
              Select Case Mid(Item, i, 1)
                Case """"
                  ias = Not ias
                  i = i + 1
                Case ")"
                  If ias Then
                    i = i + 1
                  Else
                    If LCase89(Mid(Item, i, 4)) = ")and" Then
                      Item = Left(Item, i - 1) & ")*and" & Mid(Item, i + 4)
                      i = i + 1
                    ElseIf LCase89(Mid(Item, i, 3)) = ")or" Then
                      Item = Left(Item, i - 1) & ")*or" & Mid(Item, i + 3)
                      i = i + 1
                    ElseIf LCase89(Mid(Item, i, 4)) = ")xor" Then
                      Item = Left(Item, i - 1) & ")*xor" & Mid(Item, i + 4)
                      i = i + 1
                    Else
                      i = i + 1
                    End If
                  End If
                Case "*"
                  If ias Then
                    i = i + 1
                  Else
                    If LCase89(Mid(Item, i, 5)) = "*and*" Then
                      Mid(Item, i, 5) = " and "
                      i = i + 5
                    ElseIf LCase89(Mid(Item, i, 4)) = "*or*" Then
                      Mid(Item, i, 4) = " or "
                      i = i + 4
                    ElseIf LCase89(Mid(Item, i, 5)) = "*xor*" Then
                      Mid(Item, i, 5) = " xor "
                      i = i + 5
                    ElseIf LCase89(Mid(Item, i, 5)) = "*and(" Then
                      Item = Left(Item, i - 1) & " and (" & Mid(Item, i + 5)
                      i = i + 6
                    ElseIf LCase89(Mid(Item, i, 4)) = "*or(" Then
                      Item = Left(Item, i - 1) & " or (" & Mid(Item, i + 4)
                      i = i + 5
                    ElseIf LCase89(Mid(Item, i, 5)) = "*xor(" Then
                      Item = Left(Item, i - 1) & " xor (" & Mid(Item, i + 5)
                      i = i + 6
                    Else
                      i = i + 1
                    End If
                  End If
                Case "n", "N"
                  If ias Then
                    i = i + 1
                  Else
                    vn = False
                    If i = 1 Then
                      vn = True
                    Else
                      Select Case Mid(Item, i - 1, 1)
                      Case "0" To "9", "a" To "z", "A" To "Z", Chr(128), Chr(129), Chr(130), Chr(131), Chr(132), Chr(133), Chr(134), Chr(135), Chr(136), Chr(137), Chr(138), Chr(139), Chr(141), Chr(142), Chr(143), Chr(144), Chr(145), Chr(146), Chr(147), Chr(148), "À" To "Ö", "Ø" To "ö", "ø" To "ÿ", "_", Chr(154), Chr(155), Chr(178), "\"
                        'avoid Case Else
                      Case Else
                        vn = True
                    End Select
                  End If
                  If vn Then
                    If LCase89(Mid(Item, i, 4)) = "not*" Then
                      Mid(Item, i, 4) = "not "
                      i = i + 4
                    ElseIf LCase89(Mid(Item, i, 4)) = "not(" Then
                      Item = Left(Item, i - 1) & "not (" & Mid(Item, i + 4)
                      i = i + 5
                    Else
                      i = i + 1
                    End If
                  Else
                    i = i + 1
                  End If
                End If
                Case Else
                  i = i + 1
              End Select
            Wend
            ',
            ias = False
            nb = 0
            For i = 1 To Len(Item)
              Select Case Mid(Item, i, 1)
                Case """"
                  ias = Not ias
                Case "(", "{", "["
                  If Not ias Then nb = nb + 1
                Case ")", "}", "]"
                  If Not ias Then nb = nb - 1
                Case ","
                  If Not ias And nb = 0 Then
                    If Mid(Item, i + 1, 1) = Chr(159) Then
                      Item = Left(Item, i) & Mid(Item, i + 2)
                    End If
                    stack = Left(Item, i - 1) & Chr(0) & Mid(Item, i + 1) & Chr(0) & stack
                    Item = ""
                    Exit For
                  End If
              End Select
            Next
            '_angle_ (/_)
            ias = False
            nb = 0
            For i = Len(Item) To 1 Step -1
              Select Case Mid(Item, i, 1)
                Case """"
                  ias = Not ias
                Case "(", "{", "["
                  If Not ias Then nb = nb + 1
                Case ")", "}", "]"
                  If Not ias Then nb = nb - 1
                Case Chr(159)
                  If Not ias And nb = 0 Then
                    TL = Chr(&H26) & Chr(&HE3) & TL
                    stack = Mid(Item, i + 1) & Chr(0) & Left(Item, i - 1) & Chr(0) & stack
                    Item = ""
                    Exit For
                  End If
              End Select
            Next
            '_->_
            ias = False
            nb = 0
            j = 0
            For i = Len(Item) To 1 Step -1
              Select Case Mid(Item, i, 1)
                Case """"
                  ias = Not ias
                Case "("
                  If Not ias Then nb = nb + 1
                  j = j + 1
                Case "{", "["
                  If Not ias Then nb = nb + 1
                Case ")", "}", "]"
                  If Not ias Then nb = nb - 1
                Case Chr(22)
                  If Not ias And nb = 0 Then
                    TL = Chr(&H80) & TL
                    If j > 0 Then
                      locvars = locvars & Chr(1) & Chr(0)
                      funcdef = True
                    End If
                    stack = Mid(Item, i + 1) & Chr(0) & "__nofdef" & Chr(0) & Left(Item, i - 1) & Chr(0) & stack
                    Item = ""
                    Exit For
                  End If
              End Select
            Next
            '|
            ias = False
            nb = 0
            For i = Len(Item) To 1 Step -1
              Select Case Mid(Item, i, 1)
                Case """"
                  ias = Not ias
                Case "(", "{", "["
                  If Not ias Then nb = nb + 1
                Case ")", "}", "]"
                  If Not ias Then nb = nb - 1
                Case "|"
                  If Not ias And nb = 0 Then
                    TL = Chr(&H81) & TL
                    stack = Left(Item, i - 1) & Chr(0) & Mid(Item, i + 1) & Chr(0) & stack
                    Item = ""
                    Exit For
                  End If
              End Select
            Next
            '_>_
            ias = False
            nb = 0
            For i = Len(Item) To 1 Step -1
              Select Case Mid(Item, i, 1)
                Case """"
                  ias = Not ias
                Case "(", "{", "["
                  If Not ias Then nb = nb + 1
                Case ")", "}", "]"
                  If Not ias Then nb = nb - 1
                Case Chr(18)
                  If Not ias And nb = 0 Then
                    'P_>_Rx
                    If LCase89(Mid(Item, i + 1, 3)) = "rx(" Then
                      If LCase89(Right(Left(Item, i - 1), 1)) <> "p" Then
                        TL = Chr(&H5) & Chr(&HE3) & TL
                        stack = Mid(Item, i + 1) & Chr(0) & Left(Item, i - 1) & Chr(0) & stack
                        Item = ""
                        Exit For
                      End If
                      'P_>_Ry
                    ElseIf LCase89(Mid(Item, i + 1, 3)) = "ry(" Then
                      If LCase89(Right(Left(Item, i - 1), 1)) <> "p" Then
                        TL = Chr(&H5) & Chr(&HE3) & TL
                        stack = Mid(Item, i + 1) & Chr(0) & Left(Item, i - 1) & Chr(0) & stack
                        Item = ""
                        Exit For
                      End If
                      'R_>_P_theta_
                    ElseIf LCase89(Mid(Item, i + 1, 3)) = "p" & Chr(136) & "(" Then
                      If LCase89(Right(Left(Item, i - 1), 1)) <> "r" Then
                        TL = Chr(&H5) & Chr(&HE3) & TL
                        stack = Mid(Item, i + 1) & Chr(0) & Left(Item, i - 1) & Chr(0) & stack
                        Item = ""
                        Exit For
                      End If
                      'R_>_Pr
                    ElseIf LCase89(Mid(Item, i + 1, 3)) = "pr(" Then
                      If LCase89(Right(Left(Item, i - 1), 1)) <> "r" Then
                        TL = Chr(&H5) & Chr(&HE3) & TL
                        stack = Mid(Item, i + 1) & Chr(0) & Left(Item, i - 1) & Chr(0) & stack
                        Item = ""
                        Exit For
                      End If
                      'list_>_mat
                    ElseIf LCase89(Mid(Item, i + 1, 4)) = "mat(" Then
                      If LCase89(Right(Left(Item, i - 1), 4)) <> "list" Then
                        TL = Chr(&H5) & Chr(&HE3) & TL
                        stack = Mid(Item, i + 1) & Chr(0) & Left(Item, i - 1) & Chr(0) & stack
                        Item = ""
                        Exit For
                      End If
                      'mat_>_list, exp_>_list
                    ElseIf LCase89(Mid(Item, i + 1, 5)) = "list(" Then
                      If LCase89(Right(Left(Item, i - 1), 3)) <> "mat" And LCase89(Right(Left(Item, i - 1), 3)) <> "exp" Then
                        TL = Chr(&H5) & Chr(&HE3) & TL
                        stack = Mid(Item, i + 1) & Chr(0) & Left(Item, i - 1) & Chr(0) & stack
                        Item = ""
                        Exit For
                      End If
                    Else
                      Select Case LCase89(Mid(Item, i + 1))
                      Case "bin" '_>_Bin
                        TL = Chr(&H2D) & Chr(&HE3) & TL
                        stack = Left(Item, i - 1) & Chr(0) & stack
                      Case "hex" '_>_Hex
                        TL = Chr(&H2F) & Chr(&HE3) & TL
                        stack = Left(Item, i - 1) & Chr(0) & stack
                      Case "dec" '_>_Dec
                        TL = Chr(&H2E) & Chr(&HE3) & TL
                        stack = Left(Item, i - 1) & Chr(0) & stack
                      Case "dd" '_>_DD
                        TL = Chr(&H15) & Chr(&HE3) & TL
                        stack = Left(Item, i - 1) & Chr(0) & stack
                      Case "dms" '_>_DMS
                        TL = Chr(&H16) & Chr(&HE3) & TL
                        stack = Left(Item, i - 1) & Chr(0) & stack
                      Case "grad" '_>_Grad
                        TL = Chr(&H5F) & Chr(&HE3) & TL
                        stack = Left(Item, i - 1) & Chr(0) & stack
                      Case "rad" '_>_Rad
                        TL = Chr(&H60) & Chr(&HE3) & TL
                        stack = Left(Item, i - 1) & Chr(0) & stack
                      Case "rect" '_>_Rect
                        TL = Chr(&H17) & Chr(&HE3) & TL
                        stack = Left(Item, i - 1) & Chr(0) & stack
                      Case "polar" '_>_Polar
                        TL = Chr(&H18) & Chr(&HE3) & TL
                        stack = Left(Item, i - 1) & Chr(0) & stack
                      Case "cylind" '_>_Cylind
                        TL = Chr(&H19) & Chr(&HE3) & TL
                        stack = Left(Item, i - 1) & Chr(0) & stack
                      Case "sphere" '_>_Sphere
                        TL = Chr(&H1A) & Chr(&HE3) & TL
                        stack = Left(Item, i - 1) & Chr(0) & stack
                      Case Else
                        TL = Chr(&H5) & Chr(&HE3) & TL
                        stack = Mid(Item, i + 1) & Chr(0) & Left(Item, i - 1) & Chr(0) & stack
                    End Select
                    Item = ""
                    Exit For
                  End If
                End If
              End Select
            Next
            'or, xor
            ias = False
            nb = 0
            For i = Len(Item) To 1 Step -1
              Select Case Mid(Item, i, 1)
                Case """"
                  ias = Not ias
                Case "(", "{", "["
                  If Not ias Then nb = nb + 1
                Case ")", "}", "]"
                  If Not ias Then nb = nb - 1
                Case " "
                  If Not ias And nb = 0 Then
                    If LCase89(Mid(Item, i, 4)) = " or " Then
                      TL = Chr(&H83) & TL
                      stack = Left(Item, i - 1) & Chr(0) & Mid(Item, i + 4) & Chr(0) & stack
                      Item = ""
                      Exit For
                    ElseIf LCase89(Mid(Item, i, 5)) = " xor " Then
                      TL = Chr(&H82) & TL
                      stack = Left(Item, i - 1) & Chr(0) & Mid(Item, i + 5) & Chr(0) & stack
                      Item = ""
                      Exit For
                    End If
                  End If
              End Select
            Next
            'and
            ias = False
            nb = 0
            For i = Len(Item) To 1 Step -1
              Select Case Mid(Item, i, 1)
                Case """"
                  ias = Not ias
                Case "(", "{", "["
                  If Not ias Then nb = nb + 1
                Case ")", "}", "]"
                  If Not ias Then nb = nb - 1
                Case " "
                  If Not ias And nb = 0 Then
                    If LCase89(Mid(Item, i, 5)) = " and " Then
                      TL = Chr(&H84) & TL
                      stack = Left(Item, i - 1) & Chr(0) & Mid(Item, i + 5) & Chr(0) & stack
                      Item = ""
                      Exit For
                    End If
                  End If
              End Select
            Next
            'not
            If LCase89(Left(Item, 4)) = "not " Then
              TL = Chr(&H79) & TL
              stack = Mid(Item, 5) & Chr(0) & stack
              Item = ""
            End If
            '=, /=, <, <=, >, >=
            ias = False
            nb = 0
            For i = Len(Item) To 1 Step -1
              Select Case Mid(Item, i, 1)
                Case """"
                  ias = Not ias
                Case "(", "{", "["
                  If Not ias Then nb = nb + 1
                Case ")", "}", "]"
                  If Not ias Then nb = nb - 1
                Case "="
                  If Not ias And nb = 0 Then
                    If Mid(Item, i - 1, 2) = "/=" Then
                      TL = Chr(&H8A) & TL
                      stack = Left(Item, i - 2) & Chr(0) & Mid(Item, i + 1) & Chr(0) & stack
                    ElseIf Mid(Item, i - 1, 2) = ">=" Then
                      TL = Chr(&H88) & TL
                      stack = Left(Item, i - 2) & Chr(0) & Mid(Item, i + 1) & Chr(0) & stack
                    ElseIf Mid(Item, i - 1, 2) = "<=" Then
                      TL = Chr(&H86) & TL
                      stack = Left(Item, i - 2) & Chr(0) & Mid(Item, i + 1) & Chr(0) & stack
                    Else
                      TL = Chr(&H87) & TL
                      stack = Left(Item, i - 1) & Chr(0) & Mid(Item, i + 1) & Chr(0) & stack
                    End If
                    Item = ""
                    Exit For
                  End If
                Case Chr(157)
                  If Not ias And nb = 0 Then
                    TL = Chr(&H8A) & TL
                    stack = Left(Item, i - 1) & Chr(0) & Mid(Item, i + 1) & Chr(0) & stack
                    Item = ""
                    Exit For
                  End If
                Case Chr(156)
                  If Not ias And nb = 0 Then
                    TL = Chr(&H86) & TL
                    stack = Left(Item, i - 1) & Chr(0) & Mid(Item, i + 1) & Chr(0) & stack
                    Item = ""
                    Exit For
                  End If
                Case Chr(158)
                  If Not ias And nb = 0 Then
                    TL = Chr(&H88) & TL
                    stack = Left(Item, i - 1) & Chr(0) & Mid(Item, i + 1) & Chr(0) & stack
                    Item = ""
                    Exit For
                  End If
                Case "<"
                  If Not ias And nb = 0 Then
                    TL = Chr(&H85) & TL
                    stack = Left(Item, i - 1) & Chr(0) & Mid(Item, i + 1) & Chr(0) & stack
                    Item = ""
                    Exit For
                  End If
                Case ">"
                  If Not ias And nb = 0 Then
                    TL = Chr(&H89) & TL
                    stack = Left(Item, i - 1) & Chr(0) & Mid(Item, i + 1) & Chr(0) & stack
                    Item = ""
                    Exit For
                  End If
              End Select
            Next
            '+, -, .+, .-, binary _(+/-)_
            ias = False
            nb = 0
            For i = Len(Item) To 1 Step -1
              Select Case Mid(Item, i, 1)
                Case """"
                  ias = Not ias
                Case "(", "{", "["
                  If Not ias Then nb = nb + 1
                Case ")", "}", "]"
                  If Not ias Then nb = nb - 1
                Case "+"
                  If Not ias And nb = 0 Then
                    If i < 3 Then
                      TL = Chr(&H8B) & TL
                      stack = Mid(Item, i + 1) & Chr(0) & Left(Item, i - 1) & Chr(0) & stack
                    Else
                      If Mid(Item, i - 2, 3) = " .+" Then
                        TL = Chr(&H8C) & TL
                        stack = Mid(Item, i + 1) & Chr(0) & Left(Item, i - 3) & Chr(0) & stack
                      Else
                        TL = Chr(&H8B) & TL
                        stack = Mid(Item, i + 1) & Chr(0) & Left(Item, i - 1) & Chr(0) & stack
                      End If
                    End If
                    Item = ""
                    Exit For
                  End If
                Case "-"
                  If Not ias And nb = 0 Then
                    If i < 3 Then
                      TL = Chr(&H8D) & TL
                      stack = Mid(Item, i + 1) & Chr(0) & Left(Item, i - 1) & Chr(0) & stack
                    Else
                      If Mid(Item, i - 2, 3) = " .-" Then
                        TL = Chr(&H8E) & TL
                        stack = Mid(Item, i + 1) & Chr(0) & Left(Item, i - 3) & Chr(0) & stack
                      Else
                        TL = Chr(&H8D) & TL
                        stack = Mid(Item, i + 1) & Chr(0) & Left(Item, i - 1) & Chr(0) & stack
                      End If
                    End If
                    Item = ""
                    Exit For
                  End If
                Case "±"
                  If Not ias And nb = 0 And i > 1 Then
                    'detect if really binary operator
                    Select Case Right(RTrim(Left(Item, i - 1)), 1)
                    Case "+", "-", "±", "*", "/", "^"
                      'avoid Case Else
                    Case Else
                      TL = Chr(&HEB) & TL
                      stack = Mid(Item, i + 1) & Chr(0) & Left(Item, i - 1) & Chr(0) & stack
                      Item = ""
                      Exit For
                  End Select
                End If
              End Select
            Next
            '*, /, .*, ./
            ias = False
            nb = 0
            For i = Len(Item) To 1 Step -1
              Select Case Mid(Item, i, 1)
                Case """"
                  ias = Not ias
                Case "(", "{", "["
                  If Not ias Then nb = nb + 1
                Case ")", "}", "]"
                  If Not ias Then nb = nb - 1
                Case "*"
                  If Not ias And nb = 0 Then
                    If i < 3 Then
                      TL = Chr(&H8F) & TL
                      stack = Mid(Item, i + 1) & Chr(0) & Left(Item, i - 1) & Chr(0) & stack
                    Else
                      If Mid(Item, i - 2, 3) = " .*" Then
                        TL = Chr(&H90) & TL
                        stack = Mid(Item, i + 1) & Chr(0) & Left(Item, i - 3) & Chr(0) & stack
                      Else
                        TL = Chr(&H8F) & TL
                        stack = Mid(Item, i + 1) & Chr(0) & Left(Item, i - 1) & Chr(0) & stack
                      End If
                    End If
                    Item = ""
                    Exit For
                  End If
                Case "/"
                  If Not ias And nb = 0 Then
                    If i < 3 Then
                      TL = Chr(&H91) & TL
                      stack = Mid(Item, i + 1) & Chr(0) & Left(Item, i - 1) & Chr(0) & stack
                    Else
                      If Mid(Item, i - 2, 3) = " ./" Then
                        TL = Chr(&H92) & TL
                        stack = Mid(Item, i + 1) & Chr(0) & Left(Item, i - 3) & Chr(0) & stack
                      Else
                        TL = Chr(&H91) & TL
                        stack = Mid(Item, i + 1) & Chr(0) & Left(Item, i - 1) & Chr(0) & stack
                      End If
                    End If
                    Item = ""
                    Exit For
                  End If
              End Select
            Next
            '&
            ias = False
            nb = 0
            For i = 1 To Len(Item)
              Select Case Mid(Item, i, 1)
                Case """"
                  ias = Not ias
                Case "(", "{", "["
                  If Not ias Then nb = nb + 1
                Case ")", "}", "]"
                  If Not ias Then nb = nb - 1
                Case "&"
                  If Not ias And nb = 0 Then
                    TL = Chr(&H14) & Chr(&HE3) & TL
                    stack = Mid(Item, i + 1) & Chr(0) & Left(Item, i - 1) & Chr(0) & stack
                    Item = ""
                    Exit For
                  End If
              End Select
            Next
            '_negate_
            If Left(Item, 1) = Chr(173) Then
              If Trim(Mid(Item, 2)) <> Chr(190) Then
                TL = Chr(&H7A) & TL
                stack = Mid(Item, 2) & Chr(0) & stack
                Item = ""
              End If
            End If
            'unary _(+/-)_
            If Left(Item, 1) = "±" Then
              TL = Chr(&HEA) & TL
              stack = Mid(Item, 2) & Chr(0) & stack
              Item = ""
            End If
            '^, .^, _e_^
            ias = False
            nb = 0
            For i = 1 To Len(Item)
              Select Case Mid(Item, i, 1)
                Case """"
                  ias = Not ias
                Case "(", "{", "["
                  If Not ias Then nb = nb + 1
                Case ")", "}", "]"
                  If Not ias Then nb = nb - 1
                Case "^"
                  If Not ias And nb = 0 Then
                    If i < 3 Then
                      If Trim(Left(Item, i - 1)) = Chr(150) Then
                        TL = Chr(&H52) & TL
                        stack = Mid(Item, i + 1) & Chr(0) & stack
                      Else
                        TL = Chr(&H93) & TL
                        stack = Left(Item, i - 1) & Chr(0) & Mid(Item, i + 1) & Chr(0) & stack
                      End If
                    Else
                      If Mid(Item, i - 2, 3) = " .^" Then
                        TL = Chr(&H94) & TL
                        stack = Left(Item, i - 3) & Chr(0) & Mid(Item, i + 1) & Chr(0) & stack
                      ElseIf Trim(Left(Item, i - 1)) = Chr(150) Then
                        TL = Chr(&H52) & TL
                        stack = Mid(Item, i + 1) & Chr(0) & stack
                      Else
                        TL = Chr(&H93) & TL
                        stack = Left(Item, i - 1) & Chr(0) & Mid(Item, i + 1) & Chr(0) & stack
                      End If
                    End If
                    Item = ""
                    Exit For
                  End If
              End Select
            Next
            '° (converted from °, ', ")
            If Right(Item, 1) = "°" Then
              TL = Chr(&HCD) & TL
              ias = False
              nb = 0
              For i = Len(Item) - 1 To 1 Step -1
                Select Case Mid(Item, i, 1)
                  Case """"
                    ias = Not ias
                  Case "(", "{", "["
                    If Not ias Then nb = nb + 1
                  Case ")", "}", "]"
                    If Not ias Then nb = nb - 1
                  Case "°"
                    If Not ias And nb = 0 Then
                      For j = i - 1 To 1 Step -1
                        Select Case Mid(Item, j, 1)
                        Case """"
                          ias = Not ias
                        Case "(", "{", "["
                          If Not ias Then nb = nb + 1
                        Case ")", "}", "]"
                          If Not ias Then nb = nb - 1
                        Case "°"
                          If Not ias And nb = 0 Then
                            stack = Left(Item, j - 1) & Chr(0) & Mid(Item, j + 1, i - j - 1) & Chr(0) & Mid(Item, i + 1, Len(Item) - i - 1) & Chr(0) & "___E5" & Chr(0) & stack
                            Item = ""
                            Exit For
                          End If
                      End Select
                    Next
                    If Item <> "" Then
                      stack = Left(Item, i - 1) & Chr(0) & Mid(Item, i + 1, Len(Item) - i - 1) & Chr(0) & "___E5" & Chr(0) & stack
                      Item = ""
                      Exit For
                    End If
                  End If
                End Select
              Next
              If Item <> "" Then
                stack = Left(Item, Len(Item) - 1) & Chr(0) & "___E5" & Chr(0) & stack
                Item = ""
              End If
            End If
            '!
            If Right(Item, 1) = "!" Then
              TL = Chr(&H76) & TL
              stack = Left(Item, Len(Item) - 1) & Chr(0) & stack
              Item = ""
            End If
            '%
            If Right(Item, 1) = "%" Then
              TL = Chr(&H77) & TL
              stack = Left(Item, Len(Item) - 1) & Chr(0) & stack
              Item = ""
            End If
            '_radians_
            If Right(Item, 1) = Chr(152) Then
              TL = Chr(&H78) & TL
              stack = Left(Item, Len(Item) - 1) & Chr(0) & stack
              Item = ""
            End If
            '_transpose_
            If Right(Item, 1) = Chr(153) Then
              TL = Chr(&H75) & TL
              stack = Left(Item, Len(Item) - 1) & Chr(0) & stack
              Item = ""
            End If
            ' ' prime
            If Right(Item, 1) = "'" Then
              TL = Chr(&HEF) & TL
              stack = Left(Item, Len(Item) - 1) & Chr(0) & stack
              Item = ""
            End If
            '[] array subscript
            If Right(Item, 1) = "]" Then
              ias = False
              nb = 0
              Item = Left(Item, Len(Item) - 1)
              For i = 1 To Len(Item)
                Select Case Mid(Item, i, 1)
                  Case """"
                    ias = Not ias
                  Case "(", "{"
                    If Not ias Then nb = nb + 1
                  Case ")", "}"
                    If Not ias Then nb = nb - 1
                  Case "["
                    If Not ias And nb = 0 Then
                      For j = i + 1 To Len(Item)
                        Select Case Mid(Item, j, 1)
                        Case """"
                          ias = Not ias
                        Case "(", "{", "["
                          If Not ias Then nb = nb + 1
                        Case ")", "}", "]"
                          If Not ias Then nb = nb - 1
                        Case ","
                          If Not ias And nb = 0 Then
                            TL = Chr(&HD5) & TL
                            stack = Left(Item, i - 1) & Chr(0) & Mid(Item, i + 1, j - i - 1) & Chr(0) & Mid(Item, j + 1) & Chr(0) & "___E5" & Chr(0) & stack
                            Item = ""
                            Exit For
                          End If
                      End Select
                    Next
                    If Item <> "" Then
                      TL = Chr(&HD5) & TL
                      stack = Left(Item, i - 1) & Chr(0) & Mid(Item, i + 1) & Chr(0) & "___E5" & Chr(0) & stack
                      Item = ""
                    End If
                    Exit For
                  End If
                End Select
              Next
            End If
            
            'TRICK TO BREAK 64K LIMIT:
            'CALL SUBPROCEDURE PASSING ALL ARGUMENTS AND
            'LOCAL VARIABLES "BYREF" TO TRICK IT INTO
            'BEING ABLE TO USE THEM IN THE SAME WAY AS
            'IF IT WAS 1 PROCEDURE
            
            Token_3 S, FType, Tokenize, L2, L, TL, c, ias, Item, stack, nb, nb2, useless, i, j, dms, vn, nr, brex, Temp, iarg, narg, locvars, loccmd, PV, AddrStack, ExitStack, CycleStack, Token, funcdef, poplocalateol
            
            'CODE CONTINUES HERE
            
          End If
        End If
      Wend
      Token = TL & Token
    End If
    If S <> "" Then
      Select Case c
        Case ":"
          i = 0
          While Mid(S, 2, 1) = " "
            i = i + 1
            S = Left(S, 1) & Mid(S, 3)
          Wend
          Token = Chr(i) & Chr(&HE7) & Token
          If poplocalateol Then
            j = 0
            For i = Len(locvars) To 1 Step -1
              If Asc(Mid(locvars, i, 1)) = 1 Then
                j = i
                Exit For
              End If
            Next
            If j <> 0 Then
              locvars = Left(locvars, j - 1)
            Else
              locvars = Chr(0)
            End If
            poplocalateol = False
          End If
        Case Chr(13)
          i = 0
          While Mid(S, 2, 1) = " " And i < 255
            i = i + 1
            S = Left(S, 1) & Mid(S, 3)
          Wend
          Token = Chr(i) & Chr(&HE8) & Token
          If poplocalateol Then
            j = 0
            For i = Len(locvars) To 1 Step -1
              If Asc(Mid(locvars, i, 1)) = 1 Then
                j = i
                Exit For
              End If
            Next
            If j <> 0 Then
              locvars = Left(locvars, j - 1)
            Else
              locvars = Chr(0)
            End If
            poplocalateol = False
          End If
        Case Chr(0)
          Token = Chr(&HE9) & Token
      End Select
      S = Mid(S, 2)
    End If
  Wend
  ' Close #1
End Function

Private Sub Token_2(S$, FType As FTypes, Tokenize As Boolean, L2$, L$, TL$, c$, ias As Boolean, Item$, stack$, nb&, nb2&, useless As Boolean, i&, j&, dms As Boolean, vn As Boolean, nr As Boolean, brex As Boolean, Temp$, iarg&, narg&, locvars$, loccmd As Boolean, PV&, AddrStack$, ExitStack$, CycleStack$, Token$, funcdef As Boolean, poplocalateol As Boolean)
  'THIS SUBROUTINE IS NECESSARY TO BREAK VB'S
  '64K LIMIT!
  'READ IT AS IF IT WAS INSERTED DIRECTLY
  'INSTEAD OF THE FUNCTION CALL IN TOKEN.
  
  'instructions without arguments
  Select Case LCase89(Item)
    Case "__nofdef"
      funcdef = False
      Item = ""
    Case "clrdraw"
      TL = Chr(1) & Chr(&HE4) & TL
      Item = ""
    Case "clrgraph"
      TL = Chr(2) & Chr(&HE4) & TL
      Item = ""
    Case "clrhome"
      TL = Chr(3) & Chr(&HE4) & TL
      Item = ""
    Case "clrio"
      TL = Chr(4) & Chr(&HE4) & TL
      Item = ""
    Case "clrtable"
      TL = Chr(5) & Chr(&HE4) & TL
      Item = ""
    Case "custom"
      TL = Chr(6) & Chr(&HE4) & TL
      Item = ""
    Case "cycle"
      TL = Chr(0) & Chr(0) & Chr(7) & Chr(&HE4) & TL
      CycleStack = ChrW(Len(Token) + Len(TL)) & CycleStack
      Item = ""
    Case "dialog"
      TL = Chr(8) & Chr(&HE4) & TL
      Item = ""
    Case "dispg"
      TL = Chr(9) & Chr(&HE4) & TL
      Item = ""
    Case "disptbl"
      TL = Chr(&HA) & Chr(&HE4) & TL
      Item = ""
    Case "else"
      If AscW(Left(AddrStack, 1)) = -2 Then
        '"Else" in "Try" block
        TL = Chr(&H87) & Chr(&HE4) & TL
      Else
        '"Else" in "If" block
        TL = Chr(&HB) & Chr(&HE4) & TL
      End If
      Item = ""
    Case "endcustm"
      TL = Chr(&HC) & Chr(&HE4) & TL
      Item = ""
    Case "enddlog"
      TL = Chr(&HD) & Chr(&HE4) & TL
      Item = ""
    Case "endfor"
      TL = Chr(&HE) & Chr(&HE4) & TL
      i = AscW(Left(AddrStack, 1))
      If i < 0 Then i = i + 65536
      i = Len(Token) + Len(TL) - i
      AddrStack = Mid(AddrStack, 2)
      TL = Chr(i Mod 256) & Chr(i \ 256) & TL
      While AscW(Left(ExitStack, 1)) <> -1
        i = AscW(Left(ExitStack, 1))
        If i < 0 Then i = i + 65536
        j = Len(Token) + Len(TL) - i + 2
        Mid(Token, Len(Token) - i + 1, 1) = Chr(j Mod 256)
        Mid(Token, Len(Token) - i + 2, 1) = Chr(j \ 256)
        ExitStack = Mid(ExitStack, 2)
      Wend
      ExitStack = Mid(ExitStack, 2)
      While AscW(Left(CycleStack, 1)) <> -1
        i = AscW(Left(CycleStack, 1))
        If i < 0 Then i = i + 65536
        j = Len(Token) + Len(TL) - i - 2
        Mid(Token, Len(Token) - i + 1, 1) = Chr(j Mod 256)
        Mid(Token, Len(Token) - i + 2, 1) = Chr(j \ 256)
        CycleStack = Mid(CycleStack, 2)
      Wend
      CycleStack = Mid(CycleStack, 2)
      Item = ""
    Case "endfunc"
      TL = Chr(&HF) & Chr(&HE4) & TL
      Item = ""
      j = 0
      For i = Len(locvars) To 1 Step -1
        If Asc(Mid(locvars, i, 1)) = 1 Then
          j = i
          Exit For
        End If
      Next
      If j <> 0 Then
        locvars = Left(locvars, j - 1)
      Else
        locvars = Chr(0)
      End If
      
    Case "endif"
      TL = Chr(&H10) & Chr(&HE4) & TL
      AddrStack = Mid(AddrStack, 2)
      Item = ""
    Case "endloop"
      TL = Chr(&H11) & Chr(&HE4) & TL
      i = AscW(Left(AddrStack, 1))
      If i < 0 Then i = i + 65536
      i = Len(Token) + Len(TL) - i
      AddrStack = Mid(AddrStack, 2)
      TL = Chr(i Mod 256) & Chr(i \ 256) & TL
      While AscW(Left(ExitStack, 1)) <> -1
        i = AscW(Left(ExitStack, 1))
        If i < 0 Then i = i + 65536
        j = Len(Token) + Len(TL) - i + 2
        Mid(Token, Len(Token) - i + 1, 1) = Chr(j Mod 256)
        Mid(Token, Len(Token) - i + 2, 1) = Chr(j \ 256)
        ExitStack = Mid(ExitStack, 2)
      Wend
      ExitStack = Mid(ExitStack, 2)
      While AscW(Left(CycleStack, 1)) <> -1
        i = AscW(Left(CycleStack, 1))
        If i < 0 Then i = i + 65536
        j = Len(Token) + Len(TL) - i - 2
        Mid(Token, Len(Token) - i + 1, 1) = Chr(j Mod 256)
        Mid(Token, Len(Token) - i + 2, 1) = Chr(j \ 256)
        CycleStack = Mid(CycleStack, 2)
      Wend
      CycleStack = Mid(CycleStack, 2)
      Item = ""
    Case "endprgm"
      TL = Chr(&H12) & Chr(&HE4) & TL
      Item = ""
      j = 0
      For i = Len(locvars) To 1 Step -1
        If Asc(Mid(locvars, i, 1)) = 1 Then
          j = i
          Exit For
        End If
      Next
      If j <> 0 Then
        locvars = Left(locvars, j - 1)
      Else
        locvars = Chr(0)
      End If
    Case "endtbar"
      TL = Chr(&H13) & Chr(&HE4) & TL
      Item = ""
    Case "endtry"
      TL = Chr(&H14) & Chr(&HE4) & TL
      AddrStack = Mid(AddrStack, 2)
      Item = ""
    Case "endwhile"
      TL = Chr(&H15) & Chr(&HE4) & TL
      i = AscW(Left(AddrStack, 1))
      If i < 0 Then i = i + 65536
      i = Len(Token) + Len(TL) - i
      AddrStack = Mid(AddrStack, 2)
      TL = Chr(i Mod 256) & Chr(i \ 256) & TL
      While AscW(Left(ExitStack, 1)) <> -1
        i = AscW(Left(ExitStack, 1))
        If i < 0 Then i = i + 65536
        j = Len(Token) + Len(TL) - i + 2
        Mid(Token, Len(Token) - i + 1, 1) = Chr(j Mod 256)
        Mid(Token, Len(Token) - i + 2, 1) = Chr(j \ 256)
        ExitStack = Mid(ExitStack, 2)
      Wend
      ExitStack = Mid(ExitStack, 2)
      While AscW(Left(CycleStack, 1)) <> -1
        i = AscW(Left(CycleStack, 1))
        If i < 0 Then i = i + 65536
        j = Len(Token) + Len(TL) - i - 2
        Mid(Token, Len(Token) - i + 1, 1) = Chr(j Mod 256)
        Mid(Token, Len(Token) - i + 2, 1) = Chr(j \ 256)
        CycleStack = Mid(CycleStack, 2)
      Wend
      CycleStack = Mid(CycleStack, 2)
      Item = ""
    Case "exit"
      TL = Chr(0) & Chr(0) & Chr(&H16) & Chr(&HE4) & TL
      ExitStack = ChrW(Len(Token) + Len(TL)) & ExitStack
      Item = ""
    Case "func"
      TL = Chr(&H17) & Chr(&HE4) & TL
      Item = ""
      poplocalateol = False
    Case "loop"
      AddrStack = ChrW(Len(Token) + Len(TL)) & AddrStack
      ExitStack = ChrW(-1) & ExitStack
      CycleStack = ChrW(-1) & CycleStack
      TL = Chr(&H18) & Chr(&HE4) & TL
      Item = ""
    Case "prgm"
      TL = Chr(&H19) & Chr(&HE4) & TL
      Item = ""
      poplocalateol = False
    Case "showstat"
      TL = Chr(&H1A) & Chr(&HE4) & TL
      Item = ""
    Case "stop"
      TL = Chr(&H1B) & Chr(&HE4) & TL
      Item = ""
    Case "then"
      AddrStack = ChrW(-1) & AddrStack
      TL = Chr(&H1C) & Chr(&HE4) & TL
      Item = ""
    Case "toolbar"
      TL = Chr(&H1D) & Chr(&HE4) & TL
      Item = ""
    Case "trace"
      TL = Chr(&H1E) & Chr(&HE4) & TL
      Item = ""
    Case "try"
      AddrStack = ChrW(-2) & AddrStack
      TL = Chr(&H1F) & Chr(&HE4) & TL
      Item = ""
    Case "zoombox"
      TL = Chr(&H20) & Chr(&HE4) & TL
      Item = ""
    Case "zoomdata"
      TL = Chr(&H21) & Chr(&HE4) & TL
      Item = ""
    Case "zoomdec"
      TL = Chr(&H22) & Chr(&HE4) & TL
      Item = ""
    Case "zoomfit"
      TL = Chr(&H23) & Chr(&HE4) & TL
      Item = ""
    Case "zoomin"
      TL = Chr(&H24) & Chr(&HE4) & TL
      Item = ""
    Case "zoomint"
      TL = Chr(&H25) & Chr(&HE4) & TL
      Item = ""
    Case "zoomout"
      TL = Chr(&H26) & Chr(&HE4) & TL
      Item = ""
    Case "zoomprev"
      TL = Chr(&H27) & Chr(&HE4) & TL
      Item = ""
    Case "zoomrcl"
      TL = Chr(&H28) & Chr(&HE4) & TL
      Item = ""
    Case "zoomsqr"
      TL = Chr(&H29) & Chr(&HE4) & TL
      Item = ""
    Case "zoomstd"
      TL = Chr(&H2A) & Chr(&HE4) & TL
      Item = ""
    Case "zoomsto"
      TL = Chr(&H2B) & Chr(&HE4) & TL
      Item = ""
    Case "zoomtrig"
      TL = Chr(&H2C) & Chr(&HE4) & TL
      Item = ""
    Case "pause"
      TL = Chr(&HE5) & Chr(&H51) & Chr(&HE4) & TL
      Item = ""
    Case "return"
      TL = Chr(&HE5) & Chr(&H52) & Chr(&HE4) & TL
      Item = ""
    Case "input"
      TL = Chr(&HE5) & Chr(&H53) & Chr(&HE4) & TL
      Item = ""
    Case "plotsoff"
      TL = Chr(&HE5) & Chr(&H54) & Chr(&HE4) & TL
      Item = ""
    Case "plotson"
      TL = Chr(&HE5) & Chr(&H55) & Chr(&HE4) & TL
      Item = ""
    Case "disp"
      TL = Chr(&HE5) & Chr(&H7A) & Chr(&HE4) & TL
      Item = ""
    Case "fnoff"
      TL = Chr(&HE5) & Chr(&H7B) & Chr(&HE4) & TL
      Item = ""
    Case "fnon"
      TL = Chr(&HE5) & Chr(&H7C) & Chr(&HE4) & TL
      Item = ""
    Case "clrerr"
      TL = Chr(&H88) & Chr(&HE4) & TL
      Item = ""
    Case "passerr"
      TL = Chr(&H89) & Chr(&HE4) & TL
      Item = ""
    Case "disphome"
      TL = Chr(&H8A) & Chr(&HE4) & TL
      Item = ""
    Case "newprob"
      TL = Chr(&H92) & Chr(&HE4) & TL
      Item = ""
    Case "custmon"
      TL = Chr(&H95) & Chr(&HE4) & TL
      Item = ""
    Case "custmoff"
      TL = Chr(&H96) & Chr(&HE4) & TL
      Item = ""
    Case "clockon" '(AMS 2.07)
      TL = Chr(&H9A) & Chr(&HE4) & TL
      Item = ""
    Case "clockoff" '(AMS 2.07)
      TL = Chr(&H9B) & Chr(&HE4) & TL
      Item = ""
  End Select
  'instructions with arguments
  If InStr(Item, " ") <> 0 Then
    Select Case LCase89(Left(Item, InStr(Item, " ") - 1))
      Case "drawfunc"
        TL = Chr(&H2D) & Chr(&HE4) & TL
        narg = 1
      Case "drawinv"
        TL = Chr(&H2E) & Chr(&HE4) & TL
        narg = 1
      Case "goto"
        TL = Chr(&H2F) & Chr(&HE4) & TL
        narg = 1
      Case "lbl"
        TL = Chr(&H30) & Chr(&HE4) & TL
        narg = 1
      Case "get"
        TL = Chr(&H31) & Chr(&HE4) & TL
        narg = 1
      Case "send"
        TL = Chr(&H32) & Chr(&HE4) & TL
        narg = 1
      Case "getcalc"
        TL = Chr(&H33) & Chr(&HE4) & TL
        narg = 1
      Case "sendcalc"
        TL = Chr(&H34) & Chr(&HE4) & TL
        narg = 1
      Case "newfold"
        TL = Chr(&H35) & Chr(&HE4) & TL
        narg = 1
      Case "printobj"
        TL = Chr(&H36) & Chr(&HE4) & TL
        narg = 1
      Case "rclgdb"
        TL = Chr(&H37) & Chr(&HE4) & TL
        narg = 1
      Case "stogdb"
        TL = Chr(&H38) & Chr(&HE4) & TL
        narg = 1
      Case "elseif"
        If LCase89(Right(Item, 5)) = " then" Then
        Item = Left(Item, Len(Item) - 5)
      End If
      TL = Chr(&H39) & Chr(&HE4) & TL
      narg = 1
    Case "if"
      If LCase89(Right(Item, 5)) = " then" Then
      AddrStack = ChrW(-1) & AddrStack
      TL = Chr(&H3B) & Chr(&HE4) & TL
      Item = Left(Item, Len(Item) - 5)
    Else
      TL = Chr(&H3A) & Chr(&HE4) & TL
    End If
    narg = 1
  Case "randseed"
    TL = Chr(&H3C) & Chr(&HE4) & TL
    narg = 1
  Case "while"
    AddrStack = ChrW(Len(Token) + Len(TL)) & AddrStack
    ExitStack = ChrW(-1) & ExitStack
    CycleStack = ChrW(-1) & CycleStack
    TL = Chr(&H3D) & Chr(&HE4) & TL
    narg = 1
  Case "linetan"
    TL = Chr(&H3E) & Chr(&HE4) & TL
    narg = 2
  Case "copyvar"
    TL = Chr(&H3F) & Chr(&HE4) & TL
    narg = 2
  Case "rename"
    TL = Chr(&H40) & Chr(&HE4) & TL
    narg = 2
  Case "style"
    TL = Chr(&H41) & Chr(&HE4) & TL
    narg = 2
  Case "fill"
    TL = Chr(&H42) & Chr(&HE4) & TL
    narg = 2
  Case "request"
    nb = 0
    ias = False
    narg = 1
    For i = 9 To Len(Item) - 1
      Select Case Mid(Item, i, 1)
      Case """"
        ias = Not ias
      Case "("
        If Not ias Then nb = nb + 1
      Case ")"
        If Not ias Then nb = nb - 1
      Case ","
        If Not ias And nb = 0 Then
          narg = narg + 1
        End If
    End Select
  Next
  If narg = 2 Then
    TL = Chr(&H43) & Chr(&HE4) & TL
  Else '(AMS 2.07)
    TL = Chr(&H99) & Chr(&HE4) & TL
    narg = -1
  End If
  Case "popup"
    TL = Chr(&H44) & Chr(&HE4) & TL
    narg = 2
  Case "ptchg"
    TL = Chr(&H45) & Chr(&HE4) & TL
    narg = 2
  Case "ptoff"
    TL = Chr(&H46) & Chr(&HE4) & TL
    narg = 2
  Case "pton"
    TL = Chr(&H47) & Chr(&HE4) & TL
    narg = 2
  Case "pxlchg"
    TL = Chr(&H48) & Chr(&HE4) & TL
    narg = 2
  Case "pxloff"
    TL = Chr(&H49) & Chr(&HE4) & TL
    narg = 2
  Case "pxlon"
    TL = Chr(&H4A) & Chr(&HE4) & TL
    narg = 2
  Case "movevar"
    TL = Chr(&H4B) & Chr(&HE4) & TL
    narg = 3
  Case "dropdown"
    TL = Chr(&H4C) & Chr(&HE4) & TL
    narg = 3
  Case "output"
    TL = Chr(&H4D) & Chr(&HE4) & TL
    narg = 3
  Case "pttext"
    TL = Chr(&H4E) & Chr(&HE4) & TL
    narg = 3
  Case "pxltext"
    TL = Chr(&H4F) & Chr(&HE4) & TL
    narg = 3
  Case "drawslp"
    TL = Chr(&H50) & Chr(&HE4) & TL
    narg = 3
  Case "pause"
    TL = Chr(&H51) & Chr(&HE4) & TL
    narg = -1
  Case "return"
    TL = Chr(&H52) & Chr(&HE4) & TL
    narg = -1
  Case "input"
    TL = Chr(&H53) & Chr(&HE4) & TL
    narg = -1
  Case "plotsoff"
    TL = Chr(&H54) & Chr(&HE4) & TL
    narg = -1
  Case "plotson"
    TL = Chr(&H55) & Chr(&HE4) & TL
    narg = -1
  Case "title"
    TL = Chr(&H56) & Chr(&HE4) & TL
    narg = -1
  Case "item"
    TL = Chr(&H57) & Chr(&HE4) & TL
    narg = -1
  Case "inputstr"
    TL = Chr(&H58) & Chr(&HE4) & TL
    narg = -1
  Case "linehorz"
    TL = Chr(&H59) & Chr(&HE4) & TL
    narg = -1
  Case "linevert"
    TL = Chr(&H5A) & Chr(&HE4) & TL
    narg = -1
  Case "pxlhorz"
    TL = Chr(&H5B) & Chr(&HE4) & TL
    narg = -1
  Case "pxlvert"
    TL = Chr(&H5C) & Chr(&HE4) & TL
    narg = -1
  Case "andpic"
    TL = Chr(&H5D) & Chr(&HE4) & TL
    narg = -1
  Case "rclpic"
    TL = Chr(&H5E) & Chr(&HE4) & TL
    narg = -1
  Case "rplcpic"
    TL = Chr(&H5F) & Chr(&HE4) & TL
    narg = -1
  Case "xorpic"
    TL = Chr(&H60) & Chr(&HE4) & TL
    narg = -1
  Case "drawpol"
    TL = Chr(&H61) & Chr(&HE4) & TL
    narg = -1
  Case "text"
    TL = Chr(&H62) & Chr(&HE4) & TL
    narg = -1
  Case "onevar"
    TL = Chr(&H63) & Chr(&HE4) & TL
    narg = -1
  Case "stopic"
    TL = Chr(&H64) & Chr(&HE4) & TL
    narg = -1
  Case "graph"
    TL = Chr(&H65) & Chr(&HE4) & TL
    narg = -1
  Case "table"
    TL = Chr(&H66) & Chr(&HE4) & TL
    narg = -1
  Case "newpic"
    TL = Chr(&H67) & Chr(&HE4) & TL
    narg = -1
  Case "drawparm"
    TL = Chr(&H68) & Chr(&HE4) & TL
    narg = -1
  Case "cyclepic"
    TL = Chr(&H69) & Chr(&HE4) & TL
    narg = -1
  Case "cubicreg"
    TL = Chr(&H6A) & Chr(&HE4) & TL
    narg = -1
  Case "expreg"
    TL = Chr(&H6B) & Chr(&HE4) & TL
    narg = -1
  Case "linreg"
    TL = Chr(&H6C) & Chr(&HE4) & TL
    narg = -1
  Case "lnreg"
    TL = Chr(&H6D) & Chr(&HE4) & TL
    narg = -1
  Case "medmed"
    TL = Chr(&H6E) & Chr(&HE4) & TL
    narg = -1
  Case "powerreg"
    TL = Chr(&H6F) & Chr(&HE4) & TL
    narg = -1
  Case "quadreg"
    TL = Chr(&H70) & Chr(&HE4) & TL
    narg = -1
  Case "quartreg"
    TL = Chr(&H71) & Chr(&HE4) & TL
    narg = -1
  Case "twovar"
    TL = Chr(&H72) & Chr(&HE4) & TL
    narg = -1
  Case "shade"
    TL = Chr(&H73) & Chr(&HE4) & TL
    narg = -1
  Case "for"
    AddrStack = ChrW(Len(Token) + Len(TL)) & AddrStack
    ExitStack = ChrW(-1) & ExitStack
    CycleStack = ChrW(-1) & CycleStack
    TL = Chr(&H74) & Chr(&HE4) & TL
    narg = -1
  Case "circle"
    TL = Chr(&H75) & Chr(&HE4) & TL
    narg = -1
  Case "pxlcrcl"
    TL = Chr(&H76) & Chr(&HE4) & TL
    narg = -1
  Case "newplot"
    TL = Chr(&H77) & Chr(&HE4) & TL
    narg = -1
  Case "line"
    TL = Chr(&H78) & Chr(&HE4) & TL
    narg = -1
  Case "pxlline"
    TL = Chr(&H79) & Chr(&HE4) & TL
    narg = -1
  Case "disp"
    TL = Chr(&H7A) & Chr(&HE4) & TL
    narg = -1
  Case "fnoff"
    TL = Chr(&H7B) & Chr(&HE4) & TL
    narg = -1
  Case "fnon"
    TL = Chr(&H7C) & Chr(&HE4) & TL
    narg = -1
  Case "local"
    TL = Chr(&H7D) & Chr(&HE4) & TL
    narg = -1
    loccmd = True
  Case "delfold"
    TL = Chr(&H7E) & Chr(&HE4) & TL
    narg = -1
  Case "delvar"
    TL = Chr(&H7F) & Chr(&HE4) & TL
    narg = -1
  Case "lock"
    TL = Chr(&H80) & Chr(&HE4) & TL
    narg = -1
  Case "prompt"
    TL = Chr(&H81) & Chr(&HE4) & TL
    narg = -1
  Case "sorta"
    TL = Chr(&H82) & Chr(&HE4) & TL
    narg = -1
  Case "sortd"
    TL = Chr(&H83) & Chr(&HE4) & TL
    narg = -1
  Case "unlock"
    TL = Chr(&H84) & Chr(&HE4) & TL
    narg = -1
  Case "newdata"
    TL = Chr(&H85) & Chr(&HE4) & TL
    narg = -1
  Case "define"
    locvars = locvars & Chr(1) & Chr(0)
    ias = False
    nb = 0
    For i = 8 To Len(Item)
      Select Case Mid(Item, i, 1)
      Case """"
        ias = Not ias
      Case "(", "{", "["
        If Not ias Then nb = nb + 1
      Case ")", "}", "]"
        If Not ias Then nb = nb - 1
      Case "="
        If Not ias And nb = 0 Then
          TL = Chr(&H86) & Chr(&HE4) & TL
          funcdef = True
          stack = Mid(Item, 8, i - 8) & Chr(0) & "__nofdef" & Chr(0) & Mid(Item, i + 1) & Chr(0) & stack
          Item = ""
          Exit For
        End If
    End Select
  Next
  narg = -2
  Case "exec"
    TL = Chr(&H8B) & Chr(&HE4) & TL
    narg = -1
  Case "archive"
    TL = Chr(&H8C) & Chr(&HE4) & TL
    narg = -1
  Case "unarchiv"
    TL = Chr(&H8D) & Chr(&HE4) & TL
    narg = -1
  Case "lu"
    TL = Chr(&H8E) & Chr(&HE4) & TL
    narg = -1
  Case "qr"
    TL = Chr(&H8F) & Chr(&HE4) & TL
    narg = -1
  Case "blddata"
    TL = Chr(&H90) & Chr(&HE4) & TL
    narg = 1
  Case "drwctour"
    TL = Chr(&H91) & Chr(&HE4) & TL
    narg = 1
  Case "sinreg"
    TL = Chr(&H93) & Chr(&HE4) & TL
    narg = -1
  Case "logistic"
    TL = Chr(&H94) & Chr(&HE4) & TL
    narg = -1
  Case "sendchat"
    TL = Chr(&H97) & Chr(&HE4) & TL
    narg = 1
  Case Else
    narg = -2
End Select
If narg > -2 Then
  j = Len(Item)
  iarg = 0
  Temp = ""
  ias = False
  nb = 0
  For i = Len(Item) To InStr(Item, " ") + 1 Step -1
    Select Case Mid(Item, i, 1)
      Case """"
        ias = Not ias
      Case "(", "{", "["
        If Not ias Then nb = nb + 1
      Case ")", "}", "]"
        If Not ias Then nb = nb - 1
      Case ",", ";"
        If Not ias And nb = 0 Then
          Temp = Mid(Item, i + 1, j - i) & Chr(0) & Temp
          j = i - 1
          iarg = iarg + 1
          If iarg = narg Then Exit For
        End If
    End Select
  Next
  If iarg < narg Or narg = -1 Then
    Temp = Mid(Item, i + 1, j - i) & Chr(0) & Temp
    iarg = iarg + 1
  End If
  If narg = -1 Then
    stack = Temp & "___E5" & Chr(0) & stack
  Else
    If iarg < narg Then
      For i = iarg + 1 To narg
        Temp = Temp & "___2E" & Chr(0)
      Next
    End If
    stack = Temp & stack
  End If
  Item = ""
End If
End If
End Sub

Private Sub Token_3(S$, FType As FTypes, Tokenize As Boolean, L2$, L$, TL$, c$, ias As Boolean, Item$, stack$, nb&, nb2&, useless As Boolean, i&, j&, dms As Boolean, vn As Boolean, nr As Boolean, brex As Boolean, Temp$, iarg&, narg&, locvars$, loccmd As Boolean, PV&, AddrStack$, ExitStack$, CycleStack$, Token$, funcdef As Boolean, poplocalateol As Boolean)
  'THIS SUBROUTINE IS NECESSARY TO BREAK VB'S
  '64K LIMIT!
  'READ IT AS IF IT WAS INSERTED DIRECTLY
  'INSTEAD OF THE FUNCTION CALL IN TOKEN.
  Dim k As Integer, ibrace As Boolean
  
  'strings
  If Left(Item, 1) = """" And Right(Item, 1) = """" Then
    TL = Chr(0) & Replace(Mid(Item, 2, Len(Item) - 2), """""", """") & Chr(0) & Chr(&H2D) & TL
    Item = ""
  End If
  
  'look for function call (including __args)
  'push all arguments to stack
  'push END_TAG if function requires it
  'push NOTHING for missing arguments
  If InStr(Item, "(") <> 0 Then
    Select Case LCase89(Left(Item, InStr(Item, "(") - 1))
      Case "__args"
        TL = Chr(0) & Chr(0) & Chr(0) & Chr(&HDC) & TL
        c = "a"
        loccmd = True
        narg = -1
      Case "#"
        'indirection with parentheses
        TL = Chr(1) & Chr(&HE3) & TL
        narg = 1
      Case "cosh" & Chr(180) 'cosh_^-1_
        TL = Chr(&H2F) & TL
        narg = 1
      Case "sinh" & Chr(180) 'sinh_^-1_
        TL = Chr(&H30) & TL
        narg = 1
      Case "tanh" & Chr(180) 'tanh_^-1_
        TL = Chr(&H31) & TL
        narg = 1
      Case "sech" & Chr(180) 'sech_^-1_ (AMS 2.08)
        TL = Chr(&H32) & TL
        narg = 1
      Case "csch" & Chr(180) 'csch_^-1_ (AMS 2.08)
        TL = Chr(&H33) & TL
        narg = 1
      Case "coth" & Chr(180) 'coth_^-1_ (AMS 2.08)
        TL = Chr(&H34) & TL
        narg = 1
      Case "cosh"
        TL = Chr(&H35) & TL
        narg = 1
      Case "sinh"
        TL = Chr(&H36) & TL
        narg = 1
      Case "tanh"
        TL = Chr(&H37) & TL
        narg = 1
      Case "sech" '(AMS 2.08)
        TL = Chr(&H38) & TL
        narg = 1
      Case "csch" '(AMS 2.08)
        TL = Chr(&H39) & TL
        narg = 1
      Case "coth" '(AMS 2.08)
        TL = Chr(&H3A) & TL
        narg = 1
      Case "cos" & Chr(180) 'cos_^-1_
        TL = Chr(&H3B) & TL
        narg = 1
      Case "sin" & Chr(180) 'sin_^-1_
        TL = Chr(&H3C) & TL
        narg = 1
      Case "tan" & Chr(180) 'tan_^-1_
        TL = Chr(&H3D) & TL
        narg = 1
      Case "sec" & Chr(180) 'sec_^-1_ (AMS 2.08)
        TL = Chr(&H3E) & TL
        narg = 1
      Case "csc" & Chr(180) 'csc_^-1_ (AMS 2.08)
        TL = Chr(&H3F) & TL
        narg = 1
      Case "cot" & Chr(180) 'cot_^-1_ (AMS 2.08)
        TL = Chr(&H40) & TL
        narg = 1
      Case "cos"
        TL = Chr(&H44) & TL
        narg = 1
      Case "sin"
        TL = Chr(&H45) & TL
        narg = 1
      Case "tan"
        TL = Chr(&H46) & TL
        narg = 1
      Case "sec" '(AMS 2.08)
        TL = Chr(&H47) & TL
        narg = 1
      Case "csc" '(AMS 2.08)
        TL = Chr(&H48) & TL
        narg = 1
      Case "cot" '(AMS 2.08)
        TL = Chr(&H49) & TL
        narg = 1
      Case "abs"
        TL = Chr(&H4B) & TL
        narg = 1
      Case "angle"
        TL = Chr(&H4C) & TL
        narg = 1
      Case "ceiling"
        TL = Chr(&H4D) & TL
        narg = 1
      Case "floor"
        TL = Chr(&H4E) & TL
        narg = 1
      Case "int"
        TL = Chr(&H4F) & TL
        narg = 1
      Case "sign"
        TL = Chr(&H50) & TL
        narg = 1
      Case Chr(168) '_sqrt_
        TL = Chr(&H51) & TL
        narg = 1
      Case "ln"
        TL = Chr(&H53) & TL
        narg = 1
      Case "log"
        TL = Chr(&H54) & TL
        narg = 1
      Case "fpart"
        TL = Chr(&H55) & TL
        narg = 1
      Case "ipart"
        TL = Chr(&H56) & TL
        narg = 1
      Case "conj"
        TL = Chr(&H57) & TL
        narg = 1
      Case "imag"
        TL = Chr(&H58) & TL
        narg = 1
      Case "real"
        TL = Chr(&H59) & TL
        narg = 1
      Case "approx"
        TL = Chr(&H5A) & TL
        narg = 1
      Case "texpand"
        TL = Chr(&H5B) & TL
        narg = 1
      Case "tcollect"
        TL = Chr(&H5C) & TL
        narg = 1
      Case "getdenom"
        TL = Chr(&H5D) & TL
        narg = 1
      Case "getnum"
        TL = Chr(&H5E) & TL
        narg = 1
      Case "cumsum"
        TL = Chr(&H60) & TL
        narg = 1
      Case "colnorm"
        TL = Chr(&H62) & TL
        narg = 1
      Case "rownorm"
        TL = Chr(&H63) & TL
        narg = 1
      Case "norm"
        TL = Chr(&H64) & TL
        narg = 1
      Case "median"
        TL = Chr(&H66) & TL
        narg = 1
      Case "unitv"
        TL = Chr(&H6B) & TL
        narg = 1
      Case "dim"
        TL = Chr(&H6C) & TL
        narg = 1
      Case "mat" & Chr(18) & "list" 'mat_>_list
        TL = Chr(&H6D) & TL
        narg = 1
      Case "newlist"
        TL = Chr(&H6E) & TL
        narg = 1
      Case "identity"
        TL = Chr(&H71) & TL
        narg = 1
      Case "diag"
        TL = Chr(&H72) & TL
        narg = 1
      Case "coldim"
        TL = Chr(&H73) & TL
        narg = 1
      Case "rowdim"
        TL = Chr(&H74) & TL
        narg = 1
      Case "solve"
        TL = Chr(&H96) & TL
        narg = 2
      Case "csolve"
        TL = Chr(&H97) & TL
        narg = 2
      Case "nsolve"
        TL = Chr(&H98) & TL
        narg = 2
      Case "zeros"
        TL = Chr(&H99) & TL
        narg = 2
      Case "czeros"
        TL = Chr(&H9A) & TL
        narg = 2
      Case "fmin"
        TL = Chr(&H9B) & TL
        narg = 2
      Case "fmax"
        TL = Chr(&H9C) & TL
        narg = 2
      Case "polyeval"
        TL = Chr(&H9E) & TL
        narg = 2
      Case "randpoly"
        TL = Chr(&H9F) & TL
        narg = 2
      Case "crossp"
        TL = Chr(&HA0) & TL
        narg = 2
      Case "dotp"
        TL = Chr(&HA1) & TL
        narg = 2
      Case "gcd"
        TL = Chr(&HA2) & TL
        narg = 2
      Case "lcm"
        TL = Chr(&HA3) & TL
        narg = 2
      Case "mod"
        TL = Chr(&HA4) & TL
        narg = 2
      Case "intdiv"
        TL = Chr(&HA5) & TL
        narg = 2
      Case "remain"
        TL = Chr(&HA6) & TL
        narg = 2
      Case "ncr"
        TL = Chr(&HA7) & TL
        narg = 2
      Case "npr"
        TL = Chr(&HA8) & TL
        narg = 2
      Case "p" & Chr(18) & "rx" 'P_>_Rx
        TL = Chr(&HA9) & TL
        narg = 2
      Case "p" & Chr(18) & "ry" 'P_>_Ry
        TL = Chr(&HAA) & TL
        narg = 2
      Case "r" & Chr(18) & "p" & Chr(136) 'R_>_P_theta_
        TL = Chr(&HAB) & TL
        narg = 2
      Case "r" & Chr(18) & "pr" 'R_>_Pr
        TL = Chr(&HAC) & TL
        narg = 2
      Case "newmat"
        TL = Chr(&HAE) & TL
        narg = 2
      Case "randmat"
        TL = Chr(&HAF) & TL
        narg = 2
      Case "part"
        TL = Chr(&HB1) & TL
        narg = -1
      Case "exp" & Chr(18) & "list" 'exp_>_list
        TL = Chr(&HB2) & TL
        narg = 2
      Case "randnorm"
        TL = Chr(&HB3) & TL
        narg = 2
      Case "mrow"
        TL = Chr(&HB4) & TL
        narg = -1
      Case "rowadd"
        TL = Chr(&HB5) & TL
        narg = -1
      Case "rowswap"
        TL = Chr(&HB6) & TL
        narg = -1
      Case "arclen"
        TL = Chr(&HB7) & TL
        narg = -1
      Case "nint"
        TL = Chr(&HB8) & TL
        narg = -1
      Case Chr(139) '_PI_
        TL = Chr(&HB9) & TL
        narg = -1
      Case Chr(142) '_SIGMA_
        TL = Chr(&HBA) & TL
        narg = -1
      Case "mrowadd"
        TL = Chr(&HBB) & TL
        narg = -1
      Case "ans"
        TL = Chr(&HBC) & TL
        narg = -1
      Case "entry"
        TL = Chr(&HBD) & TL
        narg = -1
      Case "exact"
        TL = Chr(&HBE) & TL
        narg = -1
      Case "comdenom"
        TL = Chr(&HC0) & TL
        narg = -1
      Case "expand"
        TL = Chr(&HC1) & TL
        narg = -1
      Case "factor"
        TL = Chr(&HC2) & TL
        narg = -1
      Case "cfactor"
        TL = Chr(&HC3) & TL
        narg = -1
      Case Chr(189) '_integrate_
        TL = Chr(&HC4) & TL
        narg = -1
      Case Chr(188) '_differentiate_
        TL = Chr(&HC5) & TL
        narg = -1
      Case "avgrc"
        TL = Chr(&HC6) & TL
        narg = -1
      Case "nderiv"
        TL = Chr(&HC7) & TL
        narg = -1
      Case "taylor"
        TL = Chr(&HC8) & TL
        narg = -1
      Case "limit"
        TL = Chr(&HC9) & TL
        narg = -1
      Case "propfrac"
        TL = Chr(&HCA) & TL
        narg = -1
      Case "when"
        TL = Chr(&HCB) & TL
        narg = -1
      Case "round"
        TL = Chr(&HCC) & TL
        narg = -1
      Case "left"
        TL = Chr(&HCE) & TL
        narg = -1
      Case "right"
        TL = Chr(&HCF) & TL
        narg = -1
      Case "mid"
        TL = Chr(&HD0) & TL
        narg = -1
      Case "shift"
        TL = Chr(&HD1) & TL
        narg = -1
      Case "seq"
        TL = Chr(&HD2) & TL
        narg = -1
      Case "list" & Chr(18) & "mat" 'list_>_mat
        TL = Chr(&HD3) & TL
        narg = -1
      Case "submat"
        TL = Chr(&HD4) & TL
        narg = -1
      Case "rand"
        TL = Chr(&HD6) & TL
        narg = -1
      Case "min"
        TL = Chr(&HD7) & TL
        narg = -1
      Case "max"
        TL = Chr(&HD8) & TL
        narg = -1
      Case "eigvc"
        TL = Chr(&HED) & TL
        narg = 1
      Case "eigvl"
        TL = Chr(&HEE) & TL
        narg = 1
      Case "desolve"
        TL = Chr(&HF1) & TL
        narg = -1
      Case "isprime"
        TL = Chr(&HF4) & TL
        narg = 1
      Case "rotate"
        TL = Chr(&HF9) & TL
        narg = -1
      Case "getkey"
        TL = Chr(2) & Chr(&HE3) & TL
        narg = -1
      Case "getfold"
        TL = Chr(3) & Chr(&HE3) & TL
        narg = -1
      Case "switch"
        TL = Chr(4) & Chr(&HE3) & TL
        narg = -1
      Case "ord"
        TL = Chr(6) & Chr(&HE3) & TL
        narg = 1
      Case "expr"
        TL = Chr(7) & Chr(&HE3) & TL
        narg = 1
      Case "char"
        TL = Chr(8) & Chr(&HE3) & TL
        narg = 1
      Case "string"
        TL = Chr(9) & Chr(&HE3) & TL
        narg = 1
      Case "gettype"
        TL = Chr(&HA) & Chr(&HE3) & TL
        narg = 1
      Case "getmode"
        TL = Chr(&HB) & Chr(&HE3) & TL
        narg = 1
      Case "setfold"
        TL = Chr(&HC) & Chr(&HE3) & TL
        narg = 1
      Case "pttest"
        TL = Chr(&HD) & Chr(&HE3) & TL
        narg = 2
      Case "pxltest"
        TL = Chr(&HE) & Chr(&HE3) & TL
        narg = 2
      Case "setgraph"
        TL = Chr(&HF) & Chr(&HE3) & TL
        narg = 2
      Case "settable"
        TL = Chr(&H10) & Chr(&HE3) & TL
        narg = 2
      Case "setmode"
        TL = Chr(&H11) & Chr(&HE3) & TL
        narg = -1
      Case "format"
        TL = Chr(&H12) & Chr(&HE3) & TL
        narg = -1
      Case "instring"
        TL = Chr(&H13) & Chr(&HE3) & TL
        narg = -1
      Case "tmpcnv"
        TL = Chr(&H27) & Chr(&HE3) & TL
        narg = 2
      Case Chr(132) & "tmpcnv" '_DELTA_tmpCnv
        TL = Chr(&H28) & Chr(&HE3) & TL
        narg = 2
      Case "getunits"
        TL = Chr(&H29) & Chr(&HE3) & TL
        narg = -1
      Case "setunits"
        TL = Chr(&H2A) & Chr(&HE3) & TL
        narg = 1
      Case "getconfg"
        TL = Chr(&H34) & Chr(&HE3) & TL
        narg = -1
      Case Chr(132) & "list" '_DELTA_list (AMS 2)
        TL = Chr(&H3B) & Chr(&HE3) & TL
        narg = 1
      Case "isclkon" '(AMS 2.07)
        TL = Chr(&H46) & Chr(&HE3) & TL
        narg = -1
      Case "getdate" '(AMS 2.07)
        TL = Chr(&H47) & Chr(&HE3) & TL
        narg = -1
      Case "gettime" '(AMS 2.07)
        TL = Chr(&H48) & Chr(&HE3) & TL
        narg = -1
      Case "gettmzn" '(AMS 2.07)
        TL = Chr(&H49) & Chr(&HE3) & TL
        narg = -1
      Case "setdate" '(AMS 2.07)
        TL = Chr(&H4A) & Chr(&HE3) & TL
        narg = -1
      Case "settime" '(AMS 2.07)
        TL = Chr(&H4B) & Chr(&HE3) & TL
        narg = -1
      Case "settmzn" '(AMS 2.07)
        TL = Chr(&H4C) & Chr(&HE3) & TL
        narg = 1
      Case "dayofwk" '(AMS 2.07)
        TL = Chr(&H4D) & Chr(&HE3) & TL
        narg = -1
      Case "starttmr" '(AMS 2.07)
        TL = Chr(&H4E) & Chr(&HE3) & TL
        narg = -1
      Case "checktmr" '(AMS 2.07)
        TL = Chr(&H4F) & Chr(&HE3) & TL
        narg = 1
      Case "timecnv" '(AMS 2.07)
        TL = Chr(&H50) & Chr(&HE3) & TL
        narg = 1
      Case "getdtfmt" '(AMS 2.07)
        TL = Chr(&H51) & Chr(&HE3) & TL
        narg = -1
      Case "gettmfmt" '(AMS 2.07)
        TL = Chr(&H52) & Chr(&HE3) & TL
        narg = -1
      Case "getdtstr" '(AMS 2.07)
        TL = Chr(&H53) & Chr(&HE3) & TL
        narg = -1
      Case "gettmstr" '(AMS 2.07)
        TL = Chr(&H54) & Chr(&HE3) & TL
        narg = -1
      Case "setdtfmt" '(AMS 2.07)
        TL = Chr(&H55) & Chr(&HE3) & TL
        narg = 1
      Case "settmfmt" '(AMS 2.07)
        TL = Chr(&H56) & Chr(&HE3) & TL
        narg = 1
      Case "root" '(AMS 3.xy)
        TL = Chr(&H57) & Chr(&HE3) & TL
        narg = 2
      Case "impdif" '(AMS 3.xy)
        TL = Chr(&H59) & Chr(&HE3) & TL
        narg = -1
      Case "isvar" '(AMS 3.xy)
        TL = Chr(&H5B) & Chr(&HE3) & TL
        narg = 1
      Case "islocked" '(AMS 3.xy)
        TL = Chr(&H5C) & Chr(&HE3) & TL
        narg = 1
      Case "isarchiv" '(AMS 3.xy)
        TL = Chr(&H5D) & Chr(&HE3) & TL
        narg = 1
      Case "det"
        nb = 0
        ias = False
        narg = 1
        For i = InStr(Item, "(") + 1 To Len(Item) - 1
          Select Case Mid(Item, i, 1)
          Case """"
            ias = Not ias
          Case "("
            If Not ias Then nb = nb + 1
          Case ")"
            If Not ias Then nb = nb - 1
          Case ","
            If Not ias And nb = 0 Then
              narg = 2
              Exit For
            End If
        End Select
      Next
      If narg = 1 Then
        TL = Chr(&H61) & TL
      Else
        TL = Chr(&H30) & Chr(&HE3) & TL
      End If
      Case "rref"
        nb = 0
        ias = False
        narg = 1
        For i = InStr(Item, "(") + 1 To Len(Item) - 1
          Select Case Mid(Item, i, 1)
          Case """"
            ias = Not ias
          Case "("
            If Not ias Then nb = nb + 1
          Case ")"
            If Not ias Then nb = nb - 1
          Case ","
            If Not ias And nb = 0 Then
              narg = 2
              Exit For
            End If
        End Select
      Next
      If narg = 1 Then
        TL = Chr(&H6F) & TL
      Else
        TL = Chr(&H32) & Chr(&HE3) & TL
      End If
      Case "ref"
        nb = 0
        ias = False
        narg = 1
        For i = InStr(Item, "(") + 1 To Len(Item) - 1
          Select Case Mid(Item, i, 1)
          Case """"
            ias = Not ias
          Case "("
            If Not ias Then nb = nb + 1
          Case ")"
            If Not ias Then nb = nb - 1
          Case ","
            If Not ias And nb = 0 Then
              narg = 2
              Exit For
            End If
        End Select
      Next
      If narg = 1 Then
        TL = Chr(&H70) & TL
      Else
        TL = Chr(&H31) & Chr(&HE3) & TL
      End If
      Case "simult"
        nb = 0
        ias = False
        narg = 1
        For i = InStr(Item, "(") + 1 To Len(Item) - 1
          Select Case Mid(Item, i, 1)
          Case """"
            ias = Not ias
          Case "("
            If Not ias Then nb = nb + 1
          Case ")"
            If Not ias Then nb = nb - 1
          Case ","
            If Not ias And nb = 0 Then narg = narg + 1
        End Select
      Next
      If narg <= 2 Then
        narg = 2
        TL = Chr(&HB0) & TL
      Else
        narg = 3
        TL = Chr(&H33) & Chr(&HE3) & TL
      End If
      Case "mean"
        nb = 0
        ias = False
        ibrace = False
        narg = 1
        For i = InStr(Item, "(") + 1 To Len(Item) - 1
          Select Case Mid(Item, i, 1)
          Case """"
            ias = Not ias
          Case "{"
            ibrace = True
          Case "}"
            ibrace = False
          Case "("
            If Not ias Then nb = nb + 1
          Case ")"
            If Not ias Then nb = nb - 1
          Case ","
            If Not ibrace And Not ias And nb = 0 Then
              narg = 2
              Exit For
            End If
        End Select
      Next
      If narg = 1 Then
        TL = Chr(&H65) & TL
      Else
        TL = Chr(&H36) & Chr(&HE3) & TL
      End If
      Case "stddev"
        nb = 0
        ias = False
        ibrace = False
        narg = 1
        For i = InStr(Item, "(") + 1 To Len(Item) - 1
          Select Case Mid(Item, i, 1)
          Case """"
            ias = Not ias
          Case "("
            If Not ias Then nb = nb + 1
          Case "{"
            ibrace = True
          Case "}"
            ibrace = False
          Case ")"
            If Not ias Then nb = nb - 1
          Case ","
            If Not ibrace And Not ias And nb = 0 Then
              narg = 2
              Exit For
            End If
        End Select
      Next
      If narg = 1 Then
        TL = Chr(&H68) & TL
      Else
        TL = Chr(&H38) & Chr(&HE3) & TL
      End If
      Case "variance"
        nb = 0
        ias = False
        ibrace = False
        narg = 1
        For i = InStr(Item, "(") + 1 To Len(Item) - 1
          Select Case Mid(Item, i, 1)
          Case """"
            ias = Not ias
          Case "{"
            ibrace = True
          Case "}"
            ibrace = False
          Case "("
            If Not ias Then nb = nb + 1
          Case ")"
            If Not ias Then nb = nb - 1
          Case ","
            If Not ibrace And Not ias And nb = 0 Then
              narg = 2
              Exit For
            End If
        End Select
      Next
      If narg = 1 Then
        TL = Chr(&H6A) & TL
      Else
        TL = Chr(&H3A) & Chr(&HE3) & TL
      End If
      Case "product"
        nb = 0
        ias = False
        ibrace = False
        narg = 1
        For i = InStr(Item, "(") + 1 To Len(Item) - 1
          Select Case Mid(Item, i, 1)
          Case """"
            ias = Not ias
          Case "{"
            ibrace = True
          Case "}"
            ibrace = False
          Case "("
            If Not ias Then nb = nb + 1
          Case ")"
            If Not ias Then nb = nb - 1
          Case ","
            If Not ibrace And Not ias And nb = 0 Then narg = narg + 1
        End Select
      Next
      If narg = 1 Then
        TL = Chr(&H67) & TL
      Else
        narg = -1
        TL = Chr(&H37) & Chr(&HE3) & TL
      End If
      Case "sum"
        nb = 0
        ias = False
        ibrace = False
        narg = 1
        For i = InStr(Item, "(") + 1 To Len(Item) - 1
          Select Case Mid(Item, i, 1)
          Case """"
            ias = Not ias
          Case "{"
            ibrace = True
          Case "}"
            ibrace = False
          Case "("
            If Not ias Then nb = nb + 1
          Case ")"
            If Not ias Then nb = nb - 1
          Case ","
            If Not ibrace And Not ias And nb = 0 Then narg = narg + 1
        End Select
      Next
      If narg = 1 Then
        TL = Chr(&H69) & TL
      Else
        narg = -1
        TL = Chr(&H39) & Chr(&HE3) & TL
      End If
      Case "augment"
        nb = 0
        ias = False
        For i = InStr(Item, "(") + 1 To Len(Item) - 1
          Select Case Mid(Item, i, 1)
          Case """"
            ias = Not ias
          Case "("
            If Not ias Then nb = nb + 1
          Case ")"
            If Not ias Then nb = nb - 1
          Case ","
            If Not ias And nb = 0 Then
              vn = False
              Exit For
            End If
          Case ";"
            If Not ias And nb = 0 Then
              vn = True
              Exit For
            End If
        End Select
      Next
      If vn Then
        TL = Chr(&H35) & Chr(&HE3) & TL
      Else
        TL = Chr(&HAD) & TL
      End If
      narg = 2
      Case Else 'user function
        If Right(Left(Item, InStr(Item, "(") - 1), 1) = "'" Then
          'function' (prime)
          TL = Chr(&HDA) & Chr(&HF2) & TL
          Item = Left(Item, InStr(Item, "(") - 2) & Mid(Item, InStr(Item, "("))
        Else
          TL = Chr(&HDA) & TL
        End If
        k = Len(locvars)
        If funcdef Then
        For i = Len(locvars) To 1 Step -1
          If Asc(Mid(locvars, i)) = 1 Then
            k = i - 1
            Exit For
          End If
        Next
        End If
        j = 0
        For i = k To 1 Step -1
          If Asc(Mid(locvars, i)) = 1 Then
            j = i
            Exit For
          End If
        Next
        If InStr(Right(locvars, Len(locvars) - j), Chr(0) & LCase89(Left(Item, InStr(Item, "(") - 1)) & Chr(0)) Then
          TL = Chr(&HF0) & TL
        End If
        Select Case LCase89(Left(Item, InStr(Item, "(") - 1))
          '1 letter functions
        Case "a"
          TL = Chr(&HB) & TL
        Case "b"
          TL = Chr(&HC) & TL
        Case "c"
          TL = Chr(&HD) & TL
        Case "d"
          TL = Chr(&HE) & TL
        Case "e"
          TL = Chr(&HF) & TL
        Case "f"
          TL = Chr(&H10) & TL
        Case "g"
          TL = Chr(&H11) & TL
        Case "h"
          TL = Chr(&H12) & TL
        Case "i"
          TL = Chr(&H13) & TL
        Case "j"
          TL = Chr(&H14) & TL
        Case "k"
          TL = Chr(&H15) & TL
        Case "l"
          TL = Chr(&H16) & TL
        Case "m"
          TL = Chr(&H17) & TL
        Case "n"
          TL = Chr(&H18) & TL
        Case "o"
          TL = Chr(&H19) & TL
        Case "p"
          TL = Chr(&H1A) & TL
        Case "q"
          TL = Chr(&H1B) & TL
        Case "r"
          TL = Chr(2) & TL
        Case "s"
          TL = Chr(3) & TL
        Case "t"
          TL = Chr(4) & TL
        Case "u"
          TL = Chr(5) & TL
        Case "v"
          TL = Chr(6) & TL
        Case "w"
          TL = Chr(7) & TL
        Case "x"
          TL = Chr(8) & TL
        Case "y"
          TL = Chr(9) & TL
        Case "z"
          TL = Chr(&HA) & TL
        Case Else
          TL = Chr(0) & LCase89(Left(Item, InStr(Item, "(") - 1)) & Chr(0) & TL
      End Select
      narg = -1
      If funcdef Then
        loccmd = True
        poplocalateol = True
      End If
    End Select
    j = Len(Item) - 1
    iarg = 0
    Temp = ""
    ias = False
    nb = 0
    If narg <> 0 Then
      For i = Len(Item) - 1 To InStr(Item, "(") + 1 Step -1
        Select Case Mid(Item, i, 1)
          Case """"
            ias = Not ias
          Case "(", "{", "["
            If Not ias Then nb = nb + 1
          Case ")", "}", "]"
            If Not ias Then nb = nb - 1
          Case ",", ";"
            If Not ias And nb = 0 Then
              Temp = Mid(Item, i + 1, j - i) & Chr(0) & Temp
              j = i - 1
              iarg = iarg + 1
              If iarg = narg Then Exit For
            End If
        End Select
      Next
    End If
    If iarg < narg Or narg = -1 And Mid(Item, InStr(Item, "(") + 1) <> ")" Then
      Temp = Mid(Item, i + 1, j - i) & Chr(0) & Temp
      iarg = iarg + 1
    End If
    If narg = -1 Then
      stack = Temp & "___E5" & Chr(0) & stack
    Else
      If iarg < narg Then
        For i = iarg + 1 To narg
          Temp = Temp & "___2E" & Chr(0)
        Next
      End If
      stack = Temp & stack
    End If
    Item = ""
  End If
  
  '# indirection
  If Left(Item, 1) = "#" Then
    TL = Chr(1) & Chr(&HE3) & TL
    stack = Mid(Item, 2) & Chr(0) & stack
    Item = ""
    '_e_
  ElseIf Item = Chr(150) Then
    TL = Chr(&H25) & TL
    Item = ""
    '_i_
  ElseIf Item = Chr(151) Then
    TL = Chr(&H26) & TL
    Item = ""
    '_pi_
  ElseIf Item = Chr(140) Then
    TL = Chr(&H24) & TL
    Item = ""
    '_infinity_
  ElseIf Item = Chr(190) Then
    TL = Chr(&H28) & TL
    Item = ""
    '_(-)__infinity_
  ElseIf Item = Chr(173) & Chr(190) Then
    TL = Chr(&H27) & TL
    Item = ""
  End If
  Select Case Left(Item, 1)
    'numbers
    Case "0" To "9", "."
      If LCase89(Left(Item, 2)) = "0b" Then
        Item = bVal(Mid(Item, 3))
        TL = Chr(&H2B) & Chr(&HE3) & TL
      ElseIf LCase89(Left(Item, 2)) = "0h" Then
        Item = hVal(Mid(Item, 3))
        TL = Chr(&H2C) & Chr(&HE3) & TL
      End If
      If InStr(Item, Chr(149)) And Not InStr(Item, ".") Then
        Item = Left(Item, InStr(Item, Chr(149)) - 1) & "." & Mid(Item, InStr(Item, Chr(149)))
      End If
      If InStr(Item, ".") = 0 Then
        'integer
        Temp = ""
        While Item <> "0"
          Temp = Temp & Chr(StrDiv256(Item))
        Wend
        TL = Temp & Chr(Len(Temp)) & Chr(&H1F) & TL
      Else
        'decimal
        'extract mantissa
        Temp = ""
        For i = 1 To Len(Item)
          Select Case Mid(Item, i, 1)
          Case "0"
            If Temp <> "" Then
              Temp = Temp & "0"
              If Len(Temp) = 14 Then Exit For
            End If
          Case "1" To "9"
            Temp = Temp & Mid(Item, i, 1)
            If Len(Temp) = 14 Then Exit For
          Case Chr(149)
            Exit For
          End Select
        Next
      If Temp = "" Then
        '0.
        TL = Chr(&H40) & String(8, 0) & Chr(&H23) & TL
      Else
        If Len(Temp) < 14 Then Temp = Temp & String(14 - Len(Temp), "0")
        For i = 1 To 14 Step 2
          Temp = Temp & Chr(Val("&H" & Mid(Temp, i, 2)))
        Next
        'compute exponent
        j = -1
        For i = 1 To Len(Item)
          Select Case Mid(Item, i, 1)
            Case "0"
              If j <> -1 Then j = j + 1
            Case "1" To "9"
              j = j + 1
            Case ".", Chr(149)
              Exit For
          End Select
        Next
        If j = -1 Then
          For i = InStr(Item, ".") + 1 To Len(Item)
            Select Case Mid(Item, i, 1)
              Case "0"
                j = j - 1
              Case "1" To "9"
                Exit For
            End Select
          Next
        End If
        If InStr(Item, Chr(149)) <> 0 Then
          If Mid(Item, InStr(Item, Chr(149)) + 1, 1) = Chr(173) Then
            j = j - Val(Mid(Item, InStr(Item, Chr(149)) + 2))
          Else
            j = j + Val(Mid(Item, InStr(Item, Chr(149)) + 1))
          End If
        End If
        j = j + &H4000
        TL = Chr(j \ 256) & Chr(j Mod 256) & Mid(Temp, 15) & Chr(&H23) & TL
      End If
    End If
    Item = ""
    'variables
    Case "a" To "z", "A" To "Z", Chr(128), Chr(129), Chr(130), Chr(131), Chr(132), Chr(133), Chr(134), Chr(135), Chr(136), Chr(137), Chr(138), Chr(139), Chr(141), Chr(142), Chr(143), Chr(144), Chr(145), Chr(146), Chr(147), Chr(148), "À" To "Ö", "Ø" To "ö", "ø" To "ÿ", "_", Chr(154), Chr(155), Chr(178), "\"
        j = 0
        For i = Len(locvars) To 1 Step -1
          If Asc(Mid(locvars, i)) = 1 Then
            j = i
            Exit For
          End If
        Next
      If InStr(Right(locvars, Len(locvars) - j), Chr(0) & LCase89(Item) & Chr(0)) Then
        TL = Chr(&HF0) & TL
      End If
      Select Case LCase89(Item)
        '1 letter variables
      Case "a"
        TL = Chr(&HB) & TL
      Case "b"
        TL = Chr(&HC) & TL
      Case "c"
        TL = Chr(&HD) & TL
      Case "d"
        TL = Chr(&HE) & TL
      Case "e"
        TL = Chr(&HF) & TL
      Case "f"
        TL = Chr(&H10) & TL
      Case "g"
        TL = Chr(&H11) & TL
      Case "h"
        TL = Chr(&H12) & TL
      Case "i"
        TL = Chr(&H13) & TL
      Case "j"
        TL = Chr(&H14) & TL
      Case "k"
        TL = Chr(&H15) & TL
      Case "l"
        TL = Chr(&H16) & TL
      Case "m"
        TL = Chr(&H17) & TL
      Case "n"
        TL = Chr(&H18) & TL
      Case "o"
        TL = Chr(&H19) & TL
      Case "p"
        TL = Chr(&H1A) & TL
      Case "q"
        TL = Chr(&H1B) & TL
      Case "r"
        TL = Chr(2) & TL
      Case "s"
        TL = Chr(3) & TL
      Case "t"
        TL = Chr(4) & TL
      Case "u"
        TL = Chr(5) & TL
      Case "v"
        TL = Chr(6) & TL
      Case "w"
        TL = Chr(7) & TL
      Case "x"
        TL = Chr(8) & TL
      Case "y"
        TL = Chr(9) & TL
      Case "z"
        TL = Chr(&HA) & TL
        'system constants (true, false, undef)
      Case "true"
        TL = Chr(&H2C) & TL
      Case "false"
        TL = Chr(&H2B) & TL
      Case "undef"
        TL = Chr(&H2A) & TL
        'system variables
      Case Chr(154) '_x_bar_
        TL = Chr(1) & Chr(&H1C) & TL
      Case Chr(155) '_y_bar_
        TL = Chr(2) & Chr(&H1C) & TL
      Case Chr(142) & "x" '_SIGMA_x
        TL = Chr(3) & Chr(&H1C) & TL
      Case Chr(142) & "x²" '_SIGMA_x²
        TL = Chr(4) & Chr(&H1C) & TL
      Case Chr(142) & "y" '_SIGMA_y
        TL = Chr(5) & Chr(&H1C) & TL
      Case Chr(142) & "y²" '_SIGMA_y²
        TL = Chr(6) & Chr(&H1C) & TL
      Case Chr(142) & "xy" '_SIGMA_xy
        TL = Chr(7) & Chr(&H1C) & TL
      Case "sx"
        TL = Chr(8) & Chr(&H1C) & TL
      Case "sy"
        TL = Chr(9) & Chr(&H1C) & TL
      Case Chr(143) & "x" '_sigma_x
        TL = Chr(&HA) & Chr(&H1C) & TL
      Case Chr(143) & "y" '_sigma_y
        TL = Chr(&HB) & Chr(&H1C) & TL
      Case "nstat"
        TL = Chr(&HC) & Chr(&H1C) & TL
      Case "minx"
        TL = Chr(&HD) & Chr(&H1C) & TL
      Case "miny"
        TL = Chr(&HE) & Chr(&H1C) & TL
      Case "q1"
        TL = Chr(&HF) & Chr(&H1C) & TL
      Case "medstat"
        TL = Chr(&H10) & Chr(&H1C) & TL
      Case "q3"
        TL = Chr(&H11) & Chr(&H1C) & TL
      Case "maxx"
        TL = Chr(&H12) & Chr(&H1C) & TL
      Case "maxy"
        TL = Chr(&H13) & Chr(&H1C) & TL
      Case "corr"
        TL = Chr(&H14) & Chr(&H1C) & TL
      Case "r²"
        TL = Chr(&H15) & Chr(&H1C) & TL
      Case "medx1"
        TL = Chr(&H16) & Chr(&H1C) & TL
      Case "medx2"
        TL = Chr(&H17) & Chr(&H1C) & TL
      Case "medx3"
        TL = Chr(&H18) & Chr(&H1C) & TL
      Case "medy1"
        TL = Chr(&H19) & Chr(&H1C) & TL
      Case "medy2"
        TL = Chr(&H1A) & Chr(&H1C) & TL
      Case "medy3"
        TL = Chr(&H1B) & Chr(&H1C) & TL
      Case "xc"
        TL = Chr(&H1C) & Chr(&H1C) & TL
      Case "yc"
        TL = Chr(&H1D) & Chr(&H1C) & TL
      Case "zc"
        TL = Chr(&H1E) & Chr(&H1C) & TL
      Case "tc"
        TL = Chr(&H1F) & Chr(&H1C) & TL
      Case "rc"
        TL = Chr(&H20) & Chr(&H1C) & TL
      Case Chr(136) & "c" '_theta_c
        TL = Chr(&H21) & Chr(&H1C) & TL
      Case "nc"
        TL = Chr(&H22) & Chr(&H1C) & TL
      Case "xfact"
        TL = Chr(&H23) & Chr(&H1C) & TL
      Case "yfact"
        TL = Chr(&H24) & Chr(&H1C) & TL
      Case "zfact"
        TL = Chr(&H25) & Chr(&H1C) & TL
      Case "xmin"
        TL = Chr(&H26) & Chr(&H1C) & TL
      Case "xmax"
        TL = Chr(&H27) & Chr(&H1C) & TL
      Case "xscl"
        TL = Chr(&H28) & Chr(&H1C) & TL
      Case "ymin"
        TL = Chr(&H29) & Chr(&H1C) & TL
      Case "ymax"
        TL = Chr(&H2A) & Chr(&H1C) & TL
      Case "yscl"
        TL = Chr(&H2B) & Chr(&H1C) & TL
      Case Chr(132) & "x" '_DELTA_x
        TL = Chr(&H2C) & Chr(&H1C) & TL
      Case Chr(132) & "y" '_DELTA_y
        TL = Chr(&H2D) & Chr(&H1C) & TL
      Case "xres"
        TL = Chr(&H2E) & Chr(&H1C) & TL
      Case "xgrid"
        TL = Chr(&H2F) & Chr(&H1C) & TL
      Case "ygrid"
        TL = Chr(&H30) & Chr(&H1C) & TL
      Case "zmin"
        TL = Chr(&H31) & Chr(&H1C) & TL
      Case "zmax"
        TL = Chr(&H32) & Chr(&H1C) & TL
      Case "zscl"
        TL = Chr(&H33) & Chr(&H1C) & TL
      Case "eye" & Chr(136) 'eye_theta_
        TL = Chr(&H34) & Chr(&H1C) & TL
      Case "eye" & Chr(145) 'eye_phi_
        TL = Chr(&H35) & Chr(&H1C) & TL
      Case Chr(136) & "min" '_theta_min
        TL = Chr(&H36) & Chr(&H1C) & TL
      Case Chr(136) & "max" '_theta_max
        TL = Chr(&H37) & Chr(&H1C) & TL
      Case Chr(136) & "step" '_theta_step
        TL = Chr(&H38) & Chr(&H1C) & TL
      Case "tmin"
        TL = Chr(&H39) & Chr(&H1C) & TL
      Case "tmax"
        TL = Chr(&H3A) & Chr(&H1C) & TL
      Case "tstep"
        TL = Chr(&H3B) & Chr(&H1C) & TL
      Case "nmin"
        TL = Chr(&H3C) & Chr(&H1C) & TL
      Case "nmax"
        TL = Chr(&H3D) & Chr(&H1C) & TL
      Case "plotstrt"
        TL = Chr(&H3E) & Chr(&H1C) & TL
      Case "plotstep"
        TL = Chr(&H3F) & Chr(&H1C) & TL
      Case "zxmin"
        TL = Chr(&H40) & Chr(&H1C) & TL
      Case "zxmax"
        TL = Chr(&H41) & Chr(&H1C) & TL
      Case "zxscl"
        TL = Chr(&H42) & Chr(&H1C) & TL
      Case "zymin"
        TL = Chr(&H43) & Chr(&H1C) & TL
      Case "zymax"
        TL = Chr(&H44) & Chr(&H1C) & TL
      Case "zyscl"
        TL = Chr(&H45) & Chr(&H1C) & TL
      Case "zxres"
        TL = Chr(&H46) & Chr(&H1C) & TL
      Case "z" & Chr(136) & "min" 'z_theta_min
        TL = Chr(&H47) & Chr(&H1C) & TL
      Case "z" & Chr(136) & "max" 'z_theta_max
        TL = Chr(&H48) & Chr(&H1C) & TL
      Case "z" & Chr(136) & "step" 'z_theta_step
        TL = Chr(&H49) & Chr(&H1C) & TL
      Case "ztmin"
        TL = Chr(&H4A) & Chr(&H1C) & TL
      Case "ztmax"
        TL = Chr(&H4B) & Chr(&H1C) & TL
      Case "ztstep"
        TL = Chr(&H4C) & Chr(&H1C) & TL
      Case "zxgrid"
        TL = Chr(&H4D) & Chr(&H1C) & TL
      Case "zygrid"
        TL = Chr(&H4E) & Chr(&H1C) & TL
      Case "zzmin"
        TL = Chr(&H4F) & Chr(&H1C) & TL
      Case "zzmax"
        TL = Chr(&H50) & Chr(&H1C) & TL
      Case "zzscl"
        TL = Chr(&H51) & Chr(&H1C) & TL
      Case "zeye" & Chr(136) 'zeye_theta_
        TL = Chr(&H52) & Chr(&H1C) & TL
      Case "zeye" & Chr(145) 'zeye_phi_
        TL = Chr(&H53) & Chr(&H1C) & TL
      Case "znmin"
        TL = Chr(&H54) & Chr(&H1C) & TL
      Case "znmax"
        TL = Chr(&H55) & Chr(&H1C) & TL
      Case "zpltstrt"
        TL = Chr(&H56) & Chr(&H1C) & TL
      Case "zpltstep"
        TL = Chr(&H57) & Chr(&H1C) & TL
      Case "seed1"
        TL = Chr(&H58) & Chr(&H1C) & TL
      Case "seed2"
        TL = Chr(&H59) & Chr(&H1C) & TL
      Case "ok"
        TL = Chr(&H5A) & Chr(&H1C) & TL
      Case "errornum"
        TL = Chr(&H5B) & Chr(&H1C) & TL
      Case "sysmath"
        TL = Chr(&H5C) & Chr(&H1C) & TL
      Case "sysdata"
        TL = Chr(&H5D) & Chr(&H1C) & TL
        '&H5E is invalid!
      Case "regcoef"
        TL = Chr(&H5F) & Chr(&H1C) & TL
      Case "tblinput"
        TL = Chr(&H60) & Chr(&H1C) & TL
      Case "tblstart"
        TL = Chr(&H61) & Chr(&H1C) & TL
      Case Chr(132) & "tbl" '_DELTA_tbl
        TL = Chr(&H62) & Chr(&H1C) & TL
        '&H63 is invalid!
      Case "eye" & Chr(146) 'eye_psi_
        TL = Chr(&H64) & Chr(&H1C) & TL
      Case "tplot"
        TL = Chr(&H65) & Chr(&H1C) & TL
      Case "diftol"
        TL = Chr(&H66) & Chr(&H1C) & TL
      Case "zeye" & Chr(146) 'zeye_psi_
        TL = Chr(&H67) & Chr(&H1C) & TL
      Case "t0"
        TL = Chr(&H68) & Chr(&H1C) & TL
      Case "dtime"
        TL = Chr(&H69) & Chr(&H1C) & TL
      Case "ncurves"
        TL = Chr(&H6A) & Chr(&H1C) & TL
      Case "fldres"
        TL = Chr(&H6B) & Chr(&H1C) & TL
      Case "estep"
        TL = Chr(&H6C) & Chr(&H1C) & TL
      Case "zt0de"
        TL = Chr(&H6D) & Chr(&H1C) & TL
      Case "ztmaxde"
        TL = Chr(&H6E) & Chr(&H1C) & TL
      Case "ztstepde"
        TL = Chr(&H6F) & Chr(&H1C) & TL
      Case "ztplotde"
        TL = Chr(&H70) & Chr(&H1C) & TL
      Case "ncontour"
        TL = Chr(&H71) & Chr(&H1C) & TL
      Case Else
        TL = Chr(0) & LCase89(Item) & Chr(0) & TL
    End Select
    If loccmd Then
      locvars = locvars & LCase89(Item) & Chr(0)
    End If
    Item = ""
      If funcdef Then
        loccmd = True
        poplocalateol = True
      End If
    'arbitrary constants
    Case "@"
      If LCase89(Mid(Item, 2, 1)) = "n" Then
        TL = Chr(Mid(Item, 3)) & Chr(&H1E) & TL
      Else
        TL = Chr(Mid(Item, 2)) & Chr(&H1D) & TL
      End If
      Item = ""
  End Select
  
  If Item <> "" Then 'cannot tokenize this item -> NOTHING
    TL = Chr(&H2E) & TL
    Item = ""
  End If
End Sub

Public Sub WriteTIVar(ByVal FN$, ByVal TIFolder$, ByVal TIName$, ByVal S$)
Attribute WriteTIVar.VB_Description = "Takes a file name FN, an on-calc folder name TIFolder, an on-calc file name TIName and a string of raw data S. Saves the data S to the file named FN, and sets the on-calc folder name and the on-calc file name to TIFolder and TIFile, respectively."
  Dim F As TIFile, CS&, i&, j&, Checksum%, T As Byte
  With F
    .Fixed1(1) = 1
    .Fixed2(1) = 1
    .Fixed2(3) = &H52
    .Fixed3(1) = &HA5
    .Fixed3(2) = &H5A
    .FixedFT(1) = 0
    If Len(S) >= 4 Then
        If Asc(Mid(S, Len(S) - 1, 1)) And 8 Then
          .FixedFT(2) = 0
        Else
          .FixedFT(2) = 3
        End If
    Else
      .FixedFT(2) = 0
    End If
    .Signature = "**TI89**"
    .Folder = TIFolder & Chr(0)
    .FileName = TIName & Chr(0)
    .Description = "saved with " & App.Title & " v." & App.Major & "." & Format(App.Minor, "00") & "." & Format(App.Revision, "0000") & Chr(0)
    Select Case Asc(Right(S, 1))
      Case &H2D 'String
        .FileType = &HC
      Case &HD9 'List
        If Asc(Mid(S, Len(S) - 1, 1)) = &HD9 Then 'Matrix
          .FileType = 6
        Else
          .FileType = 4
        End If
      Case &HDB 'Matrix
        .FileType = 6
      Case &HDC 'Program / Function
        j = Len(S) - 2
        Do
          j = j - 1
          If Asc(Mid(S, j, 1)) = &HE5 Then
            Exit Do
          End If
      Loop
      If Mid(S, j - 2, 2) = Chr(&H19) & Chr(&HE4) Then
        .FileType = &H12 'Program
      Else
        .FileType = &H13 'Function
      End If
      Case &HDD 'Data
        .FileType = &HA
      Case &HDE 'GDB
        .FileType = &HD
      Case &HDF 'PIC
        .FileType = &H10
      Case &HE0 'Text
        .FileType = &HB
      Case &HE1 'FIG
        .FileType = &HE
      Case &HE2 'MAC
        .FileType = &H14
      Case &HF3 'Assembler
        .FileType = &H21
      Case &HF8 'ZIP
        .FileType = &H1C
      Case Else
        .FileType = 0
    End Select
    .TISize(1) = Len(S) \ 256
    .TISize(2) = Len(S) Mod 256
    CS = CLng(.TISize(1)) + CLng(.TISize(2))
    For i = 1 To Len(S)
      CS = CS + Asc(Mid(S, i, 1))
    Next
    CS = CS Mod 65536
    If CS > 32767 Then CS = CS - 65536
    Checksum = CS
    .FileSize = Len(F) + Len(S) + 2
  End With
  If Dir(FN) <> "" Then Kill FN
  Open FN For Binary As #1
  Put #1, 1, F
  Put #1, , S
  Put #1, , Checksum
  Close #1
End Sub

Public Function CRLFToCROnly$(ByVal S$)
Attribute CRLFToCROnly.VB_Description = "Converts the line-endings in S from CR-LF to CR-only and returns the converted string."
  CRLFToCROnly = Replace(S, vbCrLf, Chr(13))
End Function

Public Function CROnlyToCRLF$(ByVal S$)
Attribute CROnlyToCRLF.VB_Description = "Converts the line-endings in S from CR-only to CR-LF and returns the converted string."
  CROnlyToCRLF = Replace(S, Chr(13), vbCrLf)
End Function

Private Function StrAdd$(ByVal S1$, ByVal S2$)
  Dim i&, S3$, S4$, S5$
  If Len(S1) > Len(S2) Then
    S2 = String(Len(S1) - Len(S2), "0") & S2
  Else
    S1 = String(Len(S2) - Len(S1), "0") & S1
  End If
  StrAdd = ""
  Dec1 = CDec(0)
  For i = Len(S1) To 1 Step -28
    S3 = Right(S1, 28)
    S4 = Right(S2, 28)
    S5 = CDec(S3) + CDec(S4) + Dec1
    If Len(S5) = 29 Then
      Dec1 = CDec(1)
      StrAdd = Mid(S5, 2) & StrAdd
    Else
      Dec1 = CDec(0)
      If Len(S1) > 28 Then
        StrAdd = String(28 - Len(S5), "0") & S5 & StrAdd
      Else
        StrAdd = S5 & StrAdd
      End If
    End If
    If Len(S1) > 28 Then
      S1 = Left(S1, Len(S1) - 28)
      S2 = Left(S2, Len(S2) - 28)
    End If
  Next
  If Dec1 <> CDec(0) Then StrAdd = Dec1 & StrAdd
End Function

Private Function StrMult256$(ByVal S$)
  Dim i&, S1$, S2$
  StrMult256 = ""
  Dec1 = CDec(0)
  For i = Len(S) To 1 Step -26
    S1 = Right(S, 26)
    S2 = CDec(256) * CDec(S1) + Dec1
    If Len(S2) > 26 Then
      Dec1 = CDec(Left(S2, Len(S2) - 26))
      StrMult256 = Mid(S2, Len(S2) - 25) & StrMult256
    Else
      Dec1 = CDec(0)
      If Len(S) > 26 Then
        StrMult256 = String(26 - Len(S2), "0") & S2 & StrMult256
      Else
        StrMult256 = S2 & StrMult256
      End If
    End If
    If Len(S) > 26 Then
      S = Left(S, Len(S) - 26)
    End If
  Next
  If Dec1 <> CDec(0) Then StrMult256 = Dec1 & StrMult256
End Function

Private Function StrMult256P$(ByVal S$, ByVal p&)
  For i& = 1 To p
    S = StrMult256(S)
  Next
  StrMult256P = S
End Function

Private Function StrDiv256&(S$) 'stores q to S, returns r
  Dim A&, b$, q$, r&
  If Len(S) Mod 8 <> 0 Then
    S = String(8 - Len(S) Mod 8, "0") & S
  End If
  For i& = 1 To Len(S) Step 8
    A = Mid(S, i, 8)
    b = r * 390625 + A \ 256&
    If q <> "" And Len(b) < 8 Then
      b = String(8 - Len(b), "0") & b
    End If
    If b <> "0" Then
      q = q & b
    End If
    r = A Mod 256&
  Next
  If q = "" Then
    S = "0"
  Else
    S = q
  End If
  StrDiv256 = r
End Function

Private Function bVal#(ByVal num$)
  Dim i&
  For i = 0 To Len(num) - 1
    If Mid(num, Len(num) - i, 1) Then
      bVal = bVal + 2# ^ i
    End If
  Next
End Function

Private Function hVal#(ByVal num$)
  hVal = Val("&H" & num & "&")
  If hVal < 0 Then hVal = hVal + 4294967296#
End Function

Private Function bStr$(ByVal num#)
  Dim t1 As String * 1, n&
  If num >= 2147483648# Then
    t1 = "1"
    n = num - 2147483648#
  Else
    t1 = "0"
    n = num
  End If
  While n > 0
    bStr = (n Mod 2) & bStr
    n = n \ 2
  Wend
  bStr = t1 & bStr
  While Left(bStr, 1) = "0" And bStr <> "0"
    bStr = Mid(bStr, 2)
  Wend
End Function

Private Function hStr$(ByVal num#)
  If num >= 2147483648# Then num = num - 4294967296#
  hStr = Hex(num)
End Function

Private Function LCase89$(ByVal Str$)
  Dim i&
  For i = 1 To Len(Str)
    If Asc(Mid(Str, i, 1)) < 128 Or Asc(Mid(Str, i, 1)) >= 192 Then Mid(Str, i, 1) = LCase(Mid(Str, i, 1))
  Next
  LCase89 = Str
End Function

Private Function Replace$(ByVal Original$, ByVal OrigStr$, ByVal NewStr$)
  Dim i&
  Replace = Original
  i = 1
  While i <> 0
    i = InStr(i, Replace, OrigStr)
    If i <> 0 Then
      Replace = Left(Replace, i - 1) & NewStr & Mid(Replace, i + Len(OrigStr))
      i = i + Len(NewStr)
    End If
  Wend
End Function
