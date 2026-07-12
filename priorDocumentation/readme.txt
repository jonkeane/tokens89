TOKENS89 OCX - Copyright (C) 2000-2003 Kevin Kofler, 2013-2014 Peter Engels
===========================================================================

I. Introduction
---------------

Tokens89 OCX is an OCX control which contains routines to:
* tokenize TI-89/92+/V200 programs
* detokenize TI-89/92+/V200 programs
* import TI-89/92+/V200 programs in TI-GraphLink 7-bit-ASCII format
* export TI-89/92+/V200 programs to TI-GraphLink 7-bit-ASCII format
* convert between CR-LF and CR-only line endings as needed
It is released under the GNU LGPL (see section V for details).

II. Usage
---------

* Import the component to your VB, Delphi, MSVC or MinGW project.
  (WARNING: I have only tested the component with VB and MinGW.)
* Instantiate one object of class "Tokens89_OCX.Tokens89OCX". In a RAD
  environment like VB, this is most easily done by adding one to a
  form. (Don't worry, the component will NOT be visible at runtime.)
* Refer to that object when accessing any of the functions documented
  below.

III. Commented list of functions
--------------------------------

1) File Type Enumeration

i) Public Enum FTypes

List of supported file types for use in the export functions. This type
controls the way the string passed to the function is interpreted:
ft89t = 0: text file
ft89p89f = 1: program or function file
ft89e89l89m89s = 2: file of an algebraic expression type

2) File Opening and Detokenization Functions

i) Public Function TIVar$(ByVal FN$)

Takes the file name of a variable in TI-GraphLink binary format. Returns
the raw data contained in the variable as a string.
WARNING: The data returned may be tokenized. Run DeToken on it or use
         OpenTI instead to get the content in a readable content.

ii) Public Function DeToken$(ByVal S$)

Takes the possibly tokenized raw data of a variable as returned by TIVar
and returns the data in a readable form.
WARNING: The data returned uses CR-only line endings. Use CROnlyToCRLF to
         convert it to Windows CR-LF line endings.

iii) Public Function OpenTI$(ByVal FN$)

Takes the file name of a variable in TI-GraphLink binary format. Returns
the data in readable form. This function simply calls DeToken(TIVar(FN)).
Using this function is the recommended way to open a file using Tokens89
OCX.
WARNING: The data returned uses CR-only line endings. Use CROnlyToCRLF to
         convert it to Windows CR-LF line endings.

iv) Public Function GetTIFolder$(ByVal FN$)

Takes the file name of a variable in TI-GraphLink binary format. Returns
its on-calc folder name.

v) Public Function GetTIFile$(ByVal FN$)

Takes the file name of a variable in TI-GraphLink binary format. Returns
its on-calc file name.

3) File Saving and Tokenization Functions

i) Public Sub WriteTIVar(ByVal FN$, ByVal TIFolder$, ByVal TIName$, ByVal S$)

Takes a file name FN, an on-calc folder name TIFolder, an on-calc file name
TIName and a string of raw data S. Saves the data S to the file named FN,
and sets the on-calc folder name and the on-calc file name to TIFolder and
TIFile, respectively.
WARNING: This function does NOT work correctly with readable data. You need
         to add the necessary tags, and possibly to tokenize the data, using
         the Token function, or to use SaveTI instead of WriteTIVar.

ii) Public Function Token$(ByVal S$, ByVal FType As FTypes,
                           [Tokenize As Boolean = True])

Takes a string of readable data S, a file type FType (as defined in the
FTypes enumeration above), and an optional parameter Tokenize. Returns a
string in the format expected by WriteTIVar. The effects of the
Tokenize parameter are described below:
* Text files CANNOT be tokenized. For text files, Tokenize is ignored, and
  the data is never tokenized.
* Algebraic expressions and one-line functions MUST be tokenized. Tokenize
  is ignored, and the data is always tokenized.
* Programs and multi-line functions CAN be tokenized. If Tokenize is set to
  True (default), the data is tokenized, if it is set to False, the data is
  left in text form.
The expected format is as follows:
* For text files (ft89t):
 Line 1
 Line 2
CCommand
 Line 4
* For algebraic expressions (ft89e89l89m89s):
expression
* For one-line functions (ft89p89f, no Func or Prgm):
(arglist)
function of arglist
* For multi-line functions (ft89p89f with Func):
(arglist)
Func
instructions
EndFunc
* For multi-line programs (ft89p89f with Prgm):
(arglist)
Prgm
instructions
EndPrgm
WARNING: The expected line-ending is CR-only. Use CRLFToCROnly to convert
         Windows CR-LF line endings to the expected CR-only line ending.
WARNING2: Token does not do any kind of syntax-checking. It just converts 
		  the readable Text into a tokenized form. Therefore you will not get	
		  any kind of errormessages during the tokenizing process. Syntax-Errors
		  will lead to runtime-errors on the TI.

iii) Public Sub SaveTI(ByVal FN$, ByVal TIFolder$, ByVal TIName$, ByVal S$,
                       ByVal FType As FTypes, [Tokenize As Boolean = True])

Takes a file name FN, an on-calc folder name TIFolder, an on-calc file name
TIName, a string of readable data S, a file type FType (as defined in the
FTypes enumeration above), and an optional parameter Tokenize. Converts the
data S to the format expected by AMS and saves it to the file named FN, and
sets the on-calc folder name and the on-calc file name to TIFolder and
TIFile, respectively. The expected format and the effects of the Tokenize
parameter are the same as for Token. This function simply calls Token and
WriteTIVar. Using this function is the recommended way to save a file using
Tokens89 OCX.
WARNING: The expected line-ending is CR-only. Use CRLFToCROnly to convert
         Windows CR-LF line endings to the expected CR-only line ending.

4) Functions for TI-GraphLink 7-bit-ASCII interchange format

i) Public Function OpenTIGLAscii$(ByVal FN$)

Takes the file name of a program or function in TI-GraphLink 7-bit-ASCII
interchange format. Returns the data in readable form. Converts the 7-bit
escape sequences to their equivalent 8-bit characters. The data returned
uses CR-LF line endings.

ii) Public Function GetTIGLAsciiName$(ByVal FN$)

Takes the file name of a variable in TI-GraphLink 7-bit-ASCII interchange
format. Returns its on-calc file name.
NOTE: The TI-GraphLink 7-bit-ASCII interchange format does not contain a
      folder name. I recommend simply setting the folder name to "main".

iii) Public Sub SaveTIGLAscii(ByVal FN$, ByVal TIName$, ByVal S$)

Takes a file name FN, an on-calc file name TIName and a string of a
program or function in readable form. Saves the program or function to the
TI-GraphLink 7-bit-ASCII interchange format. Converts the 8-bit characters
to their equivalent 7-bit escape sequences. The expected line ending is
CR-LF.
NOTE: There is no TIFolder argument because the TI-GraphLink 7-bit-ASCII
      interchange format does not contain a folder name.

5) Utility Functions

i) Public Function CRLFToCROnly$(ByVal S$)

Converts the line-endings in S from CR-LF to CR-only and returns the
converted string.

ii) Public Function CROnlyToCRLF$(ByVal S$)

Converts the line-endings in S from CR-only to CR-LF and returns the
converted string.

IV. History
----------

v.1.00.0009 (2014-09-29): * Binary-compatible with v.1.00.0000, 1.00.0001,
                            1.00.0002, 1.00.0003, 1.00.0004, 1.00.0005, 1.00.0006,
                            1.00.0007, 1.00.0008.
              * Bugfix: 12°34'56" conversion
              * Bugfix: counting number of parenthesis
              * Bugfix: [3,/_pi/4] recognition
              * Bugfix: All file types can be saved in *.txt format
              * Bugfix: Folder name is saved in *.txt files too.
v.1.00.0008 (2014-09-10): * Binary-compatible with v.1.00.0000, 1.00.0001,
                            1.00.0002, 1.00.0003, 1.00.0004, 1.00.0005, 1.00.0006,
                            1.00.0007.
              * Bugfix: TypeID for Prgm/Func
              * Bugfix: Flag for tokenized files  
v.1.00.0007 (2014-03-07): * Binary-compatible with v.1.00.0000, 1.00.0001,
                            1.00.0002, 1.00.0003, 1.00.0004, 1.00.0005, 1.00.0006.
						  * Bugfix: negative floating point values in lists/matrices
v.1.00.0006 (2013-09-12): * Binary-compatible with v.1.00.0000, 1.00.0001,
                            1.00.0002, 1.00.0003, 1.00.0004, 1.00.0005.
						  * Changed: WriteTIVar now uses attribute RAM instead of archive
						  * Bugfix: WriteTIVar saves the tokenized flag in the fileheader
						  * Changed: DeToken inserts a space before terms beginning with |>, |>DMS e.q.
						  * Bugfix: DeToken handles .+, .-, .*, ./, .^ correctly
						  * Bugfix: Token handles .+, .-, .*, ./, .^ correctly
						  * Bugfix: Fixed tokenization of PowerReg, list>mat, regCoef[1],
									floating point values, 0b11011, 0hf7e4, sin^-1(y),
									f(.5), boolean expressions and local variables, programs and functions
									(Thanks to Kevin Kofler for his help.)
v.1.00.0005 (2004-12-01): * Binary-compatible with v.1.00.0000, 1.00.0001,
                            1.00.0002, 1.00.0003, 1.00.0004.
                          * Bugfix: Fixed tokenization of square roots,
                                    differentiation and integration.
                                    (Thanks to Nils Hahnfeld for the bug
                                    report.)
v.1.00.0004 (2003-10-20): * Binary-compatible with v.1.00.0000, 1.00.0001,
                            1.00.0002, 1.00.0003.
                          * Bugfix: Fixed tokenization and detokenization
                                    of floating-point values.
v.1.00.0003 (2003-08-05): * Binary-compatible with v.1.00.0000, 1.00.0001,
                            1.00.0002.
                          * Bugfix: Closing the file when an error occurs
                                    in OpenTIGLAscii. (Thanks to José A.
                                    Miranda for the report and the fix.)
                          * Bugfix: Indentation is limited to 255
                                    characters.
                          * Bugfix: Now tokenizing comments in the middle
                                    of a line correctly.
                          * Now handling indentation in the line following
                            a comment.
                          * Now handling spaces in front of a (C) comment
                            sign (in both tokenization and detokenization).
v.1.00.0002 (2003-05-11): * Binary-compatible with v.1.00.0000, 1.00.0001.
                          * Bugfix: Variables with a name containing some
                                    Greek characters were not tokenized
                                    correctly.
                          * The comment string in 89?/9x?/v2? files is now
                            null-terminated.
v.1.00.0001 (2003-05-10): * Binary-compatible with v.1.00.0000.
                          * Now handling indentation at the beginning of a
                            line (in both tokenization and detokenization).
                          * Bugfix: There were junk characters when opening
                                    an untokenized program. (I did not
                                    notice them before because they were
                                    preceded by a Chr(0), so the TextEdit
                                    control removed them during my tests.)
                          * Bugfix: There were junk characters at the end
                                    of the on-calc folder and file names
                                    returned by GetTIFolder and GetTIVar.
                                    (I did not notice them either, for the
                                    same reason as above.)
v.1.00.0000 (2003-05-04): * Initial public release.

V. License
----------

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

VI. Credits
-----------

Thanks to Gareth James and Zeljko Juric for their work on documenting the
TI-89/92+/V200 tokenized format.

VII. Contact
------------

e-mail: kevin.kofler@chello.at or Kevin@tigcc.ticalc.org
E-Mail: tiedit@arcor.de (Peter Engels)
webpage: http://members.chello.at/gerhard.kofler/kevin/pcprogs/