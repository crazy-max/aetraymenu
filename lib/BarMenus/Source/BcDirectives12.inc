{ Bluecave Common Compiler Directives Include file version 1.2
  ===========================================================================
  Created: 2001-05-09 (yyyy-mm-dd)
  ===========================================================================

  The contents of this file are subject to the Bluecave Public License V 1.1
  (the "License"). You may not copy or use this file, in either source code
  or executable form, except in compliance with the License. You may obtain
  a copy of the License at http://www.bluecave.net/licenses/.

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the
  License.

  Copyright (C) 2001-2003 Bluecave Software. All Rights Reserved.
  http://www.bluecave.net/

  Copyright (C) 2001-2003 Jouni Airaksinen. All Rights Reserved.
  http://www.bluecave.net/mintus/ -- Mintus@Codefield.com

  ===========================================================================

  Unit description:

    Common Compiler directives. File name is versioned so multiple versions
    of this file can co-exists.

    Requires DFS.inc to work properly.

  History:
    
    2002-08-13: - Fixed "$ObjExportAll On" bug when using DCC32.EXE.
    2001-07-11: - Added better debugging support. Mainly for MemProof.
                - Support for the MSWINDOWS definition for Delphi 5 and
                  earlier. Kylix and Delphi 6 or later has native support
                  for MSWINDOWS.
    2001-05-09: Initial version.

  =========================================================================== }

{$DEFINE BLUECAVE}

{ === Code Generation Directives === }

{$F-}    { Force Far Calls }
{$A+}    { Word Align Data }
{$U-}    { Pentium-Save FDIV }
{$K-}    { Smart Callbacks }

{$IFDEF BCB}
  {$ObjExportAll on}
{$ELSE}
  {$IFNDEF VER120}
    {$IFNDEF VER130}
      {$ObjExportAll on}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{ === Debugging Directives (Mainly for MemProof) === }

{ Note: If using MemProof, you need to enable Debug Information, Use Debug DCUs
        and Include TD32 Debug Information when compiling Applications, DLLs or
        Packages. }

{$IFOPT D+}
  {$O-}    { Optimization - Off }
  {$W+}    { Windows Stack Frame - On }
  {$C+}    { Assertions - On }
  {$L+}    { Local Symbols - On }
  {$YD}    { Symbol Reference Info, Definitions Only }
{$ELSE}
  {$O+}    { Optimization - On }
  {$W-}    { Windows Stack Frame - Off }
  {$C+}    { Assertions - Off }
  {$L-}    { Local Symbols - On }
  {$Y-}    { No Symbol Reference Info }
{$ENDIF}


{ === Runtime Errors === }

{$IFOPT D+}
  {$R+}    { Range Checking - On - if compiled with Debug Information }
  {$Q+}    { Overflow Checking - On }
  {$S+}    { Stack Checking - On }
{$ELSE}
  {$R-}    { Range Checking - Off - if compiled without Debug Information }
  {$Q-}    { Overflow Checking - Off }
  {$S-}    { Stack Checking - Off }
{$ENDIF}

{$I+}    { I/O Checking }


{ === Syntax Options === }

{$V+}    { Strict Var-Strings }
{$B-}    { Complete Boolean Evaluation. Note: overrides DFS.inc setting by purpose. }
{$X+}    { Extended Syntax }
{$T-}    { Typed @ Operator }
{$P+}    { Open Parameters }
{$IFDEF DFS_COMPILER_4_UP}
{$J+}    { Writeable Typed Constants }
{$ENDIF}

{ === Miscellaneous Directives === }

{$C MOVEABLE DEMANDLOAD DISCARDABLE}    { Code Segment Attribute }
{$G+}    { Delphi 1: 286 Instructions / Delphi 3 & later: Imported Data }
{$N+}    { Numeric Coprocessor }
{$Z-}    { Word Size Enumerated Types }

{$IFDEF WIN32}
{$H+}    { Long String Support }
{$ENDIF}

{ Compatibility for older Delphi compilers }
{$IFDEF WIN32}
  {.$IFNDEF DFS_COMPILER_6_UP}
    {$DEFINE MSWINDOWS}
  {.$ENDIF}
{$ENDIF}

{$IFDEF WIN16}
  {$DEFINE MSWINDOWS}
{$ENDIF}
