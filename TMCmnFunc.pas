unit TMCmnFunc;

{
  Aestan Tray Menu
  Made by Onno Broekmans; visit http://www.xs4all.nl/~broekroo/aetraymenu
  for more information.

  This work is hereby released into the Public Domain. To view a copy of the
  public domain dedication, visit:
      http://creativecommons.org/licenses/publicdomain/
  or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford,
  California 94305, USA.

  This unit contains some general functions that are mainly used by the
  TMConfig unit.
}

{
  NOTE:
  Lots of code from this unit are based on the Inno Setup source code,
  which was written by Jordan Russell (portions by Martijn Laan).
  Inno Setup is a great, free installer for Windows; see:
    http://www.jrsoftware.org
  >>PLEASE DO NOT REMOVE THIS NOTE<<
}

{$WARN UNIT_PLATFORM OFF}

interface

uses
  Windows, SysUtils, Classes, Contnrs,
  TMStruct;

type
  TBreakStringRec = record
    ParamName: String;
    ParamData: String;
  end;
  PBreakStringArray = ^TBreakStringArray;
  TBreakStringArray = array[0..15] of TBreakStringRec;

  TParamInfo = record
    Name: PChar;
    Flags: set of (piNoEmpty, piNoQuotes);
  end;

function AddBackslash(const S: String): String;
function AdjustLength(var S: String; const Res: Cardinal): Boolean;
function ExpandVariables(S: String; Variables: TObjectList): String;
function ExtractFlag(var S: String; const FlagStrs: array of PChar): Integer;
function ExtractStr(var S: String): String;
function FindCmdSwitch(const Switch: String; const Default: String = ''): String;
function GetCmdFileName: String;
function GetCommonFiles: String;
function GetEnv(const EnvVar: String): String;
function GetPathFromRegistry(const Name: PChar): String;
function GetProgramFiles: String;
function GetSystemDir: String;
function GetSystemDrive: String;
function GetTempDir: String;
function GetWinDir: String;
//function InstExec(const Filename, Params: String; WorkingDir: String;
//  const ShowCmd: Integer; var ResultCode: Integer): Boolean;
function InstExec (const Filename, Params: String; WorkingDir: String;
  const WaitUntilTerminated, WaitUntilIdle: Boolean; const ShowCmd: Integer;
  const ProcessMessagesProc: TProcedure; var ResultCode: Integer): Boolean;
function InstShellExec(const Filename, Params, Verb: String; WorkingDir: String;
  const ShowCmd: Integer; var ErrorCode: Integer): Boolean;
function InternalRegQueryStringValue (H: HKEY; Name: PChar; var ResultStr: String;
  Type1, Type2: DWORD): Boolean;
function KillInstance(const ID: String): Boolean;
function RegQueryStringValue(H: HKEY; Name: PChar; var ResultStr: String): Boolean;
function RemoveBackslash(const S: String): String;
function RemoveBackslashUnlessRoot(const S: String): String;
function RemoveQuotes(const S: String): String;
procedure SeparateDirective(const Line: PChar; var Key, Value: String);
procedure StringChange(var S: String; const FromStr, ToStr: String);

implementation

uses ShellApi, JclStrings, StrUtils, FileCtrl, RegStr, Messages;

function RemoveQuotes(const S: String): String;
{ Opposite of AddQuotes; removes any quotes around the string. }
begin
  Result := S;
  while (Result <> '') and (Result[1] = '"') do
    Delete (Result, 1, 1);
  while (Result <> '') and (AnsiLastChar(Result)^ = '"') do
    SetLength (Result, Length(Result)-1);
end;

procedure SeparateDirective(const Line: PChar; var Key, Value: String);
var
  P, P2: PChar;
  L: Cardinal;
begin
  Key := '';
  Value := '';
  P := Line;
  while (P^ <> #0) and (P^ <= ' ') do
    Inc (P);
  if P^ = #0 then
    Exit;
  P2 := P;
  while (P2^ <> #0) and (P2^ <> '=') do
    Inc (P2);
  L := P2 - P;
  SetLength (Key, L);
  Move (P^, Key[1], Length(Key));
  Key := TrimRight(Key);
  if P2^ = #0 then
    Exit;
  P := P2 + 1;
  while (P^ <> #0) and (P^ <= ' ') do
    Inc (P);
  if P^ = #0 then
    Exit;
  Value := TrimRight(StrPas(P));
end;

function ExtractStr(var S: String): String;
var
  I: Integer;
begin
  I := Pos(' ', S);
  if I = 0 then I := Length(S)+1;
  Result := Trim(Copy(S, 1, I-1));
  S := Trim(Copy(S, I+1, Maxint));
end;

function ExtractFlag(var S: String; const FlagStrs: array of PChar): Integer;
var
  I: Integer;
  F: String;
begin
  F := ExtractStr(S);
  if F = '' then begin
    Result := -2;
    Exit;
  end;

  Result := -1;
  for I := 0 to High(FlagStrs) do
    if StrIComp(FlagStrs[I], PChar(F)) = 0 then begin
      Result := I;
      Break;
    end;
end;

function RemoveBackslash(const S: String): String;
{ Removes the trailing backslash from the string, if one exists }
begin
  Result := S;
  if (Result <> '') and (AnsiLastChar(Result)^ = '\') then
    SetLength (Result, Length(Result)-1);
end;

procedure StringChange(var S: String; const FromStr, ToStr: String);
{ Change all occurrences in S of FromStr to ToStr }
var
  StartPos, I: Integer;
label 1;
begin
  if FromStr = '' then Exit;
  StartPos := 1;
1:for I := StartPos to Length(S)-Length(FromStr)+1 do begin
    if Copy(S, I, Length(FromStr)) = FromStr then begin
      Delete (S, I, Length(FromStr));
      Insert (ToStr, S, I);
      StartPos := I + Length(ToStr);
      goto 1;
    end;
  end;
end;

function InstExec (const Filename, Params: String; WorkingDir: String;
  const WaitUntilTerminated, WaitUntilIdle: Boolean; const ShowCmd: Integer;
  const ProcessMessagesProc: TProcedure; var ResultCode: Integer): Boolean;
var
  CmdLine: String;
  WorkingDirP: PChar;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  Result := True;
  CmdLine := '"' + Filename + '"';
  if Params <> '' then
    CmdLine := CmdLine + ' ' + Params;
  if WorkingDir = '' then
    WorkingDir := RemoveBackslashUnlessRoot(ExtractFilePath(Filename));
  FillChar (StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := ShowCmd;
  if WorkingDir <> '' then
    WorkingDirP := PChar(WorkingDir)
  else
    WorkingDirP := nil;
  if not CreateProcess(nil, PChar(CmdLine), nil, nil, False, 0, nil,
     WorkingDirP, StartupInfo, ProcessInfo) then begin
    Result := False;
    ResultCode := GetLastError;
    Exit;
  end;
  with ProcessInfo do begin
    { Don't need the thread handle, so close it now }
    CloseHandle (hThread);
    if WaitUntilIdle then
      WaitForInputIdle (hProcess, INFINITE);
    if WaitUntilTerminated then
      { Wait until the process returns, but still process any messages that
        arrive. }
      repeat
        { Process any pending messages first because MsgWaitForMultipleObjects
          (called below) only returns when *new* messages arrive }
        if Assigned(ProcessMessagesProc) then
          ProcessMessagesProc;
      until MsgWaitForMultipleObjects(1, hProcess, False, INFINITE, QS_ALLINPUT) <> WAIT_OBJECT_0+1;
    { Get the exit code. Will be set to STILL_ACTIVE if not yet available }
    GetExitCodeProcess(hProcess, DWORD(ResultCode));
    { Then close the process handle }
    CloseHandle (hProcess);
  end;
end;

function InstShellExec(const Filename, Params, Verb: String; WorkingDir: String;
  const ShowCmd: Integer; var ErrorCode: Integer): Boolean;
var
  WorkingDirP: PChar;
  E: Integer;
begin
  if WorkingDir = '' then
    WorkingDir := RemoveBackslashUnlessRoot(ExtractFilePath(Filename));
  if WorkingDir <> '' then
    WorkingDirP := PChar(WorkingDir)
  else
    WorkingDirP := nil;
  E := ShellExecute(0, PChar(Verb), PChar(Filename), PChar(Params), WorkingDirP,
    ShowCmd);
  Result := E > 32;
  if not Result then
    ErrorCode := E;
end;

function RemoveBackslashUnlessRoot(const S: String): String;
{ Removes the trailing backslash from the string, if one exists and does
  not specify a root directory of a drive (i.e. "C:\"}
var
  L: Integer;
begin
  Result := S;
  L := Length(Result);
  if L < 2 then
    Exit;
  if (AnsiLastChar(Result)^ = '\') and
     ((Result[L-1] <> ':') or (ByteType(Result, L-1) <> mbSingleByte)) then
    SetLength (Result, L-1);
end;

function ExpandVariables(S: String; Variables: TObjectList): String;
var
  I: Integer;
begin
  if Length(S) = 0 then
  begin
    Result := '';
    Exit;
  end;
  for I := 0 to Variables.Count - 1 do
    with Variables[I] as TVariableBase do
    begin
      if (vfIsPath in Flags) and (RightStr(Value, 1) = '\') then
        StrReplace(S, StrQuote(Name, '%') + '\', StrQuote(Name, '%'),
          [rfIgnoreCase, rfReplaceAll]);
      if AnsiPos(StrQuote(Name, '%'), S) <> 0 then
        StrReplace(S, StrQuote(Name, '%'), Value, [rfIgnoreCase, rfReplaceAll]);
    end;  //with variables[i]
  StrReplace(S, '%%', '%', [rfIgnoreCase, rfReplaceAll]);
  Result := S;
end;

function GetWinDir: String;
{ Returns fully qualified path of the Windows directory. Only includes a
  trailing backslash if the Windows directory is the root directory. }
var
  Buf: array[0..MAX_PATH-1] of Char;
begin
  GetWindowsDirectory (Buf, SizeOf(Buf));
  Result := StrPas(Buf);
end;

function GetSystemDir: String;
{ Returns fully qualified path of the Windows System directory. Only includes a
  trailing backslash if the Windows System directory is the root directory. }
var
  Buf: array[0..MAX_PATH-1] of Char;
begin
  GetSystemDirectory(Buf, SizeOf(Buf));
  Result := StrPas(Buf);
end;

function GetTempDir: String;
{ Returns fully qualified path of the temporary directory, with trailing
  backslash. This does not use the Win32 function GetTempPath, due to platform
  differences.

  Gets the temporary file path as follows:
  1. The path specified by the TMP environment variable.
  2. The path specified by the TEMP environment variable, if TMP is not
     defined or if TMP specifies a directory that does not exist.
  3. The Windows directory, if both TMP and TEMP are not defined or specify
     nonexistent directories.
}
begin
  Result := GetEnv('TMP');
  if (Result = '') or not DirectoryExists(Result) then
    Result := GetEnv('TEMP');
  if (Result = '') or not DirectoryExists(Result) then
    Result := GetWinDir;
  Result := AddBackslash(ExpandFileName(Result));
end;

function GetEnv(const EnvVar: String): String;
{ Gets the value of the specified environment variable. (Just like TP's GetEnv) }
var
  Res: DWORD;
begin
  SetLength (Result, 255);
  repeat
    Res := GetEnvironmentVariable(PChar(EnvVar), PChar(Result), Length(Result));
    if Res = 0 then begin
      Result := '';
      Break;
    end;
  until AdjustLength(Result, Res);
end;

function AddBackslash(const S: String): String;
{ Adds a trailing backslash to the string, if one wasn't there already.
  But if S is an empty string, the function returns an empty string. }
begin
  Result := S;
  if (Result <> '') and (AnsiLastChar(Result)^ <> '\') then
    Result := Result + '\';
end;

function AdjustLength(var S: String; const Res: Cardinal): Boolean;
{ Returns True if successful. Returns False if buffer wasn't large enough,
  and called AdjustLength to resize it. }
begin
  Result := Integer(Res) < Length(S);
  SetLength (S, Res);
end;

function GetProgramFiles: String;
{ Gets path of Program Files.
  Returns blank string if not found in registry. }
begin
  Result := GetPathFromRegistry('ProgramFilesDir');
end;

function GetCommonFiles: String;
{ Gets path of Common Files.
  Returns blank string if not found in registry. }
begin
  Result := GetPathFromRegistry('CommonFilesDir');
end;

function GetPathFromRegistry(const Name: PChar): String;
var
  H: HKEY;
begin
  if RegOpenKeyEx(HKEY_LOCAL_MACHINE, REGSTR_PATH_SETUP, 0,
     KEY_QUERY_VALUE, H) = ERROR_SUCCESS then begin
    if not RegQueryStringValue(H, Name, Result) then
      Result := '';
    RegCloseKey (H);
  end
  else
    Result := '';
end;

function RegQueryStringValue(H: HKEY; Name: PChar; var ResultStr: String): Boolean;
{ Queries the specified REG_SZ or REG_EXPAND_SZ registry key/value, and returns
  the value in ResultStr. Returns True if successful. When False is returned,
  ResultStr is unmodified. }
begin
  Result := InternalRegQueryStringValue(H, Name, ResultStr, REG_SZ,
    REG_EXPAND_SZ);
end;

function InternalRegQueryStringValue(H: HKEY; Name: PChar; var ResultStr: String;
  Type1, Type2: DWORD): Boolean;
var
  Typ, Size: DWORD;
  S: String;
begin
  Result := False;
  if (RegQueryValueEx(H, Name, nil, @Typ, nil, @Size) = ERROR_SUCCESS) and
     ((Typ = Type1) or (Typ = Type2)) then begin
    if Size < 2 then begin  {for the following code to work properly, Size can't be 0 or 1}
      ResultStr := '';
      Result := True;
    end
    else begin
      SetLength (S, Size-1); {long strings implicity include a null terminator}
      if RegQueryValueEx(H, Name, nil, nil, @S[1], @Size) = ERROR_SUCCESS then begin
        ResultStr := S;
        Result := True;
      end;
    end;
  end;
end;

function GetCmdFileName: String;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := AddBackslash(GetSystemDir) + 'cmd.exe'
  else
    Result := AddBackslash(GetWinDir) + 'COMMAND.COM';
end;

function GetSystemDrive: String;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := GetEnv('SystemDrive')
  else
    Result := '';
  if Result = '' then begin
    Result := ExtractFileDrive(GetWinDir);
    if Result = '' then
      { In some rare case that ExtractFileDrive failed, just default to C }
      Result := 'C:';
  end;
end;

function FindCmdSwitch(const Switch: String; const Default: String = ''): String;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to ParamCount do
    if UpperCase(Copy(ParamStr(I), 1, Length(Switch))) = UpperCase(Switch) then
    begin
      Result := Copy(ParamStr(I), Length(Switch) + 1, MaxInt);
      Exit;
    end;
  if Result = '' then
    Result := Default;
end;

function KillInstance(const ID: String): Boolean;
{ Kills another instance of the aetraymenu application }
var
  wndHandle: HWND;
begin
  Result := False;
  if ID = '' then
    Exit; //must specify an id

  wndHandle := FindWindow(nil, PChar('AeTrayMenu[' + ID + ']'));
  if wndHandle <> 0 then
  begin
    SendMessage(wndHandle, WM_CLOSE, 0, 0);
    Result := True;
  end;  //if wndhandle <> nil
end;

end.

