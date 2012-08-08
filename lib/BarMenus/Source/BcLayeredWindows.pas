{ Unit: BcLayeredWindows
  ===========================================================================
  Created: 2001-07-29 (yyyy-mm-dd)
  ===========================================================================

  The contents of this file are subject to the Bluecave Public License V 1.1
  (the "License"). You may not copy or use this file, in either source code
  or executable form, except in compliance with the License. You may obtain
  a copy of the License at http://www.bluecave.net/licenses/.

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the
  License.

  Copyright (C) 2001-2002 Bluecave Software. All Rights Reserved.
  http://www.bluecave.net/

  Copyright (C) 2001-2002 Jouni Airaksinen. All Rights Reserved.
  http://Mintus.Codefield.com/ -- Mintus@Codefield.com

  ===========================================================================

  Unit description:

    Needed definitions for Layered Windows API.

    Notes: This is not complete api as it has been made for BarMenu Components.
      It might be expanded later. API is loaded dynamically, so be sure
      to check the SupportsLayeredWindows global to determine if LayeredWindows
      API is available.

    Layered Windows Bug Note:
      I'm not sure if it's just my computer (Radeon VE) and Windows 2000,
      but if the layered window flag is not set for window to be used with
      UpdateLayeredWindow on the window creating (CreateParams) then all
      windows in the system with SetLayeredWindowsAttributes will have
      very jerky/choppy update until the flag is reset.

      I did notice this when creating On-screen-displays for my Bluecave
      Winamp Slider! plug-in while the Slider! fader was enabled.

      This is not big deal as the UpdateLayeredWindow windows usually
      have always the layered flag set because of their special nature.
      Just be sure to override the CreateParams and set the flag after
      inherited call: 

      procedure TForm1.CreateParams(var Params: TCreateParams);
      begin
        inherited CreateParams(Params);
        Params.ExStyle := Params.ExStyle or WS_EX_LAYERED;
      end;



  History:

    2001-10-30: Added SetWindowLayered support function
    2001-10-11: Added UpdateLayeredWindow
    2001-07-29: Initial version.

  =========================================================================== }

{$I DFS.inc}
{$I BcDirectives.inc}

unit BcLayeredWindows;

interface

{$IFDEF MSWINDOWS}

uses Windows;

const
  LWA_ALPHA = $00000002;
  LWA_COLORKEY  = $00000001;
  ULW_COLORKEY  = $00000001;
  ULW_ALPHA     = $00000002;
  ULW_OPAQUE    = $00000004;
  AC_SRC_ALPHA  = $01;
  WS_EX_LAYERED = $00080000;


function SupportsLayeredWindows: Boolean;

function SetWindowLayered(Handle: THandle; Value: Boolean): Boolean;

type
  TOpacity = 0..255;

  TSetLayeredWindowAttributes = function(hWnd: THandle; crKey: TColorRef; bAlpha: Byte;
    dwFlags: Cardinal): BOOL; stdcall;
  TUpdateLayeredWindow = function(hWnd: THandle; hdcDst: HDC; pptDst: PPoint;
    psize: PSize; hdcSrc: HDC; pptSrc: PPoint; crKey: TColorRef;
    pblend: PBlendFunction; dwFlags: Cardinal): BOOL; stdcall;

var
  UpdateLayeredWindow: TUpdateLayeredWindow = nil;
  SetLayeredWindowAttributes: TSetLayeredWindowAttributes = nil;
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
var
  User32Dll: HMODULE;
  FSupportsLayeredWindows: Boolean;

function SupportsLayeredWindows: Boolean;
begin
  Result := FSupportsLayeredWindows;
end;

function SetWindowLayered(Handle: THandle; Value: Boolean): Boolean;
var Flags: Integer;
begin
  Result := True;
  Flags := GetWindowLong(Handle, GWL_EXSTYLE);
  if Value then
  begin
    if ((Flags and WS_EX_LAYERED) = 0) then
      SetWindowLong(Handle, GWL_EXSTYLE, Flags or WS_EX_LAYERED)
  end else if (Flags and WS_EX_LAYERED) <> 0 then
    SetWindowLong(Handle, GWL_EXSTYLE, Flags and not WS_EX_LAYERED);
end;

initialization
  User32Dll := LoadLibrary(user32);
  FSupportsLayeredWindows := False;
  if User32Dll <> 0 then
  begin
    UpdateLayeredWindow := GetProcAddress(User32Dll, 'UpdateLayeredWindow');
    SetLayeredWindowAttributes := GetProcAddress(User32Dll, 'SetLayeredWindowAttributes');
    FreeLibrary(User32Dll);

    FSupportsLayeredWindows := Assigned(SetLayeredWindowAttributes);
  end;

finalization
{$ENDIF}

end.
