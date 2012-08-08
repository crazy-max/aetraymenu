{ Unit: BcRectUtilities
  ===========================================================================
  Created: 2001-07-11 (yyyy-mm-dd)
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

  History:

    2001-07-11: Initial version.

  =========================================================================== }

{$I DFS.inc}
{$I BcDirectives.inc}

unit BcRectUtilities;

interface

uses Classes, SysUtils, Windows, Graphics;

function RectWidth(const ARect: TRect): Integer;
function RectHeight(const ARect: TRect): Integer;
function RectInRect(const Source, Target: TRect): Boolean;

function BitmapRect(const ABitmap: TBitmap): TRect;

var NilRect: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);

implementation

function RectInRect(const Source, Target: TRect): Boolean;
begin
  Result := (PtInRect(Target, Source.TopLeft) and PtInRect(Target, Source.BottomRight));
end;

function RectWidth(const ARect: TRect): Integer;
begin
  Result := ARect.Right - ARect.Left;
end;

function RectHeight(const ARect: TRect): Integer;
begin
  Result := ARect.Bottom - ARect.Top;
end;

function BitmapRect(const ABitmap: TBitmap): TRect;
begin
  if Assigned(ABitmap) then
    Result := Rect(0, 0, ABitmap.Width, ABitmap.Height)
  else
    Result := Rect(0, 0, 0, 0);
end;

end.
