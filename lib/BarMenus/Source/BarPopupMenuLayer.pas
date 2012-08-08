{ Unit: BarPopupMenuLayer
  ===========================================================================
  Created: 2001-07-12 (yyyy-mm-dd)
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

    Do not use this unless really necessary. This is not supported anymore.
    It just maps the old properties to new location. The new Bar property
    is also available, so old properties ARE stored twice in the dfm
    resource! Only port to the new features is the Bar property. Period.

  History:
    2001-07-27: Changed base class to TBcBarPopupMenu so this won't require
      any maintainance anymore. Thus, making the component identical with
      TBcBarPopupMenu, but publising the old properties as mapped along all
      the new onces.
    2001-07-12: Initial version.

  =========================================================================== }

{$I DFS.inc}
{$I BcDirectives.inc}
{$I BarMenus.inc}

unit BarPopupMenuLayer;

interface

uses Classes, Graphics, BarMenus;

{$IFDEF OLDCLASS}
type
  { simple compatibility layer }
  TBarPopupMenu = class(TBcBarPopupMenu)
  private
    function GetBitmap: TBitmap;
    function GetBitmapHorzAlignment: THorzAlignment;
    function GetBitmapOffset(const Index: Integer): Integer;
    function GetBitmapVertAlignment: TVertAlignment;
    function GetTransparent: Boolean;
    function GetVerticalFont: TFont;
    function GetVerticalText: string;
    function GetVerticalTextOffset(const Index: Integer): Integer;
    procedure SetBitmap(const Value: TBitmap);
    procedure SetBitmapHorzAlignment(const Value: THorzAlignment);
    procedure SetBitmapOffset(const Index, Value: Integer);
    procedure SetBitmapVertAlignment(const Value: TVertAlignment);
    procedure SetTransparent(const Value: Boolean);
    procedure SetVerticalFont(const Value: TFont);
    procedure SetVerticalText(const Value: string);
    procedure SetVerticalTextOffset(const Index, Value: Integer);
    function GetBarWidth: Integer;
    function GetBarVisible: Boolean;
    function GetGradientColor(const Index: Integer): TColor;
    procedure SetGradientColor(const Index: Integer; const Value: TColor);
    procedure SetBarWidth(const Value: Integer);
    procedure SetBarVisible(const Value: Boolean);
  public

  published
    { mapped properties }
    property BarWidth: Integer read GetBarWidth write SetBarWidth default DefaultBarWidth;
    property BarVisible: Boolean read GetBarVisible write SetBarVisible default DefaultBarVisible;
    property GradientEnd: TColor index 0 read GetGradientColor write SetGradientColor default DefaultGradientStart;
    property GradientStart: TColor index 1 read GetGradientColor write SetGradientColor default DefaultGradientEnd;

    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property BitmapOffsetX: Integer index 0 read GetBitmapOffset write SetBitmapOffset default 0;
    property BitmapOffsetY: Integer index 1 read GetBitmapOffset write SetBitmapOffset default 0;
    property BitmapVertAlignment: TVertAlignment read GetBitmapVertAlignment
      write SetBitmapVertAlignment default DefaultBarBitmapVertAlignment;
    property BitmapHorzAlignment: THorzAlignment read GetBitmapHorzAlignment
      write SetBitmapHorzAlignment default DefaultBarBitmapHorzAlignment;
    property Transparent: Boolean read GetTransparent write SetTransparent default DefaultBarBitmapTransparent;
    property VerticalFont: TFont read GetVerticalFont write SetVerticalFont;
    property VerticalText: string read GetVerticalText write SetVerticalText;
    property VerticalTextOffsetY: Integer index 1 read GetVerticalTextOffset
      write SetVerticalTextOffset default DefaultBarCaptionOffsetY;
  end;
{$ENDIF}

implementation

{$IFDEF OLDCLASS}
uses BcExceptions;

{ TBarPopupMenu }
{ compatibility layer }

function TBarPopupMenu.GetBarWidth: Integer;
begin
  Result := Bar.Width;
end;

function TBarPopupMenu.GetBarVisible: Boolean;
begin
  Result := Bar.Visible;
end;

function TBarPopupMenu.GetBitmap: TBitmap;
begin
  Result := Bar.BarBitmap.Bitmap;
end;

function TBarPopupMenu.GetBitmapHorzAlignment: THorzAlignment;
begin
  Result := Bar.BarBitmap.HorzAlignment;
end;

function TBarPopupMenu.GetBitmapOffset(const Index: Integer): Integer;
begin
  case Index of
    0: Result := Bar.BarBitmap.OffsetX;
    1: Result := Bar.BarBitmap.OffsetY;
    else
      raise EInvalidPropertyIndex.CreateFmt(SInvalidPropertyIndex, [Index]);
  end;
end;

function TBarPopupMenu.GetBitmapVertAlignment: TVertAlignment;
begin
  Result := Bar.BarBitmap.VertAlignment;
end;

function TBarPopupMenu.GetGradientColor(const Index: Integer): TColor;
begin
  case Index of
    0: Result := Bar.GradientStart;
    1: Result := Bar.GradientEnd;
    else
      raise EInvalidPropertyIndex.CreateFmt(SInvalidPropertyIndex, [Index]);
  end;
end;

procedure TBarPopupMenu.SetGradientColor(const Index: Integer;
  const Value: TColor);
begin
  case Index of
    0: Bar.GradientStart := Value;
    1: Bar.GradientEnd := Value;
    else
      raise EInvalidPropertyIndex.CreateFmt(SInvalidPropertyIndex, [Index]);
  end;
end;

function TBarPopupMenu.GetTransparent: Boolean;
begin
  Result := Bar.BarBitmap.Transparent;
end;

function TBarPopupMenu.GetVerticalFont: TFont;
begin
  Result := Bar.BarCaption.Font;
end;

function TBarPopupMenu.GetVerticalText: string;
begin
  Result := Bar.BarCaption.Caption;
end;

function TBarPopupMenu.GetVerticalTextOffset(const Index: Integer): Integer;
begin
  case Index of
    1: Result := Bar.BarCaption.OffsetY;
    else
      raise EInvalidPropertyIndex.CreateFmt(SInvalidPropertyIndex, [Index]);
  end;
end;

procedure TBarPopupMenu.SetBarWidth(const Value: Integer);
begin
  Bar.Width := Value;
end;

procedure TBarPopupMenu.SetBarVisible(const Value: Boolean);
begin
  Bar.Visible := Value;
end;

procedure TBarPopupMenu.SetBitmap(const Value: TBitmap);
begin
  Bar.BarBitmap.Bitmap := Value;
end;

procedure TBarPopupMenu.SetBitmapHorzAlignment(const Value: THorzAlignment);
begin
  Bar.BarBitmap.HorzAlignment := Value;
end;

procedure TBarPopupMenu.SetBitmapOffset(const Index, Value: Integer);
begin
  case Index of
    0: Bar.BarBitmap.OffsetX := Value;
    1: Bar.BarBitmap.OffsetY := Value;
    else
      raise EInvalidPropertyIndex.CreateFmt(SInvalidPropertyIndex, [Index]);
  end;
end;

procedure TBarPopupMenu.SetBitmapVertAlignment(const Value: TVertAlignment);
begin
  Bar.BarBitmap.VertAlignment := Value;
end;

procedure TBarPopupMenu.SetTransparent(const Value: Boolean);
begin
  Bar.BarBitmap.Transparent := Value;
end;

procedure TBarPopupMenu.SetVerticalFont(const Value: TFont);
begin
  Bar.BarCaption.Font := Value;
end;

procedure TBarPopupMenu.SetVerticalText(const Value: string);
begin
  Bar.BarCaption.Caption := Value;
end;

procedure TBarPopupMenu.SetVerticalTextOffset(const Index, Value: Integer);
begin
  case Index of
    1: Bar.BarCaption.OffsetY := Value;
    else
      raise EInvalidPropertyIndex.CreateFmt(SInvalidPropertyIndex, [Index]);
  end;
end;

{$ENDIF}

end.
