{ Component: TBarPopupMenu
  ===========================================================================

              Download the latest version of the component from:

            http://Mintus.Codefield.com/download/BarPopupMenu.zip

  The contents of this file are subject to the Bluecave Public License V 1.0
  (the "License"). You may not copy or use this file, in either source code
  or executable form, except in compliance with the License. You may obtain
  a copy of the License at http://www.bluecave.net/licenses/.

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the
  License.

  Copyright (C) 2000-2001 Bluecave Software. All Rights Reserved.
  http://www.bluecave.net/

  Copyright (C) 2000-12001 Jouni Airaksinen. All Rights Reserved.
  http://Mintus.Codefield.com/ -- Mintus@Codefield.com

  ===========================================================================

  Unit description:

    This is a continued part of my article at www.delphi3000.com. Article
    ID 1133, http://www.delphi3000.com/article.asp?id=1133

    TBarPopupMenu component for Delphi 5 and above. Be my guest and use
    it where-ever you like, just mention my name and e-mail somewhere in
    your software.

    There is a demo project at Demo\ directory.

  History:

    Look at History.txt.


  =========================================================================== }

{$I DFS.inc}
{$I BCDirectives10.inc}

{
    New properties in TBarPopupMenu:

    property Bitmap: TBitmap
      Bitmap to be positioned somewhere in the popupmenu (e.g. application
      logo)

    property BarWidth: Integer
      Width of bar on the left side of popupmenu

    property BarVisible: Boolean
      Toggles bar visibility. If it's false, menu looks like normal menu.

    property BitmapOffsetX: Integer
    property BitmapOffsetY: Integer
      Offset values to position bitmap

    property BitmapVertAlignment: TBitmapVertAlignment
    property BitmapHorzAlignment: TBitmapHorzAlignment
      Alignment values to position bitmap, fine tune position with Offsets

    property GradientEnd: TColor
    property GradientStart: TColor
      Gradient colors, if set to same color is drawn with one FillRect call

    property Transparent: Boolean
      Transparency of Bitmap

    property VerticalFont: TFont
    property VerticalText: string
      Vertical text and it's font on the bar

    property VerticalTextOffsetY: Integer
      Offset for vertical text, normally you want it to be negative e.g.
      defaults to -6.


    Notes about root Items:


      Otherwise, you can use Actions and other stuff normally.

  =========================================================================== }

unit BarPopupMenu;

interface

uses
  Windows, SysUtils, Classes, Graphics, Menus, Forms;

const
  BarSpace = 2;
  DefaultStart = clBlue;
  DefaultEnd = clBlack;

  cExpand = '#'; { ExpandMenuConst.pas }

type
  TBitmapVertAlignment = (bvaTop, bvaBottom, bvaMiddle);
  TBitmapHorzAlignment = (bhaLeft, bhaRight, bhaCenter);

  TBarPopupMenu = class(TPopupMenu)
  private
    { Private declarations }
    FBitmap: TBitmap;
    FBitmapOffsetX,
    FBitmapOffsetY: Integer;
    FBitmapVertAlignment: TBitmapVertAlignment;
    FBitmapHorzAlignment: TBitmapHorzAlignment;

    FclStart,
    FclEnd: TColor;

    FVerticalText: string;
    FVerticalFont: TFont;
{    FVerticalTextOffsetX,}
    FVerticalTextOffsetY: Integer;
    FBarWidth: Integer;
    FBarVisible: Boolean;

    PopupHeight: Integer;
    Drawn: Boolean;
    DrawGlyph: Boolean;

    FBarBitmap: TBitmap;
  protected
    { Protected declarations }
    procedure ExpandItemWidth(Sender: TObject; ACanvas: TCanvas; var Width,
      Height: Integer);
    procedure AdvancedDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
      State: TOwnerDrawState);

    procedure SetTransparent(Value: Boolean);
    function GetTransparent: Boolean;

    function GetBitmap: TBitmap;
    procedure SetBitmap(Value: TBitmap);

    procedure SetBarVisible(Value: Boolean);

    procedure SetVerticalFont(Value: TFont);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Popup(X, Y: Integer); override;
//    property OwnerDraw;
  published
    { Published declarations }
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property BarWidth: Integer read FBarWidth write FBarWidth default 31;
    property BitmapOffsetX: Integer read FBitmapOffsetX write FBitmapOffsetX default 0;
    property BitmapOffsetY: Integer read FBitmapOffsetY write FBitmapOffsetY default 0;
    property BitmapVertAlignment: TBitmapVertAlignment read FBitmapVertAlignment
      write FBitmapVertAlignment default bvaBottom;
    property BitmapHorzAlignment: TBitmapHorzAlignment read FBitmapHorzAlignment
      write FBitmapHorzAlignment default bhaLeft;
    property BarVisible: Boolean read FBarVisible write SetBarVisible default True;
    property GradientEnd: TColor read FclEnd write FclEnd default DefaultEnd;
    property GradientStart: TColor read FclStart write FclStart default DefaultStart;
    property Transparent: Boolean read GetTransparent write SetTransparent default True;
    property VerticalFont: TFont read FVerticalFont write SetVerticalFont;
    property VerticalText: string read FVerticalText write FVerticalText;
    property VerticalTextOffsetY: Integer read FVerticalTextOffsetY
      write FVerticalTextOffsetY default -6;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Bluecave', [TBarPopupMenu]);
end;

constructor TBarPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  OwnerDraw := True;

  FBitmapOffsetX := 0;
  FBitmapOffsetY := 0;
  FBitmapVertAlignment := bvaBottom;
  FBitmapHorzAlignment := bhaLeft;

  FVerticalFont := TFont.Create;
  with FVerticalFont do
  begin
    Name := 'Tahoma';
    Size := 14;
    Color := clWhite;
    Style := [fsBold, fsItalic];
  end;

  FVerticalTextOffsetY := -6;

  FclStart := DefaultStart;
  FclEnd := DefaultEnd;

  FBarWidth := 31;

  FBarVisible := True;
  FBarBitmap := TBitmap.Create;

  if (Application.Handle <> 0) then
    FVerticalText := Application.Title; { some defaults }
end;

destructor TBarPopupMenu.Destroy;
begin
  FVerticalFont.Free;
  if Assigned(FBitmap) then FBitmap.Free;
  inherited Destroy;
end;

procedure TBarPopupMenu.SetTransparent(Value: Boolean);
begin
  if FBitmap = nil then Exit;
  if (Value <> FBitmap.Transparent) then
    FBitmap.Transparent := Value;
end;

function TBarPopupMenu.GetTransparent: Boolean;
begin
  if FBitmap = nil then
    Result := False
  else
    Result := FBitmap.Transparent;
end;

procedure TBarPopupMenu.SetBitmap(Value: TBitmap);
begin
  if FBitmap = nil then
  begin
    FBitmap := TBitmap.Create;
    FBitmap.Transparent := True;
  end;
{  if Value = nil then
    FBitmap.Free
  else}
    FBitmap.Assign(Value);
end;

function TBarPopupMenu.GetBitmap: TBitmap;
begin
  if FBitmap = nil then
  begin
    FBitmap := TBitmap.Create;
    FBitmap.Transparent := True;
  end;
  Result := FBitmap;
end;

procedure TBarPopupMenu.SetVerticalFont(Value: TFont);
begin
  FVerticalFont.Assign(Value);
end;

procedure TBarPopupMenu.SetBarVisible(Value: Boolean);
begin
  if FBarVisible = Value then Exit;
  if not FBarVisible then
    FBarBitmap.Free
  else if not Assigned(FBarBitmap) then
    FBarBitmap := TBitmap.Create;

  FBarVisible := Value;
end;

{ ============================================================================
  CreateRotatedFont
  Date: 2000-06-22
  Description: Creates rotated font, returns handle to it
  Parameters:
    F: TFont, where to copy styles
    Angle: Integer, font angle
  ---------------------------------------------------------------------------- }
function CreateRotatedFont(F: TFont; Angle: Integer): hFont;
var
  LF : TLogFont;
begin
  FillChar(LF, SizeOf(LF), #0);
  with LF do
  begin
    lfHeight := F.Height;
    lfWidth := 0;
    lfEscapement := Angle*10;
    lfOrientation := 0;
    if fsBold in F.Style then
      lfWeight := FW_BOLD
    else
      lfWeight := FW_NORMAL;
    lfItalic := Byte(fsItalic in F.Style);
    lfUnderline := Byte(fsUnderline in F.Style);
    lfStrikeOut := Byte(fsStrikeOut in F.Style);
    lfCharSet := DEFAULT_CHARSET;
    StrPCopy(lfFaceName, F.Name);
    lfQuality := DEFAULT_QUALITY;

    lfOutPrecision := OUT_DEFAULT_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    case F.Pitch of
      fpVariable: lfPitchAndFamily := VARIABLE_PITCH;
      fpFixed: lfPitchAndFamily := FIXED_PITCH;
    else
      lfPitchAndFamily := DEFAULT_PITCH;
    end;
  end;
  Result := CreateFontIndirect(LF);
end;


{ ============================================================================
  TBarPopupMenu.Popup
  Date: 2000-06-22
  Description:
    Set initial events for popup items. Currently just overwrites old events..
  ---------------------------------------------------------------------------- }
procedure TBarPopupMenu.Popup(X, Y: Integer);
var i: Integer;
begin
  PopupHeight := 0;
  Drawn := False;
  DrawGlyph := False;

  if (Items.Count > 0) then
    for i := 0 to Items.Count-1 do
    begin
      Items[i].OnMeasureItem := ExpandItemWidth;
      Items[i].OnAdvancedDrawItem := AdvancedDrawItem;

      { check if there are glyphs, if not then we need to set extra space to
        item (bug in VCL?) }
      if {(not DrawGlyph) and} ((Assigned(Images) and (Items[i].ImageIndex > -1)) or
        (Assigned(FBitmap) and not FBitmap.Empty)) then DrawGlyph := True;
    end;

{  if not DrawGlyph then
    for i := 0 to Items.Count-1 do
      ModifyMenu(Items.Handle, i, MF_POSITION);}

  inherited Popup(X, Y);
end;

procedure TBarPopupMenu.ExpandItemWidth(Sender: TObject;
  ACanvas: TCanvas; var Width, Height: Integer);
var
  MenuItem: TMenuItem;
begin
  if not FBarVisible then Exit;

  Inc(Width, FBarWidth); { make space for graphical bar }

{  if not DrawGlyph then
    Inc(Width, 16 + 15 + 100);}

  MenuItem := TMenuItem(Sender);

  if MenuItem.Visible and (MenuItem.Caption <> cExpand) then
    PopupHeight := PopupHeight + Height;
end;

procedure TBarPopupMenu.AdvancedDrawItem(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
var
  i, itmp, x, y: Integer;
  r: TRect;
  rc1, rc2, gc1, gc2, bc1, bc2: Byte;
  ColorStart, ColorEnd: Longint;
  OnAdvancedDrawItem: TAdvancedMenuDrawItemEvent;
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem(Sender);

  if MenuItem.Caption = cExpand then Exit; { it has been already drawn }

  { we need to remove draw event so DrawMenuItem won't generate infinite loop!
    (Recursive) }
  OnAdvancedDrawItem := MenuItem.OnAdvancedDrawItem;
  MenuItem.OnAdvancedDrawItem := nil;

  { align rect where item is draw so that vcl will leave bar for us }
  r := ARect;
  if FBarVisible then
  begin
    r.Right := r.Right - FBarWidth; { remove bar width }
    OffsetRect(r, FBarWidth, 0);
  end;

  { draw item and restore event back }
  DrawMenuItem(MenuItem, ACanvas, r, State);
  MenuItem.OnAdvancedDrawItem := OnAdvancedDrawItem;

  if not FBarVisible then Exit;

  { set doublebuffer bitmap to right size }
  if (FBarBitmap.Height <> PopupHeight) then
    FBarBitmap.Height := PopupHeight;

  if (FBarBitmap.Width <> (BarWidth - BarSpace)) then
    FBarBitmap.Width := BarWidth - BarSpace;
  with FBarBitmap.Canvas do
    if not Drawn then
    begin
      Brush.Style := bsSolid;
      if (FclStart = FclEnd) then { same color, just one fillrect required }
        begin
          Brush.Color := FclStart;
          FillRect(Rect(0, ARect.Top, FBarWidth - BarSpace, ARect.Bottom));
        end
      else { draw smooth gradient bar part for this item }
      begin
        ColorStart := ColorToRGB(FclStart);
        ColorEnd := ColorToRGB(FclEnd);

        rc1 := GetRValue(ColorStart);
        gc1 := GetGValue(ColorStart);
        bc1 := GetBValue(ColorStart);
        rc2 := GetRValue(ColorEnd);
        gc2 := GetGValue(ColorEnd);
        bc2 := GetBValue(ColorEnd);

        for i := 0 to (ARect.Bottom - ARect.Top) do
        begin
          Brush.Color := RGB(
            (rc1 + (((rc2 - rc1) * (ARect.Top + i)) div PopupHeight)),
            (gc1 + (((gc2 - gc1) * (ARect.Top + i)) div PopupHeight)),
            (bc1 + (((bc2 - bc1) * (ARect.Top + i)) div PopupHeight)));
          FillRect(Rect(0, ARect.Top + i, FBarWidth - BarSpace,
            ARect.Top + i + 1));
        end;
      end;

      { vertical text to gradient bar }
      with Font do
      begin
        Assign(FVerticalFont);

        itmp := Handle; { store old }
        Handle := CreateRotatedFont(Font, 90);

        { gives much better centering }
        x := Round((FBarWidth - TextHeight('X')) / 2 - 0.5);
  //      x := (FBarWidth - Canvas.TextHeight('X')) div 2;
      end;

      Brush.Style := bsClear;

      r := Rect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom + 1);

      y := PopupHeight + FVerticalTextOffsetY;
      if Assigned(FBitmap) and (FBitmapVertAlignment = bvaBottom) then
        y := y - FBitmap.Height;

      ExtTextOut(Handle, x - 1, y, ETO_CLIPPED, @r, PChar(VerticalText),
        Length(VerticalText), nil);

      { delete created font and restore old handle }
      DeleteObject(Font.Handle);
      Font.Handle := itmp;

      if PopupHeight = ARect.Bottom then
        begin
          Drawn := True;
          { draw bitmap }
          if Assigned(FBitmap) then
          begin
            y := 0; x := 0;
            case FBitmapVertAlignment of
              bvaTop:    y := FBitmapOffsetY;
              bvaBottom: y := PopupHeight + FBitmapOffsetY - FBitmap.Height;
              bvaMiddle: y := ((PopupHeight - Fbitmap.Height) div 2) + FBitmapOffsetY;
            end;

            case FBitmapHorzAlignment of
              bhaLeft:   x := FBitmapOffsetX;
              bhaRight:  x := (FBarWidth - BarSpace) + FBitmapOffsetX - FBitmap.Width;
              bhaCenter: x := ((FBarWidth - BarSpace - FBitmap.Width) div 2) + FBitmapOffsetX;
            end;

            Draw(x, y, FBitmap);
          end;

          { draw the double buffered bar now }
          r := Rect(0, 0, FBarBitmap.Width, FBarBitmap.Height);
          ACanvas.CopyRect(r, FBarBitmap.Canvas, r);
        end;
    end else { draw from double buffer }
    begin
      r := Rect(0, ARect.Top, FBarBitmap.Width, ARect.Bottom);
      ACanvas.CopyRect(r, FBarBitmap.Canvas, r);
    end;
end;

end.
