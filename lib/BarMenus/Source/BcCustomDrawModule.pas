{ Unit: BcCustomDrawModule
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

    Simple example and useful drawmodule. Publishes events to do the
    drawing.

  History:

    2001-10-22: Initial version

  =========================================================================== }

{$I DFS.inc}
{$I BcDirectives.inc}

unit BcCustomDrawModule;

interface

uses Windows, Messages, Classes, SysUtils, Menus, Graphics, BcDrawModule, BarMenus;

type
  TBcCustomDrawModule = class(TBcBarMenusDrawModule)
  private
    FOnDrawMenuItem: TOnDrawMenuItemEvent;
    FOnMeasureMenuItem: TOnMeasureMenuItemEvent;
  protected
    procedure DrawMenuItem(AMenuItem: TMenuItem; ACanvas: TCanvas; ARect: TRect;
      State: TOwnerDrawState; ABarVisible: Boolean; var DefaultDraw: Boolean); override;
    procedure MeasureMenuItem(AMenuItem: TMenuItem; ACanvas: TCanvas;
      var Width, Height: Integer; ABarVisible: Boolean; var DefaultMeasure: Boolean); override;
  published
    property OnDrawMenuItem: TOnDrawMenuItemEvent read FOnDrawMenuItem write FOnDrawMenuItem;
    property OnMeasureMenuItem: TOnMeasureMenuItemEvent read FOnMeasureMenuItem write FOnMeasureMenuItem;
  end;

implementation

{ TBcCustomDrawModule }

procedure TBcCustomDrawModule.DrawMenuItem(AMenuItem: TMenuItem;
  ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState;
  ABarVisible: Boolean; var DefaultDraw: Boolean);
begin
  if Assigned(FOnDrawMenuItem) then
    FOnDrawMenuItem(Self, AMenuItem, ACanvas, ARect, State, ABarVisible,
    DefaultDraw);
end;

procedure TBcCustomDrawModule.MeasureMenuItem(AMenuItem: TMenuItem;
  ACanvas: TCanvas; var Width, Height: Integer; ABarVisible: Boolean; var DefaultMeasure: Boolean);
begin
  if Assigned(FOnMeasureMenuItem) then
    FOnMeasureMenuItem(Self, AMenuItem, ACanvas, Width, Height, ABarVisible, DefaultMeasure);
end;

end.
