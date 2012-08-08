{ Unit: BcXPMenuDrawModule
  ===========================================================================

  The contents of this file are subject to the Bluecave Public License V 1.1
  (the "License"). You may not copy or use this file, in either source code
  or executable form, except in compliance with the License. You may obtain
  a copy of the License at http://www.bluecave.net/licenses/.

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the
  License.

  Copyright (C) 2001 Bluecave Software. All Rights Reserved.
  http://www.bluecave.net/

  Copyright (C) 2001 Jouni Airaksinen. All Rights Reserved.
  http://Mintus.Codefield.com/ -- Mintus@Codefield.com

  ===========================================================================

  Unit description:

    Uses XPMenu to do the drawing. Note, unit file name has been renamed
    to Bc_XPMenu and the components have been separated to
    BcXPMenuDrawModule50 package. You cannot install this package if
    you have XPMenu installed as you cannot install another TXPMenu class.

    Drop TBcXPMenuDrawModule and TXPMenu components to your form. Be sure
    that TXPMenu Active is set False (should be default. If not, it is set
    to False when you link it to the DrawModule). Then link your TXPMenu
    to the TBcXPMenuDrawModule. After that use the TBcXPMenuDrawModule normally
    with TBcBarPopupMenu or TBcBarMainMenu.

  Notes:

    The Bc_XPMenu is almost unmodified version 1.505. Unit name differs and
    small changes in lines which are marked with "ja" were required.

    Because of the try to preserve it in it's original form it is required
    to use couple of hacks to access the protected methods and private
    FActive field of the TXPMenu instance. They work with me, so if you
    have problems (access violations and such) then it does not work with you :)

  XPMenu Copyright:

    XPMenu for Delphi
    Author: Khaled Shagrouni
    URL: http://www.shagrouni.com
    e-mail: shagrouni@hotmail.com
    Version 1.505, October 3, 2001

    XPMenu is a Delphi component to mimic Office XP menu and toolbar style.
    Copyright (C) 2001 Khaled Shagrouni.

  History:

    2001-10-22: Initial version

  =========================================================================== }

{$I DFS.inc}
{$I BcDirectives.inc}

unit BcXPMenuDrawModule;

interface

uses Windows, Messages, Classes, SysUtils, Menus, Graphics, BcDrawModule,
  Bc_XPMenu;

type
  TOnDrawMenuItemEvent = procedure(Sender: TObject; AMenuItem: TMenuItem; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState; ABarVisible: Boolean; var DefaultDraw: Boolean) of object;
  TOnMeasureMenuItemEvent = procedure(Sender: TObject; AMenuItem: TMenuItem; ACanvas: TCanvas; var Width, Height: Integer; ABarVisible: Boolean;  var DefaultMeasure: Boolean) of object;

  TBcXPMenuDrawModule = class(TBcBarMenusDrawModule)
  private
    FXPMenu: TXPMenu;
    procedure SetXPMenu(const Value: TXPMenu);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DrawMenuItem(AMenuItem: TMenuItem; ACanvas: TCanvas; ARect: TRect;
      State: TOwnerDrawState; ABarVisible: Boolean; var DefaultDraw: Boolean); override;
    procedure MeasureMenuItem(AMenuItem: TMenuItem; ACanvas: TCanvas;
      var Width, Height: Integer; ABarVisible: Boolean; var DefaultMeasure: Boolean); override;
  published
    property XPMenu: TXPMenu read FXPMenu write SetXPMenu;
  end;

  TXPMenuAccess = class(TXPMenu)
  end;

  TXPMenuPrivateHack = class(TComponent)
  private
    FActive: Boolean; // FActive is first field in the TXPMenu class
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Bluecave', [TBcXPMenuDrawModule]);
end;

{ TBcCustomDrawModule }

procedure TBcXPMenuDrawModule.DrawMenuItem(AMenuItem: TMenuItem;
  ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState;
  ABarVisible: Boolean; var DefaultDraw: Boolean);
begin
  if Assigned(FXPMenu) then
  begin
    TXPMenuAccess(FXPMenu).MenueDrawItem(AMenuItem, ACanvas, ARect, odSelected in State);
    DefaultDraw := False;
  end;
end;

procedure TBcXPMenuDrawModule.MeasureMenuItem(AMenuItem: TMenuItem;
  ACanvas: TCanvas; var Width, Height: Integer; ABarVisible: Boolean; var DefaultMeasure: Boolean);
begin
  if Assigned(FXPMenu) then
  begin
    TXPMenuPrivateHack(FXPMenu).FActive := True; // hack to skip SetActive property method
    TXPMenuAccess(FXPMenu).MeasureItem(AMenuItem, ACanvas, Width, Height);
    TXPMenuPrivateHack(FXPMenu).FActive := False;
    DefaultMeasure := False;
  end;
end;

procedure TBcXPMenuDrawModule.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FXPMenu) then
    XPMenu := nil;
end;

procedure TBcXPMenuDrawModule.SetXPMenu(const Value: TXPMenu);
begin
  FXPMenu := Value;
  if Assigned(FXPMenu) then
    with FXPMenu do
    begin
      Active := False;
      OverrideOwnerDraw := False;
      AutoDetect := False;
    end;
  RedrawMenus;
end;

end.
