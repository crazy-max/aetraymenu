{ Unit: BcDrawModule
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

    Base class for DrawModules. To implement own drawing module inherit
    your class from this base class and implement the abstract methods.

  Notes:

    If you implement own drawing modules send those to Bluecave Software
    (Mintus@Codefield.com) to be added as part of the official release.
    Bluecave Software reserves right to fit the code into Bluecave
    Software coding style. When sending please state if Bluecave Software
    may distribute your code under Corporation License. Your Copyrights
    will be preserved.

    The module should be independed components (they do not require any 3rd
    party packages).

    How to access the menu BarMenusIntf:

      Add uses BarMenus to implementation part and use the function
      GetMenuBarMenusIntf() on DrawingMenu.

  History:

    2001-10-22: Initial version

  =========================================================================== }

{$I DFS.inc}
{$I BcDirectives.inc}

unit BcDrawModule;

interface

uses Windows, Messages, Classes, SysUtils, Menus, Graphics;

type

  TBcBarMenusDrawModule = class(TComponent)
  private
    FDrawingMenu: TMenu;
    FDescription: string;
    function GetDrawingMenu: TMenu;
    procedure SetDrawingMenu(const Value: TMenu);
    function GetDescription: string;
    procedure SetDescription(const Value: string);
  protected
    procedure DrawMenuItem(AMenuItem: TMenuItem; ACanvas: TCanvas; ARect: TRect;
      State: TOwnerDrawState; ABarVisible: Boolean; var DefaultDraw: Boolean); virtual; abstract;
    procedure MeasureMenuItem(AMenuItem: TMenuItem; ACanvas: TCanvas;
      var Width, Height: Integer; ABarVisible: Boolean; var DefaultMeasure: Boolean); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RedrawMenus; virtual;
    property DrawingMenu: TMenu read GetDrawingMenu write SetDrawingMenu;
  published
    property Description: string read GetDescription write SetDescription;
  end;

  TMainMenuAccess = class(TMainMenu)
  end;

implementation

uses BarMenus;

{ TBcBarMenusDrawModule }

constructor TBcBarMenusDrawModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDrawingMenu := nil;
end;

destructor TBcBarMenusDrawModule.Destroy;
begin
  FDrawingMenu := nil;
  inherited Destroy;
end;

function TBcBarMenusDrawModule.GetDescription: string;
begin
  Result := FDescription;
end;

function TBcBarMenusDrawModule.GetDrawingMenu: TMenu;
begin
  Result := FDrawingMenu;
end;

procedure TBcBarMenusDrawModule.RedrawMenus;
begin
  if Assigned(DrawingMenu) then
    GetMenuBarMenusIntf(DrawingMenu).DrawModule := Self;
end;

procedure TBcBarMenusDrawModule.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TBcBarMenusDrawModule.SetDrawingMenu(const Value: TMenu);
begin
  FDrawingMenu := Value;
end;

end.
