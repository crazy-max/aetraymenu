{ Component: Bluecave BarMenu Components
  ===========================================================================

            Download the latest version of the component from:

            http://www.bluecave.net/barmenus/


  The contents of this file are subject to the Bluecave Public License V 1.1
  (the "License"). You may not copy or use this file, in either source code
  or executable form, except in compliance with the License. You may obtain
  a copy of the License at http://www.bluecave.net/licenses/.

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the
  License.

  Copyright (C) 2000-2003 Bluecave Software. All Rights Reserved.
  http://www.bluecave.net/

  Copyright (C) 2000-2003 Jouni Airaksinen. All Rights Reserved.
  http://Mintus.Codefield.com/ -- Mintus@Codefield.com

  Portions by:
    Copyright (C) 2001-2003 Magnus Flysjö, magnus.flysjo@visuellkommunikation.com

  ===========================================================================

  Unit description:

    TBcBarMainMenu and TBcBarPopupMenu components for Delphi 5 and above.

    This is a continued part of my article at www.delphi3000.com. Article
    ID 1133, http://www.delphi3000.com/article.asp?id=1133

    Article describes old version of this component, so the code here
    differs very much. You probably won't even regonize this as the same
    thing :) Never the less, you may still learn Delphi programming tricks
    from the code.

    There are demo projects at Demo1\ and Demo2\ directories.

    For usage and details read Readme.txt.

  History:

    Look at History.txt.

  =========================================================================== }

{$I DFS.inc}
{$I BcDirectives.inc}
{$I BarMenus.inc}
{$DEFINE DESIGNTIMEPREVIEW}
{$DEFINE PICTUREASTBITMAP}

{$R BarMenusResources.RES}

unit BarMenus;

interface

uses
  BcDrawModule,
  Windows, SysUtils, Classes, Graphics, Menus, Forms;

{$IFDEF LINUX}
  {$MESSAGE WARNING 'This probably will not compile under Kylix yet.'}
  SysUtils, Classes, QGraphics, QMenus, QForms;
{$ENDIF}

type
  TVertAlignment  = (vaTop, vaBottom, vaMiddle);
  THorzAlignment  = (haLeft, haRight, haCenter);
  TDrawStyle      = (dsNormal, dsTile, dsStretch);
  TGradientStyle  = (gsHorizontal, gsVertical, gsDiagonalLeftRight, gsDiagonalRightLeft);
  TBarPart        = (bpGradient, bpPicture, bpBackPicture, bpCaption);
  TBarParts       = set of TBarPart;
  TSide           = (sLeft, sRight);
  TDirection      = (dDownToUp, dUpToDown);
  TSeparatorStyle = (ssNormal, ssShortLine, ssCaption); // 2001-07-29: mf
  TBcMenuStyle    = (msAuto, msStandard, msWindowsXP);


  TOpacity = 0..255;

  TBcPicture = {type} {$IFDEF PICTUREASTBITMAP}TBitmap{$ELSE}TPicture{$ENDIF};

resourcestring
  BcResourceRadioItem = 'BCRADIOITEM';
  BcWinKeyString = 'Win+';

const
  ShortLineSpace = 12;
  RightMarginFromLine = 10;
  LineItemHeight = 12;
  LineItemHeightXP = 9;
  CaptionLineItemHeight = 14;
  GlyphSpace = 1;
  ItemSpace = 2;
  ItemSpaceXP = 4;
  SystemImageWidth = 8;

  DefaultOpacity = 255; // non-opaque
  DefaultFlat = False;
  DefaultUseSystemFont = True;
  DefaultMenuStyle = msAuto;

  DefaultGradientStart = TColor($00D8BDAF); // clBlue;
  DefaultGradientEnd = clBlack;
  DefaultBarBorder = clNone;

  DefaultBarSpace = 1;
  DefaultBarWidth = 32;
  DefaultBarVisible = True;
  DefaultBarSide = sLeft;
  DefaultGradientStyle = gsVertical;

  DefaultBarCaptionOffsetY = 6;
  DefaultBarCaptionVertAlignment = vaBottom;
  DefaultBarCaptionDirection = dDownToUp;
  DefaultBarCaptionAutoSize = False;
  DefaultBarCaptionShadowColor = clNone;
  DefaultBarCaptionHighlightColor = clNone;
  DefaultBarCaptionDepth = 1;
  DefaultBarCaptionVisible = True;

  DefaultBarBitmapHorzAlignment = haCenter;
  DefaultBarBitmapVertAlignment = vaBottom;
  DefaultBarBitmapTransparent = True;
  DefaultBarBitmapVisible = True;

  DefaultBarBackBitmapDrawStyle = dsNormal;
  DefaultBarBackBitmapHorzAlignment = haLeft;
  DefaultBarBackBitmapVertAlignment = vaTop;
  DefaultBarBackBitmapTransparent = False;

  DefaultSeparatorsGradientStart = clGray; // 2001-07-29: mf
  DefaultSeparatorsGradientEnd = clSilver;
  DefaultSeparatorsGradientStyle = gsHorizontal;
  DefaultSeparatorsFade = False;
  DefaultSeparatorsFadeWidth = 32;
  DefaultSeparatorsFadeColor = clBtnShadow;
  DefaultSeparatorsAlignment = taCenter; //taLeftJustify;
  DefaultSeparatorsSeparatorStyle = ssNormal;
  DefaultSeparatorsUseSystemFont = True;
  DefaultSeparatorsFlatLines = False;

type
  { events }
  TOnBeforeDrawBarEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRect; var DefaultDraw: Boolean) of object;
  TOnAfterDrawBarEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRect) of object;

  TOnDrawMenuItemEvent = procedure(Sender: TObject; AMenuItem: TMenuItem; ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState; ABarVisible: Boolean; var DefaultDraw: Boolean) of object;
  TOnMeasureMenuItemEvent = procedure(Sender: TObject; AMenuItem: TMenuItem; ACanvas: TCanvas; var Width, Height: Integer; ABarVisible: Boolean;  var DefaultMeasure: Boolean) of object;

  TOnAdvancedBeforeDrawBarEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRect; var DrawParts: TBarParts) of object;

  { Forward declarations }
  TBcCustomBarPopupMenu = class;
  TBcCustomBarMainMenu = class;
  TBcBarProtected = class;
  TBcGradientBar = class;
  TBcBar = class;
  TBcSeparators = class;
  TBcBarPicture = class;
  TBcBarBackPicture = class;
  TBcBarCaption = class;

  { IBarMenusIntf:
    This Interface layer provides common interface to properties
    of TBcCustomBarPopupMenu and TCustomBarMainMenu. There is no
    interface reference counting. This because the interfaced
    objects are VCL components. It is not possible to put the
    properties to common anchestor as it would be TMenu. That is
    why we use interface.

    Note: You shouldn't use this interface due it's nature as
    it's definition will most likely be different between
    different versions of BarMenus. Use it only through the
    BarMenu Components. }

  IBarMenusIntf = interface
  { Methods }
    procedure UpdateItems;
    procedure FlushDoubleBuffer;

    function UseMenuStyle: TBcMenuStyle;
    function GetSelf: TMenu; // access to the menu object, in a "Self".
    function GetBar: TBcBar;
    function GetSeparators : TBcSeparators; // 2001-07-29: mf

    function GetBarProtected: TBcBarProtected;

    procedure SetMenuWindowHandle(Handle: THandle);

    function GetOpacity: TOpacity;
    procedure SetOpacity(const Value: TOpacity);
    function GetFlat: Boolean;
    procedure SetFlat(const Value: Boolean);
    function GetMenuFont: TFont;
    procedure SetMenuFont(const Value: TFont);
    function GetMenuStyle: TBcMenuStyle;
    procedure SetMenuStyle(const Value: TBcMenuStyle);
    function GetUseSystemFont: Boolean;
    procedure SetUseSystemFont(const Value: Boolean);

    function GetOnBeforeDrawBar: TOnBeforeDrawBarEvent;
    procedure SetOnBeforeDrawBar(const Value: TOnBeforeDrawBarEvent);
    function GetOnAdvancedBeforeDrawBar: TOnAdvancedBeforeDrawBarEvent;
    procedure SetOnAdvancedBeforeDrawBar(const Value: TOnAdvancedBeforeDrawBarEvent);
    function GetOnAfterDrawBar: TOnAfterDrawBarEvent;
    procedure SetOnAfterDrawBar(const Value: TOnAfterDrawBarEvent);
    function GetOnMeasureMenuItem: TOnMeasureMenuItemEvent;
    procedure SetOnMeasureMenuItem(const Value: TOnMeasureMenuItemEvent);

    function GetDrawModule: TBcBarMenusDrawModule;
    procedure SetDrawModule(const Value: TBcBarMenusDrawModule);

  { Properties }
  { Protected }
    property BarProtected: TBcBarProtected read GetBarProtected;
    property Self: TMenu read GetSelf;
  { Public }
    property Bar: TBcBar read GetBar;
    property Separators: TBcSeparators read GetSeparators; // 2001-07-29: mf

    property Opacity: TOpacity read GetOpacity write SetOpacity;
    property Flat: Boolean read GetFlat write SetFlat;
    property MenuFont: TFont read GetMenuFont write SetMenuFont;
    property UseSystemFont: Boolean read GetUseSystemFont write SetUseSystemFont;
    property MenuStyle: TBcMenuStyle read GetMenuStyle write SetMenuStyle;

    property DrawModule: TBcBarMenusDrawModule read GetDrawModule write SetDrawModule;

    property OnBeforeDrawBar: TOnBeforeDrawBarEvent read GetOnBeforeDrawBar write SetOnBeforeDrawBar;
    property OnAdvancedBeforeDrawBar: TOnAdvancedBeforeDrawBarEvent read GetOnAdvancedBeforeDrawBar write SetOnAdvancedBeforeDrawBar;
    property OnAfterDrawBar: TOnAfterDrawBarEvent read GetOnAfterDrawBar write SetOnAfterDrawBar;
    property OnMeasureMenuItem: TOnMeasureMenuItemEvent read GetOnMeasureMenuItem write SetOnMeasureMenuItem;
  end;

  { TBcBarProtected }

  TBcBarProtected = class(TObject)
  private
    FOwner: TMenu;

    FPopupHeight: Integer;
    FBarDoubleBuffer: TBitmap;
    FDrawn: Boolean;
    FUpdatePopupHeight: Boolean;

    function GetBarDoubleBuffer: TBitmap;
    procedure SetBarDoubleBuffer(const Value: TBitmap);
    procedure SetUpdatePopupHeight(const Value: Boolean);
    procedure SetPopupHeight(const Value: Integer);
  protected
    procedure FreeBarDoubleBuffer;

    property BarDoubleBuffer: TBitmap read GetBarDoubleBuffer write SetBarDoubleBuffer;
    property UpdatePopupHeight: Boolean read FUpdatePopupHeight write SetUpdatePopupHeight;
    property PopupHeight: Integer read FPopupHeight{ write SetPopupHeight};
    property Drawn: Boolean read FDrawn write FDrawn;
  public
    constructor Create(AOwner: TMenu); virtual;
    destructor Destroy; override;
    property Owner: TMenu read FOwner;
  end;

  { TBcGradientBar }

  TBcGradientBar = class(TPersistent)
  private
    FOwner: TMenu;

    FBorder: TColor;
    FGradientStyle: TGradientStyle;

    function GetGradientColor(const Index: Integer): TColor;
    procedure SetGradientColor(const Index: Integer; const Value: TColor);
    procedure SetGradientStyle(const Value: TGradientStyle);
  protected
    { for C++Builder compatibility }
    FGradientStart: TColor;
    FGradientEnd: TColor;
  public
    constructor Create(AOwner: TMenu); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Owner: TMenu read FOwner;

    property GradientStart: TColor index 0 read GetGradientColor write SetGradientColor default DefaultGradientStart;
    property GradientEnd: TColor index 1 read GetGradientColor write SetGradientColor default DefaultGradientEnd;
    property Border: TColor read FBorder write FBorder default DefaultBarBorder;
    property GradientStyle: TGradientStyle read FGradientStyle write SetGradientStyle default DefaultGradientStyle;
  published
  end;

  { TBcBar }

  TBcBar = class(TBcGradientBar)
  private
    FWidth: Integer;
    FVisible: Boolean;

    FBarBackPicture: TBcBarBackPicture;
    FBarPicture: TBcBarPicture;
    FBarCaption: TBcBarCaption;
    FSpace: Integer;
    FSide: TSide;

    function GetBarPicture: TBcBarPicture;
    function GetBarBackPicture: TBcBarBackPicture;
    function GetBarCaption: TBcBarCaption;
    procedure SetBarPicture(const Value: TBcBarPicture);
    procedure SetBarBackPicture(const Value: TBcBarBackPicture);
    procedure SetBarCaption(const Value: TBcBarCaption);

    procedure SetVisible(Value: Boolean);
    function GetVisible: Boolean;
  protected
  public
    constructor Create(AOwner: TMenu); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Border;
    property GradientStart;
    property GradientEnd;
    property GradientStyle;
    property Width: Integer read FWidth write FWidth default DefaultBarWidth;
    property Space: Integer read FSpace write FSpace default DefaultBarSpace;
    property Visible: Boolean read GetVisible write SetVisible default DefaultBarVisible;
    property Side: TSide read FSide write FSide default DefaultBarSide;
    property BarPicture: TBcBarPicture read GetBarPicture write SetBarPicture;
    property BarBackPicture: TBcBarBackPicture read GetBarBackPicture write SetBarBackPicture;
    property BarCaption: TBcBarCaption read GetBarCaption write SetBarCaption;
  end;

  { TBcBarPicture }

  TBcBarPicture = class(TPersistent)
  private
    FOwner: TMenu;
    FTransparent: Boolean;
    FPicture: TBcPicture;
    FOffsetX,
    FOffsetY: Integer;
    FVertAlignment: TVertAlignment;
    FHorzAlignment: THorzAlignment;
    FVisible: Boolean;
  protected
    function GetPicture: TBcPicture;
    procedure SetPicture(Value: TBcPicture);
    procedure SetTransparent(Value: Boolean);
    function GetTransparent: Boolean;
  public
    constructor Create(AOwner: TMenu); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure CalcPicturePosition(var X, Y: Integer);

    property Owner: TMenu read FOwner;
  published
    property Transparent: Boolean read GetTransparent write SetTransparent default DefaultBarBitmapTransparent;
    property Picture: TBcPicture read GetPicture write SetPicture;
    property OffsetX: Integer read FOffsetX write FOffsetX default 0;
    property OffsetY: Integer read FOffsetY write FOffsetY default 0;
    property VertAlignment: TVertAlignment read FVertAlignment
      write FVertAlignment default DefaultBarBitmapVertAlignment;
    property HorzAlignment: THorzAlignment read FHorzAlignment
      write FHorzAlignment default DefaultBarBitmapHorzAlignment;
    property Visible: Boolean read FVisible write FVisible default DefaultBarBitmapVisible;
  end;

  { TBcBarBackPicture }

  TBcBarBackPicture = class(TBcBarPicture)
  private
    FDrawStyle: TDrawStyle;
  protected
  public
    constructor Create(AOwner: TMenu); override;
    destructor Destroy; override;
  published
    property DrawStyle: TDrawStyle read FDrawStyle write FDrawStyle default DefaultBarBackBitmapDrawStyle;
    property VertAlignment default DefaultBarBackBitmapVertAlignment;
    property HorzAlignment default DefaultBarBackBitmapHorzAlignment;
  end;

  { TBcBarCaption }

  TBcBarCaption = class(TPersistent)
  private
    FOwner: TMenu;
    FCaption: string;
    FFont: TFont;
{    FOffsetX,}
    FOffsetY: Integer;
    FVertAlignment: TVertAlignment;
    FDirection: TDirection;
    FAutoSize: Boolean;
    FShadowColor: TColor;
    FHighlightColor: TColor;
    FDepth: Integer;
    FVisible: Boolean;
  protected
    procedure SetFont(Value: TFont);
    // does not do anything
    property AutoSize: Boolean read FAutoSize write FAutoSize default DefaultBarCaptionAutoSize;
  public
    constructor Create(AOwner: TMenu);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Owner: TMenu read FOwner;
  published
    property Font: TFont read FFont write SetFont;
    property ShadowColor: TColor read FShadowColor write FShadowColor default DefaultBarCaptionShadowColor;
    property HighlightColor: TColor read FHighlightColor write FHighlightColor default DefaultBarCaptionHighlightColor;
    property Depth: Integer read FDepth write FDepth default DefaultBarCaptionDepth;
    property Caption: string read FCaption write FCaption;
{    property OffsetX: Integer read FOffsetX
      write FOffsetX default 0;}
    property OffsetY: Integer read FOffsetY
      write FOffsetY default DefaultBarCaptionOffsetY;
    property Alignment: TVertAlignment read FVertAlignment write FVertAlignment default DefaultBarCaptionVertAlignment;
    property Direction: TDirection read FDirection write FDirection default DefaultBarCaptionDirection;
    property Visible: Boolean read FVisible write FVisible default DefaultBarCaptionVisible;
  end;

  { TBcSepartors }

  TBcSeparators = class(TBcGradientBar) // 2001-07-29: mf, 2001-07-30: ja
  private
    FOwner: TMenu;
    FFont: TFont;
    FUseSystemFont: Boolean;
    FAlignment: TAlignment;
    FSeparatorStyle: TSeparatorStyle;
    FFade: Boolean;
    FFadeWidth: Integer;
    FFadeColor: TColor;
    FFlatLines: Boolean;
  protected
    procedure SetFont(Value: TFont);
  public
    constructor Create(AOwner: TMenu); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Owner: TMenu read FOwner;

  published
    property GradientStart default DefaultSeparatorsGradientStart;
    property GradientEnd default DefaultSeparatorsGradientEnd;
    property GradientStyle default DefaultSeparatorsGradientStyle;
    property Fade: Boolean read FFade write FFade default DefaultSeparatorsFade;
    property FadeWidth: Integer read FFadeWidth write FFadeWidth default DefaultSeparatorsFadeWidth;
    property FadeColor: TColor read FFadeColor write FFadeColor default DefaultSeparatorsFadeColor;
    property FlatLines: Boolean read FFlatLines write FFlatLines default DefaultSeparatorsFlatLines;
    property Font: TFont read FFont write SetFont;
    property Alignment: TAlignment read FAlignment write FAlignment default DefaultSeparatorsAlignment;
    property SeparatorStyle: TSeparatorStyle read FSeparatorStyle write FSeparatorStyle default DefaultSeparatorsSeparatorStyle;
    property UseSystemFont: Boolean read FUseSystemFont write FUseSystemFont default DefaultSeparatorsUseSystemFont;
  end;

  { TBcBarMainMenu }

  TBcCustomBarMainMenu = class(TMainMenu, IBarMenusIntf, IUnknown)
  private
    { Private declarations }
    FBarProtected: TBcBarProtected;
    FBar: TBcBar;
    FSeparators: TBcSeparators;                             // 2001-07-29: mf
    FOnBeforeDrawBar: TOnBeforeDrawBarEvent;
    FOnAdvancedBeforeDrawBar: TOnAdvancedBeforeDrawBarEvent;
    FOnAfterDrawBar: TOnAfterDrawBarEvent;
    FOnMeasureMenuItem: TOnMeasureMenuItemEvent;
    FMenuFont: TFont;
    FMenuStyle: TBcMenuStyle;
    FUseSystemFont: Boolean;
    FDrawModule: TBcBarMenusDrawModule;
    FFlat: Boolean;
    FOpacity: TOpacity;
    FMenuUpdating: Boolean;
    FUpdateRef: Integer;
    function GetBar: TBcBar;
    function GetSeparators: TBcSeparators;                  // 2001-07-29: mf
    function GetBarProtected: TBcBarProtected;
    procedure SetBar(const Value: TBcBar);
    procedure SetSeparators(const Value: TBcSeparators);    // 2001-07-29: mf
    function GetOnBeforeDrawBar: TOnBeforeDrawBarEvent;
    procedure SetOnBeforeDrawBar(const Value: TOnBeforeDrawBarEvent);
    function GetOnAfterDrawBar: TOnAfterDrawBarEvent;
    procedure SetOnAfterDrawBar(const Value: TOnAfterDrawBarEvent);
    function GetOpacity: TOpacity;
    procedure SetOpacity(const Value: TOpacity);
    function GetFlat: Boolean;
    procedure SetFlat(const Value: Boolean);
    function GetMenuFont: TFont;
    procedure SetMenuFont(const Value: TFont);
    function GetUseSystemFont: Boolean;
    procedure SetUseSystemFont(const Value: Boolean);

    function GetOnAdvancedBeforeDrawBar: TOnAdvancedBeforeDrawBarEvent;
    procedure SetOnAdvancedBeforeDrawBar(
      const Value: TOnAdvancedBeforeDrawBarEvent);
    function GetDrawModule: TBcBarMenusDrawModule;
    procedure SetDrawModule(const Value: TBcBarMenusDrawModule);
    function GetOnMeasureMenuItem: TOnMeasureMenuItemEvent;
    procedure SetOnMeasureMenuItem(const Value: TOnMeasureMenuItemEvent);
    function GetMenuStyle: TBcMenuStyle;
    procedure SetMenuStyle(const Value: TBcMenuStyle);
  protected
    { Reference counting disable for TComponent as life span is controlled by owner. }
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    { Protected declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetMenuWindowHandle(Handle: THandle);
    function GetSelf: TMenu;
    function UseMenuStyle: TBcMenuStyle;
    procedure RefreshMenu(const BarChanged, WidthChanged: Boolean); virtual;
    procedure MenuChanged(Sender: TObject; Source: TMenuItem; Rebuild: Boolean); override;
    procedure Loaded; override;
    procedure MenuFontChanged(Sender: TObject);

    property BarProtected: TBcBarProtected read GetBarProtected;

    property Flat: Boolean read GetFlat write SetFlat default DefaultFlat;

    { does not work in TMainMenu }
    property Opacity: TOpacity read GetOpacity write SetOpacity default DefaultOpacity;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure FlushDoubleBuffer;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure ExpandItemWidth(Sender: TObject; ACanvas: TCanvas; var Width,
      Height: Integer);
    procedure AdvancedDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
      State: TOwnerDrawState);

    property MenuUpdating: Boolean read FMenuUpdating;

    property MenuFont: TFont read GetMenuFont write SetMenuFont;
    property MenuStyle: TBcMenuStyle read GetMenuStyle write SetMenuStyle default DefaultMenuStyle;
    property UseSystemFont: Boolean read GetUseSystemFont write SetUseSystemFont default DefaultUseSystemFont;
    property Bar: TBcBar read GetBar write SetBar;
    property Separators: TBcSeparators read GetSeparators write SetSeparators; // 2001-07-29: mf

    property DrawModule: TBcBarMenusDrawModule read GetDrawModule write SetDrawModule;

    property OnBeforeDrawBar: TOnBeforeDrawBarEvent read GetOnBeforeDrawBar write SetOnBeforeDrawBar;
    property OnAdvancedBeforeDrawBar: TOnAdvancedBeforeDrawBarEvent read GetOnAdvancedBeforeDrawBar write SetOnAdvancedBeforeDrawBar;
    property OnAfterDrawBar: TOnAfterDrawBarEvent read GetOnAfterDrawBar write SetOnAfterDrawBar;

    property OnMeasureMenuItem: TOnMeasureMenuItemEvent read GetOnMeasureMenuItem write SetOnMeasureMenuItem;

    property OwnerDraw default True;
  end;

  TBcBarMainMenu = class(TBcCustomBarMainMenu)
  published
    property Bar;
    property Separators; // 2001-07-29: mf
//    property Flat;
    property MenuFont;
    property MenuStyle;
    property UseSystemFont;
    property DrawModule;
    property OnBeforeDrawBar;
    property OnAdvancedBeforeDrawBar;
    property OnAfterDrawBar;
    property OnMeasureMenuItem;
  end;

  { TBcBarPopupMenu }

  TBcCustomBarPopupMenu = class(TPopupMenu, IBarMenusIntf, IUnknown)
  private
    { Private declarations }
    FBarProtected: TBcBarProtected;
    FBar: TBcBar;
    FSeparators: TBcSeparators;
    FOnBeforeDrawBar: TOnBeforeDrawBarEvent;
    FOnAdvancedBeforeDrawBar: TOnAdvancedBeforeDrawBarEvent;
    FOnAfterDrawBar: TOnAfterDrawBarEvent;
    FOnMeasureMenuItem: TOnMeasureMenuItemEvent;
    FMenuFont: TFont;
    FMenuStyle: TBcMenuStyle;
    FUseSystemFont: Boolean;
    FDrawModule: TBcBarMenusDrawModule;
    FFlat: Boolean;
    FOpacity: TOpacity;
    FMenuWindowHandle: THandle;
    FMenuUpdating: Boolean;
    FUpdateRef: Integer;
    function GetBar: TBcBar;
    function GetSeparators: TBcSeparators; // 2001-07-29: mf
    function GetBarProtected: TBcBarProtected;
    procedure SetBar(const Value: TBcBar);
    procedure SetSeparators(const Value: TBcSeparators); // 2001-07-29: mf
    function GetOnBeforeDrawBar: TOnBeforeDrawBarEvent;
    procedure SetOnBeforeDrawBar(const Value: TOnBeforeDrawBarEvent);
    function GetOnAfterDrawBar: TOnAfterDrawBarEvent;
    procedure SetOnAfterDrawBar(const Value: TOnAfterDrawBarEvent);
    function GetOpacity: TOpacity;
    procedure SetOpacity(const Value: TOpacity);
    function GetFlat: Boolean;
    procedure SetFlat(const Value: Boolean);
    function GetUseSystemFont: Boolean;
    procedure SetUseSystemFont(const Value: Boolean);
    function GetMenuFont: TFont;
    procedure SetMenuFont(const Value: TFont);
    function GetOnAdvancedBeforeDrawBar: TOnAdvancedBeforeDrawBarEvent;
    procedure SetOnAdvancedBeforeDrawBar(
      const Value: TOnAdvancedBeforeDrawBarEvent);
    function GetDrawModule: TBcBarMenusDrawModule;
    procedure SetDrawModule(const Value: TBcBarMenusDrawModule);
    function GetOnMeasureMenuItem: TOnMeasureMenuItemEvent;
    procedure SetOnMeasureMenuItem(const Value: TOnMeasureMenuItemEvent);
    function GetMenuStyle: TBcMenuStyle;
    procedure SetMenuStyle(const Value: TBcMenuStyle);
  protected
    { Reference counting disable for TComponent as life span is controlled by owner. }
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    { Protected declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
    function GetSelf: TMenu;
    function UseMenuStyle: TBcMenuStyle;
    procedure SetMenuWindowHandle(Handle: THandle);
    procedure RefreshMenu(const BarChanged, WidthChanged: Boolean); virtual;
    procedure MenuChanged(Sender: TObject; Source: TMenuItem; Rebuild: Boolean); override;
    property BarProtected: TBcBarProtected read GetBarProtected;

    property Opacity: TOpacity read GetOpacity write SetOpacity default DefaultOpacity;
    property Flat: Boolean read GetFlat write SetFlat default DefaultFlat;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure FlushDoubleBuffer;

    procedure ExpandItemWidth(Sender: TObject; ACanvas: TCanvas; var Width,
      Height: Integer);
    procedure AdvancedDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
      State: TOwnerDrawState);

    procedure Popup(X, Y: Integer); overload; override;
    procedure Popup(P: TPoint); reintroduce; overload; virtual;
    procedure PopupAtCursor;

    procedure BeginUpdate;
    procedure EndUpdate;

    property MenuUpdating: Boolean read FMenuUpdating;

    property MenuWindowHandle: THandle read FMenuWindowHandle;

    property MenuFont: TFont read GetMenuFont write SetMenuFont;
    property MenuStyle: TBcMenuStyle read GetMenuStyle write SetMenuStyle default DefaultMenuStyle;
    property UseSystemFont: Boolean read GetUseSystemFont write SetUseSystemFont default DefaultUseSystemFont;
    property Bar: TBcBar read GetBar write SetBar;
    property Separators: TBcSeparators read GetSeparators write SetSeparators; // 2001-07-29: mf
    property DrawModule: TBcBarMenusDrawModule read GetDrawModule write SetDrawModule;

    property OnBeforeDrawBar: TOnBeforeDrawBarEvent read GetOnBeforeDrawBar write SetOnBeforeDrawBar;
    property OnAdvancedBeforeDrawBar: TOnAdvancedBeforeDrawBarEvent read GetOnAdvancedBeforeDrawBar write SetOnAdvancedBeforeDrawBar;
    property OnAfterDrawBar: TOnAfterDrawBarEvent read GetOnAfterDrawBar write SetOnAfterDrawBar;

    property OnMeasureMenuItem: TOnMeasureMenuItemEvent read GetOnMeasureMenuItem write SetOnMeasureMenuItem;

    property OwnerDraw default True;
  end;

  TBcBarPopupMenu = class(TBcCustomBarPopupMenu)
  published
    property Bar;
    property Separators; // 2001-07-29: mf
//    property Flat;
    property MenuFont;
    property MenuStyle;
    property UseSystemFont;
//    property Opacity;
    property DrawModule;
    property OnBeforeDrawBar;
    property OnAdvancedBeforeDrawBar;
    property OnAfterDrawBar;
    property OnMeasureMenuItem;
  end;

  TBcBarMenusDrawModuleAccess = class(TBcBarMenusDrawModule)
  end;

  TBcMenuItemAccess = class(TMenuItem)
  end;

{ intended to be used from events }
procedure BarDrawMenuItem(MenuItem: TMenuItem;
  ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState; ABarVisible: Boolean = False);

procedure BarMeasureItem(MenuItem: TMenuItem; ACanvas: TCanvas; var Width, Height: Integer;
  ABarVisible: Boolean = False);

procedure DrawMenuItemEx(BarMenusIntf: IBarMenusIntf; MenuItem: TMenuItem; ACanvas: TCanvas; ARect: TRect;
  State: TOwnerDrawState);
procedure DrawMenuItemText(MenuItem: TMenuItem; ACanvas: TCanvas; const ACaption: string; var Rect: TRect;
  Selected: Boolean; Flags: Longint);
procedure AdvancedDrawItem(MenuItem: TMenuItem; ACanvas: TCanvas; ARect: TRect;
  State: TOwnerDrawState; TopLevel: Boolean);

function IsLineItem(MenuItem: TMenuItem; var LineCaption: string): Boolean;
procedure DrawLineItem(BarMenusIntf: IBarMenusIntf; MenuItem: TMenuItem; LineCaption: string; ACanvas: TCanvas; ARect: TRect;
  State: TOwnerDrawState); // 2001-07-29: mf
procedure DrawFadeLine(ACanvas: TCanvas; AClipRect, ALineRect: TRect; AColor: TColor; AFadeWidth: Integer; AClip: Boolean);

procedure DrawGradient(ABitmap: TBitmap; ACanvas: TCanvas; ARect: TRect; ColorFrom, ColorTo: TColor;  Style: TGradientStyle);
procedure DrawBarGradient(BarMenusIntf: IBarMenusIntf; ARect: TRect);
procedure DrawBarCaption(BarMenusIntf: IBarMenusIntf; AClipRect: TRect);
procedure DrawBarPicture(BarMenusIntf: IBarMenusIntf);
procedure DrawBarBackPicture(BarMenusIntf: IBarMenusIntf);
procedure DrawDoubleBuffer(BarMenusIntf: IBarMenusIntf; ACanvas: TCanvas; Dest, Source: TRect);
procedure DrawMenuWindowBorder(BarMenusIntf: IBarMenusIntf; WindowHandle: THandle); overload;
procedure DrawMenuWindowBorder(MenuItem: TMenuItem; Canvas: TCanvas); overload;
procedure RefreshMenu(BarMenusIntf: IBarMenusIntf; const BarChanged, WidthChanged: Boolean);
procedure SetMenuOpacity(BarMenusIntf: IBarMenusIntf; const WindowHandle: THandle; const Opacity: TOpacity); overload;
procedure SetMenuOpacity(MenuItem: TMenuItem; Canvas: TCanvas); overload;

function GetParentMenuEx(MenuItem: TMenuItem): TMenu;

procedure SetMenuItemEvents(Menu: TMenu; MenuItem: TMenuItem; ExpandItemWidth: TMenuMeasureItemEvent;
  AdvancedDrawItem: TAdvancedMenuDrawItemEvent; Recursive: Boolean;  AllowOwnerDrawn: Boolean = True);

function FirstVisibleMenuItem(Menu: TMenu): TMenuItem;
function IsInFirstVisibleMenu(Menu: TMenu; MenuItem: TMenuItem): Boolean;
function IsInTopMainMenu(MenuItem: TMenuItem): Boolean;
function IsInTopPopupMenu(MenuItem: TMenuItem): Boolean;
function IsInTopMenu(MenuItem: TMenuItem): Boolean;
function IsAfterMenuBreak(MenuItem: TMenuItem): Boolean;

function GetMenuBarMenusIntf(AMenu: TMenu): IBarMenusIntf;

function GetMenuItemBarMenusIntf(AMenuItem: TMenuItem): IBarMenusIntf;

function BcShortCutToText(ShortCut: TShortCut): string;

var
  Win98Plus: Boolean = False;
  WinXP: Boolean = False;
  Win2K: Boolean = False;

implementation

uses
  Controls,
  Consts,
  BcFontUtilities,
  BcRectUtilities,
  BcExceptions,
  BcUtilities,
  BCLayeredWindows,
  Dialogs,
  ImgList;

const
  scWinKey = $0200; // My extension to TShortCut to allow Win key status

procedure BarMeasureItem(MenuItem: TMenuItem;
  ACanvas: TCanvas; var Width, Height: Integer; ABarVisible: Boolean);
var
  LineCaption, S: string;
  BarMenusIntf: IBarMenusIntf;
  DefaultMeasure, InTopMenu: Boolean;
  R: TRect;
//  TE: TSize;
begin
  BarMenusIntf := GetMenuItemBarMenusIntf(MenuItem);
  with BarMenusIntf.BarProtected, BarMenusIntf do
  begin
    InTopMenu := IsInTopMainMenu(MenuItem);
    if IsLineItem(MenuItem, LineCaption) then
    begin
      if Length(LineCaption) > 0 then
        Height := CaptionLineItemHeight
      else
      begin
        if UseMenuStyle = msWindowsXP then
          Height := LineItemHeightXP
        else
          Height := LineItemHeight;
      end;
    end else
    begin
      if not UseSystemFont then
      begin
        ACanvas.Font.Assign(MenuFont);
        if MenuItem.ShortCut = 0 then
          S := MenuItem.Caption
        else
          S := MenuItem.Caption + BcShortCutToText(MenuItem.ShortCut);
        //TE := ACanvas.TextExtent(S);
        R := NilRect;
        Height := DrawText(ACanvas.Handle, PChar(S), Length(S), R,
          DT_CALCRECT or DT_SINGLELINE) + 6;
        Width := RectWidth(R);
//        Height := TE.cy + 6;
        if Height < 16 then
          Height := 16;
      end;

      { VCL OwnerDrawn bug work-around }
      if not InTopMenu then
      begin
        if not UseSystemFont then
          Inc(Width, 18);
        
        { If shortcut has winkey extension, increase the width of the
          menuitem. }
        if (MenuItem.ShortCut and scWinKey) <> 0 then
          Inc(Width, ACanvas.TextWidth(BcWinKeyString));
          
        if not Assigned(Owner.Images) and
        not (Assigned(MenuItem.Bitmap) and not MenuItem.Bitmap.Empty) then
          Inc(Width, 15 + SystemImageWidth);
        if UseMenuStyle = msWindowsXP then
        begin
  //        Inc(Height, 4);
          Height := ((Height + 1) div 2) * 2; // make Height even and round to up
          Inc(Width, Abs(ItemSpaceXP - ItemSpace));
        end;
      end;
    end;

    DefaultMeasure := True;
    if Assigned(BarMenusIntf.OnMeasureMenuItem) then
      BarMenusIntf.OnMeasureMenuItem(BarMenusIntf.Self, MenuItem, ACanvas,
        Width, Height, ABarVisible, DefaultMeasure);

    if DefaultMeasure and Assigned(BarMenusIntf.DrawModule) then
      TBcBarMenusDrawModuleAccess(BarMenusIntf.DrawModule).MeasureMenuItem(MenuItem,
        ACanvas, Width, Height, ABarVisible, DefaultMeasure);

    if DefaultMeasure then
    begin
      { don't increase the margin for mainmenu top }
      if (Separators.SeparatorStyle = ssShortLine) and not InTopMenu then
        Inc(Width, RightMarginFromLine); // we use this space in the right side
    end;

    if not ABarVisible or IsAfterMenuBreak(MenuItem) then
      Exit;

    Inc(Width, Bar.Width + Bar.Space); { make space for graphical bar }

    if UpdatePopupHeight and MenuItem.Visible then
      SetPopupHeight(PopupHeight + Height);
  end;
end;

procedure BarDrawMenuItem(MenuItem: TMenuItem;
  ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState; ABarVisible: Boolean);
var
  R, TargetRect: TRect;
//  OnAdvancedDrawItem: TAdvancedMenuDrawItemEvent;
  BarMenusIntf: IBarMenusIntf;
  DefaultDraw: Boolean;
  WindowHandle: THandle;
  DrawParts: TBarParts;
  ABarDoubleBuffer: TBitmap;
begin
  WindowHandle := WindowFromDC(ACanvas.Handle);

  BarMenusIntf := GetMenuItemBarMenusIntf(MenuItem);

  with BarMenusIntf.BarProtected, BarMenusIntf.Bar, BarMenusIntf do
  begin
    { update the read-only property value }
    SetMenuWindowHandle(WindowHandle);

    { set menu opacity }
    SetMenuOpacity(BarMenusIntf, WindowHandle, Opacity);

    { we need to remove draw event so DrawMenuItem won't generate infinite loop!
      (Recursive) }
//    OnAdvancedDrawItem := MenuItem.OnAdvancedDrawItem;
//    MenuItem.OnAdvancedDrawItem := nil;

    { check bar visiblity in case of menu breaks }
    ABarVisible := ABarVisible and not IsAfterMenuBreak(MenuItem);

    { align rect where item is draw so that vcl will leave bar for us }
    R := ARect;
    if ABarVisible then
    begin
      R.Right := R.Right - Width - Space; { remove bar width }
      { TODO -cKylix/Linux -oJouni Airaksinen : Kylix supports OffsetRect or similar? }
      if Bar.Side = sLeft then
        OffsetRect(R, Width + Space, 0);
    end;

    { set font if not set to use system font }
    if not UseSystemFont then
      ACanvas.Font.Assign(MenuFont);

    { draw item and restore event back }
    DefaultDraw := True;
    if Assigned(BarMenusIntf.DrawModule) then
      TBcBarMenusDrawModuleAccess(BarMenusIntf.DrawModule).DrawMenuItem(
        MenuItem, ACanvas, R, State, ABarVisible, DefaultDraw);
    if DefaultDraw then
    begin
      { separator identing only for BarMenus drawing }
      if (Separators.SeparatorStyle = ssShortLine) and not IsInTopMainMenu(MenuItem) then
        Dec(R.Right, RightMarginFromLine); // space we have reserved
      DrawMenuItemEx(BarMenusIntf, MenuItem, ACanvas, R, State);
    end;

//    MenuItem.OnAdvancedDrawItem := OnAdvancedDrawItem;

    if ABarVisible then
    begin
      if UpdatePopupHeight then
      begin
        if GetClientRect(WindowHandle, TargetRect) then
        begin
          SetPopupHeight(RectHeight(TargetRect));
        end;
      end;
      ABarDoubleBuffer := BarDoubleBuffer;
      if UpdatePopupHeight then
      begin
        UpdatePopupHeight := False;
        { set doublebuffer bitmap to right size }
        if {ABarDoubleBuffer.Empty or} (ABarDoubleBuffer.Height <> PopupHeight) then
        begin
          ABarDoubleBuffer.Height := PopupHeight;
          Drawn := False;
        end;
        if {ABarDoubleBuffer.Empty or} (ABarDoubleBuffer.Width <> Width) then
        begin
          ABarDoubleBuffer.Width := Width;
          Drawn := False;
        end;
      end;
      with ABarDoubleBuffer.Canvas do
      begin
        if not Drawn then
          TargetRect := BitmapRect(ABarDoubleBuffer)
        else
          TargetRect := ARect;
        if Bar.Side = sRight then
          OffsetRect(TargetRect, RectWidth(ARect) - Width, 0);
        if not Drawn then { double buffer is empty or need refresh so create it }
        begin
          R := BitmapRect(ABarDoubleBuffer);

          { execute user event }
          DefaultDraw := True;
          if Assigned(BarMenusIntf.OnBeforeDrawBar) then
            BarMenusIntf.OnBeforeDrawBar(MenuItem.GetParentMenu, ABarDoubleBuffer.Canvas, R, DefaultDraw);

          { if event did not disable default drawing, then do drawing }
          if DefaultDraw then
          begin
            DrawParts := [bpGradient];
            if BarPicture.Visible then
              Include(DrawParts, bpPicture);
            if BarBackPicture.Visible then
            Include(DrawParts, bpBackPicture);
            if BarCaption.Visible then
              Include(DrawParts, bpCaption);
            if Assigned(BarMenusIntf.OnAdvancedBeforeDrawBar) then
              BarMenusIntf.OnAdvancedBeforeDrawBar(MenuItem.GetParentMenu, ABarDoubleBuffer.Canvas, R, DrawParts);

            if bpGradient in DrawParts then
              DrawBarGradient(BarMenusIntf, R);      // 1. Gradient
            if (bpBackPicture in DrawParts) then
              DrawBarBackPicture(BarMenusIntf);      // 2. Bar background picture
            if (bpPicture in DrawParts) then
              DrawBarPicture(BarMenusIntf);          // 3. Bar picture
            if bpCaption in DrawParts then
              DrawBarCaption(BarMenusIntf, R);       // 4. Bar caption
          end;

          { draw the whole doublebuffer }
          DrawDoubleBuffer(BarMenusIntf, ACanvas, TargetRect, R);

          { execute user event }
          if Assigned(BarMenusIntf.OnAfterDrawBar) then
            BarMenusIntf.OnAfterDrawBar(MenuItem.GetParentMenu, ABarDoubleBuffer.Canvas, R);

          Drawn := True;
        end else { draw just the menuitem part of the doublebuffer }
        begin
          DrawDoubleBuffer(BarMenusIntf, ACanvas, TargetRect, ARect);
  //        { execute user event }
  //        if Assigned(BarMenusIntf.OnAfterDrawBar) then
  //          BarMenusIntf.OnAfterDrawBar(MenuItem.GetParentMenu, ABarDoubleBuffer.Canvas, ARect);
        end;
      end;
    end;
    DrawMenuWindowBorder(BarMenusIntf, WindowHandle);
  end;
end;

{ This procedure has been copied from VCL50 Menus.pas TMenuItem.DoDrawText. }
procedure DrawMenuItemText(MenuItem: TMenuItem; ACanvas: TCanvas; const ACaption: string; var Rect: TRect;
  Selected: Boolean; Flags: Longint);
var
  Text: string;
  R: TRect;
  ParentMenu: TMenu;
begin
  with MenuItem do
  begin
    ParentMenu := GetParentMenu;
    if (ParentMenu <> nil) and (ParentMenu.IsRightToLeft) then
    begin
      if Flags and DT_LEFT = DT_LEFT then
        Flags := Flags and (not DT_LEFT) or DT_RIGHT
      else if Flags and DT_RIGHT = DT_RIGHT then
        Flags := Flags and (not DT_RIGHT) or DT_LEFT;
      Flags := Flags or DT_RTLREADING;
    end;
    Text := ACaption;
    if (Flags and DT_CALCRECT <> 0) and ((Text = '') or
      (Text[1] = cHotkeyPrefix) and (Text[2] = #0)) then Text := Text + ' ';
    with ACanvas do
    begin
      if Text = cLineCaption then
      begin
        if Flags and DT_CALCRECT = 0 then
        begin
          R := Rect;
          Inc(R.Top, 4);
          DrawEdge(Handle, R, EDGE_ETCHED, BF_TOP);
        end;
      end
      else
      begin
        Brush.Style := bsClear;
        if Default then
          Font.Style := Font.Style + [fsBold];
        if not Enabled then
        begin
          if not Selected then
          begin
            OffsetRect(Rect, 1, 1);
            Font.Color := clBtnHighlight;
            DrawText(Handle, PChar(Text), Length(Text), Rect, Flags);
            OffsetRect(Rect, -1, -1);
          end;
          if Selected and (ColorToRGB(clHighlight) = ColorToRGB(clBtnShadow)) then
            Font.Color := clBtnHighlight else
            Font.Color := clBtnShadow;
        end;
        DrawText(Handle, PChar(Text), Length(Text), Rect, Flags);
      end;
    end;
  end;
end;

{ This procedure has been copied from VCL50 Menus.pas TMenuItem.AdvancedDrawItem.
  This version has small fixes and few new features. }
procedure AdvancedDrawItem(MenuItem: TMenuItem; ACanvas: TCanvas; ARect: TRect;
  State: TOwnerDrawState; TopLevel: Boolean);
const
  Alignments: array[TPopupAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  EdgeStyle: array[Boolean] of Longint = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
var
  ImageList: TCustomImageList;
  ParentMenu: TMenu;
  Alignment: TPopupAlignment;
  DrawImage, DrawGlyph: Boolean;
  GlyphRect, SaveRect: TRect;
  DrawStyle: Longint;
  OldBrushColor: TColor;
  Selected: Boolean;
  GlyphOffset: Integer;
  WinXPStyle: Boolean;
  BarMenusIntf: IBarMenusIntf;

  procedure DrawCheckMark;
  var
    Glyph: TBitmap;
    P: TPoint;
  begin
    { Draw a menu check }
    Glyph := TBitmap.Create;
    try
      Glyph.Transparent := True;
      { todo : properties for these default glyphs? }
      if MenuItem.RadioItem then
        Glyph.Handle := LoadBitmap(HInstance, PChar(BcResourceRadioItem))
      else
        Glyph.Handle := LoadBitmap(0, PChar(OBM_CHECK));
      OldBrushColor := ACanvas.Font.Color;
      ACanvas.Font.Color := clBtnText;
      P.X := (GlyphRect.Right + GlyphRect.Left - Glyph.Width) div 2;
      P.Y := (GlyphRect.Bottom + GlyphRect.Top - Glyph.Height) div 2;
      if not WinXPStyle then
      begin
        Inc(P.X);
        Inc(P.Y);
      end else
        if not MenuItem.RadioItem then // fix the Check mark to the center of the glyph
          Inc(P.X);
      ACanvas.Draw(P.X, P.Y, Glyph);
      ACanvas.Font.Color := OldBrushColor;
    finally
      FreeAndNil(Glyph);
    end;
  end;

  procedure NormalDraw;
  begin
    with MenuItem, ACanvas do
    begin
      //ImageList := GetImageList;
      if not Selected then FillRect(ARect);
      if ParentMenu is TMenu then
        Alignment := paLeft
      else if ParentMenu is TPopupMenu then
        Alignment := TPopupMenu(ParentMenu).Alignment
      else
        Alignment := paLeft;
      GlyphRect.Left := ARect.Left + 1;
      GlyphRect.Top := ARect.Top + 1;
      if Caption = cLineCaption then
      begin
        FillRect(ARect);
        GlyphRect.Left := 0;
        GlyphRect.Right := -4;
        DrawGlyph := False;
      end else
      begin
        DrawImage := (ImageList <> nil) and ((ImageIndex > -1) and
          (ImageIndex < ImageList.Count) or Checked and Bitmap.Empty);
        if DrawImage or not Bitmap.Empty then
        begin
          DrawGlyph := True;

          if DrawImage then
          begin
            GlyphRect.Right := GlyphRect.Left + ImageList.Width;
            GlyphRect.Bottom := GlyphRect.Top + ImageList.Height;
          end else
          begin
            { Need to add BitmapWidth/Height properties for TMenuItem if we're to
              support them.  Right now let's hardcode them to 16x16. }
            GlyphRect.Right := GlyphRect.Left + 16;
            GlyphRect.Bottom := GlyphRect.Top + 16;
          end;

          { Center the glyph the the item (looks better and it's XP way) }
          GlyphOffset := (((ARect.Bottom - ARect.Top) - (GlyphRect.Bottom - GlyphRect.Top)) div 2) - 1;
          Inc(GlyphRect.Top, GlyphOffset);
          Inc(GlyphRect.Bottom, GlyphOffset);

          if WinXPStyle then
          begin
            SaveRect := ARect;
            SaveRect.Right := GlyphRect.Right + GlyphSpace + ItemSpaceXP;
            if Checked then
            begin
              OldBrushColor := Brush.Color;
              Brush.Color := clHighlight;
              if Selected then FillRect(SaveRect);
              Inc(GlyphRect.Left);
              Inc(GlyphRect.Right);
              if not Selected then
              begin
                FrameRect(Rect(GlyphRect.Left - 1, GlyphRect.Top - 1,
                  GlyphRect.Right + 1, GlyphRect.Bottom + 1));
              end else
              begin
                Brush.Color := clMenu;
                FillRect(GlyphRect);
              end;
              Brush.Color := OldBrushColor;
            end else
            begin
              FillRect(SaveRect);
              OffsetRect(GlyphRect, 1, 0);
            end;
          end else
          begin
            { Draw background pattern brush if selected }
            if Checked then
            begin
              Inc(GlyphRect.Right);
              Inc(GlyphRect.Bottom);
              OldBrushColor := Brush.Color;
              if not (odSelected in State) then
              begin
                OldBrushColor := Brush.Color;
                Brush.Bitmap := AllocPatternBitmap(clBtnFace, clBtnHighlight);
                FillRect(GlyphRect);
              end else
              begin
                Brush.Color := clBtnFace;
                FillRect(GlyphRect);
              end;
              Brush.Color := OldBrushColor;
              Inc(GlyphRect.Left);
              Inc(GlyphRect.Top);
            end;
          end;

          if DrawImage then
          begin
            if (ImageIndex > -1) and (ImageIndex < ImageList.Count) then
              ImageList.Draw(ACanvas, GlyphRect.Left, GlyphRect.Top, ImageIndex,
                Enabled)
            else
              DrawCheckMark;
          end else
          begin
            SaveRect := GlyphRect;
            { Make sure image is within glyph bounds }
            if Bitmap.Width < GlyphRect.Right - GlyphRect.Left then
              with GlyphRect do
              begin
                Left := Left + ((Right - Left) - Bitmap.Width) div 2 + 1;
                Right := Left + Bitmap.Width;
              end;
            if Bitmap.Height < GlyphRect.Bottom - GlyphRect.Top then
              with GlyphRect do
              begin
                Top := Top + ((Bottom - Top) - Bitmap.Height) div 2 + 1;
                Bottom := Top + Bitmap.Height;
              end;
            StretchDraw(GlyphRect, Bitmap);
            GlyphRect := SaveRect;
          end;

          if Checked then
          begin
            Dec(GlyphRect.Right);
            Dec(GlyphRect.Bottom);
          end;
        end else
        begin
          if (ImageList <> nil) and not TopLevel then
          begin
            GlyphRect.Right := GlyphRect.Left + ImageList.Width;
            GlyphRect.Bottom := GlyphRect.Top + ImageList.Height;
          end else
          begin
            Inc(GlyphRect.Left);
            GlyphRect.Right := GlyphRect.Left;
            GlyphRect.Bottom := GlyphRect.Top;
            if not TopLevel then
            begin
              Inc(GlyphRect.Right, SystemImageWidth);
              GlyphRect.Bottom := ARect.Bottom - 2;
              if not WinXPStyle then
                Dec(GlyphRect.Bottom);
            end;
          end;
          DrawGlyph := False;
        end;
      end;
      with GlyphRect do
      begin
        Dec(Left);
        Dec(Top);
        Inc(Right, 2);
        Inc(Bottom, 2);
      end;

      if not WinXPStyle and ((Checked or Selected) and DrawGlyph) then
        DrawEdge(Handle, GlyphRect, EdgeStyle[Checked], BF_RECT);

      if WinXPStyle then
      begin
        if Selected then
        begin
          SaveRect := ARect;
          if DrawGlyph then
          begin
            ARect.Left := GlyphRect.Right + GlyphSpace;
            SaveRect.Left := GlyphRect.Right - 1;
          end;
          Brush.Color := clHighlight;
          FillRect(SaveRect);
        end;
      end else
      begin
        if Selected then
        begin
          if DrawGlyph then
            ARect.Left := GlyphRect.Right + GlyphSpace;
          if not (Win98Plus and TopLevel) then
            Brush.Color := clHighlight;
          FillRect(ARect);
        end;
      end;
      if not DrawGlyph and not TopLevel and Checked then
        DrawCheckMark;
      if TopLevel then
      begin
        if WinXPStyle then
        begin
          if Selected or (odHotLight in State) then
            Brush.Color := clHighlight;
          FillRect(ARect);
        end else if Win98Plus then
        begin
          if Selected then
            DrawEdge(Handle, ARect, BDR_SUNKENOUTER, BF_RECT)
          else if odHotLight in State then
            DrawEdge(Handle, ARect, BDR_RAISEDINNER, BF_RECT);
          if not Selected then
            OffsetRect(ARect, 0, -1);
        end;
      end;
      if not (Selected and DrawGlyph) then
        ARect.Left := GlyphRect.Right + GlyphSpace;
      if WinXPStyle and not TopLevel then
        Inc(ARect.Left, ItemSpaceXP)
      else
        Inc(ARect.Left, ItemSpace);
      Dec(ARect.Right, 1);

      DrawStyle := DT_EXPANDTABS or DT_SINGLELINE or Alignments[Alignment];
      if Win2K and (odNoAccel in State) then
        DrawStyle := DrawStyle or DT_HIDEPREFIX;
      { Calculate vertical layout }
      SaveRect := ARect;
      if odDefault in State then
        Font.Style := [fsBold];
      DrawMenuItemText(MenuItem, ACanvas, Caption, ARect, Selected, DrawStyle or DT_CALCRECT or DT_NOCLIP);
      OffsetRect(ARect, 0, ((SaveRect.Bottom - SaveRect.Top) - (ARect.Bottom - ARect.Top)) div 2);
      if TopLevel then
      begin
        if WinXPStyle then
        begin
          if not (odInactive in State) and (Selected or (odHotLight in State)) then
            Font.Color := clHighlightText;
          Dec(ARect.Top);
        end else if Win98Plus and Selected then
          OffsetRect(ARect, 1, 0);
      end;

      DrawMenuItemText(MenuItem, ACanvas, Caption, ARect, Selected, DrawStyle);
      if (ShortCut <> 0) and not TopLevel then
      begin
        ARect.Left := ARect.Right;
        ARect.Right := SaveRect.Right - 10;
        DrawMenuItemText(MenuItem, ACanvas, BcShortCutToText(ShortCut), ARect, Selected, DT_RIGHT);
      end;
    end;
  end;
(*
  procedure BiDiDraw;
  var
    S: string;
  begin
    with MenuItem, ACanvas do
    begin
      //ImageList := GetImageList;
      if not Selected then FillRect(ARect);
      if ParentMenu is TMenu then
        Alignment := paLeft
      else if ParentMenu is TPopupMenu then
        Alignment := TPopupMenu(ParentMenu).Alignment
      else
        Alignment := paLeft;    
      GlyphRect.Right := ARect.Right - 1;
      GlyphRect.Top := ARect.Top + 1;
      if Caption = cLineCaption then
      begin
        FillRect(ARect);
        GlyphRect.Left := GlyphRect.Right + 2;
        GlyphRect.Right := 0;
        DrawGlyph := False;
      end
      else
      begin
        DrawImage := (ImageList <> nil) and ((ImageIndex > -1) and
          (ImageIndex < ImageList.Count) or Checked and Bitmap.Empty);
        if DrawImage or Assigned(Bitmap) and not Bitmap.Empty then
        begin
          DrawGlyph := True;

          if DrawImage then
          begin
            GlyphRect.Left := GlyphRect.Right - ImageList.Width;
            GlyphRect.Bottom := GlyphRect.Top + ImageList.Height;
          end
          else
          begin
            { Need to add BitmapWidth/Height properties for TMenuItem if we're to
              support them.  Right now let's hardcode them to 16x16. }
            GlyphRect.Left := GlyphRect.Right - 16;
            GlyphRect.Bottom := GlyphRect.Top + 16;
          end;

          { Draw background pattern brush if selected }
          if Checked then
          begin
            Dec(GlyphRect.Left);
            Inc(GlyphRect.Bottom);
            OldBrushColor := Brush.Color;
            if not Selected then
            begin
              OldBrushColor := Brush.Color;
              Brush.Bitmap := AllocPatternBitmap(clBtnFace, clBtnHighlight);
              FillRect(GlyphRect);
            end
            else
            begin
              Brush.Color := clBtnFace;
              FillRect(GlyphRect);
            end;
            Brush.Color := OldBrushColor;
            Dec(GlyphRect.Right);
            Inc(GlyphRect.Top);
          end;

          if DrawImage then
          begin
            if (ImageIndex > -1) and (ImageIndex < ImageList.Count) then
              ImageList.Draw(ACanvas, GlyphRect.Left, GlyphRect.Top, ImageIndex,
                Enabled)
            else
            begin
              { Draw a menu check }
              Glyph := TBitmap.Create;
              try
                Glyph.Transparent := True;
                if RadioItem then
                  Glyph.Handle := LoadBitmap(HInstance, PChar(BcResourceRadioItem))
                else
                  Glyph.Handle := LoadBitmap(0, PChar(OBM_CHECK));
                OldBrushColor := Font.Color;
                Font.Color := clBtnText;
                Draw(GlyphRect.Left + (GlyphRect.Right - GlyphRect.Left - Glyph.Width) div 2 + 1,
                  GlyphRect.Top + (GlyphRect.Bottom - GlyphRect.Top - Glyph.Height) div 2 + 1, Glyph);
                Font.Color := OldBrushColor;
              finally
                Glyph.Free;
              end;
            end;
          end
          else
          begin
            SaveRect := GlyphRect;
            { Make sure image is within glyph bounds }
            if Bitmap.Width < GlyphRect.Right - GlyphRect.Left then
              with GlyphRect do
              begin
                Right := Right - ((Right - Left) - Bitmap.Width) div 2 + 1;
                Left := Right - Bitmap.Width;
              end;
            if Bitmap.Height < GlyphRect.Bottom - GlyphRect.Top then
              with GlyphRect do
              begin
                Top := Top + ((Bottom - Top) - Bitmap.Height) div 2 + 1;
                Bottom := Top + Bitmap.Height;
              end;
            StretchDraw(GlyphRect, Bitmap);
            GlyphRect := SaveRect;
          end;

          if Checked then
          begin
            Dec(GlyphRect.Right);
            Dec(GlyphRect.Bottom);
          end;
        end
        else
        begin
          if (ImageList <> nil) and not TopLevel then
          begin
            GlyphRect.Left := GlyphRect.Right - ImageList.Width;
            GlyphRect.Bottom := GlyphRect.Top + ImageList.Height;
          end
          else
          begin
            GlyphRect.Left := GlyphRect.Right;
            GlyphRect.Bottom := GlyphRect.Top;
          end;
          DrawGlyph := False;
        end;
      end;
      with GlyphRect do
      begin
        Dec(Left);
        Dec(Top);
        Inc(Right, 2);
        Inc(Bottom, 2);
      end;

      if Checked or Selected and DrawGlyph then
        DrawEdge(Handle, GlyphRect, EdgeStyle[Checked], BF_RECT);

      if Selected then
      begin
        if DrawGlyph then ARect.Right := GlyphRect.Left - 1;
        if not (Win98Plus and TopLevel) then
          Brush.Color := clHighlight;
        FillRect(ARect);
      end;

      if TopLevel then
      begin
        if WinXPStyle then
        begin
          if Selected or (odHotLight in State) then
            Brush.Color := clHighlight;
          FillRect(ARect);
        end else if Win98Plus then
        begin
          if Selected then
            DrawEdge(Handle, ARect, BDR_SUNKENOUTER, BF_RECT)
          else if odHotLight in State then
            DrawEdge(Handle, ARect, BDR_RAISEDINNER, BF_RECT);
          if not Selected then
            OffsetRect(ARect, 0, -1);
        end;
      end;
      if not (Selected and DrawGlyph) then
        ARect.Right := GlyphRect.Left - 1;
      Inc(ARect.Left, 2);
      Dec(ARect.Right, 1);

      DrawStyle := DT_EXPANDTABS or DT_SINGLELINE or Alignments[Alignment];
      if Win2K and (odNoAccel in State) then
        DrawStyle := DrawStyle or DT_HIDEPREFIX;
      { Calculate vertical layout }
      SaveRect := ARect;
      if odDefault in State then
        Font.Style := [fsBold];
      DrawMenuItemText(MenuItem, ACanvas, Caption, ARect, Selected, DrawStyle or DT_CALCRECT or DT_NOCLIP);
      { the DT_CALCRECT does not take into account alignment }
      ARect.Left := SaveRect.Left;
      ARect.Right := SaveRect.Right;
      OffsetRect(ARect, 0, ((SaveRect.Bottom - SaveRect.Top) - (ARect.Bottom - ARect.Top)) div 2);
      if TopLevel then
      begin
        if WinXPStyle then
        begin
          if not (odInactive in State) and (Selected or (odHotLight in State)) then
            Font.Color := clHighlightText;
        end else if Win98Plus and Selected then
          OffsetRect(ARect, 1, 0);
      end;

      DrawMenuItemText(MenuItem, ACanvas, Caption, ARect, Selected, DrawStyle);
      if (ShortCut <> 0) and not TopLevel then
      begin
        S := ShortCutToText(ShortCut);
        ARect.Left := 10;
        ARect.Right := ARect.Left + ACanvas.TextWidth(S);
        DrawMenuItemText(MenuItem, ACanvas, S, ARect, Selected, DT_RIGHT);
      end;
    end;
  end;
*)
begin
  BarMenusIntf := GetMenuItemBarMenusIntf(MenuItem);
  ParentMenu := MenuItem.GetParentMenu;
  ImageList := MenuItem.GetImageList;
  Selected := odSelected in State;
  WinXPStyle := BarMenusIntf.UseMenuStyle = msWindowsXP;
  if (ParentMenu <> nil) and (not ParentMenu.IsRightToLeft) then
    NormalDraw
  else
    NormalDraw;
//    BiDiDraw;
end;

procedure DrawMenuItemEx(BarMenusIntf: IBarMenusIntf; MenuItem: TMenuItem;
  ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
var
  LineCaption: string;
  R: TRect;
  InTopMenu: Boolean;

  { Copied from VCL50 Menus.pas and modified a bit. It had small bug under Windows XP
    making the top level menuitem background color wrong. Here we fix it. }
  procedure FixedDrawMenuItem(MenuItem: TMenuItem; ACanvas: TCanvas; ARect: TRect;
    State: TOwnerDrawState);
  begin
    with ACanvas do
    begin
      if (odSelected in State) and (not InTopMenu or (InTopMenu and not Win98Plus)) then
      begin
        Brush.Color := clHighlight;
        Font.Color := clHighlightText;
      end else
      begin
        if Win98Plus and (odInactive in State) then
        begin
          Font.Color := clGrayText;
        end else
        begin
          Font.Color := clMenuText;
        end;
        if (InTopMenu and WinXP{(BarMenusIntf.UseMenuStyle = msWindowsXP)}) then
          Brush.Color := clBtnFace // In Windows XP the toplevel menu is in clBtnFace color
        else
          Brush.Color := clMenu;
      end;

//      TBcMenuItemAccess(MenuItem).AdvancedDrawItem(ACanvas, ARect, State, InTopMenu);
      AdvancedDrawItem(MenuItem, ACanvas, ARect, State, InTopMenu);
    end;
  end;

begin
  { if we have lineitem then draw it }
  if IsLineItem(MenuItem, LineCaption) then
    DrawLineItem(BarMenusIntf, MenuItem, LineCaption, ACanvas, ARect, State)
  else // normal item
  begin
    InTopMenu := IsInTopMainMenu(MenuItem);
    { fixes the right margin selection }
    if (BarMenusIntf.Separators.SeparatorStyle = ssShortLine) and not InTopMenu then
    begin
      R := ARect;
      R.Left := R.Right;
      Inc(R.Right, RightMarginFromLine);
      ACanvas.FillRect(R);
    end;
    FixedDrawMenuItem(MenuItem, ACanvas, ARect, State);
  end;
end;

function IsLineItem(MenuItem: TMenuItem; var LineCaption: string): Boolean;
begin
  Result := False;
  { If the item is lineitem, then set the Hint as LineCaption }
  if MenuItem.IsLine then
  begin
    if MenuItem.Enabled then
      LineCaption := MenuItem.Hint
    else
      LineCaption := '';
    Result := True;
  end;
end;

{ This procedure draws the LineItem using the given properties of Separators. Internally
  it matches the one in the menu components. You can use this though your own code if,
  you wish. Just pass appropiate parameters. }

{  2001-07-30: rewrite; Jouni Airaksinen
    - fixes in caption type drawing
    - possibilty use system font (now default)
  2001-07-29: initial code; Magnus Flysjö }

{ todo : DrawLineItem --> }
//!    - alignment work with all separtor types
//!    - make normal/shortline types to draw differently for better transparency
//!      support when background images are implemented in the future.
procedure DrawLineItem(BarMenusIntf: IBarMenusIntf; MenuItem: TMenuItem; LineCaption: string; ACanvas: TCanvas; ARect: TRect;
  State: TOwnerDrawState);
{$IFDEF MSWINDOWS}
var
  R, TextArea, LineArea: TRect;
  Flags: Longint;
  I: Integer;
//  WinXPStyle: Boolean;
begin
  R := ARect;
  with BarMenusIntf do
  begin
//    WinXPStyle := UseMenuStyle = msWindowsXP;
    if (Separators.SeparatorStyle = ssShortLine) then
    begin
      Inc(R.Right, RightMarginFromLine);   // this was decreased for normal items
      InflateRect(R, -ShortLineSpace, 0);
    end;
    LineArea := R;
    if Length(LineCaption) > 0 then
    begin
      if (Separators.SeparatorStyle = ssCaption) then
      begin
        LineCaption := Format(' %s ', [LineCaption]);
        Flags := DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER;
        case Separators.Alignment of
          taLeftJustify:  Flags := Flags or DT_LEFT;
          taRightJustify: Flags := Flags or DT_RIGHT;
          taCenter:       Flags := Flags or DT_CENTER;
        end;
        with ACanvas do
        begin
          if not Separators.UseSystemFont then
            Font.Assign(Separators.Font);
          Brush.Color := Separators.GradientStart;
          DrawGradient(nil, ACanvas, R, Separators.GradientStart, Separators.GradientEnd,
            Separators.GradientStyle);
        end;
        ACanvas.Brush.Style := bsClear;
        DrawTextEx(ACanvas.Handle, PChar(LineCaption), Length(LineCaption), R, Flags, nil);
      end else
      begin
        ACanvas.Brush.Style := bsClear;
        TextArea := LineArea;
        Dec(TextArea.Bottom, 1);
        Flags := DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER or DT_CENTER;
        LineCaption := Format(' %s ', [LineCaption]);
        DrawTextEx(ACanvas.Handle, PChar(LineCaption), Length(LineCaption), TextArea, Flags or DT_CALCRECT, nil);

        case Separators.Alignment of
          taLeftJustify: ;
          taRightJustify: OffsetRect(TextArea, RectWidth(LineArea) - RectWidth(TextArea), 0);
          taCenter: OffsetRect(TextArea, Round((RectWidth(LineArea) - RectWidth(TextArea)) / 2 - 0.5), 0);
        end;

        Inc(R.Top, (CaptionLineItemHeight div 2) - 1);
        I := R.Right;
        if Separators.Fade then
        begin
          Inc(R.Top);
          Inc(R.Bottom);
          DrawFadeLine(ACanvas, TextArea, R, Separators.FadeColor, Separators.FadeWidth, True);
          if not Separators.FlatLines then
          begin
            Inc(R.Top);
            Inc(R.Bottom);
            DrawFadeLine(ACanvas, TextArea, R, clBtnHighlight, Separators.FadeWidth, True);
          end;
        end else
        begin
          R.Right := TextArea.Left;
          if Separators.FlatLines then
          begin
            Inc(R.Top);
            Inc(R.Bottom);
            DrawEdge(ACanvas.Handle, R, BDR_RAISEDOUTER, BF_TOP or BF_FLAT);
          end else
            DrawEdge(ACanvas.Handle, R, EDGE_ETCHED, BF_TOP);
        end;
        R.Right := I;
        R.Left := TextArea.Right;
        if not Separators.Fade then
        begin
          if Separators.FlatLines then
            DrawEdge(ACanvas.Handle, R, BDR_RAISEDOUTER, BF_TOP or BF_FLAT)
          else
            DrawEdge(ACanvas.Handle, R, EDGE_ETCHED, BF_TOP);
        end;
        DrawTextEx(ACanvas.Handle, PChar(LineCaption), Length(LineCaption), TextArea, Flags, nil);
      end;
    end else
    begin
      Inc(R.Top, (LineItemHeight div 2) - 1);
      if Separators.Fade then
      begin
        if not Separators.FlatLines then
          DrawFadeLine(ACanvas, NilRect, R, clBtnHighlight, Separators.FadeWidth, False);
        Dec(R.Top);
        Dec(R.Bottom);
        DrawFadeLine(ACanvas, NilRect, R, Separators.FadeColor, Separators.FadeWidth, False);
      end else
      begin
        if Separators.FlatLines then
          DrawEdge(ACanvas.Handle, R, BDR_RAISEDOUTER, BF_TOP or BF_FLAT)
        else
          DrawEdge(ACanvas.Handle, R, EDGE_ETCHED, BF_TOP);
      end;
    end;
  end;
end;
{$ENDIF}
{$IFDEF LINUX}
begin
  { TODO -cKylix/Linux : Linux line draw doesn't support any fancy stuff; uses VCL to draw line }
  DrawMenuItem(MenuItem, ACanvas, ARect, State);
end;
{$ENDIF}

procedure DrawFadeLine(ACanvas: TCanvas; AClipRect, ALineRect: TRect; AColor: TColor; AFadeWidth: Integer; AClip: Boolean);
var
  I, AToDiv2, ATo, AFrom, ATop, R1, G1, B1, R2, G2, B2: Integer;
  C: TColor;
begin
  AToDiv2 := ALineRect.Left - (ALineRect.Left - ALineRect.Right) div 2;
  ATop := Max(ALineRect.Top, AClipRect.Top);
  if AClip then
  begin
    ATo := Min(AToDiv2, AClipRect.Left) - 1;
    AFrom := Max(AToDiv2, AClipRect.Right);
  end else
  begin
    ATo := AToDiv2;
    AFrom := AToDiv2;
  end;
  AColor := ColorToRGB(AColor);
  R1 := TColorQuad(AColor).Red;
  G1 := TColorQuad(AColor).Green;
  B1 := TColorQuad(AColor).Blue;
  for I := ALineRect.Left to ATo do
  begin
    if I < (ALineRect.Left + AFadeWidth) then
    begin
      C := ACanvas.Pixels[I, ATop];
      R2 := TColorQuad(C).Red;
      G2 := TColorQuad(C).Green;
      B2 := TColorQuad(C).Blue;
      R2 := R2 + (((R1 - R2) * (I - ALineRect.Left)) div AFadeWidth);
      G2 := G2 + (((G1 - G2) * (I - ALineRect.Left)) div AFadeWidth);
      B2 := B2 + (((B1 - B2) * (I - ALineRect.Left)) div AFadeWidth);
      C := BcUtilities.RGB(R2, G2, B2, 0);
      ACanvas.Pixels[I, ATop] := C;
    end else
      ACanvas.Pixels[I, ATop] := AColor;
  end;
//  if (AClipRect.Right > AToDiv2) then
    for I := AFrom to ALineRect.Right do
    begin
      if I > (ALineRect.Right - AFadeWidth) then
      begin
        C := ACanvas.Pixels[I, ATop];
        R2 := TColorQuad(C).Red;
        G2 := TColorQuad(C).Green;
        B2 := TColorQuad(C).Blue;
        R2 := R2 + (((R1 - R2) * (ALineRect.Right - I)) div AFadeWidth);
        G2 := G2 + (((G1 - G2) * (ALineRect.Right - I)) div AFadeWidth);
        B2 := B2 + (((B1 - B2) * (ALineRect.Right - I)) div AFadeWidth);
        C := BcUtilities.RGB(R2, G2, B2, 0);
        ACanvas.Pixels[I, ATop] := C;
      end else
        ACanvas.Pixels[I, ATop] := AColor;
    end;
end;

{ This procedure will draw the text to doublebuffer clipping with ClipRect. }
procedure DrawBarCaption(BarMenusIntf: IBarMenusIntf; AClipRect: TRect);
var
  OldFontHandle, NewFontHandle: HFONT;
  ADepth, X, Y: Integer;
  TextSize: TSize;
  R: TRect;
  Angle: Integer;
  C: TColor;
begin
  with BarMenusIntf.BarProtected, BarMenusIntf.Bar, BarMenusIntf do
  begin
    with BarDoubleBuffer.Canvas do
    begin
      { vertical text to gradient bar }
      {$IFDEF MSWINDOWS}
      Font.Assign(BarCaption.Font);

      if BarCaption.Direction = dUpToDown then
        Angle := -90
      else
        Angle := 90;

      { Resource leak fix provided by Magnus Flysjö }
      NewFontHandle := CreateRotatedFont(Font, Angle);                       // 2001-07-11: mf
      OldFontHandle := SelectObject(Handle, NewFontHandle);                  // 2001-07-11: mf

      TextSize := TextExtent(BarCaption.Caption);
      X := Round((Width - TextSize.cy) / 2 - 0.5);

      SetBkMode(Handle, TRANSPARENT);                                        // 2001-07-11: mf

      R := AClipRect;
      Inc(R.Bottom, 1);

      Y := 0;
      case BarCaption.Alignment of
        vaTop:    Y := BarCaption.OffsetY + TextSize.cx;
        vaMiddle: Y := Round((PopupHeight + TextSize.cx) / 2 - 0.5);
        vaBottom:
          begin
            Y := PopupHeight - BarCaption.OffsetY;
            if Assigned(BarPicture.Picture) and (BarPicture.VertAlignment = vaBottom) then
              Y := Y - BarPicture.Picture.Height;
          end;
      end;
      if BarCaption.Direction = dUpToDown then
      begin
        Dec(Y, TextSize.cx);
        Inc(X, TextSize.cy + 1);
      end;

      ADepth := BarCaption.Depth;
      { highlight }
      C := ColorToRGB(BarCaption.HighlightColor);
      if (C <> clNone) then
      begin
        SetTextColor(Handle, Cardinal(C));
        ExtTextOut(Handle, X - ADepth, Y + ADepth, ETO_CLIPPED, @R, PChar(BarCaption.Caption),
          Length(BarCaption.Caption), nil);
      end;

      { shadow }
      C := ColorToRGB(BarCaption.ShadowColor);
      if (C <> clNone) then
      begin
        SetTextColor(Handle, Cardinal(C));
        ExtTextOut(Handle, X + ADepth, Y - ADepth, ETO_CLIPPED, @R, PChar(BarCaption.Caption),
          Length(BarCaption.Caption), nil);
      end;

      { text itself }
      SetTextColor(Handle, BarCaption.Font.Color);
      ExtTextOut(Handle, X, Y, ETO_CLIPPED, @R, PChar(BarCaption.Caption),
        Length(BarCaption.Caption), nil);

      { restore old font and delete new font }
      SelectObject(Handle, OldFontHandle);                                   // 2001-07-11: mf
      DeleteObject(NewFontHandle);                                           // 2001-07-11: mf
      SetBkMode(Handle, OPAQUE);                                             // 2001-07-11: mf

      {$ENDIF}
      {$IFDEF LINUX}
      { TODO -oJouni Airaksinen -cKylix/Linux : Kylix and vertical text .. hmm. need to check X Api, etc.. Unless
        there is direct support in Kylix TFont. }
      {$ENDIF}
    end;
  end;
end;

{ Draws gradient bar to the bitmap or canvas with given parameters. Does not support
  negative rects. Switch ColorFrom/ColorTo to archive same effect. ARect
  should be inside valid ABitmap. Set ABitmap to nil if you want to use ACanvas.
  Using TBitmap is faster but not always possible. }
procedure DrawGradient(ABitmap: TBitmap; ACanvas: TCanvas; ARect: TRect; ColorFrom, ColorTo: TColor; Style: TGradientStyle);
var
  UseBitmap: Boolean;
  DrawCanvas: TCanvas;
  OldBrushHandle, NewBrushHandle: HBRUSH;

  GradientWidth, GradientHeight: Integer;
  DrawFrom, DrawTo, I, X, Y: Integer;

  ColorFromQuad, ColorToQuad: TLargeColorQuad;
  ColorValue: TColor;
  P, PF: P32bitQuadScanLine;

  procedure FillScanLine(AColor: TColor);
  var X: Integer;
  begin
    if UseBitmap then
    begin
      for X := ARect.Left to ARect.Right - 1 do
      begin
        {$R-}
        P^[X].Red := TColorQuad(AColor).Red;
        P^[X].Green := TColorQuad(AColor).Green;
        P^[X].Blue := TColorQuad(AColor).Blue;
        P^[X].Alpha := $00;
      end;
    end else
    begin
      {$IFDEF MSWINDOWS}
      NewBrushHandle := CreateSolidBrush(AColor);
      OldBrushHandle := SelectObject(DrawCanvas.Handle, NewBrushHandle);
      try
        PatBlt(DrawCanvas.Handle, ARect.Left, Y, GradientWidth, 1, PATCOPY);
      finally
        SelectObject(DrawCanvas.Handle, OldBrushHandle);
        DeleteObject(NewBrushHandle);
      end;
      {$ENDIF}
      {$IFDEF LINUX}
      DrawCanvas.Brush.Color := ColorValue;
      FillRect(Rect(ARect.Left, Y, Width, Y + 1));
      {$ENDIF}
    end;
  end;

begin
  UseBitmap := Assigned(ABitmap);
  if UseBitmap then
  begin
    DrawCanvas := ABitmap.Canvas;
    ABitmap.PixelFormat := pf32bit;
  end else
    DrawCanvas := ACanvas;

  with DrawCanvas do
  begin
    GradientWidth := ARect.Right - ARect.Left;
    GradientHeight := ARect.Bottom - ARect.Top;
    DrawFrom := ARect.Top;
    DrawTo := ARect.Bottom - 1;
    {$IFDEF LINUX}
    if UseBitmap then
      Brush.Style := bsSolid;
    {$ENDIF}
    if (ColorFrom = ColorTo) then { same color, just one drawing phase required }
    begin
      if UseBitmap then
      begin
        for I := DrawFrom to DrawTo do
        begin
          P := ABitmap.ScanLine[I];
          FillScanLine(ColorFrom);
        end;
      end else
      begin
        {$IFDEF MSWINDOWS}
        NewBrushHandle := CreateSolidBrush(ColorFrom);
        OldBrushHandle := SelectObject(Handle, NewBrushHandle);
        try
          PatBlt(Handle, 0, ARect.Top, GradientWidth, ARect.Bottom, PATCOPY);
        finally
          SelectObject(Handle, OldBrushHandle);
          DeleteObject(NewBrushHandle);
        end;
        {$ENDIF}
        {$IFDEF LINUX}
        Brush.Color := GradientStart;
        FillRect(Rect(0, ARect.Top, Width, ARect.Bottom));
        {$ENDIF}
      end;
    end else
    begin
      ColorValue := ColorToRGB(ColorFrom);
      ColorFromQuad.Red := TColorQuad(ColorValue).Red;
      ColorFromQuad.Green := TColorQuad(ColorValue).Green;
      ColorFromQuad.Blue := TColorQuad(ColorValue).Blue;

      ColorValue := ColorToRGB(ColorTo);
      ColorToQuad.Red := TColorQuad(ColorValue).Red - ColorFromQuad.Red;
      ColorToQuad.Green := TColorQuad(ColorValue).Green - ColorFromQuad.Green;
      ColorToQuad.Blue := TColorQuad(ColorValue).Blue - ColorFromQuad.Blue;

      if GradientHeight > 0 then
      begin
        case Style of
          gsVertical:
            begin
              for Y := DrawFrom to DrawTo do
              begin
                I := Y - DrawFrom;
                ColorValue := BcUtilities.RGB(
                  (ColorFromQuad.Red + ((ColorToQuad.Red * I) div GradientHeight)),
                  (ColorFromQuad.Green + ((ColorToQuad.Green * I) div GradientHeight)),
                  (ColorFromQuad.Blue + ((ColorToQuad.Blue * I) div GradientHeight)));
                if UseBitmap then
                  P := ABitmap.ScanLine[Y];
                try
                  FillScanLine(ColorValue);
                except
                  Exit;
                end;
              end;
            end;
          gsDiagonalLeftRight, gsDiagonalRightLeft:
            begin
              { draw first line of the gradient }
              for Y := DrawFrom to DrawTo do
              begin
                if UseBitmap then
                  P := ABitmap.ScanLine[Y];
                for X := ARect.Left to ARect.Right - 1 do
                begin
                  { I is Integer so to get decimal precision we use 1024 as multiplier.
                    Formula is "(a + b) / 2" (where a,b are percents) the precision
                    1024 is pre-divided. }
                  I := Trunc(((Y - DrawFrom) / GradientHeight + (X - ARect.Left) / GradientWidth) * 512);
                  ColorValue := BcUtilities.RGB(
                    (ColorFromQuad.Red + ((ColorToQuad.Red * I) div 1024)),
                    (ColorFromQuad.Green + ((ColorToQuad.Green * I) div 1024)),
                    (ColorFromQuad.Blue + ((ColorToQuad.Blue * I) div 1024)));
                  try
                    if Style = gsDiagonalRightLeft then
                      I := (ARect.Right - 1) - X + ARect.Left // flip on X axis
                    else
                      I := X;
                    if UseBitmap then
                    begin
                      P^[I].Red := TColorQuad(ColorValue).Red;
                      P^[I].Green := TColorQuad(ColorValue).Green;
                      P^[I].Blue := TColorQuad(ColorValue).Blue;
                      P^[I].Alpha := $00;
                    end else
                      Pixels[I, Y] := ColorValue;
                  except
                    Exit;
                  end;
                end;
              end;
            end;
          gsHorizontal:
            begin
              PF := nil;
              if UseBitmap then
                PF := ABitmap.ScanLine[DrawFrom];
              for X := ARect.Left to ARect.Right - 1 do
              begin
                I := X - ARect.Left;
                ColorValue := BcUtilities.RGB(
                  (ColorFromQuad.Red + ((ColorToQuad.Red * I) div GradientWidth)),
                  (ColorFromQuad.Green + ((ColorToQuad.Green * I) div GradientWidth)),
                  (ColorFromQuad.Blue + ((ColorToQuad.Blue * I) div GradientWidth)));
                try
                  if UseBitmap and Assigned(PF) then
                  begin
                    PF^[X].Red := TColorQuad(ColorValue).Red;
                    PF^[X].Green := TColorQuad(ColorValue).Green;
                    PF^[X].Blue := TColorQuad(ColorValue).Blue;
                    PF^[X].Alpha := $00;
                  end else
                    Pixels[X, DrawFrom] := ColorValue;
                except
                  Exit;
                end;
              end;
              { copy the first line till end }
              if UseBitmap then
              begin
                for Y := DrawFrom + 1 to DrawTo do
                  for X := ARect.Left to ARect.Right - 1 do
                  begin
                    P := ABitmap.ScanLine[Y];
                    P^[X].Red := PF^[X].Red;
                    P^[X].Green := PF^[X].Green;
                    P^[X].Blue := PF^[X].Blue;
                    P^[X].Alpha := PF^[X].Alpha;
                  end;
              end else
              begin
                CopyRect(Rect(ARect.Left, DrawFrom + 1, ARect.Right, DrawTo + 1),
                  DrawCanvas,
                  Rect(ARect.Left, DrawFrom, ARect.Right, DrawFrom + 1));
              end;
            end;
        end;
      end;
    end;
  end;
end;

{ This procedure will overwrite the doublebuffer content from ARect. }
procedure DrawBarGradient(BarMenusIntf: IBarMenusIntf; ARect: TRect);
begin
  with BarMenusIntf.BarProtected, BarMenusIntf.Bar, BarMenusIntf do
  begin
    { pre-check bar drawing so we don't spend time drawing gradient bar, if
      it's going to be erased anyway. }
    with BarBackPicture do
      if not Transparent and Assigned(Picture) and
        (DrawStyle in [dsTile, dsStretch])
        and not Picture{$IFNDEF PICTUREASTBITMAP}.Graphic{$ENDIF}.Empty then Exit;

   DrawGradient(BarDoubleBuffer, nil, ARect, GradientStart, GradientEnd, GradientStyle);
  end;
end;

procedure DrawBarPicture(BarMenusIntf: IBarMenusIntf);
var
  X, Y: Integer;
  APicture: TBcPicture;
begin
  with BarMenusIntf.BarProtected, BarMenusIntf.Bar, BarMenusIntf do
  begin
    with BarDoubleBuffer.Canvas do
    begin
      { calculate position and draw bar bitmap }
      APicture := BarPicture.Picture;
      if Assigned(APicture) and
        not APicture{$IFNDEF PICTUREASTBITMAP}.Graphic{$ENDIF}.Empty then
      begin
//        APicture.Transparent := BarPicture.Transparent;
//        APicture.TransparentMode := tmAuto;
        BarPicture.CalcPicturePosition(X, Y);
        Draw(X, Y, APicture{$IFNDEF PICTUREASTBITMAP}.Graphic{$ENDIF});
      end;
    end;
  end;
end;

{ This procedure will overwrite the doublebuffer content, if the BackBitmap is
  set and isn't empty. However, bitmap is drawn transparently. }
procedure DrawBarBackPicture(BarMenusIntf: IBarMenusIntf);
var
  X, Y: Integer;
  XStart, YStart: Integer;
  ABarDoubleBuffer: TBitmap;
  APicture: TBcPicture;

  function AlignPositiveOffset(Size, Offset: Integer): Integer;
  begin
    if Offset > 0 then
    begin
      while Offset > 0 do
        Dec(Offset, Size);
    end;
    Result := Offset;
  end;

begin
  with BarMenusIntf.BarProtected, BarMenusIntf.Bar, BarMenusIntf do
  begin
    with Bar.BarBackPicture do
    begin
      ABarDoubleBuffer := BarDoubleBuffer;
      APicture := Picture;
      if Assigned(APicture) and not APicture{$IFNDEF PICTUREASTBITMAP}.Graphic{$ENDIF}.Empty then
      begin
        case DrawStyle of
          dsNormal:
            begin
              CalcPicturePosition(X, Y);
              ABarDoubleBuffer.Canvas.Draw(X, Y, APicture{$IFNDEF PICTUREASTBITMAP}.Graphic{$ENDIF});
            end;
          dsTile:
            begin
              { make sure we don't end up infinite loops }
              if (APicture.Width > 0) and (APicture.Height > 0) then
              begin
                { we align the tiling starting offset so it will look like
                  if the tiling would be aligned. }
                XStart := 0;
                YStart := 0;
                case HorzAlignment of
                  haLeft: XStart := 0;
                  haRight, haCenter:
                    XStart := -(APicture.Width - (ABarDoubleBuffer.Width mod APicture.Width));
                end;
                if HorzAlignment = haCenter then  // make it look like centering
                  XStart := XStart div 2;

                case VertAlignment of
                  vaTop: YStart := 0;
                  vaBottom, vaMiddle:
                    YStart := -(APicture.Height - (ABarDoubleBuffer.Height mod APicture.Height));
                end;
                if VertAlignment = vaMiddle then  // make it look like centering
                  YStart := YStart div 2;

                Y := AlignPositiveOffset(APicture.Height, YStart + OffsetY);
                while Y <= ABarDoubleBuffer.Height do
                begin
                  X := AlignPositiveOffset(APicture.Width, XStart + OffsetX);
                  while X <= ABarDoubleBuffer.Width do
                  begin
                    ABarDoubleBuffer.Canvas.Draw(X, Y, APicture{$IFNDEF PICTUREASTBITMAP}.Graphic{$ENDIF});
                    Inc(X, APicture.Width);
                  end;
                  Inc(Y, APicture.Height);
                end;
              end;
            end;
          dsStretch: { No support for Offsets nor Alignments }
            ABarDoubleBuffer.Canvas.StretchDraw(BitmapRect(ABarDoubleBuffer),
              APicture{$IFNDEF PICTUREASTBITMAP}.Graphic{$ENDIF});
        end;
      end;
      { draw border }
      if Border <> clNone then
        with ABarDoubleBuffer.Canvas do
        begin
          Brush.Color := Border;
          FrameRect(BitmapRect(ABarDoubleBuffer));
        end;
    end;
  end;
end;

procedure DrawDoubleBuffer(BarMenusIntf: IBarMenusIntf; ACanvas: TCanvas; Dest, Source: TRect);
begin
  with BarMenusIntf.BarProtected do
    ACanvas.CopyRect(Dest, BarDoubleBuffer.Canvas, Source);
end;

function GetParentMenuEx(MenuItem: TMenuItem): TMenu;
begin
  Result := nil;
  if not Assigned(MenuItem) then Exit;
  if not Assigned(MenuItem.Parent) then
    Result := TMenu(MenuItem.Owner)
  else
    Result := MenuItem.GetParentMenu;
end;

procedure SetMenuItemEvents(Menu: TMenu; MenuItem: TMenuItem; ExpandItemWidth: TMenuMeasureItemEvent;
  AdvancedDrawItem: TAdvancedMenuDrawItemEvent; Recursive: Boolean; AllowOwnerDrawn: Boolean);

  procedure SetBarEvents(AMenuItem: TMenuItem);
  begin
    if not (csLoading in Menu.ComponentState) then
      with AMenuItem do
      begin
        if (Addr(OnAdvancedDrawItem) <> Addr(AdvancedDrawItem)) then
        begin
          if not Assigned(OnMeasureItem) or not AllowOwnerDrawn then
            OnMeasureItem := ExpandItemWidth;
          if (not Assigned(OnAdvancedDrawItem) and not Assigned(OnDrawItem)) or
            not AllowOwnerDrawn then
            OnAdvancedDrawItem := AdvancedDrawItem;
        end;
      end;
  end;

  procedure SetBarEventsForItems(AMenuItem: TMenuItem; ARecursive: Boolean);
  var
    I: Integer;
  begin
    for I := 0 to AMenuItem.Count - 1 do
    begin
      SetBarEvents(AMenuItem.Items[I]);
      if ARecursive and (AMenuItem.Items[I].Count > 0) then
        SetBarEventsForItems(AMenuItem.Items[I], ARecursive);
    end;
  end;

begin
  if Assigned(MenuItem) then
    SetBarEvents(MenuItem)
  else
    SetBarEventsForItems(Menu.Items, Recursive);
end;

procedure SetMenuOpacity(MenuItem: TMenuItem; Canvas: TCanvas);
var BarMenusIntf: IBarMenusIntf;
begin
  BarMenusIntf := GetMenuItemBarMenusIntf(MenuItem);
  SetMenuOpacity(BarMenusIntf, WindowFromDC(Canvas.Handle), BarMenusIntf.Opacity);
end;

procedure SetMenuOpacity(BarMenusIntf: IBarMenusIntf; const WindowHandle: THandle; const Opacity: TOpacity);
var
  IsLayered: Boolean;
  Flags: Longint;
begin
  if WindowHandle = 0 then
    Exit;
  if SupportsLayeredWindows then // has layered support
    with BarMenusIntf do
    begin
      { TMainMenu is problematic because the handles are system wide, so
        setting layered, will affect other applications too!! Solution ->
        todo own menutoolbar which uses popupmenus. For now, MainMenu does
        not have opacity. }
      Flags := GetWindowLong(WindowHandle, GWL_EXSTYLE);
      IsLayered := ((Flags and WS_EX_LAYERED) <> 0);
      if (Self is TPopupMenu) then
      begin
        if (not IsLayered) and (Opacity < 255) then
        begin
          SetWindowLong(WindowHandle, GWL_EXSTYLE, Flags or WS_EX_LAYERED);
          SetLayeredWindowAttributes(WindowHandle, 0, Opacity, LWA_ALPHA);
        end;
      end
{$IFOPT D+}
      { Debugging code which disables the layered flags in TMainMenu }
      else if (Self is TMainMenu) then
      begin
        if IsLayered then
        begin
          SetLayeredWindowAttributes(WindowHandle, 0, 255, LWA_ALPHA);
          SetWindowLong(WindowHandle, GWL_EXSTYLE, Flags and not WS_EX_LAYERED);
        end;
      end;
{$ENDIF}
    end;
end;

procedure DrawMenuWindowBorder(MenuItem: TMenuItem; Canvas: TCanvas);
begin
  DrawMenuWindowBorder(GetMenuItemBarMenusIntf(MenuItem), WindowFromDC(Canvas.Handle));
end;

procedure DrawMenuWindowBorder(BarMenusIntf: IBarMenusIntf; WindowHandle: THandle);
type
  TFlatMenuOption = (moDropShadow, moObiliqueAngled);
  TFlatMenuOptions = set of TFlatMenuOption;
  PPoints = ^TPoints;
  TPoints = array[0..0] of TPoint;

const
  MenuBorderColor = clBtnShadow;
  DropHeight = 4; { 4x4 pixel boxes "chopped" from corners }
  ObiliqueAngelSize = 12; { 12x12 box diagonally cut }

//  FlatMenuOptions: TFlatMenuOptions = [msObiliqueAngled];
  FlatMenuOptions: TFlatMenuOptions = [];

  USERDATA_NULL        = $00000000;
  USERDATA_REGIONSET   = $00000001;
  USERDATA_BORDERDRAWN = $00000002;

var
  R: TRect;
  WindowRect: TRect;
  ClientRect: TRect;
  WindowCanvas: TCanvas;
  WindowRgnMenu, WindowRgnShadow: HRGN;
  UserFlags: Longint;
  OldPenColor: TColor;

  function CreatePolygonRgn(const Points: array of TPoint): HRGN;
  begin
    Result := Windows.CreatePolygonRgn(PPoints(@Points)^, High(Points) + 1, ALTERNATE);
  end;

begin
  if WindowHandle = 0 then
    Exit;
  if not (csDesigning in BarMenusIntf.Self.ComponentState) and BarMenusIntf.Flat then
  begin
    with BarMenusIntf do
    begin
      { check for valid handle and for non-mainmenu topitem }
      if (WindowHandle > 0) and (WindowHandle <> Self.WindowHandle) then
      begin
        UserFlags := GetWindowLong(WindowHandle, GWL_USERDATA);
        if ((UserFlags and USERDATA_REGIONSET) = 0) then
        begin
          try
            GetClientRect(WindowHandle, ClientRect);
            GetWindowRect(WindowHandle, WindowRect);
            if (Self is TPopupMenu) then
            begin
              R := Rect(0, 0, RectWidth(WindowRect){ - DropHeight}, RectHeight(WindowRect){ - DropHeight});
              { todo : should flat angel be mirrored if bidimode fromrighttoleft? }
              if (moObiliqueAngled in FlatMenuOptions) then
              begin
                try
                  WindowRgnMenu := CreatePolygonRgn([
                    R.TopLeft,
                    Point(R.Right - ObiliqueAngelSize, 0),
                    Point(R.Right, ObiliqueAngelSize),
                    R.BottomRight,
                    Point(ObiliqueAngelSize, R.Bottom),
                    Point(0, R.Bottom - ObiliqueAngelSize),
                    R.TopLeft
                    ]);
                finally
                  //
                end;
              end else
              begin
                WindowRgnMenu := CreateRectRgnIndirect(R);
              end;

              if (moDropShadow in FlatMenuOptions) then
              begin
                WindowRgnShadow := 0;
                try
                  WindowRgnShadow := CreateRectRgnIndirect(ClientRect); // just create something
                  CombineRgn(WindowRgnShadow, WindowRgnMenu, 0, RGN_COPY);
                  OffsetRgn(WindowRgnShadow, DropHeight, DropHeight);
                  CombineRgn(WindowRgnMenu, WindowRgnMenu, WindowRgnShadow, RGN_OR);
                finally
                  DeleteObject(WindowRgnShadow);
                end;
              end;

              SetWindowRgn(WindowHandle, WindowRgnMenu, True); // this region is deleted automatically by system
            {$IFOPT D+}
            end else
              SetWindowRgn(WindowHandle, 0, True);
            {$ELSE}
            end;
            {$ENDIF}
          finally
            SetWindowLong(WindowHandle, GWL_USERDATA, UserFlags or USERDATA_REGIONSET); // set the user flag
          end;
        end;
//        if ((UserFlags and USERDATA_BORDERDRAWN) = 0) then
        begin
          WindowCanvas := nil;
          try
            WindowCanvas := TCanvas.Create;
            WindowCanvas.Handle := GetWindowDC(WindowHandle);

            WindowCanvas.Brush.Color := MenuBorderColor;
            R := Rect(0, 0, RectWidth(WindowRect), RectHeight(WindowRect));
            with R do
              WindowCanvas.FrameRect(R);

            InflateRect(R, -1, -1);

            WindowCanvas.Brush.Color := clBtnFace;
            with R do
              WindowCanvas.FrameRect(R);

            if (moObiliqueAngled in FlatMenuOptions) then
            begin
              with WindowCanvas do
              begin
                InflateRect(R, 1, 1);
                OldPenColor := Pen.Color;
                Pen.Color := MenuBorderColor;

                MoveTo(R.Right - ObiliqueAngelSize - 1, R.Top);
                LineTo(R.Right, ObiliqueAngelSize + 1);

                MoveTo(0, R.Bottom - ObiliqueAngelSize);
                LineTo(ObiliqueAngelSize, R.Bottom);

                InflateRect(R, -1, -1);

                Pen.Color := clBtnFace;

                MoveTo(R.Right - ObiliqueAngelSize, R.Top);
                LineTo(R.Right, ObiliqueAngelSize + 1);
                MoveTo(R.Right - ObiliqueAngelSize - 1, R.Top);
                LineTo(R.Right, ObiliqueAngelSize + 2);

                MoveTo(1, R.Bottom - ObiliqueAngelSize + 1);
                LineTo(ObiliqueAngelSize, R.Bottom);
                MoveTo(1, R.Bottom - ObiliqueAngelSize);
                LineTo(ObiliqueAngelSize + 1, R.Bottom);

                Pen.Color := OldPenColor;
              end;
            end;

          finally
            ReleaseDC(WindowHandle, WindowCanvas.Handle);
            WindowCanvas.Handle := 0;
            FreeAndNil(WindowCanvas);
//            SetWindowLong(WindowHandle, GWL_USERDATA, UserFlags or USERDATA_BORDERDRAWN); // set the user flag
          end;
        end;
      end;
    end;
  end;
end;

procedure RefreshMenu(BarMenusIntf: IBarMenusIntf; const BarChanged, WidthChanged: Boolean);
begin
  { force doublebuffer to redraw }
  with BarMenusIntf do
  begin
    if BarChanged then
    begin
      with BarProtected do
      begin
        UpdatePopupHeight := True;
        Drawn := False;
      end;
    end;
    { Windows.DrawMenuBar does not execute MeasureItem events unless menu needs
      rebuild. UpdateItems forces to rebuild and redraw. For PopupMenus the
      MeasureItem events are executed. }
    if BarChanged or WidthChanged then
      UpdateItems { todo : check if this is required only in designtime }
    else begin
      { if menu is TMainMenu, then the WindowHandle points to the form which
        uses the menu. If no form uses it, WindowHandle is nil. }
      if (Self is TMainMenu) and (Self.WindowHandle <> 0) then
        Windows.DrawMenuBar(Self.WindowHandle);
    end;
  //  Self.MenuChanged(Self, FirstVisibleMenuItem(Self), (BarChanged or WidthChanged));
  end;
end;

function FirstVisibleMenuItem(Menu: TMenu): TMenuItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Menu.Items.Count - 1 do
  begin
    if Menu.Items[I].Visible then // find first visible item
    begin
      Result := Menu.Items[I];
      Break;
    end;
  end;
end;

function IsInFirstVisibleMenu(Menu: TMenu; MenuItem: TMenuItem): Boolean;
var FirstMI: TMenuItem;
begin
  Result := False;
  if not Assigned(MenuItem) then Exit;
  FirstMI := FirstVisibleMenuItem(Menu);
  if Assigned(FirstMI) then
    Result := (MenuItem.Parent = FirstMI);
end;

function IsInTopMenu(MenuItem: TMenuItem): Boolean;
begin
  { tells if the item is in top menu of mainmenu or popupmenu }
  Result := MenuItem.GetParentComponent is TMenu;
end;

function IsInTopPopupMenu(MenuItem: TMenuItem): Boolean;
begin
  { tells if the item is in top menu of popupmenu }
  Result := MenuItem.GetParentComponent is TPopupMenu;
end;

function IsInTopMainMenu(MenuItem: TMenuItem): Boolean;
begin
  { tells if the item is in top menu of mainmenu }
  Result := MenuItem.GetParentComponent is TMainMenu;
end;

{ look back for visible menu items and for break items }
function IsAfterMenuBreak(MenuItem: TMenuItem): Boolean;
var
  I: Integer;
  PMI: TMenuItem;
begin
  Result := False;
  if not Assigned(MenuItem.Parent) then
    Exit;
  with MenuItem do
  begin
    for I := MenuIndex{ - 1} downto 0 do
    begin
      PMI := Parent.Items[I];
      if not PMI.Visible then
        Continue;
      if PMI.Break <> mbNone then
      begin
        Result := True;
        System.Break;
      end;
    end;
  end;
end;

function GetMenuBarMenusIntf(AMenu: TMenu): IBarMenusIntf;
begin
  Result := nil;
  if AMenu is TBcCustomBarPopupMenu then
    Result := TBcCustomBarPopupMenu(AMenu)
  else if AMenu is TBcCustomBarMainMenu then
    Result := TBcCustomBarMainMenu(AMenu);
end;

function GetMenuItemBarMenusIntf(AMenuItem: TMenuItem): IBarMenusIntf;
begin
  Result := GetMenuBarMenusIntf(GetParentMenuEx(AMenuItem));
end;

function BcShortCutToText(ShortCut: TShortCut): string;
begin
  Result := ShortCutToText(ShortCut);
  if (ShortCut and scWinKey) <> 0 then
    Result := BcWinKeyString + Result;
end;

{ TBcBarProtected }

{ ============================================================================
  TBcBarProtected.Create
  TBcBarProtected.Destroy
  ---------------------------------------------------------------------------- }
constructor TBcBarProtected.Create(AOwner: TMenu);
begin
  inherited Create;
  if not Assigned(AOwner) then
    raise ERequireOwner.CreateFmt(SRequireOwner, [Self.ClassName]);
  FOwner := AOwner;

  FDrawn := False;
  FPopupHeight := 0;
  FUpdatePopupHeight := True;
end;

destructor TBcBarProtected.Destroy;
begin
  FreeAndNil(FBarDoubleBuffer);
  inherited Destroy;
end;

{ ============================================================================
  TBcBarProtected
  Set methods
  ---------------------------------------------------------------------------- }
procedure TBcBarProtected.SetBarDoubleBuffer(const Value: TBitmap);
begin
{$IFNDEF DESIGNTIMEPREVIEW}
  if (csDesigning in Owner.ComponentState) then Exit;
{$ENDIF}
  if not Assigned(FBarDoubleBuffer) then
  begin
    FBarDoubleBuffer := TBitmap.Create;
    UpdatePopupHeight := True;
  end;
  FBarDoubleBuffer.Assign(Value);
end;

procedure TBcBarProtected.SetUpdatePopupHeight(const Value: Boolean);
begin
  FUpdatePopupHeight := Value;
  if Value then
    FPopupHeight := 0;
end;

procedure TBcBarProtected.SetPopupHeight(const Value: Integer);
begin
  FPopupHeight := Value;
end;

{ ============================================================================
  TBcBarProtected
  Get methods
  ---------------------------------------------------------------------------- }
function TBcBarProtected.GetBarDoubleBuffer: TBitmap;
begin
{$IFNDEF DESIGNTIMEPREVIEW}
  Result := nil;
  if not (csDesigning in Owner.ComponentState) then
{$ENDIF}
  begin
    if not Assigned(FBarDoubleBuffer) then
    begin
      FBarDoubleBuffer := TBitmap.Create;
//      UpdatePopupHeight := True;
    end;

    Result := FBarDoubleBuffer;
  end;
end;

procedure TBcBarProtected.FreeBarDoubleBuffer;
begin
  FreeAndNil(FBarDoubleBuffer);
  UpdatePopupHeight := True;
end;

{ TBcGradientBar }

{ ============================================================================
  TBcGradientBar.Create
  TBcGradientBar.Destroy
  ---------------------------------------------------------------------------- }
constructor TBcGradientBar.Create(AOwner: TMenu);
begin
  inherited Create;
  if not Assigned(AOwner) then
    raise ERequireOwner.CreateFmt(SRequireOwner, [Self.ClassName]);
  FOwner := AOwner;

  FGradientStart := DefaultGradientStart;
  FGradientEnd := DefaultGradientEnd;
  FBorder := DefaultBarBorder;
  FGradientStyle := DefaultGradientStyle;
end;

destructor TBcGradientBar.Destroy;
begin
  inherited Destroy;
end;

function TBcGradientBar.GetGradientColor(const Index: Integer): TColor;
begin
  case Index of
    0: Result := FGradientStart;
    1: Result := FGradientEnd;
  else
    raise EInvalidPropertyIndex.CreateFmt(SInvalidPropertyIndex, [Index]);
  end;
end;

procedure TBcGradientBar.SetGradientColor(const Index: Integer; const Value: TColor);
begin
  case Index of
    0: FGradientStart := Value;
    1: FGradientEnd := Value;
  else
    raise EInvalidPropertyIndex.CreateFmt(SInvalidPropertyIndex, [Index]);
  end;
end;

procedure TBcGradientBar.SetGradientStyle(const Value: TGradientStyle);
begin
  FGradientStyle := Value;
end;

{ ============================================================================
  TBcBarGradient.Assign
  ---------------------------------------------------------------------------- }
procedure TBcGradientBar.Assign(Source: TPersistent);
begin
  if Source is TBcGradientBar then
  begin
    with TBcGradientBar(Source) do
    begin
      Self.GradientStart := GradientStart;
      Self.GradientEnd := GradientEnd;
      Self.GradientStyle := GradientStyle;
      Self.Border := Border;
    end;
  end else
    inherited Assign(Source); // raises an exception if cannot assign
end;

{ TBcSeparators }

{ ============================================================================
  TBcSeparators.Create
  TBcSeparators.Destroy
  ---------------------------------------------------------------------------- }
constructor TBcSeparators.Create(AOwner: TMenu);
begin
  inherited Create(AOwner);

  FFont := TFont.Create;
  FGradientStart := DefaultSeparatorsGradientStart;
  FGradientEnd := DefaultSeparatorsGradientEnd;
  FGradientStyle := DefaultSeparatorsGradientStyle;
  FAlignment := DefaultSeparatorsAlignment;
  FSeparatorStyle := DefaultSeparatorsSeparatorStyle;
  FUseSystemFont := DefaultSeparatorsUseSystemFont;
  FFade := DefaultSeparatorsFade;
  FFadeWidth := DefaultSeparatorsFadeWidth;
  FFadeColor := DefaultSeparatorsFadeColor;
  FFlatLines := DefaultSeparatorsFlatLines;
end;

destructor TBcSeparators.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TBcSeparators.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

{ ============================================================================
  TBcSeparators.Assign
  ---------------------------------------------------------------------------- }
procedure TBcSeparators.Assign(Source: TPersistent);
begin
  if Source is TBcSeparators then
  begin
    with TBcSeparators(Source) do
    begin
      Self.Font := Font;
      Self.Alignment := Alignment;
      Self.SeparatorStyle := SeparatorStyle;
      Self.UseSystemFont := UseSystemFont;
    end;
  end;
  inherited Assign(Source); // raises an exception if cannot assign
end;

{ TBcBar }

{ ============================================================================
  TBcBar.Create
  TBcBar.Destroy
  ---------------------------------------------------------------------------- }
constructor TBcBar.Create(AOwner: TMenu);
begin
  inherited Create(AOwner);

  FSpace := DefaultBarSpace;
  FWidth := DefaultBarWidth;
  FVisible := DefaultBarVisible;
  FSide := DefaultBarSide;

  { Note that we use Owner as parameter and not Self. This way
    the objects can access then Owner menu directly. }
  FBarPicture := TBcBarPicture.Create(Owner);
  FBarBackPicture := TBcBarBackPicture.Create(Owner);
  FBarCaption := TBcBarCaption.Create(Owner);
end;

destructor TBcBar.Destroy;
begin
  FreeAndNil(FBarPicture);
  FreeAndNil(FBarBackPicture);
  FreeAndNil(FBarCaption);

  inherited Destroy;
end;

{ ============================================================================
  TBcBar
  Set methods
  ---------------------------------------------------------------------------- }
procedure TBcBar.SetVisible(Value: Boolean);
var ABarMenusIntf: IBarMenusIntf;
begin
{$IFNDEF DESIGNTIMEPREVIEW}
  if not (csDesigning in Owner.ComponentState) then
{$ENDIF}
  begin
    if not Value then
    begin
      ABarMenusIntf := GetMenuBarMenusIntf(Owner);
      with ABarMenusIntf do
      begin
        FreeAndNil(BarProtected.FBarDoubleBuffer);
        BarProtected.UpdatePopupHeight := True;
        BarProtected.Drawn := False;
      end;
    end;
  end;
  FVisible := Value;
{$IFDEF DESIGNTIMEPREVIEW}
  if (Owner is TBcCustomBarMainMenu) and (Owner.WindowHandle <> 0) then
  begin
    TBcCustomBarMainMenu(Owner).RefreshMenu(False, True);
  end;
{$ENDIF}
end;

procedure TBcBar.SetBarPicture(const Value: TBcBarPicture);
begin
  FBarPicture.Assign(Value);
end;

procedure TBcBar.SetBarBackPicture(const Value: TBcBarBackPicture);
begin
  FBarBackPicture.Assign(Value);
end;

procedure TBcBar.SetBarCaption(const Value: TBcBarCaption);
begin
  FBarCaption.Assign(Value);
end;

{ ============================================================================
  TBcBar
  Get methods
  ---------------------------------------------------------------------------- }
function TBcBar.GetBarPicture: TBcBarPicture;
begin
  Result := FBarPicture;
end;

function TBcBar.GetBarBackPicture: TBcBarBackPicture;
begin
  Result := FBarBackPicture;
end;

function TBcBar.GetBarCaption: TBcBarCaption;
begin
  Result := FBarCaption;
end;

function TBcBar.GetVisible: Boolean;
begin
  Result := FVisible;
end;

{ ============================================================================
  TBcBar.Assign
  ---------------------------------------------------------------------------- }
procedure TBcBar.Assign(Source: TPersistent);
begin
  if Source is TBcBar then
  begin
    with TBcBar(Source) do
    begin
      Self.Width := Width;
      Self.Visible := Visible;
      Self.Side := Side;
      Self.Space := Space;
      Self.BarPicture.Assign(BarPicture);
      Self.BarBackPicture.Assign(BarBackPicture);
      Self.BarCaption.Assign(BarCaption);
    end;
  end;
  inherited Assign(Source); // try to assign TBcGradientBar
end;

{ TBcBarPicture }

{ ============================================================================
  TBcBarPicture.Create
  TBcBarPicture.Destroy
  ---------------------------------------------------------------------------- }
constructor TBcBarPicture.Create(AOwner: TMenu);
begin
  inherited Create;
  if not Assigned(AOwner) then
    raise ERequireOwner.CreateFmt(SRequireOwner, [Self.ClassName]);
  FOwner := AOwner;

  FTransparent := DefaultBarBitmapTransparent;

  FOffsetX := 0;
  FOffsetY := 0;
  FVertAlignment := DefaultBarBitmapVertAlignment;
  FHorzAlignment := DefaultBarBitmapHorzAlignment;
  FVisible := DefaultBarBitmapVisible;
end;

destructor TBcBarPicture.Destroy;
begin
  if Assigned(FPicture) then
    FreeAndNil(FPicture);

  inherited Destroy;
end;

{ ============================================================================
  TBcBarPicture.Assign
  ---------------------------------------------------------------------------- }
procedure TBcBarPicture.Assign(Source: TPersistent);
begin
  if Source is TBcBarPicture then
  begin
    with TBcBarPicture(Source) do
    begin
      Self.Picture := Picture; // this will use the SetPicture method
      Self.Transparent := Transparent;

      Self.OffsetX := OffsetX;
      Self.OffsetY := OffsetY;
      Self.VertAlignment := VertAlignment;
      Self.HorzAlignment := HorzAlignment;
    end;
  end else
    inherited Assign(Source); // raises exception
end;

{ ============================================================================
  TBcBarPicture.CalcPicturePosition
  Description: Calculates the position of the picture where is should be drawn.
    Uses the owner menus PopupHeight property.
  ---------------------------------------------------------------------------- }
procedure TBcBarPicture.CalcPicturePosition(var X, Y: Integer);
var
  PopupHeight, BarWidth: Integer;
begin
  Y := 0;
  X := 0;
  if not Assigned(Picture) then
     Exit;

  PopupHeight := GetMenuBarMenusIntf(Owner).BarProtected.PopupHeight;
  BarWidth := GetMenuBarMenusIntf(Owner).Bar.Width;
  case VertAlignment of
    vaTop:    Y := OffsetY;
    vaBottom: Y :=
      PopupHeight + OffsetY - Picture.Height;
    vaMiddle: Y :=
      ((PopupHeight - Picture.Height) div 2) + OffsetY;
  end;

  case HorzAlignment of
    haLeft:   X := OffsetX;
    haRight:  X :=
      BarWidth + OffsetX - Picture.Width;
    haCenter: X :=
      ((BarWidth - Picture.Width) div 2) +
      OffsetX;
  end;
end;

{ ============================================================================
  TBcBarPicture
  Set methods
  ---------------------------------------------------------------------------- }
procedure TBcBarPicture.SetTransparent(Value: Boolean);
begin
  FTransparent := Value;
  if Assigned(FPicture)
{$IFNDEF PICTUREASTBITMAP}and Assigned(FPicture.Graphic){$ENDIF} then
  begin
    FPicture{$IFNDEF PICTUREASTBITMAP}.Bitmap{$ENDIF}.Transparent := Value;
    FPicture{$IFNDEF PICTUREASTBITMAP}.Bitmap{$ENDIF}.TransparentMode := tmAuto;
  end;
end;

procedure TBcBarPicture.SetPicture(Value: TBcPicture);
begin
  { If the new value is nil or empty then free the internal instance of
    TBcPicture to save resources. }
  if Assigned(Value) then
  begin
  { Important that we use the "Picture" not "FPicture". With Picture the
    property GetPicture method creates the FPicture automatically. }
    Picture.Assign(Value)
  end else
  begin
{$IFNDEF PICTUREASTBITMAP}
    if Assigned(FPicture) and Assigned(FPicture.Graphic) then
      FPicture.Graphic := nil;
{$ENDIF}
    FreeAndNil(FPicture);
  end;
end;

{ ============================================================================
  TBcBarPicture
  Get methods
  ---------------------------------------------------------------------------- }
function TBcBarPicture.GetPicture: TBcPicture;
begin
  if not Assigned(FPicture) then
  begin
    FPicture := TBcPicture.Create;
{$IFNDEF PICTUREASTBITMAP}
    if FPicture.Graphic is TBitmap then
    begin
      FPicture.Bitmap.Transparent := FTransparent;
      FPicture.Bitmap.TransparentMode := tmAuto;
    end;
{$ELSE}
    FPicture.Transparent := FTransparent;
    FPicture.TransparentMode := tmAuto;
{$ENDIF}
  end;
  Result := FPicture;
end;

function TBcBarPicture.GetTransparent: Boolean;
begin
//  if Assigned(FPicture)
//{$IFNDEF PICTUREASTBITMAP}and Assigned(FPicture.Graphics){$ENDIF}then
//    Result := FPicture{$IFNDEF PICTUREASTBITMAP}.Graphic{$ENDIF}.Transparent
//  else
    Result := FTransparent;
end;

{ TBcBarBackPicture }

{ ============================================================================
  TBcBarBackPicture.Create
  TBcBarBackPicture.Destroy
  ---------------------------------------------------------------------------- }
constructor TBcBarBackPicture.Create(AOwner: TMenu);
begin
  inherited Create(AOwner);

  FVertAlignment := DefaultBarBackBitmapVertAlignment;
  FHorzAlignment := DefaultBarBackBitmapHorzAlignment;
end;

destructor TBcBarBackPicture.Destroy;
begin
  inherited Destroy;
end;

{ TBcBarCaption }

{ ============================================================================
  TBcBarCaption.Create
  TBcBarCaption.Destroy
  ---------------------------------------------------------------------------- }
constructor TBcBarCaption.Create(AOwner: TMenu);
begin
  inherited Create;
  FOwner := AOwner;

  FVertAlignment := DefaultBarCaptionVertAlignment;
  FOffsetY := DefaultBarCaptionOffsetY;
  FDirection := DefaultBarCaptionDirection;
  FAutoSize := DefaultBarCaptionAutoSize;
  FShadowColor := DefaultBarCaptionShadowColor;
  FHighlightColor := DefaultBarCaptionHighlightColor;
  FDepth := DefaultBarCaptionDepth;
  FVisible := DefaultBarCaptionVisible;

  FFont := TFont.Create;
  with FFont do
  begin
{$IFDEF MSWINDOWS}
    Name := 'Tahoma';
    Size := 14;
{$ENDIF}
{$IFDEF LINUX}
    Name := 'Helvetica';
    Size := 14;
{$ENDIF}
    Color := clWhite;
    Style := [fsBold, fsItalic];
  end;

//  if (Application.Handle <> 0) then
//    FCaption := Application.Title; { some defaults }
  FCaption := ''; { TODO -cFix -oJouni Airaksinen : solve how to make this as the owner component name }
end;

destructor TBcBarCaption.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TBcBarCaption.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

{ ============================================================================
  TBcBarCaption.Assign
  ---------------------------------------------------------------------------- }
procedure TBcBarCaption.Assign(Source: TPersistent);
begin
  if Source is TBcBarCaption then
  begin
    with TBcBarCaption(Source) do
    begin
      Self.Font.Assign(Font);
      Self.Caption := Caption;
      Self.OffsetY := OffsetY;
      Self.Alignment := Alignment;
      Self.Depth := Depth;
      Self.ShadowColor := ShadowColor;
      Self.HighlightColor := HighlightColor;
      Self.Direction := Direction;
    end;
  end else
    inherited Assign(Source); // raises exception
end;

{ TBcCustomPopupMenu }

{ ============================================================================
  TBcCustomPopupMenu.Create
  TBcCustomPopupMenu.Destroy
  ---------------------------------------------------------------------------- }
constructor TBcCustomBarPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OwnerDraw := True; // required!!

  FFlat := DefaultFlat;
  FOpacity := DefaultOpacity;
  FUseSystemFont := DefaultUseSystemFont;
  FMenuStyle := DefaultMenuStyle;
  FUpdateRef := 0;
  FMenuUpdating := False;

  FBarProtected := TBcBarProtected.Create(Self);
  FBar := TBcBar.Create(Self);
  FSeparators := TBcSeparators.Create(self); // 2001-07-29: mf

  FMenuFont := TFont.Create;
end;

destructor TBcCustomBarPopupMenu.Destroy;
begin
  FreeAndNil(FBarProtected);
  FreeAndNil(FBar);
  FreeAndNil(FSeparators); // 2001-07-29: mf
  FreeAndNil(FMenuFont);
  inherited Destroy;
end;

procedure TBcCustomBarPopupMenu.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Assigned(FDrawModule) and (AComponent = FDrawModule) and
    (Operation = opRemove) then
    FDrawModule := nil;
  inherited;
end;

function TBcCustomBarPopupMenu._AddRef: Integer;
begin
  Result := 1;
end;

function TBcCustomBarPopupMenu._Release: Integer;
begin
  Result := 1;
end;

{ ============================================================================
  TBcCustomBarPopupMenu
  Set methods
  ---------------------------------------------------------------------------- }
procedure TBcCustomBarPopupMenu.SetFlat(const Value: Boolean);
begin
  FFlat := Value;
end;

procedure TBcCustomBarPopupMenu.SetOpacity(const Value: TOpacity);
begin
  FOpacity := Value;
end;

procedure TBcCustomBarPopupMenu.SetBar(const Value: TBcBar);
begin
  FBar.Assign(Value);
end;

procedure TBcCustomBarMainMenu.SetUseSystemFont(const Value: Boolean);
begin
  FUseSystemFont := Value;
  RefreshMenu(False, FUseSystemFont);
end;

procedure TBcCustomBarPopupMenu.SetSeparators(const Value: TBcSeparators); // 2001-07-29: mf
begin
  FSeparators.Assign(Value);
end;

procedure TBcCustomBarPopupMenu.SetOnBeforeDrawBar(const Value: TOnBeforeDrawBarEvent);
begin
  FOnBeforeDrawBar := Value;
end;

procedure TBcCustomBarPopupMenu.SetOnAfterDrawBar(const Value: TOnAfterDrawBarEvent);
begin
  FOnAfterDrawBar := Value;
end;

procedure TBcCustomBarPopupMenu.SetOnAdvancedBeforeDrawBar(
  const Value: TOnAdvancedBeforeDrawBarEvent);
begin
  FOnAdvancedBeforeDrawBar := Value;
end;

procedure TBcCustomBarPopupMenu.SetMenuFont(const Value: TFont);
begin
  FMenuFont.Assign(Value);
end;

procedure TBcCustomBarPopupMenu.SetUseSystemFont(const Value: Boolean);
begin
  FUseSystemFont := Value;
end;

procedure TBcCustomBarPopupMenu.SetMenuWindowHandle(Handle: THandle);
begin
  FMenuWindowHandle := Handle;
end;

procedure TBcCustomBarPopupMenu.SetDrawModule(
  const Value: TBcBarMenusDrawModule);
begin
  if Assigned(FDrawModule) then
    FDrawModule.DrawingMenu := nil;
  FDrawModule := Value;
  if Assigned(FDrawModule) then
    FDrawModule.DrawingMenu := Self;
  FlushDoubleBuffer;
end;

procedure TBcCustomBarPopupMenu.SetOnMeasureMenuItem(
  const Value: TOnMeasureMenuItemEvent);
begin
  FOnMeasureMenuItem := Value;
end;

procedure TBcCustomBarPopupMenu.SetMenuStyle(const Value: TBcMenuStyle);
begin
  FMenuStyle := Value;
end;

function TBcCustomBarPopupMenu.UseMenuStyle: TBcMenuStyle;
begin
  Result := GetMenuStyle;
  if Result = msAuto then
  begin
    if WinXP then
      Result := msWindowsXP
    else
      Result := msStandard;
  end;
end;

{ ============================================================================
  TBcCustomBarPopupMenu
  Get methods
  ---------------------------------------------------------------------------- }
function TBcCustomBarPopupMenu.GetSelf: TMenu;
begin
  Result := Self;
end;

function TBcCustomBarPopupMenu.GetFlat: Boolean;
begin
  Result := FFlat;
end;

function TBcCustomBarPopupMenu.GetOpacity: TOpacity;
begin
  Result := FOpacity;
end;

function TBcCustomBarPopupMenu.GetBar: TBcBar;
begin
  Result := FBar;
end;

function TBcCustomBarPopupMenu.GetSeparators: TBcSeparators; // 2001-07-29: mf
begin
 Result := FSeparators;
end;

function TBcCustomBarPopupMenu.GetBarProtected: TBcBarProtected;
begin
  Result := FBarProtected;
end;

function TBcCustomBarPopupMenu.GetOnBeforeDrawBar: TOnBeforeDrawBarEvent;
begin
  Result := FOnBeforeDrawBar;
end;

function TBcCustomBarPopupMenu.GetOnAfterDrawBar: TOnAfterDrawBarEvent;
begin
  Result := FOnAfterDrawBar;
end;

function TBcCustomBarPopupMenu.GetOnAdvancedBeforeDrawBar: TOnAdvancedBeforeDrawBarEvent;
begin
  Result := FOnAdvancedBeforeDrawBar;
end;

function TBcCustomBarPopupMenu.GetMenuFont: TFont;
begin
  Result := FMenuFont;
end;

function TBcCustomBarPopupMenu.GetUseSystemFont: Boolean;
begin
  Result := FUseSystemFont;
end;

function TBcCustomBarPopupMenu.GetDrawModule: TBcBarMenusDrawModule;
begin
  Result := FDrawModule;
end;

function TBcCustomBarPopupMenu.GetOnMeasureMenuItem: TOnMeasureMenuItemEvent;
begin
  Result := FOnMeasureMenuItem;
end;

function TBcCustomBarPopupMenu.GetMenuStyle: TBcMenuStyle;
begin
  Result := FMenuStyle;
end;

{ ============================================================================
  TBcCustomBarPopupMenu.Popup
  TBcCustomBarPopupMenu.PopupAtCursor
  Description: Overloaded method for P: TPoint parameter. Good for e.g.
    MyPopup.Popup(Mouse.CursorPos); or use the PopupAtCursor.
  ---------------------------------------------------------------------------- }
procedure TBcCustomBarPopupMenu.Popup(X, Y: Integer);
begin
  BarProtected.UpdatePopupHeight := True;
  inherited Popup(X, Y);
end;

procedure TBcCustomBarPopupMenu.Popup(P: TPoint);
begin
  Popup(P.X, P.Y);
end;

procedure TBcCustomBarPopupMenu.PopupAtCursor;
begin
  Popup(Mouse.CursorPos);
end;

{ ============================================================================
  TBcCustomBarPopupMenu.ExpandItemWidth
  TBcCustomBarPopupMenu.AdvancedDrawWidth
  Description: Events for menuitems
  ---------------------------------------------------------------------------- }
procedure TBcCustomBarPopupMenu.ExpandItemWidth(Sender: TObject;
  ACanvas: TCanvas; var Width, Height: Integer);
var MI: TMenuItem;
begin
  { bar only visible if FBarVisible is True and menuitem is
    in the root menu. }
  MI := TMenuItem(Sender);
  BarMeasureItem(MI, ACanvas, Width, Height,
    Bar.Visible and not Assigned(MI.Parent.Parent));
end;

procedure TBcCustomBarPopupMenu.AdvancedDrawItem(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
var MI: TMenuItem;
begin
  { bar only visible if FBarVisible is True and menuitem is
    in the root menu. }
  MI := TMenuItem(Sender);
  BarDrawMenuItem(MI, ACanvas, ARect, State,
    Bar.Visible and not Assigned(MI.Parent.Parent));
end;

{ ============================================================================
  TBcCustomBarPopupMenu.MenuChanged
  Description: Monitors changes in the menu and makes any post changes.
  ---------------------------------------------------------------------------- }
procedure TBcCustomBarPopupMenu.MenuChanged(Sender: TObject;
  Source: TMenuItem; Rebuild: Boolean);
begin
{$IFNDEF DESIGNTIMEPREVIEW}
  if not (csDesigning in ComponentState) then
{$ENDIF}
  begin
    if not MenuUpdating then
      SetMenuItemEvents(Self, Source, ExpandItemWidth, AdvancedDrawItem, True);

    { 2.0.1 !! we need to update doublebuffer everytime something changes in the
      root menu (e.g. Visible property of item), not just when items are
      added/removed.  }

//    if not Assigned(Source) or not Assigned(Source.Parent.Parent) {and Rebuild} then
    if Assigned(Source) and IsInTopPopupMenu(Source) then
    begin // changes in the menu which contains the bar
      BarProtected.UpdatePopupHeight := True;
      BarProtected.Drawn := False;
    end;
  end;
  inherited MenuChanged(Sender, Source, Rebuild);
end;

procedure TBcCustomBarPopupMenu.RefreshMenu(const BarChanged, WidthChanged: Boolean);
begin
  BarMenus.RefreshMenu(Self, BarChanged, WidthChanged);
end;

procedure TBcCustomBarPopupMenu.Loaded;
begin
  inherited Loaded;
  OwnerDraw := True;
  SetMenuItemEvents(Self, nil, ExpandItemWidth, AdvancedDrawItem, True);
end;

{ ============================================================================
  TBcCustomBarPopupMenu.FlushDoubleBuffer
  Description: Frees DoubleBuffer TBitmap resources.
  ---------------------------------------------------------------------------- }
procedure TBcCustomBarPopupMenu.FlushDoubleBuffer;
begin
  BarProtected.FreeBarDoubleBuffer;
end;


procedure TBcCustomBarPopupMenu.BeginUpdate;
begin
  FMenuUpdating := True;
  Inc(FUpdateRef);
end;

procedure TBcCustomBarPopupMenu.EndUpdate;
begin
  if FUpdateRef > 0 then
    Dec(FUpdateRef);

  if FUpdateRef = 0 then
  begin
    SetMenuItemEvents(Self, nil, ExpandItemWidth, AdvancedDrawItem, True);
    FMenuUpdating := False;
  end;
end;

{ TBcCustomMainMenu }

{ ============================================================================
  TBcCustomMainMenu.Create
  TBcCustomMainMenu.Destroy
  ---------------------------------------------------------------------------- }
constructor TBcCustomBarMainMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OwnerDraw := True; // required!!

  FOpacity := DefaultOpacity;
  FFlat := DefaultFlat;
  FUseSystemFont := DefaultUseSystemFont;
  FMenuStyle := DefaultMenuStyle;
  FUpdateRef := 0;
  FMenuUpdating := False;

  FBarProtected := TBcBarProtected.Create(Self);
  FBar := TBcBar.Create(Self);
  FSeparators := TBcSeparators.Create(self); // 2001-07-29: mf

  FMenuFont := TFont.Create;
  FMenuFont.OnChange := MenuFontChanged;
end;

destructor TBcCustomBarMainMenu.Destroy;
begin
  FreeAndNil(FBarProtected);
  FreeAndNil(FBar);
  FreeAndNil(FSeparators); // 2001-07-29: mf
  FreeAndNil(FMenuFont);
  inherited Destroy;
end;

procedure TBcCustomBarMainMenu.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Assigned(FDrawModule) and (AComponent = FDrawModule) and
    (Operation = opRemove) then
    FDrawModule := nil;
  inherited;
end;

function TBcCustomBarMainMenu._AddRef: Integer;
begin
  Result := 1;
end;

function TBcCustomBarMainMenu._Release: Integer;
begin
  Result := 1;
end;

function TBcCustomBarMainMenu.UseMenuStyle: TBcMenuStyle;
begin
  Result := GetMenuStyle;
  if Result = msAuto then
  begin
    if WinXP then
      Result := msWindowsXP
    else
      Result := msStandard;
  end;
end;

{ ============================================================================
  TBcCustomBarMainMenu
  Set methods
  ---------------------------------------------------------------------------- }
procedure TBcCustomBarMainMenu.SetFlat(const Value: Boolean);
begin
  FFlat := Value;
end;

procedure TBcCustomBarMainMenu.SetOpacity(const Value: TOpacity);
begin
  FOpacity := Value;
end;

procedure TBcCustomBarMainMenu.SetBar(const Value: TBcBar);
begin
  FBar.Assign(Value);
end;

procedure TBcCustomBarMainMenu.SetSeparators(const Value: TBcSeparators); // 2001-07-29: mf
begin
  FSeparators.Assign(Value);
end;

procedure TBcCustomBarMainMenu.SetOnBeforeDrawBar(const Value: TOnBeforeDrawBarEvent);
begin
  FOnBeforeDrawBar := Value;
end;

procedure TBcCustomBarMainMenu.SetOnAfterDrawBar(const Value: TOnAfterDrawBarEvent);
begin
  FOnAfterDrawBar := Value;
end;

procedure TBcCustomBarMainMenu.SetOnAdvancedBeforeDrawBar(
  const Value: TOnAdvancedBeforeDrawBarEvent);
begin
  FOnAdvancedBeforeDrawBar := Value;
end;

procedure TBcCustomBarMainMenu.SetDrawModule(
  const Value: TBcBarMenusDrawModule);
begin
  if Assigned(FDrawModule) then
    FDrawModule.DrawingMenu := nil;
  FDrawModule := Value;
  if Assigned(FDrawModule) then
    FDrawModule.DrawingMenu := Self;

  SetMenuItemEvents(Self, nil, ExpandItemWidth, AdvancedDrawItem, True);

  if Items.Count > 0 then
  begin
    Items[0].Visible := False;
    Items[0].Visible := True;
  end;
  RefreshMenu(True, True);
end;

procedure TBcCustomBarMainMenu.SetMenuFont(const Value: TFont);
begin
  FMenuFont.Assign(Value);
end;

procedure TBcCustomBarMainMenu.SetMenuWindowHandle(Handle: THandle);
begin
  { no support, just to make the interface happy }
end;

procedure TBcCustomBarMainMenu.SetOnMeasureMenuItem(
  const Value: TOnMeasureMenuItemEvent);
begin
  FOnMeasureMenuItem := Value;
end;

procedure TBcCustomBarMainMenu.SetMenuStyle(const Value: TBcMenuStyle);
begin
  if FMenuStyle <> Value then
  begin
    FMenuStyle := Value;
    RefreshMenu(False, False); // Force redraw menubar
    RefreshMenu(True, True);  // Force remeasure
  end;
end;

{ ============================================================================
  TBcCustomBarMainMenu
  Get methods
  ---------------------------------------------------------------------------- }
function TBcCustomBarMainMenu.GetSelf: TMenu;
begin
  Result := Self;
end;

function TBcCustomBarMainMenu.GetFlat: Boolean;
begin
  Result := FFlat;
end;

function TBcCustomBarMainMenu.GetMenuFont: TFont;
begin
  Result := FMenuFont;
end;

function TBcCustomBarMainMenu.GetUseSystemFont: Boolean;
begin
  Result := FUseSystemFont;
end;

function TBcCustomBarMainMenu.GetOpacity: TOpacity;
begin
  Result := FOpacity;
end;

function TBcCustomBarMainMenu.GetBar: TBcBar;
begin
  Result := FBar;
end;

function TBcCustomBarMainMenu.GetSeparators: TBcSeparators; // 2001-07-29: mf
begin
 Result := FSeparators;
end;

function TBcCustomBarMainMenu.GetBarProtected: TBcBarProtected;
begin
  Result := FBarProtected;
end;

function TBcCustomBarMainMenu.GetOnBeforeDrawBar: TOnBeforeDrawBarEvent;
begin
  Result := FOnBeforeDrawBar;
end;

function TBcCustomBarMainMenu.GetOnAfterDrawBar: TOnAfterDrawBarEvent;
begin
  Result := FOnAfterDrawBar;
end;

function TBcCustomBarMainMenu.GetOnAdvancedBeforeDrawBar: TOnAdvancedBeforeDrawBarEvent;
begin
  Result := FOnAdvancedBeforeDrawBar;
end;

function TBcCustomBarMainMenu.GetDrawModule: TBcBarMenusDrawModule;
begin
  Result := FDrawModule;
end;

function TBcCustomBarMainMenu.GetOnMeasureMenuItem: TOnMeasureMenuItemEvent;
begin
  Result := FOnMeasureMenuItem;
end;

function TBcCustomBarMainMenu.GetMenuStyle: TBcMenuStyle;
begin
  Result := FMenuStyle;
end;

{ ============================================================================
  TBcCustomBarMainMenu.ExpandItemWidth
  TBcCustomBarMainMenu.AdvancedDrawWidth
  Description: Events for menuitems
  ---------------------------------------------------------------------------- }
procedure TBcCustomBarMainMenu.ExpandItemWidth(Sender: TObject;
  ACanvas: TCanvas; var Width, Height: Integer);
var MI: TMenuItem;
begin
  { bar only visible if FBarVisible is True and menuitem is
    in the root menu. }
  MI := TMenuItem(Sender);
  BarMeasureItem(MI, ACanvas, Width, Height,
    Bar.Visible and IsInFirstVisibleMenu(Self, MI));
end;

procedure TBcCustomBarMainMenu.AdvancedDrawItem(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
var MI: TMenuItem;
begin
  { bar only visible if FBarVisible is True and menuitem is
    in the root menu. }
  MI := TMenuItem(Sender);
  BarDrawMenuItem(MI, ACanvas, ARect, State,
    Bar.Visible and IsInFirstVisibleMenu(Self, MI));
end;

{ ============================================================================
  TBcCustomBarMainMenu.MenuChanged
  Description: Monitors changes in the menu and makes any post changes.
  ---------------------------------------------------------------------------- }
procedure TBcCustomBarMainMenu.MenuChanged(Sender: TObject;
  Source: TMenuItem; Rebuild: Boolean);
begin
{$IFNDEF DESIGNTIMEPREVIEW}
  if not (csDesigning in ComponentState) then
{$ENDIF}
  begin
    if FMenuUpdating then
      SetMenuItemEvents(Self, Source, ExpandItemWidth, AdvancedDrawItem, True);
//    if not Assigned(Source) or IsInFirstVisibleMenu(Self, Source) then
//    begin
//      if Rebuild then
//      begin
//        BarProtected.UpdatePopupHeight := True;
//        BarProtected.Drawn := False;
//      end;
//    end;
  end;
  inherited MenuChanged(Sender, Source, Rebuild);
//  SetMenuItemEvents(Self, nil, ExpandItemWidth, AdvancedDrawItem, True);
end;

{ ============================================================================
  TBcCustomBarMainMenu.RefreshMenu
  Description: If menu is set mainmenu, it needs refreshing so the changes
    take effect. To make refreshing a bit faster, we do not use the
    MenuChanged method as it will update the events. Thus, this does not
    handle added or deleted menuitems as it is not necessary.
  ---------------------------------------------------------------------------- }
procedure TBcCustomBarMainMenu.RefreshMenu(const BarChanged, WidthChanged: Boolean);
begin
  BarMenus.RefreshMenu(Self, BarChanged, WidthChanged);
end;

{ ============================================================================
  TBcCustomBarMainMenu.Loaded
  TBcCustomBarMainMenu.MenuFontChanged
  Description: Refresh the menufont after components have been Loaded or
    MenuFont changed.
  ---------------------------------------------------------------------------- }
procedure TBcCustomBarMainMenu.Loaded;
begin
  inherited Loaded;
  OwnerDraw := True;
  SetMenuItemEvents(Self, nil, ExpandItemWidth, AdvancedDrawItem, True);

  { todo : why the measure events are not called without this "workaround" ?? }
  if Items.Count > 0 then
  begin
    Items[0].Visible := False;
    Items[0].Visible := True;
  end;

{$IFDEF DESIGNTIMEPREVIEW}
//  MenuChanged(Self, nil, False);
{$ENDIF}
  RefreshMenu(False, False);
end;

procedure TBcCustomBarMainMenu.MenuFontChanged(Sender: TObject);
begin
  RefreshMenu(False, False);
end;

{ ============================================================================
  TBcCustomBarMainMenu.FlushDoubleBuffer
  Description: Frees DoubleBuffer TBitmap resources.
  ---------------------------------------------------------------------------- }
procedure TBcCustomBarMainMenu.FlushDoubleBuffer;
begin
  BarProtected.FreeBarDoubleBuffer;
  UpdateItems; // TMainMenu does not call Measure events unless items are updated
end;

procedure TBcCustomBarMainMenu.BeginUpdate;
begin
  FMenuUpdating := True;
  Inc(FUpdateRef);
end;

procedure TBcCustomBarMainMenu.EndUpdate;
begin
  if FUpdateRef > 0 then
    Dec(FUpdateRef);

  if FUpdateRef = 0 then
  begin
    SetMenuItemEvents(Self, nil, ExpandItemWidth, AdvancedDrawItem, True);
    FMenuUpdating := False;
  end;
end;

initialization
  Win98Plus := (Win32MajorVersion > 4) or
    ((Win32MajorVersion = 4) and (Win32MinorVersion > 0));
  WinXP := (Win32Platform = VER_PLATFORM_WIN32_NT) and
    (Win32MajorVersion > 4) and (Win32MinorVersion > 0);
  Win2K := (Win32MajorVersion > 4) and (Win32Platform = VER_PLATFORM_WIN32_NT); // Note, this is true under XP

finalization


end.
