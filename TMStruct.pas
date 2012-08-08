unit TMStruct;

{
  Aestan Tray Menu
  Made by Onno Broekmans; visit http://www.xs4all.nl/~broekroo/aetraymenu
  for more information.

  This work is hereby released into the Public Domain. To view a copy of the
  public domain dedication, visit:
      http://creativecommons.org/licenses/publicdomain/
  or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford,
  California 94305, USA.

  This unit contains some classes that are used for saving the settings
  read from the configuration file.
}

{
  NOTE:
  Lots of code from this unit have been taken from the Inno Setup source code.
  The original copyright notice for this code is:
    Copyright (C) 1998-2002 Jordan Russell
    Portions by Martijn Laan
}

interface

uses Classes, Windows, Contnrs, TMSrvCtrl;

type
  TConfigSectionDirective = (
    csImageList,
    csServiceCheckInterval,
    csTrayIconAllRunning,
    csTrayIconSomeRunning,
    csTrayIconNoneRunning,
    csTrayIcon,
    csServiceGlyphRunning,
    csServiceGlyphPaused,
    csServiceGlyphStopped,
    csID,
    csAboutHeader,
    csAboutVersion,
    csAboutTextFile,
    csHtmlActions
    );

  TMessagesSectionDirective = (
    msAllRunningHint,
    msSomeRunningHint,
    msNoneRunningHint
    );

  TMenuSectionDirective = (
    mnAutoHotkeys,
    mnAutoLineReduction,
    mnBarBackPictureDrawStyle,
    mnBarBackPictureHorzAlignment,
    mnBarBackPictureOffsetX,
    mnBarBackPictureOffsetY,
    mnBarBackPicturePicture,
    mnBarBackPictureTransparent,
    mnBarBackPictureVertAlignment,
    mnBarCaptionAlignment,
    mnBarCaptionCaption,
    mnBarCaptionDepth,
    mnBarCaptionDirection,
    mnBarCaptionFont,
    mnBarCaptionHighlightColor,
    mnBarCaptionOffsetY,
    mnBarCaptionShadowColor,
    mnBarPictureHorzAlignment,
    mnBarPictureOffsetX,
    mnBarPictureOffsetY,
    mnBarPicturePicture,
    mnBarPictureTransparent,
    mnBarPictureVertAlignment,
    mnBarBorder,
    mnBarGradientEnd,
    mnBarGradientStart,
    mnBarGradientStyle,
    mnBarSide,
    mnBarSpace,
    mnBarVisible,
    mnBarWidth,
    mnMenuFont,
    mnSeparatorsAlignment,
    mnSeparatorsFade,
    mnSeparatorsFadeColor,
    mnSeparatorsFadeWidth,
    mnSeparatorsFlatLines,
    mnSeparatorsFont,
    mnSeparatorsGradientEnd,
    mnSeparatorsGradientStart,
    mnSeparatorsGradientStyle,
    mnSeparatorsSeparatorStyle
    );

  TShowCmd = (
    swNormal = SW_SHOWNORMAL,
    swHidden = SW_HIDE,
    swMaximized = SW_SHOWMAXIMIZED,
    swMinimized = SW_SHOWMINNOACTIVE
    );

  TMenuItemType = (
    mitItem,
    mitSeparator,
    mitSubMenu,
    mitServiceSubMenu
    );

  TVarType = (
    vtStatic,
    vtRegistry,
    vtEnvironment,
    vtCmdLine,
    vtPrompt
    );

  TVariableFlag = (   //see also VariableFlags
    vfIsPath
    );
  TVariableFlags = set of TVariableFlag;

  TActionFlag = (     //see also ActionFlags
    afIgnoreErrors,
    afWaitUntilTerminated,
    afWaitUntilIdle
    );
  TActionFlags = set of TActionFlag;

  THtmlWindowFlag = (     //see also HtmlWindowFlags
    hwfMaximized,
    hwfNoResize,
    hwfNoScrollbars,
    hwfEnableContextMenu,
    hwfNoCloseButton,
    hwfNoHeader,
    hwfAlwaysOnTop
    );
  THtmlWindowFlags = set of THtmlWindowFlag;

  TBuiltInAction = (
    biaAbout,
    biaExit,
    biaControlPanelServices,
    biaReadConfig,
    biaCloseServices,
    biaResetServices
    );

  TServiceAction = (
    saStartResume,
    saPause,
    saStop,
    saRestart
    );

//  TVariable = class
//  public
//    Name, Value: String;
//    Flags: TVariableFlags;
//  end;

  TVariableBase = class
  protected
    FName: String;
    FFlags: TVariableFlags;
    function GetValue: String; virtual; abstract;
    procedure SetValue(const AValue: String); virtual; abstract;
  public
    property Name: String read FName write FName;
    property Value: String read GetValue write SetValue;
    property Flags: TVariableFlags read FFlags write FFlags;
  end;

  TVariable = class(TVariableBase)
  protected
    FValue: String;
    function GetValue: String; override;
    procedure SetValue(const AValue: String); override;
  end;

  TPromptVariable = class(TVariableBase)
  private
    FText: String;
    FCaption: String;
    FDefaultValue: String;
  protected
    function GetValue: String; override;
    procedure SetValue(const AValue: String); override;
  public
    property Caption: String read FCaption write FCaption;
    property Text: String read FText write FText;
    property DefaultValue: String read FDefaultValue write FDefaultValue;
  end;

  TTMAction = class
  private
    FFlags: TActionFlags;
  public
    property Flags: TActionFlags read FFlags write FFlags;

    procedure ExecuteAction; virtual; abstract;
      //Performs the action
    function CanExecute: Boolean; virtual;
      //Returns if the corresponding menu item should be enabled;
      //the default implementation always returns true.
  end;  //TTMAction

  TTMBuiltInAction = class(TTMAction)
  private
    FOnExecute: TNotifyEvent;
    FBuiltInAction: TBuiltInAction;
  public
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
    property BuiltInAction: TBuiltInAction read FBuiltInAction write FBuiltInAction;

    constructor Create(ABuiltInAction: TBuiltInAction); virtual;

    procedure ExecuteAction; override;
  end;  //TTMBuiltInAction

  TTMRunAction = class(TTMAction)
  private
    FFileName: String;
    FWorkingDir: String;
    FParameters: String;
    FShowCmd: TShowCmd;
    FVariables: TObjectList;
  public
    property FileName: String read FFileName write FFileName;
    property WorkingDir: String read FWorkingDir write FWorkingDir;
    property Parameters: String read FParameters write FParameters;
    property ShowCmd: TShowCmd read FShowCmd write FShowCmd;

    property Variables: TObjectList read FVariables write FVariables;

    constructor Create; virtual;

    procedure ExecuteAction; override;
  end;  //TTMRunAction

  TTMShellExecuteAction = class(TTMAction)
  private
    FFileName: String;
    FWorkingDir: String;
    FParameters: String;
    FShowCmd: TShowCmd;
    FVerb: String;
    FVariables: TObjectList;
  public
    property FileName: String read FFileName write FFileName;
    property WorkingDir: String read FWorkingDir write FWorkingDir;
    property Parameters: String read FParameters write FParameters;
    property ShowCmd: TShowCmd read FShowCmd write FShowCmd;
    property Verb: String read FVerb write FVerb;

    property Variables: TObjectList read FVariables write FVariables;

    constructor Create; virtual;

    procedure ExecuteAction; override;
  end;  //TTMShellExecuteAction

  TTMServiceAction = class(TTMAction)
  private
    FService: TTMService;
    FAction: TServiceAction;
  public
    property Service: TTMService read FService write FService;
    property Action: TServiceAction read FAction write FAction;

    procedure ExecuteAction; override;
    function CanExecute: Boolean; override;
  end;  //TTMServiceAction

  TTMMultiAction = class(TTMAction)
  private
    FItems: TObjectList;
    function GetItems(Index: Integer): TTMAction;
    function GetCount: Integer;
  public
    property Items[Index: Integer]: TTMAction read GetItems; default;
    property Count: Integer read GetCount;

    procedure ExecuteAction; override;

    constructor Create; virtual;
    destructor Destroy; override;

    function Add(AAction: TTMAction): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; AAction: TTMAction);
    procedure Move(CurIndex, NewIndex: Integer);
  end;  //TTMMultiAction

  TTMHtmlWindowAction = class(TTMAction)
  private
    FSrc: String;
    FHeader: String;
    FVariables: TObjectList;
    FHtmlActions: TStringList;
    FHtmlWindowFlags: THtmlWindowFlags;
    FHeight: Integer;
    FWidth: Integer;
    FLeft: Integer;
    FTop: Integer;
  public
    property Header: String read FHeader write FHeader;
    property Src: String read FSrc write FSrc;
    property HtmlWindowFlags: THtmlWindowFlags read FHtmlWindowFlags write FHtmlWindowFlags;
    property Height: Integer read FHeight write FHeight;
    property Width: Integer read FWidth write FWidth;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;

    property Variables: TObjectList read FVariables write FVariables;
    property HtmlActions: TStringList read FHtmlActions write FHtmlActions;

    procedure ExecuteAction; override;
  end;  //TTmHtmlWindowAction

const
  { Param name constants }
  ParamCommonFlags = 'Flags';
  ParamServicesName = 'Name';
  ParamServicesDisplayName = 'DisplayName';
  ParamVariablesName = 'Name';
  ParamVariablesType = 'Type';
  ParamVariablesFlags = 'Flags';
  ParamVariablesValue = 'Value';
  ParamVariablesRegRoot = 'Root';
  ParamVariablesRegKey = 'Key';
  ParamVariablesRegValue = 'ValueName';
  ParamVariablesEnvName = 'EnvName';
  ParamVariablesCmdLineParamName = 'ParamName';
  ParamVariablesDefaultValue = 'DefaultValue';
  ParamVariablesPromptCaption = 'PromptCaption';
  ParamVariablesPromptText = 'PromptText';
  ParamMenuItemType = 'Type';
  ParamMenuItemCaption = 'Caption';
  ParamMenuItemGlyph = 'Glyph';
  ParamMenuItemSection = 'SubMenu';
  ParamActionAction = 'Action';
  ParamActionFileName = 'FileName';
  ParamActionParams = 'Parameters';
  ParamActionWorkingDir = 'WorkingDir';
  ParamActionShowCmd = 'ShowCmd';
  ParamActionShellExecVerb = 'Verb';
  ParamActionMultiSection = 'Actions';
  ParamActionFlags = 'Flags';
  ParamServiceService = 'Service';
  ParamActionServiceAction = 'ServiceAction';
  ParamActionHtmlSrc = 'Src';
  ParamActionHtmlHeader = 'Header';
  ParamActionHtmlWindowFlags = 'HtmlWindowFlags';
  ParamActionHtmlHeight = 'Height';
  ParamActionHtmlWidth = 'Width';
  ParamActionHtmlLeft = 'Left';
  ParamActionHtmlTop = 'Top';

  { Flag name constants }
  VariableFlags: array[0..0] of PChar = (   //see also TVariableFlags
    'ispath');
  ActionFlags: array[0..2] of PChar = (     //see also TActionFlags
    'ignoreerrors', 'waituntilterminated', 'waituntilidle');
  HtmlWindowFlags: array[0..6] of PChar = (     //see also THtmlWindowFlags
    'maximized', 'noresize', 'noscrollbars', 'enablecontextmenu',
    'noclosebutton', 'noheader', 'alwaysontop');

implementation

uses SysUtils, Forms,
     TMCmnFunc, Dialogs, TMMsgs, TMHtmlWindow;

{ TTMAction }

function TTMAction.CanExecute: Boolean;
begin
  Result := True;
end;

{ TTMBuiltInAction }

constructor TTMBuiltInAction.Create(ABuiltInAction: TBuiltInAction);
begin
  BuiltInAction := ABuiltInAction;
end;

procedure TTMBuiltInAction.ExecuteAction;
begin
  inherited;

  try
    if Assigned(OnExecute) then
      OnExecute(Self);
  except
    on E: Exception do
      if not (afIgnoreErrors in Flags) then
        raise Exception.Create('Could not perform built-in action:'#13#10 + E.Message);
  end;  //try..except
end;

{ TTMRunAction }

constructor TTMRunAction.Create;
begin
  ShowCmd := swNormal;
end;

procedure ProcessMessagesProc; far;
begin
  Application.ProcessMessages;
end;

procedure TTMRunAction.ExecuteAction;
var
  ResultCode: Integer;
begin
  inherited;

  if not InstExec(ExpandVariables(FileName, Variables),
                  ExpandVariables(Parameters, Variables),
                  ExpandVariables(WorkingDir, Variables),
                  afWaitUntilTerminated in Flags,
                  afWaitUntilIdle in Flags,
                  Ord(ShowCmd),
                  ProcessMessagesProc,
                  ResultCode) then
    if not (afIgnoreErrors in Flags) then
      raise Exception.Create('Could not execute run action:'#13#10 + SysErrorMessage(ResultCode));
    { TODO 0 -cError handling : Improve TTMRunAction error handling (added ignoreerrors flag; need further improvement?}
end;

{ TTMShellExecuteAction }

constructor TTMShellExecuteAction.Create;
begin
  ShowCmd := swNormal;
end;

procedure TTMShellExecuteAction.ExecuteAction;
var
  ErrorCode: Integer;
begin
  inherited;

  if not InstShellExec(ExpandVariables(FileName, Variables),
                       ExpandVariables(Parameters, Variables),
                       Verb,
                       ExpandVariables(WorkingDir, Variables),
                       Ord(ShowCmd), ErrorCode) then
    if not (afIgnoreErrors in Flags) then
      raise Exception.Create('Could not execute shellexecute action:'#13#10 + SysErrorMessage(ErrorCode));
    { TODO 0 -cError handling : Improve TTMShellExecuteAction error handling (added ignoreerrors flag; need further improvement? }
end;

{ TTMServiceAction }

function TTMServiceAction.CanExecute: Boolean;
begin
  Result := True;
  with Service do
    if not Active then
      Result := False
    else
      case Action of
        saStartResume:
            Result := (State <> svsRunning);
        saPause:
            Result := (svcPauseContinue in ControlsAccepted) and (State <> svsPaused);
        saStop:
            Result := (svcStop in ControlsAccepted) and (State <> svsStopped);
        saRestart:
            Result := (svcStop in ControlsAccepted) and (State <> svsStopped);
      end;  //else not active then with service do case action of
end;

procedure TTMServiceAction.ExecuteAction;
begin
  inherited;

  { TODO 0 : Improve TTMServiceAction error handling  (added ignoreerrors flag; need further improvement?}
  try
    with Service do
    begin
      if not Active then
        Exit;

      case Action of
        saStartResume:
            if State = svsPaused then
            begin
              Continue;
              if afWaitUntilTerminated in Flags then
                while State = svsContinuePending do
                begin
                  Application.ProcessMessages;
                  if Application.Terminated then
                    Exit;
                end;
            end
            else
            begin
              Start;
              if afWaitUntilTerminated in Flags then
                while State = svsStartPending do
                begin
                  Application.ProcessMessages;
                  if Application.Terminated then
                    Exit;
                end;
            end;
        saPause: begin
            Pause;
            if afWaitUntilTerminated in Flags then
              while State = svsPausePending do
              begin
                Application.ProcessMessages;
                if Application.Terminated then
                  Exit;
              end;
            end;
        saStop: begin
            Stop;
            if afWaitUntilTerminated in Flags then
              while State = svsStopPending do
              begin
                Application.ProcessMessages;
                if Application.Terminated then
                  Exit;
              end;
            end;
        saRestart: begin
            Stop;
            while State = svsStopPending do
            begin
              Application.ProcessMessages;
              if Application.Terminated then
                Exit;
            end;
            Start;
          end;
      end;  //case
    end;  //with service do
  except
    on E: Exception do
      if not (afIgnoreErrors in Flags) then
        raise Exception.Create('Could not perform service action:'#13#10 + E.Message);
  end;  //try..except
end;

{ TTMMultiAction }

function TTMMultiAction.Add(AAction: TTMAction): Integer;
begin
  Result := FItems.Add(AAction);
end;

procedure TTMMultiAction.Clear;
begin
  FItems.Clear;
end;

constructor TTMMultiAction.Create;
begin
  FItems := TObjectList.Create(True);
end;

procedure TTMMultiAction.Delete(Index: Integer);
begin
  FItems.Delete(Index);
end;

destructor TTMMultiAction.Destroy;
begin
  FreeAndNil(FItems);
end;

procedure TTMMultiAction.ExecuteAction;
var
  I: Integer;
begin
  inherited;

  for I := 0 to FItems.Count - 1 do
    (Items[I] as TTMAction).ExecuteAction;
end;

function TTMMultiAction.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TTMMultiAction.GetItems(Index: Integer): TTMAction;
begin
  Result := (FItems[Index] as TTMAction);
end;

procedure TTMMultiAction.Insert(Index: Integer; AAction: TTMAction);
begin
  FItems.Insert(Index, AAction);
end;

procedure TTMMultiAction.Move(CurIndex, NewIndex: Integer);
begin
  FItems.Move(CurIndex, NewIndex);
end;

{ TVariable }

function TVariable.GetValue: String;
begin
  Result := FValue;
end;

procedure TVariable.SetValue(const AValue: String);
begin
  inherited;

  FValue := AValue;
end;

{ TPromptVariable }

function TPromptVariable.GetValue: String;
begin
  Result := DefaultValue;
  if not InputQuery(Caption, Text, Result) then
    Abort;
end;

procedure TPromptVariable.SetValue(const AValue: String);
begin
  inherited;

  raise EInvalidOperation.Create(SCannotSetPromptVarValue);
end;

{ TTMHtmlWindowAction }

procedure TTMHtmlWindowAction.ExecuteAction;
begin
  inherited;

  if Assigned(HtmlWindow) then
    FreeAndNil(HtmlWindow);
  HtmlWindow := THtmlWindow.Create(Application);
  with HtmlWindow do
  begin
    HtmlWindowFlags := Self.HtmlWindowFlags;
    Src := ExpandVariables(Self.Src, Variables);
    Header := ExpandVariables(Self.Header, Variables);
    HtmlActions := Self.HtmlActions;
    SetPosAndDimensions(Self.Left, Self.Top, Self.Width, Self.Height);
    Show;
  end;  //with htmlwindow
end;

end.
