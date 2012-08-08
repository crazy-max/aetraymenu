unit TMConfig;

{
  Aestan Tray Menu
  Made by Onno Broekmans; visit http://www.xs4all.nl/~broekroo/aetraymenu
  for more information.

  This work is hereby released into the Public Domain. To view a copy of the
  public domain dedication, visit:
      http://creativecommons.org/licenses/publicdomain/
  or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford,
  California 94305, USA.

  This unit contains the code that handles reading, parsing and handling
  the AeTrayMenu configuration file.
}

{
  NOTE:
  Lots of code from this unit are based on the Inno Setup source code,
  which was written by Jordan Russell (portions by Martijn Laan).
  Inno Setup is a great, free installer for Windows; see:
    http://www.jrsoftware.org
  >>PLEASE DO NOT REMOVE THIS NOTE<<
}

interface

uses Windows, SysUtils, Consts, Classes, Contnrs, JvComponent, JvTrayIcon,
     ImgList, BarMenus, Controls, Menus, ExtCtrls, Graphics,
     TMStruct, TMCmnFunc, TMSrvCtrl;

const
  MENUSETTINGSSUFFIX = '.Settings';
      //suffix used in TTMConfigReader.ReadBcBarPopupMenu

type
  EParseError = class(Exception)
      { This exception class is raised when the TTMConfigReader class
        can't read the script because of syntax errors in the script }
  public
    LineNumber: Integer;
      { The line number on which the parse error occurred }
      { (LineNumber is -1 if it is some kind of 'general' error) }

    constructor CreateLine(const Msg: String; const LineNumber: Integer = -1);
    constructor CreateLineFmt(const Msg: String; Args: array of const;
      const LineNumber: Integer = -1);
  end;

  TEnumIniSectionProc = procedure(const Line: PChar; const Ext: Longint) of object;

  TTMConfigReader = class
  private
    FTrayIcon: TJvTrayIcon;
    FImageList: TImageList;
    FServices: TObjectList;
    FCheckServicesTimer: TTimer;
    FTrayIconSomeRunning: Integer;
    FTrayIconNoneRunning: Integer;
    FTrayIconAllRunning: Integer;
    FSomeRunningHint: String;
    FNoneRunningHint: String;
    FAllRunningHint: String;
    FVariables: TObjectList;
    FServiceGlyphRunning: Integer;
    FServiceGlyphStopped: Integer;
    FServiceGlyphPaused: Integer;
    FOnSelectMenuItem: TNotifyEvent;
    FDoubleClickAction, FLeftClickAction, FRightClickAction: TTMMultiAction;
    FStartupAction: TTMMultiAction;
    FScript: TStringList;
    FOnBuiltInActionExecute: TNotifyEvent;
    FID: String;
    FCustomAboutVersion: String;
    FCustomAboutHeader: String;
    FCustomAboutText: TStrings;
    FHtmlActions: TStringList;
    procedure SetScript(const Value: TStringList);
    procedure SetCustomAboutText(const Value: TStrings);
  protected
    { FIELDS }
    { Miscellaneous internal data }
    LineNumber: Integer;
      { The line number of the line the parser is currently working on }
      { This value is used in combination with the AbortParsing(..) procs }
    ConfigDirectiveLines: array[TConfigSectionDirective] of Integer;
    MessagesDirectiveLines: array[TMessagesSectionDirective] of Integer;
    MenuDirectiveLines: array[TMenuSectionDirective] of Integer;

    { METHODS }
    { Exception methods }
    procedure AbortParsing(const Msg: String);
      { Raises an EScriptParseError exception; used while reading
        the script }
      { The line number is automatically read from "LineNumber" (see above) }
    procedure AbortParsingFmt(const Msg: String; const Args: array of const);
      { As AbortParsing, but calls Format(Msg, Args) }

    { Reader methods }
    procedure EnumIniSection(const EnumProc: TEnumIniSectionProc;
      const SectionName: String; const Ext: Longint);
      { Enumerates values in a specified section, using the EnumProc
        procedure as a callback function. "Ext" will be used in the call
        to the EnumProc procedure. }
    procedure EnumAboutText(const Line: PChar; const Ext: Longint);
    procedure EnumConfig(const Line: PChar; const Ext: Longint);
    procedure EnumMenuItems(const Line: PChar; const Ext: Integer);
    procedure EnumMenuSettings(const Line: PChar; const Ext: Integer);
    procedure EnumMultiAction(const Line: PChar; const Ext: Integer);
    procedure EnumMessages(const Line: PChar; const Ext: Longint);
    procedure EnumServices(const Line: PChar; const Ext: Integer);
    procedure EnumVariables(const Line: PChar; const Ext: Integer);

    { Miscellaneous procs  }
    procedure BreakString(S: PChar; var Output: TBreakStringArray);
    function CompareParamName (const S: TBreakStringRec;
      const ParamInfo: array of TParamInfo;
      var ParamNamesFound: array of Boolean): Integer;
    function ISStrToBool(S: String): Boolean;
    procedure ISStrToFont(const S: String; F: TFont);
    procedure ValidateProperties;
  public
    { PROPERTIES }
    { *** Script file buffer *** }
    property Script: TStringList read FScript write SetScript;

    { *** Customizable objects *** }
    property TrayIcon: TJvTrayIcon read FTrayIcon write FTrayIcon;
    property ImageList: TImageList read FImageList write FImageList;
    property Services: TObjectList read FServices write FServices;
    property CheckServicesTimer: TTimer read FCheckServicesTimer write FCheckServicesTimer;
    property Variables: TObjectList read FVariables write FVariables;
    property DoubleClickAction: TTMMultiAction read FDoubleClickAction write FDoubleClickAction;
    property LeftClickAction: TTMMultiAction read FLeftClickAction write FLeftClickAction;
    property RightClickAction: TTMMultiAction read FRightClickAction write FRightClickAction;
    property StartupAction: TTMMultiAction read FStartupAction write FStartupAction;
    property HtmlActions: TStringList read FHtmlActions write FHtmlActions;

    { *** Customizable settings *** }
    property TrayIconAllRunning: Integer read FTrayIconAllRunning;
    property TrayIconSomeRunning: Integer read FTrayIconSomeRunning;
    property TrayIconNoneRunning: Integer read FTrayIconNoneRunning;
    property AllRunningHint: String read FAllRunningHint;
    property SomeRunningHint: String read FSomeRunningHint;
    property NoneRunningHint: String read FNoneRunningHint;
    property ServiceGlyphRunning: Integer read FServiceGlyphRunning;
    property ServiceGlyphPaused: Integer read FServiceGlyphPaused;
    property ServiceGlyphStopped: Integer read FServiceGlyphStopped;
    property ID: String read FID;
    property CustomAboutHeader: String read FCustomAboutHeader write FCustomAboutHeader;
    property CustomAboutVersion: String read FCustomAboutVersion write FCustomAboutVersion;
    property CustomAboutText: TStrings read FCustomAboutText write SetCustomAboutText;

    { *** Events *** }
    property OnSelectMenuItem: TNotifyEvent read FOnSelectMenuItem write FOnSelectMenuItem;
    property OnBuiltInActionExecute: TNotifyEvent read FOnBuiltInActionExecute write FOnBuiltInActionExecute;

    { METHODS }
    { *** General procs *** }
    constructor Create;
    destructor Destroy; override;

    { *** Key procs *** }
    procedure ReadSettings;
    procedure ReadBcBarPopupMenu(const BcBarPopupMenu: TBcBarPopupMenu;
      const SectionName: String);
        { This procedure reads popup menu settings from a section
          with name SectionName + MENUSETTINGSSUFFIX, and the menu
          from the section with name SectionName }
    procedure ReadMenu(const MenuItems: TMenuItem; const SectionName: String);
    procedure ReadMultiAction(const MultiAction: TTMMultiAction;
      const SectionName: String);
  end;

implementation

uses TypInfo, JclStrings, Forms, Registry, JclFileUtils,
     TMMsgs;

{ EParseError }

constructor EParseError.CreateLine(const Msg: String;
  const LineNumber: Integer = -1);
begin
  inherited Create(Msg);
  Self.LineNumber := LineNumber;
end;

constructor EParseError.CreateLineFmt(const Msg: String;
  Args: array of const; const LineNumber: Integer);
begin
  inherited CreateFmt(Msg, Args);
  Self.LineNumber := LineNumber;
end;

{ TTMConfigReader }

procedure TTMConfigReader.AbortParsing(const Msg: String);
begin
  raise EParseError.CreateLine(Msg, LineNumber);
end;

procedure TTMConfigReader.AbortParsingFmt(const Msg: String;
  const Args: array of const);
begin
  raise EParseError.CreateLineFmt(Msg, Args, LineNumber);
end;

procedure TTMConfigReader.BreakString(S: PChar;
  var Output: TBreakStringArray);
var
  ColonPos, SemicolonPos, QuotePos, P, P2: PChar;
  ParamName, Data: String;
  QuoteFound, FirstQuoteFound, LastQuoteFound, AddChar, FirstNonspaceFound: Boolean;
  CurParm, Len, I: Integer;
begin
  CurParm := 0;
  while (S <> nil) and (CurParm <= High(TBreakStringArray)) do begin
    ColonPos := StrScan(S, ':');
    if ColonPos = nil then
      ParamName := StrPas(S)
    else
      SetString (ParamName, S, ColonPos-S);
    ParamName := Trim(ParamName);
    if ParamName = '' then Break;
    if ColonPos = nil then
      AbortParsingFmt(SParsingParamHasNoValue, [ParamName]);
    S := ColonPos + 1;
    SemicolonPos := StrScan(S, ';');
    QuotePos := StrScan(S, '"');
    QuoteFound := QuotePos <> nil;
    if QuoteFound and (SemicolonPos <> nil) and (QuotePos > SemicolonPos) then
      QuoteFound := False;
    if not QuoteFound then begin
      Data := '';
      P := S;
      if SemicolonPos <> nil then
        P2 := SemicolonPos
      else
        P2 := StrEnd(S);
      FirstNonspaceFound := False;
      Len := 0;
      I := 0;
      while P < P2 do begin
        if (P^ <> ' ') or FirstNonspaceFound then begin
          FirstNonspaceFound := True;
          Data := Data + P^;
          Inc (I);
          if P^ <> ' ' then Len := I;
        end;
        Inc (P);
      end;
      SetLength (Data, Len);
    end
    else begin
      Data := '';
      SemicolonPos := nil;
      P := S;
      FirstQuoteFound := False;
      LastQuoteFound := False;
      while P^ <> #0 do begin
        AddChar := False;
        case P^ of
          ' ': AddChar := FirstQuoteFound;
          '"': if not FirstQuoteFound then
                 FirstQuoteFound := True
               else begin
                 Inc (P);
                 if P^ = '"' then
                   AddChar := True
                 else begin
                   LastQuoteFound := True;
                   while P^ <> #0 do begin
                     case P^ of
                       ' ': ;
                       ';': begin
                              SemicolonPos := P;
                              Break;
                            end;
                     else
                       AbortParsingFmt(SParsingParamQuoteError, [ParamName]);
                     end;
                     Inc (P);
                   end;
                   Break;
                 end;
               end;
        else
          if not FirstQuoteFound then
            AbortParsingFmt(SParsingParamQuoteError, [ParamName]);
          AddChar := True;
        end;
        if AddChar then
          Data := Data + P^;
        Inc (P);
      end;
      if not LastQuoteFound then
        AbortParsingFmt(SParsingParamMissingClosingQuote, [ParamName]);
    end;
    S := SemicolonPos;
    if S <> nil then Inc (S);
    Output[CurParm].ParamName := ParamName;
    Output[CurParm].ParamData := Data;
    Inc (CurParm);
  end;
end;

function TTMConfigReader.CompareParamName (const S: TBreakStringRec;
  const ParamInfo: array of TParamInfo;
  var ParamNamesFound: array of Boolean): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(ParamInfo) do
    if StrIComp(ParamInfo[I].Name, PChar(S.ParamName)) = 0 then begin
      Result := I;
      if ParamNamesFound[I] then
        AbortParsingFmt(SParsingParamDuplicated, [ParamInfo[I].Name]);
      ParamNamesFound[I] := True;
      if (piNoEmpty in ParamInfo[I].Flags) and (S.ParamData = '') then
        AbortParsingFmt(SParsingParamEmpty2, [ParamInfo[I].Name]);
      if (piNoQuotes in ParamInfo[I].Flags) and (Pos('"', S.ParamData) <> 0) then
        AbortParsingFmt(SParsingParamNoQuotes2, [ParamInfo[I].Name]);
      Exit;
    end;
  { Unknown parameter }
  AbortParsingFmt(SParsingParamUnknownParam, [S.ParamName]);
end;

constructor TTMConfigReader.Create;
begin
  inherited Create;

  { Memory initialization }
  FScript := TStringList.Create;
  FCustomAboutText := TStringList.Create;

  { Other inits }
  FTrayIconAllRunning := -1;
  FTrayIconSomeRunning := -1;
  FTrayIconNoneRunning := -1;
  FAllRunningHint := SDefAllRunningHint;
  FSomeRunningHint := SDefSomeRunningHint;
  FNoneRunningHint := SDefNoneRunningHint;
  FServiceGlyphRunning := -1;
  FServiceGlyphPaused := -1;
  FServiceGlyphStopped := -1;
end;

destructor TTMConfigReader.Destroy;
begin
  { Free memory }
  FreeAndNil(FCustomAboutText);
  FreeAndNil(FScript);

  inherited Destroy;
end;

procedure TTMConfigReader.EnumAboutText(const Line: PChar;
  const Ext: Integer);
begin
  CustomAboutText.Add(String(Line));
end;

procedure TTMConfigReader.EnumConfig(const Line: PChar; const Ext: Longint);

  procedure LoadImageList(BitmapFile: String);
  var
    Bmp: TBitmap;
  begin
    Bmp := TBitmap.Create();
    try
      Bmp.LoadFromFile(BitmapFile);
      with ImageList do
      begin
        Clear;
        AddMasked(Bmp, Bmp.Canvas.Pixels[0, Bmp.Height-1]);
      end;  //with imagelist
    finally
      FreeAndNil(Bmp);
    end;  //try..finally
  end;

var
  KeyName, Value: String;
  I, J: Integer;
  Directive: TConfigSectionDirective;
  MultiAction: TTMMultiAction;
begin
  SeparateDirective(Line, KeyName, Value);

  if KeyName = '' then
    Exit;
  I := GetEnumValue(TypeInfo(TConfigSectionDirective), 'cs' + KeyName);
  if I = -1 then
    AbortParsingFmt(SParsingUnknownDirective, ['Config', KeyName]);
  Directive := TConfigSectionDirective(I);
  if ConfigDirectiveLines[Directive] <> 0 then
    AbortParsingFmt(SParsingEntryAlreadySpecified, ['Config', KeyName]);
  ConfigDirectiveLines[Directive] := LineNumber;
  case Directive of
    csImageList: begin
        try
          Value := ExpandVariables(Value, Variables);
          if PathIsAbsolute(Value) then
            LoadImageList(Value)
          else
            LoadImageList(
                PathAppend(ExtractFileDir(Application.ExeName), Value));
        except
          AbortParsingFmt(SParsingCouldNotLoadFile, [Value, KeyName]);
        end;
      end;
    csServiceCheckInterval: begin
        try
          CheckServicesTimer.Interval := 1000 * StrToInt(Value);
        except
          AbortParsingFmt(SParsingParamInvalidIntValue, [KeyName]);
        end;
      end;
    csTrayIconAllRunning: begin
        if ConfigDirectiveLines[csTrayIcon] <> 0 then
          AbortParsing(SParsingAmbiguousTrayIcon);
        try
          FTrayIconAllRunning := StrToInt(Value);
        except
          AbortParsingFmt(SParsingParamInvalidIntValue, [KeyName]);
        end;
      end;
    csTrayIconSomeRunning: begin
        if ConfigDirectiveLines[csTrayIcon] <> 0 then
          AbortParsing(SParsingAmbiguousTrayIcon);
        try
          FTrayIconSomeRunning := StrToInt(Value);
        except
          AbortParsingFmt(SParsingParamInvalidIntValue, [KeyName]);
        end;
      end;
    csTrayIconNoneRunning: begin
        if ConfigDirectiveLines[csTrayIcon] <> 0 then
          AbortParsing(SParsingAmbiguousTrayIcon);
        try
          FTrayIconNoneRunning := StrToInt(Value);
        except
          AbortParsingFmt(SParsingParamInvalidIntValue, [KeyName]);
        end;
      end;
    csTrayIcon: begin
        if (ConfigDirectiveLines[csTrayIconAllRunning] <> 0) or
            (ConfigDirectiveLines[csTrayIconSomeRunning] <> 0) or
            (ConfigDirectiveLines[csTrayIconNoneRunning] <> 0) then
          AbortParsing(SParsingAmbiguousTrayIcon);
        try
          Value := ExpandVariables(Value, Variables);
          if PathIsAbsolute(Value) then
            TrayIcon.Icon.LoadFromFile(Value)
          else
            TrayIcon.Icon.LoadFromFile(
                PathAppend(ExtractFileDir(Application.ExeName), Value));
        except
          AbortParsingFmt(SParsingCouldNotLoadFile, [Value, KeyName]);
        end;
      end;
    csServiceGlyphRunning: begin
        try
          FServiceGlyphRunning := StrToInt(Value);
        except
          AbortParsingFmt(SParsingParamInvalidIntValue, [KeyName]);
        end;
      end;
    csServiceGlyphPaused: begin
        try
          FServiceGlyphPaused := StrToInt(Value);
        except
          AbortParsingFmt(SParsingParamInvalidIntValue, [KeyName]);
        end;
      end;
    csServiceGlyphStopped: begin
        try
          FServiceGlyphStopped := StrToInt(Value);
        except
          AbortParsingFmt(SParsingParamInvalidIntValue, [KeyName]);
        end;
      end;
    csID:
        FID := Value;
    csAboutHeader:
        CustomAboutHeader := Value;
    csAboutVersion: begin
        StrReplace(Value, '\n', #13#10, [rfReplaceAll, rfIgnoreCase]);
        CustomAboutVersion := Value;
      end;
    csAboutTextFile: begin
        if CustomAboutText.Count > 0 then
          AbortParsing(SParsingAmbiguousAboutText);
        try
          Value := ExpandVariables(Value, Variables);
          if PathIsAbsolute(Value) then
            CustomAboutText.LoadFromFile(Value)
          else
            CustomAboutText.LoadFromFile(
                PathAppend(ExtractFileDir(Application.ExeName), Value));
        except
          AbortParsingFmt(SParsingCouldNotLoadFile, [Value, KeyName]);
        end;
      end;
    csHtmlActions: begin
        StrTokenToStrings(Value, ' ', HtmlActions);
        for J := 0 to (HtmlActions.Count - 1) do
        begin
          MultiAction := TTMMultiAction.Create;
          try
            ReadMultiAction(MultiAction, HtmlActions[J]);
          except
            FreeAndNil(MultiAction);
            raise;
          end;  //try..except
          HtmlActions.Objects[J] := MultiAction;
        end;
      end;
  end; //case directive of
end;

procedure TTMConfigReader.EnumIniSection(const EnumProc: TEnumIniSectionProc;
  const SectionName: String; const Ext: Longint);
var
  FoundSection: Boolean;
  B, L, LastSection: String;
  I: Integer;
begin
  FoundSection := False;
  LineNumber := 0;
  LastSection := '';
  while LineNumber < Script.Count do
  begin
    B := Script[LineNumber];
    Inc(LineNumber);
    L := Trim(B);
    { Check for blank lines or comments }
    if (L = '') then
      Continue
    else
      if L[1] = ';' then
        Continue;
    if L[1] = '[' then
    begin
      { Section tag }
      I := Pos(']', L);
      if I < 3 then
        AbortParsing(SParsingSectionTagInvalid);
      L := Copy(L, 2, I-2);
      if L[1] = '/' then
      begin
        L := Copy(L, 2, Maxint);
        if (LastSection = '') or (not SameText(L, LastSection)) then
          AbortParsingFmt(SParsingSectionBadEndTag, [L]);
        FoundSection := False;
        LastSection := '';
      end
      else
      begin
        FoundSection := (CompareText(L, SectionName) = 0);
        LastSection := L;
      end;
    end
    else
    begin
      if not FoundSection then
      begin
        if LastSection = '' then
          AbortParsing(SParsingTextNotInSection);
        Continue;  { not on the right section }
      end;
      EnumProc(PChar(B), Ext);
    end;
  end;
end;

procedure TTMConfigReader.EnumMenuItems(const Line: PChar;
  const Ext: Longint);
const
  ParamNames: array[0..20] of TParamInfo = (
    (Name: ParamMenuItemType; Flags: [piNoEmpty]),      //0
    (Name: ParamMenuItemCaption; Flags: []),            //1
    (Name: ParamMenuItemGlyph; Flags: []),
    (Name: ParamMenuItemSection; Flags: []),
    (Name: ParamActionAction; Flags: []),
    (Name: ParamActionFileName; Flags: [piNoQuotes]),   //5
    (Name: ParamActionParams; Flags: []),
    (Name: ParamActionWorkingDir; Flags: []),
    (Name: ParamActionShowCmd; Flags: []),
    (Name: ParamActionShellExecVerb; Flags: []),
    (Name: ParamActionMultiSection; Flags: []),         //10
    (Name: ParamServiceService; Flags: []),
    (Name: ParamActionServiceAction; Flags: []),
    (Name: ParamActionHtmlSrc; Flags: []),
    (Name: ParamActionHtmlHeader; Flags: []),
    (Name: ParamActionHtmlWindowFlags; Flags: []),      //15
    (Name: ParamActionHtmlHeight; Flags: []),
    (Name: ParamActionHtmlWidth; Flags: []),
    (Name: ParamActionHtmlLeft; Flags: []),
    (Name: ParamActionHtmlTop; Flags: []),
    (Name: ParamActionFlags; Flags: []));               //20
var
  Params: TBreakStringArray;
  ParamNameFound: array[Low(ParamNames)..High(ParamNames)] of Boolean;
  NewMenuItem: TMenuItem;
  P, I, FoundIndex: Integer;
  B: TBuiltInAction;
  ParentMenu: TMenuItem;
  TMAction: TTMAction;
  ItemType: TMenuItemType;
begin
  ParentMenu := TMenuItem(Ext);
  ItemType := mitItem;
  TMAction := nil;

  FillChar(ParamNameFound, SizeOf(ParamNameFound), 0);
  BreakString(Line, Params);

  NewMenuItem := TMenuItem.Create(ParentMenu);
  try
    with NewMenuItem do
    begin
      for P := Low(Params) to High(Params) do
        with Params[P] do
        begin
          if ParamName = '' then
            System.Break;
          case CompareParamName(Params[P], ParamNames, ParamNameFound) of
            0: begin
                ParamData := UpperCase(Trim(ParamData));
                if ParamData = 'ITEM' then
                  ItemType := mitItem
                else if ParamData = 'SEPARATOR' then
                begin
                  ItemType := mitSeparator;
                  Caption := '-';
                end
                else if ParamData = 'SERVICESUBMENU' then
                  ItemType := mitServiceSubMenu
                else if ParamData = 'SUBMENU' then
                  ItemType := mitSubMenu
                else
                  AbortParsingFmt(SParsingUnknownMenuItemType, [ParamData]);
              end;
            1: begin
                if ItemType = mitSeparator then
                  Hint := ParamData
                else
                  Caption := ParamData;
              end;
            2: begin
                if ItemType in [mitSeparator, mitServiceSubMenu] then
                  AbortParsingFmt(SParsingInvalidMenuItemParam, [ParamName]);
                try
                  ImageIndex := StrToInt(ParamData);
                except
                  AbortParsingFmt(SParsingParamInvalidIntValue, [ParamName]);
                end;
              end;
            3: begin
                if not (ItemType in [mitSubMenu, mitServiceSubMenu]) then
                  AbortParsingFmt(SParsingInvalidMenuItemParam, [ParamName]);
                ReadMenu(NewMenuItem, ParamData);
              end;
            4: begin
                if ItemType <> mitItem then
                  AbortParsingFmt(SParsingInvalidMenuItemParam, [ParamName]);
                ParamData := UpperCase(Trim(ParamData));
                if ParamData = 'RUN' then
                begin
                  TMAction := TTMRunAction.Create;
                  (TMAction as TTMRunAction).Variables := Variables;
                end
                else if ParamData = 'SHELLEXECUTE' then
                begin
                  TMAction := TTMShellExecuteAction.Create;
                  (TMAction as TTMShellExecuteAction).Variables := Variables;
                end
                else if ParamData = 'SERVICE' then
                  TMAction := TTMServiceAction.Create
                else if ParamData = 'MULTI' then
                  TMAction := TTMMultiAction.Create
                else if ParamData = 'HTMLWINDOW' then
                begin
                  TMAction := TTMHtmlWindowAction.Create;
                  with (TMAction as TTMHtmlWindowAction) do
                  begin
                    Variables := Self.Variables;
                    HtmlActions := Self.HtmlActions;
                  end;  //tmaction as ttmhtmlwindowaction
                end
                else
                begin
                  FoundIndex := -1;
                  for B := Low(B) to High(B) do
                    if SameText('BIA' + ParamData,
                                UpperCase(GetEnumName(TypeInfo(TBuiltInAction), Ord(B)))) then
                    begin
                      FoundIndex := 0;
                      TMAction := TTMBuiltInAction.Create(B);
                      with TMAction as TTMBuiltInAction do
                        OnExecute := OnBuiltInActionExecute;
                      System.Break;
                    end;  //if sametext
                  if FoundIndex = -1 then
                    AbortParsingFmt(SParsingUnknownActionType, [ParamData]);
                end;
              end;
            5: begin
                if (TMAction is TTMRunAction) then
                  (TMAction as TTMRunAction).FileName := ParamData
                else if (TMAction is TTMShellExecuteAction) then
                  (TMAction as TTMShellExecuteAction).FileName := ParamData
                else
                  AbortParsingFmt(SParsingInvalidActionParam, [ParamName]);
              end;
            6: begin
                if (TMAction is TTMRunAction) then
                  (TMAction as TTMRunAction).Parameters := ParamData
                else if (TMAction is TTMShellExecuteAction) then
                  (TMAction as TTMShellExecuteAction).Parameters := ParamData
                else
                  AbortParsingFmt(SParsingInvalidActionParam, [ParamName]);
              end;
            7: begin
                if (TMAction is TTMRunAction) then
                  (TMAction as TTMRunAction).WorkingDir := ParamData
                else if (TMAction is TTMShellExecuteAction) then
                  (TMAction as TTMShellExecuteAction).WorkingDir := ParamData
                else
                  AbortParsingFmt(SParsingInvalidActionParam, [ParamName]);
              end;
            8: begin
                ParamData := UpperCase(Trim(ParamData));
                if (TMAction is TTMRunAction) then
                  with TMAction as TTMRunAction do
                    if ParamData = 'NORMAL' then
                      ShowCmd := swNormal
                    else if ParamData = 'HIDDEN' then
                      ShowCmd := swHidden
                    else if ParamData = 'MAXIMIZED' then
                      ShowCmd := swMaximized
                    else if ParamData = 'MINIMIZED' then
                      ShowCmd := swMinimized
                    else
                      AbortParsingFmt(SParsingUnknownShowCmd, [ParamData])
                else if (TMAction is TTMShellExecuteAction) then
                  with TMAction as TTMShellExecuteAction do
                    if ParamData = 'NORMAL' then
                      ShowCmd := swNormal
                    else if ParamData = 'HIDDEN' then
                      ShowCmd := swHidden
                    else if ParamData = 'MAXIMIZED' then
                      ShowCmd := swMaximized
                    else if ParamData = 'MINIMIZED' then
                      ShowCmd := swMinimized
                    else
                      AbortParsingFmt(SParsingUnknownShowCmd, [ParamData])
                else
                  AbortParsingFmt(SParsingInvalidActionParam, [ParamName]);
              end;
            9: begin
                if not (TMAction is TTMShellExecuteAction) then
                  AbortParsingFmt(SParsingInvalidActionParam, [ParamName])
                else
                  (TMAction as TTMShellExecuteAction).Verb := ParamData;
              end;
            10: begin
                if not (TMAction is TTMMultiAction) then
                  AbortParsingFmt(SParsingInvalidActionParam, [ParamName])
                else
                  ReadMultiAction(TMAction as TTMMultiAction, ParamData);
              end;
            11: begin
                if (TMAction is TTMServiceAction) or (ItemType = mitServiceSubMenu) then
                begin
                  //We first have to search for the service in the
                  // "Services" list
                  FoundIndex := -1;
                  for I := 0 to Services.Count - 1 do
                    if SameText((Services[I] as TTMService).ServiceName, Trim(ParamData)) then
                    begin
                      FoundIndex := I;
                      System.Break;
                    end;
                  if FoundIndex = -1 then
                    AbortParsingFmt(SParsingParamUnknownService, [ParamName]);

                  if TMAction is TTMServiceAction then
                    (TMAction as TTMServiceAction).Service :=
                        (Services[FoundIndex] as TTMService)
                  else
                    Tag := Longint(Services[FoundIndex]);
                end
                else  //tmaction is ttmserviceaction or itemtype = mitservicesubmenu
                  AbortParsing(SParsingServiceParamInvalid);
              end;
            12: begin
                  if not (TMAction is TTMServiceAction) then
                    AbortParsingFmt(SParsingInvalidActionParam, [ParamName]);
                  ParamData := UpperCase(Trim(ParamData));
                  with TMAction as TTMServiceAction do
                    if ParamData = 'STARTRESUME' then
                      Action := saStartResume
                    else if ParamData = 'PAUSE' then
                      Action := saPause
                    else if ParamData = 'STOP' then
                      Action := saStop
                    else if ParamData = 'RESTART' then
                      Action := saRestart
                    else
                      AbortParsingFmt(SParsingInvalidServiceAction, [ParamData]);
              end;
            13: begin
                if not (TMAction is TTMHtmlWindowAction) then
                    AbortParsingFmt(SParsingInvalidActionParam, [ParamName]);
                (TMAction as TTMHtmlWindowAction).Src := ParamData;
              end;
            14: begin
                if not (TMAction is TTMHtmlWindowAction) then
                    AbortParsingFmt(SParsingInvalidActionParam, [ParamName]);
                (TMAction as TTMHtmlWindowAction).Header := ParamData;
              end;
            15: begin
                if not (TMAction is TTMHtmlWindowAction) then
                    AbortParsingFmt(SParsingInvalidActionParam, [ParamName]);
                with (TMAction as TTMHtmlWindowAction) do
                  while True do
                    case ExtractFlag(ParamData, TMStruct.HtmlWindowFlags) of
                      -2: System.Break;
                      -1: AbortParsingFmt(SParsingParamUnknownFlag2, [ParamName]);
                      0: HtmlWindowFlags := HtmlWindowFlags + [hwfMaximized];
                      1: HtmlWindowFlags := HtmlWindowFlags + [hwfNoResize];
                      2: HtmlWindowFlags := HtmlWindowFlags + [hwfNoScrollbars];
                      3: HtmlWindowFlags := HtmlWindowFlags + [hwfEnableContextMenu];
                      4: HtmlWindowFlags := HtmlWindowFlags + [hwfNoCloseButton];
                      5: HtmlWindowFlags := HtmlWindowFlags + [hwfNoHeader];
                      6: HtmlWindowFlags := HtmlWindowFlags + [hwfAlwaysOnTop];
                    end;  //with (..) do while true do case extractflag of
              end;
            16: begin
                if not (TMAction is TTMHtmlWindowAction) then
                    AbortParsingFmt(SParsingInvalidActionParam, [ParamName]);
                try
                  (TMAction as TTMHtmlWindowAction).Height := StrToInt(ParamData);
                except
                  AbortParsingFmt(SParsingParamInvalidIntValue, [ParamName]);
                end;
              end;
            17: begin
                if not (TMAction is TTMHtmlWindowAction) then
                    AbortParsingFmt(SParsingInvalidActionParam, [ParamName]);
                try
                  (TMAction as TTMHtmlWindowAction).Width := StrToInt(ParamData);
                except
                  AbortParsingFmt(SParsingParamInvalidIntValue, [ParamName]);
                end;
              end;
            18: begin
                if not (TMAction is TTMHtmlWindowAction) then
                    AbortParsingFmt(SParsingInvalidActionParam, [ParamName]);
                try
                  (TMAction as TTMHtmlWindowAction).Left := StrToInt(ParamData);
                except
                  AbortParsingFmt(SParsingParamInvalidIntValue, [ParamName]);
                end;
              end;
            19: begin
                if not (TMAction is TTMHtmlWindowAction) then
                    AbortParsingFmt(SParsingInvalidActionParam, [ParamName]);
                try
                  (TMAction as TTMHtmlWindowAction).Top := StrToInt(ParamData);
                except
                  AbortParsingFmt(SParsingParamInvalidIntValue, [ParamName]);
                end;
              end;
            20: begin
                if not Assigned(TMAction) then
                  AbortParsing(SParsingCannotAssignActionFlagsYet);
                while True do
                  case ExtractFlag(ParamData, ActionFlags) of
                    -2: System.Break;
                    -1: AbortParsingFmt(SParsingParamUnknownFlag2, [ParamName]);
                    0: TMAction.Flags := TMAction.Flags + [afIgnoreErrors];
                    1: TMAction.Flags := TMAction.Flags + [afWaitUntilTerminated];
                    2: TMAction.Flags := TMAction.Flags + [afWaitUntilIdle];
                  end;  //while true do case extractflag of
              end;
          end;  //case
        end;  //for p do with params[p] do

      { Validation }
      //Type:
      if (not ParamNameFound[0]) then
        AbortParsingFmt(SParsingParamExpected, [ParamMenuItemType]);
      //Caption:
      if (ItemType in [mitSubMenu, mitItem]) and (not ParamNameFound[1]) then
        AbortParsingFmt(SParsingParamExpected, [ParamMenuItemCaption]);
      //SubMenu:
      if (ItemType in [mitSubMenu, mitServiceSubMenu]) and
          (not ParamNameFound[3]) then
        AbortParsingFmt(SParsingParamExpected, [ParamMenuItemSection]);
      //Service:
      if ((ItemType = mitServiceSubMenu) or (TMAction is TTMServiceAction))
          and (not ParamNameFound[11]) then
        AbortParsingFmt(SParsingParamExpected, [ParamServiceService]);
      //FileName:
      if ((TMAction is TTMRunAction) or (TMAction is TTMShellExecuteAction))
          and (not ParamNameFound[5]) then
        AbortParsingFmt(SParsingParamExpected, [ParamActionFileName]);
      //Action:
      if (ItemType = mitItem) and (not ParamNameFound[4]) then
        AbortParsingFmt(SParsingParamExpected, [ParamActionAction]);
      //Actions:
      if (TMAction is TTMMultiAction) and (not ParamNameFound[10]) then
        AbortParsingFmt(SParsingParamExpected, [ParamActionMultiSection]);
      //ServiceAction:
      if (TMAction is TTMServiceAction) and (not ParamNameFound[12]) then
        AbortParsingFmt(SParsingParamExpected, [ParamActionServiceAction]);
      //Flags:
      if Assigned(TMAction) then
      begin
        if (afWaitUntilTerminated in TMAction.Flags) and
            (not ((TMAction is TTMRunAction) or (TMAction is TTMServiceAction))) then
          AbortParsingFmt(SParsingInvalidActionFlag, ['waituntilterminated']);
        if (afWaitUntilIdle in TMAction.Flags) and
            (not ((TMAction is TTMRunAction) or (TMAction is TTMServiceAction))) then
          AbortParsingFmt(SParsingInvalidActionFlag, ['waituntilidle']);
      end;  //if assigned(tmaction)
    end;  //with newserviceitem
  except
    on E: Exception do
    begin
      FreeAndNil(NewMenuItem);
      AbortParsing(E.Message);
      Exit;
    end;
  end;
  if ItemType = mitItem then
    with NewMenuItem do
    begin
      Tag := Longint(TMAction);
      OnClick := OnSelectMenuItem;
    end;  //if itemtype = mititem with newmenuitem do
  ParentMenu.Add(NewMenuItem);
end;

procedure TTMConfigReader.EnumMenuSettings(const Line: PChar; const Ext: Longint);
var
  KeyName, Value: String;
  I: Integer;
  Directive: TMenuSectionDirective;
begin
  SeparateDirective(Line, KeyName, Value);

  if KeyName = '' then
    Exit;
  I := GetEnumValue(TypeInfo(TMenuSectionDirective), 'mn' + KeyName);
  if I = -1 then
    AbortParsingFmt(SParsingUnknownDirective, ['Menu.*.Settings', KeyName]);
  Directive := TMenuSectionDirective(I);
  if MenuDirectiveLines[Directive] <> 0 then
    AbortParsingFmt(SParsingEntryAlreadySpecified, ['Menu.*.Settings', KeyName]);
  MenuDirectiveLines[Directive] := LineNumber;
  with TBcBarPopupMenu(Ext) do
    case Directive of
      mnAutoHotkeys: begin
          if ISStrToBool(Value) then
            AutoHotkeys := maAutomatic
          else
            AutoHotkeys := maManual;
        end;
      mnAutoLineReduction: begin
          if ISStrToBool(Value) then
            AutoLineReduction := maAutomatic
          else
            AutoLineReduction := maManual;
        end;
      mnBarBackPictureDrawStyle: begin
          Value := UpperCase(Trim(Value));
          with Bar.BarBackPicture do
            if Value = 'NORMAL' then
              DrawStyle := BarMenus.dsNormal
            else if Value = 'STRETCH' then
              DrawStyle := BarMenus.dsStretch
            else if Value = 'TILE' then
              DrawStyle := BarMenus.dsTile
            else
              AbortParsingFmt(SParsingInvalidDirectiveValue, [Value, KeyName]);
        end;
      mnBarBackPictureHorzAlignment: begin
          Value := UpperCase(Trim(Value));
          with Bar.BarBackPicture do
            if Value = 'LEFT' then
              HorzAlignment := haLeft
            else if Value = 'CENTER' then
              HorzAlignment := haCenter
            else if Value = 'RIGHT' then
              HorzAlignment := haRight
            else
              AbortParsingFmt(SParsingInvalidDirectiveValue, [Value, KeyName]);
        end;
      mnBarBackPictureOffsetX: begin
          try
            Bar.BarBackPicture.OffsetX := StrToInt(Value);
          except
            AbortParsingFmt(SParsingParamInvalidIntValue, [KeyName]);
          end;
        end;
      mnBarBackPictureOffsetY: begin
          try
            Bar.BarBackPicture.OffsetY := StrToInt(Value);
          except
            AbortParsingFmt(SParsingParamInvalidIntValue, [KeyName]);
          end;
        end;
      mnBarBackPicturePicture: begin
          try
            if PathIsAbsolute(Value) then
              Bar.BarBackPicture.Picture.LoadFromFile(Value)
            else
              Bar.BarBackPicture.Picture.LoadFromFile(
                  PathAppend(ExtractFileDir(Application.ExeName), Value));
          except
            AbortParsingFmt(SParsingCouldNotLoadFile, [Value, KeyName]);
          end;
          Bar.BarBackPicture.Visible := True;
        end;
      mnBarBackPictureTransparent:
          Bar.BarBackPicture.Transparent := ISStrToBool(Value);
      mnBarBackPictureVertAlignment: begin
          Value := UpperCase(Trim(Value));
          with Bar.BarBackPicture do
            if Value = 'TOP' then
              VertAlignment := vaTop
            else if Value = 'MIDDLE' then
              VertAlignment := vaMiddle
            else if Value = 'BOTTOM' then
              VertAlignment := vaBottom
            else
              AbortParsingFmt(SParsingInvalidDirectiveValue, [Value, KeyName]);
        end;
      mnBarCaptionAlignment: begin
          Value := UpperCase(Trim(Value));
          with Bar.BarCaption do
            if Value = 'TOP' then
              Alignment := vaTop
            else if Value = 'MIDDLE' then
              Alignment := vaMiddle
            else if Value = 'BOTTOM' then
              Alignment := vaBottom
            else
              AbortParsingFmt(SParsingInvalidDirectiveValue, [Value, KeyName]);
        end;
      mnBarCaptionCaption: begin
          with Bar.BarCaption do
          begin
            Caption := Value;
            Visible := True;
          end;  //with bar.barcaption
        end;
      mnBarCaptionDepth: begin
          try
            Bar.BarCaption.Depth := StrToInt(Value);
          except
            AbortParsingFmt(SParsingParamInvalidIntValue, [KeyName]);
          end;
        end;
      mnBarCaptionDirection: begin
          Value := UpperCase(Trim(Value));
          with Bar.BarCaption do
            if Value = 'DOWNTOUP' then
              Direction := dDownToUp
            else if Value = 'UPTODOWN' then
              Direction := dUpToDown
            else
              AbortParsingFmt(SParsingInvalidDirectiveValue, [Value, KeyName]);
        end;
      mnBarCaptionFont:
          ISStrToFont(Value, Bar.BarCaption.Font);
      mnBarCaptionHighlightColor: begin
          try
            Bar.BarCaption.HighlightColor := StringToColor(Value);
          except
            AbortParsingFmt(SParsingInvalidColor, [KeyName]);
          end;
        end;
      mnBarCaptionOffsetY: begin
          try
            Bar.BarCaption.OffsetY := StrToInt(Value);
          except
            AbortParsingFmt(SParsingParamInvalidIntValue, [KeyName]);
          end;
        end;
      mnBarCaptionShadowColor: begin
          try
            Bar.BarCaption.ShadowColor := StringToColor(Value);
          except
            AbortParsingFmt(SParsingInvalidColor, [KeyName]);
          end;
        end;
      mnBarPictureHorzAlignment: begin
          Value := UpperCase(Trim(Value));
          with Bar.BarPicture do
            if Value = 'LEFT' then
              HorzAlignment := haLeft
            else if Value = 'CENTER' then
              HorzAlignment := haCenter
            else if Value = 'RIGHT' then
              HorzAlignment := haRight
            else
              AbortParsingFmt(SParsingInvalidDirectiveValue, [Value, KeyName]);
        end;
      mnBarPictureOffsetX: begin
          try
            Bar.BarPicture.OffsetX := StrToInt(Value);
          except
            AbortParsingFmt(SParsingParamInvalidIntValue, [KeyName]);
          end;
        end;
      mnBarPictureOffsetY: begin
          try
            Bar.BarPicture.OffsetY := StrToInt(Value);
          except
            AbortParsingFmt(SParsingParamInvalidIntValue, [KeyName]);
          end;
        end;
      mnBarPicturePicture: begin
          try
            if PathIsAbsolute(Value) then
              Bar.BarPicture.Picture.LoadFromFile(Value)
            else
              Bar.BarPicture.Picture.LoadFromFile(
                  PathAppend(ExtractFileDir(Application.ExeName), Value));
          except
            AbortParsingFmt(SParsingCouldNotLoadFile, [Value, KeyName]);
          end;
          Bar.BarPicture.Visible := True;
        end;
      mnBarPictureTransparent:
          Bar.BarPicture.Transparent := ISStrToBool(Value);
      mnBarPictureVertAlignment: begin
          Value := UpperCase(Trim(Value));
          with Bar.BarPicture do
            if Value = 'TOP' then
              VertAlignment := vaTop
            else if Value = 'MIDDLE' then
              VertAlignment := vaMiddle
            else if Value = 'BOTTOM' then
              VertAlignment := vaBottom
            else
              AbortParsingFmt(SParsingInvalidDirectiveValue, [Value, KeyName]);
        end;
      mnBarBorder: begin
          try
            Bar.Border := StringToColor(Value);
          except
            AbortParsingFmt(SParsingInvalidColor, [KeyName]);
          end;
        end;
      mnBarGradientEnd: begin
          try
            Bar.GradientEnd := StringToColor(Value);
          except
            AbortParsingFmt(SParsingInvalidColor, [KeyName]);
          end;
        end;
      mnBarGradientStart: begin
          try
            Bar.GradientStart := StringToColor(Value);
          except
            AbortParsingFmt(SParsingInvalidColor, [KeyName]);
          end;
        end;
      mnBarGradientStyle: begin
          Value := UpperCase(Trim(Value));
          with Bar do
            if Value = 'DIAGONALLEFTTORIGHT' then
              GradientStyle := gsDiagonalLeftRight
            else if Value = 'DIAGONALRIGHTTOLEFT' then
              GradientStyle := gsDiagonalRightLeft
            else if Value = 'HORIZONTAL' then
              GradientStyle := gsHorizontal
            else if Value = 'VERTICAL' then
              GradientStyle := gsVertical
            else
              AbortParsingFmt(SParsingInvalidDirectiveValue, [Value, KeyName]);
        end;
      mnBarSide: begin
          Value := UpperCase(Trim(Value));
          with Bar do
            if Value = 'LEFT' then
              Side := sLeft
            else if Value = 'RIGHT' then
              Side := sRight
            else
              AbortParsingFmt(SParsingInvalidDirectiveValue, [Value, KeyName]);
        end;
      mnBarSpace: begin
          try
            Bar.Space := StrToInt(Value);
          except
            AbortParsingFmt(SParsingParamInvalidIntValue, [KeyName]);
          end;
        end;
      mnBarVisible:
          Bar.Visible := ISStrToBool(Value);
      mnBarWidth: begin
          try
            Bar.Width := StrToInt(Value);
          except
            AbortParsingFmt(SParsingParamInvalidIntValue, [KeyName]);
          end;
        end;
      mnMenuFont: begin
          ISStrToFont(Value, MenuFont);
          UseSystemFont := False;
        end;
      mnSeparatorsAlignment: begin
          Value := UpperCase(Trim(Value));
          with Separators do
            if Value = 'LEFT' then
              Alignment := taLeftJustify
            else if Value = 'CENTER' then
              Alignment := taCenter
            else if Value = 'RIGHT' then
              Alignment := taRightJustify
            else
              AbortParsingFmt(SParsingInvalidDirectiveValue, [Value, KeyName]);
        end;
      mnSeparatorsFade:
          Separators.Fade := ISStrToBool(Value);
      mnSeparatorsFadeColor: begin
          try
            Separators.FadeColor := StringToColor(Value);
          except
            AbortParsingFmt(SParsingInvalidColor, [KeyName]);
          end;
        end;
      mnSeparatorsFadeWidth: begin
          try
            Separators.FadeWidth := StrToInt(Value);
          except
            AbortParsingFmt(SParsingParamInvalidIntValue, [KeyName]);
          end;
        end;
      mnSeparatorsFlatLines:
          Separators.FlatLines := ISStrToBool(Value);
      mnSeparatorsFont: begin
          ISStrToFont(Value, Separators.Font);
          Separators.UseSystemFont := False;
        end;
      mnSeparatorsGradientEnd: begin
          try
            Separators.GradientEnd := StringToColor(Value);
          except
            AbortParsingFmt(SParsingInvalidColor, [KeyName]);
          end;
        end;
      mnSeparatorsGradientStart: begin
          try
            Separators.GradientStart := StringToColor(Value);
          except
            AbortParsingFmt(SParsingInvalidColor, [KeyName]);
          end;
        end;
      mnSeparatorsGradientStyle: begin
          Value := UpperCase(Trim(Value));
          with Separators do
            if Value = 'DIAGONALLEFTTORIGHT' then
              GradientStyle := gsDiagonalLeftRight
            else if Value = 'DIAGONALRIGHTTOLEFT' then
              GradientStyle := gsDiagonalRightLeft
            else if Value = 'HORIZONTAL' then
              GradientStyle := gsHorizontal
            else if Value = 'VERTICAL' then
              GradientStyle := gsVertical
            else
              AbortParsingFmt(SParsingInvalidDirectiveValue, [Value, KeyName]);
        end;
       mnSeparatorsSeparatorStyle: begin
          Value := UpperCase(Trim(Value));
          with Separators do
            if Value = 'NORMAL' then
              SeparatorStyle := ssNormal
            else if Value = 'SHORTLINE' then
              SeparatorStyle := ssShortLine
            else if Value = 'CAPTION' then
              SeparatorStyle := ssCaption
            else
              AbortParsingFmt(SParsingInvalidDirectiveValue, [Value, KeyName]);
        end;
    end; //case directive of
end;

procedure TTMConfigReader.EnumMessages(const Line: PChar; const Ext: Longint);
var
  KeyName, Value: String;
  I: Integer;
  Directive: TMessagesSectionDirective;
begin
  SeparateDirective(Line, KeyName, Value);

  if KeyName = '' then
    Exit;
  I := GetEnumValue(TypeInfo(TMessagesSectionDirective), 'ms' + KeyName);
  if I = -1 then
    AbortParsingFmt(SParsingUnknownDirective, ['Messages', KeyName]);
  Directive := TMessagesSectionDirective(I);
  if MessagesDirectiveLines[Directive] <> 0 then
    AbortParsingFmt(SParsingEntryAlreadySpecified, ['Messages', KeyName]);
  MessagesDirectiveLines[Directive] := LineNumber;
  case Directive of
    msAllRunningHint: begin
        FAllRunningHint := Value;
      end;
    msSomeRunningHint: begin
        FSomeRunningHint := Value;
      end;
    msNoneRunningHint: begin
        FNoneRunningHint := Value;
      end;
  end; //case directive of
end;

procedure TTMConfigReader.EnumMultiAction(const Line: PChar;
  const Ext: Integer);
const
  ParamNames: array[0..16] of TParamInfo = (
    (Name: ParamActionAction; Flags: []),               //0
    (Name: ParamActionFileName; Flags: [piNoQuotes]),   //1
    (Name: ParamActionParams; Flags: []),
    (Name: ParamActionWorkingDir; Flags: []),
    (Name: ParamActionShowCmd; Flags: []),
    (Name: ParamActionShellExecVerb; Flags: []),        //5
    (Name: ParamActionMultiSection; Flags: []),
    (Name: ParamServiceService; Flags: []),
    (Name: ParamActionServiceAction; Flags: []),
    (Name: ParamActionFlags; Flags: []),
    (Name: ParamActionHtmlSrc; Flags: []),              //10
    (Name: ParamActionHtmlHeader; Flags: []),
    (Name: ParamActionHtmlWindowFlags; Flags: []),
    (Name: ParamActionHtmlHeight; Flags: []),
    (Name: ParamActionHtmlWidth; Flags: []),
    (Name: ParamActionHtmlLeft; Flags: []),             //15
    (Name: ParamActionHtmlTop; Flags: [])
    );
var
  Params: TBreakStringArray;
  ParamNameFound: array[Low(ParamNames)..High(ParamNames)] of Boolean;
  P, I, FoundIndex: Integer;
  B: TBuiltInAction;
  NewAction: TTMAction;
  MultiAction: TTMMultiAction;
begin
  MultiAction := TTMMultiAction(Ext);
  NewAction := nil;

  FillChar(ParamNameFound, SizeOf(ParamNameFound), 0);
  BreakString(Line, Params);

  try
    with NewAction do
    begin
      for P := Low(Params) to High(Params) do
        with Params[P] do
        begin
          if ParamName = '' then
            System.Break;
          case CompareParamName(Params[P], ParamNames, ParamNameFound) of
            0: begin
                ParamData := UpperCase(Trim(ParamData));
                if ParamData = 'RUN' then
                begin
                  NewAction := TTMRunAction.Create;
                  (NewAction as TTMRunAction).Variables := Variables;
                end
                else if ParamData = 'SHELLEXECUTE' then
                begin
                  NewAction := TTMShellExecuteAction.Create;
                  (NewAction as TTMShellExecuteAction).Variables := Variables;
                end
                else if ParamData = 'SERVICE' then
                  NewAction := TTMServiceAction.Create
                else if ParamData = 'MULTI' then
                  NewAction := TTMMultiAction.Create
                else if ParamData = 'HTMLWINDOW' then
                begin
                  NewAction := TTMHtmlWindowAction.Create;
                  with (NewAction as TTMHtmlWindowAction) do
                  begin
                    Variables := Self.Variables;
                    HtmlActions := Self.HtmlActions;
                  end;  //tmaction as ttmhtmlwindowaction
                end
                else
                begin
                  FoundIndex := -1;
                  for B := Low(B) to High(B) do
                    if SameText('BIA' + ParamData,
                                UpperCase(GetEnumName(TypeInfo(TBuiltInAction), Ord(B)))) then
                    begin
                      FoundIndex := 0;
                      NewAction := TTMBuiltInAction.Create(B);
                      with NewAction as TTMBuiltInAction do
                        OnExecute := OnBuiltInActionExecute;
                      System.Break;
                    end;  //if sametext
                  if FoundIndex = -1 then
                    AbortParsingFmt(SParsingUnknownActionType, [ParamData]);
                end;
              end;
            1: begin
                if (NewAction is TTMRunAction) then
                  (NewAction as TTMRunAction).FileName := ParamData
                else if (NewAction is TTMShellExecuteAction) then
                  (NewAction as TTMShellExecuteAction).FileName := ParamData
                else
                  AbortParsingFmt(SParsingInvalidActionParam, [ParamName]);
              end;
            2: begin
                if (NewAction is TTMRunAction) then
                  (NewAction as TTMRunAction).Parameters := ParamData
                else if (NewAction is TTMShellExecuteAction) then
                  (NewAction as TTMShellExecuteAction).Parameters := ParamData
                else
                  AbortParsingFmt(SParsingInvalidActionParam, [ParamName]);
              end;
            3: begin
                if (NewAction is TTMRunAction) then
                  (NewAction as TTMRunAction).WorkingDir := ParamData
                else if (NewAction is TTMShellExecuteAction) then
                  (NewAction as TTMShellExecuteAction).WorkingDir := ParamData
                else
                  AbortParsingFmt(SParsingInvalidActionParam, [ParamName]);
              end;
            4: begin
                ParamData := UpperCase(Trim(ParamData));
                if (NewAction is TTMRunAction) then
                  with NewAction as TTMRunAction do
                    if ParamData = 'NORMAL' then
                      ShowCmd := swNormal
                    else if ParamData = 'HIDDEN' then
                      ShowCmd := swHidden
                    else if ParamData = 'MAXIMIZED' then
                      ShowCmd := swMaximized
                    else if ParamData = 'MINIMIZED' then
                      ShowCmd := swMinimized
                    else
                      AbortParsingFmt(SParsingUnknownShowCmd, [ParamData])
                else if (NewAction is TTMShellExecuteAction) then
                  with NewAction as TTMShellExecuteAction do
                    if ParamData = 'NORMAL' then
                      ShowCmd := swNormal
                    else if ParamData = 'HIDDEN' then
                      ShowCmd := swHidden
                    else if ParamData = 'MAXIMIZED' then
                      ShowCmd := swMaximized
                    else if ParamData = 'MINIMIZED' then
                      ShowCmd := swMinimized
                    else
                      AbortParsingFmt(SParsingUnknownShowCmd, [ParamData])
                else
                  AbortParsingFmt(SParsingInvalidActionParam, [ParamName]);
              end;
            5: begin
                if not (NewAction is TTMShellExecuteAction) then
                  AbortParsingFmt(SParsingInvalidActionParam, [ParamName])
                else
                  (NewAction as TTMShellExecuteAction).Verb := ParamData;
              end;
            6: begin
                if not (NewAction is TTMMultiAction) then
                  AbortParsingFmt(SParsingInvalidActionParam, [ParamName])
                else
                  ReadMultiAction(NewAction as TTMMultiAction, ParamData);
              end;
            7: begin
                if NewAction is TTMServiceAction then
                begin
                  //We first have to search for the service in the
                  // "Services" list
                  FoundIndex := -1;
                  for I := 0 to Services.Count - 1 do
                    if SameText((Services[I] as TTMService).ServiceName, Trim(ParamData)) then
                    begin
                      FoundIndex := I;
                      System.Break;
                    end;
                  if FoundIndex = -1 then
                    AbortParsingFmt(SParsingParamUnknownService, [ParamName]);

                  (NewAction as TTMServiceAction).Service :=
                      (Services[FoundIndex] as TTMService);
                end
                else  //NewAction is ttmserviceaction or itemtype = mitservicesubmenu
                  AbortParsing(SParsingServiceParamInvalid);
              end;
            8: begin
                  if not (NewAction is TTMServiceAction) then
                    AbortParsingFmt(SParsingInvalidActionParam, [ParamName]);
                  ParamData := UpperCase(Trim(ParamData));
                  with NewAction as TTMServiceAction do
                    if ParamData = 'STARTRESUME' then
                      Action := saStartResume
                    else if ParamData = 'PAUSE' then
                      Action := saPause
                    else if ParamData = 'STOP' then
                      Action := saStop
                    else if ParamData = 'RESTART' then
                      Action := saRestart
                    else
                      AbortParsingFmt(SParsingInvalidServiceAction, [ParamData]);
              end;
            9: begin
                if not Assigned(NewAction) then
                  AbortParsing(SParsingCannotAssignActionFlagsYet);
                while True do
                  case ExtractFlag(ParamData, ActionFlags) of
                    -2: System.Break;
                    -1: AbortParsingFmt(SParsingParamUnknownFlag2, [ParamName]);
                    0: NewAction.Flags := NewAction.Flags + [afIgnoreErrors];
                    1: NewAction.Flags := NewAction.Flags + [afWaitUntilTerminated];
                    2: NewAction.Flags := NewAction.Flags + [afWaitUntilIdle];
                  end;  //while true do case extractflag of
              end;
            10: begin
                if not (NewAction is TTMHtmlWindowAction) then
                    AbortParsingFmt(SParsingInvalidActionParam, [ParamName]);
                (NewAction as TTMHtmlWindowAction).Src := ParamData;
              end;
            11: begin
                if not (NewAction is TTMHtmlWindowAction) then
                    AbortParsingFmt(SParsingInvalidActionParam, [ParamName]);
                (NewAction as TTMHtmlWindowAction).Header := ParamData;
              end;
            12: begin
                if not (NewAction is TTMHtmlWindowAction) then
                    AbortParsingFmt(SParsingInvalidActionParam, [ParamName]);
                with (NewAction as TTMHtmlWindowAction) do
                  while True do
                    case ExtractFlag(ParamData, TMStruct.HtmlWindowFlags) of
                      -2: System.Break;
                      -1: AbortParsingFmt(SParsingParamUnknownFlag2, [ParamName]);
                      0: HtmlWindowFlags := HtmlWindowFlags + [hwfMaximized];
                      1: HtmlWindowFlags := HtmlWindowFlags + [hwfNoResize];
                      2: HtmlWindowFlags := HtmlWindowFlags + [hwfNoScrollbars];
                      3: HtmlWindowFlags := HtmlWindowFlags + [hwfEnableContextMenu];
                      4: HtmlWindowFlags := HtmlWindowFlags + [hwfNoCloseButton];
                      5: HtmlWindowFlags := HtmlWindowFlags + [hwfNoHeader];
                      6: HtmlWindowFlags := HtmlWindowFlags + [hwfAlwaysOnTop];
                    end;  //with (..) do while true do case extractflag of
              end;
            13: begin
                if not (NewAction is TTMHtmlWindowAction) then
                    AbortParsingFmt(SParsingInvalidActionParam, [ParamName]);
                try
                  (NewAction as TTMHtmlWindowAction).Height := StrToInt(ParamData);
                except
                  AbortParsingFmt(SParsingParamInvalidIntValue, [ParamName]);
                end;
              end;
            14: begin
                if not (NewAction is TTMHtmlWindowAction) then
                    AbortParsingFmt(SParsingInvalidActionParam, [ParamName]);
                try
                  (NewAction as TTMHtmlWindowAction).Width := StrToInt(ParamData);
                except
                  AbortParsingFmt(SParsingParamInvalidIntValue, [ParamName]);
                end;
              end;
            15: begin
                if not (NewAction is TTMHtmlWindowAction) then
                    AbortParsingFmt(SParsingInvalidActionParam, [ParamName]);
                try
                  (NewAction as TTMHtmlWindowAction).Left := StrToInt(ParamData);
                except
                  AbortParsingFmt(SParsingParamInvalidIntValue, [ParamName]);
                end;
              end;
            16: begin
                if not (NewAction is TTMHtmlWindowAction) then
                    AbortParsingFmt(SParsingInvalidActionParam, [ParamName]);
                try
                  (NewAction as TTMHtmlWindowAction).Top := StrToInt(ParamData);
                except
                  AbortParsingFmt(SParsingParamInvalidIntValue, [ParamName]);
                end;
              end;
          end;  //case
        end;  //for p do with params[p] do

      { Validation }
      //Service:
      if (NewAction is TTMServiceAction)
          and (not ParamNameFound[7]) then
        AbortParsingFmt(SParsingParamExpected, [ParamServiceService]);
      //FileName:
      if ((NewAction is TTMRunAction) or (NewAction is TTMShellExecuteAction))
          and (not ParamNameFound[1]) then
        AbortParsingFmt(SParsingParamExpected, [ParamActionFileName]);
      //Action:
      if not ParamNameFound[0] then
        AbortParsingFmt(SParsingParamExpected, [ParamActionAction]);
      //Actions:
      if (NewAction is TTMMultiAction) and (not ParamNameFound[6]) then
        AbortParsingFmt(SParsingParamExpected, [ParamActionMultiSection]);
      //ServiceAction:
      if (NewAction is TTMServiceAction) and (not ParamNameFound[8]) then
        AbortParsingFmt(SParsingParamExpected, [ParamActionServiceAction]);
      //Flags:
      if (afWaitUntilTerminated in NewAction.Flags) and
          (not ((NewAction is TTMRunAction) or (NewAction is TTMServiceAction))) then
        AbortParsingFmt(SParsingInvalidActionFlag, ['waituntilterminated']);
      if (afWaitUntilIdle in NewAction.Flags) and
          (not ((NewAction is TTMRunAction) or (NewAction is TTMServiceAction))) then
        AbortParsingFmt(SParsingInvalidActionFlag, ['waituntilidle']);
    end;  //with newserviceitem
  except
    on E: Exception do
    begin
      FreeAndNil(NewAction);
      AbortParsing(E.Message);
      Exit;
    end;
  end;
  MultiAction.Add(NewAction)
end;

procedure TTMConfigReader.EnumServices(const Line: PChar;
  const Ext: Longint);
const
  ParamNames: array[0..0] of TParamInfo = (
    (Name: ParamServicesName; Flags: [piNoEmpty]){,
    (Name: ParamServicesDisplayName; Flags: [piNoEmpty])});
var
  Params: TBreakStringArray;
  ParamNameFound: array[Low(ParamNames)..High(ParamNames)] of Boolean;
  NewServiceItem: TTMService;
  P: Integer;
begin
  FillChar(ParamNameFound, SizeOf(ParamNameFound), 0);
  BreakString(Line, Params);

  NewServiceItem := TTMService.Create;
    //the TTMService object is already owned by the list
  try
    with NewServiceItem do
    begin
      for P := Low(Params) to High(Params) do
        with Params[P] do
        begin
          if ParamName = '' then
            Break;
          case CompareParamName(Params[P], ParamNames, ParamNameFound) of
            0: ServiceName := ParamData;
          end;
        end;  //with params[p] do
      try
        Open;
      except
        //Apparently, the service hasn't been installed; the menu items
        // will be disabled
      end;  //try..except
    end;  //with newserviceitem
  except
    on E: Exception do
    begin
      FreeAndNil(NewServiceItem);
      AbortParsing(E.Message);
      Exit;
    end;
  end;

  Services.Add(NewServiceItem);
end;

procedure TTMConfigReader.EnumVariables(const Line: PChar;
  const Ext: Longint);

  function GetRegVariable(RegRoot, RegKey, RegValue: String): String;
  var
    Reg: TRegistry;
  begin
    try
      Reg := TRegistry.Create(KEY_QUERY_VALUE);
      with Reg do
      begin
        //Set root
        if RegRoot = 'HKCR' then
          RootKey := HKEY_CLASSES_ROOT
        else if RegRoot = 'HKCU' then
          RootKey := HKEY_CURRENT_USER
        else if RegRoot = 'HKLM' then
          RootKey := HKEY_LOCAL_MACHINE
        else if RegRoot = 'HKU' then
          RootKey := HKEY_USERS
        else if RegRoot = 'HKCC' then
          RootKey := HKEY_CURRENT_CONFIG
        else
          AbortParsingFmt(SParsingInvalidRegRoot, [RegRoot]);

        //Open key
        try
          if not OpenKey(RegValue, False) then
            raise Exception.Create('Could not open registry key (OpenKey returned false)');
        except
          AbortParsingFmt(SParsingCouldNotOpenRegKey, [RegRoot, RegKey]);
        end;

        //Read value
        try
          case GetDataType(RegValue) of
            rdString, rdExpandString:
              Result := ReadString(RegValue);
            rdInteger:
              Result := IntToStr(ReadInteger(RegValue));
            else
              AbortParsingFmt(SParsingRegValueInvalidFormat,
                [RegRoot, RegKey, RegValue,
                 GetEnumName(TypeInfo(TRegDataType), Ord(GetDataType(RegValue)))]); { ? }
          end;  //case getdatatype(regvalue)
        except
          AbortParsingFmt(SParsingCouldNotReadRegValue, [RegRoot, RegKey, RegValue]);
        end;

      end;  //with reg
    finally
      FreeAndNil(Reg);
    end;
  end;

const
  ParamNames: array[0..11] of TParamInfo = (
    (Name: ParamVariablesName; Flags: [piNoEmpty]),     //0
    (Name: ParamVariablesType; Flags: []),              //1
    (Name: ParamVariablesFlags; Flags: []),
    (Name: ParamVariablesValue; Flags: []),
    (Name: ParamVariablesRegRoot; Flags: []),
    (Name: ParamVariablesRegKey; Flags: []),            //5
    (Name: ParamVariablesRegValue; Flags: []),
    (Name: ParamVariablesEnvName; Flags: []),
    (Name: ParamVariablesCmdLineParamName; Flags: []),
    (Name: ParamVariablesDefaultValue; Flags: []),
    (Name: ParamVariablesPromptCaption; Flags: []),     //10
    (Name: ParamVariablesPromptText; Flags: []));
var
  Params: TBreakStringArray;
  ParamNameFound: array[Low(ParamNames)..High(ParamNames)] of Boolean;
  NewVariableItem: TVariableBase;
  VarType: TVarType;
  RegRoot, RegKey, RegValue: String;
  EnvName: String;
  SwitchName: String;
  DefValue: String;
  P: Integer;
begin
  FillChar(ParamNameFound, SizeOf(ParamNameFound), 0);
  BreakString(Line, Params);

  VarType := vtStatic;
//  NewVariableItem := TVariable.Create;
  NewVariableItem := nil;
  try
    with NewVariableItem do
    begin
      for P := Low(Params) to High(Params) do
        with Params[P] do
        begin
          if ParamName = '' then
            Break;
          case CompareParamName(Params[P], ParamNames, ParamNameFound) of
            0: begin
                if not Assigned(NewVariableItem) then
                  AbortParsing(SParsingAssignVarTypeFirst);
                Name := ParamData;
              end;
            1: begin
                ParamData := UpperCase(Trim(ParamData));
                if ParamData = 'STATIC' then
                  VarType := vtStatic
                else if ParamData = 'REGISTRY' then
                  VarType := vtRegistry
                else if ParamData = 'ENVIRONMENT' then
                  VarType := vtEnvironment
                else if ParamData = 'COMMANDLINE' then
                  VarType := vtCmdLine
                else if ParamData = 'PROMPT' then
                  VarType := vtPrompt
                else
                  AbortParsingFmt(SParsingUnknownVarType, [ParamData]);
                if VarType = vtPrompt then
                  NewVariableItem := TPromptVariable.Create
                else
                  NewVariableItem := TVariable.Create;
              end;
            2: begin
                  if not Assigned(NewVariableItem) then
                    AbortParsing(SParsingAssignVarTypeFirst);
                  while True do
                  case ExtractFlag(ParamData, VariableFlags) of
                    -2: Break;
                    -1: AbortParsingFmt(SParsingParamUnknownFlag2, [ParamName]);
                    0: Flags := Flags + [vfIsPath];
                  end;
                end;
            3: begin
                if VarType <> vtStatic then
                  AbortParsingFmt(SParsingInvalidVarParam, [ParamName]);
                (NewVariableItem as TVariable).Value := ExpandVariables(ParamData, Variables);
              end;
            4: begin
                if VarType <> vtRegistry then
                  AbortParsingFmt(SParsingInvalidVarParam, [ParamName]);
                RegRoot := ParamData;
              end;
            5: begin
                if VarType <> vtRegistry then
                  AbortParsingFmt(SParsingInvalidVarParam, [ParamName]);
                RegKey := ExpandVariables(ParamData, Variables);
              end;
            6: begin
                if VarType <> vtRegistry then
                  AbortParsingFmt(SParsingInvalidVarParam, [ParamName]);
                RegValue := ExpandVariables(ParamData, Variables);
              end;
            7: begin
                if VarType <> vtEnvironment then
                  AbortParsingFmt(SParsingInvalidVarParam, [ParamName]);
                EnvName := ExpandVariables(ParamData, Variables);
              end;
            8: begin
                if VarType <> vtCmdLine then
                  AbortParsingFmt(SParsingInvalidVarParam, [ParamName]);
                SwitchName := ExpandVariables(ParamData, Variables);
              end;
            9: begin
                if not (VarType in [vtRegistry, vtEnvironment, vtCmdLine, vtPrompt]) then
                  AbortParsingFmt(SParsingInvalidVarParam, [ParamName]);
                DefValue := ExpandVariables(ParamData, Variables);
              end;
            10: begin
                if VarType <> vtPrompt then
                  AbortParsingFmt(SParsingInvalidVarParam, [ParamName]);
                (NewVariableItem as TPromptVariable).Caption := ExpandVariables(ParamData, Variables);
              end;
            11: begin
                if VarType <> vtPrompt then
                  AbortParsingFmt(SParsingInvalidVarParam, [ParamName]);
                (NewVariableItem as TPromptVariable).Text := ExpandVariables(ParamData, Variables);
              end;
          end;
        end;  //for p := low(params) to high do with params[p] do

      if VarType in [vtRegistry, vtEnvironment, vtCmdLine] then
        try
          case VarType of
            vtRegistry:
                Value := GetRegVariable(UpperCase(Trim(RegRoot)), RegKey, RegValue);
            vtEnvironment:
                Value := GetEnv(EnvName);
            vtCmdLine: begin
                Value := FindCmdSwitch('-' + SwitchName + '=');
                if Value = '' then
                  raise Exception.CreateFmt(SParsingCmdLineParameterNotFound, [SwitchName]);
              end;
          end;  //case vartype of
        except
          if DefValue <> '' then
            Value := DefValue
          else
            raise;
        end  //if vartype in [reg,env,cmdline] then try..except
      else if VarType = vtPrompt then
        (NewVariableItem as TPromptVariable).DefaultValue := DefValue;
    end;  //with newvariableitem
  except
    on E: Exception do
    begin
      FreeAndNil(NewVariableItem);
      AbortParsing(E.Message);
      Exit;
    end;
  end;
  Variables.Add(NewVariableItem)
end;

function TTMConfigReader.ISStrToBool(S: String): Boolean;
begin
  Result := False;
  S := Lowercase(S);
  if (S = '1') or (S = 'yes') or (S = 'true') or (S = 'on') then
    Result := True
  else if (S = '0') or (S = 'no') or (S = 'false') or (S = 'off') then
    Result := False
  else
    AbortParsing(SParsingParamInvalidBoolValue);
end;

procedure TTMConfigReader.ISStrToFont(const S: String; F: TFont);
const
  FontStyleFlags: array[0..3] of PChar = (
    'bold', 'italic', 'underline', 'strikeout');
var
  Params: TStrings;
  FontStyleParam: String;
  I: Integer;
begin
  Params := TStringList.Create;
  try
    try
      StrTokenToStrings(S, ',', Params);
      with F do
        for I := 0 to Params.Count - 1 do
          case I of
            0: Name := Params[I];
            1: Size := StrToInt(Params[I]);
            2: Color := StringToColor(Params[I]);
            3: begin
                 FontStyleParam := Params[I];
                 while True do
                   case ExtractFlag(FontStyleParam, FontStyleFlags) of
                     -2: Break;
                     -1: AbortParsing(SParsingUnknownFontStyle);
                     0: Style := Style + [fsBold];
                     1: Style := Style + [fsItalic];
                     2: Style := Style + [fsUnderline];
                     3: Style := Style + [fsStrikeOut];
                   end;
               end;
          end;  //case i
    except
      AbortParsing(SParsingInvalidFontSpec);
    end;
  finally
    FreeAndNil(Params);
  end;
end;

procedure TTMConfigReader.ReadBcBarPopupMenu(
  const BcBarPopupMenu: TBcBarPopupMenu; const SectionName: String);
{ This procedure reads popup menu settings from a section
  with name SectionName + MENUSETTINGSSUFFIX, and the menu
  from the section with name SectionName }
var
  I: TMenuSectionDirective;
begin
  ValidateProperties;

  { Initializations }
  for I := Low(I) to High(I) do
    MenuDirectiveLines[I] := 0;

  { Read the settings }
  EnumIniSection(EnumMenuSettings, SectionName + MENUSETTINGSSUFFIX,
      Longint(BcBarPopupMenu));
  BcBarPopupMenu.FlushDoubleBuffer;

  { Read the menu items }
  ReadMenu(BcBarPopupMenu.Items, SectionName);
end;

procedure TTMConfigReader.ReadMenu(const MenuItems: TMenuItem;
  const SectionName: String);
var
  SaveLineNumber: Integer;
begin
  ValidateProperties;

  { Read the menu items }
  SaveLineNumber := LineNumber;
  EnumIniSection(EnumMenuItems, SectionName, Longint(MenuItems));
  LineNumber := SaveLineNumber;
end;

procedure TTMConfigReader.ReadMultiAction(
  const MultiAction: TTMMultiAction; const SectionName: String);
var
  SaveLineNumber: Integer;
begin
  ValidateProperties;

  { Read the action items }
  SaveLineNumber := LineNumber;
  EnumIniSection(EnumMultiAction, SectionName, Longint(MultiAction));
  LineNumber := SaveLineNumber;
end;

procedure TTMConfigReader.ReadSettings;
var
  I: TConfigSectionDirective;
  J: TMessagesSectionDirective;
begin
  ValidateProperties;

  { Validation }
  if (not Assigned(FImageList)) or (not Assigned(FServices)) or
       (not Assigned(FTrayIcon)) or (not Assigned(FCheckServicesTimer)) then
    raise Exception.Create(SReaderHasntBeenInitializedYet);

  { Initializations }
  for I := Low(I) to High(I) do
    ConfigDirectiveLines[I] := 0;
  for J := Low(J) to High(J) do
    MessagesDirectiveLines[J] := 0;

  { Read the settings }
  EnumIniSection(EnumVariables, 'Variables', 0);
  EnumIniSection(EnumAboutText, 'AboutText', 0);
  EnumIniSection(EnumServices, 'Services', 0);
  EnumIniSection(EnumConfig, 'Config', 0);
  //Validate [Config] settings
  if (ConfigDirectiveLines[csTrayIcon] = 0) then
    if (ConfigDirectiveLines[csTrayIconAllRunning] = 0) and
         (ConfigDirectiveLines[csTrayIconSomeRunning] = 0) and
         (ConfigDirectiveLines[csTrayIconNoneRunning] = 0) then
      AbortParsing(SValNoTrayIconAssigned)
    else
      if (ConfigDirectiveLines[csTrayIconAllRunning] = 0) or
           (ConfigDirectiveLines[csTrayIconSomeRunning] = 0) or
           (ConfigDirectiveLines[csTrayIconNoneRunning] = 0) then
        AbortParsing(SValMustSpecifyThreeStateIcons);

  EnumIniSection(EnumMessages, 'Messages', 0);

  ReadMultiAction(DoubleClickAction, 'DoubleClickAction');
  ReadMultiAction(LeftClickAction, 'LeftClickAction');
  ReadMultiAction(RightClickAction, 'RightClickAction');
  ReadMultiAction(StartupAction, 'StartupAction');
end;

procedure TTMConfigReader.SetCustomAboutText(const Value: TStrings);
begin
  FCustomAboutText.Assign(Value);
end;

procedure TTMConfigReader.SetScript(const Value: TStringList);
begin
  FScript.Assign(Value);
end;

procedure TTMConfigReader.ValidateProperties;
begin
  if (not Assigned(CheckServicesTimer)) or
     (not Assigned(DoubleClickAction)) or
     (not Assigned(LeftClickAction)) or
     (not Assigned(RightClickAction)) or
     (not Assigned(ImageList)) or
     (not Assigned(OnBuiltInActionExecute)) or
     (not Assigned(OnSelectMenuItem)) or
     (not Assigned(Services)) or
     (not Assigned(TrayIcon)) or
     (not Assigned(Variables)) then
    raise Exception.Create(SParsingValidatePropertiesFailed);
end;

end.
