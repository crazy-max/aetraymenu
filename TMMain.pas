unit TMMain;

{
  Aestan Tray Menu
  Made by Onno Broekmans; visit http://www.xs4all.nl/~broekroo/aetraymenu
  for more information.

  This work is hereby released into the Public Domain. To view a copy of the
  public domain dedication, visit:
      http://creativecommons.org/licenses/publicdomain/
  or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford,
  California 94305, USA.

  This is the main unit of AeTrayMenu.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, BarMenus, JvComponent, JvTrayIcon, ExtCtrls,
  Contnrs, ImgList,
  TMStruct, TMSrvCtrl, JvComponentBase;

type
  TMainForm = class(TForm)
    LeftClickPopup: TBcBarPopupMenu;
    RightClickPopup: TBcBarPopupMenu;
    TrayIcon: TJvTrayIcon;
    CheckServicesTimer: TTimer;
    ImageList: TImageList;
    procedure CheckServicesTimerTimer(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BuiltInActionExecute(Sender: TObject);
    procedure SelectMenuItem(Sender: TObject);
    procedure LeftRightClickPopupPopup(Sender: TObject);
  private
    { FIELDS }
    FServices: TObjectList;
    FVariables: TObjectList;
    FDoubleClickAction: TTMMultiAction;
    FStartupAction: TTMMultiAction;
    FTrayIconSomeRunning: Integer;
    FServiceGlyphStopped: Integer;
    FServiceGlyphRunning: Integer;
    FTrayIconNoneRunning: Integer;
    FServiceGlyphPaused: Integer;
    FTrayIconAllRunning: Integer;
    FSomeRunningHint: String;
    FAllRunningHint: String;
    FNoneRunningHint: String;
    FID: String;
    FCustomAboutVersion: String;
    FCustomAboutHeader: String;
    FCustomAboutText: TStrings;
    FHtmlActions: TStringList;

    { METHODS }
    procedure SetServices(const Value: TObjectList);
    procedure SetVariables(const Value: TObjectList);
    procedure SetCustomAboutText(const Value: TStrings);
    procedure SetHtmlAction(const Value: TStringList);
  protected
    { FIELDS }
    mutexHandle: THandle;
  public
    { PROPERTIES }
    property Services: TObjectList read FServices write SetServices;
    property Variables: TObjectList read FVariables write SetVariables;
    property HtmlActions: TStringList read FHtmlActions write SetHtmlAction;
    property DoubleClickAction: TTMMultiAction read FDoubleClickAction;
    property StartupAction: TTMMultiAction read FStartupAction;
    property TrayIconAllRunning: Integer read FTrayIconAllRunning write FTrayIconAllRunning;
    property TrayIconSomeRunning: Integer read FTrayIconSomeRunning write FTrayIconSomeRunning;
    property TrayIconNoneRunning: Integer read FTrayIconNoneRunning write FTrayIconNoneRunning;
    property AllRunningHint: String read FAllRunningHint write FAllRunningHint;
    property SomeRunningHint: String read FSomeRunningHint write FSomeRunningHint;
    property NoneRunningHint: String read FNoneRunningHint write FNoneRunningHint;
    property ServiceGlyphRunning: Integer read FServiceGlyphRunning write FServiceGlyphRunning;
    property ServiceGlyphPaused: Integer read FServiceGlyphPaused write FServiceGlyphPaused;
    property ServiceGlyphStopped: Integer read FServiceGlyphStopped write FServiceGlyphStopped;
    property ID: String read FID write FID;
    property CustomAboutHeader: String read FCustomAboutHeader write FCustomAboutHeader;
    property CustomAboutVersion: String read FCustomAboutVersion write FCustomAboutVersion;
    property CustomAboutText: TStrings read FCustomAboutText write SetCustomAboutText;

    { METHODS }
    procedure ClearMenus;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExceptionMsgBox(Msg: String; E: Exception);
    procedure LoadBuiltInVariables;
    procedure ReadConfig;
      { Reads and applies the configuration file }
  end;

var
  MainForm: TMainForm;

implementation

uses JclFileUtils, JclStrings,
     TMAbout, TMConfig, TMMsgs, TMCmnFunc;

{$R *.dfm}

procedure TMainForm.BuiltInActionExecute(Sender: TObject);
var
  AboutDialog: TAboutDiag;
  ErrorCode: Integer;
  I: Integer;
begin
  case (Sender as TTMBuiltInAction).BuiltInAction of
    biaAbout: begin
        AboutDialog := TAboutDiag.Create(Self);
        with AboutDialog do
        begin
          CustomAboutHeader := Self.CustomAboutHeader;
          CustomAboutVersion := Self.CustomAboutVersion;
          CustomAboutText := Self.CustomAboutText;
          ShowModal;
        end;  //with aboutdialog
        FreeAndNil(AboutDialog);
      end;
    biaExit:
      Application.Terminate;
    biaReadConfig:
      ReadConfig;
    biaControlPanelServices: begin
        if not InstShellExec(ExpandVariables('%System%\services.msc', Variables),
            '/s', '', '', SW_SHOWNORMAL, ErrorCode) then
          raise Exception.Create('Could not open Services applet');
            { DONE 3 : Improve ControlPanelServices built-in action }
      end;
    biaCloseServices: begin
        for I := 0 to Services.Count - 1 do
          (Services[I] as TTMService).Close;
      end;
    biaResetServices: begin
        for I := 0 to Services.Count - 1 do
          with (Services[I] as TTMService) do
            try
              Open; //if it's already open, it will be closed first
            except
              //Apparently, the service hasn't been installed; the menu items
              // will be disabled
            end;  //for i with services[i] try..except
      end;
    else
      ShowMessage(SMainBuiltInNotImplemented);
      { DONE 4 -cMissing feaures : Implement all built-in actions }
  end;
end;

procedure TMainForm.CheckServicesTimerTimer(Sender: TObject);
var
  I, runningCount: Integer;
  HintText: String;
begin
  { Check if the configured services are still up & running }
  { DONE 3 : Implement TMainForm.CheckServicesTimerTimer }
  runningCount := 0;
  for I := 0 to Services.Count - 1 do
    with (Services[I] as TTMService) do
    begin
      if Active then
        if State = svsRunning then 
          Inc(runningCount);
    end;  //for i with services[i] as ttmservice do

  if runningCount = 0 then
  begin
    HintText := NoneRunningHint;
    if TrayIconNoneRunning <> -1 then
      TrayIcon.IconIndex := TrayIconNoneRunning;
  end
  else if runningCount < Services.Count then
  begin
    HintText := SomeRunningHint;
    if TrayIconSomeRunning <> -1 then
      TrayIcon.IconIndex := TrayIconSomeRunning;
  end
  else
  begin
    HintText := AllRunningHint;
    if TrayIconAllRunning <> -1 then
      TrayIcon.IconIndex := TrayIconAllRunning;
  end;
  StrReplace(HintText, '%n', IntToStr(runningCount), [rfIgnoreCase, rfReplaceAll]);
  StrReplace(HintText, '%t', IntToStr(Services.Count), [rfIgnoreCase, rfReplaceAll]);
  TrayIcon.Hint := ExpandVariables(HintText, Variables);
end;

procedure TMainForm.ClearMenus;

  procedure ClearItems(Item: TMenuItem);
  var
    I: Integer;
  begin
    for I := 0 to Item.Count - 1 do
    begin
      if Item.Items[0].Count > 0 then
        ClearItems(Item.Items[0])
      else
        if Item.Items[0].Tag <> 0 then
          TTMAction(Item.Items[0].Tag).Free;
      Item.Items[0].Free;
    end;  //for i
  end;

begin
  ClearItems(LeftClickPopup.Items);
  ClearItems(RightClickPopup.Items);
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;

  { Memory inits }
  mutexHandle := 0;
  FServices := TObjectList.Create(True);
  FVariables := TObjectList.Create(True);
  FHtmlActions := TStringList.Create;
  FDoubleClickAction := TTMMultiAction.Create;
  FStartupAction := TTMMultiAction.Create;
  FCustomAboutText := TStringList.Create;

  { General inits }
  with HtmlActions do
  begin
    CaseSensitive := False;
    Sorted := True;
  end;  //with

  { Read and apply the configuration file }
  ReadConfig;

  { Run the startup action }
  StartupAction.ExecuteAction;
end;

destructor TMainForm.Destroy;
var
  J: Integer;
begin
  { Stop the timer }
  CheckServicesTimer.Enabled := False;

  { Clear menus }
  ClearMenus;

  { Memory cleanup }
  FreeAndNil(FCustomAboutText);
  FreeAndNil(FDoubleClickAction);
  FreeAndNil(FStartupAction);
  for J := 0 to (HtmlActions.Count - 1) do
    (HtmlActions.Objects[J] as TTMAction).Free;
  FreeAndNil(FHtmlActions);
  FreeAndNil(FVariables);
  FreeAndNil(FServices);

  inherited;
end;

procedure TMainForm.ExceptionMsgBox(Msg: String; E: Exception);
begin
  Application.MessageBox(PChar(Msg + #13#10 + '[' + E.ClassName + '] ' +
      E.Message), 'Aestan Tray Menu', MB_OK + MB_ICONERROR);
end;

procedure TMainForm.LeftRightClickPopupPopup(Sender: TObject);

  procedure EnableItems(Item: TMenuItem);
  var
    I: Integer;
  begin
    for I := 0 to Item.Count - 1 do
      with Item.Items[I] do
        if Count > 0 then
        begin
          { DONE 3 : Use correct imageindex depending on service status }
          if Tag <> 0 then
            with TTMService(Tag) do
            begin
              if Active then
              begin
                if Caption = '' then
                  Caption := DisplayName;
                case State of
                  svsRunning:
                      ImageIndex := ServiceGlyphRunning;
                  svsPaused:
                      ImageIndex := ServiceGlyphPaused;
                  svsStopped:
                      ImageIndex := ServiceGlyphStopped;
                  else
                      ImageIndex := -1;
                end;  //case state
              end;  //if active
            end;  //if tag <> 0 then with ttmservice(tag) do
          EnableItems(Item.Items[I]);
        end
        else
          if Tag <> 0 then
            Enabled := TTMAction(Tag).CanExecute;
  end;

begin
  { DONE 2 : Enable/disable service control menu items in LeftRightClickPopupPopup }
  EnableItems((Sender as TBcBarPopupMenu).Items);
end;

procedure TMainForm.LoadBuiltInVariables;

  procedure AddVar(AName, AValue: String; IsPath: Boolean = True);
  var
    NewVar: TVariable;
  begin
    NewVar := TVariable.Create;
    with NewVar do
    begin
      Name := AName;
      Value := AValue;
      if IsPath then
        Flags := [vfIsPath];
    end;  //with newvar
    Variables.Add(NewVar);
  end;

begin
  { DONE 3 -cMissing feaures : Automatically add built-in variables }
  AddVar('AeTrayMenuPath', ExtractFilePath(Application.ExeName));
  AddVar('Windows', GetWinDir);
  AddVar('System', GetSystemDir);
  AddVar('SysDrive', GetSystemDrive);
  AddVar('ProgramFiles', GetProgramFiles);
  AddVar('CommonFiles', GetCommonFiles);
  AddVar('Cmd', GetCmdFileName, False);
  AddVar('Temp', GetTempDir);
end;

procedure TMainForm.ReadConfig;
var
  ConfigReader: TTMConfigReader;
  configFile: String;
begin
  { Disable CheckServicesTimer }
  CheckServicesTimer.Enabled := False;

  ConfigReader := TTMConfigReader.Create;
  with ConfigReader do
    try
      { Clear the menus, services etc. }
      ClearMenus;
      FDoubleClickAction.Clear;
      FStartupAction.Clear;
      FServices.Clear;
      FVariables.Clear;

      { Initialize built-in variables }
      LoadBuiltInVariables;

      { Initialize the configuration reader }
      CheckServicesTimer := Self.CheckServicesTimer;
      DoubleClickAction := Self.DoubleClickAction;
      StartupAction := Self.StartupAction;
      ImageList := Self.ImageList;
      OnBuiltInActionExecute := BuiltInActionExecute;
      OnSelectMenuItem := SelectMenuItem;
      Services := Self.Services;
      TrayIcon := Self.TrayIcon;
      Variables := Self.Variables;
      HtmlActions := Self.HtmlActions;

      { Load the configuration file }
      try
        configFile := FindCmdSwitch('-scriptfile=');
        if configFile <> '' then
        begin
          //Load file specified on the command line
          if PathIsAbsolute(configFile) then
            Script.LoadFromFile(configFile)
          else
            Script.LoadFromFile(PathAppend(ExtractFileDir(Application.ExeName), configFile));
        end
        else
          //The configuration file has the same file & path as this executable,
          // but has extension .ini instead of .exe
          Script.LoadFromFile(PathRemoveExtension(Application.ExeName) + '.ini');
      except
        on E: Exception do
        begin
          ExceptionMsgBox(SMainCouldNotLoadConfig, E);
          Application.Terminate;
          Exit;
        end;
      end;

      { Read the configuration file }
      try
        ReadSettings;
        ReadBcBarPopupMenu(LeftClickPopup, 'Menu.Left');
        ReadBcBarPopupMenu(RightClickPopup, 'Menu.Right');
      except
        on E: EParseError do
        begin
          ExceptionMsgBox(Format(SReaderSyntaxError, [E.LineNumber]), E);
          Application.Terminate;
          Exit;
        end;
      end;

      Self.TrayIconAllRunning := TrayIconAllRunning;
      Self.TrayIconSomeRunning := TrayIconSomeRunning;
      Self.TrayIconNoneRunning := TrayIconNoneRunning;
      Self.AllRunningHint := AllRunningHint;
      Self.SomeRunningHint := SomeRunningHint;
      Self.NoneRunningHint := NoneRunningHint;
      Self.ServiceGlyphRunning := ServiceGlyphRunning;
      Self.ServiceGlyphPaused := ServiceGlyphPaused;
      Self.ServiceGlyphStopped := ServiceGlyphStopped;
      Self.ID := ID;
      Self.CustomAboutHeader := CustomAboutHeader;
      Self.CustomAboutVersion := CustomAboutVersion;
      Self.CustomAboutText := CustomAboutText;

      { DONE 3 : Read && apply all the other settings, too }
    finally
      FreeAndNil(ConfigReader);
    end;  //with configreader try..finally

  { Handle the ID stuff }
  //Try creating a mutex
  if mutexHandle = 0 then
  begin
    mutexHandle := CreateMutex(nil, True, PChar(ID));
    if GetLastError = ERROR_ALREADY_EXISTS then
    begin
      Application.Terminate;
      Exit;
    end;
    //Set the form caption
    Caption := 'AeTrayMenu[' + ID + ']';
  end;  //if mutexhandle = nil

  { Show the tray icon }
  TrayIcon.Active := True;

  { Start the timer }
  if CheckServicesTimer.Interval > 0 then
  begin
    CheckServicesTimer.Enabled := True;
    CheckServicesTimerTimer(Self);
  end;
end;

procedure TMainForm.SelectMenuItem(Sender: TObject);
begin
  if Sender is TMenuItem then
    if (Sender as TMenuItem).Count = 0 then
        //submenus cannot have actions linked to them
      with Sender as TMenuItem do
      begin
        if Tag <> 0 then
          try
            TTMAction(Tag).ExecuteAction;
          except
            on E: Exception do
              ExceptionMsgBox(SMainCouldNotExecuteMenuItem, E);
          end;
      end;  //if sender is tmenuitem then with sender as tmenuitem
end;

procedure TMainForm.SetCustomAboutText(const Value: TStrings);
begin
  FCustomAboutText.Assign(Value);
end;

procedure TMainForm.SetHtmlAction(const Value: TStringList);
begin
  FHtmlActions.Assign(Value);
end;

procedure TMainForm.SetServices(const Value: TObjectList);
begin
  FServices.Assign(Value);
end;

procedure TMainForm.SetVariables(const Value: TObjectList);
begin
  FVariables.Assign(Value);
end;

procedure TMainForm.TrayIconDblClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  { Respond to double-clicks }
  DoubleClickAction.ExecuteAction;
end;

end.
