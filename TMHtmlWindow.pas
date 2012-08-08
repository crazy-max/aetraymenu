unit TMHtmlWindow;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, OleCtrls, SHDocVw, StdCtrls, JvGradient, ExtCtrls, TMStruct,
  JvExControls;

type
  THtmlWindow = class(TForm)
    TopPanel: TPanel;
    AboutHeader: TLabel;
    JvGradient2: TJvGradient;
    BottomPanel: TPanel;
    CloseBtn: TButton;
    WebBrowser: TWebBrowser;
    Bevel1: TBevel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CloseBtnClick(Sender: TObject);
    procedure WebBrowserBeforeNavigate2(Sender: TObject;
      const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
      Headers: OleVariant; var Cancel: WordBool);
    procedure WebBrowserDocumentComplete(Sender: TObject;
      const pDisp: IDispatch; var URL: OleVariant);
    procedure FormShow(Sender: TObject);
    procedure WebBrowserTitleChange(Sender: TObject;
      const Text: WideString);
  private
    FSrc: String;
    FHeader: String;
    FHtmlActions: TStringList;
    FHtmlWindowFlags: THtmlWindowFlags;
    procedure SetSrc(const Value: String);
    procedure SetHeader(const Value: String);
    procedure SetHtmlWindowFlags(const Value: THtmlWindowFlags);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    property Header: String read FHeader write SetHeader;
    property Src: String read FSrc write SetSrc;
    property HtmlActions: TStringList read FHtmlActions write FHtmlActions;
    property HtmlWindowFlags: THtmlWindowFlags read FHtmlWindowFlags write SetHtmlWindowFlags;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetPosAndDimensions(ALeft, ATop, AWidth, AHeight: Integer);
  end;

var
  HtmlWindow: THtmlWindow;

implementation

uses
  TMMsgs, JclStrings, TMMain;

{$R *.dfm}

var
  HookID: THandle;
  MouseHookEnabled: Boolean;

function MouseProc(nCode: Integer; wParam, lParam: Longint): Longint; stdcall;
var
  szClassName: array[0..255] of Char;
const
  ie_name = 'Internet Explorer_Server';
begin
  case nCode < 0 of
    True:
      Result := CallNextHookEx(HookID, nCode, wParam, lParam)
      else
        case wParam of
          WM_RBUTTONDOWN,
          WM_RBUTTONUP:
            begin
              GetClassName(PMOUSEHOOKSTRUCT(lParam)^.HWND, szClassName, SizeOf(szClassName));
              if (lstrcmp(@szClassName[0], @ie_name[1]) = 0) and MouseHookEnabled then
                Result := HC_SKIP
              else
                Result := CallNextHookEx(HookID, nCode, wParam, lParam);
            end
            else
              Result := CallNextHookEx(HookID, nCode, wParam, lParam);
        end;
  end;
end;

{ THtmlWindow }

procedure THtmlWindow.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

constructor THtmlWindow.Create(AOwner: TComponent);
begin
  inherited;

  { Hook the window to prevent the TWebBrowser popup menu }
  HookID := SetWindowsHookEx(WH_MOUSE, MouseProc, 0, GetCurrentThreadId());
end;

procedure THtmlWindow.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if hwfAlwaysOnTop in HtmlWindowFlags then
    Params.WndParent := GetDesktopWindow
  else
    Params.WndParent := Application.Handle;
end;

destructor THtmlWindow.Destroy;
begin
  { Unhook the window }
  if HookID <> 0 then
    UnHookWindowsHookEx(HookID);

  { Other de-inits }
  HtmlWindow := nil;

  inherited;
end;

procedure THtmlWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure THtmlWindow.SetPosAndDimensions(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if AWidth > 0 then
    ClientWidth := AWidth;
  if AHeight > 0 then
  begin
    ClientHeight := AHeight;
    if BottomPanel.Visible then
      ClientHeight := ClientHeight + BottomPanel.Height;
    if TopPanel.Visible then
      ClientHeight := ClientHeight + TopPanel.Height;
  end;
  if ALeft >= 0 then
    Left := ALeft
  else if ALeft < 0 then
    Left := Screen.Width + ALeft;
  if ATop >= 0 then
    Top := ATop
  else if ATop < 0 then
    Top := Screen.Height + ATop;
end;

procedure THtmlWindow.SetHeader(const Value: String);
begin
  FHeader := Value;
  AboutHeader.Caption := ' ' + Value;
end;

procedure THtmlWindow.SetHtmlWindowFlags(const Value: THtmlWindowFlags);
begin
  FHtmlWindowFlags := Value;

  { Apply the flags }
  if hwfMaximized in HtmlWindowFlags then
    WindowState := wsMaximized
  else
    WindowState := wsNormal;
  if hwfNoResize in HtmlWindowFlags then
    BorderStyle := bsDialog
  else
    BorderStyle := bsSizeable;
  MouseHookEnabled := not (hwfEnableContextMenu in HtmlWindowFlags);
  BottomPanel.Visible := not (hwfNoCloseButton in HtmlWindowFlags);
  TopPanel.Visible := not (hwfNoHeader in HtmlWindowFlags);
  if hwfAlwaysOnTop in HtmlWindowFlags then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
//    SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0,
//        SWP_NOACTIVATE + SWP_NOSIZE + SWP_NOMOVE)
//  else
//    SetWindowPos(Handle, HWND_NOTOPMOST, 0, 0, 0, 0,
//        SWP_NOACTIVATE + SWP_NOSIZE + SWP_NOMOVE);
end;

procedure THtmlWindow.SetSrc(const Value: String);
begin
  FSrc := Value;
  WebBrowser.Navigate(Value);
end;

procedure THtmlWindow.WebBrowserBeforeNavigate2(Sender: TObject;
  const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
  Headers: OleVariant; var Cancel: WordBool);
var
  I: Integer;
begin
  { Intercept and handle action URLs }
  if StrIPos('action:', Trim(URL)) = 1 then
  begin
    Cancel := True;
    if HtmlActions.Find( Trim(StrAfter('action:', Trim(URL))), I) then
      (HtmlActions.Objects[I] as TTMMultiAction).ExecuteAction
    else
      raise Exception.CreateFmt(SHtmlUnknownAction, [Trim(StrAfter('action:', Trim(URL)))]);
  end;
  if SameText('close:', Trim(URL)) then
  begin
    Cancel := True;
    Close;
  end;
end;

procedure THtmlWindow.WebBrowserDocumentComplete(Sender: TObject;
  const pDisp: IDispatch; var URL: OleVariant);
begin
  { Apply some flags }
  if hwfNoScrollbars in HtmlWindowFlags then
    if not VarIsEmpty(WebBrowser.OleObject.Document) then
    begin
      WebBrowser.OleObject.Document.Body.Style.OverflowX := 'hidden';
      WebBrowser.OleObject.Document.Body.Style.OverflowY := 'hidden';
    end;
end;

procedure THtmlWindow.FormShow(Sender: TObject);
begin
  if hwfAlwaysOnTop in HtmlWindowFlags then
    ShowWindow(Application.Handle, SW_HIDE);
      //Prevent application taskbar button from showing up
end;

procedure THtmlWindow.WebBrowserTitleChange(Sender: TObject;
  const Text: WideString);
begin
  Caption := Text;
end;

initialization
  MouseHookEnabled := True;
end.
