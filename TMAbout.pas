unit TMAbout;

{
  Aestan Tray Menu
  Made by Onno Broekmans; visit http://www.xs4all.nl/~broekroo/aetraymenu
  for more information.

  This work is hereby released into the Public Domain. To view a copy of the
  public domain dedication, visit:
      http://creativecommons.org/licenses/publicdomain/
  or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford,
  California 94305, USA.

  This is the about dialog for AeTrayMenu.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvGradient, StdCtrls, ExtCtrls, JvExStdCtrls, JvRichEdit, ComCtrls,
  JvStringHolder, JvExControls;

type
  TAboutDiag = class(TForm)
    VersionLabel: TLabel;
    CloseBtn: TButton;
    AboutText: TJvRichEdit;
    AboutHeader: TLabel;
    BottomLine: TBevel;
    OtherAboutLabel: TLabel;
    AboutAeTrayMenu: TJvStrHolder;
    JvGradient2: TJvGradient;
    procedure AboutTextsURLClick(Sender: TObject; const URLText: String;
      Button: TMouseButton);
    procedure OtherAboutLabelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FCustomAboutVersion: String;
    FCustomAboutHeader: String;
    FCustomAboutText: TStrings;
    InternalVersionText: String;
    FShowingCustom: Boolean;
    procedure SetCustomAboutText(const Value: TStrings);
    procedure SetShowingCustom(const Value: Boolean);
  public
    property CustomAboutHeader: String read FCustomAboutHeader write FCustomAboutHeader;
    property CustomAboutVersion: String read FCustomAboutVersion write FCustomAboutVersion;
    property CustomAboutText: TStrings read FCustomAboutText write SetCustomAboutText;
    property ShowingCustom: Boolean read FShowingCustom write SetShowingCustom;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SwitchAboutBox(Custom: Boolean);
  end;

var
  AboutDiag: TAboutDiag;

implementation

uses ShellApi, JclFileUtils;

{$R *.dfm}
{$I AeTrayMenu.lc}

procedure TAboutDiag.AboutTextsURLClick(Sender: TObject;
  const URLText: String; Button: TMouseButton);
begin
  ShellExecute(0, nil, PChar(URLText), nil, nil, SW_SHOW);
end;

constructor TAboutDiag.Create(AOwner: TComponent);
var
  VersionInfo: TJclFileVersionInfo;
begin
  inherited;

  FCustomAboutText := TStringList.Create;

  try
    VersionInfo := TJclFileVersionInfo.Create(ParamStr(0));
    try
      InternalVersionText := 'Version ' + VersionInfo.FileVersion + #13#10 +
                              'Built on ' + __COMPILE_DATETIME_AS_STRING__;
    finally
      FreeAndNil(VersionInfo);
    end;  //try..finally
  except
    //Ignore exceptions
  end;
end;

destructor TAboutDiag.Destroy;
begin
  FreeAndNil(FCustomAboutText);

  inherited;
end;

procedure TAboutDiag.OtherAboutLabelClick(Sender: TObject);
begin
  ShowingCustom := not ShowingCustom;
end;

procedure TAboutDiag.SetCustomAboutText(const Value: TStrings);
begin
  FCustomAboutText.Assign(Value);
end;

procedure TAboutDiag.SwitchAboutBox(Custom: Boolean);
begin
  if Custom then
  begin
    AboutHeader.Caption := ' ' + CustomAboutHeader;
    VersionLabel.Caption := CustomAboutVersion;
    AboutText.Lines.Assign(CustomAboutText);
    OtherAboutLabel.Caption := 'About Aestan Tray Menu';
  end
  else  //custom
  begin
    AboutHeader.Caption := ' Aestan Tray Menu';
    VersionLabel.Caption := InternalVersionText;
    AboutText.Lines.Assign(AboutAeTrayMenu.Strings);
    OtherAboutLabel.Caption := 'About ' + CustomAboutHeader;
  end;  //else custom
end;

procedure TAboutDiag.FormShow(Sender: TObject);
begin
  ShowingCustom := (CustomAboutHeader <> '');
  SwitchAboutBox(ShowingCustom);
  OtherAboutLabel.Visible := ShowingCustom;
end;

procedure TAboutDiag.SetShowingCustom(const Value: Boolean);
begin
  FShowingCustom := Value;
  SwitchAboutBox(Value);
end;

end.
