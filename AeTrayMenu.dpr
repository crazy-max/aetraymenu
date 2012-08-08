program AeTrayMenu;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  SysUtils,
  Windows,
  TMMain in 'TMMain.pas' {MainForm},
  TMAbout in 'TMAbout.pas' {AboutDiag},
  TMConfig in 'TMConfig.pas',
  TMStruct in 'TMStruct.pas',
  TMCmnFunc in 'TMCmnFunc.pas',
  TMMsgs in 'TMMsgs.pas',
  TMHtmlWindow in 'TMHtmlWindow.pas' {HtmlWindow},
  TMSrvCtrl in 'TMSrvCtrl.pas';

{$R *.res}

begin
  { Do some command-line parameter handling first }
  if FindCmdLineSwitch('quit', ['-'], True) then
  begin
    KillInstance(FindCmdSwitch('-id='));
    Exit;
  end;

  { Start it up! }
  Application.Initialize;
  Application.MainFormOnTaskbar := False;
  Application.Title := 'Aestan Tray Menu';
  Application.CreateForm(TMainForm, MainForm);
  Application.ShowMainForm := False;
  ShowWindow(Application.Handle, SW_HIDE);
  Application.Run;
end.
