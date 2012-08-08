unit TMSrvCtrl;

{
  Aestan Tray Menu
  Made by Onno Broekmans; visit http://www.xs4all.nl/~broekroo/aetraymenu
  for more information.

  This work is hereby released into the Public Domain. To view a copy of the
  public domain dedication, visit:
      http://creativecommons.org/licenses/publicdomain/
  or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford,
  California 94305, USA.

  This unit contains the TTMService class, which can be used to control
  Windows services.
}

{
  NOTE:
  This unit is a replacement of the TSrvCtrl component, which was originally
  used by AeTrayMenu. However, this component had a lot of features ATM
  didn't need, and most of all, TSrvCtrl was only for non-commercial use.
  Some of this code has been inspired by the TSrvCtrl component, though,
  and therefore I'd like to thank the original authors:
  * GSC (www.gsc.hu)
  * TMEDIA (www.tmedia.de)
  >>PLEASE DO NOT REMOVE THIS NOTE<<
}

interface

uses
  Windows, SysUtils, Classes, WinSvc;

type
  { Indicates the status of the service }
  TServiceState = (svsStopped = $00000001, svsStartPending, svsStopPending,
      svsRunning, svsContinuePending, svsPausePending, svsPaused);
  TServiceControl = (svcStop, svcPauseContinue);
  TServiceControls = set of TServiceControl;

  EServiceException = class(Exception)
  public
    constructor Create(AErrorCode: Integer); overload;
  end;

  { This class can control Windows services.
    Note: if you make calls to methods or use properties that need a handle to
    the service (in other words: that need TTMService to be activated),
    while TTMService hasn't been activated (yet), an exception will be raised. }
  TTMService = class
  private
    FServiceName: String;
    FServiceHandle: SC_HANDLE;
    FSCManagerHandle: SC_HANDLE;
    procedure SetServiceName(const Value: String);
    function GetDisplayName: String;
    function GetState: TServiceState;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function GetControlsAccepted: TServiceControls;
  protected
    property SCManagerHandle: SC_HANDLE read FSCManagerHandle;
    property ServiceHandle: SC_HANDLE read FServiceHandle;

    procedure ControlService(ControlCode: DWORD); virtual;
    procedure GetServiceConfig(var Config: TQueryServiceConfig);
    procedure GetServiceStatus(var Status: TServiceStatus);
    procedure VerifyActive;
  public
    { Read to determine whether TTMService has an active/open handle to
      the service.
      Setting Active to true does the same as calling Open; setting to false
      is equivalent to calling Close. }
    property Active: Boolean read GetActive write SetActive;
    { Can be used to see if the service accepts a specific control code/action }
    property ControlsAccepted: TServiceControls read GetControlsAccepted;
    { Can be used to retrieve the "display name" of the service }
    property DisplayName: String read GetDisplayName;
    { Can be used to retrieve the status of the service }
    property State: TServiceState read GetState;
    { The name of the service (*not* the display name). If TTMService has
      already been activated/opened, it will first disconnect and then
      store the property. You always have to activate the TTMService yourself,
      since this will *not* be done for you automatically. }
    property ServiceName: String read FServiceName write SetServiceName;

    { Opens a handle to the specified service. If TTMService has already been
      activated, Close is called first, and then the handle is opened. }
    procedure Open; virtual;
    { Closes the handle to the service. If TTMService is inactive, nothing
      is done. }
    procedure Close; virtual;

    { Starts the service. }
    procedure Start; virtual;
    { Continues a paused service.}
    procedure Continue; virtual;
    { Stops the service. }
    procedure Stop; virtual;
    { Suspends/pauses the service. }
    procedure Pause; virtual;

    constructor Create; virtual;
    destructor Destroy; override;
  end;  //ttmservice

implementation

uses
  Forms;

resourcestring
  SNoServiceOpen = 'No service has been opened yet!';

{ EServiceException }

constructor EServiceException.Create(AErrorCode: Integer);
begin
  inherited Create(SysErrorMessage(AErrorCode));
end;

{ TTMService }

procedure TTMService.Close;
begin
  VerifyActive;

  if not CloseServiceHandle(ServiceHandle) then
    raise EServiceException.Create(GetLastError);
  FServiceHandle := 0;

  if not CloseServiceHandle(SCManagerHandle) then
    raise EServiceException.Create(GetLastError);
  FSCManagerHandle := 0;
end;

procedure TTMService.Continue;
begin
  ControlService(SERVICE_CONTROL_CONTINUE);
end;

procedure TTMService.ControlService(ControlCode: Cardinal);
var
  ServiceStatus: TServiceStatus;
begin
  VerifyActive;

  if not WinSvc.ControlService(ServiceHandle, ControlCode, ServiceStatus) then
    raise EServiceException.Create(GetLastError);
end;

constructor TTMService.Create;
begin
  inherited Create;
end;

destructor TTMService.Destroy;
begin
  if Active then
    Close;

  inherited Destroy;
end;

function TTMService.GetActive: Boolean;
begin
  Result := (SCManagerHandle <> 0) and (ServiceHandle <> 0);
end;

function TTMService.GetControlsAccepted: TServiceControls;
var
  Status: TServiceStatus;
begin
  GetServiceStatus(Status);
  with Status do
  begin
    if (dwControlsAccepted and SERVICE_ACCEPT_STOP) = SERVICE_ACCEPT_STOP then
      Include(Result, svcStop);
    if (dwControlsAccepted and SERVICE_ACCEPT_PAUSE_CONTINUE) = SERVICE_ACCEPT_PAUSE_CONTINUE then
      Include(Result, svcPauseContinue);
  end;  //with config
end;

function TTMService.GetDisplayName: String;
var
  Config: TQueryServiceConfig;
begin
  GetServiceConfig(Config);
  Result := Config.lpDisplayName;
end;

procedure TTMService.GetServiceConfig(var Config: TQueryServiceConfig);
var
  BufferSizeNeeded, ErrorCode: Cardinal;
  Buffer: PQueryServiceConfig;
begin
  VerifyActive;

  Buffer := nil;
  if not QueryServiceConfig(ServiceHandle, Buffer, 0, BufferSizeNeeded) then
  begin
    ErrorCode := GetLastError;
    if ErrorCode = ERROR_INSUFFICIENT_BUFFER then
    begin
      GetMem(Buffer, BufferSizeNeeded);
      try
        if not QueryServiceConfig(ServiceHandle, Buffer, BufferSizeNeeded,
                BufferSizeNeeded) then
          raise EServiceException.Create(GetLastError);
        Config := Buffer^;
      finally
        FreeMem(Buffer);
      end;  //try..finally
    end
    else  //if errorcode = error_insufficient_buffer
      raise EServiceException.Create(ErrorCode);
  end;  //if not queryserviceconfig(<empty buffer>)
end;

procedure TTMService.GetServiceStatus(var Status: TServiceStatus);
begin
  VerifyActive;

  if not QueryServiceStatus(ServiceHandle, Status) then
    raise EServiceException.Create(GetLastError);
end;

function TTMService.GetState: TServiceState;
var
  Status: TServiceStatus;
begin
  GetServiceStatus(Status);
  Result := TServiceState(Status.dwCurrentState);
end;

procedure TTMService.Open;
begin
  if Active then
    Close;

  //Open the service control manager
  FSCManagerHandle := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if FSCManagerHandle = 0 then
    raise EServiceException.Create(GetLastError);

  //Open the service
  try
    FServiceHandle := OpenService(SCManagerHandle, PChar(ServiceName),
        SERVICE_PAUSE_CONTINUE or SERVICE_QUERY_CONFIG or SERVICE_QUERY_STATUS or
        SERVICE_START or SERVICE_STOP);
    if ServiceHandle = 0 then
      raise EServiceException.Create(GetLastError);
  except
    //Clean up the service control manager handle
    CloseServiceHandle(SCManagerHandle);
    raise;
  end;
end;

procedure TTMService.Pause;
begin
  ControlService(SERVICE_CONTROL_PAUSE);
end;

procedure TTMService.SetActive(const Value: Boolean);
begin
  if Value then
    Open
  else
    Close;
end;

procedure TTMService.SetServiceName(const Value: String);
begin
  FServiceName := Value;
end;

procedure TTMService.Start;
var
  Parameters: PChar;
begin
  Parameters := nil;
  if not StartService(ServiceHandle, 0, Parameters) then
    raise EServiceException.Create(GetLastError);
end;

procedure TTMService.Stop;
begin
  ControlService(SERVICE_CONTROL_STOP);
end;

procedure TTMService.VerifyActive;
begin
  if not Active then
    raise EServiceException.Create(SNoServiceOpen);
end;

end.
