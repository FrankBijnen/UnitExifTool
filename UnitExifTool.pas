unit UnitExifTool;

interface

uses Winapi.Windows, System.Classes, System.SysUtils, System.SyncObjs;

const
  BLOCK_SIZE      = 4096;
  BEGINMARK       = '{ready'; // '{ready' should be on pos 1 of the line!
  ENDMARK         = '}' + #13#10;
  ENDREQUEST      = '{endrequest}';
  SENDENDREQUEST  = '-echo1' + #10 + ENDREQUEST + #10 + '-stay_open' + #10 + 'False' + #10;
  MAXEXECNUM      = 99999;
  QUIET           = '-quiet' + #10;
  EXECUTE         = '-execute';
  STARTEXIFTOOL   = 'exiftool -stay_open true -@ -';
  DateTimeShift   = '%s=%u:%u:%u %u:%u:%u';

type
  TContent = record
    ExecNum: integer;
    Request: string;
    Response: string;
    Error: string;
  end;
  PContent = ^TContent;

  TGetExif = procedure(const AImage: string) of object;
  TOnReadyEvent = procedure(Sender: TObject; Content: TContent) of object;

  TExifToolBase = class
  private
    FRunning: boolean;
    FExecNum: integer;
    FLock: TMutex;
    FOnReady: TOnReadyEvent;
    procedure SetExecNum(AExecNum: integer);
  protected
    function Send(var AStream: TStringStream): integer; virtual;
    procedure DataReady(const Content: TContent); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property OnReady: TOnReadyEvent read FOnReady write FOnReady;
    property ExecNum: integer read FExecNum write SetExecNum;
  end;

  TReadThread = class(TThread)
  private
    FLockList: TMutex;
    ContentList: TList;
    FExifTool: TExifToolBase;
    FPipeOut: THandle;
    FPipeErr: THandle;
    FBuffer: String;
  protected
    procedure Execute; override;
    function ReadErrors: String;
    function CheckBuffer: boolean;
    function GetPending: integer;
  public
    constructor Create(const PipeOut, PipeErr: THandle; const AExifTool: TExifToolBase);
    destructor Destroy; override;
    procedure PushContent(const ExecNum: integer; const Request: string);
    procedure UpdateContent(const ExecNum: integer; const Response, Error: string);
    function PopContent: TContent;
    procedure SignalExifTool(const ExecNum: integer; const Response: string);
    procedure OnReadyEvent;
    procedure ClearContent;
    property PendingRequests: integer read GetPending;
  end;

  TExifTool = class(TExifToolBase)
  protected
    FCmdLine: string;
    FReadPipe: TReadThread;
    HPipeInputRead, HPipeInputWrite: THandle;
    HPipeOutputRead, HPipeOutputWrite: THandle;
    HPipeErrorRead, HPipeErrorWrite: THandle;
    procedure AddExecute(var AStream: TstringStream);
    function Send(var AStream: TStringStream): integer; overload; override;
  public
    constructor Create(const ACmdLine: string = STARTEXIFTOOL);
    destructor Destroy; override;
    procedure StopExifTool;
    function StartExifTool: boolean;
    procedure WaitFor;
    function Send(const ACmd: string): integer; reintroduce; overload;
    function Send(const ACmds: TStringList): integer; reintroduce; overload;
    property ReadPipe: TReadThread read FReadPipe;
  end;

implementation

{Light weight processmessages. Dont need to include Vcl.Forms}

procedure ProcessMessages;
var PmMsg: TMsg;
begin
  while (PeekMessage(PmMsg, 0, 0, 0, PM_REMOVE)) do
  begin
    TranslateMessage(PmMsg);
    DispatchMessage(PmMsg);
  end;
end;

{ TExifToolBase }

constructor TExifToolBase.Create;
begin
  inherited Create;
  FRunning := false;
  FExecNum := 0;
  FLock := Tmutex.Create();
end;

destructor TExifToolBase.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

function TExifToolBase.Send(var AStream: TStringStream): integer;
begin
  if (not FRunning) then
    raise Exception.Create('ExifTool not running');
  inc(FExecNum);
  if (FExecNum > MAXEXECNUM) then
    FExecNum := 1;
  result := FExecNum;
end;

procedure TExifToolBase.DataReady(const Content: TContent);
begin
  if Assigned(FOnReady) then
    FOnReady(Self, Content);
end;

procedure TExifToolBase.SetExecNum(AExecNum: integer);
begin
  FExecNum := AExecNum;
end;

{ TReadThread }

constructor TReadThread.Create(const PipeOut, PipeErr: THandle; const AExifTool: TExifToolBase);
begin
  FPipeOut := PipeOut;
  FPipeErr := PipeErr;
  FExifTool := AExifTool;
  FLockList := TMutex.Create();
  ContentList := TList.Create;
  ClearContent;
  inherited Create(false); // start running
end;

destructor TReadThread.Destroy;
begin
  ClearContent;
  ContentList.Free;
  FLockList.Free;
  inherited Destroy;
end;

procedure TReadThread.PushContent(const ExecNum: integer; const Request: string);
var Content: PContent;
begin
  FLockList.Acquire;
  try
    new(Content);
    Content.ExecNum := ExecNum;
    Content.Request := Request;
    ContentList.Add(Content);
  finally
    FLockList.Release;
  end;
end;

function TReadThread.PopContent: TContent;
var Content: PContent;
begin
  FillChar(result, SizeOf(result), 0);
  FLockList.Acquire;
  try
    if (ContentList.Count > 0) then
    begin
      Content := ContentList[0];

      result.ExecNum  := Content.ExecNum;
      result.Request  := Content.Request;
      result.Response := Content.Response;
      result.Error    := Content.Error;

      SetLength(Content.Request, 0);
      SetLength(Content.Response, 0);
      SetLength(Content.Error, 0);
      Dispose(Content);

      ContentList.Delete(0);
    end;
  finally
    FLockList.Release;
  end;
end;

procedure TReadThread.UpdateContent(const ExecNum: integer; const Response, Error: string);
var Content: PContent;
begin
  FLockList.Acquire;
  try
    for Content in ContentList do
    begin
      if (Content.ExecNum = ExecNum) then
      begin
        Content.Response  := Response;
        Content.Error     := Error;
        break;
      end;
    end;
  finally
    FLockList.Release;
  end;
end;

function TReadThread.GetPending: integer;
begin
  result := ContentList.Count;
end;

procedure TReadThread.Execute;
var BytesRead: DWORD;
    FileBuffer: AnsiString; // Exiftool only uses ansi?
begin
  repeat
    // try to read from pipe
    SetLength(FileBuffer, BLOCK_SIZE);
    if ReadFile(FPipeOut, FileBuffer[1], BLOCK_SIZE, BytesRead, nil) then
    begin
      SetLength(FileBuffer, BytesRead);
      FBuffer := FBuffer + string(FileBuffer);
    end;
  until (CheckBuffer);
  ClearContent;
end;

function TReadThread.CheckBuffer: boolean;
var
  ExecStr: string;
  ExecNum: integer;
  Response: string;
  P: integer;
  J: integer;

  function FindEndReq: boolean;
  begin
    P := pos(ENDREQUEST, FBuffer);
    result := P > 0;
  end;

  procedure FindMarkers;
  begin
    P := pos(BEGINMARK, FBuffer);
    J := pos(ENDMARK, FBuffer, P);
  end;

begin
  // End requested?
  result := FindEndReq;
  if result then
    exit;

  FindMarkers;

  while (P > 0) and
        (J > 0) do
  begin
    // We have a complete message in our buffer.
    // ie. something like {readynnnn}#13#10
    Response := copy(FBuffer, 1, P - 1); // Our Response
    ExecStr := copy(FBuffer, P + length(BEGINMARK), J - P - length(BEGINMARK));
    try
      ExecNum := StrToInt(ExecStr); // Our Execnumber
    except
      ExecNum := 0;
    end;
    Delete(FBuffer, 1, J + length(ENDMARK) - 1); // Remove from buffer
    SignalExifTool(ExecNum, Response); // Inform (T)Exiftool

    FindMarkers; // Scan for more messages in our buffer.
  end;
end;

function TReadThread.ReadErrors: String;
var BytesRead: DWORD;
    BytesAvail: DWORD;
    FileBuffer: AnsiString; // Exiftool only uses ansi?
begin
  result := '';
  // Any errors?
  PeekNamedPipe(FPipeErr, nil, 0, nil, @BytesAvail, nil);
  while (BytesAvail > 0) do
  begin
    SetLength(FileBuffer, BLOCK_SIZE);
    if (ReadFile(FPipeErr, FileBuffer[1], BLOCK_SIZE, BytesRead, nil)) then
    begin
      SetLength(FileBuffer, BytesRead);
      result := result + string(FileBuffer);
    end;
    PeekNamedPipe(FPipeErr, nil, 0, nil, @BytesAvail, nil);
  end;
end;

procedure TReadThread.SignalExifTool(const ExecNum: integer; const Response: String);
begin
  UpdateContent(ExecNum, Response, ReadErrors); // Update the list with our response
  Queue(OnReadyEvent);                          // Queue in the main thread.
end;

procedure TReadThread.OnReadyEvent;
var Content: TContent;
begin
  if (ContentList.Count > 0) then
  begin
    Content := PopContent;
    FExifTool.DataReady(Content);
  end;
end;

procedure TReadThread.ClearContent;
begin
  FLockList.Acquire;
  try
    while (ContentList.Count > 0) do
      PopContent;

    SetLength(FBuffer, 0);
  finally
    FLockList.Release;
  end;
end;

{ TExifTool }

constructor TExifTool.Create(const ACmdLine: string = StartExifTool);
begin
  inherited Create;
  FCmdLine := ACmdLine;
  FRunning := StartExifTool;
  if (not FRunning) then
    raise Exception.Create(ACmdLine + ' could not be started');
end;

destructor TExifTool.Destroy;
begin
  // If the constructor raises an exception, Destroy gets called rightaway
  StopExifTool;
  inherited Destroy;
end;

procedure TExifTool.WaitFor;
begin
  // Note: When the reader stops, it calls ClearContent. setting PendingRequests to 0
  while (FRunning) and
        (FReadPipe.PendingRequests > 0) do
  begin
    ProcessMessages;
    Sleep(50);
  end;
end;

procedure TExifTool.StopExifTool;
begin
  if (FRunning) then
  begin
    // Sending the final call. ExifTool will end.
    Send(SENDENDREQUEST);
    // Wait for processing to complete
    WaitFor;
    // Now Free
    FReadPipe.Free;
  end;

  CloseHandle(HPipeOutputRead);
  CloseHandle(HPipeInputWrite);
  CloseHandle(HPipeErrorRead);
end;

function TExifTool.StartExifTool: boolean;
var MySecurityAttributes: SECURITY_ATTRIBUTES;
    MyStartupInfo: STARTUPINFO;
    MyProcessInfo: PROCESS_INFORMATION;
begin
  // prepare security structure
  ZeroMemory(@MySecurityAttributes, sizeof(SECURITY_ATTRIBUTES));
  MySecurityAttributes.nLength := sizeof(SECURITY_ATTRIBUTES);
  MySecurityAttributes.bInheritHandle := TRUE;

  // create pipe to set stdinput
  HPipeInputRead := 0;
  HPipeInputWrite := 0;

  CreatePipe(HPipeInputRead, HPipeInputWrite, @MySecurityAttributes, 0);

  // create pipes to get stdoutput and stderror
  CreatePipe(HPipeOutputRead, HPipeOutputWrite, @MySecurityAttributes, 0);
  CreatePipe(HPipeErrorRead, HPipeErrorWrite, @MySecurityAttributes, 0);

  // prepare startupinfo structure
  ZeroMemory(@MyStartupInfo, SizeOf(STARTUPINFO));
  MyStartupInfo.cb := SizeOf(STARTUPINFO);

  // hide application
  MyStartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  MyStartupInfo.wShowWindow := SW_HIDE;

  // assign pipes
  MyStartupInfo.dwFlags := MyStartupInfo.dwFlags or STARTF_USESTDHANDLES;
  MyStartupInfo.hStdInput := HPipeInputRead;
  MyStartupInfo.hStdOutput := HPipeOutputWrite;
  MyStartupInfo.hStdError := HPipeErrorWrite;

  // start the process
  result := CreateProcess(nil,
                          PChar(FCmdLine),
                          nil,
                          nil,
                          TRUE,
                          CREATE_NEW_CONSOLE + HIGH_PRIORITY_CLASS,
                          nil,
                          nil,
                          MyStartupInfo,
                          MyProcessInfo);

  // create thread for reading
  if (result) then
    FReadPipe := TReadThread.Create(HPipeOutputRead, HPipeErrorRead, self);

  // close the ends of the pipes, now used by the process
  CloseHandle(HPipeInputRead);
  CloseHandle(HPipeOutputWrite);
  CloseHandle(HPipeErrorWrite);
end;

procedure TExifTool.AddExecute(var AStream: TstringStream);
begin
  AStream.Position := AStream.Size;
  AStream.WriteString(EXECUTE + IntToStr(ExecNum) + #10);
  FReadPipe.PushContent(ExecNum, AStream.DataString);
  AStream.Position := 0;
end;

function TExifTool.Send(var AStream: TStringStream): integer;
var MyBuffer: array [0 .. BLOCK_SIZE - 1] of Byte;
    IBytesToWrite: DWORD;
    IBytesWritten: DWORD;
begin
  result := inherited Send(AStream); // Set ExecNum

  AddExecute(AStream);

  IBytesToWrite := AStream.Read(MyBuffer, BLOCK_SIZE);
  while (IBytesToWrite > 0) do
  begin
    WriteFile(HPipeInputWrite, MyBuffer, IBytesToWrite, IBytesWritten, nil);
    IBytesToWrite := AStream.Read(MyBuffer, BLOCK_SIZE);
  end;
end;

function TExifTool.Send(const ACmd: string): integer;
var AStream: TstringStream;
begin
  AStream := TstringStream.Create(ACmd);
  try
    result := Send(AStream);
  finally
    AStream.Free;
  end;
end;

function TExifTool.Send(const ACmds: TStringList): integer;
begin
  result := Send(ACmds.Text);
end;

end.
