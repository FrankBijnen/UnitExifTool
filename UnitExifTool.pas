unit UnitExifTool;

interface

uses Winapi.Windows, System.Classes, System.SysUtils;

const
  BLOCK_SIZE              = 4096;
  BEGINMARK: Utf8string   = '{ready';       // '{ready' is usually on pos 1 of the line!
  ENDMARK: Utf8String     = '}' + #13#10;
  ENDREQUEST: Utf8String  = '{endrequest}';
  SENDENDREQUEST          = '-echo1' + #10 + '{endrequest}' + #10 + '-stay_open' + #10 + 'False' + #10;
  MAXEXECNUM              = 99999;
  QUIET                   = '-quiet' + #10;
  CHARSET                 = '-CHARSET';
  CHARSETUTF8             = 'UTF8';
  CHARSETFILEUTF8         = 'FILENAME=UTF8';
  EXECUTE                 = '-execute';
  STARTEXIFTOOL           = 'exiftool -stay_open true -@ -';
  DateTimeShift           = '%s=%u:%u:%u %u:%u:%u';

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
  TSignalExifToolProc = procedure(const ExecNum: integer; const Response: string) of object;

  // Stream for handling reads from a Pipe written to by ExifTool.
  // A Pipe is Ansi, but ExifTool writes UTF8.
  TPipeStream = class(TBytesStream)
  private
    HFile: THandle;
    FBufSize: integer;
    FBeginMarker: integer; // Position Begin marker  {ready
    FEndMarker: integer;   // Position End marker    }#13#10
    FileBuffer: array of byte;

    function GetPipe: Utf8String;
    procedure SetPipe(const APipe: Utf8String; Offset: integer = 1);
    function HasEndReq(const APipeString: UTF8String): boolean;
    function FindMarkers(const APipeString: UTF8String; const ContinueFrom: Integer): boolean;
    function ParseBuffer(ASignalExifToolProc: TSignalExifToolProc): boolean;

  public
    constructor Create(AFile: THandle; ABufSize: integer);
    function PipeHasData: boolean;
    function ReadPipe: DWORD;
    function AsString: string;
  end;

  TExifToolBase = class
  private
    FRunning: boolean;
    FExecNum: integer;
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
    ContentList: TList;
    FExifTool: TExifToolBase;
    FPipeOut: THandle;
    FPipeErr: THandle;
    FOutputBuffer: TPipeStream;
    FErrorBuffer: TPipeStream;
  protected
    procedure Execute; override;
    function ReadErrors: string;
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

var ExifTool: TExifTool;

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

{ TPipeStream }

constructor TPipeStream.Create(AFile: THandle; ABufSize: integer);
begin
  inherited Create;
  HFile := AFile;
  FBufSize := ABufSize;
  SetLength(FileBuffer, FBufSize);
end;

function TPipeStream.PipeHasData: boolean;
var BytesAvail: DWORD;
begin
  result := PeekNamedPipe(HFile, nil, 0, nil, @BytesAvail, nil);
  result := result and (BytesAvail > 0);
end;

function TPipeStream.ReadPipe: DWORD;
begin
  if (Winapi.Windows.ReadFile(HFile, FileBuffer[0], FBufSize, result, nil)) then
    Write(FileBuffer[0], result);
end;

function TPipeStream.GetPipe: UTf8String;
begin
  SetLength(result, Size);
  if (Size > 0) then
    Move(Memory^, result[1], Size);
end;

procedure TPipeStream.SetPipe(const APipe: Utf8String; Offset: integer = 1);
var L: integer;
begin
  Clear;
  L := Length(APipe) + 1 - Offset;
  if (L > 0) then
    Write(APipe[Offset], L);
end;

function TPipeStream.HasEndReq(const APipeString: UTF8String): boolean;
begin
  result := (Copy(APipeString, 1, Length(ENDREQUEST)) = ENDREQUEST);
end;

function TPipeStream.FindMarkers(const APipeString: UTF8String; const ContinueFrom: Integer): boolean;
begin
  FBeginMarker := Pos(BEGINMARK, APipeString, ContinueFrom);
  FEndMarker := Pos(ENDMARK, APipeString, FBeginMarker);
  result := (FBeginMarker > 0) and (FEndMarker > 0);
end;

function TPipeStream.ParseBuffer(ASignalExifToolProc: TSignalExifToolProc): boolean;
var
  PipeString: Utf8String;   // Pipe as UTF8
  ExecStr: string;
  ExecNum: integer;
  Response: string;
  ContinueFrom: integer;    // Continue search from Position for Markers
begin
  PipeString := GetPipe;

  // End requested?
  result := HasEndReq(PipeString);
  if result then
    exit;

  ContinueFrom := 1;
  if not FindMarkers(PipeString, ContinueFrom) then
    exit;

  while (FBeginMarker > 0) and
        (FEndMarker > 0) do
  begin
    // We have a complete message in our buffer.
    // ie. something like {readynnnn}#13#10
    Response := Utf8ToString(Copy(PipeString, 1, FBeginMarker - 1)); // Our Response
    ExecStr := Utf8ToString(Copy(PipeString, FBeginMarker + Length(BEGINMARK), FEndMarker - FBeginMarker - Length(BEGINMARK)));
    ExecNum := StrToIntDef(ExecStr, 0);           // Our Execnumber
    ASignalExifToolProc(ExecNum, Response);       // Signal (T)Exiftool

    ContinueFrom := FEndMarker + Length(ENDMARK); // Continue to search from last Endmark
    FindMarkers(PipeString, ContinueFrom);        // Scan for more messages in our buffer
  end;

  // Store remainder back
  SetPipe(PipeString, ContinueFrom);
end;

function TPipeStream.AsString: string;
begin
  result := UTF8ToString(GetPipe);
end;

{ TExifToolBase }

constructor TExifToolBase.Create;
begin
  inherited Create;
  FRunning := false;
  FExecNum := 0;
end;

destructor TExifToolBase.Destroy;
begin
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
  ContentList := TList.Create;
  FOutputBuffer := TPipeStream.Create(FPipeOut, BLOCK_SIZE);
  FErrorBuffer := TPipeStream.Create(FPipeErr, BLOCK_SIZE);
  ClearContent;
  inherited Create(false); // start running
end;

destructor TReadThread.Destroy;
begin
  ClearContent;
  ContentList.Free;
  FOutputBuffer.Free;
  FErrorBuffer.Free;
  inherited Destroy;
end;

procedure TReadThread.PushContent(const ExecNum: integer; const Request: string);
var Content: PContent;
begin
  System.TMonitor.Enter(ContentList);
  try
    New(Content);
    Content.ExecNum := ExecNum;
    Content.Request := Request;
    ContentList.Add(Content);
  finally
    System.TMonitor.Exit(ContentList);
  end;
end;

function TReadThread.PopContent: TContent;
var Content: PContent;
begin
  FillChar(result, SizeOf(result), 0);
  System.TMonitor.Enter(ContentList);
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
    System.TMonitor.Exit(ContentList);
  end;
end;

procedure TReadThread.UpdateContent(const ExecNum: integer; const Response, Error: string);
var Content: PContent;
begin
  System.TMonitor.Enter(ContentList);
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
    System.TMonitor.Exit(ContentList);
  end;
end;

function TReadThread.GetPending: integer;
begin
  result := ContentList.Count;
end;

procedure TReadThread.Execute;
begin
  repeat
    // try to read from pipe. If nothing avail this thread will be blocked.
    FOutputBuffer.ReadPipe;
  until (FOutputBuffer.ParseBuffer(SignalExifTool));
  ClearContent;
end;

function TReadThread.ReadErrors: string;
begin
  while (FErrorBuffer.PipeHasData) and
        (FErrorBuffer.ReadPipe > 0) do;
  result := FErrorBuffer.AsString;
  FErrorBuffer.Clear;
end;

procedure TReadThread.SignalExifTool(const ExecNum: integer; const Response: string);
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
  System.TMonitor.Enter(ContentList);
  try
    while (ContentList.Count > 0) do
      PopContent;

    FOutputBuffer.Clear;
  finally
    System.TMonitor.Exit(ContentList);
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
  AStream.WriteString(CHARSET + #10);
  AStream.WriteString(CHARSETFILEUTF8 + #10);
  AStream.WriteString(CHARSET + #10);
  AStream.WriteString(CHARSETUTF8 + #10);
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
  AStream := TStringStream.Create(ACmd, TEncoding.UTF8);
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

initialization
begin
  ExifTool := TExifTool.Create;
end;

finalization
begin
  ExifTool.Free;
end;

end.
