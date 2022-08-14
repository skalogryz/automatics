unit runproctypes;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Pipes, Process, ExtraFileUtils;

type
  TStdHandler = (sdIgnore, sdPipeOnly, sdFile);

  TRunInput = record
    exec   : string;
    args   : array of string;
    rundir : string;
    env    : array of string; // Key=Value array
    timeOutMs  : integer;
    tempDir    : string;

    // if true, the redirection will be done to a file, instead of the pipe
    // However, std out and std err ALWAYS go to a file
    stdOutToFile : TStdHandler;
    stdErrToFile : TStdHandler;
  end;

  TRunResult = record
    startTime : TDatetime;
    endTime   : TDateTime;
    runTimeMs : Int64;    // runtime im
    timedOut  : Boolean;
    startTick : UInt64;
    endTick   : UInt64;

    exitCode  : LongWord;
    outSize   : Uint64;
    errSize   : Uint64;
    tempDir   : string; // temporary directory. Everything inside can be deleted
    stdOutFn  : string; // should be placed within the temp dir
    stdErrFn  : string; // should be placed within the temp dir
    isRunning : Boolean;

    runError  : Integer; // 0 - if ran succesfully
    runSysErr : Integer;
  end;

  { TRunProcess }

  TRunProcess = class(TObject)
  private
    proc     : TProcess;
    track    : TThread;
    usePipes : Boolean;
  protected
    fAborted : Boolean;
    buf      : array of byte;
    fTempDir     : string;
    fTimeOutWait : Int64;
    fExitCode    : integer;
    fExitStatus  : integer;
    fStartTime   : TDateTime;
    fStartTick   : UInt64;
    fEndTime     : TDateTime;
    fEndTick     : UInt64;
    fIsTimeOut   : Boolean;
    fOutSize     : UInt64;
    fErrSize     : Uint64;
    fOutFn       : string;
    fErrFn       : string;
    fRunError    : integer;
    fRunSysError : integer;

    procedure ThreadProc;
    function GatherOutput(StdOutSt, StdErrSt: TStream): Boolean;
    function GatherOutputStream(src: TInputPipeStream; dst: TStream): Integer;
    function PrepareOutFileStream(const name: string): TFileStream;
    procedure WaitEvents(timeLeft: Int64);
  public
    // the path where to store temporary files
    tempPath : string;
    bufSize  : integer; // 4Kb by default
    constructor Create;
    destructor Destroy; override;
    procedure Prepare(const inp: TRunInput);
    function Run: Boolean;
    function isRunning: Boolean;
    procedure Abort;
    procedure GetResults(out res: TRunResult);
  end;

procedure ProcPrepare(const exec: string; out inp: TRunInput);
function ProcStart(const inp: TRunInput): TRunProcess;
procedure ProcPrepareTempDir(var inp: TRunInput; const prefix: string = '');
procedure ProcArgs(var inp: TRunInput; args: TStrings);
procedure ProcWaitLoop(p : TRunProcess);
procedure ProcDeleteTempFolder(p: TRunProcess);

function GetSysError: Integer;

implementation

type

  { TProcessTrackThread }

  TProcessTrackThread = class(TThread)
  protected
    fRunProcess : TRunProcess;
    procedure Execute; override;
  public
    constructor Create(ArunProcess: TRunProcess);
  end;

procedure ProcPrepare(const exec: string; out inp: TRunInput);
begin
  inp := Default(TRunInput);
  inp.exec := exec;
  inp.stdErrToFile:=sdPipeOnly;
  inp.stdOutToFile:=sdPipeOnly;
  inp.timeOutMs:=-1;
end;

function ProcStart(const inp: TRunInput): TRunProcess;
begin
  Result:=TRunProcess.Create;
  Result.Prepare(inp);
  Result.Run;
end;

procedure ProcWaitLoop(p : TRunProcess);
begin
  while p.isRunning do begin
    Sleep(15);
  end;
end;

procedure ProcDeleteTempFolder(p: TRunProcess);
begin
  DeleteDirectory(p.tempPath);
end;

{ TProcessTrackThread }

constructor TProcessTrackThread.Create(ArunProcess: TRunProcess);
begin
  inherited Create(true);
  FreeOnTerminate := false;
  fRunProcess := ARunProcess;
end;

procedure TProcessTrackThread.Execute;
begin
  try
    fRunProcess.ThreadProc;
  except
    // any unhandled error
  end;
end;

function GetSysError: Integer;
begin
  Result := GetLastOSError;
end;

{ TRunProcess }

procedure TRunProcess.ThreadProc;
var
  msleft   : Int64;
  StdOutSt : TFileStream;
  StdErrSt : TFileStream;
begin
  StdOutSt := nil;
  StdErrSt := nil;
  try
    try
      fRunError := 0;
      fRunSysError := 0;
      fTempDir := tempPath;
      fStartTime:=Now;
      fStartTick:=GetTickCount64;

      proc.Execute;

      if (fTempDir <>'') then begin
        StdOutSt := PrepareOutFileStream('stdout');
        if Assigned(StdOutSt) then fOutFn := StdOutSt.FileName;
      end;
      if (fTempDir <>'') then begin
        StdErrSt := PrepareOutFileStream('stderr');
        if Assigned(StdErrSt) then fErrFn := StdErrSt.FileName;
      end;

      // run and wait for the output
      while not fAborted and proc.Running do begin
        GatherOutput(StdOutSt, StdErrSt);
        if (fTimeOutWait > 0) and ((GetTickCount64-fStartTick) > fTimeOutWait) then begin
          // timeout did occur
          fIsTimeOut := true;
          fAborted := true;
        end;
        if fTimeOutWait > 0 then begin
          msleft := GetTickCount64 - fStartTick;
        end else
          msleft := -1;
        WaitEvents(msleft);
      end;
      fEndTick:=GetTickCount64;
      fEndTime:=now;
      if proc.Running and fAborted then
        proc.Terminate(0);

      // collect the remaining output
      // todo: need a time out for gathering leftovers
      while not fAborted and GatherOutput(StdOutSt, StdErrSt) do
        ;
    except
      on EProcess do begin
        fRunError := -1;
        fRunSysError := GetSysError;
      end;
    end;
  finally
    StdOutSt.Free;
    StdErrSt.Free;
  end;
  fExitCode := proc.ExitCode;
  fExitStatus := proc.ExitStatus;
end;

function TRunProcess.GatherOutput(StdOutSt, StdErrSt: TStream): Boolean;
var
  sz : integer;
begin
  Result := false;
  if Assigned(proc.Output) then begin
    sz := GatherOutputStream(proc.Output, StdOutSt);
    inc(fOutSize, sz);
    if sz > 0 then Result := true;
  end;
  if Assigned(proc.Stderr) then begin
    sz := GatherOutputStream(proc.Stderr, StdErrSt);
    inc(fErrSize, sz);
    if sz > 0 then Result := true;
  end;
end;

function TRunProcess.GatherOutputStream(src: TInputPipeStream; dst: TStream
  ): Integer;
var
  sz : integer;
begin
  Result := 0;
  if (src = nil) then Exit;
  sz := src.NumBytesAvailable;
  if sz <= 0 then Exit;

  if length(buf)=0 then SetLength(buf, bufSize);
  if (sz > bufSize) then sz := bufSize;
  sz := src.Read(buf[0], sz);
  Result := sz;
  if Result < 0 then Result := 0; // do not output negative
  if (sz>0) and (Assigned(dst)) then
    dst.Write(buf[0], sz);
end;

function TRunProcess.PrepareOutFileStream(const name: string): TFileStream;
var
  fn : string;
begin
  if fTempDir = '' then begin
    Result := nil;
    Exit;
  end;
  ForceDirectories(fTempDir);
  fn := IncludeTrailingPathDelimiter(fTempDir)+name;
  if not FileExists(fn) then begin
    try
      Result := TFileStream.Create(fn, fmCreate or fmShareDenyNone);
      try
      finally
        Result.Free;
      end;
    except
    end;
  end;
  Result := TFileStream.Create( IncludeTrailingPathDelimiter(fTempDir)+name, fmOpenReadWrite or fmShareDenyNone);
end;

procedure TRunProcess.WaitEvents(timeLeft: Int64);
begin
  // todo:
  // The following objects needs to be "waited"
  //   - process (if it finishes)
  //   - pipe streams (if data is available)
  //   - event (not indicate the time was reset)
  //   - timeout occuring (if timeout is used)
  // for Windows, one can use WaitForMultipleObjects.
  if (timeLeft < 0) or (timeLeft >=10)
    then Sleep(10)
    else Sleep(timeLeft);
end;

constructor TRunProcess.Create;
begin
  inherited Create;
  bufSize := 1024 * 4;
end;

destructor TRunProcess.Destroy;
begin
  Abort;
  inherited Destroy;
end;

procedure TRunProcess.Prepare(const inp: TRunInput);
var
  i : integer;
begin
  if isRunning then Exit;

  if Assigned(track) then Abort;

  if Assigned(proc) then begin
    proc.Free;
    proc :=nil;
  end;

  fAborted := false;
  proc := TProcess.Create(nil);
  proc.Executable:=inp.exec;
  for i:=0 to length(inp.args)-1 do
    proc.Parameters.Add(inp.args[i]);
  for i:=0 to length(inp.env)-1 do
    proc.Environment.Add(inp.env[i]);
  proc.CurrentDirectory := inp.rundir;
  usePipes := (inp.stdErrToFile <> sdIgnore) or (inp.stdOutToFile <> sdIgnore);
  fTimeOutWait := inp.timeOutMs;
  tempPath := inp.tempDir;
end;

function TRunProcess.Run: Boolean;
begin
  if not Assigned(proc) then begin
    Result := false;
    Exit;
  end;

  if Assigned(track) then begin
    Result := true;
    Exit;
  end;
  proc.Options := proc.Options + [poNoConsole];
  if usePipes then proc.Options:=proc.Options+[poUsePipes];

  track := TProcessTrackThread.Create(Self);
  track.Start;
  Result := false;
end;

function TRunProcess.isRunning: Boolean;
begin
  Result := Assigned(track) and not track.Finished;
  if not Result and Assigned(track) then begin
    track.Free;
    track :=nil;
  end;
end;

procedure TRunProcess.Abort;
begin
  if not Assigned(track) then Exit;
  fAborted := true;
  track.WaitFor;

  track := nil;
end;

procedure TRunProcess.GetResults(out res: TRunResult);
begin
  res := Default(TRunResult);
  res.startTime:=fStartTime;
  res.endTime:=fEndTime;
  res.exitCode:=fExitCode;
  res.timedOut:=fIsTimeOut;
  res.runTimeMs:=fEndTick-fStartTick;
  res.startTick:=fStartTick;
  res.endTick:=fEndTick;
  res.outSize:=fOutSize;
  res.errSize:=fErrSize;
  res.stdOutFn:=fOutFn;
  res.stdErrFn:=fErrFn;
  res.isRunning:=isRunning;
  res.runError:=fRunError;
  res.runSysErr:=fRunSysError;
end;

procedure ProcPrepareTempDir(var inp: TRunInput; const prefix: string = '');
var
  nm : string;
  g : Tguid;
begin
  CreateGUID(g);
  nm := GUIDToString(g);
  if (nm<>'') and (nm[1]='{') then
    nm := Copy(nm, 2, length(nm)-2);
  if prefix <> '' then
    nm := prefix+'_'+GUIDToString(g);
  inp.tempDir:=IncludeTrailingPathDelimiter(GetTempDir)+nm;
end;

procedure ProcArgs(var inp: TRunInput; args: TStrings);
var
  i : integer;
begin
  if args = nil then Exit;
  if args.Count = 0 then Exit;
  SetLength(inp.args, args.Count);
  for i:=0 to args.Count-1 do
    inp.args[i] := args[i];
end;

end.

