unit tstrunnercore;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ExtraFileUtils
  ,tstrunnerlog
  ,runtesttypes
  ,plainsyntaxtype
  ,plainsyntaxexeccond
  ,plainsyntaxexec;

type
  TTestInput = record
    subject   : string;    // subject test file (the executable)
    maxPara   : Integer;   // maximum parallel executions
    tempDir   : string;    // the directory used to store all the results
                           // it's an optional parameter that can be empty
  end;

  { TFileRunInfo }

  TFileRunInfo = class
  protected
    procedure ExecProc;
  public
    fn       : string;
    delegate : TObject;
    exec     : TPlainSyntaxExec;
    execThr  : TThread;
    done     : Boolean;

    tempDir  : string; // target temporary dir to store results. It can be empty
    destructor Destroy; override;
    function IsRunning: Boolean;
    function GetTestResult: TTestResult;
    procedure Start(const scriptFn, subjectFile: string);
  end;

  { TTestPerformer }

  TTestPerformer = class(TObject)
  private
    filesToTest : TStringList; // of TFileRunInfo
    dirToTest   : TStringList; // of
    dirDone     : Boolean;
    ctrlThread  : TThread;
    cancel      : Boolean;
    fileExt     : TStringList;
    maxPara     : integer;
    subject     : string;
    ftempdir    : string;

  protected
    procedure ControlThreadProc;
    procedure StartDirSearch;
    procedure CheckDirSearch;
    procedure VerifyFilesFromDirSearch(l: TStringList);
    function CheckTestFiles: Integer;
  public
    OwnResult : Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Init(Target: TStringList; const inp : TTestInput);
    procedure StartTests;
    function IsDone: Boolean;
    procedure Abort;
    function GetResults(dst: TStrings): Boolean;
    property tempDir   : string read ftempDir;
  end;

procedure PerformTests(Target: TStringList; const inp : TTestInput; result: TStrings);
procedure InitTestInput(out inp: TTestInput);

const
  DEFAULT_PARALLEL = 8; // in fact is should 100500

function NewGuidStr: string;

const
  DelegateLogFileName = 'log.txt';

implementation

type

  { TControlThread }

  TControlThread = class(TThread)
  protected
    p : TTestPerformer;
    procedure Execute; override;
  public
    constructor Create(performer: TTestPerformer);
  end;

function NewGuidStr: string;
var
  nm : string;
  g : Tguid;
begin
  CreateGUID(g);
  nm := GUIDToString(g);
  if (nm<>'') and (nm[1]='{') then
    nm := Copy(nm, 2, length(nm)-2);
  Result := nm;
end;

procedure PerformTests(Target: TStringList; const inp : TTestInput; result: TStrings);
var
  t : TTestPerformer;
begin
  t := TTestPerformer.Create;
  try
    t.Init(Target, inp);
    t.StartTests;
    while not t.IsDone do begin
      Sleep(15);
    end;
    if Assigned(result) then
      t.OwnResult := false;
    t.GetResults(result);
  finally
    t.Free;
  end;
end;

{ TFileRunThread }

type
  TFileRunThread = class(TThread)
  protected
    p : TFileRunInfo;
    procedure Execute; override;
  public
    constructor Create(afile: TFileRunInfo);
  end;

procedure TFileRunThread.Execute;
begin
  try
    p.ExecPRoc;
  except
  end;
end;

constructor TFileRunThread.Create(afile: TFileRunInfo);
begin
  inherited Create(true);
  p:=afile;
end;

type

  { TFileInfoDelegate }

  TFileInfoDelegate = class(TPlainSyntaxExecEnv)
  protected
    logfn : string;
    procedure DoLog(const pfx, msg: string);
  public
    finfo : TfileRunInfo;
    constructor Create(Ainfo: TfileRunInfo);
    procedure StartCommand(const rawCmd, finalCmd: TPlainCommand); override;
    procedure CommandFinished(rawCmd: TPlainCommand; res: TCommandExecResult); override;
    procedure EchoMsg(const msg: string; cmd: TPlainCommand); override;
    procedure ErrorMsg(const msg: string; cmd: TPlainCommand); override;
    procedure LogMsg(const msg: string; cmd: TPlainCommand); override;
    function DisplayCommand(cmd: TPlainCommand): string;
  end;

procedure SW(const dst: TStream; const txt: string);
begin
  if Assigned(dst) and (length(txt)>0) then
    dst.Write(txt[1], length(txt));
end;

procedure SWLn(const dst: TStream; const txt: string);
begin
  SW(dst, txt);
  SW(dst, LineEnding);
end;

{ TFileInfoDelegate }

procedure TFileInfoDelegate.DoLog(const pfx, msg: string);
var
  f : Text;
  fn : string;
  fs : TFileStream;
  t  : string;
begin
  try
    if (finfo.tempDir = '') then begin
      if pfx='' then
        Verbose(msg)
      else
        Verbose('%s: %s',[pfx, msg]);
      Exit;
    end;

    if (logfn = '') then begin
      ForceDirectories(finfo.tempDir);
      logfn := IncludeTrailingPathDelimiter(finfo.tempDir)+DelegateLogFileName;
      fs := TFileStream.create(logfn, fmCreate);
      try
      finally
        fs.Free;
      end;
    end;
    if logfn <>'' then begin
      fs := TFileStream.create(logfn, fmOpenReadWrite or fmShareDenyNone);
      try
        fs.Seek(0, soEnd);
        SW(fs, FormatDateTime('hh:nn:ss:zzz',now)+' ['+INtToStr(GetCurrentThreadId)+']: ');
        if pfx <> '' then begin
          SW(fs, pfx);
          SW(fs, ': ');
        end;
        SWLn(fs, msg);
      finally
        fs.Free;
      end;
    end;
  except
  end;
end;

constructor TFileInfoDelegate.Create(Ainfo: TfileRunInfo);
begin
  inherited Create;
  finfo := AInfo;
end;

procedure TFileInfoDelegate.StartCommand(const rawCmd, finalCmd: TPlainCommand);
begin
  DoLog('run command: ', DisplayCommand(finalCmd));
end;

procedure TFileInfoDelegate.CommandFinished(rawCmd: TPlainCommand;
  res: TCommandExecResult);
begin
  DoLog('end command: ', DisplayCommand(res.cmd));
end;

procedure TFileInfoDelegate.EchoMsg(const msg: string; cmd: TPlainCommand);
begin
  DoLog('  ECHO: ', msg);
end;

procedure TFileInfoDelegate.ErrorMsg(const msg: string; cmd: TPlainCommand);
begin
  DoLog('  CMD ERROR: ', msg);
end;

procedure TFileInfoDelegate.LogMsg(const msg: string; cmd: TPlainCommand);
begin
  DoLog('  CMD LOG: ', msg);
end;

function TFileInfoDelegate.DisplayCommand(cmd: TPlainCommand): string;
begin
  if cmd = nil then Result := ''
  else if cmd.cmdlow = '' then Result := PlainCmdStr[cmd.cmd]
  else Result := cmd.cmdlow;
end;

{ TFileRunInfo }

procedure TFileRunInfo.ExecProc;
var
  cmds : TList;
  i    : integer;
begin
  cmds:=nil;
  try
    Log('executing: %s', [fn]);
    cmds := ReadPlainCommandFile(fn);
    exec.RunTempDir := tempDir;
    exec.RunCommands(cmds);
    Log('finished: %s (%s)', [ExtractFileName(fn), TestResultNameStr[exec.FinalResult]]);
  finally
    done := true;
    if (cmds<>nil) then begin
      for i := 0 to cmds.Count-1 do TObject(cmds[i]).Free;
      cmds.Free;
    end;
  end;
end;

destructor TFileRunInfo.Destroy;
begin
  delegate.Free;
  exec.Free;
  execThr.Free;
  inherited Destroy;
end;

function TFileRunInfo.IsRunning: Boolean;
begin
  Result := Assigned(execThr)
       and (not execThr.Finished)
       and not done;
end;

function TFileRunInfo.GetTestResult: TTestResult;
begin
  Result :=exec.FinalResult;
end;

procedure TFileRunInfo.Start(const scriptFn, subjectFile: string);
begin
  done := false;
  fn := scriptFn;
  exec := TPlainSyntaxExec.Create;
  exec.Delegate := TFileInfoDelegate.Create(Self);
  delegate := exec.Delegate;
  exec.Params.Values['subject']:=subjecTfile;
  exec.Params.Values['subj']:=subjecTfile;
  exec.Params.Values['subjdir']:=ExtractFileDir(subjecTfile);
  exec.CurDir := ExtractFileDir(scriptFn);

  execThr := TFileRunThread.Create(Self);
  execThr.Start;
end;

{ TControlThread }

procedure TControlThread.Execute;
begin
  try
    p.ControlThreadProc;
  except
  end;
end;

constructor TControlThread.Create(performer: TTestPerformer);
begin
  inherited Create(true);
  p := performer;
end;

{ TTestPerformer }

procedure TTestPerformer.ControlThreadProc;
var
  lfn  : TStringList;
begin
  StartDirSearch;

  lfn := TStringList.Create;
  try
    while not cancel do begin
      if not dirDone then begin
        CheckDirSearch;
        dirDone := dirToTest.Count=0;
      end;

      if CheckTestFiles = 0 then begin
        if dirDone then
          break;
      end;
    end;
  finally
    lfn.Free;
  end;
end;

procedure TTestPerformer.CheckDirSearch;
var
  i    : integer;
  srch : TAsyncFileSearch;
  lfn  : TStringList;
  sr   : Boolean;
begin
  lfn := TStringList.Create;
  try
    for i := dirToTest.Count-1 downto 0 do begin
      srch := TAsyncFileSearch(dirToTest.Objects[i]);
      lfn.Clear;
      sr := srch.IsSearching;
      if srch.GatherNewFound(lfn) > 0 then
        VerifyFilesFromDirSearch(lfn);
      if not sr then begin
        srch.Free;
        dirToTest.Delete(i);
      end;
    end;
  finally
    lfn.Free;
  end;
end;

procedure TTestPerformer.VerifyFilesFromDirSearch(l: TStringList);
var
  i : integer;
  x  : string;
begin
  if (l = nil) or (l.Count = 0) then Exit;
  for i:=0 to l.Count-1 do begin
    x := AnsiLowerCase( ExtractFileExt(l[i]));
    Verbose('found: (%s) %s', [x, l[i]]);
    if (fileExt.IndexOf(x)>=0) then begin
      Log('scheduling the file: %s', [l[i]]);
      filesToTest.Add(l[i]);
    end;
  end;
end;

function TTestPerformer.CheckTestFiles: Integer;
var
  i    : Integer;
  fi   : Integer;
  used : integer;
  info : TFileRunInfo;
begin
  // checking ran files
  used := 0;
  fi := -1;
  for i := 0 to filesToTest.Count-1 do begin
    info := TFileRunInfo(filesToTest.Objects[i]);
    if info = nil then begin
      fi := i;
      Break;
    end else begin
      if info.IsRunning then
        inc(used);
    end;
  end;

  // scheduling the new files
  if (fi>=0) then begin
    for i := fi to filesToTest.Count-1 do begin
      info := TFileRunInfo(filesToTest.Objects[i]);
      if info <> nil then continue;

      info := TFileRunInfo.Create;

      if ftempDir<>'' then begin
        info.tempDir := IncludeTrailingPathDelimiter(ftempDir)+ExtractFileName(filesToTest[i]);
        ForceDirectories(info.tempDir);
      end;

      info.Start(filesToTest[i], subject);
      filesToTest.Objects[i] := info;
      inc(used);
      if (used >= maxPara) then break;
    end;
  end;
  Result := used;
end;

procedure TTestPerformer.StartDirSearch;
var
  i    : integer;
  srch : TAsyncFileSearch;
begin
  for i:=0 to dirToTest.Count-1 do begin
    srch := TAsyncFileSearch.Create;
    Log('searching directory: %s', [dirToTest[i]]);
    srch.StartSearch(dirToTest[i]);
    dirToTest.Objects[i] := srch;
  end;
end;

constructor TTestPerformer.Create;
begin
  inherited Create;
  OwnResult := true;
  filesToTest := TStringList.Create;
  dirToTest   := TStringList.Create;
  fileExt := TStringList.Create;
  fileExt.Add('.tst');
  fileExt.Add('.test');
  fileExt.Add('.testbat');
end;

destructor TTestPerformer.Destroy;
var
  i : integer;
begin
  Abort;
  fileExt.Free;
  if OwnResult then
    for i := 0 to filesToTest.Count-1 do begin
      filesToTest.Objects[i].Free;
      filesToTest.Objects[i] := nil;
    end;
  filesToTest.Free;
  dirToTest.Free;
  inherited Destroy;
end;

procedure TTestPerformer.Init(Target: TStringList; const inp: TTestInput);
var
  d           : string;
  i           : integer;
begin
  // initial collecting  of files and directories to test
  cancel := false;
  maxPara := inp.maxPara;
  subject := inp.subject;
  ftempDir := inp.tempDir;

  if (maxPara <= 0) then maxPara := DEFAULT_PARALLEL;

  for i:=0 to Target.Count-1 do begin
    d := Target[i];
    if DirectoryExists(d) then
      dirToTest.Add(d)
    else
      filesToTest.Add(d);
  end;
end;

procedure TTestPerformer.StartTests;
begin
  if Assigned(ctrlThread) then Exit;

  dirDone := dirToTest.Count=0;
  ctrlThread := TControlThread.Create(Self);
  ctrlThread.Start;
end;

function TTestPerformer.IsDone: Boolean;
begin
  Result := Assigned(ctrlThread)
       and (ctrlThread.Finished)
       and dirDone;
end;

procedure TTestPerformer.Abort;
begin
  if Assigned(ctrlThread) then begin
    cancel := true;
    ctrlThread.WaitFor;
    ctrlThread.Free;
    ctrlThread := nil;
  end;
end;

function TTestPerformer.GetResults(dst: TStrings): Boolean;
var
  i : integer;
begin
  Result := IsDone;
  if dst = nil then Exit;
  for i:=0 to filesToTest.Count-1 do begin
    if Assigned(filesToTest.Objects[i]) then
      dst.AddObject(filesToTest[i], filesToTest.Objects[i]);
  end;
end;

procedure InitTestInput(out inp: TTestInput);
begin
  inp.subject := '';
  inp.maxPara := DEFAULT_PARALLEL;
end;

end.

