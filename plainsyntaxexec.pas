unit plainsyntaxexec;

interface

{$mode delphi}{$H+}

uses
  Classes, SysUtils, runproctypes, plainsyntaxtype, runtesttypes;

type

  { TCommandExecResult }

  TCommandExecResult = class(TObject)
    cmdran        : Boolean;
    cmd           : TPlainCommand;
    ownCmd        : Boolean;
    err           : string;

    // for the run process command
    procRun       : Boolean;    // set to true if "run" command is used
    procInp       : TRunInput;  // input parameters for the command
    procRes       : TRunResult; // the result of the process

    hasTestResult : Boolean;
    testResult    : TTestResult;
    constructor Create;
    destructor Destroy; override;
  end;

  { TPlainSyntaxExecEnv }

  TPlainSyntaxExecEnv = class(TObject)
  public
    procedure StartCommand(const rawCmd, finalCmd: TPlainCommand); virtual;
    procedure CommandFinished(rawCmd: TPlainCommand; res: TCommandExecResult); virtual;
    procedure EchoMsg(const msg: string; cmd: TPlainCommand); virtual;
    procedure ErrorMsg(const msg: string; cmd: TPlainCommand); virtual;
    procedure LogMsg(const msg: string; cmd: TPlainCommand); virtual;
  end;

  { TPlainSyntaxExec }

  TPlainSyntaxExec = class(TObject)
  protected
    CurCmd  : TCommandExecResult;
    function ExecProcess(c: TPlainCommand; res: TCommandExecResult; isTestProc: Boolean): Boolean;
    function ExecCommand(c: TPlainCommand): TCommandExecResult;
    procedure ErrorMsg(const Msg: string);
    procedure EchoMsg(const Msg: string);
    procedure Log(const msg: string);

    function CopyCmd(src: TPlainCommand): TPlainCommand;
  public
    LastExitCode  : Integer;

    CurTimeOut    : Integer;
    CurDir        : string;
    HasTestResult : Boolean; // set to true, if commands did include any commands
    FinalResult   : TTestResult;
    CommandLogs   : TList; // array of TCommandExecResult
    Params        : TStringList; // key/value
    Delegate      : TPlainSyntaxExecEnv;
    constructor Create;
    destructor Destroy; override;
    procedure RunCommands(cmds: TList);
    procedure Clear;
  end;

function ReplaceParams(const cmd : string; params: TStrings): string;


type

  { TPlainSyntaxExecStdErrEnv }

  TPlainSyntaxExecStdErrEnv = class(TPlainSyntaxExecEnv)
  protected
    procedure DoLog(const m: string);
  public
    procedure StartCommand(const rawCmd, finalCmd: TPlainCommand); override;
    procedure CommandFinished(rawCmd: TPlainCommand; res: TCommandExecResult); override;
    procedure EchoMsg(const msg: string; cmd: TPlainCommand); override;
    procedure ErrorMsg(const msg: string; cmd: TPlainCommand); override;
    procedure LogMsg(const msg: string; cmd: TPlainCommand); override;
  end;

implementation

const
  TestIsUnableToRun = 'the test cannot be executed';
  InvalidParams = 'invalid parameters';

function ReplaceParams(const cmd : string; params: TStrings): string;
var
  i : integer;
  j : integer;
  n : integer;
  nm : string;
begin
  i := 1;
  j := 1;
  Result := '';
  while (i <= length(cmd)) do begin
    if cmd[i]='%' then begin
      Result := Result + Copy(cmd, j, i-j);
      inc(i);
      if (i<=length(cmd)) and (cmd[i]='%') then begin
        Result:=Result+'%';
        inc(i);
      end else begin
        n := i;
        while (i<= length(cmd)) and (cmd[i]<>'%') do inc(i);
        nm := Copy(cmd, n, i-n);
        nm := AnsiLowerCase(nm);
        Result := Result + Params.Values[nm];
        inc(i);
      end;
      j:=i;
    end else
      inc(i);
  end;
  i:=length(cmd)+1;
  Result := REsult+Copy(cmd, j, i-j);
end;

{ TPlainSyntaxExecStdErr }

procedure TPlainSyntaxExecStdErrEnv.DoLog(const m: string);
begin
  writeln(stderr,FormatDateTime('hh:nn:ss:zzz', now),': ',m);
end;

procedure TPlainSyntaxExecStdErrEnv.StartCommand(const rawCmd,
  finalCmd: TPlainCommand);
var
  i : integer;
begin
  if finalCmd.cmd='echo' then Exit;
  DoLog('---- Executing command: ' + finalCmd.cmd+' ---- ');
  if finalCmd.args.Count>0 then
    DoLog('Arguments: ');
  for i:=0 to finalCmd.args.Count-1 do
    DoLog(' - '+ finalCmd.args[i]);
  if finalCmd.cmd <> rawcmd.cmd then begin
    DoLog('Raw command: '+Trim(rawCmd.lines.Text));
  end;
end;

procedure TPlainSyntaxExecStdErrEnv.CommandFinished(rawCmd: TPlainCommand;
  res: TCommandExecResult);
begin
  if res.cmd.cmd ='echo' then Exit;
  DoLog('---- Command Finished -----');
end;

procedure TPlainSyntaxExecStdErrEnv.EchoMsg(const msg: string; cmd: TPlainCommand);
begin
  DoLog('ECHO: '+ msg);
end;

procedure TPlainSyntaxExecStdErrEnv.ErrorMsg(const msg: string; cmd: TPlainCommand
  );
begin
  DoLog('ERROR: '+ msg);
end;

procedure TPlainSyntaxExecStdErrEnv.LogMsg(const msg: string; cmd: TPlainCommand);
begin
  DoLog(msg);
end;

{ TPlainSyntaxExecEnv }

procedure TPlainSyntaxExecEnv.StartCommand(const rawCmd, finalCmd: TPlainCommand
  );
begin

end;

procedure TPlainSyntaxExecEnv.CommandFinished(rawCmd: TPlainCommand;
  res: TCommandExecResult);
begin

end;

procedure TPlainSyntaxExecEnv.EchoMsg(const msg: string; cmd: TPlainCommand);
begin

end;

procedure TPlainSyntaxExecEnv.ErrorMsg(const msg: string; cmd: TPlainCommand);
begin

end;

procedure TPlainSyntaxExecEnv.LogMsg(const msg: string; cmd: TPlainCommand);
begin

end;

{ TCommandExecResult }

constructor TCommandExecResult.Create;
begin
  ownCmd := true;
end;

destructor TCommandExecResult.Destroy;
begin
  if ownCmd then cmd.Free;
  inherited Destroy;
end;


{ TPlainSyntaxExec }

// returns true, if the process WAS able to run
// and did finish in time.
// False: otherwise
function TPlainSyntaxExec.ExecProcess(c: TPlainCommand; res: TCommandExecResult; isTestProc: Boolean): Boolean;
var
  i : integer;
  p : TRunProcess;
begin
  res.procRun := true;
  res.hasTestResult := isTestProc;
  if c.args.Count=0 then begin
    res.testResult := trUnableToRun;
    ErrorMsg(InvalidParams);
    Exit;
  end;

  res.procInp.exec := c.args[0];
  res.procInp.timeOutMs := CurTimeOut;
  res.procInp.rundir := CurDir;
  SetLength(res.procInp.args, c.args.Count-1);
  for i:=1 to c.args.Count-1 do
    res.procInp.args[i-1]:=c.args[i];

  ProcPrepareTempDir(res.procInp);

  Log('running process: "'+ res.procInp.exec+'"');
  if (res.procInp.timeOutMs>0) then
    Log('process timeout: '+IntToStr(res.procInp.timeOutMs));

  p := ProcStart(res.procInp);
  try
    ProcWaitLoop(p);
    p.GetResults(res.procRes);

    if res.procRes.runError <> 0 then begin
      ErrorMsg('failed to run the process '+IntToStr(res.procRes.runSysErr));
    end else if res.procRes.timedOut then begin
      ErrorMsg('process timed out  '+IntToStr(res.procREs.runTimeMs));
    end else
      ErrorMsg('process finished with code: '+IntToStr(res.procRes.exitCode));

    Result := (res.procRes.runError <> 0)
      and not (res.procRes.timedOut);
    if (isTestProc) then begin
      if Result then
        // preliminary
        res.testResult := trSuccess
      else
        res.testResult := trUnableToRun
    end;
  finally
    p.Free;
  end;

end;

function TPlainSyntaxExec.ExecCommand(c: TPlainCommand): TCommandExecResult;
var
  ran : Boolean;
begin
  if c = nil then begin
    Result := nil;
    Exit;
  end;

  Result := TCommandExecResult.Create;
  ran := true;

  CurCmd := Result;
  Result.cmd := c;
  try
    if (c.cmd = 'cd') then begin
      ran := c.args.Count>1;
      if not ran then begin
        ErrorMsg(InvalidParams);
      end;
      CurDir := c.args[0]
    end else if (c.cmd = 'echo') then begin
      EchoMsg(ArgsToOneLine(c.args));
    end else if (c.cmd = 'run') then begin
      LastExitCode := 0;
      if ExecProcess(c, result, true) then
        LastExitCode := result.procRes.exitCode;
    end else
      ran := false;
  finally
    CurCmd := nil;
    Result.cmdran := ran;
    Result.cmd := c;
  end;
end;

procedure TPlainSyntaxExec.ErrorMsg(const Msg: string);
begin
  if Assigned(CurCmd) then begin
    if CurCmd.err = ''
      then CurCmd.err := Msg
      else CurCmd.err := CurCmd.err + #13#10 + Msg;
  end;
  Delegate.ErrorMsg(Msg, CurCmd.cmd);
  // do nothing yet
end;

procedure TPlainSyntaxExec.EchoMsg(const Msg: string);
begin
  Delegate.EchoMsg(Msg, CurCmd.cmd);
end;

procedure TPlainSyntaxExec.Log(const msg: string);
begin
  delegate.LogMsg(msg, CurCmd.cmd);
end;

function TPlainSyntaxExec.CopyCmd(src: TPlainCommand): TPlainCommand;
var
  c : TPlainCommand;
  i : integer;
begin
  c := TPlainCommand.Create;
  c.lines.Assign(src.lines);

  c.ParseCommand;
  c.cmd := ReplaceParams(c.cmd, Params);
  for i:=0 to c.args.Count-1 do
    c.args[i] := ReplaceParams(c.args[i], Params);
  Result := c;
end;

constructor TPlainSyntaxExec.Create;
begin
  inherited Create;

  CurTimeOut := -1;
  CurDir := GetCurrentDir;

  CommandLogs := TList.Create; // array of TCommandExecResult
  Params := TStringList.Create;
end;

destructor TPlainSyntaxExec.Destroy;
begin
  Params.Free;
  CommandLogs.Free;
  inherited Destroy;
end;

procedure TPlainSyntaxExec.RunCommands(cmds: TList);
var
  src : TPlainCommand;
  c   : TPlainCommand;
  i   : integer;
  res : TCommandExecResult;
  ownDel : Boolean;
begin
  ownDel := not Assigned(Delegate);
  if ownDel then
    Delegate := TPlainSyntaxExecEnv.Create;

  try
    HasTestResult := false;
    FinalResult := trUnableToRun;

    for i := 0 to cmds.Count-1 do begin
      src := TPlainCommand(cmds[i]);
      c := CopyCmd(src);

      Delegate.StartCommand(src, c);
      res := ExecCommand(c);
      CommandLogs.Add(res);
      if not res.cmdran then Delegate.LogMsg('Unregonized command', c);
      Delegate.CommandFinished(src, res);

      HasTestResult := res.hasTestResult;
      FinalResult := res.testResult;
      if (HasTestResult) and (FinalResult = trUnableToRun) then begin
        ErrorMsg(TestIsUnableToRun);
        Break;
      end;
    end;
  finally
    if ownDel then Delegate.Free;
  end;

end;

procedure TPlainSyntaxExec.Clear;
var
  i : integer;
begin
  for i := 0 to CommandLogs.Count-1 do
    TObject(CommandLogs[i]).Free;
  CommandLogs.Clear;
end;

end.
