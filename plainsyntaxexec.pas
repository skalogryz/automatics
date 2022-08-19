unit plainsyntaxexec;

interface

{$mode delphi}{$H+}

uses
  {$ifdef mswindows}
  // this is only for error messages
  Windows,
  {$endif}
  Classes, SysUtils,
  fpexprpars, ExtraFileUtils,
  runproctypes, plainsyntaxtype, runtesttypes;

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

  // This is a delegate for the plain syntax executor. It's primary purpose
  // is to log the execution of commands.
  // Implementing the delegate is optional. If not provided the executor
  // creates it's own silent delegate that does nothing.
  // The use of "class" instead of an "interface" is to allow adding new
  // "default" be
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
    CurRes  : TCommandExecResult;
    CurCmd  : TPlainCommand;
    procedure ExecExpect(c: TPlainCommand; res: TCommandExecResult);
    function ExecProcess(c: TPlainCommand; res: TCommandExecResult; isTestProc: Boolean): Boolean;
    function ExecCommand(c: TPlainCommand): TCommandExecResult;
    procedure ErrorMsg(const Msg: string);
    procedure EchoMsg(const Msg: string);
    procedure Log(const msg: string);

    function CopyCmd(src: TPlainCommand): TPlainCommand;
  public
    LastExitCode  : Integer;
    LastStdOutFn  : string;
    LastStdErrFn  : string;

    ToEnv         : TStringList;
    CurTimeOut    : Integer;
    DefTimeOut    : Integer; // the default timeout.
    CurDir        : string;
    HasTestResult : Boolean; // set to true, if commands did include any commands
    FinalResult   : TTestResult;
    DefaultResult : TTestResult;
    CommandLogs   : TList; // array of TCommandExecResult
    Params        : TStringList; // key/value
    Delegate      : TPlainSyntaxExecEnv;
    FailMessage   : string;
    UnrunReason   : string;
    isTimeout     : Boolean;
    RunTempDir    : string;  // target temp dir
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


type
  TCustomFuncResultType = (
    tpError,
    tpInt,
    tpString,
    tpBool
  );

  TCustomFuncResult = record
    resType : TCustomFuncResultType;
    int  : Integer;
    str  : String;
    bool : Boolean;
  end;

  TFuncExecContext = record
    fnname : string; // function name that's being called.
                     // used for the cases when the same method is used to implement different functions
    curdir : string; // current thread for the exec
  end;

  TCustomCondFunc = procedure(
    const execContext: TFuncExecContext;
    const args: array of string;
    var res: TCustomFuncResult) of object;

function RegisterCondFunc(const nm: string; fn: TCustomCondFunc;
  res: TCustomFuncResultType;
  const params: array of TCustomFuncResultType): Boolean;
procedure RegisterFuncInExprParser(x: TFPExpressionParser);

procedure RegisterFuncInExprParserForContext(x: TFPExpressionParser; context: TPlainSyntaxExec; tags: TList {of object, that to be freed});

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
  if finalCmd.cmd=pcEcho then Exit;
  DoLog('---- Executing command: ' + PlainCmdStr[finalCmd.cmd]+' ---- ');
  if finalCmd.args.Count>0 then
    DoLog('Arguments: ');
  for i:=0 to finalCmd.args.Count-1 do
    DoLog(' - '+ finalCmd.args[i]);
  if finalCmd.cmd <> rawcmd.cmd then begin
    DoLog('Raw command: '+TemplatesToStr(rawCmd.tmp));
  end;
end;

procedure TPlainSyntaxExecStdErrEnv.CommandFinished(rawCmd: TPlainCommand;
  res: TCommandExecResult);
begin
  if res.cmd.cmd = pcEcho then Exit;
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

procedure TPlainSyntaxExec.ExecExpect(c: TPlainCommand; res: TCommandExecResult);
var
  cond : string;
  x    : TFPExpressionParser;
  xr   : TFPExpressionResult;
  rb   : boolean;
  tags : TList;
  i    : integer;
const
  BoolRes : array [boolean] of TTestResult = (trFail, trSuccess);
begin
  res.hasTestResult := true;
  cond := Trim(ArgsToOneLine(c.args));
  Log('condition: '+ cond);
  if cond ='' then begin
    res.testResult := trUnableToRun;
    ErrorMsg(InvalidParams);
    Exit;
  end;

  x := TFPExpressionParser.Create(nil);
  tags := TList.Create;
  try
    try
      x.Identifiers.AddIntegerVariable('exitcode', LastExitCode);
      x.Identifiers.AddIntegerVariable('errorlevel', LastExitCode);
      x.Identifiers.AddStringVariable('stdoutfn', LastStdOutFn);
      x.Identifiers.AddStringVariable('stderrfn', LastStdErrFn);
      RegisterFuncInExprParserForContext(x, Self, tags);

      x.Expression := cond;
      xr := x.Evaluate;
      case xr.ResultType of
        rtBoolean : rb := xr.ResBoolean;
        rtInteger : rb := xr.ResInteger <> 0;
        rtString  : rb := xr.ResString <> '';
        rtFloat   : rb := xr.ResFloat <> 0;
        {$if FPC_FULlVERSION >= 030200}
        rtCurrency : rb := xr.ResCurrency <> 0;
        {$endif}
      else
        res.testResult := trUnableToRun;
        ErrorMsg('invalid condition expression '+ cond);
        Exit;
      end;
      res.testResult := BoolRes[rb];
    finally
      x.Free;
      for i:=0 to Tags.Count-1 do TObject(tags[i]).Free;
      tags.Free;
    end;
  except
    on e: exception do begin
      res.testResult := trUnableToRun;
      UnRunReason := 'error while evaluating '+ e.Message;
      ErrorMsg(UnRunReason);
    end;
  end;
end;

function GetSystemMessageProcFail(code: integer): string;
begin
  Result := 'err code: '+IntToStr(code);
  {$ifdef mswindows}
  case code of
    0: Result := ''; // all is well
    ERROR_FILE_NOT_FOUND: Result := 'File not found. ('+Result+')';
  end;
  {$endif}
end;

// returns true, if the process WAS able to run
// and did finish in time.
// False: otherwise
function TPlainSyntaxExec.ExecProcess(c: TPlainCommand; res: TCommandExecResult; isTestProc: Boolean): Boolean;
var
  i : integer;
  p : TRunProcess;
begin
  res.procRun := true;
  if c.args.Count=0 then begin
    res.testResult := trUnableToRun;
    res.hasTestResult := true;
    ErrorMsg(InvalidParams);
    Exit;
  end;

  res.procInp.exec := SlashToNative(c.args[0]);
  res.procInp.timeOutMs := CurTimeOut;
  res.procInp.rundir := CurDir;
  res.procInp.stdErrToFile:=sdPipeOnly;
  res.procInp.stdOutToFile:=sdPipeOnly;

  SetLength(res.procInp.args, c.args.Count-1);
  for i:=1 to c.args.Count-1 do
    res.procInp.args[i-1]:=c.args[i];

  SetLength(res.procInp.env, ToEnv.Count);
  for i:= 0 to ToEnv.Count-1 do
    res.procInp.env[i] := Format('%s=%s', [ToEnv[i], Params.Values[ToEnv[i]] ]);

  if RunTempDir = '' then begin
    ProcPrepareTempDir(res.procInp);
  end else begin
    res.procInp.tempDir := IncludeTrailingPathDelimiter(RunTempDir)+'procLn_'+IntToStr(c.lineNum)+'_'+c.cmdlow;
  end;

  Log('running process: "'+ res.procInp.exec+'"');
  if length(res.procInp.args) > 0 then begin
    Log('arguments: ' );
    for i := 0 to length(res.procInp.args)-1 do begin
      Log('  '+res.procInp.args[i]);
    end;
  end;
  if length(res.procInp.env) > 0 then begin
    Log('env vars: ' );
    for i := 0 to length(res.procInp.env)-1 do begin
      Log('  '+res.procInp.env[i]);
    end;
  end;
  Log('using timeout: ' + intToStr(CurTimeOut));
  if (res.procInp.timeOutMs>0) then
    Log('process timeout: '+IntToStr(res.procInp.timeOutMs));

  p := ProcStart(res.procInp);
  try
    ProcWaitLoop(p);
    p.GetResults(res.procRes);

    if res.procRes.runError <> 0 then begin
      ErrorMsg('failed to run the process '+IntToStr(res.procRes.runSysErr));
      UnrunReason:=GetSystemMessageProcFail(res.procRes.runSysErr);
    end else if res.procRes.timedOut then begin
      UnrunReason:='process timed out '+IntToStr(res.procRes.runTimeMs)+' ms';
      isTimeout := true;
      ErrorMsg(UnrunReason);
    end else
      Log('process finished with code: '+IntToStr(res.procRes.exitCode));
    Log('temp directory: '+ res.procRes.tempDir);
    if res.procRes.outSize>0 then begin
      Log('std out size: '+IntToStr(res.procRes.outSize));
      Log('std out file: '+res.procRes.stdOutFn);
    end;
    if res.procRes.errSize>0 then begin
      Log('std err size: '+IntToStr(res.procRes.errSize));
      Log('std err file: '+res.procRes.stdErrFn);
    end;

    Result := (res.procRes.runError = 0)
      and not (res.procRes.timedOut);

    if (isTestProc) and not Result then begin
      // by default "run" of the test doesn't set the result of the test
      // it's only set by the "assess" (or "expect", "assert") functions
      // However, if "run" process FAILS to execute, the testResult is set to "fail to run"
      res.testResult := trUnableToRun;
      res.hasTestResult := true;
    end;
  finally
    p.Free;
  end;

end;

function TPlainSyntaxExec.ExecCommand(c: TPlainCommand): TCommandExecResult;
var
  ran : Boolean;
  isRun: Boolean;
begin
  if c = nil then begin
    Result := nil;
    Exit;
  end;

  Result := TCommandExecResult.Create;
  ran := true;

  CurRes := Result;
  CurCmd := c;
  Result.cmd := c;
  try
    case c.cmd of
      pcCd: begin
        ran := c.args.Count>1;
        if not ran then begin
          ErrorMsg(InvalidParams);
        end;
        CurDir := SlashToNative(c.args[0]);
      end;
      pcEcho:
        EchoMsg(ArgsToOneLine(c.args));
      pcSetVar, pcSetVarEnv: begin
        if (c.args.Count>0) then
           Params.Values[c.varname] := c.args[0];
        if (c.cmd = pcSetVarEnv) then ToEnv.Add(c.varname);
      end;
      pcEnv:
        ToEnv.Add(c.varname);
      pcExec: begin
        if (c.cmdlow = CMD_FAILMSG) then begin
          c.Args.Delete(0);
          failMessage := ArgsToOneLine(c.args);
        end else if (c.cmdlow = CMD_TIMEOUT) then begin
          c.Args.Delete(0);
          if not GetTimeOutMs(ArgsToOneLine(c.args), CurTimeOut) then begin
            Log('Invalid time out line: ' + ArgsToOneLine(c.args)+', defaulting');
            CurTimeOut := DefTimeOut;
          end;
          Log('Time out is: '+IntToStr(CurTimeOut));
        end else if (c.cmdlow = CMD_EXPECT) then begin
          c.Args.Delete(0);
          ExecExpect(c, result);
        end else begin
          isRun := c.cmdLow = CMD_RUN;
          if isRun then c.args.Delete(0);

          LastExitCode := 0;
          if ExecProcess(c, result, isRun) then begin
            LastExitCode := result.procRes.exitCode;
            LastStdOutFn := result.procRes.stdOutFn;
            LastStdErrFn := result.procRes.stdErrFn;
          end;
        end;
      end;
    end;
  finally
    CurCmd := nil;
    CurRes := nil;
    Result.cmdran := ran;
    Result.cmd := c;
  end;
end;

procedure TPlainSyntaxExec.ErrorMsg(const Msg: string);
begin
  if Assigned(CurRes) then begin
    if CurRes.err = ''
      then CurRes.err := Msg
      else CurRes.err := CurRes.err + #13#10 + Msg;
    Delegate.ErrorMsg(Msg, CurRes.cmd);
  end else
    Delegate.ErrorMsg(Msg, nil);
end;

procedure TPlainSyntaxExec.EchoMsg(const Msg: string);
begin
  Delegate.EchoMsg(Msg, CurCmd);
end;

procedure TPlainSyntaxExec.Log(const msg: string);
begin
  delegate.LogMsg(msg, CurCmd);
end;

function TPlainSyntaxExec.CopyCmd(src: TPlainCommand): TPlainCommand;
var
  c : TPlainCommand;
begin
  c := TPlainCommand.Create;
  c.syntax := src.syntax;
  c.lineNum := src.lineNum;
  c.tmp := CopyTemplateLine(src.tmp);
  c.ParseCommand(Params);
  Result := c;
end;

constructor TPlainSyntaxExec.Create;
begin
  inherited Create;
  DefTimeOut := -1;
  CurTimeOut := -1;
  CurDir := GetCurrentDir;
  DefaultResult := trUnableToRun;
  ToEnv := TStringList.Create;
  ToEnv.Duplicates := dupIgnore;
  CommandLogs := TList.Create; // array of TCommandExecResult
  Params := TStringList.Create;
end;

destructor TPlainSyntaxExec.Destroy;
var
  i : integer;
begin
  Params.Free;
  for i:=0 to CommandLogs.Count-1 do
    TObject(CommandLogs[i]).Free;
  CommandLogs.Free;
  ToEnv.Free;
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

    if not HasTestResult then begin
      Delegate.LogMsg('the script didn''t contain any result assessment, assuming default result ('+TestResultNameStr[DefaultResult]+')',nil);
      HasTestResult := true;
      FinalResult := DefaultResult;
      if FinalResult = trUnableToRun then UnrunReason := 'no "expect" command specified';
    end;
    Delegate.LogMsg('the script result ('+TestResultNameStr[FinalResult]+')',nil);

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


type

  { TCustomFuncRecord }

  TCustomFuncRecord = class(TObject)
  public
    name     : string;
    customFn : TCustomCondFunc;
    paramStr : string;
    resType  : TCustomFuncResultType;

    context  : TPlainSyntaxExec;
    procedure ExprParsFunc(Var Result : TFPExpressionResult; Const Args : TExprParameterArray);
    procedure CopyFrom(ASrc: TCustomFuncRecord);
  end;

{ TCustomFuncRecord }

procedure TCustomFuncRecord.ExprParsFunc(var Result: TFPExpressionResult;
  const Args: TExprParameterArray);
var
  astr : array of string;
  i    : integer;
  res  : TCustomFuncResult;
  ctx  : TFuncExecContext;
const
  BoolToStr : array [boolean] of string = ('0','1');
begin
  SetLength(astr, length(Args));
  for i:=0 to length(args)-1 do begin
    case args[i].ResultType of
      rtBoolean  : astr[i] := BoolToStr[args[i].ResBoolean];
      rtInteger  : astr[i] := IntTostr(args[i].ResInteger);
      rtFloat    : astr[i] := FloatTostr(args[i].ResFloat);
      {$if FPC_FULlVERSION >= 030200}
      rtCurrency : astr[i] := FloatTostr(args[i].ResCurrency);
      {$endif}
      rtDateTime : astr[i] := FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', args[i].ResDateTime);
    else
      astr[i]:=args[i].ResString;
    end;
  end;

  res.resType := resType;
  res.int := 0;
  res.bool := false;
  res.str := '';

  ctx.fnname := name;

  if Assigned(context) then begin
    ctx.curdir := context.curdir;
  end;

  try
    customFn(ctx, astr, res);
  except
  end;
  if resType = tpInt then begin
    Result.ResultType := rtInteger;
    Result.ResInteger := res.int;
  end else if resType = tpString then begin
    Result.ResultType := rtString;
    Result.ResString := res.str;
  end else begin
    Result.ResultType := rtBoolean;
    Result.ResBoolean := res.bool;
  end;
end;

procedure TCustomFuncRecord.CopyFrom(ASrc: TCustomFuncRecord);
begin
  if not Assigned(Asrc) then Exit;
  name     := ASrc.name;
  customFn := ASrc.customFn;
  paramStr := ASrc.paramStr;
  resType  := ASrc.resType;
  context  := ASrc.context;
end;

var
  CustomFuncReg : TStringList;

const
  ResTypeToChar : array [TCustomFuncResultType] of char = (
    #0  // tpError
   ,'I' // tpINt
   ,'S' // tpStr
   ,'B' // tpBool
  );

function RegisterCondFunc(const nm: string; fn: TCustomCondFunc; res: TCustomFuncResultType; const params: array of TCustomFuncResultType): Boolean;
var
  r : TCustomFuncRecord;
  i : integer;
  l : string;
  ps : string;
begin
  Result := false;
  if (res = tpError) then Exit;
  for i := 0 to length(params)-1 do
    if params[i] = tpError then Exit;
  if (@fn = nil) then Exit;

  Result := true;

  l := AnsiLowerCase(nm);
  i := CustomFuncReg.IndexOf(l);
  if i<0 then begin
    r := TCustomFuncRecord.Create;
    r.Name := l;
    CustomFuncReg.AddObject(l, r);
  end;
  r.customFn := fn;
  ps := '';
  for i := 0 to length(params)-1 do begin
    ps := ps + ResTypeToChar[params[i]];
  end;
  r.paramStr := ps;
  r.resType := res;
end;

procedure FreeCustomFuncReg;
begin
  CustomFuncReg.Free;
end;


procedure RegisterFuncInExprParser(x: TFPExpressionParser);
var
  i  : integer;
  r  : TCustomFuncRecord;
begin
  if not Assigned(x) then Exit;
  for i:=0 to CustomFuncReg.Count-1 do
  begin
    r := TCustomFuncRecord(CustomFuncReg.Objects[i]);
    if r = nil then Continue;
    x.Identifiers.AddFunction(r.name, ResTypeToChar[r.ResType], r.paramStr, r.ExprParsFunc);
  end;
end;

procedure RegisterFuncInExprParserForContext(x: TFPExpressionParser; context: TPlainSyntaxExec; tags: TList {of object, that to be freed});
var
  i   : integer;
  src : TCustomFuncRecord;
  r   : TCustomFuncRecord;
begin
  if not Assigned(x) or not Assigned(tags) then Exit;

  for i:=0 to CustomFuncReg.Count-1 do
  begin
    src := TCustomFuncRecord(CustomFuncReg.Objects[i]);
    if src = nil then Continue;

    r := TCustomFuncRecord.Create;
    r.CopyFrom(src);
    r.context := context;
    x.Identifiers.AddFunction(r.name, ResTypeToChar[r.ResType], r.paramStr, r.ExprParsFunc);
    tags.Add(r);
  end;
end;

initialization
  CustomFuncReg := TStringList.Create;
  CustomFuncReg.OwnsObjects := true;
  CustomFuncReg.CaseSensitive := false;

finalization
  FreeCustomFuncReg;

end.
