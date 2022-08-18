unit plainsyntaxtype;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ExtraFileUtils, batchparser, shparser, parseutils;

type
  TScriptSyntax = class;
  TScriptSyntaxClass = class of TScriptSyntax;
  TTemplateLine = class;

  TPlainCmd = (
    pcExec       // execute anything
   ,pcSetVar     // set variable
   ,pcSetVarEnv  // set variable and make it environmental variable
   ,pcEnv        // set variable to be environmental variable
   ,pcEcho       // echo
   ,pcCd         // change directory
   ,pcNone       // a placeholder for not supported functionality

   // test specific :(.. it would be nice to keep them as "pcExt" and
   //                    have them recorded elsewher
   //,pcFailMsg
   //,pcTimeout
   //,pcExpect
  );

  TPlainCmdOpts = set of(
    pcoMath  // mathematical expression
   ,pcoNoEcho // @ commant for win batch
  );

  { TPlainCommand }

  TPlainCommand = class(TObject)
    syntax  : TScriptSyntaxClass;
    tmp     : TTemplateLine;
    cmd     : TPlainCmd;
    cmdlow  : string;
    cmdopts : TPlainCmdOpts;
    args    : TStringList;
    varname : string;  // use for
    lineNum : Integer; // line number in the file
    constructor Create;
    destructor Destroy; override;
    // if vars is nil, then all variables will be substituted to empty values
    procedure ParseCommand(vars: TStrings);
  end;

  { TTemplateLine }

  TTemplateLine = class(TObject)
  public
    text    : string;
    isVar   : boolean;
    varName : string;
    next    : TTemplateLine;
    func    : string; // the function to be applied to the value
    funcparams : TStringList;
    constructor Create; overload;
    constructor Create(const atxt: string); overload;
    constructor Create(const atxt, aname: string); overload;
    procedure PushFuncParam(const paramVal: string);
    destructor Destroy; override;
  end;


  TSyntaxError = record
    err : string;
    pos : integer;
  end;


  TScriptSyntaxResult = (srError, srTemplateAvail, srNeedMoreLines, srComment);

  { TScriptSyntax }

  TScriptSyntax = class(TObject)
  public
    constructor Create; virtual;

    function FeedLine(const s: string; var err: TSyntaxError): TScriptSyntaxResult; virtual;
    procedure FeedEndOfFile(var err: TSyntaxError); virtual;
    function GetNextTemplate(var err: TSyntaxError; out tmp: TTemplateLine): Boolean; virtual;

    class function IsCaseSensitive: Boolean; virtual;

    class function ParseComamnd(const ln: string; dst : TPlainCommand; var err: TSyntaxError): Boolean; virtual;

    // breaks out the line into the list of arguments.
    // the input line doesn't contain any variables.
    class procedure LineToArgs(const ln: string; dst: TStrings; var idx: Integer; var err: TSyntaxError); virtual;

    class function PathsToScriptNative(const pth: string): string; virtual;
  end;

  { TPlainParser }

  TPlainParser = class(TObject)
  private
    lineCount: Integer;
    ctx      : TScriptSyntax;
    procedure GatherCommands;
  public
    commands : TList;
    syntax   : TScriptSyntaxClass;
    constructor Create;
    destructor Destroy; override;
    procedure ParseLine(const ins: string);
    procedure EndOfFile;
  end;

function ReadPlainCommandFile(const fn: string): TList;

// converting TStrings to a single string
function ArgsToOneLine(s : TStrings): string;

const
  CMD_CD   = 'cd';  // change directory
  CMD_ECHO = 'echo';
  CMD_RUN  = 'run'; // run

  CMD_EXPECT = 'expect'; // all of those commands are alias to "expect"
  CMD_ASSES  = 'assess';
  CMD_ASSERT = 'assert';

  CMD_FAILMSG = 'failmsg';
  CMD_TIMEOUT = 'timeout';

  CMD_STORE   = 'store'; // probe,

  PlainCmdStr : array [TPlainCmd] of string = (
    'exec'       // execute anything
   ,'setVar'     // set variable
   ,'setVarEnv'  // set variable and make it environmental variable
   ,'env'        // set variable to be environmental variable
   ,'echo'       // echo
   ,'cd'
   ,'none'
   // test specific
   // ,'failmsg'
   // ,'timeout'
   // ,'expect'
  );

function GetTimeOutMs(const ts: string; out ms: Integer): Boolean;

type
  { TBatSyntax }

  TBatSyntax = class(TScriptSyntax)
  public
    retain : Boolean;
    intbuf : string;
    constructor Create; override;

    function FeedLine(const s: string; var err: TSyntaxError): TScriptSyntaxResult; override;
    procedure FeedEndOfFile(var err: TSyntaxError); override;
    function GetNextTemplate(var err: TSyntaxError; out tmp: TTemplateLine): Boolean; override;

    function ParseTemplateLine(const buf: string; var err: TSyntaxError): TTemplateLine;

    function IsComment(const s: string): boolean;
    class function IsCaseSensitive: Boolean; override;
    class procedure LineToArgs(const buf: string; args: TStrings; var idx: integer; var err: TSyntaxError); override;
    class function PathsToScriptNative(const pth: string): string; override;
    class function ParseComamnd(const ln: string; dst : TPlainCommand; var err: TSyntaxError): Boolean; override;
  end;

  { TShSyntax }

  TShSyntax = class(TScriptSyntax)
  protected
    b         : string;
    retain    : Boolean;
    waitQuote : char;
  public
    constructor Create; override;

    function ParseTemplateLine(const buf: string; var idx: Integer;var err: TSyntaxError): TTemplateLine;

    function FeedLine(const s: string; var err: TSyntaxError): TScriptSyntaxResult; override;
    procedure FeedEndOfFile(var err: TSyntaxError); override;
    function GetNextTemplate(var err: TSyntaxError; out tmp: TTemplateLine): Boolean; override;

    class function IsCaseSensitive: Boolean; override;
    class procedure LineToArgs(const buf: string; args: TStrings; var idx: Integer; var err: TSyntaxError); override;
    class function PathsToScriptNative(const pth: string): string; override;
    class function ParseComamnd(const ln: string; dst : TPlainCommand; var err: TSyntaxError): Boolean; override;
  end;

procedure FreeTemplateLine(var l : TTemplateLine);
function CopyTemplateLine(l : TTemplateLine): TTemplateLine;

// replaces l.text paramters with their actual values from vars
procedure SubstitueValues(l : TTemplateLine; vars: TStrings; caseSensitive: Boolean);

function TemplatesToStr(l: TTemplateLine): string;

implementation

procedure SyntaxErrorInit(out se: TSyntaxError);
begin
  se.err := '';
  se.pos := 0;
end;

{ TTemplateLine }

constructor TTemplateLine.Create;
begin
  inherited Create;
end;

constructor TTemplateLine.Create(const atxt: string);
begin
  Create;
  text := atxt;
end;

constructor TTemplateLine.Create(const atxt, aname: string);
begin
  Create(atxt);
  varName := aname;
  isVar := aname<>'';
end;

procedure TTemplateLine.PushFuncParam(const paramVal: string);
begin
  if not Assigned(funcparams) then funcparams := TStringList.Create;
  funcparams.Add(paramVal);
end;

destructor TTemplateLine.Destroy;
begin
  funcParams.Free;
  inherited Destroy;
end;

{ TShSyntax }

function TShSyntax.FeedLine(const s: string; var err: TSyntaxError
  ): TScriptSyntaxResult;
var
  i : integer;
begin
  if waitQuote = #0 then begin
    if s = '' then begin
      Result := srComment;
      Exit;
    end;
    i := 1;
    SkipWhile(s, i, WhiteSpaceChars);
    if (i>length(s)) or (s[i]='#') then begin
      Result := srComment;
      // the commant after \ starts a new line
      if b<>'' then Result := srTemplateAvail;
      Exit;
    end;
  end;

  if not retain then
    // we don't care about the previous buffer
    b := '';

  i := 1;
  while (i<=length(s)) do
    ScanQuotes(s, i, waitQuote);

  if (waitQuote <> #0) then begin
    b := b + s + #10; // adding explicit line break
    Result := srNeedMoreLines
  end else if ((waitQuote = #0) and (s[length(s)] = '\')) then begin
    b := b + Copy(s, 1, length(s)-1);
    Result := srNeedMoreLines;
  end else begin
    b := b + s;
    Result := srTemplateAvail;
  end;
  retain := Result = srNeedMoreLines;
end;

procedure TShSyntax.FeedEndOfFile(var err: TSyntaxError);
begin
  if waitQuote <> #0 then begin
    err.err :='unclosed quotation mark';
  end;
end;

function TShSyntax.GetNextTemplate(var err: TSyntaxError; out tmp: TTemplateLine
  ): Boolean;
var
  i : integer;
begin
  if b = '' then begin
    tmp := nil;
    Result := false;
    Exit;
  end;
  i := 1;
  tmp := ParseTemplateLine(b, i, err);
  inc(i);
  b := Copy(b, i, length(b));
  Result := true;
end;

const
  SpaceChars = [#9,#32];
  FirstIdChars = ['a'..'z','A'..'Z','_'];
  IdChars = FirstIdChars+['0'..'9'];

constructor TShSyntax.Create;
begin
  inherited Create;
end;

function TShSyntax.ParseTemplateLine(const buf: string; var idx: Integer;
  var err: TSyntaxError): TTemplateLine;
var
  i : integer;
  j : integer;
  head : TTemplateLine;
  tail : TTemplateLine;
  nm : string;
  q  : Boolean;

  procedure Push(cur: TTemplateLine);
  begin
    if head = nil then head := cur;
    if tail <> nil then tail.next := cur;
    tail:=cur;
  end;

  procedure ConsumeText;
  var
    txt : string;
  begin
    txt := Copy(buf, j,i-j);
    if txt<>'' then
      Push(TTemplateLine.Create(txt));
  end;

begin
  head := nil;
  tail := nil;
  try
    i := 1;
    j := 1;
    q := false;
    while i<=length(buf) do begin
      if (buf[i] = ';') and not q then begin
        // command separator
        Break;
      end else if (buf[i]= #39) then begin
        inc(i);
        // none of the variables is escaped
        while (i<=length(buf)) and (buf[i] <> #39) do
          inc(i);
        inc(i);
      end else if (buf[i] = '\') and q and (i < length(buf)) and (buf[i+1]='"') then begin
        inc(i, 2);
      end else if (buf[i]='"') then begin
        q := not q;
        inc(i);
      end else if (buf[i]='$') then begin
        ConsumeText;
        j:=i;
        inc(i);
        if (i<=length(buf)) and (buf[i]='{') then begin
          // cases for ${varname}
          inc(i);
          while (i<=length(buf)) and (buf[i]<>'}') do begin
            if buf[i] in SpaceChars then begin
              err.err := 'bad substitution';
              err.pos := i;
              exit;
            end;
            inc(i);
          end;
          inc(i);
          nm:=Copy(buf, j, i-j);
          Push(TTemplateLine.Create(nm, Copy(nm,3, length(nm)-3)));
        end else if (i<=length(buf)) and (buf[i]='(') then begin
          // cases for $(cmd)
          err.err := 'command substitution is not supported';
          err.pos := i;
          exit;
        end else if buf[i] in IdChars then begin
          // cases for $varname
          inc(i);
          while (i<=length(buf)) and (buf[i] in IdChars) do
            inc(i);
          nm:=Copy(buf, j, i-j);
          Push(TTemplateLine.Create(nm, Copy(nm,2, length(nm)-1)));
        end else
          // cases for $., $ $, etc
          ConsumeText;
        j:=i;
      end else
        inc(i);
    end;
    idx := i;
    ConsumeText;
  finally
    Result := head;
  end;
end;

class function TShSyntax.IsCaseSensitive: Boolean;
begin
  Result := true;
end;

class procedure TShSyntax.LineToArgs(const buf: string; args: TStrings;
  var idx: Integer;
  var err: TSyntaxError);
var
  i : integer;
  j : integer;
begin
  i:=1;
  while i<=length(buf) do begin
    if buf[i] in ['"'] then begin
      inc(i);
      j := i;
      while (i<=length(buf)) and (not (buf[i] in ['"'])) do begin
        if (buf[i] = '\') and (i<length(buf)) and (buf[i+1] = '"') then
          inc(i);
        inc(i);
      end;
      args.Add( copy(buf, j, i-j) );
      inc(i);
    end else if not (buf[i] in [#9,#32]) then begin
      j := i;
      while (i<=length(buf)) and not (buf[i] in [#9,#32]) do
        inc(i);
      args.Add( copy(buf, j, i-j) );
    end else
      inc(i);
  end;
  idx := i;
  for i:=0 to args.Count-1 do
    args[i]:=UnixUnescape(args[i]);
end;

class function TShSyntax.PathsToScriptNative(const pth: string): string;
begin
  Result:=SlashToUnix(pth);
end;

class function TShSyntax.ParseComamnd(const ln: string; dst: TPlainCommand;
  var err: TSyntaxError): Boolean;
var
  f : string;
  i : integer;
  isExport : Boolean;
  pr : string;
begin
  Result := Assigned(dst);
  if not Result then Exit;
  i := 1;
  SkipWhile(ln, i, WhiteSpaceChars);
  if (i > length(ln)) then Exit;

  isExport := false;
  pr := StrWhile(ln, i, AlphaNumUnderChars);

  if (pr = 'export') then begin
    isExport := true;
    inc(i);
    SkipWhile(ln, i, WhiteSpaceChars);
    pr := StrWhile(ln, i, AlphaNumUnderChars);
  end;

  if ((i<=length(ln)) and (ln[i]='=')) then begin
    if isExport then
      dst.cmd := pcSetVarEnv
    else
      dst.cmd := pcSetVar;
    inc(i);
    dst.varname := pr;
    dst.args.Add( ScanBashValue(ln, i));
  end else if (isExport) and (pr <> '') then begin
    dst.cmd := pcEnv;
    dst.varname := pr;
  end else begin
    i := 1;
    LineToArgs(ln, dst.args, i, err);
    if (dst.Args.Count>0) then begin
      pr := dst.Args[0];
      if (pr = 'echo') then begin
        dst.cmd := pcEcho;
        dst.Args.Delete(0);
      end else if (pr = 'cd') then begin
        dst.cmd := pcCd;
        dst.Args.Delete(0);
      end else begin
        dst.cmd := pcExec;
      end;
    end;
  end;
end;

{ TBatSyntax }

function TBatSyntax.IsComment(const s: string): boolean;
var
  i : integer;
  r : string;
begin
  i := 1;
  SkipWhile(s, i, WhiteSpaceChars);
  r := StrTo(s, i, WhiteSpaceChars);

  Result :=
    (r = '::')
    or (AnsiLowerCase(r)='rem')
    // this is not Windows batch compatible:
    or ((r<>'') and (r[1] = '#'));
end;

constructor TBatSyntax.Create;
begin
  inherited Create;
end;

function TBatSyntax.FeedLine(const s: string; var err: TSyntaxError
  ): TScriptSyntaxResult;
begin
  if not retain then begin
    intbuf := '';
    if (IsComment(s)) or (s ='') then begin
      Result := srComment;
      Exit;
    end;
  end;

  if (s<>'') and (s[length(s)]='^') then begin
    intbuf := intbuf + Copy(s, 1, length(s)-1);
    Result := srNeedMoreLines;
  end else begin
    intbuf := intbuf + s;
    Result := srTemplateAvail;
  end;
end;

procedure TBatSyntax.FeedEndOfFile(var err: TSyntaxError);
begin

end;

function TBatSyntax.GetNextTemplate(var err: TSyntaxError; out
  tmp: TTemplateLine): Boolean;
begin
  tmp := ParseTemplateLine(intbuf, err);
  Result := Assigned(tmp);
  intbuf := '';
end;

function TBatSyntax.ParseTemplateLine(const buf: string; var err: TSyntaxError): TTemplateLine;
var
  i : integer;
  j : integer;
  head : TTemplateLine;
  tail : TTemplateLine;
  nm : string;

  procedure Push(cur: TTemplateLine);
  begin
    if head = nil then head := cur;
    if tail <> nil then tail.next := cur;
    tail:=cur;
  end;

  procedure ConsumeText;
  var
    txt : string;
  begin
    txt := StringReplace( Copy(buf, j,i-j),'%%','%',[rfReplaceAll]);
    if txt<>'' then
      Push(TTemplateLine.Create(txt));
  end;

var
  closePerc: Boolean;
  ln: integer;

begin
  head := nil;
  tail := nil;
  i := 1;
  j := 1;
  while i<=length(buf) do begin
    if (buf[i] = '%') and (i<length(buf)) and (buf[i+1]='%') then begin
      inc(i);
    end else if (buf[i]='%') then begin
      ConsumeText;
      j:=i;
      inc(i);
      if (i<=length(buf)) and (buf[i] in ['~','0'..'9']) then begin
        closePerc := false;
        if buf[i] = '~' then begin
          inc(i);
          while (i<=length(buf)) and not (buf[i] in ['0'..'9',' ',#9]) do
            inc(i);
        end;
        while (i<=length(buf)) and (buf[i] in ['0'..'9']) do inc(i);
      end else begin
        closePerc := true;
        while (i<=length(buf)) and (buf[i]<>'%') do inc(i);
        if i>length(buf) then begin
          inc(j); // skip the initial '%', closing was not found
          ConsumeText;
          j := i;
          break; // end of the line
        end else
          inc(i);
      end;

      nm:=Copy(buf, j, i-j);
      if closePerc then ln := length(nm)-2
      else ln := length(nm)-1;
      Push(TTemplateLine.Create(nm, Copy(nm,2, ln)));
      j:=i;
    end else
      inc(i);
  end;
  ConsumeText;
  Result := head;
end;

class function TBatSyntax.IsCaseSensitive: Boolean;
begin
  Result := false;
end;

class procedure TBatSyntax.LineToArgs(const buf: string; args: TStrings;
  var idx: integer; var err: TSyntaxError);
var
  i : integer;
  j : integer;
  b : string;
begin
  i:=1;
  while i<=length(buf) do begin
    if buf[i] in ['"'] then begin
      inc(i);
      j := i;
      while (i<=length(buf)) and (not (buf[i] in ['"'])) do
        inc(i);
      args.Add( copy(buf, j, i-j) );
      inc(i);
    end else if not (buf[i] in [#9,#32]) then begin
      j := i;
      while (i<=length(buf)) and not (buf[i] in [#9,#32]) do
        inc(i);
      b := copy(buf, j, i-j);
      b := WindowsUnescape(b);
      args.Add(b);
    end else
      inc(i);
  end;
  idx := i;
end;

class function TBatSyntax.PathsToScriptNative(const pth: string): string;
begin
  Result:=SlashToWindows(pth);
end;

class function TBatSyntax.ParseComamnd(const ln: string; dst: TPlainCommand;
  var err: TSyntaxError): Boolean;
var
  id: string;
  lw : string;
  t  : string;
  n,o,v : string;
  eq:Boolean;
  idx : integer;
begin
  if not Assigned(dst) then begin
    Result:=false;
    Exit;
  end;

  idx := 1;
  SkipWhile(ln, idx, WhiteSpaceChars);
  if (idx>length(ln)) then begin
    Result := false;
    Exit;
  end;
  Result := true;
  if (ln[idx] = '@') then begin
    inc(idx);
    dst.cmdopts:=dst.cmdopts+[pcoNoEcho];
    SkipWhile(ln, idx, WhiteSpaceChars);
  end;
  id := StrWhile(ln, idx, AlphaNumUnderChars);
  lw := LowerCase(id);
  if lw = 'echo' then begin
    dst.cmd := pcEcho;
    inc(idx);
    dst.args.Add(Copy(ln, idx, length(ln)));
    idx := length(ln)+1;
  end else if (lw = 'set') then begin
    inc(idx);
    ParseSet(ln, idx, n,o,v, eq);
    if not eq then begin
      dst.cmd := pcNone;
      Exit;
    end;
    dst.cmd := pcSetVarEnv;
    dst.varname := n;
    dst.args.Add(v);
    if o<>'' then begin
      err.err :='set options are not supported';
      err.pos := 1;
      Result := false;
      exit;
    end;
  end else if (lw = 'cd') then begin
    dst.cmd:= pcCd;
    ParseCd(ln, idx, o, v);
    dst.args.Add(v);
  end else begin
    dst.cmd := pcExec;
    dst.Args.Add(id);
    LineToArgs(ln,dst.Args,idx,err);
  end;
end;

{ TScriptSyntax }

constructor TScriptSyntax.Create;
begin
  inherited Create;
end;

function TScriptSyntax.FeedLine(const s: string; var err: TSyntaxError): TScriptSyntaxResult;
begin
  Result := srError
end;

function TScriptSyntax.GetNextTemplate(var err: TSyntaxError; out tmp: TTemplateLine): Boolean;
begin
  Result := false;
end;

procedure TScriptSyntax.FeedEndOfFile(var err: TSyntaxError);
begin
end;

class function TScriptSyntax.IsCaseSensitive: Boolean;
begin
  Result := false;
end;

class function TScriptSyntax.ParseComamnd(const ln: string; dst: TPlainCommand;
  var err: TSyntaxError): Boolean;
begin
  Result := false;
end;

class procedure TScriptSyntax.LineToArgs(const ln: string; dst: TStrings;
  var idx: Integer; var err: TSyntaxError);
begin

end;

class function TScriptSyntax.PathsToScriptNative(const pth: string): string;
begin
  Result := pth;
end;

{ TPlainCommand }

constructor TPlainCommand.Create;
begin
  inherited Create;
  args := TStringList.Create;
end;

destructor TPlainCommand.Destroy;
begin
  FreeTemplateLine(tmp);
  args.Free;
  inherited Destroy;
end;

procedure TPlainCommand.ParseCommand(vars: TStrings);
var
  buf : string;
  err : TSyntaxError;
begin
  args.Clear;
  cmd := pcNone;
  if tmp = nil then Exit;

  SubstitueValues(tmp, vars, syntax.IsCaseSensitive);
  buf := TemplatesToStr(tmp);

  SyntaxErrorInit(err);
  syntax.ParseComamnd(buf, Self, err);

  if (cmd = pcExec) and (args.Count > 0) then
    cmdlow := AnsiLowerCase(args[0]);
end;

{ TPlainParser }

procedure TPlainParser.GatherCommands;
var
  err : TSyntaxError;
  t   : TTemplateLine;
  cmd : TPlainCommand;
begin
  SyntaxErrorInit(err);
  while ctx.GetNextTemplate(err, t) do begin
    if not Assigned(t) then Continue;

    cmd := TPlainCommand.Create;
    cmd.syntax := TScriptSyntaxClass(ctx.ClassType);
    cmd.lineNum := lineCount;
    cmd.tmp := t;
    commands.Add(cmd);
  end;
end;

constructor TPlainParser.Create;
begin
  inherited Create;
  commands := TList.Create;
  lineCount := 0;
  syntax := TBatSyntax;
end;

destructor TPlainParser.Destroy;
begin
  commands.Free;
  ctx.Free;
  inherited Destroy;
end;

procedure TPlainParser.ParseLine(const ins: string);
var
  s : string;
  r : TScriptSyntaxResult;
  err : TSyntaxError;
begin
  if (ctx=nil) then begin
    if (ins = '#!/bin/bash') then
      syntax := TShSyntax
    else if (ins = '#!/bin/bat') or (ins = '#!/bin/batch')
      or (ins = '#batch') or (ins = '#bat') then
      syntax := TBatSyntax;
    ctx := syntax.Create;
  end;

  inc(lineCount);
  s := Trim(ins);

  SyntaxErrorInit(err);
  r := ctx.FeedLine(s, err);

  if (r = srError) then Exit;
  if (r = srComment) then Exit;

  if r = srTemplateAvail then
    GatherCommands;
end;

procedure TPlainParser.EndOfFile;
var
  err : TSyntaxError;
begin
  if (ctx=nil) then Exit;

  SyntaxErrorInit(err);
  ctx.FeedEndOfFile(err);
  GatherCommands;
end;

function ReadPlainCommandFile(const fn: string): TList;
var
  st  : TStringList;
  res : TList;
  i   : integer;
  pp  : TPlainParser;
begin
  st := TStringList.Create;
  pp := TPlainParser.Create;
  try
    st.LoadFromFile(fn);
    for i:=0 to st.Count-1 do begin
      pp.ParseLine(st[i]);
    end;
    pp.EndOfFile;
    res := TList.Create;
    for i:=0 to pp.commands.Count-1 do
      res.Add(pp.commands[i]);
    Result := res;
  finally
    st.Free;
    pp.Free;
  end;
end;

function ArgsToOneLine(s : TStrings): string;
var
  i  : integer;
  j  : integer;
  sz : integer;
  t  : string;
begin
  Result := '';
  if (s = nil) or (s.Count= 0) then
    Exit;

  sz := length(s[0]);
  for i:=1 to s.Count-1 do
    inc(sz, length(s[i]));
  inc(sz, s.Count-1);

  j := 1;
  SetLength(result, sz);
  for i:=0 to s.Count-1 do begin
    t := s[i];
    if length(t) = 0 then Continue;
    Move(t[1], Result[j], length(t));
    inc(j, length(t));
    Result[j]:=' ';
    inc(j);
  end;
end;

function GetTimeOutMs(const ts: string; out ms: Integer): Boolean;
var
  i : integer;
  s : string;
  d : double;
  err : integer;
begin
  s := AnsiLowercase(Trim(ts));
  if s = 'never' then begin
    ms := -1;
    Result := true;
    Exit;
  end;
  i:=1;

  while (i<=length(s)) and (s[i] in ['+','-']) do inc(i);
  while (i<=length(s)) and (s[i] in ['0'..'9','.']) do inc(i);
  Val(Copy(s, 1, i-1), d, err);
  Result := err = 0;
  if not Result then begin
    ms := 0;
    Exit;
  end;
  s := Trim(Copy(s, i, length(s)));
  if (s = 's') or (s = 'sec') or (s = 'second') or (s='seconds') then
    ms := Round(d * 1000)
  else if (s = 'min') or (s = 'minute') or (s = 'minutes') then
    ms := Round(d * 1000 * 60)
  else if (s = '') or (s = 'ms') or (s = 'milliseconds') or (s = 'millisecond') or (s = 'milisecond') or (s = 'milisecond')then
    ms := Round(d)
  else
    Result := false; // unknown suffix
end;

procedure FreeTemplateLine(var l : TTemplateLine);
var
  t : TTemplateLine;
begin
  while Assigned(l) do begin
    t := l.next;
    FreeAndNil(l);
    l := t;
  end;
end;

function CopyTemplateLine(l : TTemplateLine): TTemplateLine;
var
  n : TTemplateLine;
begin
  if l = nil then begin
    Result := nil;
    Exit;
  end;
  n := TTemplateLine.Create;
  Result := n;
  while Assigned(l) do begin
    n.text := l.text;
    n.isVar := l.isVar;
    n.varName := l.varName;
    n.func := l.func;
    if Assigned(l.funcParams) then begin
      n.funcParams := TSTringList.Create;
      n.funcPArams.Assign(l.funcParams);
    end;
    if Assigned(l.next) then
      n.next := TTemplateLine.Create;
    l := l.next;
    n := n.next;
  end;
end;

procedure SubstitueValues(l : TTemplateLine; vars: TStrings; caseSensitive: Boolean);
var
  nv : string;
begin
  while Assigned(l) do begin
    if l.isVar then begin
      // todo: add support for functions
      if Assigned(Vars) then
        nv := Vars.Values[l.varName]
      else
        nv := '';
      l.text := nv;
    end;
    l := l.next;
  end;
end;

function TemplatesToStr(l: TTemplateLine): string;
var
  ln : integer;
  i  : integer;
  j  : integer;
  t  : TTemplateLine;
begin
  ln := 0;
  t := l;
  while Assigned(t) do begin
    inc(ln, length(t.text));
    t := t.next;
  end;

  SetLength(Result, ln);
  if ln = 0 then Exit;

  i:=1;
  t := l;
  while Assigned(t) do begin
    j := length(t.text);
    if j>0 then begin
      Move(t.text[1], Result[i], j);
      inc(i, j);
    end;
    t := t.next;
  end;

end;

end.

