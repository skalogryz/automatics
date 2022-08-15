unit plainsyntaxtype;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TScriptSyntax = class;

  { TPlainCommand }

  TPlainCommand = class(TObject)
    syntax : TScriptSyntax;
    lines  : TStringList;
    cmd    : string; // lower case of rawcmd
    rawcmd : string;
    args   : TStringList;
    lineNum : Integer; // line number in the file
    constructor Create;
    destructor Destroy; override;
    procedure ParseCommand;
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

  { TScriptSyntax }

  TScriptSyntax = class(TObject)
  public
    function IsComment(const s: string): boolean; virtual;
    function MultiLineChar: char; virtual;
    function ParseTemplateLine(const ln: string; var err: TSyntaxError): TTemplateLine; virtual;
    function IsCaseSensitive: Boolean; virtual;
  end;

  { TPlainParser }

  TPlainParser = class(TObject)
  private
    lineCount: Integer;
  public
    curcmd   : TPlainCommand;
    commands : TList;
    syntax   : TScriptSyntax;
    constructor Create;
    destructor Destroy; override;
    procedure ParseLine(const ins: string);
  end;

function ReadPlainCommandFile(const fn: string): TList;

function UnixUnescape(const s : string): string;

// converting TStrings to a single string
function ArgsToOneLine(s : TStrings): string;

// replacing command aliases
procedure UpdateCommandAlias(c: TPlainCommand);

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

function GetTimeOutMs(const ts: string; out ms: Integer): Boolean;

type
  { TBatSyntax }

  TBatSyntax = class(TScriptSyntax)
    function IsComment(const s: string): boolean; override;
    function MultiLineChar: char; override;
    function ParseTemplateLine(const buf: string; var err: TSyntaxError): TTemplateLine; override;
    function IsCaseSensitive: Boolean; override;
  end;

  { TShSyntax }

  TShSyntax = class(TScriptSyntax)
    function IsComment(const s: string): boolean; override;
    function MultiLineChar: char; override;
    function ParseTemplateLine(const buf: string; var err: TSyntaxError): TTemplateLine; override;
    function IsCaseSensitive: Boolean; override;
  end;

var
  batSyntax : TBatSyntax = nil;
  shSyntax  : TShSyntax = nil;

function GetNextWord(const s: string; var i : integer): string;

procedure FreeTemplateLine(var l : TTemplateLine);

implementation

function GetNextWord(const s: string; var i : integer): string;
var
  j : integer;
begin
  while (i<=length(s)) and (s[i] in [#32,#9]) do inc(i);
  j:=i;
  while (i<=length(s)) and not (s[i] in [#32,#9]) do inc(i);
  Result := Copy(s, j, i-j);
end;

function UnixUnescape(const s : string): string;
var
  i : integer;
  j : integer;
begin
  Result := '';
  if s = '' then Exit;
  j:=1;
  i := 1;
  while i <= length(s) do begin
    if s[i]= '\' then begin // need escape
      Result := Result + Copy(s, j, i-j);
      inc(i);
      case s[i] of
        't': Result := Result+#9;
        'r': Result := Result + #10;
        'n': Result := Result + #13;
      else
        Result := Result + s[i];
      end;
      inc(i);
      j:=i;
    end else
      inc(i);
  end;
  if (Result = '') then
    Result := s
  else
    Result := Result + Copy(s, j, length(s)-j+1);
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

function TShSyntax.IsComment(const s: string): boolean;
var
  i : integer;
  r : string;
begin
  i := 1;
  r := GetNextWord(s, i);
  Result:=(r<>'') and (r[1] = '#');
end;

function TShSyntax.MultiLineChar: char;
begin
  Result:='\';
end;

const
  SpaceChars = [#9,#32];
  FirstIdChars = ['a'..'z','A'..'Z','_'];
  IdChars = FirstIdChars+['0'..'9'];

function TShSyntax.ParseTemplateLine(const buf: string; var err: TSyntaxError): TTemplateLine;
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
    while i<=length(buf) do begin
      if (buf[i]= #39) then begin
        inc(i);
        // none of the variables is escaped
        while (i<=length(buf)) and (buf[i] <> #39) do
          inc(i);
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
        end else if buf[i] in FirstIdChars then begin
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
    ConsumeText;
  finally
    Result := head;
  end;
end;

function TShSyntax.IsCaseSensitive: Boolean;
begin
  Result := true;
end;

{ TBatSyntax }

function TBatSyntax.IsComment(const s: string): boolean;
var
  i : integer;
  r : string;
begin
  i := 1;
  r := GetNextWord(s, i);
  Result :=
    (r = '::')
    or (AnsiLowerCase(r)='rem')
    // this is not Windows batch compatible:
    or (r = '#');
end;

function TBatSyntax.MultiLineChar: char;
begin
  Result:='^';
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
      while (i<=length(buf)) and (buf[i]<>'%') do inc(i);
      if i>length(buf) then begin
        inc(j); // skip the initial '%', closing was not found
        ConsumeText;
      end else begin
        inc(i);
        nm:=Copy(buf, j, i-j);
        Push(TTemplateLine.Create(nm, Copy(nm,2, length(nm)-2)));
      end;
      j:=i;
    end else
      inc(i);
  end;
  ConsumeText;
  Result := head;
end;

function TBatSyntax.IsCaseSensitive: Boolean;
begin
  Result := false;
end;

{ TScriptSyntax }

function TScriptSyntax.IsComment(const s: string): boolean;
begin
  Result := false;
end;

function TScriptSyntax.MultiLineChar: char;
begin
  Result := #0;
end;

function TScriptSyntax.{%H-}ParseTemplateLine(const ln: string; var err: TSyntaxError): TTemplateLine;
begin
  Result := TTemplateLine.Create(ln);
end;

function TScriptSyntax.IsCaseSensitive: Boolean;
begin
  Result := false;
end;

{ TPlainCommand }

constructor TPlainCommand.Create;
begin
  inherited Create;
  lines := TStringList.Create;
  args := TStringList.Create;
end;

destructor TPlainCommand.Destroy;
begin
  args.Free;
  lines.Free;
  inherited Destroy;
end;

procedure TPlainCommand.ParseCommand;
var
  s : string;
  i   : integer;
  j   : integer;
  buf : string;
begin
  args.Clear;
  cmd := '';
  rawcmd := '';
  if lines.Count=0 then Exit;

  buf := '';
  for i:= 0 to lines.Count-2 do begin
    s := lines[i];
    buf := Copy(s, 1, length(s)-1);
  end;
  buf := buf + lines[lines.Count-1];

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
  for i:=0 to args.Count-1 do
    args[i]:=UnixUnescape(args[i]);

  if (args.Count>0) then begin
    rawcmd := args[0];
    args.Delete(0);
    cmd := AnsiLowerCase(rawcmd);
  end;

end;

{ TPlainParser }

constructor TPlainParser.Create;
begin
  inherited Create;
  commands := TList.Create;
  lineCount := 0;
  syntax := batSyntax;
end;

destructor TPlainParser.Destroy;
begin
  commands.Free;
  inherited Destroy;
end;

procedure TPlainParser.ParseLine(const ins: string);
var
  s : string;
begin
  if (lineCount=0) then begin
    if (ins = '#!/bin/bash') then
      syntax := shSyntax
    else if (ins = '#batch') or (ins = '#bat') then
      syntax := batSyntax;
  end;

  inc(lineCount);
  s := Trim(ins);
  if (syntax.IsComment(s)) then Exit;
  if s ='' then begin
    curcmd := nil;
    exit;
  end;
  if (curcmd = nil) then begin
    curcmd := TPlainCommand.Create;
    curcmd.syntax := syntax;
    curcmd.lineNum := lineCount;
    commands.Add(curcmd);
  end;
  curcmd.lines.Add(s);
  if (s <> '') and (s[length(s)] = '\') then
    // multiline
  else
    curcmd := nil;
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
    for i:=0 to st.Count-1 do
      pp.ParseLine(st[i]);
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

procedure UpdateCommandAlias(c: TPlainCommand);
begin
  if (c.cmd = CMD_EXPECT)
    or (c.cmd = CMD_ASSES)
    or (c.cmd = CMD_ASSERT) then
    c.cmd := CMD_EXPECT;
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

initialization
  batSyntax := TBatSyntax.Create;
  shSyntax  := TShSyntax.Create;

finalization
  batSyntax.Free;
  shSyntax.Free;


end.

