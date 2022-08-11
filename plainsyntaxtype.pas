unit plainsyntaxtype;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TPlainCommand }

  TPlainCommand = class(TObject)
    lines  : TStringList;
    cmd    : string; // lower case of rawcmd
    rawcmd : string;
    args   : TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure ParseCommand;
  end;

  { TPlainParser }

  TPlainParser = class(TObject)
  public
    curcmd   : TPlainCommand;
    commands : TList;
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

implementation

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
  commands := TList.Create;
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
  s := Trim(ins);
  if Pos('#',s)=1 then Exit;
  if Pos('//',s)=1 then Exit;
  if s ='' then begin
    curcmd := nil;
    exit;
  end;
  if (curcmd = nil) then begin
    curcmd := TPlainCommand.Create;
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

end.

