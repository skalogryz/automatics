unit plainsyntaxtype;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TPlainCommand }

  TPlainCommand = class(TObject)
    lines : TStringList;
    cmd   : string;
    args  : TStringList;
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

procedure GetFirstWord(const s: string; out idx: integer; out w: string);
function IsOneLineCommand(const cmd: string): boolean;

function ReadPlainCommandFile(const fn: string): TList;

function UnixUnescape(const s : string): string;

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

procedure GetFirstWord(const s: string; out idx: integer; out w: string);
var
  i : integer;
begin
  i := 1;
  while (i<=length(s)) and (s[i] in [#32,#9]) do inc(i);
  idx := i;
  while (i<=length(s)) and not (s[i] in [#32,#9]) do inc(i);
  w := Copy(s, idx, i -idx);
end;

function IsOneLineCommand(const cmd: string): boolean;
begin
  if cmd = 'cd' then Result:=true
  else if cmd ='env' then Result:=true
  else
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
  if cmd <> '' then Exit;
  if lines.Count=0 then Exit;
  GetFirstWord(lines[0], i, s);
  cmd := AnsiLowerCase(s);
  lines[0] := Copy(lines[0], i+length(s), length(lines[0]));
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

end.

