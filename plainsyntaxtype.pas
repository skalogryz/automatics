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

implementation

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
end;

destructor TPlainCommand.Destroy;
begin
  lines.Free;
  inherited Destroy;
end;

procedure TPlainCommand.ParseCommand;
var
  s : string;
  i : integer;
begin
  if cmd <> '' then Exit;
  if lines.Count=0 then Exit;
  GetFirstWord(lines[0], i, s);
  cmd := AnsiLowerCase(s);
  lines[0] := Copy(lines[0], i+length(s), length(s));
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
  curcmd.ParseCommand;
  if IsOneLineCommand(curcmd.cmd) then
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

