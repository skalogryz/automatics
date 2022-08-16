unit batchparser; 
{$mode delphi}

interface

uses
  sysutils, classes, parseutils;

function ParseOption(const ln: string; var idx: integer): string;

// "set" must already been parsed
function ParseSet(const ln: string; var idx: integer;
  out name, opts, value: string; out hasEq: Boolean): Boolean;

// "set" must already been parsed
function ParseCd(const ln: string; var idx: integer; out opts, todir: string): Boolean;

function WindowsUnescape(const s : string): string;

implementation

// presumably the %% has alredy been replaced with %
// thus no replacing here
function WindowsUnescape(const s : string): string;
var
  i : integer;
  j : integer;
begin
  Result := '';
  i := 1;
  j := 1;
  while i <= length(s) do begin
    if s[i]='^' then begin
      Result := Result+Copy(s, j, i-j);
      inc(i);
      if (i<=length(s)) then begin
        Result:=Result+s[i];
        inc(i);
      end;
      j := i;
    end;
    inc(i);
  end;
  if Result = '' then Result :=s
  else Result := REsult+Copy(s, j, length(s)-j+1);
end;


function ParseOption(const ln: string; var idx: integer): string;
var
  j : integer;
begin
  if (idx > length(ln)) or (not (ln[idx] in ['/'])) then begin
    Result:='';
    Exit;
  end;
  j := idx;
  inc(idx);
  SkipWhile(ln, idx, AlphaNumChars);
  REsult := Copy(ln, j, idx-j);
end;

function ParseSet(const ln: string; var idx: integer;
  out name, opts, value: string; out hasEq: Boolean): Boolean;
var
  o : string;
begin
  Result := true;
  opts := '';
  name := '';
  value := '';
  hasEq := false;
  SkipWhile(ln, idx, WhiteSpaceChars);
  if (idx<=length(ln)) and (ln[idx] = '/') then begin
    while (idx<=length(ln)) and (ln[idx] = '/') do begin
      o := ParseOption(ln, idx);
      opts := opts+o;
    end;
    SkipWhile(ln, idx, WhiteSpaceChars);
  end;
  name := StrTo(ln, idx, ['=']);
  if (idx > length(ln)) then Exit;
  hasEq := true;
  inc(idx);
  value := Copy(ln, idx, length(ln));
  idx := length(ln)+1;
end;

function ParseCd(const ln: string; var idx: integer; out opts, todir: string): Boolean;
var
  o : string;
begin
  opts:='';
  todir:='';
  SkipWhile(ln, idx, WhiteSpaceChars);
  if (idx<=length(ln)) and (ln[idx] = '/') then begin
    while (idx<=length(ln)) and (ln[idx] = '/') do begin
      o := ParseOption(ln, idx);
      opts := opts+o;
    end;
    SkipWhile(ln, idx, WhiteSpaceChars);
  end;
  todir := Trim(Copy(ln, idx, length(ln)));
  if (todir <> '') and (todir[1]='"') and (todir[length(todir)]='"') then
    todir := Copy(todir, 2, length(todir)-2);
  idx := length(ln)+1;
  Result := true;
end;

end.
