unit shparser;

{$mode delphi}

interface

uses 
  SysUtils, Classes, parseutils;

function UnixUnescape(const s : string): string;

// reads either value or quoted string
function ScanBashValue(const s: string; var idx: integer): string;
// DblQuote allow \"
function ScanDblQuoteStr(const s: string; var idx: integer): string;
// SingleQuote doesn't allow \'
function ScanSingleQuoteStr(const s: string; var idx: integer): string;

procedure ScanQuotes(const s: string; var idx: integer; var waitQuote: char);

implementation

function ScanBashValue(const s: string; var idx: integer): string;
begin
  if (idx>length(s)) then Result:=''
  else if s[idx]='"' then
    Result := ScanDblQuoteStr(s, idx)
  else if s[idx]=#39 then
    Result := ScanSingleQuoteStr(s, idx)
  else
    Result := StrTo(s, idx, WhiteSpaceChars+[';']);
end;

function ScanDblQuoteStr(const s: string; var idx: integer): string;
var
  j: integer;
begin
  if (idx>length(s)) or (s[idx]<>'"') then begin
    Result:='';
    exit;
  end;
  inc(idx);
  j := idx;
  while true do begin
    SkipTo(s, idx, ['"']);
    if (idx > length(s)) or ((idx <= length(s)) and (s[idx-1]<>'\')) then
      Break;
    inc(idx);
  end;
  Result := UnixUnescape(Copy(s, j, idx-j));
  inc(idx);
end;

function ScanSingleQuoteStr(const s: string; var idx: integer): string;
var
  j: integer;
begin
  if (idx>length(s)) or (s[idx]<>#39) then begin
    Result:='';
    exit;
  end;
  inc(idx);
  j := idx;
  Result := StrTo(s, idx, [#39]);
  inc(idx);
  Result := UnixUnescape(Result);
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
        '"': result := Result + '"';
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


procedure ScanQuotes(const s: string; var idx: integer; var waitQuote: char);
begin
  while (idx <= length(s)) do begin
    case s[idx] of
      #39: begin
        if waitQuote = #0 then
          waitQuote := #39
        else if waitQuote = #39 then
          waitQuote := #0;
      end;
      '"': begin
        if waitQuote = '"' then
          waitQuote := #0
        else if waitQuote = #0 then
          waitQuote := '"';
      end;
      '\': begin
        if (waitQuote <> #39) and (idx < length(s)) and (s[idx+1]='"') then
          inc(idx);
      end;
    end;
    inc(idx);
  end;
end;

end.
