unit shparser;

{$mode delphi}

interface

uses 
  SysUtils, Classes, parseutils;

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


end.
