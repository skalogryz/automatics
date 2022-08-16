unit parseutils;

{$mode delphi}

interface

type
  TCharSet = set of char;

const
  WhiteSpaceChars = [#32,#9];
  AlphaChar = ['a'..'z','A'..'Z'];
  NumChars = ['0'..'9'];
  AlphaNumChars = AlphaChar+NumChars;
  AlphaNumUnderChars = AlphaNumChars+['_'];
  AlphaUnderChars = AlphaChar+['_'];

procedure SkipTo(const s: string; var idx: integer; const toChars: TCharSet); inline;
function StrTo(const s: string; var idx: integer; const toChars: TCharSet): string; inline;

procedure SkipWhile(const s: string; var idx: integer; const skipChars: TCharSet); inline;
function StrWhile(const s: string; var idx: integer; const whileChars: TCharSet): string; inline;

function ScanIdent(const s: string; var idx: integer; const InitChars, OtherChars: TCharSet): string; inline;

implementation

procedure SkipTo(const s: string; var idx: integer; const toChars: TCharSet); inline;
begin
  while (idx<=length(s)) and not (s[idx] in toChars) do inc(idx);
end;

function StrTo(const s: string; var idx: integer; const toChars: TCharSet): string; inline;
var
  j : integer;
begin
  j := idx;
  SkipTo(s, idx, toChars);
  Result := Copy(s, j, idx-j);
end;

procedure SkipWhile(const s: string; var idx: integer; const skipChars: TCharSet); inline;
begin
  while (idx<=length(s)) and (s[idx] in skipChars) do inc(idx);
end;

function StrWhile(const s: string; var idx: integer; const whileChars: TCharSet): string; inline;
var
  j : integer;
begin
  j := idx;
  SkipWhile(s, idx, whileChars);
  Result := Copy(s, j, idx-j);
end;

function ScanIdent(const s: string; var idx: integer; const InitChars, OtherChars: TCharSet): string; inline;
var
  j : integer;
begin
  if (idx>length(s)) or not (s[idx] in InitChars) then begin
    Result := '';
    Exit;
  end;
  j := idx;
  inc(idx);
  SkipWhile(s, idx, OtherChars);
  Result := Copy(s, j, idx-j);
end;

end.
