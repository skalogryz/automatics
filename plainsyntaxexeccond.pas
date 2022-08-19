unit plainsyntaxexeccond;

interface

{$mode delphi}{$H+}

uses
  Classes, SysUtils, ExtraFileUtils,
  fpexprpars, plainsyntaxexec;


implementation

type
  TTextCompareStyle = set of (tcTrimRight, tcTrimLeft, tcIgnoreCase, tcIgnoreBlankLines);

  { TExtraFunc }

  TExtraFunc = class(TObject)
    // FileExists
    // 0 - filename  of the file to exist
    class procedure FileExists(
      const fc: TFuncExecContext;
      const args: array of string;
      var res: TCustomFuncResult);
    // TextMatch
    // 0 - file 1 name
    // 1 - file 2 name
    // returns true if textual contents matches (don't care about line breaks)
    // text comparison is case-sensitive!
    class function TextMatch(const fn1, fn2: string; style: TTextCompareStyle): Boolean;  overload;

    class procedure TextMatch(
      const fc: TFuncExecContext;
      const args: array of string;
      var res: TCustomFuncResult); overload;
    // BinMatch
    // 0 - file 1 name
    // 1 - file 2 name
    // returns true if binary identical.
    class procedure BinMatch(
      const fc: TFuncExecContext;
      const args: array of string;
      var res: TCustomFuncResult);
    // PosInFile
    // 0 - substring
    // 1 - file name
    // returns the offset (0 based) of the substring in the specified file
    // if not found returns -1
    class procedure PosInFile(
      const fc: TFuncExecContext;
      const args: array of string;
      var res: TCustomFuncResult);
  end;


{ TExtraFunc }

class procedure TExtraFunc.FileExists(
  const fc: TFuncExecContext;
  const args: array of string; var res: TCustomFuncResult);
begin
  res.bool:=SysUtils.FileExists(ExpandPathOnBase(fc.curdir,args[0]));
end;

procedure DeleteBlankLines(s : TStrings);
var
  i : integer;
begin
  if s = nil then Exit;
  for i:=s.Count-1 downto 0 do
    if Trim(s[i])='' then
      s.Delete(i);
end;

procedure StringsTrim(s: TStrings; DoTrimLeft, DoTrimRight: Boolean);
var
  i : integer;
begin
  if ((not DoTrimLeft) and (not DoTrimRight)) or (s = nil) then Exit;
  if DoTrimLeft and DoTrimRight then begin
    for i := 0 to s.Count-1 do
      s[i]:=Trim(s[i]);
  end else if DoTrimLeft then begin
    for i := 0 to s.Count-1 do
      s[i]:=TrimLeft(s[i]);
  end else
    for i := 0 to s.Count-1 do
      s[i]:=TrimRight(s[i])
end;

procedure StringsLowerCase(s: TStrings);
var
  i : integer;
  w : WideString;
begin
  if s = nil then Exit;
  for i := 0 to s.Count-1 do begin
    w := UTF8Decode(s[i]);
    w := WideLowerCase(w);
    s[i] := UTF8Encode(s[i]);
  end;
end;

class function TExtraFunc.TextMatch(const fn1, fn2: string; style: TTextCompareStyle): Boolean;
var
  s1, s2 : TStringList;
  i  : integer;
begin
  s1 := TStringList.Create;
  s2 := TStringList.Create;
  try
    s1.LoadFromFile(fn1);
    s2.LoadFromFile(fn2);

    if tcIgnoreBlankLines in style then begin
      DeleteBlankLines(s1);
      DeleteBlankLines(s2);
    end;

    Result :=s1.count = s2.count;
    if not Result then Exit;

    StringsTrim(s1, tcTrimLeft in style, tcTrimRight in style);
    StringsTrim(s2, tcTrimLeft in style, tcTrimRight in style);

    if (tcIgnoreCase in style) then begin
      StringsLowerCase(s1);
      StringsLowerCase(s2);
    end;

    for i:=0 to s1.count-1 do
      if s1[i]<>s2[i] then begin
        Result := false;
        Exit;
      end;
  finally
    s1.Free;
    s2.Free;
  end;
end;

class procedure TExtraFunc.TextMatch(const fc: TFuncExecContext;
  const args: array of string; var res: TCustomFuncResult);
var
  i   : integer;
  prm : string;
  st  : TTextCompareStyle;
begin
  if length(args)>2 then
    prm := args[2]
  else
    prm := '';
  st := [];
  for i := 1 to length(prm) do
    case prm[i] of
      'i','I': Include(st, tcIgnoreCase);
      'l','L': Include(st, tcTrimLeft);
      'r','R': Include(st, tcTrimRight);
      'b','B': Include(st, tcIgnoreBlankLines);
    end;
  res.bool := TextMatch(ExpandPathOnBase(fc.curdir, args[0]), ExpandPathOnBase(fc.curdir,args[1]), st);
end;

class procedure TExtraFunc.BinMatch(const fc: TFuncExecContext;
  const args: array of string; var res: TCustomFuncResult);
var
  fs1, fs2 : TFileStream;
  buf1, buf2 : array of byte;
  c1, c2 : integer;
begin
  fs1:=TfileStream.Create(ExpandPathOnBase(fc.curdir, args[0]), fmOpenRead or fmShareDenyNone);
  fs2:=TfileStream.Create(ExpandPathOnBase(fc.curdir, args[1]), fmOpenRead or fmShareDenyNone);
  try
    res.bool := fs1.Size = fs2.Size;
    if not res.bool then Exit;
    SetLength(Buf1, 1024 * 64);
    SetLength(Buf2, 1024 * 64);
    while res.bool do begin
      c1 := fs1.read(buf1[0], length(buf2));
      c2 := fs2.read(buf2[0], length(buf2));
      if (c1=0) and (c2=0) then break;
      res.bool := (c1=c2)
        and (CompareMem(@buf1[0], @buf2[0], c1));
    end;
  finally
    fs1.Free;
    fs2.Free;
  end;
end;

class procedure TExtraFunc.PosInFile(const fc: TFuncExecContext;
  const args: array of string; var res: TCustomFuncResult);
var
  fs1 : TFileStream;
  st  : string;
  sub : string;
begin
  fs1:=TfileStream.Create(ExpandPathOnBase(fc.curdir, args[1]), fmOpenRead or fmShareDenyNone);
  try
    sub := args[0];
    if (fs1.Size = 0) then begin
      if sub = '' then res.int:=0
      else res.int:=-1;
      Exit;
    end else if (fs1.Size < length(sub)) then begin
      res.int:=-1;
      Exit;
    end;
    SetLength(st, fs1.Size);
    fs1.Read(st[1], length(st));
    res.int := Pos(sub, st)-1;
  finally
    fs1.Free;
  end;
end;

procedure RegisterExtraProc;
begin
  RegisterCondFunc('filexists', TExtraFunc.FileExists, tpBool, [tpString]);
  RegisterCondFunc('fileexists', TExtraFunc.FileExists, tpBool, [tpString]);
  RegisterCondFunc('textmatch', TExtraFunc.TextMatch, tpBool, [tpString,tpString,tpString]);
  RegisterCondFunc('binmatch', TExtraFunc.BinMatch, tpBool, [tpString,tpString]);
  RegisterCondFunc('posinfile', TExtraFunc.PosInFile, tpInt, [tpString,tpString]);
end;


initialization
  RegisterExtraProc;

finalization

end.
