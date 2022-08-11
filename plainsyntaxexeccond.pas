unit plainsyntaxexeccond;

interface

{$mode delphi}{$H+}

uses
  Classes, SysUtils,
  fpexprpars, plainsyntaxexec;


implementation

type

  { TExtraFunc }

  TExtraFunc = class(TObject)
    // FileExists
    // 0 - filename  of the file to exist
    class procedure FileExists(const fnname: string;
      const args: array of string;
      var res: TCustomFuncResult);
    // TextMatch
    // 0 - file 1 name
    // 1 - file 2 name
    // returns true if textual contents matches (don't care about line breaks)
    // text comparison is case-sensitive!
    class procedure TextMatch(const fnname: string;
      const args: array of string;
      var res: TCustomFuncResult);
    // BinMatch
    // 0 - file 1 name
    // 1 - file 2 name
    // returns true if binary identical.
    class procedure BinMatch(const fnname: string;
      const args: array of string;
      var res: TCustomFuncResult);
    // PosInFile
    // 0 - substring
    // 1 - file name
    // returns the offset (0 based) of the substring in the specified file
    // if not found returns -1
    class procedure PosInFile(const fnname: string;
      const args: array of string;
      var res: TCustomFuncResult);
  end;


{ TExtraFunc }

class procedure TExtraFunc.FileExists(const fnname: string;
  const args: array of string; var res: TCustomFuncResult);
begin
  res.bool:=SysUtils.FileExists(args[0]);
end;

class procedure TExtraFunc.TextMatch(const fnname: string;
  const args: array of string; var res: TCustomFuncResult);
var
  s1, s2 : TStringList;
  i  : integer;
begin
  s1 := TStringList.Create;
  s2 := TStringList.Create;
  try
    s1.LoadFromFile(args[0]);
    s2.LoadFromFile(args[1]);
    res.bool:=s1.count = s2.count;
    if not res.bool then Exit;
    for i:=0 to s1.count-1 do
      if s1[i]<>s2[i] then begin
        res.bool := false;
        Exit;
      end;
  finally
    s1.Free;
    s2.Free;
  end;
end;

class procedure TExtraFunc.BinMatch(const fnname: string;
  const args: array of string; var res: TCustomFuncResult);
var
  fs1, fs2 : TFileStream;
  buf1, buf2 : array of byte;
  c1, c2 : integer;
begin
  fs1:=TfileStream.Create(args[0], fmOpenRead, fmShareDenyNone);
  fs2:=TfileStream.Create(args[1], fmOpenRead, fmShareDenyNone);
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

class procedure TExtraFunc.PosInFile(const fnname: string;
  const args: array of string; var res: TCustomFuncResult);
var
  fs1 : TFileStream;
  st  : string;
  sub : string;
begin
  fs1:=TfileStream.Create(args[1], fmOpenRead, fmShareDenyNone);
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
  RegisterCondFunc('textmatch', TExtraFunc.TextMatch, tpBool, [tpString,tpString]);
  RegisterCondFunc('binmatch', TExtraFunc.BinMatch, tpBool, [tpString,tpString]);
  RegisterCondFunc('posinfile', TExtraFunc.PosInFile, tpInt, [tpString,tpString]);
end;


initialization
  RegisterExtraProc;

finalization

end.
