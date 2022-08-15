program testsyntaxtest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, plainsyntaxtype, plainsyntaxexec;
  { you can add units after this }

procedure FreeList(var l : TList);
var
  i : integer;
begin
  for i := 0 to l.Count-1 do
    TObject(l[i]).Free;
  l.Free;
end;

procedure DumpCommands(l: TList);
var
  i   : integer;
  cmd : TPlainCommand;
begin
  writeln('commands: ',l.Count);
  for i:=0 to l.Count-1 do begin
    cmd := TPlainCommand(l[i]);
    cmd.ParseCommand(nil);
    writeln(cmd.cmd);
  end;
end;

var
  l : TList;
begin
  if ParamCOunt=0 then begin
    writeln('please specify input file name');
    exit;
  end;
  l := ReadPlainCommandFile(ParamStr(1));
  DumpCommands(l);
  FreeList(l);

end.

