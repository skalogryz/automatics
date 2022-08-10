program testsyntaxexec;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, plainsyntaxtype, plainsyntaxexec
  { you can add units after this };

procedure ExecFile(const fn: string);
var
  i    : integer;
  exec : TPlainSyntaxExec;
  cmds : TList;
begin
  cmds := ReadPlainCommandFile(fn);
  exec := TPlainSyntaxExec.Create;
  exec.Delegate := TPlainSyntaxExecStdErrEnv.Create;
  try
    exec.Params.Values['subj']:='run.exe';
    exec.Params.Values['subject']:='run.exe';
    exec.RunCommands(cmds);
  finally
    exec.Delegate.Free;
    exec.Free;
    for i :=0 to cmds.Count-1 do
      TObject(cmds[i]).Free;
    cmds.Free;
  end;
end;


begin
  if (ParamCount = 0) then begin
    writeln('please specify the script file name to execute');
    exit;
  end;

  ExecFile(ParamStr(1));
end.

