program testsyntaxexec;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF} SysUtils,
  Classes, plainsyntaxtype, plainsyntaxexec, ExtraFileUtils
  { you can add units after this };

procedure ExecFile(const fn: string);
var
  i    : integer;
  exec : TPlainSyntaxExec;
  cmds : TList;
  stx  : TScriptSyntaxClass;
begin
  cmds := ReadPlainCommandFile(fn);
  if not ASsigned(cmds) or (cmds.Count=0) then Exit;

  stx := TPlainCommand(cmds[0]).Syntax;
  exec := TPlainSyntaxExec.Create;
  exec.Delegate := TPlainSyntaxExecStdErrEnv.Create;
  try
    exec.Params.Values['subj']:='pet.exe';
    exec.Params.Values['subject']:='pet.exe';

    exec.Params.Values['0']:='"'+stx.PathsToScriptNative(ExpandFileName(fn))+'"';
    exec.Params.Values['1']:=stx.PathsToScriptNative(exec.Params.Values['subj']);
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

