unit tstrunnerlog;

interface

{$mode delphi}{$H+}
uses SysUtils, Classes, syncobjs;


procedure SetLogFile(const fn: string);

procedure Log(const s: string); overload;
procedure Log(const fmt: string; const args : array of const); overload;
procedure Warn(const s: string);
procedure Error(const s: string);
procedure Verbose(const s: string); overload;
procedure Verbose(const fmt: string; const args : array of const); overload;

var
  EnableLog: Boolean = true;
  EnableVerbose: Boolean = false;

implementation

var
  target: Text;
  closeTarget: Boolean = false;
  lock : TCriticalSection;

procedure DoLog(const pfx, s: string);
begin
  if not EnableLog then Exit;
  lock.Enter;
  try
    writeln(target, FormatDateTime('hh:nn:ss:zzz',now),' [',GetCurrentThreadId,']: ',pfx,s);
  finally
    lock.Leave;
  end;
end;

procedure SetLogFile(const fn: string);
begin
  if not FileExists(fn) then begin
    AssignFile(target, fn); Rewrite(target);
    CloseFile(target);
  end;
  AssignFile(target, fn);
  Append(target);
end;

procedure Log(const s: string);
begin
  DoLog('', s);
end;

procedure Log(const fmt: string; const args : array of const); overload;
begin
  DoLog('', format(fmt, args));
end;

procedure Warn(const s: string);
begin
  DoLog('WARN: ', s);
end;

procedure Error(const s: string);
begin
  DoLog('ERR: ', s);
end;

procedure Verbose(const s: string);
begin
  if EnableVerbose then DoLog('', s);
end;

procedure Verbose(const fmt: string; const args : array of const); overload;
begin
  if EnableVerbose then DoLog('', Format(fmt, args));
end;

initialization
  lock := TCriticalSection.Create;
  target := StdErr;

finalization
  if closeTarget then
    CloseFile(target);
    lock.Free;

end.
