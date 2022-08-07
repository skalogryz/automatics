program runprocsample;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, runproctypes, runprocutils
  { you can add units after this };

procedure PrintHelp;
begin
  writeln('please specify the executable to run');
  writeln(' [%options%] %executable% [%arguments%]');
  writeln;
  writeln('options:');
  writeln(' -d - delete temporary directory created');
end;

var
  s     : string;
  prm   : string;
  i     : integer;
  args  : TStringList;
  inp   : TRunInput;
  p     : TRunProcess;
  res   : TRunResult;
  lprm  : string;

  deleteTempFolder : Boolean;
begin
  if ParamCount=0 then begin
    PrintHelp;
    exit;
  end;
  deleteTempFolder := false;
  args := TStringList.Create;
  try
    i := 1;
    s := '';
    while i <= ParamCount do begin
      prm := ParamStr(i);
      if (s='') and (Pos('-', prm)=1) then begin
        lprm := AnsiLowerCase(prm);
        if lprm = '-d' then deleteTempFolder := true;
        // this is a parameter
      end else if s = '' then begin
        s := prm;
      end else
        args.Add(prm);
      inc(i);
    end;
    if s = '' then begin
      writeln('please specify executable');
      exit;
    end;
    ProcPrepare(s, inp);
    ProcPrepareTempDir(inp);
    ProcArgs(inp, args);
    inp.timeOutMs := 1000 * 5;

    writeln('running: ', s);
    p := ProcStart(inp);
    writeln('waiting for it to finish');
    ProcWaitLoop(p);
    p.GetResults(res);
    DumpResult(res);
    if deleteTempFolder then
      ProcDeleteTempFolder(p);
    p.Free;
  finally
    args.Free;
  end;
end.

