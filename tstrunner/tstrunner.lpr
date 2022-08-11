program tstrunner;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, tstrunnercore, tstrunnerlog;

var
  ShowHelp : Boolean = false;
  inp      : TTestInput; // subject application
  Target   : TStringList;

procedure PrintHelp;
begin
  if inp.subject ='' then
    writeln('Subject needs to be specified');
  writeln('tstrunner [%options%] [%dir or file%]');
  writeln;
  writeln(' -h - show help');
  writeln(' -s %filename% - the test subject application');
end;

procedure ParseParams;
var
  i : integer;
  l : string;
  p : string;
begin
  i:=1;
  while i<=ParamCount do begin
    p := ParamStr(i);
    l := AnsiLowerCase(p);
    if Pos('-',l)=1 then begin
    if l = '-h' then begin
      ShowHelp := true;
    end else if l = '-s' then begin
      inc(i);
      if i<=ParamCOunt then
        inp.subject := ParamStr(i);
    end;
    end else
      Target.Add(p);

    inc(i);
  end;

  if ShowHelp and (inp.Subject = '') then
    inp.Subject := '*' // something dummy not to be printed
  else if inp.Subject ='' then
    ShowHelp := true
  else begin;
    if Target.Count =0 then
      Target.Add(GetCurrentDir);
  end;
end;

begin
  try
    Target := TStringList.Create;
    try
      InitTestInput(inp);
      Target.CaseSensitive := {$ifdef linux}true{$else}false{$endif};
      Target.Duplicates  := dupIgnore;
      ParseParams;

      if ShowHelp then begin
        PrintHelp;
        Exit;
      end;

      Log('starting. Subject: "%s"', [inp.subject]);
      PerformTests(Target, inp);
      Log('done');

    finally
      Target.Free;
    end;
  except
    on e: exception do begin
      Error(e.Message);
      ExitCode := 1;
    end;
  end;
end.

