program tstrunner;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, tstrunnercore, tstrunnerlog, runtesttypes;

var
  ShowHelp : Boolean = false;
  SkipSubject : Boolean = false;
  inp      : TTestInput; // subject application
  Target   : TStringList;
  res      : TStringList;
  outFn    : string;

procedure PrintHelp;
begin
  if inp.subject ='' then
    writeln('Subject needs to be specified');
  writeln('tstrunner [%options%] [%dir or file%]');
  writeln;
  writeln(' -s %filename% - the test subject application');
  writeln(' -s- - no sujbect application to be used');
  writeln(' -o %filename% - result name');
  writeln(' -l %filename% - log file name');
  writeln(' -l- - disable logging');
  writeln(' -v - enable verbose output');
  writeln(' -h - show help');
end;

procedure ResultsToCSV(var dst: Text; res: TStringList);
var
  i : integer;
  info : TFileRunInfo;
begin
  res.Sort;
  write(dst, FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', now));
  writeln(dst);
  write(dst, inp.subject);
  writeln(dst);
  writeln('test,filename,result');
  for i := 0 to res.Count-1 do begin
    info := TFileRunInfo(res.Objects[i]);
    write(dst,  ExtractFileName(res[i]) );
    write(dst, ',', res[i]);
    write(dst, ',', TestResultNameStr[info.GetTestResult()]);
    writeln(dst);
  end;
end;

procedure ResultsToCSV(res: TStringList);
var
  f : Text;
begin
  if outFn = ''
    then ResultsToCsv(StdOut, res)
  else begin
    AssignFile(f, outFn); Rewrite(f);
    try
      ResultsToCsv(f, res);
    finally
      CloseFile(f);
    end;
  end;
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
      end else if l = '-l-' then begin
        EnableLog := false;
      end else if l = '-l' then begin
        inc(i);
        if i<=ParamCount then
          SetLogFile(ParamStr(i));
      end else if l = '-v' then begin
        EnableVerbose := true;
      end else if l = '-s-' then begin
        SkipSubject := true;
      end else if l = '-s' then begin
        inc(i);
        if i<=ParamCount then
          inp.subject := ParamStr(i);
      end else if l = '-o' then begin
        inc(i);
        if i<=ParamCount then
          outFn:= ParamStr(i);
      end;
    end else
      Target.Add(p);

    inc(i);
  end;

  if ShowHelp and (inp.Subject = '') then
    inp.Subject := '*' // something dummy not to be printed
  else if (inp.Subject ='') and not SkipSubject then
    ShowHelp := true
  else begin;
    if Target.Count =0 then
      Target.Add(GetCurrentDir);
  end;
end;

begin
  try
    Target := TStringList.Create;
    res := TStringList.Create;
    res.OwnsObjects := true;
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
      PerformTests(Target, inp, res);
      if (res.Count>0) then begin
        Log('producing results');
        ResultsToCSV(res);
      end else
        Log('no tests found');
      Log('done');

    finally
      Target.Free;
      res.Free;
    end;
  except
    on e: exception do begin
      Error(e.Message);
      ExitCode := 1;
    end;
  end;
end.

