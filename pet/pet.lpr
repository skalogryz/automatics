program pet;
// this is a pet project to help with some tstrunner test samples
// it simply executes some instuctions its is being told to do
{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes;

var
  cmd : TStringList;

procedure ParseCommands;
var
  i : integer;
  c : string;
  p : string;
begin
  i := 1;
  c := '';
  while i <= ParamCount do begin
    p := ParamStr(i);
    if (p = ';') or (p = ',') then begin
      cmd.Add(c);
      c := '';
    end else begin
      if c = '' then c := ParamStr(i)
      else c := c+ ' '+Paramstr(i);
    end;
    inc(i);
  end;
  if c <>'' then cmd.Add(c);
end;

function GetWord(const s: string; var i: integer): string;
var
  j : integer;
begin
  while (i<=length(s)) and (s[i] in [#32,#9]) do inc(i);
  j:=i;
  while (i<=length(s)) and not (s[i] in [#32,#9]) do inc(i);
  Result:=Trim(Copy(s,j,i-j));
end;

procedure WriteBin(const fn: string; const bin: string);
var
  b    : string;
  i    : integer;
  fs   : TFileStream;
  bb   : byte;
  err  : integer;
begin
  b := bin;
  b := StringReplace(b, '$',  '', [rfReplaceAll]);
  b := StringReplace(b, '0x', '', [rfReplaceAll]);
  b := StringReplace(b, ' ',  '', [rfReplaceAll]);
  b := StringReplace(b, #9,   '', [rfReplaceAll]);
  fs := TFileStream.Create(fn, fmCreate);
  try
    i:=1;
    while i<=length(b) do begin
      Val( '$'+copy(b, i, 2), bb, err);
      fs.WriteByte(bb);
      inc(i,2);
    end;
  finally
    fs.Free;
  end;
end;

procedure ExecCommands(const cmd: TStrings);
var
  j   : integer;
  i   : integer;
  c   : string;
  cc  : string;
  fn  : string;
  txt : Text;
  t   : integer;
begin
  for i:=0 to cmd.Count-1 do begin
    j:=1;
    c := cmd[i];
    cc := AnsiLowerCase(GetWord(c, j));
    if cc = 'sleep' then begin
      t := StrToIntDef( trim(Copy(c, j, length(c))), -1);
      if t>=0 then Sleep(t);
    end else if cc = 'echo' then begin
      writeln(StdOut, trim(Copy(c, j, length(c))));
    end else if cc = 'echoerr' then begin
      writeln(StdErr, trim(Copy(c, j, length(c))));
    end else if cc = 'echovar' then begin
      writeln(StdOut, GetEnvironmentVariable(trim(Copy(c, j, length(c)))));
    end else if cc = 'write' then begin
      fn := GetWord(c,j);
      AssignFile(txt, fn); Rewrite(txt);
      Write(txt, trim(Copy(c, j, length(c))));
      CloseFile(txt);
    end else if cc = 'writebin' then begin
      fn := GetWord(c,j);
      WriteBin(fn, trim(Copy(c, j, length(c))));
    end else if cc = 'exitcode' then begin
      ExitCode := StrToIntDef( trim(Copy(c, j, length(c))), ExitCode);
    end;
  end;
end;

procedure PrintHelp;
begin
  writeln('i am a pet project. I execute commands given on the command-line');
  writeln(' pet [%command1% [;%command2% ...[; %commandN]]]');
  writeln;
  writeln('commands:');
  writeln('  sleep    %N% - pause for N miliseconds');
  writeln('  echo     %txt% - write to stdout text given at txt');
  writeln('  echoerr  %txt% - write to stderr text given at txt');
  writeln('  echovar  %nm% - write to stdout environment variable named nm');
  writeln('  write    %fn% %txt% - write txt to file named fn. File is rewritten');
  writeln('  writebin %fn% %hex% - write binary codes given in hex to file named fn');
  writeln('  exitcode %N% - set the exit code to N');
end;

begin
  cmd := TStringList.Create;
  try
    ParseCommands;
    if cmd.count=0 then begin
      PrintHelp;
      Exit;
    end;
    ExecCommands(cmd);
  finally
    cmd.Free;
  end;
end.

