program testgatherfiles;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, extrafileutils;

var
  fs : TAsyncFileSearch;
  d  : string;
  l  : TStringList;
  t  : Int64;
begin
  if ParamCount=0 then
    d := GetCurrentDir
  else
    d := ParamStr(1);
  try
    fs := TAsyncFileSearch.Create;
    l := TStringList.Create;
    try
      t := GetTickCount64;
      fs.StartSearch(d);
      repeat
        l.Clear;
        if fs.GatherNewFound(l) > 0 then
          write(l.Text);
      until not fs.IsSearching;
      t := GetTickCount64 - t;
      l.Clear;
      if fs.GatherNewFound(l) > 0 then
        write(l.Text);
      writeln('total found: ', fs.GatherAllFound(nil));
      writeln('time: ', t,'ms');
    finally
      l.Free;
      fs.Free;
    end;
  except
    on e: exception do
      writeln('error: ', e.message);
  end;
end.

