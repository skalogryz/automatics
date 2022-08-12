unit tstrunnerbi;
{ tstrunning Business Intelligence tool.
  Sorry. i've been working for too long in the "enterprise environment" 

  The purpose of the unit is to convert the existing structures code into "reportable" data entries.
  Not really a BI tool
}

interface

uses
  Classes, SysUtils,
  HashStrings,
  runtesttypes,
  tstrunnercore,
  plainsyntaxexec, plainsyntaxtype;

// we're not using RTTI. or maybe we should?

procedure SubjectDateToRep(const subj: string; const datetime: string; entry: TStrings);
procedure PlainExecToRep(exec: TPlainSyntaxExec; entry: TStrings);
procedure FileRunInfoToRep(info: TFileRunInfo; entry: TStrings);

const
  data_Subject     = 'subject';
  data_RunTestDate = 'starttimestamp';
  data_FileName    = 'filename';  // just the filename of the .tst
  data_FullName    = 'fullname';  // jull path of the file of the .tst file
  data_Dir         = 'pathname';  // the name of the directory of the .tst file
  data_Result      = 'result';    // the string value of the test result: Success, Fail, Unable to Run
  data_UnRunLine   = 'unrunline'; // unable to run reason line

  defaultColumns : TStringList = nil;

procedure ResultsToRepEntries(src: TStringList;
  const subj, datetimestamp: string; dst: TList);
procedure ReleaseRepEntries(dst: TList);
procedure SortRep(rep: TList; const colName: string);

procedure ReportCSV(
  template: TStringList; // colum names
  reps: TList; {of TSTrings}
  var dst: Text);

function CSVStr(const s: string): string;

implementation

procedure ReleaseRepEntries(dst: TList);
var
  i : integer;
begin
  for i:=0 to dst.Count-1 do
    TObject(dst[i]).Free;
end;

procedure SortRep(rep: TList; const colName: string);
begin

end;

function CSVStr(const s: string): string;
var
  i : integer;
begin
  for i:=1 to length(s) do
    if (s[i]=',') or (s[i] = #13) or (s[i]=#10) then begin
      Result:='"'+StringReplace(s, '"','""', [rfReplaceAll])+'"';
      Exit;
    end;
  Result := s;
end;

procedure ReportCSV(template: TStringList; reps: TList; var dst: Text);
var
  i : integer;
  j : integer;
  rp : TStrings;
begin
  if not Assigned(template) or not Assigned(reps) then Exit;
  for i:=0 to template.Count-1 do begin
    if i >0 then write(dst, ',');
    write(dst, CSVStr(template[i]));
  end;
  writeln(dst);
  for j := 0 to reps.Count-1 do begin
    rp := TStrings(reps[j]);

    for i:=0 to template.Count-1 do begin
      if i > 0 then write(dst, ',');
      write(dst, CSVStr( rp.Values[ template[i]]));
    end;
    writeln(dst);
  end;
end;

procedure ResultsToRepEntries(src: TStringList;
  const subj, datetimestamp: string;
  dst: TList);
var
  i  : integer;
  f  : TFileRunInfo;
  h  : THashedStringList;
begin
  if (dst = nil) or (src = nil) then Exit;
  for i:=0 to src.Count-1 do begin
    f := TFileRunInfo(src.Objects[i]);
    h := THashedStringList.Create;
    SubjectDateToRep(subj, datetimestamp, h);
    FileRunInfoToRep(f, h);
    dst.Add(h);
  end;
end;

procedure PlainExecToRep(exec: TPlainSyntaxExec; entry: TStrings);
var
  r    : TTestResult;
  lc   : TPlainCommand;
  lexe : TCommandExecResult;
begin
  if (exec = nil) or (entry = nil) then Exit;
  r := exec.FinalResult;
  if exec.CommandLogs.Count>0 then begin
    lexe := TCommandExecResult(exec.CommandLogs[exec.CommandLogs.Count-1]);
    lc := lexe.cmd;
  end else begin
    lc := nil;
    lexe := nil;
  end;

  if (r = trUnableToRun) and Assigned(lc) then begin
    entry.Values[data_UnRunLine]:=IntToStr(lc.lineNum)
  end else
    entry.Values[data_UnRunLine]:='';
end;

procedure FileRunInfoToRep(info: TFileRunInfo; entry: TStrings);
var
  fn : string;
begin
  if (info = nil) or (entry = nil) then Exit;
  fn := info.fn;
  entry.Values[data_Fullname] := fn;
  entry.Values[data_fileName] := ExtractFileName(fn);
  entry.Values[data_Dir] := ExtractFileDir(fn);
  entry.Values[data_Result]:=TestResultNameStr[info.GetTestResult()];
  if Assigned(info.exec) then
    PlainExecToRep(info.exec, entry);
end;

procedure SubjectDateToRep(const subj: string; const datetime: string; entry: TStrings);
begin
  if (entry = nil) then Exit;
  entry.Values[data_Subject] := subj;
  entry.Values[data_RunTestDate] := datetime;
end;

initialization
  defaultColumns := THashedStringList.Create;
  defaultColumns.OwnsObjects := true;
  defaultColumns.Add(data_Subject);
  defaultColumns.Add(data_RunTestDate);
  defaultColumns.Add(data_FileName);
  //defaultColumns.Add(data_FullName);
  defaultColumns.Add(data_Dir);
  defaultColumns.Add(data_Result);
  defaultColumns.Add(data_UnRunLine);

finalization
  defaultColumns.Free;

end.
