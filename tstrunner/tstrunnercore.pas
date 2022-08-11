unit tstrunnercore;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ExtraFileUtils;

type
  TTestInput = record
    subject : string;
  end;

  { TTestPerformer }

  TTestPerformer = class(TObject)
  private
    filesToTest : TStringList;
    filesIdx    : integer;
    dirToTest   : TStringList;
    dirDone     : Boolean;
    pendingProc : TList;
    ctrlThread  : TThread;
    cancel      : Boolean;
    fileExt     : TStringList;

  protected
    procedure ControlThreadProc;
    procedure StartDirSearch;
    procedure CheckDirSearch;
    procedure VerifyFilesFromDirSearch(l: TStringList);
    procedure CheckTestFiles;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Init(Target: TStringList; const inp : TTestInput);
    procedure StartTests;
    function IsDone: Boolean;
    procedure Abort;
  end;

procedure PerformTests(Target: TStringList; const inp : TTestInput);

implementation

type

  { TControlThread }

  TControlThread = class(TThread)
  protected
    p : TTestPerformer;
    procedure Execute; override;
  public
    constructor Create(performer: TTestPerformer);
  end;

procedure PerformTests(Target: TStringList; const inp : TTestInput);
var
  t : TTestPerformer;
begin
  t := TTestPerformer.Create;
  try
    t.Init(Target, inp);
    t.StartTests;
    while not t.IsDone do begin
      Sleep(15);
    end;
    write(t.filesToTest.Text);
  finally
    t.Free;
  end;
end;

{ TControlThread }

procedure TControlThread.Execute;
begin
  try
    p.ControlThreadProc;
  except
  end;
end;

constructor TControlThread.Create(performer: TTestPerformer);
begin
  inherited Create(true);
  p := performer;
end;

{ TTestPerformer }

procedure TTestPerformer.ControlThreadProc;
var
  i    : integer;
  lfn  : TStringList;
begin
  StartDirSearch;

  lfn := TStringList.Create;
  try
    while true do begin
      if not dirDone then begin
        CheckDirSearch;
        dirDone := dirToTest.Count=0;
      end;
    end;
  finally
    lfn.Free;
  end;
end;

procedure TTestPerformer.CheckDirSearch;
var
  i    : integer;
  srch : TAsyncFileSearch;
  lfn  : TStringList;
begin
  lfn := TStringList.Create;
  try
    for i := dirToTest.Count-1 downto 0 do begin
      srch := TAsyncFileSearch(dirToTest.Objects[i]);
      lfn.Clear;
      if srch.GatherNewFound(lfn) > 0 then
        VerifyFilesFromDirSearch(lfn);
      if not srch.IsSearching then begin
        srch.Free;
        dirToTest.Delete(i);
      end;
    end;
  finally
    lfn.Free;
  end;
end;

procedure TTestPerformer.VerifyFilesFromDirSearch(l: TStringList);
var
  i : integer;
  x  : string;
begin
  if (l = nil) or (l.Count = 0) then Exit;
  for i:=0 to l.Count-1 do begin
    x := AnsiLowerCase( ExtractFileExt(l[i]));
    if (fileExt.IndexOf(x)>=0) then
      filesToTest.Add(l[i]);
  end;
end;

procedure TTestPerformer.CheckTestFiles;
begin

end;

procedure TTestPerformer.StartDirSearch;
var
  i    : integer;
  srch : TAsyncFileSearch;
begin
  for i:=0 to dirToTest.Count-1 do begin
    srch := TAsyncFileSearch.Create;
    srch.StartSearch(dirToTest[i]);
    dirToTest.Objects[i] := srch;
  end;
end;

constructor TTestPerformer.Create;
begin
  inherited Create;
  filesToTest := TStringList.Create;
  dirToTest   := TStringList.Create;
  pendingProc := TList.Create;
  fileExt := TStringList.Create;
  fileExt.Add('.tst');
  fileExt.Add('.test');
  fileExt.Add('.testbat');
end;

destructor TTestPerformer.Destroy;
begin
  fileExt.Free;
  pendingProc.Free;
  filesToTest.Free;
  dirToTest.Free;
  inherited Destroy;
end;

procedure TTestPerformer.Init(Target: TStringList; const inp: TTestInput);
var
  d           : string;
  i           : integer;
begin
  // initial collecting  of files and directories to test
  writeln('target= ',Target.Count);
  for i:=0 to Target.Count-1 do begin
    d := Target[i];
    if DirectoryExists(d) then
      dirToTest.Add(d)
    else
      filesToTest.Add(d);
  end;
end;

procedure TTestPerformer.StartTests;
begin
  if Assigned(ctrlThread) then Exit;

  dirDone := dirToTest.Count=0;
  ctrlThread := TControlThread.Create(Self);
  ctrlThread.Start;
end;

function TTestPerformer.IsDone: Boolean;
begin
  Result := {(filesIdx >= filesToTest.Count)
    and (pendingProc.Count = 0);
    and} (dirdone);
end;

procedure TTestPerformer.Abort;
begin

end;

end.

