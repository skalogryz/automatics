unit ExtraFileUtils;

{$mode delphi}{$h+}

interface

uses
  Classes, SysUtils, syncobjs;

type

  { TAsyncFileSearch }

  TAsyncFileSearch = class(TObject)
  private
    fsearch    : TThread;
    flock      : TCriticalSection;
    filesFound : TStringList;
    freported  : Integer;
  protected
    procedure ThreadProc;
    procedure AddFile(const fn: string);
  public
    BaseDir    : String;
    fcancel    : Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure StartSearch(const ABaseDir: string);
    procedure StopSearch;
    function IsSearching: BOolean;
    function GatherNewFound(dst: TStrings): Integer;
    function GatherAllFound(dst: TStrings): Integer;
  end;

function DeleteDirectory(const dir: string): Boolean;

// replaces all the slashes to system native slashes
// \ - windows
// / - for unix systems (anything but windows)
function SlashToNative(const s: string): string;

function SlashToUnix(const s: string): string;
function SlashToWindows(const s: string): string;

function ExpandPathOnBase(const absbase, relPath: string): string;

implementation

type

  { TSearchThread }

  TSearchThread = class(TThread)
  private

  protected
    srch: TAsyncFileSearch;
    procedure Execute; override;
  end;

{ TSearchThread }

procedure TSearchThread.Execute;
begin
  try
    srch.ThreadProc;
  except
  end;
end;

{ TAsyncFileSearch }

procedure TAsyncFileSearch.ThreadProc;
var
  dirs  : TStringList;
  sr    : TSearchRec;
  fn    : String;
  d     : string;
  based : string;
begin
  dirs  := TStringList.Create;
  try
    dirs.Add(BaseDir);
    while dirs.Count>0 do begin
      if fcancel then Break;
      based := dirs[0];
      dirs.Delete(0);

      d := IncludeTrailingPathDelimiter(based);
      if FindFirst(d+AllFilesMask,faAnyFile, sr)<> 0 then
        continue;
      try
        repeat
          // check if special file
          if (sr.Name='.') or (sr.Name='..') or (sr.Name='') then
            continue;
          fn:=d+sr.Name;
          if ((sr.Attr and faDirectory)>0) then
            dirs.add(fn)
          else
            AddFile(fn);
          if fcancel then Break;
        until FindNext(sr)<>0;
      finally
        FindClose(sr);
      end;
    end;
  finally
    dirs.Free;
  end;
end;

procedure TAsyncFileSearch.AddFile(const fn: string);
begin
  flock.Enter;
  try
    filesFound.Add(fn);
  finally
    flock.Leave;
  end;
end;

constructor TAsyncFileSearch.Create;
begin
  inherited Create;
  flock := TCriticalSection.Create;
  filesFound := TStringList.Create;
end;

destructor TAsyncFileSearch.Destroy;
begin
  StopSearch;
  filesFound.Free;
  flock.Free;
  inherited Destroy;
end;

procedure TAsyncFileSearch.StartSearch(const ABaseDir: string);
begin
  StopSearch;
  fcancel := false;

  filesFound.Clear;
  freported := 0;

  BaseDir := ABaseDir;
  fSearch := TSearchThread.Create(true);
  TSearchThread(fSearch).srch := Self;
  fSearch.Start;
end;

procedure TAsyncFileSearch.StopSearch;
begin
  if not Assigned(fsearch) then Exit;
  fcancel := true;
  fsearch.WaitFor;
  fsearch.Free;
  fsearch := nil;
end;

function TAsyncFileSearch.IsSearching: BOolean;
begin
  Result := Assigned(fsearch) and (not fsearch.Finished);
end;

function TAsyncFileSearch.GatherNewFound(dst: TStrings): Integer;
var
  i : integer;
begin
  flock.Enter;
  try
    Result := filesFound.count - freported;
    if Assigned(dst) then begin
      for i :=freported to filesFound.count-1 do
        dst.Add(filesFound[i]);
      freported := filesFound.count;
    end;
  finally
    flock.Leave;
  end;
end;

function TAsyncFileSearch.GatherAllFound(dst: TStrings): Integer;
begin
  flock.Enter;
  try
    Result := filesFound.count;
    if Assigned(dst) then
      dst.AddStrings(filesFound);
  finally
    flock.Leave;
  end;
end;

function DeleteDirectory(const dir: string): Boolean;
const
  //Don't follow symlinks on *nix, just delete them
  DeleteMask = faAnyFile {$ifdef unix} or faSymLink{%H-} {$endif unix};
var
  sr: TSearchRec;
  fn: String;
  d : string;
  based : string;
begin
  Result:=false;
  based := ExpandFileName(dir);
  d := IncludeTrailingPathDelimiter(based);
  if FindFirst(d+AllFilesMask,DeleteMask, sr)<> 0 then Exit;

  try
    repeat
      // check if special file
      if (sr.Name='.') or (sr.Name='..') or (sr.Name='') then
        continue;
      fn:=d+sr.Name;
      if ((sr.Attr and faDirectory)>0)
         {$ifdef unix} and ((sr.Attr and faSymLink{%H-})=0) {$endif unix} then begin
        if not DeleteDirectory(fn) then exit;
      end else begin
        if not DeleteFile(fn) then exit;
      end;
    until FindNext(sr)<>0;
  finally
    FindClose(sr);
  end;
  Result := RemoveDir(based);
end;


function SlashToNative(const s: string): string;
const
  Native  = {$ifdef mswindows}'\'{$else}'/'{$endif};
  Hostile = {$ifdef mswindows}'/'{$else}'\'{$endif};
begin
  Result := StringReplace(s, Hostile, Native, [rfReplaceAll]);
end;

function SlashToUnix(const s: string): string;
begin
  Result := StringReplace(s, '\', '/', [rfReplaceAll]);
end;

function SlashToWindows(const s: string): string;
begin
  Result := StringReplace(s, '/', '\', [rfReplaceAll]);
end;

function ExpandPathOnBase(const absbase, relPath: string): string;
var
  i : integer;
  j : integer;
  t : string;
begin
  Result := absbase;
  j:=1;
  i:=j;
  while (i<=length(relPath)) do begin
    if (relPath[i] in ['/','\']) then begin
      t := Copy(relPath, j, i-j);
      j := i+1;
      if (t = '..') then Result := extractFileDir(Result)
      else Result := IncludeTrailingPathDelimiter(Result)+t;
    end;
    inc(i);
  end;
  i := length(relPath)+1;
  t := Copy(relPath, j, i-j);
  if (t = '..') then Result := extractFileDir(Result)
  else Result := IncludeTrailingPathDelimiter(Result)+t;
end;

end.
