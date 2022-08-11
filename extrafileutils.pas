unit ExtraFileUtils;

{$mode delphi}{$h+}

interface

uses
  Classes, SysUtils;

type

  { TFileSearch }

  TFileSearch = class(TObject)
  private
    fsearch : TThread;
  protected
    procedure ThreadProc;
  public
    BaseDir : String;
    filesFound : TStringList;
    procedure StartSearch(const ABaseDir: string);
    procedure StopSearch;
    function IsSearching: BOolean;
  end;

function DeleteDirectory(const dir: string): Boolean;

implementation

type

  { TSearchThread }

  TSearchThread = class(TThread)
  private

  protected
    srch: TFileSearch;
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

{ TFileSearch }

procedure TFileSearch.ThreadProc;
var
  dirs  : TStringList;
begin
  dirs  := TStringList.Create;
  try
    dirs.Add(BaseDir);
    while dirs.Count>0 do begin

    end;
  finally
    dirs.Free;
  end;
end;

procedure TFileSearch.StartSearch(const ABaseDir: string);
begin

end;

procedure TFileSearch.StopSearch;
begin

end;

function TFileSearch.IsSearching: BOolean;
begin
  Result := Assigned(fsearch) and (not fsearch.Finished);
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

end.
