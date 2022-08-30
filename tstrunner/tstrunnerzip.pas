unit tstrunnerzip;

interface

{$mode delphi}

uses
  SysUtils, Classes, Zipper,
  tstrunnercore, ExtraFileUtils;

function ZipDirToFile(const inpDir, dstZipFileName: string): Boolean;

implementation

function ZipDirToFile(const inpDir, dstZipFileName: string): Boolean;
var
  z : TZipper;
  r : TStrings;
  i : integer;
  arch : string;
  d    : string;
begin
  try
    d := IncludeTrailingPathDelimiter(inpDir);
    r := FindAllFiles(inpDir);
    z := TZipper.Create;
    try
      for i := 0 to r.Count-1 do begin
        arch := r[i];
        arch := Copy(arch, length(d)+1, length(arch));
        arch := SlashToUnix(arch);
        z.Entries.AddFileEntry(r[i], arch);
      end;
      z.SaveToFile(dstZipFileName);
      Result := true;
    finally
      z.Free;
      r.Free;
    end;
  except
    Result := false;
  end;
end;

end.
