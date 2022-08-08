unit plainsyntaxexec;

interface

{$mode delphi}{$H+}

uses
  Classes, SysUtils, runtesttypes, logger;

type

  { TPlainSyntaxExec }

  TPlainSyntaxExec = class(TObject)
  public
    procedure RunCommands(cmds: TList);
  end;

implementation

{ TPlainSyntaxExec }

procedure TPlainSyntaxExec.RunCommands(cmds: TList);
begin

end;

end.
