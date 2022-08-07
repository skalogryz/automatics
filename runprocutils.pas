unit runprocutils;

interface

{$ifdef fpc}{$mode delphi}{$endif}

uses
  SysUtils, runproctypes;

procedure DumpResult(const run: TRunResult);

implementation

procedure DumpResult(const run: TRunResult);
begin
  if run.runError = 0 then begin
    writeln('Start Time:     ', DateTimeToStr(run.startTime));
    writeln('End Time:       ', DateTimeToStr(run.endTime));
    writeln('Start Tick:     ', run.startTick);
    writeln('End   Tick:     ', run.endTick);
    writeln('----------');
    writeln('Total Run Time: ', run.runTimeMs);
    writeln('Timed out:      ', run.timedOut);
    writeln('Exit Code:      ', run.exitCode);
    writeln('----------');
    writeln('Std Out Size:   ', run.outSize);
    writeln('                ', run.stdOutFn);
    writeln('Std Err Size:   ', run.errSize);
    writeln('                ', run.stdErrFn);
  end else begin
    writeln('failed to run: ', run.runSysErr);
  end;
end;

end.
