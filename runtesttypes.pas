unit runtesttypes;

interface

{$mode delphi}{$H+}

type
  TTestResult = (trFail, trSuccess, trUnableToRun);

const
  TestResultNameStr : array [TTestResult] of string = ('Success', 'Fail', 'Unable to run');

implementation

end.
