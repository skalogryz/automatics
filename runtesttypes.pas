unit runtesttypes;

interface

{$mode delphi}{$H+}

type
  TTestResult = (trSuccess, trFail, trUnableToRun);

const
  TestResultNameStr : array [TTestResult] of string = ('Success', 'Fail', 'Unable to run');

implementation

end.
