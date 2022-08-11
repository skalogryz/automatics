unit runtesttypes;

interface

{$mode delphi}{$H+}

type
  TTestResult = (trFail, trSuccess, trUnableToRun);

const
  TestResultNameStr : array [TTestResult] of string = ('Fail', 'Success', 'Unable to run');

implementation

end.
