program AutoScreenshotTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, consoletestrunner, fpcunittestrunner, uTestCases,
  GrabberTests;

{$R *.res}

var
  Application: TTestRunner;

begin
  Application := TTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.

