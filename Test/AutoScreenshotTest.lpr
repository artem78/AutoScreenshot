program AutoScreenshotTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, consoletestrunner, fpcunittestrunner, UtilsTests,
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

