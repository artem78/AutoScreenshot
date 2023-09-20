program AutoScreenshotTestGUI;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, fpcunittestrunner,
  {$I testcases.inc};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

