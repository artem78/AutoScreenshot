program AutoScreenTest;

//{$APPTYPE CONSOLE}

uses
  SysUtils,
  TestFrameWork,
  GUITestRunner,
  uTestCases in 'uTestCases.pas',
  uUtils in '..\uUtils.pas';

begin
  GUITestRunner.RunRegisteredTests;
end.
