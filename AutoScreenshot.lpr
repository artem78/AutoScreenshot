program AutoScreenshot;

{$MODE objfpc}
//{$Define LAZLOGGER_FLUSH}

{$R *.res}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Forms,
  LCLIntf, LCLType, LMessages, SysUtils,
  Dialogs, Interfaces,
  uAutoScreen in 'uAutoScreen.pas' {MainForm},
  uAbout in 'uAbout.pas' {AboutForm},
  uLocalization in 'uLocalization.pas',
  uUtils in 'uUtils.pas',
  uLanguages in 'uLanguages.pas',
  UniqueInstanceRaw;

begin
  // heaptrc settings
  {$if declared(useHeapTrace)}
    globalSkipIfNoLeaks := true;
  {$endIf}
  //setHeapTraceOutput('trace.log');

  // Prevent to run second instance
  if InstanceRunning('AutoScreenshot') then
  begin
    Application.MessageBox('Another instance is running!', 'Error', MB_ICONERROR + MB_OK);
    Application.Terminate;
    Exit;
  end;

  Application.Scaled:=True;
  Application.Initialize;
  Application.HintHidePause := -1; // Do not hide hint
  Application.Title := 'Auto Screenshot';
  Application.CreateForm(TMainForm, MainForm);
  Application.ShowMainForm := False;
  Application.Run;
end.
