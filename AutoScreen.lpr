program AutoScreen;

{$MODE objfpc}

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
  uLanguages in 'uLanguages.pas';

begin
  // heaptrc settings
  {$if declared(useHeapTrace)}
    globalSkipIfNoLeaks := true;
  {$endIf}
  //setHeapTraceOutput('trace.log');

  Application.Scaled:=True;
  Application.Initialize;
  Application.HintHidePause := -1; // Do not hide hint
  Application.Title := 'Auto Screenshot';
  Application.CreateForm(TMainForm, MainForm);
  Application.ShowMainForm := False;
  Application.Run;
end.
