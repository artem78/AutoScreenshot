program AutoScreen;

uses
  Forms,
  uAutoScreen in 'uAutoScreen.pas' {MainForm},
  uAbout in 'uAbout.pas' {AboutForm},
  uLocalization in 'uLocalization.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Auto Screenshot'; // ToDo: Also rename executable to AutoScreenshot.exe
  Application.CreateForm(TMainForm, MainForm);
  Application.ShowMainForm := False;
  Application.Run;
end.
