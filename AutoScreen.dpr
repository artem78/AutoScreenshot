program AutoScreen;

{$R 'res\MAINICON.res' 'res\MAINICON.rc'}
{$R 'res\VERSIONINFO.res' 'res\VERSIONINFO.rc'}

uses
  Forms,
  uAutoScreen in 'uAutoScreen.pas' {MainForm},
  uAbout in 'uAbout.pas' {AboutForm},
  uLocalization in 'uLocalization.pas',
  uUtils in 'uUtils.pas';

//{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Auto Screenshot'; // ToDo: Also rename executable to AutoScreenshot.exe
  Application.CreateForm(TMainForm, MainForm);
  Application.ShowMainForm := False;
  Application.Run;
end.
