program AutoScreen;

uses
  Forms,
  uAutoScreen in 'uAutoScreen.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Auto Screenshot'; // ToDo: Also rename executable to AutoScreenshot.exe
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
