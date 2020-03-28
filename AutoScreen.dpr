program AutoScreen;

uses
  Forms,
  uAutoScreen in 'uAutoScreen.pas' {Form1};
  
{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Auto Screenshot'; // ToDo: Also rename executable to AutoScreenshot.exe
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
