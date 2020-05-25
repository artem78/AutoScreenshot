program AutoScreen;

{$R 'res\MAINICON.res' 'res\MAINICON.rc'}
{$R 'res\VERSIONINFO.res' 'res\VERSIONINFO.rc'}

uses
  Forms, Windows, Dialogs,
  uAutoScreen in 'uAutoScreen.pas' {MainForm},
  uAbout in 'uAbout.pas' {AboutForm},
  uLocalization in 'uLocalization.pas',
  uUtils in 'uUtils.pas';

//{$R *.res}

var
  MutexHandle: THandle;
  //MutexName: PChar;
const
  MutexName = 'AutoScreenshotMutex';
begin
  // Prevent the launch of more than one instance of process
  //MutexName := PChar(Application.ExeName);
  MutexHandle := OpenMutex(MUTEX_ALL_ACCESS, False, MutexName);
  if MutexHandle <> 0 then
  begin
    CloseHandle(MutexHandle);
    MessageDlg('Program already running!', mtWarning, [mbOk], 0);
    Exit;
  end;
  MutexHandle := CreateMutex(nil, false, MutexName);

  Application.Initialize;
  Application.Title := 'Auto Screenshot'; // ToDo: Also rename executable to AutoScreenshot.exe
  Application.CreateForm(TMainForm, MainForm);
  Application.ShowMainForm := False;
  Application.Run;

  CloseHandle(MutexHandle);
end.
