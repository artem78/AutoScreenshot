program AutoScreen;

{$R 'res\MAINICON.res' 'res\MAINICON.rc'}
{$R 'res\VERSIONINFO.res' 'res\VERSIONINFO.rc'}
{$R 'res\ICONS.res' 'res\ICONS.rc'}

uses
  Forms,
  Windows,
  Dialogs,
  uAutoScreen in 'uAutoScreen.pas' {MainForm},
  uAbout in 'uAbout.pas' {AboutForm},
  uLocalization in 'uLocalization.pas',
  uUtils in 'uUtils.pas',
  VistaAltFixUnit in 'libs\VistaAltFixUnit.pas',
  uLanguages in 'uLanguages.pas';

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
  Application.Title := 'Auto Screenshot';
  Application.HintHidePause := -1; // Do not hide hint
  Application.CreateForm(TMainForm, MainForm);
  Application.ShowMainForm := False;
  Application.Run;

  CloseHandle(MutexHandle);
end.
