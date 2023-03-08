unit uAutoScreen;

{$mode objfpc}{$H+}

interface

uses
  {$IfDef windows}
  Windows,
  {$EndIf}
  {$IfDef Linux}
  xlib, xrandr, XRandREventWatcher,
  {$EndIf}
  {Messages,} SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, {ComCtrls,} ExtCtrls, StdCtrls, inifiles, Spin, {FileCtrl,}
  Menus, Buttons, EditBtn, UniqueInstance, uLocalization, DateTimePicker,
  LCLIntf, ScreenGrabber, uHotKeysForm, uUtilsMore, GlobalKeyHook,
  ZStream { for Tcompressionlevel };

type
  TTrayIconState = (tisDefault, tisBlackWhite, tisFlashAnimation);

  { TMainForm }

  TMainForm = class(TForm)
    AutoCheckForUpdatesMenuItem: TMenuItem;
    CompressionLevelComboBox: TComboBox;
    HotKetsSettingsMenuItem: TMenuItem;
    CompressionLevelLabel: TLabel;
    ImageFormatOptionsPanel: TPanel;
    DonateMenuItem: TMenuItem;
    PostCmdLabel: TLabel;
    PostCmdEdit: TEdit;
    CheckForUpdatesMenuItem: TMenuItem;
    OutputDirEdit: TDirectoryEdit;
    Timer: TTimer;
    OutputDirLabel: TLabel;
    CaptureIntervalLabel: TLabel;
    TrayIcon: TTrayIcon;
    ImageFormatLabel: TLabel;
    TakeScreenshotButton: TButton;
    JPEGQualityLabel: TLabel;
    JPEGQualitySpinEdit: TSpinEdit;
    OpenOutputDirButton: TButton;
    StopWhenInactiveCheckBox: TCheckBox;
    ImageFormatComboBox: TComboBox;
    JPEGQualityPercentLabel: TLabel;
    AutoCaptureControlGroup: TGroupBox;
    StartAutoCaptureButton: TBitBtn;
    StopAutoCaptureButton: TBitBtn;
    TrayIconPopupMenu: TPopupMenu;
    ExitTrayMenuItem: TMenuItem;
    TakeScreenshotTrayMenuItem: TMenuItem;
    RestoreWindowTrayMenuItem: TMenuItem;
    ToggleAutoCaptureTrayMenuItem: TMenuItem;
    Separator2TrayMenuItem: TMenuItem;
    StartCaptureOnStartUpCheckBox: TCheckBox;
    StartMinimizedCheckBox: TCheckBox;
    Separator1TrayMenuItem: TMenuItem;
    FileNameTemplateLabel: TLabel;
    FileNameTemplateComboBox: TComboBox;
    FileNameTemplateHelpButton: TButton;
    GrayscaleCheckBox: TCheckBox;
    ColorDepthLabel: TLabel;
    ColorDepthComboBox: TComboBox;
    CaptureIntervalDateTimePicker: TDateTimePicker;
    TrayIconAnimationTimer: TTimer;
    AutoRunCheckBox: TCheckBox;
    MonitorLabel: TLabel;
    MonitorComboBox: TComboBox;
    MainMenu: TMainMenu;
    HelpSubMenu: TMenuItem;
    AboutMenuItem: TMenuItem;
    OptionsSubMenu: TMenuItem;
    LanguageSubMenu: TMenuItem;
    UniqueInstance: TUniqueInstance;
    SeqNumberGroup: TGroupBox;
    SeqNumberValueLabel: TLabel;
    SeqNumberValueSpinEdit: TSpinEdit;
    SeqNumberDigitsCountSpinEdit: TSpinEdit;
    SeqNumberDigitsCountLabel: TLabel;
    procedure CheckForUpdatesMenuItemClick(Sender: TObject);
    procedure AutoCheckForUpdatesMenuItemClick(Sender: TObject);
    procedure CompressionLevelComboBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HotKetsSettingsMenuItemClick(Sender: TObject);
    procedure DonateMenuItemClick(Sender: TObject);
    procedure OutputDirEditChange(Sender: TObject);
    procedure CaptureIntervalDateTimePickerChange(Sender: TObject);
    procedure PostCmdEditChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ApplicationMinimize(Sender: TObject);
    procedure StartAutoCaptureButtonClick(Sender: TObject);
    procedure StopAutoCaptureButtonClick(Sender: TObject);
    procedure TakeScreenshotButtonClick(Sender: TObject);
    procedure JPEGQualitySpinEditChange(Sender: TObject);
    procedure OpenOutputDirButtonClick(Sender: TObject);
    procedure StopWhenInactiveCheckBoxClick(Sender: TObject);
    procedure ImageFormatComboBoxChange(Sender: TObject);
    procedure ToggleAutoCaptureTrayMenuItemClick(Sender: TObject);
    procedure RestoreWindowTrayMenuItemClick(Sender: TObject);
    procedure TakeScreenshotTrayMenuItemClick(Sender: TObject);
    procedure ExitTrayMenuItemClick(Sender: TObject);
    procedure StartCaptureOnStartUpCheckBoxClick(Sender: TObject);
    procedure StartMinimizedCheckBoxClick(Sender: TObject);
    procedure FileNameTemplateComboBoxChange(Sender: TObject);
    procedure FileNameTemplateHelpButtonClick(Sender: TObject);
    procedure GrayscaleCheckBoxClick(Sender: TObject);
    procedure ColorDepthComboBoxChange(Sender: TObject);
    procedure TrayIconAnimationTimerTimer(Sender: TObject);
    procedure AutoRunCheckBoxClick(Sender: TObject);
    procedure MonitorComboBoxChange(Sender: TObject);
    procedure AboutMenuItemClick(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
    procedure SeqNumberValueSpinEditChange(Sender: TObject);
    procedure SeqNumberDigitsCountSpinEditChange(Sender: TObject);
  private
    { Private declarations }

    { Fields and variables }
    AvailableLanguages: TLanguagesArray;
    FLanguage: TLanguageCode;  { ??? }
    FColorDepth: TColorDepth;
    FTrayIconState: TTrayIconState;

    TrayIconIdx: 1..7;

    FCounter: Integer;
    FCounterDigits: Integer {Byte};

    {$IfDef windows}
    PrevWndProc: WndProc;
    {$EndIf}
    {$IfDef Linux}
    XWatcher: TXRandREventWatcherThread;
    {$EndIf}
    Grabber: TScreenGrabber;

    FStopWhenInactive: Boolean;
    FStartMinimized: Boolean;
    FAutoRun: Boolean;
    FGrayscale: Boolean;

    KeyHook: TGlobalKeyHook;
    
    { Methods }
    procedure SetTimerEnabled(AEnabled: Boolean);
    function GetTimerEnabled: Boolean;
    function GetFinalOutputDir: String;
    function GetImagePath: String;
    procedure SetImageFormatByStr(FmtStr: String);
    procedure SetImageFormat(Fmt: TImageFormat);
    function GetImageFormat: TImageFormat;
    procedure SetColorDepth(AColorDepth: TColorDepth);
    function GetColorDepth: TColorDepth;
    procedure SetTrayIconState(IconState: TTrayIconState);
    procedure MakeScreenshot;
    procedure MinimizeToTray;
    procedure RestoreFromTray;
    //procedure SetLanguage(Lang: TLanguage);
    procedure SetLanguageByCode(LangCode: TLanguageCode);
    procedure TranslateForm;
    procedure InitUI;
    procedure ReadSettings;
    procedure UpdateColorDepthValues;
    procedure UpdateMonitorList;
    procedure FillMonitorList;
    procedure SetMonitorId(MonitorId: Integer);
    function GetMonitorId: Integer;
    procedure UpdateLanguages;
    procedure LanguageClick(Sender: TObject);
    function GetLangCodeOfLangMenuItem(const LangItem: TMenuItem): TLanguageCode;
    function FindLangMenuItem(ALangCode: TLanguageCode): TMenuItem;
    procedure RecalculateLabelWidths;
    procedure RecalculateLabelWidthsForSeqNumGroup;
    function FormatPath(Str: string): string;
    procedure SetCounter(Val: Integer);
    procedure SetCounterDigits(Val: Integer);
    procedure UpdateSeqNumGroupVisibility;
    procedure SetJPEGQuality(Val: Integer);
    function GetJPEGQuality: Integer;
    procedure SetStopWhenInactive(const Val: Boolean);
    procedure SetStartMinimized(const Val: Boolean);
    procedure SetAutoRun(const Val: Boolean);
    procedure SetGrayscale(const Val: Boolean);
    procedure SetPostCommand(ACmd: String);
    function GetPostCommand: String;
    function GetMonitorWithCursor: Integer;
    procedure CheckForUpdates(AShowMessageWhenNoUpdates: Boolean);
    function GetAutoCheckForUpdates: Boolean;
    procedure SetAutoCheckForUpdates(AVal: Boolean);
    procedure SetStartAutoCaptureHotKey(AHotKey: THotKey);
    procedure SetStopAutoCaptureHotKey(AHotKey: THotKey);
    procedure SetSingleCaptureHotKey(AHotKey: THotKey);
    procedure SetCompressionLevel(ALevel: Tcompressionlevel);
    function GetCompressionLevel: Tcompressionlevel;
    procedure UpdateFormAutoSize;

    {$IfDef Linux}
    procedure OnScreenConfigurationChanged(const AEvent: TXEvent);
    {$EndIf}


    { Properties }
    property IsTimerEnabled: Boolean read GetTimerEnabled write SetTimerEnabled;
    property FinalOutputDir: String read GetFinalOutputDir;
    property ImagePath: String read GetImagePath;
    //property Language: TLanguage read FLanguage write SetLanguage;
    property ImageFormat: TImageFormat read GetImageFormat write SetImageFormat;
    property ColorDepth: TColorDepth read GetColorDepth write SetColorDepth;
    property TrayIconState: TTrayIconState write SetTrayIconState;
    property MonitorId: Integer read GetMonitorId write SetMonitorId;
    property Counter: Integer read FCounter write SetCounter;
    property CounterDigits: {Byte} Integer read FCounterDigits write SetCounterDigits;
    property JPEGQuality: Integer read GetJPEGQuality write SetJPEGQuality;
    property StopWhenInactive: Boolean read FStopWhenInactive write SetStopWhenInactive;
    property StartMinimized: Boolean read FStartMinimized write SetStartMinimized;
    property AutoRun: Boolean read FAutoRun write SetAutoRun;
    property Grayscale: Boolean read FGrayscale write SetGrayscale;
    property PostCommand: String read GetPostCommand write SetPostCommand;
    property AutoCheckForUpdates: Boolean read GetAutoCheckForUpdates write SetAutoCheckForUpdates;
    property CompressionLevel: Tcompressionlevel read GetCompressionLevel write SetCompressionLevel;

    // Messages
    {$IfDef Windows}
    procedure WMHotKey(var AMsg: TMessage); message WM_HOTKEY;
    {$EndIf}
  public
    { Public declarations }
  end;

const
  DefaultConfigIniSection = 'main';
  HotKeysIniSection = 'hotkeys';

  MinCaptureIntervalInSeconds = 1;
  NoMonitorId = -1;
  MonitorWithCursor = -2;
  MinCounterValue  = 1;
  MinCounterDigits = 1;
  MaxCounterDigits = 10;
  UpdateCheckIntervalInSeconds = 3 * 24 * 60 * 60; // Every 3 days

var
  MainForm: TMainForm;
  Ini: TIniFile;

implementation

uses uAbout, DateUtils, StrUtils, uUtils, Math, uFileNameTemplateHelpForm,
fphttpclient, opensslsockets, fpjson, jsonparser, uIniHelper, FileUtil,
Idle, LCLType;

{$R *.lfm}

const
  LanguageSubMenuItemNamePrefix = 'LanguageSubMenuItem_';

{$IfDef windows}
function WndCallback(MyHWND: HWND; uMSG: UINT; wParam: WParam; lParam: LParam): LRESULT; StdCall;
begin
  case uMSG of
    WM_DISPLAYCHANGE, // Screen resolution/orientation changed
    WM_DEVICECHANGE:  // Any hardware configuration changed (including monitors)
      begin
        MainForm.UpdateMonitorList;
      end;
  end;

  //if WindowInfo^.WinControl is TForm1 then //Eliminate form1 global variable for safer handling.
  //  Result:= CallWindowProc(TForm1(WindowInfo^.WinControl).PrevWndProc, MyHWND, uMSG, WParam, LParam);

  Result := Windows.CallWindowProc(MainForm.PrevWndProc, MyHWND, uMsg, WParam, LParam);
end;
{$EndIf}

function MyGetApplicationName: String;
begin
  Result := 'AutoScreenshot';
end;

procedure TMainForm.InitUI;
var
  Fmt: TImageFormat;
begin
  {$IFOPT D+}
    MainForm.Caption := MainForm.Caption + '    [DEBUG BUILD]';
  {$ENDIF}

  // Set default tray icon
  TrayIconState := tisDefault;
  TrayIcon.Hint := Application.Title;

  // Fill combobox with image formats
  for Fmt in TImageFormat do
    ImageFormatComboBox.Items.Append(ImageFormatInfoArray[Fmt].Name);

  // Set min/max values for JPEG quality
  JPEGQualitySpinEdit.MinValue := Low(TJPEGQualityRange);
  JPEGQualitySpinEdit.MaxValue := High(TJPEGQualityRange);

  // Icons
  StartAutoCaptureButton.Glyph.LoadFromResourceName(HInstance, '_START_ICON');
  StopAutoCaptureButton.Glyph.LoadFromResourceName(HInstance, '_STOP_ICON');

  // Available languages
  UpdateLanguages;

  // Sequential number
  SeqNumberValueSpinEdit.MinValue := MinCounterValue;
  SeqNumberDigitsCountSpinEdit.MinValue := MinCounterDigits;
  SeqNumberDigitsCountSpinEdit.MaxValue := MaxCounterDigits;

  // Available monitors
  UpdateMonitorList;

  // Predefined filename templates
  with FileNameTemplateComboBox.Items do
  begin
    Clear;
    Append('screenshot %Y-%M-%D %H-%N-%S');
    Append('%Y' + PathDelim + '%M' + PathDelim + '%D' + PathDelim + 'screenshot %H-%N-%S');
    Append('%Y-%M' + PathDelim + '%D' + PathDelim + 'screenshot %H-%N-%S ');
    Append('%COMP' + PathDelim + '%USER' + PathDelim + 'screenshot %Y-%M-%D %H-%N-%S ');
    Append('screenshot %NUM');
  end;
end;

procedure TMainForm.ReadSettings;
const
  DefaultFileNameTemplate = '%Y-%M-%D\%Y-%M-%D %H.%N.%S';
  DefaultCaptureInterval  = 5;
  DefaultImageFormat      = fmtPNG;
  DefaultJPEGQuality      = 80;
  DefaultLanguage         = 'en';
  DefaultColorDepth       = cd24Bit;
  DefaultMonitorId        = NoMonitorId;
  DefaultCounterValue     = MinCounterValue;
  DefaultCounterDigits    = 6;
  DefaultCompressionLevel = cldefault;
var
  DefaultOutputDir, BaseDir: String;
  CfgLang, SysLang, AltLang: TLanguageCode;
  FmtStr: String;
  Seconds: Integer;
begin
  if IsPortable then
    BaseDir := ExtractFilePath(Application.ExeName)
  else
    BaseDir := GetUserPicturesDir();
  DefaultOutputDir := IncludeTrailingPathDelimiter(ConcatPaths([BaseDir, 'screenshots']));
  OutputDirEdit.Text := Ini.ReadString(DefaultConfigIniSection, 'OutputDir', DefaultOutputDir);
  // ToDo: Check that directory exists or can be created (with subdirs if needed)
  if OutputDirEdit.Text = '' then
    OutputDirEdit.Text := DefaultOutputDir;

  FileNameTemplateComboBox.Text := Ini.ReadString(DefaultConfigIniSection, 'FileNameTemplate', DefaultFileNameTemplate);

  Seconds := Round(Ini.ReadFloat(DefaultConfigIniSection, 'CaptureInterval', DefaultCaptureInterval) * SecsPerMin);
  Seconds := Max(Seconds, MinCaptureIntervalInSeconds);
  CaptureIntervalDateTimePicker.Time := EncodeTime(0, 0, 0, 0);
  CaptureIntervalDateTimePicker.Time := IncSecond(CaptureIntervalDateTimePicker.Time, Seconds);

  StopWhenInactive := Ini.ReadBool(DefaultConfigIniSection, 'StopWhenInactive', False);

  // Image format
  FColorDepth := TColorDepth(0); // Set value as unitialized to prevent
        // reset to max available value in UpdateColorDepthValues() before
        // reading color depth from ini file
  FmtStr := Ini.ReadString(DefaultConfigIniSection, 'ImageFormat',
      ImageFormatInfoArray[DefaultImageFormat].Name);
  try
    SetImageFormatByStr(FmtStr);
  except
    ImageFormat := DefaultImageFormat;
  end;

  JPEGQuality := Ini.ReadInteger(DefaultConfigIniSection, 'JPEGQuality', DefaultJPEGQuality);

  Grayscale := Ini.ReadBool(DefaultConfigIniSection, 'Grayscale', False);

  // Color depth
  try
    ColorDepth := TColorDepth(Ini.ReadInteger(DefaultConfigIniSection,
        'ColorDepth', Integer(DefaultColorDepth)));
  except
    FColorDepth := DefaultColorDepth;
  end;

  // Language
  try
    CfgLang := Ini.ReadString(DefaultConfigIniSection, 'Language', '');
    SetLanguageByCode(CfgLang);
  except
    try
      SysLang := GetSystemLanguageCode;
      SetLanguageByCode(SysLang);
    except
      try
        AltLang := GetAlternativeLanguage(AvailableLanguages, SysLang);
        SetLanguageByCode(AltLang);
      except
        SetLanguageByCode(DefaultLanguage);
      end;
    end;
  end;

  // Start autocapture
  Timer.Interval := SecondOfTheDay(CaptureIntervalDateTimePicker.Time) * MSecsPerSec;
  StartCaptureOnStartUpCheckBox.Checked :=
      Ini.ReadBool(DefaultConfigIniSection, 'StartCaptureOnStartUp', {True} False);
  IsTimerEnabled := StartCaptureOnStartUpCheckBox.Checked;

  // Start with OS
  AutoRun := Ini.ReadBool(DefaultConfigIniSection, 'AutoRun', False);
  
  // Start minimized
  StartMinimized := Ini.ReadBool(DefaultConfigIniSection, 'StartMinimized', False);
  if StartMinimized then
    MinimizeToTray
  else
    RestoreFromTray;

  // Multiple monitors
  try
    MonitorId := Ini.ReadInteger(DefaultConfigIniSection, 'Monitor', DefaultMonitorId);
  except
    MonitorId := DefaultMonitorId;
  end;

  // Incremental counter
  Counter := Ini.ReadInteger(DefaultConfigIniSection, 'Counter', DefaultCounterValue);
  CounterDigits := Ini.ReadInteger(DefaultConfigIniSection, 'CounterDigits', DefaultCounterDigits);
  UpdateSeqNumGroupVisibility;

  // User command
  PostCommand := Ini.ReadString(DefaultConfigIniSection, 'PostCmd', '');

  // Auto checking for updates
  AutoCheckForUpdates := Ini.ReadBool(DefaultConfigIniSection, 'AutoCheckForUpdates', True);

  // Compression level
  CompressionLevel := Tcompressionlevel(Ini.ReadInteger(DefaultConfigIniSection, 'Compression', Ord(DefaultCompressionLevel)));
end;

procedure TMainForm.FormCreate(Sender: TObject);
const
  StartAutoCaptureDefaultHotKey: THotKey = (
    ShiftState: [ssCtrl];
    Key: VK_F5;
  );
  StopAutoCaptureDefaultHotKey: THotKey = (
    ShiftState: [ssCtrl];
    Key: VK_F6;
  );
  SingleCaptureDefaultHotKey: THotKey = (
    ShiftState: [ssCtrl];
    Key: VK_F7;
  );
var
  ///////
  ColorDepthTmp: TColorDepth;
  ////////
  LastUpdateCheck: TDateTime;
  HotKey: THotKey;
  IniFileName: String;
begin
  {$IfDef windows}
  { Replace default window function with custom one
    for process messages when screen configuration changed }
  PrevWndProc := Windows.WNDPROC
    (SetWindowLongPtr(Self.Handle, GWL_WNDPROC {GWLP_WNDPROC}, PtrUInt(@WndCallback)));
  {$EndIf}

  Application.OnMinimize := @ApplicationMinimize;

  InitUI;

  OnGetApplicationName := @MyGetApplicationName;
  if IsPortable then
    IniFileName := ConcatPaths([ProgramDirectory, 'config.ini'])
  else
    IniFileName := ConcatPaths([GetAppConfigDir(False), 'config.ini']);
  Ini := TIniFile.Create(IniFileName);
  ReadSettings;

  //if FindCmdLineSwitch('autorun') then
  //  OutputDebugString('AutoRun');

  //////////////
  ColorDepthTmp := cd24Bit; // Any value
  try
    ColorDepthTmp := ColorDepth;
  except
  end;
  ///////////////
  Grabber := TScreenGrabber.Create(ImageFormat, {ColorDepth} ColorDepthTmp, JPEGQuality,
    Grayscale, CompressionLevel);
  
  // Check for updates when program starts
  LastUpdateCheck := Ini.ReadDateTime(DefaultConfigIniSection, 'LastCheckForUpdates', 0);
  if AutoCheckForUpdates and (SecondsBetween(Now, LastUpdateCheck) > UpdateCheckIntervalInSeconds) then
    CheckForUpdates(False);

  // Enable global hotkeys
  KeyHook := TGlobalKeyHook.Create(Handle, 'AutoScreenshot');
  HotKey := Ini.ReadHotKey(HotKeysIniSection, 'StartAutoCapture', StartAutoCaptureDefaultHotKey);
  KeyHook.RegisterKey('StartAutoCapture', HotKey);
  HotKey := Ini.ReadHotKey(HotKeysIniSection, 'StopAutoCapture', StopAutoCaptureDefaultHotKey);
  KeyHook.RegisterKey('StopAutoCapture', HotKey);
  HotKey := Ini.ReadHotKey(HotKeysIniSection, 'SingleCapture', SingleCaptureDefaultHotKey);
  KeyHook.RegisterKey('SingleCapture', HotKey);

  {$IfDef Linux}
  // Enable monitor confuguration changed updates in Linux
  XWatcher := TXRandREventWatcherThread.Create(RRScreenChangeNotifyMask, @OnScreenConfigurationChanged);
  {$EndIf}
end;

procedure TMainForm.CheckForUpdatesMenuItemClick(Sender: TObject);
begin
  CheckForUpdates(True);
end;

procedure TMainForm.AutoCheckForUpdatesMenuItemClick(Sender: TObject);
begin
  AutoCheckForUpdates := not AutoCheckForUpdates;
end;

procedure TMainForm.CompressionLevelComboBoxChange(Sender: TObject);
begin
  CompressionLevel := Tcompressionlevel(CompressionLevelComboBox.ItemIndex);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  XWatcher.Terminate;
  XWatcher.Free;
  Grabber.Free;
  KeyHook.Free;

  Ini.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  RecalculateLabelWidths;
end;

procedure TMainForm.HotKetsSettingsMenuItemClick(Sender: TObject);
var
  HotKeysForm: THotKeysForm;
begin
  HotKeysForm := THotKeysForm.Create(Nil);
  HotKeysForm.StartAutoCaptureKey := Self.KeyHook.FindHotKey('StartAutoCapture');
  HotKeysForm.StopAutoCaptureKey := Self.KeyHook.FindHotKey('StopAutoCapture');
  HotKeysForm.SingleCaptureKey := Self.KeyHook.FindHotKey('SingleCapture');
  if HotKeysForm.ShowModal = mrOK then
  begin
    SetStartAutoCaptureHotKey(HotKeysForm.StartAutoCaptureKey);
    SetStopAutoCaptureHotKey(HotKeysForm.StopAutoCaptureKey);
    SetSingleCaptureHotKey(HotKeysForm.SingleCaptureKey);
  end;

  HotKeysForm.Free;
end;

procedure TMainForm.DonateMenuItemClick(Sender: TObject);
begin
  ShowMessage('PayPal: megabyte1024@yandex.com');
end;

procedure TMainForm.OutputDirEditChange(Sender: TObject);
begin
    Ini.WriteString(DefaultConfigIniSection, 'OutputDir', OutputDirEdit.Text);
end;

procedure TMainForm.CaptureIntervalDateTimePickerChange(Sender: TObject);
var
  Seconds: Integer;
begin
  Seconds := SecondOfTheDay(CaptureIntervalDateTimePicker.Time);
  if Seconds < MinCaptureIntervalInSeconds then
  begin
    Seconds := MinCaptureIntervalInSeconds;
    CaptureIntervalDateTimePicker.Time := EncodeTime(0, 0, 0, 0);
    CaptureIntervalDateTimePicker.Time := IncSecond(CaptureIntervalDateTimePicker.Time, Seconds);
  end;
  Ini.WriteFloat(DefaultConfigIniSection, 'CaptureInterval', Seconds / SecsPerMin);
  Timer.Interval := Seconds * MSecsPerSec;
end;

procedure TMainForm.PostCmdEditChange(Sender: TObject);
begin
  Ini.WriteString(DefaultConfigIniSection, 'PostCmd', PostCommand);
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  if StopWhenInactive then
  begin
    // Skip taking screenshot if there are no user activity
    // for autocapture interval minutes

    // ToDo: May add check for screensaver active
    // or user logged off from the session
    // ToDo: May add comparision of current screenshot with the last one,
    // and if they equal, do not save current

    if Timer.Interval > UserIdleTime then
      MakeScreenshot;
  end
  else
    MakeScreenshot;
end;

function TMainForm.GetTimerEnabled: Boolean;
begin
  Result := Timer.Enabled;
end;

procedure TMainForm.SetTimerEnabled(AEnabled: Boolean);
begin
  Timer.Enabled := AEnabled;
  StartAutoCaptureButton.Enabled := not AEnabled;
  StopAutoCaptureButton.Enabled := AEnabled;
  // Tray menu
  ToggleAutoCaptureTrayMenuItem.Checked := AEnabled;
  // Tray icon
  if AEnabled then
    TrayIconState := tisDefault
  else
    TrayIconState := tisBlackWhite;
end;

procedure TMainForm.StartAutoCaptureButtonClick(Sender: TObject);
begin
  IsTimerEnabled := True;
end;

procedure TMainForm.StopAutoCaptureButtonClick(Sender: TObject);
begin
  IsTimerEnabled := False;
end;

procedure TMainForm.ApplicationMinimize(Sender: TObject);
begin
  MinimizeToTray;
end;

procedure TMainForm.MakeScreenshot;
var
  Cmd: String;
begin
  TrayIconState := tisFlashAnimation;

  if MonitorId = NoMonitorId then
    Grabber.CaptureAllMonitors(ImagePath)
  else
  begin
    if MonitorId = MonitorWithCursor then
      Grabber.CaptureMonitor(ImagePath, GetMonitorWithCursor)
    else
      Grabber.CaptureMonitor(ImagePath, MonitorId);
  end;


  // Run user command
  try
    Cmd := PostCommand;
    if Cmd <> '' then
    begin
      Cmd := StringReplace(Cmd, '%FILENAME%', ImagePath, [rfReplaceAll{, rfIgnoreCase}]);
      RunCmdInbackground(Cmd);
    end;
  except
  end;

  // Increment counter after successful capture
  //Inc(Counter);
  Counter := Counter + 1;
end;

procedure TMainForm.TakeScreenshotButtonClick(Sender: TObject);
var
  DefaultTransparency: Byte;
begin
  DefaultTransparency := AlphaBlendValue; // Save current transparency value (usually = 255)
  // Set form transparency to 100%
  AlphaBlendValue := 0;
  AlphaBlend := True;
  try
    MakeScreenshot;
  finally
    // Restore transparency to initial value
    AlphaBlendValue := DefaultTransparency;
    AlphaBlend := False;
  end;
end;

procedure TMainForm.JPEGQualitySpinEditChange(Sender: TObject);
begin
  if Ini = Nil then
    Exit;

  try
    Ini.WriteInteger(DefaultConfigIniSection, 'JPEGQuality', JPEGQuality);
    if Grabber <> nil then
      Grabber.Quality := JPEGQuality;
  finally
  end;
end;

function TMainForm.GetFinalOutputDir: String;
var
  BaseDir, SubDir, FullDir: String;
begin
  BaseDir := Ini.ReadString(DefaultConfigIniSection, 'OutputDir', '');

  SubDir := ExtractFileDir({Ini.ReadString(DefaultConfigIniSection, 'FileNameTemplate', '')} FileNameTemplateComboBox.Text);
  SubDir := FormatPath(SubDir);

  FullDir := IncludeTrailingPathDelimiter(ConcatPaths([BaseDir, SubDir]));

  if not DirectoryExists(FullDir) then
  begin
    if not ForceDirectories(FullDir) then
      RaiseLastOSError;
  end;

  Result := FullDir;
end;

function TMainForm.GetImagePath: String;
var
  DirName, FileName: String;
begin
  FileName := ExtractFileName(FileNameTemplateComboBox.Text);
  FileName := FormatPath(FileName);

  DirName := IncludeTrailingPathDelimiter(FinalOutputDir);

  Result := DirName + FileName + '.' + ImageFormatInfoArray[ImageFormat].Extension;
end;

procedure TMainForm.OpenOutputDirButtonClick(Sender: TObject);
begin
  OpenDocument(FinalOutputDir);
end;

procedure TMainForm.StopWhenInactiveCheckBoxClick(Sender: TObject);
begin
  StopWhenInactive := StopWhenInactiveCheckBox.Checked;
end;

procedure TMainForm.ImageFormatComboBoxChange(Sender: TObject);
var
  Format: TImageFormat;
  IsQualityVisible, IsGrayscaleVisible, IsCompressionLevelVisible: Boolean;
begin
  DisableAutoSizing;

  try
    Format := ImageFormat;
    IsQualityVisible := ImageFormatInfoArray[Format].HasQuality;

    JPEGQualitySpinEdit.Visible := IsQualityVisible;
    JPEGQualityLabel.Visible    := IsQualityVisible;
    JPEGQualityPercentLabel.Visible := IsQualityVisible;

    IsGrayscaleVisible := ImageFormatInfoArray[Format].HasGrayscale;
    GrayscaleCheckBox.Visible := IsGrayscaleVisible;

    IsCompressionLevelVisible := ImageFormatInfoArray[Format].HasCompressionLevel;
    CompressionLevelLabel.Visible := IsCompressionLevelVisible;
    CompressionLevelComboBox.Visible := IsCompressionLevelVisible;

    UpdateColorDepthValues;
  
    Ini.WriteString(DefaultConfigIniSection, 'ImageFormat', ImageFormatInfoArray[Format].Name);

    if Grabber <> nil then
      Grabber.ImageFormat := Format;
  finally
    EnableAutoSizing;

    UpdateFormAutoSize;
  end;
end;

procedure TMainForm.ToggleAutoCaptureTrayMenuItemClick(Sender: TObject);
begin
  IsTimerEnabled := not IsTimerEnabled;
end;

procedure TMainForm.RestoreWindowTrayMenuItemClick(Sender: TObject);
begin
  RestoreFromTray;
end;

procedure TMainForm.TakeScreenshotTrayMenuItemClick(Sender: TObject);
begin
  // ToDo: How to minimize this delay?
  Sleep(700); // Add some delay before capture, otherwise the popup
              // menu may be still visible on screenshot
              // (without delay: https://ibb.co/mHGKHzL)
  MakeScreenshot;
end;

procedure TMainForm.ExitTrayMenuItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.MinimizeToTray;
begin
  {TrayIcon.AppVisible := False;
  TrayIcon.FormVisible := False;
  TrayIcon.IconVisible := True; }

  {Application.MainFormOnTaskBar := False;
  Application.ShowMainForm := False;
  //Hide;}
  WindowState := wsMinimized;
  Hide;
  TrayIcon.Show;
end;

procedure TMainForm.RestoreFromTray;
begin
  {TrayIcon.IconVisible := False;
  TrayIcon.AppVisible := True;
  TrayIcon.FormVisible := True;
  Application.Restore;
  Application.BringToFront();}

  TrayIcon.Hide;
  {Application.MainFormOnTaskBar := True;
  Application.ShowMainForm := True;
  //Show;
  Application.Restore;
  Application.BringToFront;}
  WindowState := wsNormal;
  Show;
end;

//procedure TMainForm.SetLanguage(Lang: TLanguage);
//begin
//  {if (FLanguage = Lang) then
//    Exit;}
//
//  FLanguage := Lang;
//  Ini.WriteString(DefaultConfigIniSection, 'Language', LanguageCodes[Lang]);
//  LanguageSubMenu.Items[Ord(Lang)].Checked := True;
//  Localizer.SetLang(LanguageCodes[Lang]);
//  TranslateForm;
//end;

procedure TMainForm.SetLanguageByCode(LangCode: TLanguageCode);
var
  LangIdx: integer;
begin
  for LangIdx := 0 to Length(AvailableLanguages) - 1 do
  begin
    if LangCode = AvailableLanguages[LangIdx].Code then
    begin
      FLanguage := LangCode;

      Ini.WriteString(DefaultConfigIniSection, 'Language', LangCode);
      FindLangMenuItem(LangCode).Checked := True;
      Localizer.LoadFromFile(AvailableLanguages[LangIdx].FileName);
      TranslateForm;

      Exit;
    end;
  end;

  raise Exception.CreateFmt('Unknown language code "%s"', [LangCode]);
end;

procedure TMainForm.TranslateForm;
begin
  DisableAutoSizing;

  try
    // Menubar
    OptionsSubMenu.Caption := Localizer.I18N('Options');
    LanguageSubMenu.Caption := Localizer.I18N('Language');
    HelpSubMenu.Caption := Localizer.I18N('Help');
    AboutMenuItem.Caption := Localizer.I18N('About') + '...';
    CheckForUpdatesMenuItem.Caption := Localizer.I18N('CheckForUpdates');
    AutoCheckForUpdatesMenuItem.Caption := Localizer.I18N('AutoCheckForUpdates');
    HotKetsSettingsMenuItem.Caption := Localizer.I18N('EditHotKeys') + '...';
    DonateMenuItem.Caption := Localizer.I18N('Donate');

    // Main form components
    OutputDirLabel.Caption := Localizer.I18N('OutputDirectory') + ':';
    OutputDirEdit.DialogTitle := Localizer.I18N('SelectOutputDirectory');
    OpenOutputDirButton.Caption := Localizer.I18N('OpenDirectory');
    OpenOutputDirButton.Hint := Localizer.I18N('OpenDirectoryHint');
    FileNameTemplateLabel.Caption := Localizer.I18N('FileNameTemplate') + ':';
    CaptureIntervalLabel.Caption := Localizer.I18N('CaptureInterval') + ':';
    StopWhenInactiveCheckBox.Caption := Localizer.I18N('PauseCaptureWhenIdle');
    StopWhenInactiveCheckBox.Hint := Localizer.I18N('PauseCaptureWhenIdleHint');
    ImageFormatLabel.Caption := Localizer.I18N('Format') + ':';
    ColorDepthLabel.Caption := Localizer.I18N('ColorDepth') + ':';
    JPEGQualityLabel.Caption := Localizer.I18N('Quality') + ':';
    GrayscaleCheckBox.Caption := Localizer.I18N('Grayscale');
    AutoCaptureControlGroup.Caption := Localizer.I18N('AutoCapture');
    StartAutoCaptureButton.Caption := Localizer.I18N('StartCapture');
    StopAutoCaptureButton.Caption := Localizer.I18N('StopCapture');
    TakeScreenshotButton.Caption := Localizer.I18N('TakeScreenshot');
    StartCaptureOnStartUpCheckBox.Caption := Localizer.I18N('StartCaptureOnStartUp');
    StartMinimizedCheckBox.Caption := Localizer.I18N('StartMinimized');
    AutoRunCheckBox.Caption := Localizer.I18N('AutoRun');
    MonitorLabel.Caption := Localizer.I18N('UsedMonitor') + ':';
    FillMonitorList;
    SeqNumberGroup.Caption := Localizer.I18N('SequentialNumber');
    SeqNumberValueLabel.Caption := Localizer.I18N('NextValue') + ':';
    SeqNumberDigitsCountLabel.Caption := Localizer.I18N('Digits') + ':';
    PostCmdLabel.Caption := Localizer.I18N('RunCommand') + ':';
    PostCmdEdit.Hint := Localizer.I18N('RunCommandHelpText');

    CompressionLevelLabel.Caption := Localizer.I18N('CompressionLevel') + ':';
    with CompressionLevelComboBox do
    begin
      Items[0] := Localizer.I18N('CompressionLevelNone');
      Items[1] := Localizer.I18N('CompressionLevelFastest');
      Items[2] := Localizer.I18N('CompressionLevelDefault');
      Items[3] := Localizer.I18N('CompressionLevelMax');
    end;

    // Tray icon
    RestoreWindowTrayMenuItem.Caption := Localizer.I18N('Restore');
    ToggleAutoCaptureTrayMenuItem.Caption := Localizer.I18N('EnableAutoCapture');
    TakeScreenshotTrayMenuItem.Caption := Localizer.I18N('TakeScreenshot');
    ExitTrayMenuItem.Caption := Localizer.I18N('Exit');

  finally
    EnableAutoSizing;

    // Recalculate with of labels area
    RecalculateLabelWidths;

    UpdateFormAutoSize;
  end;
end;

procedure TMainForm.StartCaptureOnStartUpCheckBoxClick(Sender: TObject);
begin
  Ini.WriteBool(DefaultConfigIniSection, 'StartCaptureOnStartUp', StartCaptureOnStartUpCheckBox.Checked);
end;

procedure TMainForm.StartMinimizedCheckBoxClick(Sender: TObject);
begin
  StartMinimized := StartMinimizedCheckBox.Checked;
end;

procedure TMainForm.FileNameTemplateComboBoxChange(Sender: TObject);
begin
  Ini.WriteString(DefaultConfigIniSection, 'FileNameTemplate', FileNameTemplateComboBox.Text);

  UpdateSeqNumGroupVisibility
end;

procedure TMainForm.FileNameTemplateHelpButtonClick(Sender: TObject);
begin
  //ShowMessage(Localizer.I18N('FileNameTemplateHelpText')); // Can`t show tabs in Windows

  with TFileNameTemplateHelpForm.Create(Application) do
  begin
    ShowModal;
  end;
end;

function TMainForm.GetImageFormat: TImageFormat;
begin
  Result := TImageFormat(ImageFormatComboBox.ItemIndex);
end;

procedure TMainForm.SetImageFormatByStr(FmtStr: String);
var
  Fmt: TImageFormat;
begin
  for Fmt in TImageFormat do
  begin
    if ImageFormatInfoArray[Fmt].Name = FmtStr then
    begin
      SetImageFormat(Fmt);
      Exit;
    end;
  end;

  raise Exception.CreateFmt('Unknown format "%s"', [FmtStr]);
end;

procedure TMainForm.SetImageFormat(Fmt: TImageFormat);
begin
  ImageFormatComboBox.ItemIndex := Ord(Fmt);
  ImageFormatComboBox.OnChange(ImageFormatComboBox);
end;

procedure TMainForm.GrayscaleCheckBoxClick(Sender: TObject);
begin
  Grayscale := GrayscaleCheckBox.Checked;
end;

procedure TMainForm.ColorDepthComboBoxChange(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := ColorDepthComboBox.ItemIndex;
  if Idx <> -1 then
    ColorDepth := TColorDepth(PtrUint(ColorDepthComboBox.Items.Objects[Idx]));
end;

procedure TMainForm.UpdateColorDepthValues;
var
  ColorDepthTmp: TColorDepth;
  IsEmpty: Boolean;
  Idx: Integer;
begin
  {if ImageFormatComboBox.ItemIndex = 1 then
    Exit;}

  ColorDepthComboBox.Clear;
  ColorDepthComboBox.ItemIndex := -1;

  IsEmpty := ImageFormatInfoArray[ImageFormat].ColorDepth = [];

  if not IsEmpty then
  begin
    Idx := 0;
    //for ColorDepthTmp in TColorDepth do
    for ColorDepthTmp := Low(TColorDepth) to High(TColorDepth) do
    begin
      if ColorDepthTmp in ImageFormatInfoArray[ImageFormat].ColorDepth then
      begin
        ColorDepthComboBox.Items.AddObject(Format('%d bit', [Integer(ColorDepthTmp)]), TObject({Integer}PtrUint(ColorDepthTmp)));
        if ColorDepthTmp = FColorDepth then
        begin
          // Select last saved color depth if available
          ColorDepth := TColorDepth(PtrUint(ColorDepthComboBox.Items.Objects[Idx]));
        end;
        Inc(Idx);
      end;
    end;

    if (ColorDepthComboBox.ItemIndex = -1) and (FColorDepth <> TColorDepth(0)) then
    begin
      // Select best color depth (last one in the list)
      Idx := ColorDepthComboBox.Items.Count - 1;
      ColorDepth := TColorDepth(PtrUint(ColorDepthComboBox.Items.Objects[Idx]));
    end;
  end;

  ColorDepthLabel.Visible := not IsEmpty;
  ColorDepthComboBox.Visible := not IsEmpty;

  ColorDepthComboBox.OnChange(ColorDepthComboBox);
end;

procedure TMainForm.UpdateMonitorList;
begin
  // Update array in Screen variable first
  Screen.UpdateMonitors;

  // Disable choosing monitor if only one available
  if Screen.MonitorCount >= 2 then
  begin // Multiple monitors
    MonitorLabel.Enabled := True;
    MonitorComboBox.Enabled := True;

    // Fix out of bounds
    if MonitorId >= Screen.MonitorCount then
      //MonitorId := Screen.MonitorCount - 1; // Last
      MonitorId := NoMonitorId;
  end
  else
  begin // Only one monitor available
    MonitorLabel.Enabled := False;
    MonitorComboBox.Enabled := False;
    MonitorId := NoMonitorId;
    //MonitorId := 0;
  end;

  // Fill combobox
  FillMonitorList;
end;

function TMainForm.GetColorDepth: TColorDepth;
begin
  if ImageFormatInfoArray[ImageFormat].ColorDepth = [] then
    raise Exception.CreateFmt('%s format has no color depth option',
          [ImageFormatInfoArray[ImageFormat].Name])
  else
  begin
    if FColorDepth = TColorDepth(0) then
      raise Exception.Create('Color depth not initialized')
    else
      Result := FColorDepth;
  end;
end;

procedure TMainForm.SetColorDepth(AColorDepth: TColorDepth);
var
  Idx: Integer;
begin
  ColorDepthComboBox.ItemIndex := -1;

  if AColorDepth in ImageFormatInfoArray[ImageFormat].ColorDepth then
  begin
    // Choose new value in combobox
    for Idx := 0 to ColorDepthComboBox.Items.Count - 1 do
    begin
      if TColorDepth(PtrUint(ColorDepthComboBox.Items.Objects[Idx])) = AColorDepth then
      begin
        ColorDepthComboBox.ItemIndex := Idx;
        Break;
      end;
    end;

    FColorDepth := AColorDepth;
    Ini.WriteInteger(DefaultConfigIniSection, 'ColorDepth', Integer(AColorDepth));
    if Grabber <> nil then
      Grabber.ColorDepth := AColorDepth;
  end
  else
    raise Exception.CreateFmt('Color depth %d-bit not allowed for %s format',
      [integer(AColorDepth), ImageFormatInfoArray[ImageFormat].Name]);
end;

procedure TMainForm.SetTrayIconState(IconState: TTrayIconState);
var
  ResName: String;
begin
  if IconState <> tisFlashAnimation then
    FTrayIconState := IconState;
  
  case IconState of
    tisBlackWhite: ResName := '_CAMERA_BW';
    tisFlashAnimation:
      begin
        TrayIconIdx := Low(TrayIconIdx);
        TrayIconAnimationTimer.Enabled := True;
        ResName := Format('_CAMERA_FLASH_%d', [TrayIconIdx]);
      end
    //tisDefault:
    else ResName := '_CAMERA';
  end;

  TrayIcon.Icon.LoadFromResourceName(HInstance, ResName);
end;

procedure TMainForm.TrayIconAnimationTimerTimer(Sender: TObject);
var
  ResName: String;
begin
  if (TrayIconIdx < High(TrayIconIdx)) then
  begin
    Inc(TrayIconIdx);
    ResName := Format('_CAMERA_FLASH_%d', [TrayIconIdx]);
    TrayIcon.Icon.LoadFromResourceName(HInstance, ResName);
  end
  else
  begin
    TrayIconIdx := Low(TrayIconIdx);
    TrayIconAnimationTimer.Enabled := False; // Stop animation

    // Restore previous tray icon
    TrayIconState := FTrayIconState;
  end;
end;

procedure TMainForm.AutoRunCheckBoxClick(Sender: TObject);
begin
  AutoRun := AutoRunCheckBox.Checked;
end;

procedure TMainForm.MonitorComboBoxChange(Sender: TObject);
begin
  SetMonitorId(GetMonitorId);
end;

procedure TMainForm.FillMonitorList;
var
  Idx, SelIdx: Integer;
  Str: WideString;
  IsLocalizationLoaded: Boolean;

begin
  IsLocalizationLoaded := True;
  try
    Localizer.I18N('test...')
  except
    IsLocalizationLoaded := False;
  end;

  if not IsLocalizationLoaded then
    Exit;


  // Fill combobox with monitor list
  with MonitorComboBox do
  begin
    //SelId := MonitorId;
    SelIdx := MonitorComboBox.ItemIndex;

    Items.Clear;
    Items.Append(WideFormat(Localizer.I18N('AllMonitorsInfo'),
        [GetSystemMetrics(SM_CXVIRTUALSCREEN),
         GetSystemMetrics(SM_CYVIRTUALSCREEN)]
    ));

    Items.Append(Localizer.I18N('MonitorWithCursor'));

    for Idx := 0 to Screen.MonitorCount - 1 do
    begin
      Str := WideFormat(Localizer.I18N('MonitorInfo'),
          [Screen.Monitors[Idx].MonitorNum + 1, // Start numeration from 1
           Screen.Monitors[Idx].Width,
           Screen.Monitors[Idx].Height]
      );
      // ToDo: Also may show screen model, diagonal size
      if Screen.Monitors[Idx].Primary then
        Str := Str + ' - ' + Localizer.I18N('Primary');

      Items.Append(Str);
    end;

    // Restore previous selected item after strings updated
    //MonitorId := SelId;
    MonitorComboBox.ItemIndex := SelIdx;
  end;
end;

function TMainForm.GetMonitorId: Integer;
begin
  if MonitorComboBox.ItemIndex <= 0 then
    Result := NoMonitorId
  else
  begin
    if MonitorComboBox.ItemIndex = 1 then
      Result := MonitorWithCursor
    else
      Result := MonitorComboBox.ItemIndex - 2;
  end;
end;

procedure TMainForm.SetMonitorId(MonitorId: Integer);
begin
  if MonitorId = NoMonitorId then
    MonitorComboBox.ItemIndex := 0
  else if MonitorId = MonitorWithCursor then
    MonitorComboBox.ItemIndex := 1
  else if (MonitorId >= 0) and (MonitorId < {Screen.MonitorCount} MonitorComboBox.Items.Count) then
    MonitorComboBox.ItemIndex := MonitorId + 2
  else
    raise Exception.CreateFmt('Monitor id=%d not exists', [MonitorId]);

  if Ini <> Nil then
    Ini.WriteInteger(DefaultConfigIniSection, 'Monitor', MonitorId);
end;

procedure TMainForm.AboutMenuItemClick(Sender: TObject);
begin
  with TAboutForm.Create(Application) do
  begin
    ShowModal;
  end;
end;

procedure TMainForm.TrayIconDblClick(Sender: TObject);
begin
  RestoreFromTray;
end;

procedure TMainForm.UpdateLanguages;
{const
  GroupIdx = 1;}
var
  Lang: TLanguageInfo;
  MenuItem: TMenuItem;
  LangsList: TStringList;
  Line: String;
begin
  while LanguageSubMenu.Count > 0 do
    LanguageSubMenu.Items[0].Free;
  LanguageSubMenu.Clear;

  LangsList := TStringList.Create;

  Localizer.GetLanguages(AvailableLanguages);
  for Lang in AvailableLanguages do
  begin
    if (Lang.Name <> '') and (Lang.Code <> '') then
    begin
      if (Lang.NativeName <> '') and (Lang.NativeName <> Lang.Name) then
        Line := Format('%s (%s)', [Lang.Name, Lang.NativeName])
      else
        Line := Lang.Name;

      Line := Line + #9 + Lang.Code;

      LangsList.Append(Line);
    end;
  end;
  LangsList.Sort;

  for Line in LangsList do
  begin
    MenuItem := TMenuItem.Create(LanguageSubMenu);
    MenuItem.Caption := ExtractDelimited(1, Line, [#9]);
    MenuItem.OnClick := @LanguageClick;
    MenuItem.RadioItem := True;
    //MenuItem.GroupIndex := GroupIdx;
    MenuItem.Name := LanguageSubMenuItemNamePrefix + ExtractDelimited(2, Line, [#9]);

    LanguageSubMenu.Add(MenuItem);
  end;

  LangsList.Free;
end;


procedure TMainForm.LanguageClick(Sender: TObject);
var
  LangCode: TLanguageCode;
begin
  LangCode := GetLangCodeOfLangMenuItem(Sender as TMenuItem);
  SetLanguageByCode(LangCode);
end;

function TMainForm.FindLangMenuItem(ALangCode: TLanguageCode): TMenuItem;
var
  I: integer;
  LangCode: TLanguageCode;
  MenuItem: TMenuItem;
begin
  for I := 0 to LanguageSubMenu.Count - 1 do
  begin
    MenuItem := LanguageSubMenu.Items[I];
    LangCode := GetLangCodeOfLangMenuItem(MenuItem);
    if LangCode = ALangCode then
    begin
      Result := MenuItem;
      Exit;
    end;
  end;

  raise Exception.CreateFmt('Language code "%s" not found', [ALangCode]);
end;

procedure TMainForm.RecalculateLabelWidths;
var
  MaxWidth: Integer;
begin
  MaxWidth := 0;
  MaxWidth := max(MaxWidth, OutputDirLabel.Width);
  MaxWidth := max(MaxWidth, FileNameTemplateLabel.Width);
  MaxWidth := max(MaxWidth, CaptureIntervalLabel.Width);
  MaxWidth := max(MaxWidth, ImageFormatLabel.Width);
  MaxWidth := max(MaxWidth, MonitorLabel.Width);
  MaxWidth := max(MaxWidth, PostCmdLabel.Width);

  OutputDirEdit.Left := MaxWidth + ChildSizing.LeftRightSpacing
      + ChildSizing.HorizontalSpacing;

  // Sequential number group
  RecalculateLabelWidthsForSeqNumGroup;
end;

procedure TMainForm.RecalculateLabelWidthsForSeqNumGroup;
var
  MaxWidth: Integer;
begin
  MaxWidth := 0;
  MaxWidth := max(MaxWidth, SeqNumberValueLabel.Width);
  MaxWidth := max(MaxWidth, SeqNumberDigitsCountLabel.Width);
  SeqNumberValueSpinEdit.Left := MaxWidth
      + SeqNumberGroup.ChildSizing.LeftRightSpacing
      + SeqNumberGroup.ChildSizing.HorizontalSpacing;
end;

function TMainForm.GetLangCodeOfLangMenuItem(
  const LangItem: TMenuItem): TLanguageCode;
begin
  if Pos(LanguageSubMenuItemNamePrefix, LangItem.Name) = 1 then
    Result := Copy(LangItem.Name, Length(LanguageSubMenuItemNamePrefix) + 1, 2)
  else
    raise Exception.CreateFmt('Can`t get language code from language menu item' +
        ' "%s" (name=%s)', [LangItem.Caption, LangItem.Name]);
end;

function TMainForm.FormatPath(Str: string): string;
const
  TmplVarsChar = '%';
var
  CounterStr: String[MaxCounterDigits];
begin
  Result := Str;

  CounterStr := Dec2Numb(Counter, CounterDigits, 10); // Add leading zeros to Counter value

  Result := StringReplace(Result, TmplVarsChar + 'COMP', GetLocalComputerName, [rfReplaceAll]);
  Result := StringReplace(Result, TmplVarsChar + 'USER', GetCurrentUserName,   [rfReplaceAll]);
  Result := StringReplace(Result, TmplVarsChar + 'NUM',  CounterStr,           [rfReplaceAll]);

  // Date/time
  Result := FormatDateTime2(Result);
end;

procedure TMainForm.SetCounter(Val: Integer);
begin
  FCounter := Val;
  Ini.WriteInteger(DefaultConfigIniSection, 'Counter', FCounter);
  SeqNumberValueSpinEdit.Value := FCounter;
end;

procedure TMainForm.SeqNumberValueSpinEditChange(Sender: TObject);
begin
  if Ini = Nil then
    Exit;

  try
    Counter := SeqNumberValueSpinEdit.Value;
  finally
  end;
end;

procedure TMainForm.SetCounterDigits(Val: Integer);
begin
  FCounterDigits := Val;
  Ini.WriteInteger(DefaultConfigIniSection, 'CounterDigits', FCounterDigits);
  SeqNumberDigitsCountSpinEdit.Value := FCounterDigits;
end;

procedure TMainForm.UpdateSeqNumGroupVisibility;
begin
  SeqNumberGroup.Visible := Pos('%NUM', FileNameTemplateComboBox.Text) <> 0;
  if SeqNumberGroup.Visible then
    RecalculateLabelWidthsForSeqNumGroup;

  UpdateFormAutoSize;
end;

procedure TMainForm.SetJPEGQuality(Val: Integer);
begin
  JPEGQualitySpinEdit.Value := Val;
end;

function TMainForm.GetJPEGQuality: Integer;
begin
  Result := JPEGQualitySpinEdit.Value;
end;

procedure TMainForm.SetStopWhenInactive(const Val: Boolean);
begin
   if FStopWhenInactive <> Val then
   begin
     FStopWhenInactive := Val;
     StopWhenInactiveCheckBox.Checked := Val;
     Ini.WriteBool(DefaultConfigIniSection, 'StopWhenInactive', Val);
   end;
end;

procedure TMainForm.SetStartMinimized(const Val: Boolean);
begin
  if FStartMinimized <> Val then
  begin
    FStartMinimized := Val;
    StartMinimizedCheckBox.Checked := Val;
    Ini.WriteBool(DefaultConfigIniSection, 'StartMinimized', Val);
  end;
end;

procedure TMainForm.SetAutoRun(const Val: Boolean);
begin
  if FAutoRun <> Val then
  begin
    FAutoRun := Val;
    AutoRunCheckBox.Checked := Val;
    uUtils.AutoRun(Application.ExeName, 'Auto Screenshot', Val);
    Ini.WriteBool(DefaultConfigIniSection, 'AutoRun', Val);
  end;
end;

procedure TMainForm.SetGrayscale(const Val: Boolean);
begin
  if FGrayscale <> Val then
  begin
    FGrayscale := Val;
    GrayscaleCheckBox.Checked := Val;
    Ini.WriteBool(DefaultConfigIniSection, 'Grayscale', Val);
    if Grabber <> nil then
      Grabber.IsGrayscale := Val;
  end;
end;

procedure TMainForm.SetPostCommand(ACmd: String);
begin
  PostCmdEdit.Text := ACmd;
end;

function TMainForm.GetPostCommand: String;
begin
  Result := PostCmdEdit.Text;
end;

function TMainForm.GetMonitorWithCursor: Integer;
var
  MonitorRect: TRect;
begin
  Screen.UpdateMonitors;
  for Result := 0 to Screen.MonitorCount - 1 do
  begin
    with Screen.Monitors[Result] do
    begin
      MonitorRect.SetLocation(Left, Top);
      MonitorRect.Width:=Width;
      MonitorRect.Height:=Height;
    end;

    if MonitorRect.Contains(Mouse.CursorPos) then
      Exit;
  end;

  Exit(NoMonitorId);
end;

procedure TMainForm.CheckForUpdates(AShowMessageWhenNoUpdates: Boolean);
const
  ApiUrl =
{$IFOPT D+}
    'https://eojiuvjshd8kbzt.m.pipedream.net'
{$ELSE}
    'https://api.github.com/repos/artem78/AutoScreenshot/releases/latest'
{$ENDIF}
    ;
var
  ResponseStr: String;
  Client: TFPHTTPClient;
  JsonData: TJSONData;
  NewVersion, CurrentVersion: TProgramVersion;
  DownloadUrl, ChangeLog: String;
  Msg: {TStringBuilder} TAnsiStringBuilder;

begin
  CurrentVersion := TProgramVersion.Create(GetProgramVersionStr());

  Ini.WriteDateTime(DefaultConfigIniSection, 'LastCheckForUpdates', Now);

  Client := TFPHTTPClient.Create(Nil);
  try
    try
      Client.AllowRedirect := True;
      Client.AddHeader('Accept', 'application/vnd.github.v3+json');
      Client.AddHeader('User-Agent', Application.Title + ' v' + CurrentVersion.ToString() + ' Update Checker');
      ResponseStr := Client.Get(ApiUrl);

      JsonData := GetJSON(ResponseStr);
      try
        NewVersion := TProgramVersion.Create(JsonData.GetPath('tag_name').AsString);
        DownloadUrl := JsonData.GetPath('html_url').AsString;
        ChangeLog := JsonData.GetPath('body').AsString;

        if NewVersion > CurrentVersion then
        begin
          Msg := TAnsiStringBuilder.Create();
          try
            Msg.AppendFormat(Localizer.I18N('UpdateFound'), [NewVersion.ToString(True), CurrentVersion.ToString(True)]);
            Msg.AppendLine('');
            Msg.AppendLine('');
            Msg.AppendLine(ChangeLog);
            Msg.AppendLine('');
            Msg.AppendLine(Localizer.I18N('AskDownloadUpdate'));
            //if MessageDlg(Msg.ToString, mtInformation, mbYesNo, 0) = mrYes then
            if QuestionDlg('', Msg.ToString, {mtCustom} mtInformation,
                   [mrYes, Localizer.I18N('Yes'), mrNo, Localizer.I18N('No')], '') = mrYes then
              OpenURL(DownloadUrl);
          finally
            Msg.Free
          end;
        end
        else
        begin
          if AShowMessageWhenNoUpdates then
            ShowMessage(Localizer.I18N('NoUpdatesFound'));
        end;
      finally
        JsonData.Free;
      end;

    except on E: Exception do
      MessageDlg('', Localizer.I18N('UpdateCheckFailed') + LineEnding
                 + LineEnding + E.Message, mtError, [mbOK], 0);
    end;
  finally
    Client.Free;
  end;
end;

function TMainForm.GetAutoCheckForUpdates: Boolean;
begin
  Result := AutoCheckForUpdatesMenuItem.Checked;
end;

procedure TMainForm.SetAutoCheckForUpdates(AVal: Boolean);
begin
  Ini.WriteBool(DefaultConfigIniSection, 'AutoCheckForUpdates', AVal);
  AutoCheckForUpdatesMenuItem.Checked := AVal;
end;

procedure TMainForm.SetStartAutoCaptureHotKey(AHotKey: THotKey);
begin
  KeyHook.RegisterKey('StartAutoCapture', AHotKey);
  Ini.WriteHotKey(HotKeysIniSection, 'StartAutoCapture', AHotKey);
end;

procedure TMainForm.SetStopAutoCaptureHotKey(AHotKey: THotKey);
begin
  KeyHook.RegisterKey('StopAutoCapture', AHotKey);
  Ini.WriteHotKey(HotKeysIniSection, 'StopAutoCapture', AHotKey);
end;

procedure TMainForm.SetSingleCaptureHotKey(AHotKey: THotKey);
begin
  KeyHook.RegisterKey('SingleCapture', AHotKey);
  Ini.WriteHotKey(HotKeysIniSection, 'SingleCapture', AHotKey);
end;

procedure TMainForm.SetCompressionLevel(ALevel: Tcompressionlevel);
begin
  CompressionLevelComboBox.ItemIndex := Ord(ALevel);
  Ini.WriteInteger(DefaultConfigIniSection, 'Compression', Ord(ALevel));
  if Grabber <> nil then
    Grabber.CompressionLevel := CompressionLevel;
end;

function TMainForm.GetCompressionLevel: Tcompressionlevel;
begin
  Result := Tcompressionlevel(CompressionLevelComboBox.ItemIndex);
end;

procedure TMainForm.UpdateFormAutoSize;
begin
  {$IfDef Linux}
  // Bugfix for Linux only
  // https://forum.lazarus.freepascal.org/index.php/topic,62600.0.html
  // ToDo: Try to find better solution
  AutoSize := not AutoSize;
  AutoSize := not AutoSize;
  {$EndIf}
end;

procedure TMainForm.OnScreenConfigurationChanged(const AEvent: TXEvent);
begin
  //if AEvent._type = 89 {?} then
    UpdateMonitorList;
  //end;
end;

{$IfDef Windows}
procedure TMainForm.WMHotKey(var AMsg: TMessage);
var
  StrId: String = '';
begin
  //ShowMessage(IntToStr(lParam));

  try
    StrId := KeyHook.IdToStrId(AMsg.wParam);
  except
  end;

  case StrId of
    'StartAutoCapture': IsTimerEnabled := True;
    'StopAutoCapture':  IsTimerEnabled := False;
    'SingleCapture':    MakeScreenshot;
    {$IFOPT D+}
    else ShowMessage(Format('Unknown hotkey event! (wparam=%d, lparam=%d)', [AMsg.wParam, AMsg.lParam]));
    {$ENDIF}
  end;
end;
{$EndIf}

procedure TMainForm.SeqNumberDigitsCountSpinEditChange(Sender: TObject);
begin
  if Ini = Nil then
    Exit;

  try
    CounterDigits := SeqNumberDigitsCountSpinEdit.Value;
  finally
  end;
end;

end.
