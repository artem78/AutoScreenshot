unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  {$IfDef Windows}
  Windows,
  {$EndIf}
  {$IfDef Linux}
  xlib, xrandr, XRandREventWatcher,
  {$EndIf}
  {Messages,} SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, {ComCtrls,} ExtCtrls, StdCtrls, inifiles, Spin, {FileCtrl,}
  Menus, Buttons, EditBtn, uLocalization, DateTimePicker, LCLIntf,
  ScreenGrabber, uHotKeysForm, uUtilsMore, GlobalKeyHook, OldScreenshotCleaner,
  UniqueInstance, uplaysound, ZStream { for Tcompressionlevel };

type
  TTrayIconState = (tisDefault, tisBlackWhite, tisFlashAnimation);

  { TMainForm }

  TMainForm = class(TForm)
    AutoCheckForUpdatesMenuItem: TMenuItem;
    FileMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    LangFlagImageList: TImageList;
    MinimizeInsteadOfCloseCheckBox: TCheckBox;
    PlaySoundsCheckBox: TCheckBox;
    CompressionLevelComboBox: TComboBox;
    OldScreenshotCleanerEnabledCheckBox: TCheckBox;
    HotKetsSettingsMenuItem: TMenuItem;
    CompressionLevelLabel: TLabel;
    ImageFormatOptionsPanel: TPanel;
    DonateMenuItem: TMenuItem;
    OldScreenshotCleanerPanel: TPanel;
    OldScreenshotCleanerMaxAgeUnitComboBox: TComboBox;
    OldScreenshotCleanerMaxAgeValueSpinEdit: TSpinEdit;
    SoundPlayer: Tplaysound;
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
    SeqNumberGroup: TGroupBox;
    SeqNumberValueLabel: TLabel;
    SeqNumberValueSpinEdit: TSpinEdit;
    SeqNumberDigitsCountSpinEdit: TSpinEdit;
    SeqNumberDigitsCountLabel: TLabel;
    UniqueInstance1: TUniqueInstance;
    procedure CheckForUpdatesMenuItemClick(Sender: TObject);
    procedure AutoCheckForUpdatesMenuItemClick(Sender: TObject);
    procedure CompressionLevelComboBoxChange(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MinimizeInsteadOfCloseCheckBoxChange(Sender: TObject);
    procedure OldScreenshotCleanerEnabledCheckBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HotKetsSettingsMenuItemClick(Sender: TObject);
    procedure DonateMenuItemClick(Sender: TObject);
    procedure OldScreenshotCleanerMaxAgeUnitComboBoxChange(Sender: TObject);
    procedure OldScreenshotCleanerMaxAgeValueSpinEditChange(Sender: TObject);
    procedure OutputDirEditChange(Sender: TObject);
    procedure CaptureIntervalDateTimePickerChange(Sender: TObject);
    procedure PlaySoundsCheckBoxChange(Sender: TObject);
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
    procedure UniqueInstance1OtherInstance(Sender: TObject;
      ParamCount: Integer; const Parameters: array of String);
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

    {$IfDef Windows}
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
    OldScreenshotCleaner: TOldScreenshotCleaner;
    FormInitialized: Boolean;

    public
    FileJournal: TFileJournal;
    private
    
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
    function GetAutoCheckForUpdates: Boolean;
    procedure SetAutoCheckForUpdates(AVal: Boolean);
    procedure SetStartAutoCaptureHotKey(AHotKey: THotKey);
    procedure SetStopAutoCaptureHotKey(AHotKey: THotKey);
    procedure SetSingleCaptureHotKey(AHotKey: THotKey);
    procedure SetHotKey(AHotKeyId: String; AHotKey: THotKey);
    procedure SetCompressionLevel(ALevel: Tcompressionlevel);
    function GetCompressionLevel: Tcompressionlevel;
    procedure UpdateFormAutoSize;
    procedure PlaySound(const AFileName: String);
    procedure SetSounds(AEnabled: Boolean);
    function GetSounds: Boolean;
    procedure SetMinimizeInsteadOfClose(AEnabled: Boolean);
    function GetMinimizeInsteadOfClose: Boolean;
    function ConfirmExit: Boolean;

    procedure OnHotKeyEvent(const AHotKeyId: String);
    procedure OnDebugLnEvent(Sender: TObject; S: string; var Handled: Boolean);
    function OnHotKeysSaving(ASender: TObject; out AErrorMsg: string): Boolean;
    procedure OnScreenshotCleanerChanged;

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
    property Sounds: Boolean read GetSounds write SetSounds;
    property MinimizeInsteadOfClose: Boolean read GetMinimizeInsteadOfClose write SetMinimizeInsteadOfClose;

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
  MinOldScreenshotsRemovingPeriodValue = 1;
  MaxOldScreenshotsRemovingPeriodValue = 999;

var
  MainForm: TMainForm;
  Ini: TIniFile;

implementation

uses uAbout, DateUtils, StrUtils, uUtils, Math,
  uFileNameTemplateHelpForm, uIniHelper, UpdateChecker, FileUtil, LCLType, Idle,
  uDonateForm, LazLogger;

{$R *.lfm}

const
  LanguageSubMenuItemNamePrefix = 'LanguageSubMenuItem_';

{$IfDef Windows}
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
  I: Integer;
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
    Append('%Y-%M' + PathDelim + '%D' + PathDelim + 'screenshot %H-%N-%S');
    Append('%COMP' + PathDelim + '%USER' + PathDelim + 'screenshot %Y-%M-%D %H-%N-%S');
    Append('screenshot %NUM');
  end;

  with OldScreenshotCleanerMaxAgeValueSpinEdit do
  begin
    MinValue := MinOldScreenshotsRemovingPeriodValue;
    MaxValue := MaxOldScreenshotsRemovingPeriodValue;
  end;

  with OldScreenshotCleanerMaxAgeUnitComboBox.Items do
  begin
    Clear;
    for I := Ord(Low(TIntervalUnit)) to Ord(High(TIntervalUnit)) do
      Append('');
  end;

end;

procedure TMainForm.ReadSettings;
const
  DefaultFileNameTemplate = '%Y-%M-%D' + PathDelim + '%Y-%M-%D %H.%N.%S';
  DefaultCaptureInterval  = 5;
  DefaultImageFormat      = fmtPNG;
  DefaultJPEGQuality      = 80;
  DefaultLanguage         = 'en';
  DefaultColorDepth       = cd24Bit;
  DefaultMonitorId        = NoMonitorId;
  DefaultCounterValue     = MinCounterValue;
  DefaultCounterDigits    = 6;
  DefaultCompressionLevel = cldefault;
  DefaultScreenshotCleanerMaxAge: TInterval = (
    Val: 1;
    Unit_: iuMonths
  );
  
  LogFileName = 'log.txt';
var
  DefaultOutputDir, BaseDir: String;
  CfgLang, SysLang, AltLang: TLanguageCode;
  FmtStr: String;
  Seconds: Integer;
  LogFilePath: String;
  CleanerActive: Boolean;
begin
  // Logging
  if Ini.ReadBool(DefaultConfigIniSection, 'Logging', False) then
  begin
    if IsPortable then
      LogFilePath := ConcatPaths([ProgramDirectory, LogFileName])
    else
      LogFilePath := ConcatPaths([GetAppConfigDir(False), LogFileName]);
    DeleteFile(LogFilePath); // Overwrite log file
    DebugLogger.LogName := LogFilePath;
    //{$Define LAZLOGGER_FLUSH}
    DebugLogger.CloseLogFileBetweenWrites := True; // FixMe: Better to set LAZLOGGER_FLUSH, but seems it doesn't work
    DebugLogger.OnDebugLn := @OnDebugLnEvent;
  end
  else
  begin
    {$IFOPT D-}
    DebugLogger.LogName :=
                 {$IfDef Windows}'nul'{$EndIf}
                 {$IfDef Linux}'/dev/null'{$ENDIF}
    ;
    {$EndIf}
  end;

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

  // Old screenshots removing
  CleanerActive := Ini.ReadBool(DefaultConfigIniSection,
                                'OldScreenshotCleanerEnabled', False);
  OldScreenshotCleaner.MaxAge := TInterval(Ini.ReadString(DefaultConfigIniSection,
                                             'OldScreenshotCleanerMaxAge',
                                             String(DefaultScreenshotCleanerMaxAge)));
  OldScreenshotCleaner.Active := CleanerActive;

  // Sounds
  Sounds := Ini.ReadBool(DefaultConfigIniSection, 'Sounds', False);

  // Minimize instead of close
  MinimizeInsteadOfClose := Ini.ReadBool(DefaultConfigIniSection, 'MinimizeInsteadOfClose', False);
end;

procedure TMainForm.FormCreate(Sender: TObject);
const
  NoHotKey: THotKey = (
    ShiftState: [];
    Key: VK_UNKNOWN;
  );
var
  ///////
  ColorDepthTmp: TColorDepth;
  ////////
  LastUpdateCheck: TDateTime;
  HotKey: THotKey;
  IniFileName: String;
begin
  {DebugLn('Program started');
  DebugLn('Version: ', GetProgramVersionStr);
  DebugLn('Initializing...');}

  {$IfDef Windows}
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
  Ini.WriteString(DefaultConfigIniSection, 'ProgramVersion', GetProgramVersionStr);

  OldScreenshotCleaner := TOldScreenshotCleaner.Create;
  OldScreenshotCleaner.OnChangeCallback := @OnScreenshotCleanerChanged;

  ReadSettings;

  DebugLn('Program started at ', DateTimeToStr(Now));
  DebugLn('Version: ', GetProgramVersionStr);
  DebugLn('Initializing...');

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
  if AutoCheckForUpdates then
  begin
    DebugLn('Last update check: %s (%d hours ago)', [DateTimeToStr(LastUpdateCheck), HoursBetween(Now, LastUpdateCheck)]);
  end;
  if AutoCheckForUpdates and (SecondsBetween(Now, LastUpdateCheck) > UpdateCheckIntervalInSeconds) then
    CheckForUpdates(True);

  // Enable global hotkeys
  KeyHook := TGlobalKeyHook.Create({$IfDef Windows}Handle, 'AutoScreenshot'{$EndIf}
                                   {$IfDef Linux}@OnHotKeyEvent{$EndIf});
  HotKey := Ini.ReadHotKey(HotKeysIniSection, 'StartAutoCapture', NoHotKey);
  try
    KeyHook.RegisterKey('StartAutoCapture', HotKey);
  except
    KeyHook.RegisterKey('StartAutoCapture', NoHotKey);
  end;
  HotKey := Ini.ReadHotKey(HotKeysIniSection, 'StopAutoCapture', NoHotKey);
  try
    KeyHook.RegisterKey('StopAutoCapture', HotKey);
  except
    KeyHook.RegisterKey('StopAutoCapture', NoHotKey);
  end;
  HotKey := Ini.ReadHotKey(HotKeysIniSection, 'SingleCapture', NoHotKey);
  try
    KeyHook.RegisterKey('SingleCapture', HotKey);
  except
    KeyHook.RegisterKey('SingleCapture', NoHotKey);
  end;

  {$IfDef Linux}
  // Enable monitor confuguration changed updates in Linux
  XWatcher := TXRandREventWatcherThread.Create(RRScreenChangeNotifyMask, @OnScreenConfigurationChanged);
  {$EndIf}

  FileJournal := TFileJournal.Create;

  FormInitialized := True;
  DebugLn('Initializing finished');
end;

procedure TMainForm.CheckForUpdatesMenuItemClick(Sender: TObject);
begin
  CheckForUpdates(False);
end;

procedure TMainForm.AutoCheckForUpdatesMenuItemClick(Sender: TObject);
begin
  AutoCheckForUpdates := not AutoCheckForUpdates;
end;

procedure TMainForm.CompressionLevelComboBoxChange(Sender: TObject);
begin
  CompressionLevel := Tcompressionlevel(CompressionLevelComboBox.ItemIndex);
end;

procedure TMainForm.ExitMenuItemClick(Sender: TObject);
begin
  if ConfirmExit then
    //Close;
    Application.Terminate;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not MinimizeInsteadOfClose;
  if MinimizeInsteadOfClose then
    MinimizeToTray;

  if CanClose then
    CanClose := ConfirmExit;
end;

procedure TMainForm.MinimizeInsteadOfCloseCheckBoxChange(Sender: TObject);
begin
  MinimizeInsteadOfClose := MinimizeInsteadOfClose;
end;

procedure TMainForm.OldScreenshotCleanerEnabledCheckBoxChange(Sender: TObject);
begin
  OldScreenshotCleaner.Active := TCheckBox(Sender).Checked;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FileJournal.Free;

  {$IfDef Linux}
  //XWatcher.Terminate;
  //XWatcher.WaitFor;
  XWatcher.Free;
  {$EndIf}

  Grabber.Free;
  KeyHook.Free;
  OldScreenshotCleaner.Free;
  Ini.Free;

  DebugLn('Program ended');
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  RecalculateLabelWidths;
end;

procedure TMainForm.HotKetsSettingsMenuItemClick(Sender: TObject);
var
  HotKeysForm: THotKeysForm;
begin
  // ToDo: Reduce amount of code duplicates

  HotKeysForm := THotKeysForm.Create(Nil, @OnHotKeysSaving);
  HotKeysForm.StartAutoCaptureKey := Self.KeyHook.FindHotKey('StartAutoCapture');
  HotKeysForm.StopAutoCaptureKey := Self.KeyHook.FindHotKey('StopAutoCapture');
  HotKeysForm.SingleCaptureKey := Self.KeyHook.FindHotKey('SingleCapture');
  HotKeysForm.ShowModal;
  HotKeysForm.Free;
end;

procedure TMainForm.DonateMenuItemClick(Sender: TObject);
begin
  with TDonateForm.Create(Self) do
  begin
    try
      ShowModal;
    finally
      Free;
    end;
  end;
end;

procedure TMainForm.OldScreenshotCleanerMaxAgeUnitComboBoxChange(
  Sender: TObject);
var
  Interval: TInterval;
begin
  Interval := OldScreenshotCleaner.MaxAge;
  Interval.Unit_:= TIntervalUnit(TComboBox(Sender).ItemIndex);
  OldScreenshotCleaner.MaxAge := Interval;
end;

procedure TMainForm.OldScreenshotCleanerMaxAgeValueSpinEditChange(
  Sender: TObject);
var
  Interval: TInterval;
begin
  Interval := OldScreenshotCleaner.MaxAge;
  Interval.Val := TSpinEdit(Sender).Value;
  OldScreenshotCleaner.MaxAge := Interval;
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

procedure TMainForm.PlaySoundsCheckBoxChange(Sender: TObject);
begin
  Sounds := Sounds;
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
      MakeScreenshot
    else
      DebugLn('Automatic capture skipped (Timer.Interval=%d, UserIdleTime=%d)',
          [Timer.Interval, UserIdleTime]);
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

  // Play sound
  if FormInitialized or AEnabled then // Prevent to play "stop" sound immediately after program starts
  begin
    if AEnabled then
      PlaySound('start.wav')
    else
      PlaySound('stop.wav');
  end;

  if AEnabled then
    DebugLn('Automatic capture started')
  else
    DebugLn('Automatic capture stopped');
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
  Cmd, ImageFileName, ErrMsg: String;
begin
  ImageFileName := ImagePath; // Use local variable because ImagePath() result
                              // may be changed on next call

  PlaySound('camera_shutter.wav');
  TrayIconState := tisFlashAnimation;

  if MonitorId = NoMonitorId then
    Grabber.CaptureAllMonitors(ImageFileName)
  else
  begin
    if MonitorId = MonitorWithCursor then
      Grabber.CaptureMonitor(ImageFileName, GetMonitorWithCursor)
    else
      Grabber.CaptureMonitor(ImageFileName, MonitorId);
  end;

  FileJournal.Add(ImageFileName);


  // Run user command
  try
    Cmd := PostCommand;
    if Cmd <> '' then
    begin
      Cmd := StringReplace(Cmd, '%FILENAME%', ImageFileName, [rfReplaceAll{, rfIgnoreCase}]);
      DebugLn('Execute command: ', Cmd);
      RunCmdInbackground(Cmd);
      //DebugLn('Execution success!'); // Not works
    end;
  except
    on E: Exception do
    begin
      DebugLn('Execution failed: ', E.ToString);

      if not Timer.Enabled then // Manual capture
      begin
        ErrMsg := {'Execution of custom command failed: ' +} E.Message;
        MessageDlg('Auto Screenshot', ErrMsg, mtWarning, [mbOK], '');
      end;
    end;
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
  if ConfirmExit then
    //Close;
    Application.Terminate;
end;

procedure TMainForm.MinimizeToTray;
begin
  {TrayIcon.AppVisible := False;
  TrayIcon.FormVisible := False;
  TrayIcon.IconVisible := True; }

  {Application.MainFormOnTaskBar := False;
  Application.ShowMainForm := False;
  //Hide;}
  Hide;
  WindowState := wsMinimized;
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
const
  {$IfDef Windows}
  CmdExample = 'copy "%FILENAME%" "C:\dir\"';
  {$EndIf}
  {$IfDef Linux}
  CmdExample = 'cp "%FILENAME%" "~/dir/"';
  {$EndIf}
begin
  DisableAutoSizing;

  try
    // Menubar
    OptionsSubMenu.Caption := Localizer.I18N('Options');
    LanguageSubMenu.Caption := Localizer.I18N('Language');
    if LanguageSubMenu.Caption <> 'Language' then
      LanguageSubMenu.Caption := LanguageSubMenu.Caption + ' (Language)';
    HelpSubMenu.Caption := Localizer.I18N('Help');
    AboutMenuItem.Caption := Localizer.I18N('About') + '...';
    CheckForUpdatesMenuItem.Caption := Localizer.I18N('CheckForUpdates');
    AutoCheckForUpdatesMenuItem.Caption := Localizer.I18N('AutoCheckForUpdates');
    HotKetsSettingsMenuItem.Caption := Localizer.I18N('EditHotKeys') + '...';
    DonateMenuItem.Caption := Localizer.I18N('Donate');
    ExitMenuItem.Caption := Localizer.I18N('Exit');
    FileMenuItem.Caption := Localizer.I18N('File');

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
    PostCmdEdit.Hint := StringReplace(Localizer.I18N('RunCommandHelpText'),
                                      '%s', CmdExample, []);

    CompressionLevelLabel.Caption := Localizer.I18N('CompressionLevel') + ':';
    with CompressionLevelComboBox do
    begin
      Items[0] := Localizer.I18N('CompressionLevelNone');
      Items[1] := Localizer.I18N('CompressionLevelFastest');
      Items[2] := Localizer.I18N('CompressionLevelDefault');
      Items[3] := Localizer.I18N('CompressionLevelMax');
    end;

    OldScreenshotCleanerEnabledCheckBox.Caption := Localizer.I18N('DeleteScreenshotsOlderThan');
    with OldScreenshotCleanerMaxAgeUnitComboBox do
    begin
      Items[Ord(iuHours)]  := Localizer.I18N('Hours');
      Items[Ord(iuDays)]   := Localizer.I18N('Days');
      Items[Ord(iuWeeks)]  := Localizer.I18N('Weeks');
      Items[Ord(iuMonths)] := Localizer.I18N('Months');
    end;

    PlaySoundsCheckBox.Caption := Localizer.I18N('PlaySounds');
    MinimizeInsteadOfCloseCheckBox.Caption := Localizer.I18N('MinimizeInSteadOfClose');

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
var
  Idx: Integer;
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

  DebugLn('Monitor configuration changed');
  DebugLn(['Monitors count: ', Screen.MonitorCount]);
  for Idx := 0 to Screen.MonitorCount - 1 do
  begin
    DebugLnEnter('Monitor id=%d (%d)%s %dx%d dpi=%d',
          [Screen.Monitors[Idx].MonitorNum,
           Screen.Monitors[Idx].MonitorNum + 1,
           IfThen(Screen.Monitors[Idx].Primary, ' primary'),
           Screen.Monitors[Idx].Width,
           Screen.Monitors[Idx].Height,
           Screen.Monitors[Idx].PixelsPerInch]
    );
    DebugLnEnter('BoundsRect: ', DbgS(Screen.Monitors[Idx].BoundsRect));
    DebugLnExit('WorkareaRect: ', DbgS(Screen.Monitors[Idx].WorkareaRect));
    DebugLnExit();
  end;
  DebugLn(['SM_CXVIRTUALSCREEN=', GetSystemMetrics(SM_CXVIRTUALSCREEN)]);
  DebugLn(['SM_CYVIRTUALSCREEN=', GetSystemMetrics(SM_CYVIRTUALSCREEN)]);
  DebugLn(['SM_XVIRTUALSCREEN=', GetSystemMetrics(SM_XVIRTUALSCREEN)]);
  DebugLn(['SM_YVIRTUALSCREEN=', GetSystemMetrics(SM_YVIRTUALSCREEN)]);
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
  FlagResourceName: String;
begin
  while LanguageSubMenu.Count > 0 do
    LanguageSubMenu.Items[0].Free;
  LanguageSubMenu.Clear;
  LangFlagImageList.Clear;

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

    // Flag icon
    FlagResourceName := 'FLAG_' + UpperCase(ExtractDelimited(2, Line, [#9]));
    if system.FindResource(HINSTANCE, FlagResourceName, RT_RCDATA) <> 0 then
      MenuItem.ImageIndex := LangFlagImageList.AddResourceName(HINSTANCE, FlagResourceName)
    else
      MenuItem.ImageIndex := -1;

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
  SetHotKey('StartAutoCapture', AHotKey);
end;

procedure TMainForm.SetStopAutoCaptureHotKey(AHotKey: THotKey);
begin
  SetHotKey('StopAutoCapture', AHotKey);
end;

procedure TMainForm.SetSingleCaptureHotKey(AHotKey: THotKey);
begin
  SetHotKey('SingleCapture', AHotKey);
end;

procedure TMainForm.SetHotKey(AHotKeyId: String; AHotKey: THotKey);
begin
  KeyHook.RegisterKey(AHotKeyId, AHotKey);
  Ini.WriteHotKey(HotKeysIniSection, AHotKeyId, AHotKey);
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
  //{$IfDef Linux}
  {$IfDef LCLGTK2}
  // Bugfix for Linux only
  // https://forum.lazarus.freepascal.org/index.php/topic,62600.0.html
  // ToDo: Try to find better solution
  AutoSize := not AutoSize;
  AutoSize := not AutoSize;
  {$EndIf}
end;

procedure TMainForm.PlaySound(const AFileName: String);
var
  SoundDir: String;
begin
  if not Sounds then
    Exit;

  with SoundPlayer do
  begin
    {$IfDef Windows}
    SoundDir := ConcatPaths([ProgramDirectory, 'sounds']);
    {$EndIf}
    {$IfDef Linux}
    if IsPortable then
      SoundDir := ConcatPaths([ProgramDirectory, 'sounds'])
    else
      SoundDir := '/usr/share/autoscreenshot/sounds/';
    {$EndIf}

    SoundFile := ConcatPaths([SoundDir, AFileName]);
    Execute;
  end;
end;

procedure TMainForm.SetSounds(AEnabled: Boolean);
begin
  PlaySoundsCheckBox.Checked := AEnabled;
  Ini.WriteBool(DefaultConfigIniSection, 'Sounds', AEnabled);
end;

function TMainForm.GetSounds: Boolean;
begin
  Result := PlaySoundsCheckBox.Checked;
end;

procedure TMainForm.SetMinimizeInsteadOfClose(AEnabled: Boolean);
begin
  MinimizeInsteadOfCloseCheckBox.Checked := AEnabled;
  Ini.WriteBool(DefaultConfigIniSection, 'MinimizeInsteadOfClose', AEnabled);
end;

function TMainForm.GetMinimizeInsteadOfClose: Boolean;
begin
  Result := MinimizeInsteadOfCloseCheckBox.Checked;
end;

function TMainForm.ConfirmExit: Boolean;
var
  YesStr, NoStr: String;
begin
  YesStr := Localizer.I18N('Yes');
  NoStr  := Localizer.I18N('No');
  Result := QuestionDlg('Auto Screenshot', Localizer.I18N('ExitConfirmation'), mtConfirmation,
            [mrYes, YesStr, mrNo, NoStr, 'IsDefault'],
            0) = mrYes;
end;

procedure TMainForm.OnHotKeyEvent(const AHotKeyId: String);
begin
  case AHotKeyId of
    'StartAutoCapture': IsTimerEnabled := True;
    'StopAutoCapture':  IsTimerEnabled := False;
    'SingleCapture':    MakeScreenshot;
    (*{$IFOPT D+}
    else ShowMessage(Format('Unknown hotkey event! (wparam=%d, lparam=%d)', [AMsg.wParam, AMsg.lParam]));
    {$ENDIF}*)
  end;
end;

procedure TMainForm.OnDebugLnEvent(Sender: TObject; S: string;
  var Handled: Boolean);
var
  Callback: {TLazLoggerWriteEvent} procedure(Sender: TObject; S: string; var Handled: Boolean) of object;
  IndentLevel: Integer;
begin
  S := Format('[%s]   %s', [FormatDateTime('hh:nn:ss.zzz', Now()), S]);
  Callback := DebugLogger.OnDebugLn;
  IndentLevel := DebugLogger.NestLvlIndent;
  DebugLogger.OnDebugLn := Nil;
  DebugLogger.NestLvlIndent := 0;
  try
    DebugLn(S);
  finally
    DebugLogger.NestLvlIndent := IndentLevel;
    DebugLogger.OnDebugLn := Callback;
  end;
  Handled := True;
end;

function TMainForm.OnHotKeysSaving(ASender: TObject; out AErrorMsg: string): Boolean;
var
  HasErrors: Boolean = False;
  HotKeysForm: THotKeysForm;
begin
  HotKeysForm := THotKeysForm(ASender);

  try
    SetStartAutoCaptureHotKey(HotKeysForm.StartAutoCaptureKey);
    HotKeysForm.StartAutoCaptureMarked := False;
  except
    HasErrors := True;
    HotKeysForm.StartAutoCaptureMarked := True;
  end;

  try
    SetStopAutoCaptureHotKey(HotKeysForm.StopAutoCaptureKey);
    HotKeysForm.StopAutoCaptureMarked := False;
  except
    HasErrors := True;
    HotKeysForm.StopAutoCaptureMarked := True;
  end;

  try
    SetSingleCaptureHotKey(HotKeysForm.SingleCaptureKey);
    HotKeysForm.SingleCaptureMarked := False;
  except
    HasErrors := True;
    HotKeysForm.SingleCaptureMarked := True;
  end;

  if HasErrors then
    AErrorMsg := Localizer.I18N('HotKeyOccupied')
  else
    AErrorMsg := '';

  Result := not HasErrors;
end;

procedure TMainForm.OnScreenshotCleanerChanged;
begin
  OldScreenshotCleanerEnabledCheckBox.Checked := OldScreenshotCleaner.Active;
  Ini.WriteBool(DefaultConfigIniSection, 'OldScreenshotCleanerEnabled', OldScreenshotCleaner.Active);

  OldScreenshotCleanerMaxAgeValueSpinEdit.Value := OldScreenshotCleaner.MaxAge.Val;
  OldScreenshotCleanerMaxAgeUnitComboBox.ItemIndex := Ord(OldScreenshotCleaner.MaxAge.Unit_);
  Ini.WriteString(DefaultConfigIniSection, 'OldScreenshotCleanerMaxAge', String(OldScreenshotCleaner.MaxAge));

  OldScreenshotCleanerMaxAgeValueSpinEdit.Enabled := OldScreenshotCleaner.Active;
  OldScreenshotCleanerMaxAgeUnitComboBox.Enabled := OldScreenshotCleaner.Active;
end;

{$IfDef Linux}
procedure TMainForm.OnScreenConfigurationChanged(const AEvent: TXEvent);
begin
  //if AEvent._type = 89 {?} then
    UpdateMonitorList;
  //end;
end;
{$EndIf}

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

  OnHotKeyEvent(StrId);
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

procedure TMainForm.UniqueInstance1OtherInstance(Sender: TObject;
  ParamCount: Integer; const Parameters: array of String);
begin
  RestoreFromTray;
end;

end.
