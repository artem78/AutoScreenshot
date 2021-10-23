unit uAutoScreen;

interface

uses
  Windows, {Messages,} SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, {ComCtrls,} ExtCtrls, StdCtrls, inifiles, Spin, {FileCtrl,}
  Menus, Buttons, EditBtn, UniqueInstance, uLocalization, DateTimePicker,
  LCLIntf, ScreenGrabber;

type
  TTrayIconState = (tisDefault, tisBlackWhite, tisFlashAnimation);

  { TMainForm }

  TMainForm = class(TForm)
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
    CaptureInterval: TDateTimePicker;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OutputDirEditChange(Sender: TObject);
    procedure CaptureIntervalChange(Sender: TObject);
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

    PrevWndProc: WndProc;
    Grabber: TScreenGrabber;

    FStopWhenInactive: Boolean;
    FStartMinimized: Boolean;
    
    { Methods }
    procedure SetTimerEnabled(IsEnabled: Boolean);
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
    procedure TranslateForm();
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
  public
    { Public declarations }
  end;

const
  DefaultConfigIniSection = 'main';

  MinCaptureIntervalInSeconds = 1;
  NoMonitorId = -1;
  MinCounterValue  = 1;
  MinCounterDigits = 1;
  MaxCounterDigits = 10;

var
  MainForm: TMainForm;
  Ini: TIniFile;

implementation

uses uAbout, DateUtils, uUtils, Math, uFileNameTemplateHelpForm;

{$R *.lfm}

const
  LanguageSubMenuItemNamePrefix = 'LanguageSubMenuItem_';

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

procedure TMainForm.InitUI;
var
  Fmt: TImageFormat;
begin
  {$IFOPT D+}
    MainForm.Caption := MainForm.Caption + '    [DEBUG BUILD]';
  {$ENDIF}

  // Set default tray icon
  TrayIconState := tisDefault;

  // Fill combobox with image formats
  for Fmt := Low(TImageFormat) to High(TImageFormat) do
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
var
  DefaultOutputDir: String;
  CfgLang, SysLang, AltLang: TLanguageCode;
  FmtStr: String;
  Seconds: Integer;
begin
  DefaultOutputDir := IncludeTrailingPathDelimiter(JoinPath(ExtractFilePath(Application.ExeName), 'screenshots'));
  OutputDirEdit.Text := Ini.ReadString(DefaultConfigIniSection, 'OutputDir', DefaultOutputDir);
  // ToDo: Check that directory exists or can be created (with subdirs if needed)
  if OutputDirEdit.Text = '' then
    OutputDirEdit.Text := DefaultOutputDir;

  FileNameTemplateComboBox.Text := Ini.ReadString(DefaultConfigIniSection, 'FileNameTemplate', DefaultFileNameTemplate);

  Seconds := Round(Ini.ReadFloat(DefaultConfigIniSection, 'CaptureInterval', DefaultCaptureInterval) * SecsPerMin);
  Seconds := Max(Seconds, MinCaptureIntervalInSeconds);
  CaptureInterval.Time := EncodeTime(0, 0, 0, 0);
  CaptureInterval.Time := IncSecond(CaptureInterval.Time, Seconds);

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

  GrayscaleCheckBox.Checked := Ini.ReadBool(DefaultConfigIniSection, 'Grayscale', False);

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
  Timer.Interval := SecondOfTheDay(CaptureInterval.Time) * MSecsPerSec;
  StartCaptureOnStartUpCheckBox.Checked :=
      Ini.ReadBool(DefaultConfigIniSection, 'StartCaptureOnStartUp', {True} False);
  IsTimerEnabled := StartCaptureOnStartUpCheckBox.Checked;

  // Start with Windows
  AutoRunCheckBox.Checked :=
    ini.ReadBool(DefaultConfigIniSection, 'AutoRun', False);
  
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
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  ///////
  ColorDepthTmp: TColorDepth;
  ////////
begin
  { Replace default window function with custom one
    for process messages when screen configuration changed }
  PrevWndProc := Windows.WNDPROC
    (SetWindowLongPtr(Self.Handle, GWL_WNDPROC {GWLP_WNDPROC}, PtrUInt(@WndCallback)));

  Application.OnMinimize := ApplicationMinimize;

  InitUI;

  Ini := TIniFile.Create(ExtractFilePath(Application.ExeName) + '\config.ini');
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
    GrayscaleCheckBox.Checked);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Grabber.Free;
  Ini.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  RecalculateLabelWidths;
end;

procedure TMainForm.OutputDirEditChange(Sender: TObject);
begin
    Ini.WriteString(DefaultConfigIniSection, 'OutputDir', OutputDirEdit.Text);
end;

procedure TMainForm.CaptureIntervalChange(Sender: TObject);
var
  Seconds: Integer;
begin
  Seconds := SecondOfTheDay(CaptureInterval.Time);
  if Seconds < MinCaptureIntervalInSeconds then
  begin
    Seconds := MinCaptureIntervalInSeconds;
    CaptureInterval.Time := EncodeTime(0, 0, 0, 0);
    CaptureInterval.Time := IncSecond(CaptureInterval.Time, Seconds);
  end;
  Ini.WriteFloat(DefaultConfigIniSection, 'CaptureInterval', Seconds / SecsPerMin);
  Timer.Interval := Seconds * MSecsPerSec;
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

    // ToDo: Use TIdleTimer instead (https://forum.lazarus.freepascal.org/index.php/topic,15811.msg126545.html#msg126545)
    if Timer.Interval > LastInput then
      MakeScreenshot;
  end
  else
    MakeScreenshot;
end;

function TMainForm.GetTimerEnabled: Boolean;
begin
  Result := Timer.Enabled;
end;

procedure TMainForm.SetTimerEnabled(IsEnabled: Boolean);
begin
  Timer.Enabled := IsEnabled;
  StartAutoCaptureButton.Enabled := not IsEnabled;
  StopAutoCaptureButton.Enabled := IsEnabled;
  // Tray menu
  ToggleAutoCaptureTrayMenuItem.Checked := IsEnabled;
  // Tray icon
  if IsEnabled then
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
begin
  TrayIconState := tisFlashAnimation;

  if MonitorId = NoMonitorId then
    Grabber.CaptureAllMonitors(ImagePath)
  else
    Grabber.CaptureMonitor(ImagePath, MonitorId);


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

  FullDir := IncludeTrailingPathDelimiter(JoinPath(BaseDir, SubDir));

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
  IsQualityVisible, IsGrayscaleVisible: Boolean;
begin
  Format := ImageFormat;
  IsQualityVisible := ImageFormatInfoArray[Format].HasQuality;

  JPEGQualitySpinEdit.Visible := IsQualityVisible;
  JPEGQualityLabel.Visible    := IsQualityVisible;
  JPEGQualityPercentLabel.Visible := IsQualityVisible;

  IsGrayscaleVisible := ImageFormatInfoArray[Format].HasGrayscale;
  GrayscaleCheckBox.Visible := IsGrayscaleVisible;

  UpdateColorDepthValues;
  
  Ini.WriteString(DefaultConfigIniSection, 'ImageFormat', ImageFormatInfoArray[Format].Name);

  if Grabber <> nil then
    Grabber.ImageFormat := Format;
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

    // Tray icon
    RestoreWindowTrayMenuItem.Caption := Localizer.I18N('Restore');
    ToggleAutoCaptureTrayMenuItem.Caption := Localizer.I18N('EnableAutoCapture');
    TakeScreenshotTrayMenuItem.Caption := Localizer.I18N('TakeScreenshot');
    ExitTrayMenuItem.Caption := Localizer.I18N('Exit');

  finally
    EnableAutoSizing;

    // Recalculate with of labels area
    RecalculateLabelWidths;
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
  for Fmt := Low(TImageFormat) to High(TImageFormat) do
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
  Ini.WriteBool(DefaultConfigIniSection, 'Grayscale', GrayscaleCheckBox.Checked);
end;

procedure TMainForm.ColorDepthComboBoxChange(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := ColorDepthComboBox.ItemIndex;
  if Idx <> -1 then
    ColorDepth := TColorDepth(ColorDepthComboBox.Items.Objects[Idx]);
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
    for ColorDepthTmp := Low(TColorDepth) to High(TColorDepth) do
    begin
      if ColorDepthTmp in ImageFormatInfoArray[ImageFormat].ColorDepth then
      begin
        ColorDepthComboBox.Items.AddObject(Format('%d bit', [Integer(ColorDepthTmp)]), TObject(Integer(ColorDepthTmp)));
        if ColorDepthTmp = FColorDepth then
        begin
          // Select last saved color depth if available
          ColorDepth := TColorDepth(ColorDepthComboBox.Items.Objects[Idx]);
        end;
        Inc(Idx);
      end;
    end;

    if (ColorDepthComboBox.ItemIndex = -1) and (FColorDepth <> TColorDepth(0)) then
    begin
      // Select best color depth (last one in the list)
      Idx := ColorDepthComboBox.Items.Count - 1;
      ColorDepth := TColorDepth(ColorDepthComboBox.Items.Objects[Idx]);
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
      if TColorDepth(ColorDepthComboBox.Items.Objects[Idx]) = AColorDepth then
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
    else ResName := 'MAINICON';
  end;

  TrayIcon.Icon.Handle := LoadImage(HInstance, PChar(ResName), IMAGE_ICON,
    16, 16, LR_DEFAULTCOLOR);
end;

procedure TMainForm.TrayIconAnimationTimerTimer(Sender: TObject);
var
  ResName: String;
begin
  if (TrayIconIdx < High(TrayIconIdx)) then
  begin
    Inc(TrayIconIdx);
    ResName := Format('_CAMERA_FLASH_%d', [TrayIconIdx]);
    TrayIcon.Icon.Handle := LoadImage(HInstance, PChar(ResName), IMAGE_ICON,
      16, 16, LR_DEFAULTCOLOR);
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
var
  AutoRunEnabled: Boolean;
begin
  AutoRunEnabled := AutoRunCheckBox.Checked;
  AutoRun(Application.ExeName, 'Auto Screenshot', AutoRunEnabled);
  Ini.WriteBool(DefaultConfigIniSection, 'AutoRun', AutoRunEnabled);
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
    Result := MonitorComboBox.ItemIndex - 1;
end;

procedure TMainForm.SetMonitorId(MonitorId: Integer);
begin
  if MonitorId = NoMonitorId then
    MonitorComboBox.ItemIndex := 0
  else if (MonitorId >= 0) and (MonitorId < {Screen.MonitorCount} MonitorComboBox.Items.Count) then
    MonitorComboBox.ItemIndex := MonitorId + 1
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
  I: integer;
  MenuItem: TMenuItem;
begin
  while LanguageSubMenu.Count > 0 do
    LanguageSubMenu.Items[0].Free;
  LanguageSubMenu.Clear;

  Localizer.GetLanguages(AvailableLanguages);
  for I := 0 to Length(AvailableLanguages) - 1 do
  begin
    Lang := AvailableLanguages[I];

    if (Lang.Name <> '') and (Lang.Code <> '') then
    begin
      MenuItem := TMenuItem.Create(LanguageSubMenu);
      MenuItem.Caption := Lang.Name;
      if (Lang.NativeName <> '') and (Lang.NativeName <> Lang.Name) then
        MenuItem.Caption := MenuItem.Caption + ' (' + Lang.NativeName + ')';
      MenuItem.OnClick := LanguageClick;
      MenuItem.RadioItem := True;
      //MenuItem.GroupIndex := GroupIdx;
      MenuItem.Name := LanguageSubMenuItemNamePrefix + Lang.Code;

      LanguageSubMenu.Add(MenuItem);
    end
  end;
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

  CounterStr := Format('%.' + IntToStr(CounterDigits) + 'd', [Counter]); // Add leading zeros to Counter value

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
