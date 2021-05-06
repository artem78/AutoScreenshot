unit uAutoScreen;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, inifiles, Spin, FileCtrl, pngImage,
  TrayIcon, XPMan, jpeg, ShellAPI, Menus, GifImage, Buttons, TntForms, TntStdCtrls,
  TntMenus, TntComCtrls, TntButtons, TntExtCtrls, TntDialogs, TntFileCtrl,
  uLocalization;

type
  TImageFormat = (fmtPNG=0, fmtJPG, fmtBMP, fmtGIF);

  TColorDepth = (cd8Bit=8, cd16Bit=16, cd24Bit=24, cd32Bit=32);

  TImageFormatInfo = record
    Name: String[10];
    Extension: String[3];
    HasQuality: Boolean;
    HasGrayscale: Boolean;
    ColorDepth: Set of TColorDepth;
  end;

  TImageFormatInfoArray = array [TImageFormat] of TImageFormatInfo;

  TTrayIconState = (tisDefault, tisBlackWhite, tisFlashAnimation);

  TMainForm = class({TTntForm} TForm)
    OutputDirEdit: TTntEdit;
    ChooseOutputDirButton: TTntButton;
    Timer: TTimer;
    OutputDirLabel: TTntLabel;
    CaptureIntervalLabel: TTntLabel;
    TrayIcon: TTrayIcon;
    XPManifest: TXPManifest;
    ImageFormatLabel: TTntLabel;
    TakeScreenshotButton: TTntButton;
    JPEGQualityLabel: TTntLabel;
    JPEGQualitySpinEdit: TSpinEdit;
    OpenOutputDirButton: TTntButton;
    StopWhenInactiveCheckBox: TTntCheckBox;
    ImageFormatComboBox: TTntComboBox;
    JPEGQualityPercentLabel: TTntLabel;
    AutoCaptureControlGroup: TTntGroupBox;
    StartAutoCaptureButton: TTntBitBtn;
    StopAutoCaptureButton: TTntBitBtn;
    TrayIconPopupMenu: TTntPopupMenu;
    ExitTrayMenuItem: TTntMenuItem;
    TakeScreenshotTrayMenuItem: TTntMenuItem;
    RestoreWindowTrayMenuItem: TTntMenuItem;
    ToggleAutoCaptureTrayMenuItem: TTntMenuItem;
    Separator2TrayMenuItem: TTntMenuItem;
    StartCaptureOnStartUpCheckBox: TTntCheckBox;
    StartMinimizedCheckBox: TTntCheckBox;
    Separator1TrayMenuItem: TTntMenuItem;
    FileNameTemplateLabel: TTntLabel;
    FileNameTemplateComboBox: TTntComboBox;
    FileNameTemplateHelpButton: TTntButton;
    GrayscaleCheckBox: TTntCheckBox;
    ColorDepthLabel: TTntLabel;
    ColorDepthComboBox: TTntComboBox;
    CaptureInterval: TTntDateTimePicker;
    TrayIconAnimationTimer: TTimer;
    AutoRunCheckBox: TTntCheckBox;
    MonitorLabel: TTntLabel;
    MonitorComboBox: TTntComboBox;
    MainMenu: TTntMainMenu;
    HelpSubMenu: TTntMenuItem;
    AboutMenuItem: TTntMenuItem;
    OptionsSubMenu: TTntMenuItem;
    LanguageSubMenu: TTntMenuItem;
    SeqNumberGroup: TTntGroupBox;
    SeqNumberValueLabel: TTntLabel;
    SeqNumberValueSpinEdit: TSpinEdit;
    SeqNumberDigitsCountSpinEdit: TSpinEdit;
    SeqNumberDigitsCountLabel: TTntLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ChooseOutputDirButtonClick(Sender: TObject);
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
    procedure SeqNumberValueSpinEditChange(Sender: TObject);
    procedure SeqNumberDigitsCountSpinEditChange(Sender: TObject);
  private
    { Private declarations }
    AvailableLanguages: TLanguagesArray;
    FLanguage: TLanguageCode;  { ??? }
    FColorDepth: TColorDepth;
    FTrayIconState: TTrayIconState;

    TrayIconIdx: 1..7;

    FCounter: Integer;
    FCounterDigits: Integer {Byte};
    
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
    procedure FillMonitorList;
    procedure SetMonitorId(MonitorId: Integer);
    function GetMonitorId: Integer;
    procedure UpdateLanguages;
    procedure LanguageClick(Sender: TObject);
    function GetLangCodeOfLangMenuItem(const LangItem: TTntMenuItem): TLanguageCode;
    function FindLangMenuItem(ALangCode: TLanguageCode): TTntMenuItem;
    function FormatPath(Str: string): string;
    procedure SetCounter(Val: Integer);
    procedure SetCounterDigits(Val: Integer);

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
  public
    { Public declarations }
  end;

const
  ImageFormatInfoArray: TImageFormatInfoArray = (
    (
      Name:         'PNG';
      Extension:    'png';
      HasQuality:   False;
      HasGrayscale: False;
      ColorDepth:   [cd8Bit, cd24Bit]
    ),
    (
      Name:         'JPG';
      Extension:    'jpg';
      HasQuality:   True;
      HasGrayscale: True;
      ColorDepth:   []
    ),
    (
      Name:         'BMP';
      Extension:    'bmp';
      HasQuality:   False;
      HasGrayscale: False;
      ColorDepth:   [cd8Bit, cd16Bit, cd24Bit, cd32Bit]
    ),
    (
      Name:         'GIF';
      Extension:    'gif';
      HasQuality:   False;
      HasGrayscale: False;
      ColorDepth:   []
    )
  );

  DefaultConfigIniSection = 'main';

  MinCaptureIntervalInSeconds = 1;
  NoMonitorId = -1;
  MinCounter = 1;
  MinCounterDigits = 1;
  MaxCounterDigits = 10;

var
  MainForm: TMainForm;
  Ini: TIniFile;

implementation

uses uAbout, DateUtils, uUtils, Math, VistaAltFixUnit;

{$R *.dfm}

const
  LanguageSubMenuItemNamePrefix = 'LanguageSubMenuItem_';

procedure TMainForm.InitUI;
var
  Fmt: TImageFormat;
begin
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
  SeqNumberValueSpinEdit.MinValue := MinCounter;
  SeqNumberDigitsCountSpinEdit.MinValue := MinCounterDigits;
  SeqNumberDigitsCountSpinEdit.MaxValue := MaxCounterDigits;
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
  DefaultCounter          = MinCounter;
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

  StopWhenInactiveCheckBox.Checked := Ini.ReadBool(DefaultConfigIniSection, 'StopWhenInactive', False);

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

  JPEGQualitySpinEdit.Value := Ini.ReadInteger(DefaultConfigIniSection, 'JPEGQuality', DefaultJPEGQuality);

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
  if Ini.ReadBool(DefaultConfigIniSection, 'StartMinimized', False) then
  begin
    StartMinimizedCheckBox.Checked := True;
    MinimizeToTray;
  end
  else
    RestoreFromTray;

  // Multiple monitors
  if Screen.MonitorCount >= 2 then
  begin
    try
      MonitorId := Ini.ReadInteger(DefaultConfigIniSection, 'Monitor', DefaultMonitorId);
    except
      MonitorId := DefaultMonitorId;
    end;
  end
  else
  begin // Only one monitor available
    MonitorLabel.Enabled := False;
    MonitorComboBox.Enabled := False;
    //MonitorId := NoMonitorId;
    MonitorId := 0;
  end;

  // Incremental counter
  Counter := Ini.ReadInteger(DefaultConfigIniSection, 'Counter', DefaultCounter);
  CounterDigits := Ini.ReadInteger(DefaultConfigIniSection, 'CounterDigits', DefaultCounterDigits);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.OnMinimize := ApplicationMinimize;

  InitUI;

  // Fix components disappearing when ALT key pressed on Windows Vista and later
  TVistaAltFix.Create(Self);

  Ini := TIniFile.Create(ExtractFilePath(Application.ExeName) + '\config.ini');
  ReadSettings;

  //if FindCmdLineSwitch('autorun') then
  //  OutputDebugString('AutoRun');
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Ini.Free;
end;

procedure TMainForm.ChooseOutputDirButtonClick(Sender: TObject);
var
  Dir: WideString;
begin
  Dir := OutputDirEdit.Text;

  if WideSelectDirectory(Localizer.I18N('SelectOutputDirectory'), '' {savepath.Text}, Dir) then
  //if SelectDirectory(dir, [sdAllowCreate, sdPerformCreate], 0) then
  begin
    OutputDirEdit.Text := Dir;
    Ini.WriteString(DefaultConfigIniSection, 'OutputDir', Dir);
  end;
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
  if StopWhenInactiveCheckBox.Checked then
  begin
    // Skip taking screenshot if there are no user activity
    // for autocapture interval minutes

    // ToDo: May add check for screensaver active
    // or user logged off from the session
    // ToDo: May add comparision of current screenshot with the last one,
    // and if they equal, do not save current
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
var
  Bitmap: TBitmap;
  PNG: TPNGObject;
  JPG: TJPEGImage;
  GIF: TGIFImage;
  ScreenDC: HDC;
  ScreenWidth, ScreenHeight: Integer;
  ScreenX, ScreenY: Integer;
  //Rect: TRect;
  UsedMonitor: TMonitor;
begin
  Bitmap := TBitmap.Create;

  // Set color depth for bitmap
  try
    case Integer(ColorDepth) of
      1:  Bitmap.PixelFormat := pf1bit;
      4:  Bitmap.PixelFormat := pf4bit;
      8:  Bitmap.PixelFormat := pf8bit;
      16: Bitmap.PixelFormat := pf16bit;
      24: Bitmap.PixelFormat := pf24bit;
      32: Bitmap.PixelFormat := pf32bit;
      //else raise Exception.CreateFmt('Color depth %d bit not supported in TBitmap', [Integer(ColorDepth)]);
    end;
  except
    // Leave bitmap pixel format as default
  end;

  if MonitorId = NoMonitorId then
  begin // All displays
    ScreenWidth  := GetSystemMetrics(SM_CXVIRTUALSCREEN);
    ScreenHeight := GetSystemMetrics(SM_CYVIRTUALSCREEN);
    ScreenX := GetSystemMetrics(SM_XVIRTUALSCREEN);
    ScreenY := GetSystemMetrics(SM_YVIRTUALSCREEN);
  end
  else // Only one display
  begin
    UsedMonitor := Screen.Monitors[MonitorId];
    ScreenWidth  := UsedMonitor.Width;
    ScreenHeight := UsedMonitor.Height;
    ScreenX := UsedMonitor.Left;
    ScreenY := UsedMonitor.Top;
  end;
  //Rect := GetClientRect(0);

  Bitmap.Width := ScreenWidth;
  Bitmap.Height := ScreenHeight;
  Bitmap.Canvas.Brush.Color := clBlack;
  Bitmap.Canvas.FillRect(Rect(0, 0, ScreenWidth, ScreenHeight));
  ScreenDC := GetDC(HWND_DESKTOP); // Get DC for all monitors
  BitBlt(Bitmap.Canvas.Handle, 0, 0, ScreenWidth, ScreenHeight,
           ScreenDC, ScreenX, ScreenY, SRCCOPY);
  ReleaseDC(0, ScreenDC);

  TrayIconState := tisFlashAnimation;

  try
    case ImageFormat of
      fmtPNG:      // PNG
        begin
          PNG := TPNGObject.Create;
          try
            PNG.Assign(Bitmap);
            PNG.SaveToFile(ImagePath);
          finally
            PNG.Free;
          end;
        end;

      fmtJPG:     // JPEG
        begin
          JPG := TJPEGImage.Create;
          try
            JPG.CompressionQuality := JPEGQualitySpinEdit.Value;
            JPG.Grayscale := GrayscaleCheckBox.Checked;
            JPG.Assign(Bitmap);
            JPG.Compress;
            JPG.SaveToFile(ImagePath);
          finally
            JPG.Free;
          end;
        end;

      fmtBMP:    // Bitmap (BMP)
        begin
          Bitmap.SaveToFile(ImagePath);
        end;

      fmtGIF:    // GIF
        begin
          GIF := TGIFImage.Create;
          try
            GIF.Assign(Bitmap);
            GIF.OptimizeColorMap;
            GIF.SaveToFile(ImagePath);
          finally
            GIF.Free;
          end;
        end;
    end;
  finally
    Bitmap.Free;
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
    try
      MakeScreenshot;
    finally
      // Restore transparency to initial value
      AlphaBlendValue := DefaultTransparency;
      AlphaBlend := False;
    end;
  except
  end;
end;

procedure TMainForm.JPEGQualitySpinEditChange(Sender: TObject);
begin
  try
    Ini.WriteInteger(DefaultConfigIniSection, 'JPEGQuality', JPEGQualitySpinEdit.Value);
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
  DirName, FileName, Ext, FullFileName: String;
  I: Integer;
begin
  FileName := ExtractFileName(FileNameTemplateComboBox.Text);
  FileName := FormatPath(FileName);

  DirName := IncludeTrailingPathDelimiter(FinalOutputDir);

  Ext := ImageFormatInfoArray[ImageFormat].Extension;

  // If image file already exist create new one with index number
  I := 1;
  repeat
    if I = 1 then
      FullFileName := DirName + FileName + '.' + Ext
    else
      FullFileName := DirName + FileName + ' (' + IntToStr(I) + ')' + '.' + Ext;

    Inc(I);
  until not FileExists(FullFileName);

  Result := FullFileName;
end;

procedure TMainForm.OpenOutputDirButtonClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(FinalOutputDir), nil, nil, SW_SHOWNORMAL);
end;

procedure TMainForm.StopWhenInactiveCheckBoxClick(Sender: TObject);
begin
  Ini.WriteBool(DefaultConfigIniSection, 'StopWhenInactive', StopWhenInactiveCheckBox.Checked);
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
  TrayIcon.AppVisible := False;
  TrayIcon.FormVisible := False;
  TrayIcon.IconVisible := True;
end;

procedure TMainForm.RestoreFromTray;
begin
  TrayIcon.IconVisible := False;
  TrayIcon.AppVisible := True;
  TrayIcon.FormVisible := True;
  Application.Restore;
  Application.BringToFront();
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
  // Menubar
  OptionsSubMenu.Caption := Localizer.I18N('Options');
  LanguageSubMenu.Caption := Localizer.I18N('Language');
  HelpSubMenu.Caption := Localizer.I18N('Help');
  AboutMenuItem.Caption := Localizer.I18N('About') + '...';

  // Main form components
  OutputDirLabel.Caption := Localizer.I18N('OutputDirectory') + ':';
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
end;

procedure TMainForm.StartCaptureOnStartUpCheckBoxClick(Sender: TObject);
begin
  Ini.WriteBool(DefaultConfigIniSection, 'StartCaptureOnStartUp', StartCaptureOnStartUpCheckBox.Checked);
end;

procedure TMainForm.StartMinimizedCheckBoxClick(Sender: TObject);
begin
  Ini.WriteBool(DefaultConfigIniSection, 'StartMinimized', StartMinimizedCheckBox.Checked);
end;

procedure TMainForm.FileNameTemplateComboBoxChange(Sender: TObject);
begin
  Ini.WriteString(DefaultConfigIniSection, 'FileNameTemplate', FileNameTemplateComboBox.Text);
end;

procedure TMainForm.FileNameTemplateHelpButtonClick(Sender: TObject);
begin
  WideShowMessage(Localizer.I18N('FileNameTemplateHelpText'));
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
  Inc(TrayIconIdx);
  if (TrayIconIdx <= High(TrayIconIdx)) then
  begin
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

begin
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

  Ini.WriteInteger(DefaultConfigIniSection, 'Monitor', MonitorId);
end;

procedure TMainForm.AboutMenuItemClick(Sender: TObject);
begin
  with TAboutForm.Create(Application) do
  begin
    ShowModal;
  end;
end;

procedure TMainForm.UpdateLanguages;
{const
  GroupIdx = 1;}
var
  Lang: TLanguageInfo;
  I: integer;
  MenuItem: TTntMenuItem;
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
      MenuItem := TTntMenuItem.Create(LanguageSubMenu);
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
  LangCode := GetLangCodeOfLangMenuItem(Sender as TTntMenuItem);
  SetLanguageByCode(LangCode);
end;

function TMainForm.FindLangMenuItem(ALangCode: TLanguageCode): TTntMenuItem;
var
  I: integer;
  LangCode: TLanguageCode;
  MenuItem: TTntMenuItem;
begin
  for I := 0 to LanguageSubMenu.Count - 1 do
  begin
    MenuItem := (LanguageSubMenu.Items[I] as TTntMenuItem); // Items[] returns TMenuItem instead od TTntMenuItem
    LangCode := GetLangCodeOfLangMenuItem(MenuItem);
    if LangCode = ALangCode then
    begin
      Result := MenuItem;
      Exit;
    end;
  end;

  raise Exception.CreateFmt('Language code "%s" not found', [ALangCode]);
end;

function TMainForm.GetLangCodeOfLangMenuItem(
  const LangItem: TTntMenuItem): TLanguageCode;
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

procedure TMainForm.SeqNumberDigitsCountSpinEditChange(Sender: TObject);
begin
  try
    CounterDigits := SeqNumberDigitsCountSpinEdit.Value;
  finally
  end;
end;

end.
