unit uAutoScreen;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, inifiles, Spin, FileCtrl, pngImage,
  TrayIcon, XPMan, jpeg, ShellAPI, Menus, GifImage;

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

  TLanguage = (lngEnglish=0, lngRussian);

  TTrayIconState = (tisDefault, tisBlackWhite, tisFlashAnimation);

  TMainForm = class(TForm)
    OutputDirEdit: TEdit;
    ChooseOutputDirButton: TButton;
    Timer: TTimer;
    OutputDirLabel: TLabel;
    CaptureIntervalLabel: TLabel;
    TrayIcon: TTrayIcon;
    XPManifest: TXPManifest;
    ImageFormatLabel: TLabel;
    TakeScreenshotButton: TButton;
    JPEGQualityLabel: TLabel;
    JPEGQualitySpinEdit: TSpinEdit;
    OpenOutputDirButton: TButton;
    StopWhenInactiveCheckBox: TCheckBox;
    LanguageRadioGroup: TRadioGroup;
    ImageFormatComboBox: TComboBox;
    JPEGQualityPercentLabel: TLabel;
    AutoCaptureControlGroup: TGroupBox;
    StartAutoCaptureButton: TButton;
    StopAutoCaptureButton: TButton;
    TrayIconPopupMenu: TPopupMenu;
    ExitTrayMenuItem: TMenuItem;
    TakeScreenshotTrayMenuItem: TMenuItem;
    RestoreWindowTrayMenuItem: TMenuItem;
    ToggleAutoCaptureTrayMenuItem: TMenuItem;
    Separator2TrayMenuItem: TMenuItem;
    AboutButton: TButton;
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
    procedure AboutButtonClick(Sender: TObject);
    procedure LanguageRadioGroupClick(Sender: TObject);
    procedure StartCaptureOnStartUpCheckBoxClick(Sender: TObject);
    procedure StartMinimizedCheckBoxClick(Sender: TObject);
    procedure FileNameTemplateComboBoxChange(Sender: TObject);
    procedure FileNameTemplateHelpButtonClick(Sender: TObject);
    procedure GrayscaleCheckBoxClick(Sender: TObject);
    procedure ColorDepthComboBoxChange(Sender: TObject);
    procedure TrayIconAnimationTimerTimer(Sender: TObject);
  private
    { Private declarations }
    FLanguage: TLanguage;
    FColorDepth: TColorDepth;
    FTrayIconState: TTrayIconState;

    TrayIconIdx: 1..7;
    
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
    procedure SetLanguage(Lang: TLanguage);
    procedure SetLanguageByCode(LangCode: String);
    procedure TranslateForm();
    procedure InitUI;
    procedure ReadSettings;
    procedure UpdateColorDepthValues;

    property IsTimerEnabled: Boolean read GetTimerEnabled write SetTimerEnabled;
    property FinalOutputDir: String read GetFinalOutputDir;
    property ImagePath: String read GetImagePath;
    property Language: TLanguage read FLanguage write SetLanguage;
    property ImageFormat: TImageFormat read GetImageFormat write SetImageFormat;
    property ColorDepth: TColorDepth read GetColorDepth write SetColorDepth;
    property TrayIconState: TTrayIconState write SetTrayIconState;
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
  
  LanguageCodes: array [TLanguage] of String = ('en', 'ru');
  DefaultConfigIniSection = 'main';

  MinCaptureIntervalInSeconds = 1;

var
  MainForm: TMainForm;
  Ini: TIniFile;

implementation

uses uAbout, DateUtils, uLocalization, uUtils, Math;

{$R *.dfm}

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
end;

procedure TMainForm.ReadSettings;
const
  DefaultFileNameTemplate = '%Y-%M-%D\%Y-%M-%D %H.%N.%S';
  DefaultCaptureInterval  = 5;
  DefaultImageFormat      = fmtPNG;
  DefaultJPEGQuality      = 80;
  DefaultLanguage         = lngEnglish;
  DefaultColorDepth       = cd24Bit;
var
  FmtStr: String;
  LangCode: String;
  Seconds: Integer;
begin
  OutputDirEdit.Text := Ini.ReadString(DefaultConfigIniSection, 'OutputDir', ExtractFilePath(Application.ExeName));
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
  LangCode := Ini.ReadString(DefaultConfigIniSection, 'Language', LanguageCodes[DefaultLanguage]);
  try
    SetLanguageByCode(LangCode);
  except
    SetLanguage(DefaultLanguage);
  end;

  // Start autocapture
  Timer.Interval := SecondOfTheDay(CaptureInterval.Time) * MSecsPerSec;
  StartCaptureOnStartUpCheckBox.Checked :=
      Ini.ReadBool(DefaultConfigIniSection, 'StartCaptureOnStartUp', {True} False);
  IsTimerEnabled := StartCaptureOnStartUpCheckBox.Checked;

  // Start minimized
  if Ini.ReadBool(DefaultConfigIniSection, 'StartMinimized', False) then
  begin
    StartMinimizedCheckBox.Checked := True;
    MinimizeToTray;
  end
  else
    RestoreFromTray;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.OnMinimize := ApplicationMinimize;

  InitUI;

  Ini := TIniFile.Create(ExtractFilePath(Application.ExeName) + '\config.ini');
  ReadSettings;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Ini.Free;
end;

procedure TMainForm.ChooseOutputDirButtonClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := OutputDirEdit.Text;

  if SelectDirectory(I18N('SelectOutputDirectory'), '' {savepath.Text}, Dir) then
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

  Bitmap.Width := Screen.Width;
  Bitmap.Height := Screen.Height;
  ScreenDC := GetDC(0);
  BitBlt(Bitmap.Canvas.Handle, 0, 0, Screen.Width, Screen.Height,
           ScreenDC, 0, 0, SRCCOPY);
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
end;

procedure TMainForm.TakeScreenshotButtonClick(Sender: TObject);
begin
  Hide;
  Sleep(2000); // Add some delay in order to window has time to hide
  try
    try
      MakeScreenshot;
      Sleep(1000);
    finally
      Show;
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
  SubDir := FormatDateTime2(SubDir);

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
  FileName := FormatDateTime2(FileName);

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
  Sleep(2000); // Add some delay before capture
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

procedure TMainForm.AboutButtonClick(Sender: TObject);
begin
  with TAboutForm.Create(Application) do
  begin
    ShowModal;
  end;
end;

procedure TMainForm.LanguageRadioGroupClick(Sender: TObject);
begin
  Language := TLanguage(LanguageRadioGroup.ItemIndex)
end;

procedure TMainForm.SetLanguage(Lang: TLanguage);
begin
  {if (FLanguage = Lang) then
    Exit;}

  FLanguage := Lang;
  Ini.WriteString(DefaultConfigIniSection, 'Language', LanguageCodes[Lang]);
  LanguageRadioGroup.ItemIndex := Ord(Lang);
  I18NSetLang(LanguageCodes[Lang]);
  TranslateForm;
end;

procedure TMainForm.SetLanguageByCode(LangCode: String);
var
  LangIdx: TLanguage;
begin
  for LangIdx := Low(TLanguage) to High(TLanguage) do
  begin
    if LangCode = LanguageCodes[LangIdx] then
    begin
      SetLanguage(LangIdx);
      Exit;
    end;
  end;

  raise Exception.CreateFmt('Unknown language code "%s"', [LangCode]);
end;

procedure TMainForm.TranslateForm;
begin
  // Main form
  LanguageRadioGroup.Caption := I18N('Language');
  OutputDirLabel.Caption := I18N('OutputDirectory') + ':';
  OpenOutputDirButton.Caption := I18N('OpenDirectory');
  FileNameTemplateLabel.Caption := I18N('FileNameTemplate') + ':';
  CaptureIntervalLabel.Caption := I18N('CaptureInterval') + ':';
  StopWhenInactiveCheckBox.Caption := I18N('PauseCaptureWhenIdle');
  ImageFormatLabel.Caption := I18N('Format') + ':';
  ColorDepthLabel.Caption := I18N('ColorDepth') + ':';
  JPEGQualityLabel.Caption := I18N('Quality') + ':';
  GrayscaleCheckBox.Caption := I18N('Grayscale');
  AutoCaptureControlGroup.Caption := I18N('AutoCapture');
  StartAutoCaptureButton.Caption := I18N('StartCapture');
  StopAutoCaptureButton.Caption := I18N('StopCapture');
  TakeScreenshotButton.Caption := I18N('TakeScreenshot');
  AboutButton.Caption := I18N('About');
  StartCaptureOnStartUpCheckBox.Caption := I18N('StartCaptureOnStartUp');
  StartMinimizedCheckBox.Caption := I18N('StartMinimized');

  // Tray icon
  RestoreWindowTrayMenuItem.Caption := I18N('Restore');
  ToggleAutoCaptureTrayMenuItem.Caption := I18N('EnableAutoCapture');
  TakeScreenshotTrayMenuItem.Caption := I18N('TakeScreenshot');
  ExitTrayMenuItem.Caption := I18N('Exit');
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
  ShowMessage(I18N('FileNameTemplateHelpText'));
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
    tisBlackWhite: ResName := 'CAMERA_BW';
    tisFlashAnimation:
      begin
        TrayIconIdx := Low(TrayIconIdx);
        TrayIconAnimationTimer.Enabled := True;
        ResName := Format('CAMERA_FLASH_%d', [TrayIconIdx]);
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
    ResName := Format('CAMERA_FLASH_%d', [TrayIconIdx]);
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

end.
