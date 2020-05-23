unit uAutoScreen;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, inifiles, Spin, FileCtrl, pngImage,
  TrayIcon, XPMan, jpeg, ShellAPI, Menus, GifImage;

type
  TImageFormat = (fmtPNG=0, fmtJPG, fmtBMP, fmtGIF);

  TImageFormatInfo = record
    Name: String[10];
    Extension: String[3];
    HasQuality: Boolean;
    HasGrayscale: Boolean;
  end;

  TImageFormatInfoArray = array [TImageFormat] of TImageFormatInfo;

  TLanguage = (lngEnglish=0, lngRussian);

  TMainForm = class(TForm)
    OutputDirEdit: TEdit;
    ChooseOutputDirButton: TButton;
    Timer: TTimer;
    CaptureInterval: TSpinEdit;
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
  private
    { Private declarations }
    FLanguage: TLanguage;
    
    procedure SetTimerEnabled(IsEnabled: Boolean);
    function GetTimerEnabled: Boolean;
    function GetFinalOutputDir: String;
    function GetImagePath: String;
    procedure SetImageFormatByStr(FmtStr: String);
    procedure SetImageFormat(Fmt: TImageFormat);
    function GetImageFormat: TImageFormat;
    procedure MakeScreenshot;
    procedure MinimizeToTray;
    procedure RestoreFromTray;
    procedure SetLanguage(Lang: TLanguage);
    procedure SetLanguageByCode(LangCode: String);
    procedure TranslateForm();
    procedure InitUI;
    procedure ReadSettings;

    // ToDo: Why this do not work?
    //    property IsTimerEnabled: Boolean read Timer.Enabled write SetTimerEnabled;
    //    Error: Record, object or class type required

    property IsTimerEnabled: Boolean read GetTimerEnabled write SetTimerEnabled;
    property FinalOutputDir: String read GetFinalOutputDir;
    property ImagePath: String read GetImagePath;
    property Language: TLanguage read FLanguage write SetLanguage;
    property ImageFormat: TImageFormat read GetImageFormat write SetImageFormat;
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
    ),
    (
      Name:         'JPG';
      Extension:    'jpg';
      HasQuality:   True;
      HasGrayscale: True;
    ),
    (
      Name:         'BMP';
      Extension:    'bmp';
      HasQuality:   False;
      HasGrayscale: False;
    ),
    (
      Name:         'GIF';
      Extension:    'gif';
      HasQuality:   False;
      HasGrayscale: False;
    )
  );
  
  LanguageCodes: array [TLanguage] of String = ('en', 'ru');
  DefaultConfigIniSection = 'main';

var
  MainForm: TMainForm;
  Ini: TIniFile;

implementation

uses uAbout{, DateUtils}, uLocalization, uUtils;

{$R *.dfm}

procedure TMainForm.InitUI;
var
  Fmt: TImageFormat;
begin
  // Without next line tray icon with incorrect size be taken
  TrayIcon.Icon.Handle := LoadImage(HInstance, 'MAINICON', IMAGE_ICON,
      16, 16, LR_DEFAULTCOLOR);

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
var
  FmtStr: String;
  LangCode: String;
begin
  OutputDirEdit.Text := Ini.ReadString(DefaultConfigIniSection, 'OutputDir', ExtractFilePath(Application.ExeName));
  FileNameTemplateComboBox.Text := Ini.ReadString(DefaultConfigIniSection, 'FileNameTemplate', DefaultFileNameTemplate);
  CaptureInterval.Value := Ini.ReadInteger(DefaultConfigIniSection, 'CaptureInterval', DefaultCaptureInterval);
  StopWhenInactiveCheckBox.Checked := Ini.ReadBool(DefaultConfigIniSection, 'StopWhenInactive', False);

  // Image format
  FmtStr := Ini.ReadString(DefaultConfigIniSection, 'ImageFormat',
      ImageFormatInfoArray[DefaultImageFormat].Name);
  try
    SetImageFormatByStr(FmtStr);
  except
    ImageFormat := DefaultImageFormat;
  end;

  JPEGQualitySpinEdit.Value := Ini.ReadInteger(DefaultConfigIniSection, 'JPEGQuality', DefaultJPEGQuality);

  GrayscaleCheckBox.Checked := Ini.ReadBool(DefaultConfigIniSection, 'Grayscale', False);

  // Language
  LangCode := Ini.ReadString(DefaultConfigIniSection, 'Language', LanguageCodes[DefaultLanguage]);
  try
    SetLanguageByCode(LangCode);
  except
    SetLanguage(DefaultLanguage);
  end;

  // Start autocapture
  Timer.Interval := CaptureInterval.Value * 60 * 1000;
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
begin
  Ini.WriteInteger(DefaultConfigIniSection, 'CaptureInterval', CaptureInterval.Value);
  Timer.Interval := CaptureInterval.Value * 60 * 1000;
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
  Bitmap.Width := Screen.Width;
  Bitmap.Height := Screen.Height;
  ScreenDC := GetDC(0);
  BitBlt(Bitmap.Canvas.Handle, 0, 0, Screen.Width, Screen.Height,
           ScreenDC, 0, 0, SRCCOPY);
  ReleaseDC(0, ScreenDC);

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
  DirName: String;
begin
  DirName := ExtractFileDir({Ini.ReadString(DefaultConfigIniSection, 'FileNameTemplate', '')} FileNameTemplateComboBox.Text);
  DirName := FormatDateTime2(DirName);

  DirName := IncludeTrailingPathDelimiter(Ini.ReadString(DefaultConfigIniSection, 'OutputDir', '')) + DirName + '\';
  if not DirectoryExists(DirName) then
  begin
    if not ForceDirectories(DirName) then
      RaiseLastOSError;
  end;

  Result := DirName;
end;

function TMainForm.GetImagePath: String;
var
  DirName, FileName, Ext, FullFileName: String;
  I: Integer;
begin
  FileName := ExtractFileName(FileNameTemplateComboBox.Text);
  FileName := FormatDateTime2(FileName);

  DirName := FinalOutputDir;

  Ext := ImageFormatInfoArray[ImageFormat].Extension;

  // If image file already exist create new one with order number
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
  Ini.WriteString(DefaultConfigIniSection, 'language', LanguageCodes[Lang]);
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
  //ChooseOutputDirButton.Caption := I18N('Choose') + '...';
  OpenOutputDirButton.Caption := I18N('OpenDirectory');
  FileNameTemplateLabel.Caption := I18N('FileNameTemplate') + ':';
  CaptureIntervalLabel.Caption := I18N('CaptureInterval') + ':';
  StopWhenInactiveCheckBox.Caption := I18N('PauseCaptureWhenIdle');
  ImageFormatLabel.Caption := I18N('Format') + ':';
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

end.
